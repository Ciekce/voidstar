/*
 * Voidstar, a UCI chess engine
 * Copyright (C) 2024 Ciekce
 *
 * Voidstar is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Voidstar is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Voidstar. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::attacks;
use crate::bitboard::Bitboard;
use crate::chess_move::ChessMove;
use crate::core::{Color, PieceType, Square};
use crate::position::Position;
use crate::rays::{ray_between, ray_intersecting};
use arrayvec::ArrayVec;

pub type MoveList = ArrayVec<ChessMove, 256>;

fn serialize_normal(moves: &mut MoveList, src: Square, dsts: Bitboard) {
    for dst in dsts {
        moves.push(ChessMove::normal(src, dst));
    }
}

fn serialize_normal_pawn(moves: &mut MoveList, offset: i32, dsts: Bitboard) {
    for dst in dsts {
        let src = Square::from_raw((i32::from(dst.raw()) - offset) as u8);
        moves.push(ChessMove::normal(src, dst));
    }
}

fn serialize_promo_pawn(moves: &mut MoveList, offset: i32, dsts: Bitboard) {
    for dst in dsts {
        let src = Square::from_raw((i32::from(dst.raw()) - offset) as u8);
        moves.push(ChessMove::promotion(src, dst, PieceType::KNIGHT));
        moves.push(ChessMove::promotion(src, dst, PieceType::BISHOP));
        moves.push(ChessMove::promotion(src, dst, PieceType::ROOK));
        moves.push(ChessMove::promotion(src, dst, PieceType::QUEEN));
    }
}

fn generate_pawn_moves(moves: &mut MoveList, pos: &Position, dst_mask: Bitboard) {
    let stm = pos.side_to_move();
    let nstm = stm.flip();

    let occ = pos.occupancy();
    let ep_sq = pos.en_passant();

    let checkers = pos.checkers();
    let them = pos.color_occupancy(stm.flip());

    let (ep_mask, ep_pawn) = if ep_sq == Square::NONE {
        (Bitboard::EMPTY, Square::NONE)
    } else {
        let ep_mask = ep_sq.bit();
        let ep_pawn = ep_mask.shift_down_relative(stm);

        if !checkers.is_empty() && (ep_pawn & checkers).is_empty() {
            (Bitboard::EMPTY, Square::NONE)
        } else {
            (ep_mask, ep_pawn.lowest_square())
        }
    };

    let promo_rank = if stm == Color::BLACK {
        Bitboard::RANK_1
    } else {
        Bitboard::RANK_8
    };
    let double_push_rank = if stm == Color::BLACK {
        Bitboard::RANK_5
    } else {
        Bitboard::RANK_4
    };

    let diag_pin_mask = pos.diag_pin_mask();
    let ortho_pin_mask = pos.ortho_pin_mask();

    let pawns = pos.colored_pieces(PieceType::PAWN.colored(stm));

    let single_offset = if stm == Color::BLACK { -8 } else { 8 };
    let double_offset = single_offset * 2;

    let (left_offset, right_offset) = if stm == Color::BLACK {
        (-9, -7)
    } else {
        (7, 9)
    };

    let mut gen_pushes = |pawns: Bitboard, mask: Bitboard| {
        let mut single_pushes = pawns.shift_up_relative(stm) & !occ;
        let double_pushes = mask & double_push_rank & single_pushes.shift_up_relative(stm);
        single_pushes &= mask;

        serialize_normal_pawn(moves, single_offset, single_pushes & !promo_rank);
        serialize_promo_pawn(moves, single_offset, single_pushes & promo_rank);

        serialize_normal_pawn(moves, double_offset, double_pushes & !occ);
    };

    let pushable_pawns = pawns & !diag_pin_mask;
    gen_pushes(pushable_pawns & !ortho_pin_mask, dst_mask);
    gen_pushes(pushable_pawns & ortho_pin_mask, dst_mask & ortho_pin_mask);

    let mut gen_captures = |pawns: Bitboard, mask: Bitboard| {
        let left_captures = mask & pawns.shift_up_left_relative(stm);
        let right_captures = mask & pawns.shift_up_right_relative(stm);

        serialize_normal_pawn(moves, left_offset, left_captures & !promo_rank);
        serialize_promo_pawn(moves, left_offset, left_captures & promo_rank);

        serialize_normal_pawn(moves, right_offset, right_captures & !promo_rank);
        serialize_promo_pawn(moves, right_offset, right_captures & promo_rank);
    };

    let capture_mask = dst_mask & them;

    let pawns_that_can_capture = pawns & !ortho_pin_mask;
    gen_captures(pawns_that_can_capture & !diag_pin_mask, capture_mask);
    gen_captures(
        pawns_that_can_capture & diag_pin_mask,
        capture_mask & diag_pin_mask,
    );

    let is_ep_legal = |src: Square| -> bool {
        let king = pos.king_square(stm);
        let post_ep_occ = pos.occupancy() ^ src.bit() ^ ep_mask ^ ep_pawn.bit();

        let their_queens = pos.colored_pieces(PieceType::QUEEN.colored(nstm));
        let their_rooks = their_queens | pos.colored_pieces(PieceType::ROOK.colored(nstm));
        let their_bishops = their_queens | pos.colored_pieces(PieceType::BISHOP.colored(nstm));

        (attacks::rook_attacks(king, post_ep_occ) & their_rooks).is_empty()
            && (attacks::bishop_attacks(king, post_ep_occ) & their_bishops).is_empty()
    };

    if ep_pawn != Square::NONE {
        for capturer in pawns_that_can_capture & attacks::pawn_attacks(nstm, ep_sq) {
            if is_ep_legal(capturer) {
                moves.push(ChessMove::en_passant(capturer, ep_sq));
            }
        }
    }
}

fn generate_knight_moves(moves: &mut MoveList, pos: &Position, dst_mask: Bitboard) {
    let pinned = pos.diag_pin_mask() | pos.ortho_pin_mask();
    let unpinned_knights =
        !pinned & pos.colored_pieces(PieceType::KNIGHT.colored(pos.side_to_move()));

    for knight in unpinned_knights {
        let attacks = attacks::knight_attacks(knight);
        serialize_normal(moves, knight, attacks & dst_mask);
    }
}

fn generate_bishop_moves(moves: &mut MoveList, pos: &Position, dst_mask: Bitboard) {
    let stm = pos.side_to_move();

    let bishops = pos.colored_pieces(PieceType::BISHOP.colored(stm));
    let queens = pos.colored_pieces(PieceType::QUEEN.colored(stm));

    let movable_bishops = !pos.ortho_pin_mask() & (bishops | queens);

    let pin_mask = pos.diag_pin_mask();

    let pinned_bishops = movable_bishops & pin_mask;
    let unpinned_bishops = movable_bishops & !pinned_bishops;

    let occ = pos.occupancy();

    for bishop in pinned_bishops {
        let attacks = attacks::bishop_attacks(bishop, occ);
        serialize_normal(moves, bishop, attacks & pin_mask & dst_mask);
    }

    for bishop in unpinned_bishops {
        let attacks = attacks::bishop_attacks(bishop, occ);
        serialize_normal(moves, bishop, attacks & dst_mask);
    }
}

fn generate_rook_moves(moves: &mut MoveList, pos: &Position, dst_mask: Bitboard) {
    let stm = pos.side_to_move();

    let rooks = pos.colored_pieces(PieceType::ROOK.colored(stm));
    let queens = pos.colored_pieces(PieceType::QUEEN.colored(stm));

    let movable_rooks = !pos.diag_pin_mask() & (rooks | queens);

    let pin_mask = pos.ortho_pin_mask();

    let pinned_rooks = movable_rooks & pin_mask;
    let unpinned_rooks = movable_rooks & !pinned_rooks;

    let occ = pos.occupancy();

    for rook in pinned_rooks {
        let attacks = attacks::rook_attacks(rook, occ);
        serialize_normal(moves, rook, attacks & pin_mask & dst_mask);
    }

    for rook in unpinned_rooks {
        let attacks = attacks::rook_attacks(rook, occ);
        serialize_normal(moves, rook, attacks & dst_mask);
    }
}

fn generate_king_moves(moves: &mut MoveList, pos: &Position) {
    let king = pos.king_square(pos.side_to_move());
    let us = pos.color_occupancy(pos.side_to_move());

    let kingless_occ = pos.occupancy() ^ king.bit();

    let potential_moves = {
        let mut potential_moves = attacks::king_attacks(king) & !us;

        for checker in pos.checkers() {
            let piece = pos.piece_type_at(checker);
            if piece == PieceType::BISHOP || piece == PieceType::ROOK || piece == PieceType::QUEEN {
                potential_moves &= !ray_intersecting(king, checker).without(checker);
            }
        }

        potential_moves
    };

    for dst in potential_moves {
        if !pos.is_attacked_occ(dst, kingless_occ) {
            moves.push(ChessMove::normal(king, dst));
        }
    }
}

fn generate_castling(moves: &mut MoveList, pos: &Position) {
    let rooks = pos.castling().color(pos.side_to_move());
    let pinned = pos.ortho_pin_mask();

    let king = pos.king_square(pos.side_to_move());
    let occ = pos.occupancy();

    let mut gen_castling = |rook: Square, king_dst_file: u32, rook_dst_file: u32| {
        let occ = occ ^ king.bit() ^ rook.bit();

        let king_dst = Square::from_coords(king.rank(), king_dst_file);
        let rook_dst = Square::from_coords(king.rank(), rook_dst_file);

        let to_king_dst = ray_between(king, king_dst).with(king_dst);
        let to_rook = ray_between(king, rook).with(rook_dst);

        if (occ & (to_king_dst | to_rook)).is_empty() && !pos.any_attacked(to_king_dst) {
            moves.push(ChessMove::castling(king, rook));
        }
    };

    if rooks.short != Square::NONE && !pinned.get(rooks.short) {
        gen_castling(rooks.short, Square::G1.file(), Square::F1.file());
    }

    if rooks.long != Square::NONE && !pinned.get(rooks.long) {
        gen_castling(rooks.long, Square::C1.file(), Square::D1.file());
    }
}

pub fn generate_moves(moves: &mut MoveList, pos: &Position) {
    generate_king_moves(moves, pos);

    let checkers = pos.checkers();

    if checkers.contains_multiple() {
        return;
    } else if checkers.is_empty() {
        generate_castling(moves, pos);
    }

    let dst_mask = if checkers.is_empty() {
        !pos.color_occupancy(pos.side_to_move())
    } else {
        let checker = checkers.lowest_square();
        ray_between(pos.king_square(pos.side_to_move()), checker).with(checker)
    };

    generate_pawn_moves(moves, pos, dst_mask);
    generate_knight_moves(moves, pos, dst_mask);
    generate_bishop_moves(moves, pos, dst_mask);
    generate_rook_moves(moves, pos, dst_mask);
    // queens are considered rooks and bishops
}
