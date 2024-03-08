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

use crate::bitboard::Bitboard;
use crate::chess_move::{ChessMove, MoveType};
use crate::core::*;
use crate::rays::ray_between;
use crate::{attacks, keys};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RookPair {
    pub short: Square,
    pub long: Square,
}

impl RookPair {
    fn clear(&mut self) {
        self.short = Square::NONE;
        self.long = Square::NONE;
    }

    fn unset(&mut self, sq: Square) {
        if sq == self.short {
            self.short = Square::NONE;
        } else if sq == self.long {
            self.long = Square::NONE;
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct CastlingRooks {
    rooks: [RookPair; 2],
}

impl CastlingRooks {
    fn empty() -> Self {
        Self {
            rooks: [
                RookPair {
                    short: Square::NONE,
                    long: Square::NONE,
                },
                RookPair {
                    short: Square::NONE,
                    long: Square::NONE,
                },
            ],
        }
    }

    fn startpos() -> Self {
        Self {
            rooks: [
                RookPair {
                    short: Square::H8,
                    long: Square::A8,
                },
                RookPair {
                    short: Square::H1,
                    long: Square::A1,
                },
            ],
        }
    }

    pub fn black(self) -> RookPair {
        self.rooks[0]
    }

    pub fn white(self) -> RookPair {
        self.rooks[1]
    }

    fn black_mut(&mut self) -> &mut RookPair {
        &mut self.rooks[0]
    }

    fn white_mut(&mut self) -> &mut RookPair {
        &mut self.rooks[1]
    }

    pub fn color(self, c: Color) -> RookPair {
        self.rooks[c.idx()]
    }

    fn color_mut(&mut self, c: Color) -> &mut RookPair {
        &mut self.rooks[c.idx()]
    }
}

#[derive(Debug, Clone)]
struct BoardState {
    colors: [Bitboard; 2],
    pieces: [Bitboard; 6],
    checkers: Bitboard,
    diag_pin_mask: Bitboard,
    ortho_pin_mask: Bitboard,
    key: u64,
    castling: CastlingRooks,
    halfmove: u16,
    en_passant: Square,
}

impl BoardState {
    #[must_use]
    fn occupancy(&self) -> Bitboard {
        self.colors[0] | self.colors[1]
    }

    #[must_use]
    fn black_occupancy(&self) -> Bitboard {
        self.colors[Color::BLACK.idx()]
    }

    #[must_use]
    fn white_occupancy(&self) -> Bitboard {
        self.colors[Color::WHITE.idx()]
    }

    #[must_use]
    fn color_occupancy(&self, color: Color) -> Bitboard {
        self.colors[color.idx()]
    }

    #[must_use]
    fn color_occupancy_mut(&mut self, color: Color) -> &mut Bitboard {
        &mut self.colors[color.idx()]
    }

    #[must_use]
    fn pieces(&self, piece: PieceType) -> Bitboard {
        self.pieces[piece.idx()]
    }

    #[must_use]
    fn pieces_mut(&mut self, piece: PieceType) -> &mut Bitboard {
        &mut self.pieces[piece.idx()]
    }

    #[must_use]
    fn colored_pieces(&self, piece: Piece) -> Bitboard {
        self.pieces[piece.piece_type().idx()] & self.colors[piece.color().idx()]
    }

    #[must_use]
    fn color_at(&self, sq: Square) -> Color {
        debug_assert_ne!(sq, Square::NONE);

        if self.black_occupancy().get(sq) {
            Color::BLACK
        } else if self.white_occupancy().get(sq) {
            Color::WHITE
        } else {
            Color::NONE
        }
    }

    #[must_use]
    fn piece_type_at(&self, sq: Square) -> PieceType {
        debug_assert_ne!(sq, Square::NONE);

        for piece in PieceType::all() {
            if self.pieces(piece).get(sq) {
                return piece;
            }
        }

        PieceType::NONE
    }

    #[must_use]
    fn piece_at(&self, sq: Square) -> Piece {
        debug_assert_ne!(sq, Square::NONE);

        let color = self.color_at(sq);

        if color == Color::NONE {
            return Piece::NONE;
        }

        for piece in PieceType::all() {
            if self.pieces(piece).get(sq) {
                return piece.colored(color);
            }
        }

        unreachable!();
    }

    fn king_square(&self, c: Color) -> Square {
        self.colored_pieces(PieceType::KING.colored(c))
            .lowest_square()
    }

    fn set_piece(&mut self, sq: Square, piece: Piece) {
        debug_assert_ne!(sq, Square::NONE);
        debug_assert_ne!(piece, Piece::NONE);

        let mask = sq.bit();

        *self.pieces_mut(piece.piece_type()) |= mask;
        *self.color_occupancy_mut(piece.color()) |= mask;
    }

    fn move_piece<const UPDATE_KEY: bool>(&mut self, src: Square, dst: Square, piece: Piece) {
        debug_assert_ne!(src, Square::NONE);
        debug_assert_ne!(dst, Square::NONE);

        debug_assert_ne!(piece, Piece::NONE);

        let mask = src.bit() ^ dst.bit();

        *self.pieces_mut(piece.piece_type()) ^= mask;
        *self.color_occupancy_mut(piece.color()) ^= mask;

        if UPDATE_KEY {
            self.key ^= keys::piece_square(piece, src);
            self.key ^= keys::piece_square(piece, dst);
        }
    }

    fn move_and_change_piece<const UPDATE_KEY: bool>(
        &mut self,
        src: Square,
        dst: Square,
        src_piece: Piece,
        dst_piece: Piece,
    ) {
        debug_assert_ne!(src, Square::NONE);
        debug_assert_ne!(dst, Square::NONE);
        debug_assert_ne!(src, dst);

        debug_assert_ne!(src_piece, Piece::NONE);
        debug_assert_ne!(dst_piece, Piece::NONE);
        debug_assert_ne!(src_piece, dst_piece);
        debug_assert_eq!(src_piece.color(), dst_piece.color());

        debug_assert_eq!(self.piece_at(src), src_piece);

        let src_mask = src.bit();
        let dst_mask = dst.bit();

        *self.pieces_mut(src_piece.piece_type()) ^= src_mask;
        *self.pieces_mut(dst_piece.piece_type()) ^= dst_mask;

        *self.color_occupancy_mut(src_piece.color()) ^= src_mask ^ dst_mask;

        if UPDATE_KEY {
            self.key ^= keys::piece_square(src_piece, src);
            self.key ^= keys::piece_square(dst_piece, dst);
        }
    }

    fn remove_piece<const UPDATE_KEY: bool>(&mut self, sq: Square, piece: Piece) {
        debug_assert_ne!(sq, Square::NONE);
        debug_assert_ne!(piece, Piece::NONE);

        let mask = sq.bit();

        *self.pieces_mut(piece.piece_type()) ^= mask;
        *self.color_occupancy_mut(piece.color()) ^= mask;

        if UPDATE_KEY {
            self.key ^= keys::piece_square(piece, sq);
        }
    }
}

impl Default for BoardState {
    #[must_use]
    fn default() -> Self {
        Self {
            colors: [Bitboard::EMPTY; 2],
            pieces: [Bitboard::EMPTY; 6],
            checkers: Bitboard::EMPTY,
            diag_pin_mask: Bitboard::EMPTY,
            ortho_pin_mask: Bitboard::EMPTY,
            key: 0,
            castling: CastlingRooks::empty(),
            halfmove: 0,
            en_passant: Square::NONE,
        }
    }
}

pub struct Position {
    black_to_move: bool,
    fullmove: u32,
    states: Vec<BoardState>,
    keys: Vec<u64>,
}

#[derive(Debug)]
pub enum FenError {
    NotEnoughParts,
    NotEnoughRanks,
    TooManyRanks,
    NotEnoughFiles(u32),
    TooManyFiles(u32),
    InvalidChar(char),
    InvalidKings,
    InvalidStm,
    InvalidCastling,
    InvalidEnPassant,
    InvalidHalfmove,
    InvalidFullmove,
}

impl Display for FenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FenError::NotEnoughParts => write!(f, "Incomplete FEN"),
            FenError::NotEnoughRanks => write!(f, "Not enough ranks in FEN"),
            FenError::TooManyRanks => write!(f, "Too many ranks in FEN"),
            FenError::NotEnoughFiles(rank) => write!(f, "Not enough files in rank {}", rank + 1),
            FenError::TooManyFiles(rank) => write!(f, "Too many files in rank {}", rank + 1),
            FenError::InvalidChar(c) => write!(f, "Invalid character '{}' in FEN", c),
            FenError::InvalidKings => {
                write!(f, "Invalid kings in FEN (each side must have exactly 1)")
            }
            FenError::InvalidStm => write!(f, "Invalid side to move in FEN"),
            FenError::InvalidCastling => write!(f, "Invalid castling rights in FEN"),
            FenError::InvalidEnPassant => write!(f, "Invalid en passant square in FEN"),
            FenError::InvalidHalfmove => write!(f, "Invalid halfmove clock in FEN"),
            FenError::InvalidFullmove => write!(f, "Invalid fullmove number in FEN"),
        }
    }
}

pub enum MoveStrError {
    InvalidSrc(SquareStrError),
    InvalidDst(SquareStrError),
    WrongSize,
    InvalidPromo,
    NoMovingPiece,
}

impl Display for MoveStrError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MoveStrError::InvalidSrc(str_err) => write!(f, "invalid from-square: {}", str_err),
            MoveStrError::InvalidDst(str_err) => write!(f, "invalid to-square: {}", str_err),
            MoveStrError::WrongSize => write!(f, "wrong size"),
            MoveStrError::InvalidPromo => write!(f, "invalid promo piece"),
            MoveStrError::NoMovingPiece => write!(f, "no moving piece"),
        }
    }
}

#[allow(unused)]
impl Position {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            black_to_move: false,
            fullmove: 0,
            states: Vec::with_capacity(256),
            keys: Vec::with_capacity(512),
        }
    }

    #[must_use]
    pub fn startpos() -> Self {
        let mut result = Self::empty();
        result.reset_to_startpos();
        result
    }

    pub fn from_fen(fen: &str) -> Result<Self, FenError> {
        let mut result = Self::empty();
        result.reset_from_fen(fen)?;
        Ok(result)
    }

    #[must_use]
    fn curr_state(&self) -> &BoardState {
        self.states.last().unwrap()
    }

    #[must_use]
    fn curr_state_mut(&mut self) -> &mut BoardState {
        self.states.last_mut().unwrap()
    }

    #[allow(clippy::unreadable_literal)]
    pub fn reset_to_startpos(&mut self) {
        self.states.clear();
        self.states.push(BoardState {
            colors: [
                Bitboard::from_raw(0xFFFF000000000000),
                Bitboard::from_raw(0x000000000000FFFF),
            ],
            pieces: [
                Bitboard::from_raw(0x00FF00000000FF00),
                Bitboard::from_raw(0x4200000000000042),
                Bitboard::from_raw(0x2400000000000024),
                Bitboard::from_raw(0x8100000000000081),
                Bitboard::from_raw(0x0800000000000008),
                Bitboard::from_raw(0x1000000000000010),
            ],
            checkers: Bitboard::EMPTY,
            diag_pin_mask: Bitboard::EMPTY,
            ortho_pin_mask: Bitboard::EMPTY,
            key: 0,
            castling: CastlingRooks::startpos(),
            halfmove: 0,
            en_passant: Square::NONE,
        });

        self.black_to_move = false;
        self.fullmove = 1;

        self.regen_curr_key();
        self.update_checkers_and_pins();
    }

    #[allow(clippy::manual_range_contains)]
    #[allow(clippy::comparison_chain)]
    pub fn reset_from_fen_parts(&mut self, parts: &[&str]) -> Result<(), FenError> {
        if parts.len() < 6 {
            return Err(FenError::NotEnoughParts);
        }

        let ranks: Vec<&str> = parts[0].split('/').collect();

        if ranks.len() < 8 {
            return Err(FenError::NotEnoughRanks);
        } else if ranks.len() > 8 {
            return Err(FenError::TooManyRanks);
        }

        let mut state = BoardState::default();

        for (rank_idx, rank) in ranks.iter().enumerate() {
            let mut file_idx: u32 = 0;

            for c in rank.chars() {
                if file_idx >= 8 {
                    return Err(FenError::TooManyFiles(rank_idx as u32));
                }

                if let Some(empty_squares) = c.to_digit(10) {
                    file_idx += empty_squares;
                } else {
                    let sq = Square::from_coords(rank_idx as u32, file_idx).flip_vertical();

                    if let Some(piece) = Piece::from_char(c) {
                        state.set_piece(sq, piece);
                        file_idx += 1;
                    } else {
                        return Err(FenError::InvalidChar(c));
                    }
                }
            }

            if file_idx > 8 {
                return Err(FenError::TooManyFiles(rank_idx as u32));
            } else if file_idx < 8 {
                return Err(FenError::NotEnoughFiles(rank_idx as u32));
            }
        }

        if state.colored_pieces(Piece::BLACK_KING).popcount() != 1
            || state.colored_pieces(Piece::WHITE_KING).popcount() != 1
        {
            return Err(FenError::InvalidKings);
        }

        if parts[1].len() != 1 {
            return Err(FenError::InvalidStm);
        }

        let black_to_move = if let Some(stm) = Color::from_char(parts[1].chars().nth(0).unwrap()) {
            stm == Color::BLACK
        } else {
            return Err(FenError::InvalidStm);
        };

        if parts[2].len() > 4 {
            return Err(FenError::InvalidCastling);
        }

        let black_king = state.colored_pieces(Piece::BLACK_KING).lowest_square();
        let white_king = state.colored_pieces(Piece::WHITE_KING).lowest_square();

        let mut parse_castling = |flag: char| -> Result<(), FenError> {
            let black = flag.is_lowercase();

            let (king, rank, rook) = if black {
                (black_king, 7u32, Piece::BLACK_ROOK)
            } else {
                (white_king, 0u32, Piece::WHITE_ROOK)
            };

            if king.rank() != rank {
                return Err(FenError::InvalidCastling);
            }

            let (file, short) = if "KQkq".contains(flag) {
                let short = flag.to_ascii_lowercase() == 'k';

                let (mut candidate, end, step) = if short {
                    (king.file() as i32 + 1, 8i32, 1i32)
                } else {
                    (king.file() as i32 - 1, -1i32, -1i32)
                };

                if candidate == end {
                    return Err(FenError::InvalidCastling);
                }

                let mut file = None;

                while candidate != end {
                    if state.piece_at(Square::from_coords(rank, candidate as u32)) == rook {
                        file = Some(candidate as u32);
                    }
                    candidate += step;
                }

                if let Some(file) = file {
                    (file, short)
                } else {
                    return Err(FenError::InvalidCastling);
                }
            } else if (flag >= 'a' && flag <= 'h') || (flag >= 'A' && flag <= 'H') {
                let file = flag.to_ascii_lowercase() as u32 - 'a' as u32;
                (file, file > king.file())
            } else {
                return Err(FenError::InvalidCastling);
            };

            if king.file() == file {
                return Err(FenError::InvalidCastling);
            }

            let sq = Square::from_coords(rank, file);

            let rooks = if black {
                state.castling.black_mut()
            } else {
                state.castling.white_mut()
            };

            if short {
                rooks.short = sq;
            } else {
                rooks.long = sq;
            }

            Ok(())
        };

        if parts[2] != "-" {
            for flag in parts[2].chars() {
                parse_castling(flag)?;
            }
        }

        if parts[3] != "-" {
            if let Ok(sq) = Square::from_str(parts[3]) {
                let opp = if black_to_move {
                    Color::WHITE
                } else {
                    Color::BLACK
                };
                //TODO this does not account for pinned pawns
                if !(attacks::pawn_attacks(opp, sq)
                    & state.colored_pieces(PieceType::PAWN.colored(opp.flip())))
                .is_empty()
                {
                    state.en_passant = sq;
                }
            } else {
                return Err(FenError::InvalidEnPassant);
            }
        }

        if let Ok(halfmove) = parts[4].parse::<u16>() {
            state.halfmove = halfmove;
        } else {
            return Err(FenError::InvalidHalfmove);
        }

        let Ok(fullmove) = parts[5].parse::<u32>() else {
            return Err(FenError::InvalidFullmove);
        };

        self.black_to_move = black_to_move;
        self.fullmove = fullmove;

        self.states.clear();
        self.states.push(state);

        self.keys.clear();

        self.regen_curr_key();
        self.update_checkers_and_pins();

        Ok(())
    }

    pub fn reset_from_fen(&mut self, fen: &str) -> Result<(), FenError> {
        let parts: Vec<&str> = fen.split_whitespace().collect();
        self.reset_from_fen_parts(parts.as_slice())
    }

    fn regen_curr_key(&mut self) {
        let black_to_move = self.black_to_move;
        let state = self.curr_state_mut();

        state.key = 0;

        if black_to_move {
            state.key ^= keys::stm();
        }

        for piece in Piece::all() {
            for sq in state.colored_pieces(piece) {
                state.key ^= keys::piece_square(piece, sq);
            }
        }

        state.key ^= keys::castling(state.castling);
        state.key ^= keys::en_passant(state.en_passant);
    }

    fn update_checkers_and_pins(&mut self) {
        let stm = self.side_to_move();
        let nstm = stm.flip();

        let king = self.king_square(stm);
        let checkers = self.attackers_to(king, nstm);

        let state = self.curr_state_mut();

        state.checkers = checkers;

        let us = state.color_occupancy(stm);
        let them = state.color_occupancy(nstm);

        let their_queens = state.colored_pieces(PieceType::QUEEN.colored(nstm));

        let their_diag = their_queens | state.colored_pieces(PieceType::BISHOP.colored(nstm));
        let their_ortho = their_queens | state.colored_pieces(PieceType::ROOK.colored(nstm));

        let potential_diag_pinners = their_diag & attacks::bishop_attacks(king, them);
        let potential_ortho_pinners = their_ortho & attacks::rook_attacks(king, them);

        state.diag_pin_mask = Bitboard::EMPTY;
        state.ortho_pin_mask = Bitboard::EMPTY;

        for pinner in potential_diag_pinners {
            let potentially_pinned = ray_between(king, pinner).with(pinner);
            if (potentially_pinned & us).contains_one() {
                state.diag_pin_mask |= potentially_pinned;
            }
        }

        for pinner in potential_ortho_pinners {
            let potentially_pinned = ray_between(king, pinner).with(pinner);
            if (potentially_pinned & us).contains_one() {
                state.ortho_pin_mask |= potentially_pinned;
            }
        }
    }

    pub fn move_from_str(&self, str: &str, chess960: bool) -> Result<ChessMove, MoveStrError> {
        if str.len() < 4 || str.len() > 5 {
            return Err(MoveStrError::WrongSize);
        }

        let src = Square::from_str(&str[0..2]).map_err(MoveStrError::InvalidSrc)?;
        let dst = Square::from_str(&str[2..4]).map_err(MoveStrError::InvalidDst)?;

        if str.len() == 5 {
            if let Some(promo) = PieceType::from_char(str.chars().nth(4).unwrap()) {
                if promo != PieceType::PAWN && promo != PieceType::KING {
                    Ok(ChessMove::promotion(src, dst, promo))
                } else {
                    Err(MoveStrError::InvalidPromo)
                }
            } else {
                Err(MoveStrError::InvalidPromo)
            }
        } else {
            let moving = self.piece_type_at(src);

            match moving {
                PieceType::NONE => return Err(MoveStrError::NoMovingPiece),
                PieceType::KING => {
                    if chess960 {
                        if self.piece_at(dst) == PieceType::ROOK.colored(self.color_at(src)) {
                            return Ok(ChessMove::castling(src, dst));
                        }
                    } else if src.file().abs_diff(dst.file()) == 2 {
                        let rook_file = if src.file() < dst.file() { 7 } else { 0 };
                        return Ok(ChessMove::castling(
                            src,
                            Square::from_coords(src.rank(), rook_file),
                        ));
                    }
                }
                PieceType::PAWN => {
                    if dst == self.curr_state().en_passant {
                        return Ok(ChessMove::en_passant(src, dst));
                    }
                }
                _ => {}
            }

            Ok(ChessMove::normal(src, dst))
        }
    }

    pub fn apply_move<const HISTORY: bool, const UPDATE_KEY: bool>(&mut self, mv: ChessMove) {
        let stm = self.side_to_move();
        let nstm = stm.flip();

        self.black_to_move = !self.black_to_move;

        let mut new_state = self.curr_state().clone();

        if UPDATE_KEY {
            self.keys.push(new_state.key);
            new_state.key ^= keys::stm();
        }

        if stm == Color::BLACK {
            self.fullmove += 1;
        }

        let src = mv.src();
        let dst = mv.dst();
        debug_assert_ne!(src, dst);

        let moving = new_state.piece_at(src);
        debug_assert_ne!(moving, Piece::NONE);

        let (capture_sq, captured) = match mv.move_type() {
            MoveType::Normal | MoveType::Promotion => {
                let captured = new_state.piece_at(dst);

                if mv.move_type() == MoveType::Promotion {
                    new_state.move_and_change_piece::<UPDATE_KEY>(
                        src,
                        dst,
                        moving,
                        mv.promo().colored(stm),
                    );
                } else {
                    new_state.move_piece::<UPDATE_KEY>(src, dst, moving);
                }

                (dst, captured)
            }
            MoveType::Castling => {
                let king_dst =
                    Square::from_coords(src.rank(), if mv.is_short_castling() { 6 } else { 2 });
                let rook_dst =
                    Square::from_coords(src.rank(), if mv.is_short_castling() { 5 } else { 3 });

                let rook = PieceType::ROOK.colored(stm);
                debug_assert_eq!(rook, new_state.piece_at(dst));

                new_state.move_piece::<UPDATE_KEY>(src, king_dst, moving);
                new_state.move_piece::<UPDATE_KEY>(dst, rook_dst, rook);

                (Square::NONE, Piece::NONE)
            }
            MoveType::EnPassant => {
                debug_assert_eq!(dst, new_state.en_passant);
                new_state.move_piece::<UPDATE_KEY>(src, dst, PieceType::PAWN.colored(stm));
                (
                    Square::from_raw(if stm == Color::BLACK {
                        dst.raw() + 8
                    } else {
                        dst.raw() - 8
                    }),
                    PieceType::PAWN.colored(nstm),
                )
            }
        };

        let prev_castling = new_state.castling;
        let prev_ep = new_state.en_passant;

        if captured != Piece::NONE {
            debug_assert_ne!(captured.color(), moving.color());
            debug_assert_ne!(captured.piece_type(), PieceType::KING);

            new_state.remove_piece::<UPDATE_KEY>(capture_sq, captured);

            if captured.piece_type() == PieceType::ROOK {
                new_state.castling.color_mut(nstm).unset(capture_sq);
            }
        }

        debug_assert_eq!(
            new_state.occupancy(),
            new_state.pieces(PieceType::PAWN)
                | new_state.pieces(PieceType::KNIGHT)
                | new_state.pieces(PieceType::BISHOP)
                | new_state.pieces(PieceType::ROOK)
                | new_state.pieces(PieceType::QUEEN)
                | new_state.pieces(PieceType::KING)
        );

        new_state.en_passant = Square::NONE;

        if moving.piece_type() == PieceType::PAWN && src.rank().abs_diff(dst.rank()) == 2 {
            debug_assert_eq!(src.rank(), if stm == Color::BLACK { 6 } else { 1 });
            debug_assert_eq!(src.file(), dst.file());

            let dst_bit = dst.bit();
            //TODO does not consider pinned pawns
            if !((dst_bit.shift_left() | dst_bit.shift_right())
                & new_state.colored_pieces(PieceType::PAWN.colored(nstm)))
            .is_empty()
            {
                new_state.en_passant = Square::from_raw((src.raw() + dst.raw()) / 2);
            }
        }

        if moving.piece_type() == PieceType::KING {
            new_state.castling.color_mut(stm).clear();
        } else if moving.piece_type() == PieceType::ROOK {
            new_state.castling.color_mut(stm).unset(src);
        }

        if captured == Piece::NONE && moving.piece_type() != PieceType::PAWN {
            new_state.halfmove += 1;
        } else {
            new_state.halfmove = 0;
        }

        if UPDATE_KEY {
            if new_state.castling != prev_castling {
                new_state.key ^= keys::castling(prev_castling);
                new_state.key ^= keys::castling(new_state.castling);
            }
            if new_state.en_passant != prev_ep {
                new_state.key ^= keys::en_passant(prev_ep);
                new_state.key ^= keys::en_passant(new_state.en_passant);
            }
        }

        if HISTORY {
            self.states.push(new_state);
        } else {
            *self.curr_state_mut() = new_state;
        }

        self.update_checkers_and_pins();
    }

    pub fn pop_move<const UPDATE_KEY: bool>(&mut self) {
        self.states.pop();

        if UPDATE_KEY {
            self.keys.pop();
        }

        self.black_to_move = !self.black_to_move;

        if self.black_to_move {
            self.fullmove -= 1;
        }
    }

    #[must_use]
    pub fn side_to_move(&self) -> Color {
        if self.black_to_move {
            Color::BLACK
        } else {
            Color::WHITE
        }
    }

    #[must_use]
    pub fn occupancy(&self) -> Bitboard {
        self.curr_state().occupancy()
    }

    #[must_use]
    pub fn black_occupancy(&self) -> Bitboard {
        self.curr_state().black_occupancy()
    }

    #[must_use]
    pub fn white_occupancy(&self) -> Bitboard {
        self.curr_state().white_occupancy()
    }

    #[must_use]
    pub fn color_occupancy(&self, color: Color) -> Bitboard {
        self.curr_state().color_occupancy(color)
    }

    #[must_use]
    pub fn pieces(&self, piece: PieceType) -> Bitboard {
        self.curr_state().pieces(piece)
    }

    #[must_use]
    pub fn colored_pieces(&self, piece: Piece) -> Bitboard {
        self.curr_state().colored_pieces(piece)
    }

    #[must_use]
    pub fn color_at(&self, sq: Square) -> Color {
        self.curr_state().color_at(sq)
    }

    #[must_use]
    pub fn piece_type_at(&self, sq: Square) -> PieceType {
        self.curr_state().piece_type_at(sq)
    }

    #[must_use]
    pub fn piece_at(&self, sq: Square) -> Piece {
        self.curr_state().piece_at(sq)
    }

    #[must_use]
    pub fn key(&self) -> u64 {
        self.curr_state().key
    }

    #[must_use]
    pub fn halfmoves(&self) -> u16 {
        self.curr_state().halfmove
    }

    #[must_use]
    pub fn fullmoves(&self) -> u32 {
        self.fullmove
    }

    #[must_use]
    pub fn en_passant(&self) -> Square {
        self.curr_state().en_passant
    }

    #[must_use]
    pub fn king_square(&self, c: Color) -> Square {
        self.curr_state().king_square(c)
    }

    #[must_use]
    pub fn diag_pin_mask(&self) -> Bitboard {
        self.curr_state().diag_pin_mask
    }

    #[must_use]
    pub fn ortho_pin_mask(&self) -> Bitboard {
        self.curr_state().ortho_pin_mask
    }

    #[must_use]
    pub fn checkers(&self) -> Bitboard {
        self.curr_state().checkers
    }

    #[must_use]
    pub fn castling(&self) -> CastlingRooks {
        self.curr_state().castling
    }

    #[must_use]
    pub fn is_attacked_occ(&self, sq: Square, occupancy: Bitboard) -> bool {
        let stm = self.side_to_move();
        let nstm = stm.flip();

        let state = self.curr_state();

        let knights = state.colored_pieces(PieceType::KNIGHT.colored(nstm));
        if !(attacks::knight_attacks(sq) & knights).is_empty() {
            return true;
        }

        let pawns = state.colored_pieces(PieceType::PAWN.colored(nstm));
        if !(attacks::pawn_attacks(stm, sq) & pawns).is_empty() {
            return true;
        }

        let kings = state.colored_pieces(PieceType::KING.colored(nstm));
        if !(attacks::king_attacks(sq) & kings).is_empty() {
            return true;
        }

        let queens = state.colored_pieces(PieceType::QUEEN.colored(nstm));

        let rooks = queens | state.colored_pieces(PieceType::ROOK.colored(nstm));
        if !(attacks::rook_attacks(sq, occupancy) & rooks).is_empty() {
            return true;
        }

        let bishops = queens | state.colored_pieces(PieceType::BISHOP.colored(nstm));
        if !(attacks::bishop_attacks(sq, occupancy) & bishops).is_empty() {
            return true;
        }

        false
    }

    #[must_use]
    pub fn is_attacked(&self, sq: Square) -> bool {
        self.is_attacked_occ(sq, self.occupancy())
    }

    #[must_use]
    pub fn any_attacked(&self, squares: Bitboard) -> bool {
        for sq in squares {
            if self.is_attacked(sq) {
                return true;
            }
        }
        false
    }

    #[must_use]
    pub fn attackers_to(&self, sq: Square, attacker: Color) -> Bitboard {
        let state = self.curr_state();
        let occ = self.occupancy();

        let mut attackers = Bitboard::EMPTY;

        let pawns = state.colored_pieces(PieceType::PAWN.colored(attacker));
        attackers |= pawns & attacks::pawn_attacks(attacker.flip(), sq);

        let knights = state.colored_pieces(PieceType::KNIGHT.colored(attacker));
        attackers |= knights & attacks::knight_attacks(sq);

        let kings = state.colored_pieces(PieceType::KING.colored(attacker));
        attackers |= kings & attacks::king_attacks(sq);

        let queens = state.colored_pieces(PieceType::QUEEN.colored(attacker));

        let bishops = queens | state.colored_pieces(PieceType::BISHOP.colored(attacker));
        attackers |= bishops & attacks::bishop_attacks(sq, occ);

        let rooks = queens | state.colored_pieces(PieceType::ROOK.colored(attacker));
        attackers |= rooks & attacks::rook_attacks(sq, occ);

        attackers
    }

    #[must_use]
    pub fn to_fen(&self, chess960: bool) -> String {
        let state = self.curr_state();

        let mut fen = String::new();

        for rank in (0u32..8).rev() {
            let mut file: u32 = 0;

            while file < 8 {
                let sq = Square::from_coords(rank, file);

                match state.piece_at(sq) {
                    Piece::NONE => {
                        let mut empty_squares: u32 = 1;

                        while file < 7
                            && state.piece_type_at(Square::from_coords(rank, file + 1))
                                == PieceType::NONE
                        {
                            file += 1;
                            empty_squares += 1;
                        }

                        fen += empty_squares.to_string().as_str();
                    }
                    piece => fen.push(piece.to_char()),
                }

                file += 1;
            }

            if rank > 0 {
                fen.push('/');
            }
        }

        fen.push(' ');
        fen.push(self.side_to_move().to_char());
        fen.push(' ');

        if state.castling == CastlingRooks::empty() {
            fen += "- ";
        } else if chess960 {
            if state.castling.white().short != Square::NONE {
                fen.push((b'A' + state.castling.white().short.file() as u8) as char);
            }
            if state.castling.white().long != Square::NONE {
                fen.push((b'A' + state.castling.white().long.file() as u8) as char);
            }
            if state.castling.black().short != Square::NONE {
                fen.push((b'a' + state.castling.black().short.file() as u8) as char);
            }
            if state.castling.black().long != Square::NONE {
                fen.push((b'a' + state.castling.black().long.file() as u8) as char);
            }
            fen.push(' ');
        } else {
            if state.castling.white().short != Square::NONE {
                fen.push('K');
            }
            if state.castling.white().long != Square::NONE {
                fen.push('Q');
            }
            if state.castling.black().short != Square::NONE {
                fen.push('k');
            }
            if state.castling.black().long != Square::NONE {
                fen.push('q');
            }
            fen.push(' ');
        }

        match state.en_passant {
            Square::NONE => fen.push('-'),
            sq => fen += sq.to_string().as_str(),
        };

        fen + format!(" {} {}", state.halfmove, self.fullmove).as_str()
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rank in (0u32..8).rev() {
            writeln!(f, " +---+---+---+---+---+---+---+---+")?;

            for file in 0u32..8 {
                let sq = Square::from_coords(rank, file);
                write!(f, " | {}", self.piece_at(sq).to_char())?;
            }

            writeln!(f, " | {}", rank + 1)?;
        }

        writeln!(f, " +---+---+---+---+---+---+---+---+")?;
        writeln!(f, "   a   b   c   d   e   f   g   h")?;
        writeln!(f)?;

        write!(
            f,
            "{} to move",
            if self.side_to_move() == Color::BLACK {
                "Black"
            } else {
                "White"
            }
        )
    }
}
