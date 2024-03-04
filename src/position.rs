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
use crate::chess_move::ChessMove;
use crate::core::*;
use crate::keys;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub struct RookPair {
    pub black: Square,
    pub white: Square,
}

#[derive(Debug, Copy, Clone)]
pub struct CastlingRooks {
    pub short: RookPair,
    pub long: RookPair,
}

impl CastlingRooks {
    fn empty() -> Self {
        Self {
            short: RookPair {
                black: Square::NONE,
                white: Square::NONE,
            },
            long: RookPair {
                black: Square::NONE,
                white: Square::NONE,
            },
        }
    }

    fn startpos() -> Self {
        Self {
            short: RookPair {
                black: Square::H8,
                white: Square::H1,
            },
            long: RookPair {
                black: Square::A8,
                white: Square::A1,
            },
        }
    }
}

#[derive(Debug, Clone)]
struct BoardState {
    colors: [Bitboard; 2],
    pieces: [Bitboard; 6],
    key: u64,
    castling: CastlingRooks,
    last_move: ChessMove,
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

    fn set_piece(&mut self, sq: Square, piece: Piece) {
        debug_assert_ne!(sq, Square::NONE);
        debug_assert_ne!(piece, Piece::NONE);

        let mask = sq.bit();

        *self.pieces_mut(piece.piece_type()) |= mask;
        *self.color_occupancy_mut(piece.color()) |= mask;
    }

    fn move_piece(&mut self, src: Square, dst: Square, piece: Piece) {
        debug_assert_ne!(src, Square::NONE);
        debug_assert_ne!(dst, Square::NONE);
        debug_assert_ne!(src, dst);

        debug_assert_ne!(piece, Piece::NONE);
        debug_assert_eq!(self.piece_at(src), piece);

        let mask = src.bit() ^ dst.bit();

        *self.pieces_mut(piece.piece_type()) ^= mask;
        *self.color_occupancy_mut(piece.color()) ^= mask;
    }

    fn move_and_change_piece(
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
    }

    fn remove_piece(&mut self, sq: Square, piece: Piece) {
        debug_assert_ne!(sq, Square::NONE);
        debug_assert_ne!(piece, Piece::NONE);

        debug_assert_eq!(self.piece_at(sq), piece);

        let mask = sq.bit();

        *self.pieces_mut(piece.piece_type()) ^= mask;
        *self.color_occupancy_mut(piece.color()) ^= mask;
    }
}

impl Default for BoardState {
    #[must_use]
    fn default() -> Self {
        Self {
            colors: [Bitboard::EMPTY; 2],
            pieces: [Bitboard::EMPTY; 6],
            key: 0,
            castling: CastlingRooks::startpos(),
            last_move: ChessMove::NULL,
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
            FenError::InvalidStm => write!(f, "Invalid side to move in FEN"),
            FenError::InvalidCastling => write!(f, "Invalid castling rights in FEN"),
            FenError::InvalidEnPassant => write!(f, "Invalid en passant square in FEN"),
            FenError::InvalidHalfmove => write!(f, "Invalid halfmove clock in FEN"),
            FenError::InvalidFullmove => write!(f, "Invalid fullmove number in FEN"),
        }
    }
}

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
            key: 0,
            castling: CastlingRooks::startpos(),
            last_move: ChessMove::NULL,
            halfmove: 0,
            en_passant: Square::NONE,
        });

        self.black_to_move = false;
        self.fullmove = 1;

        self.regen_curr_key();
    }

    pub fn reset_from_fen_parts(&mut self, parts: &[&str]) -> Result<(), FenError> {
        if parts.len() < 6 {
            return Err(FenError::NotEnoughParts);
        }

        let ranks: Vec<&str> = parts[0].split('/').collect();

        if ranks.len() < 7 {
            return Err(FenError::NotEnoughRanks);
        } else if ranks.len() > 7 {
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

            if file_idx > 7 {
                return Err(FenError::TooManyFiles(rank_idx as u32));
            } else if file_idx < 7 {
                return Err(FenError::NotEnoughFiles(rank_idx as u32));
            }
        }

        if parts[1].len() != 1 {
            return Err(FenError::InvalidStm);
        }

        let black_to_move = if let Some(stm) = Color::from_char(parts[1].chars().nth(0).unwrap()) {
            stm == Color::BLACK
        } else {
            return Err(FenError::InvalidStm);
        };

        if let Ok(halfmove) = parts[2].parse::<u16>() {
            state.halfmove = halfmove;
        } else {
            return Err(FenError::InvalidHalfmove);
        }

        let fullmove = if let Ok(fullmove) = parts[3].parse::<u32>() {
            fullmove
        } else {
            return Err(FenError::InvalidFullmove);
        };

        self.black_to_move = black_to_move;
        self.fullmove = fullmove;

        self.states.clear();
        self.states.push(state);

        self.keys.clear();

        self.regen_curr_key();

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

        if state.en_passant != Square::NONE {
            state.key ^= keys::en_passant(state.en_passant.file())
        }
    }

    pub fn king_square(&self, c: Color) -> Square {
        let state = self.curr_state();
        state
            .colored_pieces(PieceType::KING.colored(c))
            .lowest_square()
    }
}
