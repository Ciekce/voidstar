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

use crate::core::*;
use crate::position::CastlingRooks;
use crate::rng;

const PIECE_SQUARE_SIZE: usize = Piece::N_PIECES * Square::N_SQUARES;
const STM_SIZE: usize = 1;
const CASTLING_SIZE: usize = 16;
const EN_PASSANT_SIZE: usize = 8;

const TOTAL_SIZE: usize = PIECE_SQUARE_SIZE + STM_SIZE + CASTLING_SIZE + EN_PASSANT_SIZE;

const PIECE_SQUARE_OFFSET: usize = 0;
const STM_OFFSET: usize = PIECE_SQUARE_OFFSET + PIECE_SQUARE_SIZE;
const CASTLING_OFFSET: usize = STM_OFFSET + STM_SIZE;
const EN_PASSANT_OFFSET: usize = CASTLING_OFFSET + CASTLING_SIZE;

#[allow(clippy::unreadable_literal)]
const KEYS: [u64; TOTAL_SIZE] = rng::fill_u64_array(0x3dd6ea8df208fc53);

#[must_use]
#[inline(always)]
pub fn piece_square(piece: Piece, sq: Square) -> u64 {
    debug_assert!(piece != Piece::NONE);
    debug_assert!(sq != Square::NONE);

    KEYS[PIECE_SQUARE_OFFSET + sq.idx() * Piece::N_PIECES + piece.idx()]
}

#[must_use]
#[inline(always)]
pub fn stm() -> u64 {
    KEYS[STM_OFFSET]
}

#[must_use]
#[inline(always)]
pub fn castling(rooks: CastlingRooks) -> u64 {
    const BLACK_SHORT_SHIFT: i32 = 0;
    const BLACK_LONG_SHIFT: i32 = 1;
    const WHITE_SHORT_SHIFT: i32 = 2;
    const WHITE_LONG_SHIFT: i32 = 3;

    let mut flags = 0usize;

    flags |= usize::from(rooks.black().short != Square::NONE) << BLACK_SHORT_SHIFT;
    flags |= usize::from(rooks.black().long != Square::NONE) << BLACK_LONG_SHIFT;
    flags |= usize::from(rooks.white().short != Square::NONE) << WHITE_SHORT_SHIFT;
    flags |= usize::from(rooks.white().long != Square::NONE) << WHITE_LONG_SHIFT;

    KEYS[CASTLING_OFFSET + flags]
}

#[must_use]
#[inline(always)]
pub fn en_passant(sq: Square) -> u64 {
    if sq == Square::NONE {
        0
    } else {
        KEYS[EN_PASSANT_OFFSET + sq.file() as usize]
    }
}
