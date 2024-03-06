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
use crate::core::Square;
use crate::{array_init, attacks};

const BETWEEN_RAYS: [[Bitboard; 64]; 64] = array_init!(|src_idx, 64| {
    let src = Square::from_raw(src_idx as u8);
    let src_mask = src.bit();

    let rook_attacks = attacks::rook_attacks(src, Bitboard::EMPTY);
    let bishop_attacks = attacks::bishop_attacks(src, Bitboard::EMPTY);

    array_init!(|dst_idx, 64| {
        if src_idx == dst_idx {
            Bitboard::EMPTY
        } else {
            let dst = Square::from_raw(dst_idx as u8);
            let dst_mask = dst.bit();

            if rook_attacks.get(dst) {
                attacks::rook_attacks(src, dst_mask).and(attacks::rook_attacks(dst, src_mask))
            } else if bishop_attacks.get(dst) {
                attacks::bishop_attacks(src, dst_mask).and(attacks::bishop_attacks(dst, src_mask))
            } else {
                Bitboard::EMPTY
            }
        }
    })
});

const INTERSECTING_RAYS: [[Bitboard; 64]; 64] = array_init!(|src_idx, 64| {
    let src = Square::from_raw(src_idx as u8);
    let src_mask = src.bit();

    let rook_attacks = attacks::rook_attacks(src, Bitboard::EMPTY);
    let bishop_attacks = attacks::bishop_attacks(src, Bitboard::EMPTY);

    array_init!(|dst_idx, 64| {
        if src_idx == dst_idx {
            Bitboard::EMPTY
        } else {
            let dst = Square::from_raw(dst_idx as u8);
            let dst_mask = dst.bit();

            if rook_attacks.get(dst) {
                src_mask
                    .or(attacks::rook_attacks(src, Bitboard::EMPTY))
                    .and(dst_mask.or(attacks::rook_attacks(dst, Bitboard::EMPTY)))
            } else if bishop_attacks.get(dst) {
                src_mask
                    .or(attacks::bishop_attacks(src, Bitboard::EMPTY))
                    .and(dst_mask.or(attacks::bishop_attacks(dst, Bitboard::EMPTY)))
            } else {
                Bitboard::EMPTY
            }
        }
    })
});

pub const fn ray_between(a: Square, b: Square) -> Bitboard {
    BETWEEN_RAYS[a.idx()][b.idx()]
}

pub const fn ray_intersecting(a: Square, b: Square) -> Bitboard {
    INTERSECTING_RAYS[a.idx()][b.idx()]
}
