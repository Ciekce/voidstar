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

#![allow(unused)]

use crate::array_init;
use crate::bitboard::Bitboard;
use crate::core::{Color, Square};

// slider attack techniques from akimbo, cheers jw

const PAWN_ATTACKS: [[Bitboard; 64]; 2] = [
    array_init!(|sq_idx, 64| {
        // black
        let sq = Square::from_raw(sq_idx as u8);
        let bit = sq.bit();

        let mut attacks = Bitboard::EMPTY;

        attacks = attacks.or(bit.shift_down_left());
        attacks = attacks.or(bit.shift_down_right());

        attacks
    }),
    array_init!(|sq_idx, 64| {
        // white
        let sq = Square::from_raw(sq_idx as u8);
        let bit = sq.bit();

        let mut attacks = Bitboard::EMPTY;

        attacks = attacks.or(bit.shift_up_left());
        attacks = attacks.or(bit.shift_up_right());

        attacks
    }),
];

const KNIGHT_ATTACKS: [Bitboard; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    let mut attacks = Bitboard::EMPTY;

    attacks = attacks.or(bit.shift_up().shift_up_left());
    attacks = attacks.or(bit.shift_up().shift_up_right());
    attacks = attacks.or(bit.shift_left().shift_up_left());
    attacks = attacks.or(bit.shift_left().shift_down_left());
    attacks = attacks.or(bit.shift_right().shift_up_right());
    attacks = attacks.or(bit.shift_right().shift_down_right());
    attacks = attacks.or(bit.shift_down().shift_down_left());
    attacks = attacks.or(bit.shift_down().shift_down_right());

    attacks
});

const KING_ATTACKS: [Bitboard; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    let mut attacks = Bitboard::EMPTY;

    attacks = attacks.or(bit.shift_up());
    attacks = attacks.or(bit.shift_down());
    attacks = attacks.or(bit.shift_left());
    attacks = attacks.or(bit.shift_right());
    attacks = attacks.or(bit.shift_up_left());
    attacks = attacks.or(bit.shift_up_right());
    attacks = attacks.or(bit.shift_down_left());
    attacks = attacks.or(bit.shift_down_right());

    attacks
});

const DIAG: u64 = 0x8040_2010_0804_0201;

const fn diag_mask(i: u32) -> u64 {
    if i > 7 {
        DIAG >> (8 * (i - 7))
    } else {
        DIAG << (8 * (7 - i))
    }
}

const DIAG_ATTACKS: [u64; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    bit.raw() ^ diag_mask(7 - sq.rank() + sq.file())
});

const ANTI_DIAG_ATTACKS: [u64; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    bit.raw() ^ diag_mask(sq.rank() + sq.file()).swap_bytes()
});

const LEFT_ATTACKS: [Bitboard; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    Bitboard::from_raw(bit.raw() - 1).and(sq.rank_bits())
});

const RIGHT_ATTACKS: [Bitboard; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    let bit = sq.bit();

    bit.xor(LEFT_ATTACKS[sq_idx]).xor(sq.rank_bits())
});

const RANK_ATTACKS: [[Bitboard; 64]; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    array_init!(|i, 64| {
        let file = sq.file() as usize;
        let occ = Bitboard::from_raw((i as u64) << 1);

        let right = RIGHT_ATTACKS[file];
        let left = LEFT_ATTACKS[file];

        let right = right.xor(RIGHT_ATTACKS[right.and(occ).with(Square::H8).lowest_square().idx()]);
        let left = left.xor(LEFT_ATTACKS[left.and(occ).with(Square::A1).highest_square().idx()]);

        right.or(left).bit_shl(sq.rank() * 8)
    })
});

const FILE_ATTACKS: [[Bitboard; 64]; 64] = array_init!(|sq_idx, 64| {
    let sq = Square::from_raw(sq_idx as u8);
    array_init!(|i, 64| {
        Bitboard::from_raw(
            RANK_ATTACKS[sq.rank() as usize ^ 0x7][i]
                .raw()
                .wrapping_mul(DIAG),
        )
        .and(Bitboard::FILE_H)
        .bit_shr(sq.file() ^ 0x7)
    })
});

#[must_use]
#[inline(always)]
pub const fn black_pawn_attacks(sq: Square) -> Bitboard {
    PAWN_ATTACKS[0][sq.idx()]
}

#[must_use]
#[inline(always)]
pub const fn white_pawn_attacks(sq: Square) -> Bitboard {
    PAWN_ATTACKS[1][sq.idx()]
}

#[must_use]
#[inline(always)]
pub const fn pawn_attacks(c: Color, sq: Square) -> Bitboard {
    PAWN_ATTACKS[c.idx()][sq.idx()]
}

#[must_use]
#[inline(always)]
pub const fn knight_attacks(sq: Square) -> Bitboard {
    KNIGHT_ATTACKS[sq.idx()]
}

#[must_use]
#[inline(always)]
pub const fn bishop_attacks(sq: Square, occupancy: Bitboard) -> Bitboard {
    let diag = DIAG_ATTACKS[sq.idx()];
    let anti = ANTI_DIAG_ATTACKS[sq.idx()];

    let bit = sq.bit().raw();
    let flipped_bit = bit.swap_bytes();

    let mut diag_attacks = occupancy.raw() & diag;
    let mut diag_flipped = diag_attacks.swap_bytes();

    let mut anti_attacks = occupancy.raw() & anti;
    let mut anti_flipped = anti_attacks.swap_bytes();

    diag_attacks = diag_attacks.wrapping_sub(bit);
    anti_attacks = anti_attacks.wrapping_sub(bit);

    diag_flipped = diag_flipped.wrapping_sub(flipped_bit);
    anti_flipped = anti_flipped.wrapping_sub(flipped_bit);

    diag_attacks ^= diag_flipped.swap_bytes();
    anti_attacks ^= anti_flipped.swap_bytes();

    diag_attacks &= diag;
    anti_attacks &= anti;

    Bitboard::from_raw(diag_attacks | anti_attacks)
}

#[must_use]
#[inline(always)]
pub const fn rook_attacks(sq: Square, occupancy: Bitboard) -> Bitboard {
    let rank_attacks =
        RANK_ATTACKS[sq.idx()][(occupancy.raw() >> (sq.rank() * 8 + 1)) as usize & 0x3f];

    let flip = ((occupancy.raw() >> sq.file()) & Bitboard::FILE_A.raw()).wrapping_mul(DIAG);
    let file_attacks = FILE_ATTACKS[sq.idx()][(flip >> 57) as usize & 0x3f];

    rank_attacks.or(file_attacks)
}

#[must_use]
#[inline(always)]
pub const fn king_attacks(sq: Square) -> Bitboard {
    KING_ATTACKS[sq.idx()]
}
