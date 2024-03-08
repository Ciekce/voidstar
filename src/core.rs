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
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Square(u8);

#[allow(unused)]
impl Square {
    pub const A1: Self = Self(0);
    pub const B1: Self = Self(1);
    pub const C1: Self = Self(2);
    pub const D1: Self = Self(3);
    pub const E1: Self = Self(4);
    pub const F1: Self = Self(5);
    pub const G1: Self = Self(6);
    pub const H1: Self = Self(7);
    pub const A2: Self = Self(8);
    pub const B2: Self = Self(9);
    pub const C2: Self = Self(10);
    pub const D2: Self = Self(11);
    pub const E2: Self = Self(12);
    pub const F2: Self = Self(13);
    pub const G2: Self = Self(14);
    pub const H2: Self = Self(15);
    pub const A3: Self = Self(16);
    pub const B3: Self = Self(17);
    pub const C3: Self = Self(18);
    pub const D3: Self = Self(19);
    pub const E3: Self = Self(20);
    pub const F3: Self = Self(21);
    pub const G3: Self = Self(22);
    pub const H3: Self = Self(23);
    pub const A4: Self = Self(24);
    pub const B4: Self = Self(25);
    pub const C4: Self = Self(26);
    pub const D4: Self = Self(27);
    pub const E4: Self = Self(28);
    pub const F4: Self = Self(29);
    pub const G4: Self = Self(30);
    pub const H4: Self = Self(31);
    pub const A5: Self = Self(32);
    pub const B5: Self = Self(33);
    pub const C5: Self = Self(34);
    pub const D5: Self = Self(35);
    pub const E5: Self = Self(36);
    pub const F5: Self = Self(37);
    pub const G5: Self = Self(38);
    pub const H5: Self = Self(39);
    pub const A6: Self = Self(40);
    pub const B6: Self = Self(41);
    pub const C6: Self = Self(42);
    pub const D6: Self = Self(43);
    pub const E6: Self = Self(44);
    pub const F6: Self = Self(45);
    pub const G6: Self = Self(46);
    pub const H6: Self = Self(47);
    pub const A7: Self = Self(48);
    pub const B7: Self = Self(49);
    pub const C7: Self = Self(50);
    pub const D7: Self = Self(51);
    pub const E7: Self = Self(52);
    pub const F7: Self = Self(53);
    pub const G7: Self = Self(54);
    pub const H7: Self = Self(55);
    pub const A8: Self = Self(56);
    pub const B8: Self = Self(57);
    pub const C8: Self = Self(58);
    pub const D8: Self = Self(59);
    pub const E8: Self = Self(60);
    pub const F8: Self = Self(61);
    pub const G8: Self = Self(62);
    pub const H8: Self = Self(63);

    pub const NONE: Self = Self(64);

    pub const N_SQUARES: usize = Self::NONE.0 as usize;

    #[must_use]
    pub const fn from_raw(value: u8) -> Self {
        debug_assert!(value <= 64);
        Self(value)
    }

    #[must_use]
    pub const fn from_coords(rank: u32, file: u32) -> Self {
        debug_assert!(rank < 8);
        debug_assert!(file < 8);
        Self((rank * 8 + file) as u8)
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.0
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.0 as usize
    }

    #[must_use]
    pub const fn bit(self) -> Bitboard {
        Bitboard::from_raw(1 << self.idx())
    }

    #[must_use]
    pub const fn rank_bits(self) -> Bitboard {
        Bitboard::RANK_1.bit_shl(self.rank() * 8)
    }

    #[must_use]
    pub const fn file_bits(self) -> Bitboard {
        Bitboard::FILE_A.bit_shl(self.file())
    }

    #[must_use]
    pub const fn rank(self) -> u32 {
        self.0 as u32 / 8
    }

    #[must_use]
    pub const fn file(self) -> u32 {
        self.0 as u32 % 8
    }

    #[must_use]
    pub const fn flip_horizontal(self) -> Self {
        Self(self.0 ^ 0b000_111)
    }

    #[must_use]
    pub const fn flip_vertical(self) -> Self {
        Self(self.0 ^ 0b111_000)
    }
}

pub enum SquareStrError {
    WrongSize,
    InvalidFile,
    InvalidRank,
}

impl Display for SquareStrError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SquareStrError::WrongSize => write!(f, "wrong size"),
            SquareStrError::InvalidFile => write!(f, "invalid file"),
            SquareStrError::InvalidRank => write!(f, "invalid rank"),
        }
    }
}

impl FromStr for Square {
    type Err = SquareStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {
            return Err(SquareStrError::WrongSize);
        }

        let mut chars = s.chars();

        let file = chars.next().unwrap();
        let rank = chars.next().unwrap();

        if !('a'..='h').contains(&file) {
            return Err(SquareStrError::InvalidFile);
        } else if !('1'..='8').contains(&rank) {
            return Err(SquareStrError::InvalidRank);
        }

        let file_idx = file as u32 - 'a' as u32;
        let rank_idx = rank as u32 - '1' as u32;

        Ok(Self::from_coords(rank_idx, file_idx))
    }
}

impl Display for Square {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if *self == Square::NONE {
            return write!(f, "00");
        }

        write!(
            f,
            "{}{}",
            char::from_u32(self.file() + 'a' as u32).unwrap(),
            char::from_u32(self.rank() + '1' as u32).unwrap()
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Color {
    value: u8,
}

#[allow(unused)]
impl Color {
    pub const BLACK: Self = Self::from_raw(0);
    pub const WHITE: Self = Self::from_raw(1);

    pub const NONE: Self = Self::from_raw(2);

    pub const N_COLORS: usize = Self::NONE.value as usize;

    #[must_use]
    pub const fn from_raw(value: u8) -> Self {
        debug_assert!(value <= 2);
        Self { value }
    }

    #[must_use]
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'b' | 'B' => Some(Self::BLACK),
            'w' | 'W' => Some(Self::WHITE),
            _ => None,
        }
    }

    #[must_use]
    pub fn to_char(self) -> char {
        match self {
            Self::BLACK => 'b',
            Self::WHITE => 'w',
            Self::NONE => ' ',
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub fn flip(self) -> Self {
        debug_assert!(self != Self::NONE);
        Self::from_raw(self.value ^ 1)
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.value
    }

    #[must_use]
    pub const fn idx(self) -> usize {
        self.value as usize
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PieceType {
    value: u8,
}

#[allow(unused)]
impl PieceType {
    pub const PAWN: Self = Self::from_raw(0);
    pub const KNIGHT: Self = Self::from_raw(1);
    pub const BISHOP: Self = Self::from_raw(2);
    pub const ROOK: Self = Self::from_raw(3);
    pub const QUEEN: Self = Self::from_raw(4);
    pub const KING: Self = Self::from_raw(5);

    pub const NONE: Self = Self::from_raw(6);

    pub const N_PIECE_TYPES: usize = Self::NONE.value as usize;

    #[must_use]
    pub const fn from_raw(value: u8) -> Self {
        debug_assert!(value <= 6);
        Self { value }
    }

    #[must_use]
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'p' | 'P' => Some(Self::PAWN),
            'n' | 'N' => Some(Self::KNIGHT),
            'b' | 'B' => Some(Self::BISHOP),
            'r' | 'R' => Some(Self::ROOK),
            'q' | 'Q' => Some(Self::QUEEN),
            'k' | 'K' => Some(Self::KING),
            _ => None,
        }
    }

    #[must_use]
    pub fn to_char(self) -> char {
        match self {
            Self::PAWN => 'p',
            Self::KNIGHT => 'n',
            Self::BISHOP => 'b',
            Self::ROOK => 'r',
            Self::QUEEN => 'q',
            Self::KING => 'k',
            Self::NONE => ' ',
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub const fn colored(self, c: Color) -> Piece {
        debug_assert!(self.raw() != PieceType::NONE.raw());
        debug_assert!(c.raw() != Color::NONE.raw());

        Piece::from_raw((self.value << 1) | c.value)
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.value
    }

    #[must_use]
    pub fn idx(self) -> usize {
        self.value as usize
    }

    #[must_use]
    pub fn all() -> PieceTypeIterator {
        PieceTypeIterator::new()
    }
}

pub struct PieceTypeIterator {
    curr: u8,
}

impl PieceTypeIterator {
    #[must_use]
    fn new() -> Self {
        Self { curr: 0 }
    }
}

impl Iterator for PieceTypeIterator {
    type Item = PieceType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr >= PieceType::NONE.raw() {
            None
        } else {
            let v = PieceType::from_raw(self.curr);
            self.curr += 1;
            Some(v)
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Piece {
    value: u8,
}

impl Piece {
    pub const BLACK_PAWN: Self = Self::from_raw(0);
    pub const WHITE_PAWN: Self = Self::from_raw(1);
    pub const BLACK_KNIGHT: Self = Self::from_raw(2);
    pub const WHITE_KNIGHT: Self = Self::from_raw(3);
    pub const BLACK_BISHOP: Self = Self::from_raw(4);
    pub const WHITE_BISHOP: Self = Self::from_raw(5);
    pub const BLACK_ROOK: Self = Self::from_raw(6);
    pub const WHITE_ROOK: Self = Self::from_raw(7);
    pub const BLACK_QUEEN: Self = Self::from_raw(8);
    pub const WHITE_QUEEN: Self = Self::from_raw(9);
    pub const BLACK_KING: Self = Self::from_raw(10);
    pub const WHITE_KING: Self = Self::from_raw(11);

    pub const NONE: Self = Self::from_raw(12);

    pub const N_PIECES: usize = Self::NONE.value as usize;

    #[must_use]
    pub const fn from_raw(value: u8) -> Self {
        debug_assert!(value <= 12);
        Self { value }
    }

    #[must_use]
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'p' => Some(Self::BLACK_PAWN),
            'P' => Some(Self::WHITE_PAWN),
            'n' => Some(Self::BLACK_KNIGHT),
            'N' => Some(Self::WHITE_KNIGHT),
            'b' => Some(Self::BLACK_BISHOP),
            'B' => Some(Self::WHITE_BISHOP),
            'r' => Some(Self::BLACK_ROOK),
            'R' => Some(Self::WHITE_ROOK),
            'q' => Some(Self::BLACK_QUEEN),
            'Q' => Some(Self::WHITE_QUEEN),
            'k' => Some(Self::BLACK_KING),
            'K' => Some(Self::WHITE_KING),
            _ => None,
        }
    }

    #[must_use]
    pub const fn color(self) -> Color {
        Color::from_raw(self.value & 1)
    }

    #[must_use]
    pub fn to_char(self) -> char {
        match self {
            Self::BLACK_PAWN => 'p',
            Self::BLACK_KNIGHT => 'n',
            Self::BLACK_BISHOP => 'b',
            Self::BLACK_ROOK => 'r',
            Self::BLACK_QUEEN => 'q',
            Self::BLACK_KING => 'k',
            Self::WHITE_PAWN => 'P',
            Self::WHITE_KNIGHT => 'N',
            Self::WHITE_BISHOP => 'B',
            Self::WHITE_ROOK => 'R',
            Self::WHITE_QUEEN => 'Q',
            Self::WHITE_KING => 'K',
            Self::NONE => ' ',
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub const fn piece_type(self) -> PieceType {
        PieceType::from_raw(self.value >> 1)
    }

    #[must_use]
    pub const fn raw(self) -> u8 {
        self.value
    }

    #[must_use]
    pub fn idx(self) -> usize {
        self.value as usize
    }

    #[must_use]
    pub fn all() -> PieceIterator {
        PieceIterator::new()
    }
}

pub struct PieceIterator {
    curr: u8,
}

impl PieceIterator {
    #[must_use]
    fn new() -> Self {
        Self { curr: 0 }
    }
}

impl Iterator for PieceIterator {
    type Item = Piece;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr >= Piece::NONE.raw() {
            None
        } else {
            let v = Piece::from_raw(self.curr);
            self.curr += 1;
            Some(v)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::core::Color;

    #[test]
    fn color_flip() {
        assert_eq!(Color::BLACK.flip(), Color::WHITE);
        assert_eq!(Color::WHITE.flip(), Color::BLACK);

        assert_eq!(Color::BLACK.flip().flip(), Color::BLACK);
        assert_eq!(Color::WHITE.flip().flip(), Color::WHITE);
    }
}
