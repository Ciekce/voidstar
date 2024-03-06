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

use crate::core::{PieceType, Square};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum MoveType {
    Normal = 0,
    Promotion,
    Castling,
    EnPassant,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ChessMove {
    value: u16,
}

impl ChessMove {
    pub const NULL: Self = Self::from_raw(0);

    const SRC_SHIFT: i32 = 10;
    const DST_SHIFT: i32 = 4;
    const PROMO_SHIFT: i32 = 2;

    #[must_use]
    pub fn normal(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::Normal as usize)) as u16,
        }
    }

    #[must_use]
    pub fn promotion(src: Square, dst: Square, promo: PieceType) -> Self {
        debug_assert!(
            promo == PieceType::KNIGHT
                || promo == PieceType::BISHOP
                || promo == PieceType::ROOK
                || promo == PieceType::QUEEN
        );

        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | ((promo.idx() - 1) << Self::PROMO_SHIFT)
                | (MoveType::Promotion as usize)) as u16,
        }
    }

    #[must_use]
    pub fn castling(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::Castling as usize)) as u16,
        }
    }

    #[must_use]
    pub fn en_passant(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::EnPassant as usize)) as u16,
        }
    }

    #[must_use]
    const fn from_raw(value: u16) -> Self {
        Self { value }
    }

    #[must_use]
    pub fn is_null(&self) -> bool {
        self.value == 0
    }

    #[must_use]
    pub fn src(&self) -> Square {
        Square::from_raw((self.value >> Self::SRC_SHIFT) as u8 & 0x3F)
    }

    #[must_use]
    pub fn dst(&self) -> Square {
        Square::from_raw((self.value >> Self::DST_SHIFT) as u8 & 0x3F)
    }

    #[must_use]
    pub fn promo(&self) -> PieceType {
        PieceType::from_raw(((self.value >> Self::PROMO_SHIFT) as u8 & 0x3) + 1)
    }

    #[must_use]
    pub fn move_type(&self) -> MoveType {
        unsafe { std::mem::transmute((self.value & 0x3) as u8) }
    }

    // only meaningful for castling moves
    #[must_use]
    pub fn is_short_castling(&self) -> bool {
        self.src().file() < self.dst().file()
    }

    pub fn to_string(&self, chess960: bool) -> String {
        if self.move_type() == MoveType::Promotion {
            return format!("{}{}{}", self.src(), self.dst(), self.promo().to_char());
        }

        let dst_sq = if !chess960 && self.move_type() == MoveType::Castling {
            Square::from_coords(
                self.src().rank(),
                if self.is_short_castling() { 6 } else { 2 },
            )
        } else {
            self.dst()
        };

        format!("{}{}", self.src(), dst_sq)
    }
}
