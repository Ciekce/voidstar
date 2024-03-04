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

    pub fn normal(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::Normal as usize)) as u16,
        }
    }

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

    pub fn castling(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::Castling as usize)) as u16,
        }
    }

    pub fn en_passant(src: Square, dst: Square) -> Self {
        Self {
            value: ((src.idx() << Self::SRC_SHIFT)
                | (dst.idx() << Self::DST_SHIFT)
                | (MoveType::EnPassant as usize)) as u16,
        }
    }

    const fn from_raw(value: u16) -> Self {
        Self { value }
    }

    pub fn is_null(&self) -> bool {
        self.value == 0
    }

    pub fn src(&self) -> Square {
        Square::from_raw((self.value >> Self::SRC_SHIFT) as u8 & 0x3F)
    }

    pub fn dst(&self) -> Square {
        Square::from_raw((self.value >> Self::SRC_SHIFT) as u8 & 0x3F)
    }

    pub fn promo(&self) -> PieceType {
        PieceType::from_raw(((self.value >> Self::PROMO_SHIFT) as u8 & 0x3) + 1)
    }

    pub fn move_type(&self) -> MoveType {
        unsafe { std::mem::transmute((self.value & 0x3) as u8) }
    }
}

pub enum MoveStrError {
    InvalidFrom,
    InvalidTo,
    InvalidPromo,
    WrongSize,
}

/*
impl FromStr for ChessMove {
    type Err = MoveStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "0000" {
            return Ok(ChessMove::NULL);
        }

        let promo_piece = if s.len() == 5 {
            match PieceType::from_char(s.chars().nth(4).unwrap()) {
                Some(promo) => promo,
                None => return Err(MoveStrError::InvalidPromo),
            }
        } else if s.len() != 4 {
            return Err(MoveStrError::WrongSize);
        } else {
            PieceType::NONE
        };

        if let Ok(from) = Square::from_str(&s[0..2]) {
            if let Ok(to) = Square::from_str(&s[2..4]) {
                todo!();
            } else {
                Err(MoveStrError::InvalidTo)
            }
        } else {
            Err(MoveStrError::InvalidFrom)
        }
    }
}
 */

impl Display for ChessMove {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_null() {
            return write!(f, "0000");
        }

        write!(f, "{}{}", self.src(), self.dst())?;

        if self.move_type() == MoveType::Promotion {
            write!(f, "{}", self.promo().to_char())?;
        }

        Ok(())
    }
}
