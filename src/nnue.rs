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

use crate::core::{Color, PieceType};
use crate::position::Position;
use std::iter::Iterator;
use std::ops::Index;

const L1_Q: i32 = 255;
const OUTPUT_Q: i32 = 64;

const SCALE: i32 = 400;

const KING_BUCKETS: usize = 4;

#[rustfmt::skip]
const BUCKET_MAP: [usize; 64] = [
    0, 0, 0, 0, 1, 1, 1, 1,
    0, 0, 0, 0, 1, 1, 1, 1,
    2, 2, 2, 2, 3, 3, 3, 3,
    2, 2, 2, 2, 3, 3, 3, 3,
    2, 2, 2, 2, 3, 3, 3, 3,
    2, 2, 2, 2, 3, 3, 3, 3,
    2, 2, 2, 2, 3, 3, 3, 3,
    2, 2, 2, 2, 3, 3, 3, 3,
];

const OUTPUT_BUCKETS: usize = 8;

const INPUT_SIZE: usize = 768;
const L1_SIZE: usize = 768;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(align(64))]
struct Align64<T>(T);

impl<T> Index<usize> for Align64<T>
where
    T: Index<usize>,
{
    type Output = T::Output;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[repr(C)]
struct Layer<T, const WEIGHTS: usize, const OUTPUTS: usize> {
    weights: Align64<[T; WEIGHTS]>,
    biases: Align64<[T; OUTPUTS]>,
}

#[repr(C)]
struct Network {
    _padding: [u8; 64],
    feature_transformer: Layer<i16, { KING_BUCKETS * INPUT_SIZE * L1_SIZE }, L1_SIZE>,
    l1: Layer<i16, { L1_SIZE * 2 * OUTPUT_BUCKETS }, OUTPUT_BUCKETS>,
}

const NETWORK: Network = unsafe { std::mem::transmute(*include_bytes!("edgelands.nnue")) };

fn activate(v: i16) -> i32 {
    let clipped = i32::from(v).clamp(0, L1_Q);
    clipped * clipped
}

fn activate_feature(acc: &mut [i16; L1_SIZE], idx: usize) {
    let offset = idx * L1_SIZE;
    let weights = &NETWORK.feature_transformer.weights.0[offset..(offset + L1_SIZE)];
    for (v, w) in acc.iter_mut().zip(weights) {
        *v += w;
    }
}

fn activate_piece(
    acc: &mut [i16; L1_SIZE],
    pos: &Position,
    c: Color,
    bucket_offset: usize,
    piece: PieceType,
) {
    let ours = pos.colored_pieces(piece.colored(c));
    let theirs = pos.colored_pieces(piece.colored(c.flip()));

    let (ours, theirs) = if c == Color::BLACK {
        (ours.flipped(), theirs.flipped())
    } else {
        (ours, theirs)
    };

    let our_offset = bucket_offset + piece.idx() * 64;
    let their_offset = our_offset + 64 * 6;

    for sq in ours {
        activate_feature(acc, our_offset + sq.idx());
    }

    for sq in theirs {
        activate_feature(acc, their_offset + sq.idx());
    }
}

fn activate_features(acc: &mut [i16; L1_SIZE], pos: &Position, c: Color) {
    let king_square = if c == Color::BLACK {
        pos.king_square(c).flip_vertical()
    } else {
        pos.king_square(c)
    };

    let bucket_offset = BUCKET_MAP[king_square.idx()] * INPUT_SIZE;

    activate_piece(acc, pos, c, bucket_offset, PieceType::PAWN);
    activate_piece(acc, pos, c, bucket_offset, PieceType::KNIGHT);
    activate_piece(acc, pos, c, bucket_offset, PieceType::BISHOP);
    activate_piece(acc, pos, c, bucket_offset, PieceType::ROOK);
    activate_piece(acc, pos, c, bucket_offset, PieceType::QUEEN);
    activate_piece(acc, pos, c, bucket_offset, PieceType::KING);
}

pub fn eval(pos: &Position) -> i32 {
    let mut accs = [[0i16; L1_SIZE]; 2];

    accs[0].copy_from_slice(&NETWORK.feature_transformer.biases.0);
    accs[1].copy_from_slice(&NETWORK.feature_transformer.biases.0);

    activate_features(&mut accs[0], pos, Color::BLACK);
    activate_features(&mut accs[1], pos, Color::WHITE);

    let stm = pos.side_to_move();

    let stm_acc = &accs[usize::from(stm == Color::WHITE)];
    let nstm_acc = &accs[usize::from(stm != Color::WHITE)];

    let output_bucket = (pos.occupancy().popcount() - 2) as usize / (32 / OUTPUT_BUCKETS);
    let weight_offset = output_bucket * L1_SIZE * 2;

    let stm_weights = &NETWORK.l1.weights.0[weight_offset..(weight_offset + L1_SIZE)];
    let nstm_weights =
        &NETWORK.l1.weights.0[(weight_offset + L1_SIZE)..(weight_offset + L1_SIZE * 2)];

    let mut sum = 0i32;

    for (&v, &w) in stm_acc.iter().zip(stm_weights) {
        sum += activate(v) * i32::from(w);
    }

    for (&v, &w) in nstm_acc.iter().zip(nstm_weights) {
        sum += activate(v) * i32::from(w);
    }

    let sum = sum / L1_Q;

    let output_bias = i32::from(NETWORK.l1.biases[output_bucket]);
    (sum + output_bias) * SCALE / (L1_Q * OUTPUT_Q)
}
