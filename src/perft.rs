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

use crate::movegen::{generate_moves, MoveList};
use crate::position::Position;
use std::time::Instant;

#[must_use]
fn do_perft(pos: &mut Position, depth: i32) -> usize {
    if depth <= 0 {
        return 1;
    }

    let mut moves = MoveList::new();
    generate_moves(&mut moves, pos);

    if depth == 1 {
        return moves.len();
    }

    let mut total = 0usize;

    for mv in moves {
        pos.apply_move::<true, false>(mv);
        total += do_perft(pos, depth - 1);
        pos.pop_move::<false>();
    }

    total
}

pub fn perft(pos: &mut Position, depth: i32) {
    let total = do_perft(pos, depth);
    println!("{}", total);
}

pub fn split_perft(pos: &mut Position, depth: i32, chess960: bool) {
    let start = Instant::now();

    let mut moves = MoveList::new();
    generate_moves(&mut moves, pos);

    let mut total = 0usize;

    for mv in moves {
        pos.apply_move::<true, false>(mv);

        let value = do_perft(pos, depth - 1);

        total += value;
        println!("{}\t{}", mv.to_string(chess960), value);

        pos.pop_move::<false>();
    }

    let time = start.elapsed().as_secs_f64();
    let nps = (total as f64 / time) as usize;

    println!();
    println!("total {}", total);
    println!("{} nps", nps);
}
