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

use crate::chess_move::ChessMove;
use crate::core::{Color, PieceType};
use crate::limit::SearchLimiter;
use crate::movegen::{generate_moves, MoveList};
use crate::position::Position;
use std::time::Instant;

#[derive(Debug, Copy, Clone)]
pub struct Params {
    pub cpuct: f32,
    pub fpu: f32,
}

impl Default for Params {
    fn default() -> Self {
        Self {
            cpuct: std::f32::consts::SQRT_2,
            fpu: f32::INFINITY,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
enum GameResult {
    #[allow(unused)]
    Win,
    Draw,
    Loss,
    Ongoing,
}

impl GameResult {
    fn u(self) -> Option<f32> {
        match self {
            GameResult::Win => Some(1.0),
            GameResult::Draw => Some(0.5),
            GameResult::Loss => Some(0.0),
            GameResult::Ongoing => None,
        }
    }

    fn is_terminal(self) -> bool {
        self != Self::Ongoing
    }
}

struct Node {
    parent: u32,
    mv: ChessMove,
    first_child: u32,
    child_count: u8,
    visits: u32,
    total_score: f32,
    result: GameResult,
}

impl Node {
    fn new(parent: u32, mv: ChessMove) -> Self {
        Self {
            parent,
            mv,
            first_child: 0,
            child_count: 0,
            visits: 0,
            total_score: 0.0,
            result: GameResult::Ongoing,
        }
    }
}

pub struct Searcher {
    tree: Vec<Node>,
    pos: Position,
    depth: u32,
}

impl Searcher {
    pub fn new() -> Self {
        Self {
            tree: Vec::new(),
            pos: Position::empty(),
            depth: 0,
        }
    }

    #[allow(clippy::unused_self)]
    pub fn new_game(&mut self) {
        // no-op
    }

    fn select(&mut self, cpuct: f32, fpu: f32) -> u32 {
        let mut curr = 0u32;

        loop {
            let node = &self.tree[curr as usize];

            if curr != 0 {
                #[cfg(debug_assertions)]
                {
                    let mut moves = MoveList::new();
                    generate_moves(&mut moves, &self.pos);
                    assert!(moves.contains(&node.mv));
                }

                self.pos.apply_move::<false, true>(node.mv);
                self.depth += 1;
            }

            // if the node isn't terminal and has no children, it has not been expanded
            if node.result.is_terminal() || node.child_count == 0 {
                break;
            }

            assert_ne!(node.visits, 0);

            let first = node.first_child as usize;
            let count = node.child_count as usize;

            let lv = (node.visits as f32).ln();

            let mut best_child = None;
            let mut best_child_uct = f32::NEG_INFINITY;

            for (child_idx, child) in self.tree[first..(first + count)].iter().enumerate() {
                let uct = if child.visits == 0 {
                    fpu
                } else {
                    let visits = child.visits as f32;
                    let score = child.total_score / visits;
                    score + cpuct * (lv / visits).sqrt()
                };

                if uct > best_child_uct {
                    best_child = Some(child_idx);
                    best_child_uct = uct;
                }
            }

            assert_ne!(best_child, None);

            curr = (first + best_child.unwrap()) as u32;
        }

        curr
    }

    fn expand(&mut self, node_idx: u32) {
        let next = self.tree.len() as u32;
        let node = &mut self.tree[node_idx as usize];

        assert_eq!(node.child_count, 0);
        assert!(!node.result.is_terminal());

        if self.pos.is_drawn(node_idx == 0) {
            node.result = GameResult::Draw;
            return;
        }

        let mut moves = MoveList::new();
        generate_moves(&mut moves, &self.pos);

        if moves.is_empty() {
            node.result = if self.pos.is_in_check() {
                GameResult::Loss
            } else {
                GameResult::Draw
            };
            return;
        }

        let child_count = moves.len() as u32;

        node.first_child = next;
        node.child_count = child_count as u8;

        for mv in moves {
            let node = Node::new(node_idx, mv);
            self.tree.push(node);
        }
    }

    fn simulate(&self, node_idx: u32) -> f32 {
        let node = &self.tree[node_idx as usize];
        node.result.u().unwrap_or_else(|| {
            fn material(pos: &Position, c: Color) -> i32 {
                (pos.colored_pieces(PieceType::PAWN.colored(c)).popcount() * 100
                    + pos.colored_pieces(PieceType::KNIGHT.colored(c)).popcount() * 300
                    + pos.colored_pieces(PieceType::BISHOP.colored(c)).popcount() * 300
                    + pos.colored_pieces(PieceType::ROOK.colored(c)).popcount() * 500
                    + pos.colored_pieces(PieceType::QUEEN.colored(c)).popcount() * 900)
                    as i32
            }

            let stm = self.pos.side_to_move();
            let eval = material(&self.pos, stm) - material(&self.pos, stm.flip());

            1.0 / (1.0 + (-eval as f32 / 400.0).exp())
        })
    }

    fn backprop(&mut self, mut node_idx: u32, mut u: f32) {
        loop {
            let node = &mut self.tree[node_idx as usize];

            node.visits += 1;

            if node_idx == 0 {
                break;
            }

            u = 1.0 - u;
            node.total_score += u;

            node_idx = node.parent;
        }
    }

    fn select_best_move(&self, chess960: bool) -> (usize, f32) {
        let root = &self.tree[0];

        let start = root.first_child as usize;
        let end = start + root.child_count as usize;

        assert!(start < end);

        let mut best = None;
        let mut best_visits = 0u32;
        let mut best_score = f32::NEG_INFINITY;

        for node_idx in start..end {
            let node = &self.tree[node_idx];
            let score = node.total_score / (node.visits as f32);

            println!(
                "info string {}: V: {}, S: {}",
                node.mv.to_string(chess960),
                node.visits,
                score,
            );

            if node.visits > best_visits {
                best = Some(node_idx);
                best_visits = node.visits;
                best_score = score;
            }
        }

        (best.unwrap(), best_score)
    }

    fn build_pv(&self, pv: &mut MoveList, chess960: bool) -> f32 {
        let (best_child_root, best_score_root) = self.select_best_move(chess960);
        pv.push(self.tree[best_child_root].mv);

        let mut node_idx = best_child_root as u32;

        loop {
            let node = &self.tree[node_idx as usize];

            if node.result.is_terminal() || node.child_count == 0 {
                break;
            }

            let mut found_child = false;

            let mut best_child = 0;
            let mut best_child_score = f32::NEG_INFINITY;

            for child in node.first_child..(node.first_child + u32::from(node.child_count)) {
                let child_node = &self.tree[child as usize];

                if child_node.visits == 0 {
                    continue;
                }

                found_child = true;

                let score = child_node.total_score / (child_node.visits as f32);

                if score > best_child_score {
                    best_child = child;
                    best_child_score = score;
                }
            }

            if !found_child {
                break;
            }

            pv.push(self.tree[best_child as usize].mv);
            node_idx = best_child;
        }

        best_score_root
    }

    pub fn search(
        &mut self,
        pos: &Position,
        params: &Params,
        mut limiter: SearchLimiter,
        chess960: bool,
    ) {
        self.tree.push(Node::new(0, ChessMove::NULL));

        let start = Instant::now();

        let mut nodes = 0usize;

        let mut max_depth = 0u32;
        let mut total_depth = 0usize;

        while !limiter.should_stop(nodes) {
            self.pos = pos.clone();
            self.depth = 1;

            let leaf = self.select(params.cpuct, params.fpu);
            let node = &self.tree[leaf as usize];

            if !node.result.is_terminal() {
                self.expand(leaf);
            }

            let u = self.simulate(leaf);
            self.backprop(leaf, u);

            nodes += 1;

            if self.depth > max_depth {
                max_depth = self.depth;
            }

            total_depth += self.depth as usize;
        }

        let average_depth = (total_depth as f64 / nodes as f64).round() as u32;

        let time = start.elapsed().as_secs_f64();
        let ms = (time * 1000.0).round() as usize;
        let nps = (nodes as f64 / time) as usize;

        let mut pv = MoveList::new();
        let score = self.build_pv(&mut pv, chess960);

        println!("info string Raw score: {}", score);

        #[allow(clippy::float_cmp)]
        let cp = if score == 1.0 {
            30000
        } else if score == 0.0 {
            -30000
        } else {
            (-400.0 * (1.0 / score - 1.0).ln()) as i32
        };

        self.tree.clear();
        self.tree.shrink_to_fit();

        print!(
            "info depth {} seldepth {} nodes {} time {} nps {} score cp {} pv",
            average_depth, max_depth, nodes, ms, nps, cp
        );

        for mv in &pv {
            print!(" {}", mv.to_string(chess960));
        }

        println!();
        println!("bestmove {}", pv[0].to_string(chess960));
    }
}
