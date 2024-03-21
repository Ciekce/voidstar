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

use crate::core::Color;
use crate::limit::SearchLimiter;
use crate::movegen::{generate_moves, MoveList};
use crate::nnue;
use crate::perft::{perft, split_perft};
use crate::position::Position;
use crate::search::Searcher;

const NAME: &str = "Voidstar";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

struct UciHandler {
    pos: Position,
    searcher: Searcher,
    chess960: bool,
}

impl UciHandler {
    #[must_use]
    fn new() -> Self {
        Self {
            pos: Position::startpos(),
            searcher: Searcher::new(),
            chess960: false,
        }
    }

    fn run(&mut self) {
        let mut line = String::with_capacity(256);
        while let Ok(bytes) = std::io::stdin().read_line(&mut line) {
            if bytes == 0 {
                break;
            }

            let cmd: Vec<&str> = line.split_whitespace().collect();
            if cmd.is_empty() {
                line.clear();
                continue;
            }

            match cmd[0] {
                "uci" => self.handle_uci(),
                "ucinewgame" => self.handle_ucinewgame(),
                "setoption" => self.handle_setoption(&cmd[1..]),
                "isready" => self.handle_isready(),
                "position" => self.handle_position(&cmd[1..]),
                "go" => self.handle_go(&cmd[1..]),
                "move" => self.handle_move(&cmd[1..]),
                "d" => self.handle_d(),
                "moves" => self.handle_moves(),
                "perft" => self.handle_perft(&cmd[1..]),
                "splitperft" => self.handle_splitperft(&cmd[1..]),
                "quit" => break,
                unknown => eprintln!("Unknown command '{}'", unknown),
            }

            line.clear();
        }
    }

    #[allow(clippy::unused_self)]
    fn handle_uci(&self) {
        println!("id name {} {}", NAME, VERSION);
        println!("id author {}", AUTHORS.replace(':', ", "));
        println!("option name UCI_Chess960 type check default false");
        println!("uciok");
    }

    fn handle_ucinewgame(&mut self) {
        self.searcher.new_game();
    }

    fn handle_setoption(&mut self, args: &[&str]) {
        if args.len() < 2 || args[0] != "name" {
            eprintln!("Missing name");
            return;
        }

        let mut idx = 1usize;
        while idx < args.len() && args[idx] != "value" {
            idx += 1;
        }

        if idx > args.len() - 2 || args[idx] != "value" {
            eprintln!("Missing value");
            return;
        }

        let name = args[1usize..idx].join(" ").to_ascii_lowercase();
        let value = args[(idx + 1)..].join(" ");

        #[allow(clippy::single_match)]
        match name.as_str() {
            "uci_chess960" => {
                if let Ok(new_chess960) = value.to_ascii_lowercase().parse::<bool>() {
                    self.chess960 = new_chess960;
                    if self.chess960 {
                        println!("info string enabled Chess960");
                    } else {
                        println!("info string disabled Chess960");
                    }
                } else {
                    eprintln!("Invalid bool");
                }
            }
            _ => {}
        }
    }

    #[allow(clippy::unused_self)]
    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_position(&mut self, args: &[&str]) {
        if args.is_empty() {
            return;
        }

        let next = match args[0] {
            "startpos" => {
                self.pos.reset_to_startpos();
                1usize
            }
            "fen" => {
                if let Err(err) = self.pos.reset_from_fen_parts(&args[1..]) {
                    eprintln!("{}", err);
                    return;
                }
                7usize
            }
            _ => return,
        };

        if args.len() <= next {
            return;
        } else if args[next] != "moves" {
            eprintln!("Unknown token '{}'", args[next]);
            return;
        }

        for move_str in &args[next + 1..] {
            match self.pos.move_from_str(move_str, self.chess960) {
                Ok(mv) => self.pos.apply_move::<false, true>(mv),
                Err(err) => {
                    eprintln!("Invalid move '{}': {}", move_str, err);
                    return;
                }
            }
        }
    }

    fn handle_go(&mut self, args: &[&str]) {
        let mut limiter: Option<SearchLimiter> = None;

        let mut tournament_time = false;

        let mut black_time = 0u64;
        let mut white_time = 0u64;
        let mut black_inc = 0u64;
        let mut white_inc = 0u64;

        let mut moves_to_go = 0u64;

        let mut i = 0usize;
        while i < args.len() {
            match args[i] {
                "infinite" => {
                    if tournament_time || limiter.is_some() {
                        eprintln!("Multiple non-depth search limits not supported");
                        return;
                    }

                    println!("info string this search will never terminate");

                    limiter = Some(SearchLimiter::infinite());
                }
                "depth" => {
                    i += 1;
                    if i >= args.len() {
                        eprintln!("Missing depth");
                        return;
                    }

                    println!("info string fixed-depth searches not supported");

                    return;
                }
                "nodes" => {
                    if tournament_time || limiter.is_some() {
                        eprintln!("Multiple non-depth search limits not supported");
                        return;
                    }

                    i += 1;
                    if i >= args.len() {
                        eprintln!("Missing node count");
                        return;
                    }

                    if let Ok(node_limit) = args[i].parse::<usize>() {
                        limiter = Some(SearchLimiter::fixed_nodes(node_limit));
                    } else {
                        eprintln!("Invalid node limit '{}'", args[i]);
                        return;
                    }
                }
                "movetime" => {
                    if tournament_time || limiter.is_some() {
                        eprintln!("Multiple non-depth search limits not supported");
                        return;
                    }

                    i += 1;
                    if i >= args.len() {
                        eprintln!("Missing move time");
                        return;
                    }

                    if let Ok(time_limit) = args[i].parse::<u64>() {
                        limiter = Some(SearchLimiter::move_time(time_limit));
                    } else {
                        eprintln!("Invalid move time '{}'", args[i]);
                        return;
                    }
                }
                "wtime" | "btime" | "winc" | "binc" | "movestogo" => {
                    if limiter.is_some() {
                        eprintln!("Multiple non-depth search limits not supported");
                        return;
                    }

                    tournament_time = true;

                    let token = args[i];

                    i += 1;
                    if i >= args.len() {
                        eprintln!("Missing {}", token);
                        return;
                    }

                    let Ok(value) = args[i].parse::<u64>() else {
                        eprintln!("Invalid {} '{}'", token, args[i]);
                        return;
                    };

                    match token {
                        "btime" => black_time = value,
                        "wtime" => white_time = value,
                        "binc" => black_inc = value,
                        "winc" => white_inc = value,
                        "movestogo" => moves_to_go = value,
                        _ => unreachable!(),
                    }
                }
                unknown => {
                    eprintln!("Unknown search limit '{}'", unknown);
                    return;
                }
            }

            i += 1;
        }

        if tournament_time {
            assert!(limiter.is_none());

            let (our_time, our_inc) = match self.pos.side_to_move() {
                Color::BLACK => (black_time, black_inc),
                Color::WHITE => (white_time, white_inc),
                _ => unreachable!(),
            };

            limiter = Some(SearchLimiter::tournament(our_time, our_inc, moves_to_go));
        } else if limiter.is_none() {
            limiter = Some(SearchLimiter::infinite());
        }

        self.searcher
            .search(&self.pos, limiter.unwrap().clone(), self.chess960);
    }

    fn handle_move(&mut self, args: &[&str]) {
        if args.is_empty() {
            return;
        }

        match self.pos.move_from_str(args[0], self.chess960) {
            Ok(mv) => self.pos.apply_move::<false, true>(mv),
            Err(err) => eprintln!("Invalid move '{}': {}", args[0], err),
        }
    }

    fn handle_d(&self) {
        println!("{}", self.pos);
        println!();
        println!("Fen: {}", self.pos.to_fen(self.chess960));
        println!("Key: {:16x}", self.pos.key());
        println!("Static eval: {}", nnue::eval(&self.pos));

        print!("Checkers:");
        for checker in self.pos.checkers() {
            print!(" {}", checker);
        }
        println!();

        print!("Pinned:");
        let all_pinned =
            self.pos.occupancy() & (self.pos.diag_pin_mask() | self.pos.ortho_pin_mask());
        for pinned in all_pinned {
            print!(" {}", pinned);
        }
        println!();
    }

    fn handle_moves(&self) {
        let mut moves = MoveList::new();
        generate_moves(&mut moves, &self.pos);
        println!(
            "{}",
            moves
                .iter()
                .map(|mv| mv.to_string(self.chess960))
                .collect::<Vec<String>>()
                .join(" ")
        );
    }

    fn handle_perft(&mut self, args: &[&str]) {
        if args.is_empty() {
            eprintln!("Missing depth");
            return;
        }

        if let Ok(depth) = args[0].parse::<i32>() {
            perft(&mut self.pos, depth);
        } else {
            eprintln!("Invalid depth");
        }
    }

    fn handle_splitperft(&mut self, args: &[&str]) {
        if args.is_empty() {
            eprintln!("Missing depth");
            return;
        }

        if let Ok(depth) = args[0].parse::<i32>() {
            split_perft(&mut self.pos, depth, self.chess960);
        } else {
            eprintln!("Invalid depth");
        }
    }
}

pub fn run() {
    let mut handler = UciHandler::new();
    handler.run();
}
