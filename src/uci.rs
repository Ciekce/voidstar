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

use crate::position::{MoveStrError, Position};

const NAME: &str = "Voidstar";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

struct UciHandler {
    pos: Position,
    chess960: bool,
}

impl UciHandler {
    #[must_use]
    fn new() -> Self {
        Self {
            pos: Position::startpos(),
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
                "setoption" => self.handle_setoption(&cmd[1..]),
                "isready" => self.handle_isready(),
                "position" => self.handle_position(&cmd[1..]),
                "d" => self.handle_d(),
                "quit" => break,
                unknown => eprintln!("Unknown command '{}'", unknown),
            }

            line.clear();
        }
    }

    fn handle_uci(&self) {
        println!("id name {} {}", NAME, VERSION);
        println!("id author {}", AUTHORS.replace(':', ", "));
        println!("option name UCI_Chess960 type check default false");
        println!("uciok");
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
                    return;
                }
            }
            _ => {}
        }
    }

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
                Ok(m) => self.pos.apply_move::<false, true>(m),
                Err(err) => {
                    eprintln!("Invalid move '{}': {}", move_str, err);
                    return;
                }
            }
        }
    }

    fn handle_d(&self) {
        println!("{}", self.pos);
        println!();
        println!("Fen: {}", self.pos.to_fen(self.chess960));
        println!("Key: {:16x}", self.pos.key());
    }
}

pub fn run() {
    let mut handler = UciHandler::new();
    handler.run();
}
