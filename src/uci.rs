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

use crate::position::Position;

const NAME: &str = "Voidstar";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");

struct UciHandler {
    pos: Position,
}

impl UciHandler {
    #[must_use]
    fn new() -> Self {
        Self {
            pos: Position::startpos(),
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
        println!("uaiok");
    }

    fn handle_isready(&self) {
        println!("readyok");
    }

    fn handle_position(&mut self, args: &[&str]) {
        if args.is_empty() {
            return;
        }

        match args[0] {
            "startpos" => {
                self.pos.reset_to_startpos();
            }
            "fen" => {
                if let Err(err) = self.pos.reset_from_fen_parts(&args[1..]) {
                    eprintln!("{}", err);
                }
            }
            _ => {}
        };
    }

    fn handle_d(&self) {
        println!("{}", self.pos);
        println!();
        println!("Fen: {}", self.pos.to_fen());
        println!("Key: {:16x}", self.pos.key());
    }
}

pub fn run() {
    let mut handler = UciHandler::new();
    handler.run();
}
