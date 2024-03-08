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

use std::time::{Duration, Instant};

#[derive(Debug, Clone)]
enum SearchLimiterType {
    Infinite,
    FixedNodes(usize),
    MoveTime(Instant),
    Tournament(TimeManager),
}

#[derive(Debug, Clone)]
pub struct SearchLimiter {
    limiter: SearchLimiterType,
    stopped: bool,
}

impl SearchLimiter {
    #[must_use]
    pub fn infinite() -> Self {
        Self {
            limiter: SearchLimiterType::Infinite,
            stopped: false,
        }
    }

    #[must_use]
    pub fn fixed_nodes(nodes: usize) -> Self {
        Self {
            limiter: SearchLimiterType::FixedNodes(nodes),
            stopped: false,
        }
    }

    #[must_use]
    pub fn move_time(ms: u64) -> Self {
        let end = Instant::now() + Duration::from_millis(ms);
        Self {
            limiter: SearchLimiterType::MoveTime(end),
            stopped: false,
        }
    }

    #[must_use]
    pub fn tournament(our_time_ms: u64, our_inc_ms: u64, moves_to_go: u64) -> Self {
        Self {
            limiter: SearchLimiterType::Tournament(TimeManager::new(
                our_time_ms,
                our_inc_ms,
                moves_to_go,
            )),
            stopped: false,
        }
    }

    #[must_use]
    pub fn should_stop(&mut self, nodes: usize) -> bool {
        if self.stopped() {
            return true;
        }

        let should_stop = match &self.limiter {
            SearchLimiterType::Infinite => false,
            SearchLimiterType::FixedNodes(node_limit) => nodes >= *node_limit,
            SearchLimiterType::MoveTime(end_time) => Instant::now() >= *end_time,
            SearchLimiterType::Tournament(time_manager) => time_manager.should_stop(),
        };

        if should_stop {
            self.stopped = true;
            return true;
        }

        false
    }

    #[must_use]
    pub fn stopped(&self) -> bool {
        self.stopped
    }
}

#[derive(Debug, Clone)]
pub struct TimeManager {
    start: Instant,
    max_time: f64,
}

impl TimeManager {
    const DEFAULT_MOVES_TO_GO: u64 = 30;
    const INCREMENT_MULTIPLIER: f64 = 0.5;

    #[must_use]
    pub fn new(our_time_ms: u64, our_inc_ms: u64, moves_to_go: u64) -> Self {
        let start = Instant::now();

        let divisor = if moves_to_go == 0 {
            Self::DEFAULT_MOVES_TO_GO
        } else {
            moves_to_go
        } as f64;

        let our_time = our_time_ms as f64 / 1000.0;
        let our_inc = our_inc_ms as f64 / 1000.0;

        let time = our_time / divisor + our_inc * Self::INCREMENT_MULTIPLIER;

        Self {
            start,
            max_time: time,
        }
    }

    #[must_use]
    pub fn should_stop(&self) -> bool {
        let total_time = self.start.elapsed().as_secs_f64();
        total_time >= self.max_time
    }
}
