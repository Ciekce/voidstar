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

macro_rules! c_for {
    ($init: stmt; $cond: expr; $step: expr; $body: block) => {
        {
            $init
            while $cond {
                $body;
                $step;
            }
        }
    }
}

macro_rules! next_jsf64 {
    ($s:ident) => {{
        let e = $s.a.wrapping_sub($s.b.rotate_left(7));
        $s.a = $s.b ^ $s.c.rotate_left(13);
        $s.b = $s.c.wrapping_add($s.d.rotate_left(37));
        $s.c = $s.d.wrapping_add(e);
        $s.d = e.wrapping_add($s.a);
        $s.d
    }};
}

#[derive(Debug, Copy, Clone)]
pub struct Jsf64Rng {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
}

#[allow(unused)]
impl Jsf64Rng {
    #[must_use]
    pub const fn new(seed: u64) -> Self {
        #[allow(clippy::unreadable_literal)]
        let mut result = Self {
            a: 0xF1EA5EED,
            b: seed,
            c: seed,
            d: seed,
        };

        c_for!(let mut i = 0; i < 20; i += 1; {
            next_jsf64!(result)
        });

        result
    }

    pub fn next_u64(&mut self) -> u64 {
        next_jsf64!(self)
    }

    #[must_use]
    pub fn next_u32(&mut self) -> u32 {
        (self.next_u64() >> 32) as u32
    }

    #[must_use]
    pub fn next_u32_bounded(&mut self, bound: u32) -> u32 {
        if bound == 0 {
            return 0;
        }

        let mut x = self.next_u32();
        let mut m = u64::from(x) * u64::from(bound);
        let mut l = m as u32;

        if l < bound {
            let mut t = bound.wrapping_neg();

            if t >= bound {
                t -= bound;
                if t >= bound {
                    t %= bound;
                }
            }

            while l < t {
                x = self.next_u32();
                m = u64::from(x) * u64::from(bound);
                l = m as u32;
            }
        }

        (m >> 32) as u32
    }
}

pub const fn fill_u64_array<const SIZE: usize>(seed: u64) -> [u64; SIZE] {
    let mut rng = Jsf64Rng::new(seed);
    let mut result = [0u64; SIZE];

    c_for!(let mut i: usize = 0; i < SIZE; i += 1; {
        result[i] = next_jsf64!(rng);
    });

    result
}
