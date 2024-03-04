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

// adapted from akimbo
#[macro_export]
macro_rules! array_init {
    (| $i:ident, $size:literal | $($r:tt)+) => {{
        let mut $i = 0usize;
        let mut _res = [{$($r)+}; $size];
        while $i < $size - 1 {
            $i += 1;
            _res[$i] = {$($r)+};
        }
        _res
    }}
}
