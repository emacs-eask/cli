/**
 * Copyright (C) 2022-2026 the Eask authors.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

"use strict";

import path from 'path';
import { cmd_count } from '../../src/util.js';

const __dirname = import.meta.dirname;

export const command = ['run <type>'];
export const desc = 'Run custom tasks';
export const builder = function (yargs) {
  yargs.usage(`${desc}

Usage: eask run <type> [options..]`)
    .commandDir(path.join(__dirname, '../run/'))
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (cmd_count() == 1) {
    yargs.positional(
      '<type>', {
        description: 'type of the execution',
      });
  }
}

export const handler = async (argv) => { };
