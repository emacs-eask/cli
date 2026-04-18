/**
 * Copyright (C) 2024-2026 the Eask authors.
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
import { cmd_count } from "../../src/util.js";

const __dirname = import.meta.dirname;

export const command = ['test <type>'];
export const desc = 'Generate test files based on the testing framework';
export const builder = function (yargs) {
  yargs.usage(`${desc}

Usage: eask generate test <type> [options..]`)
    .commandDir(path.join(__dirname, './test/'))
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (cmd_count() == 2) {
    yargs.positional(
      '<type>', {
        description: 'type of the testing framework',
      });
  }
}

export const handler = async (argv) => { };
