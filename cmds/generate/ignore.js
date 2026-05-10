/**
 * Copyright (C) 2023-2026 the Eask authors.
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

import { TITLE_CMD_OPTION } from "../../src/env.js";
import { e_call, def_flag } from "../../src/util.js";

export const command = ['ignore <name>'];
export const desc = 'Generate an ignore file using .gitignore templates';
export const builder = yargs => yargs
  .positional(
    '<name>', {
      description: 'name of the ignore template',
      type: 'string',
    })
  .options({
    'output': {
      description: 'output result to a file; the default is `.gitignore`',
      alias: 'o',
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
  });

export const handler = async (argv) => {
  await e_call(argv, 'generate/ignore'
    , argv.name
    , def_flag(argv.output, '--output', argv.output));
};
