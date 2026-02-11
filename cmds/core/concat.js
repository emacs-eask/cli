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

import { TITLE_CMD_OPTION } from "../../src/env.js";
import { e_call, hide_cmd, def_flag } from '../../src/util.js';

export const command = ['concat [names..]', 'concatenate [names..]'];
export const desc = hide_cmd('Concatenate elisp files');
export const builder = yargs => yargs
  .positional(
    '[names..]', {
      description: 'specify files to concatenate',
      type: 'array',
    })
  .options({
    'destination': {
      description: 'optional output destination',
      requiresArg: true,
      alias: 'dest',
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
    'output': {
      description: 'Output result to a file',
      alias: 'o',
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
  });

export const handler = async (argv) => {
  await e_call(argv, 'core/concat'
    , argv.names
    , def_flag(argv.dest, '--dest', argv.dest)
    , def_flag(argv.output, '--output', argv.output));
};
