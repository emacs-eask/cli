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
import { e_call, def_flag } from "../../src/util.js";

export const command = ['init [files..]'];
export const desc = 'Initialize project to use Eask';
export const builder = yargs => yargs
  .positional(
    '[files..]', {
      description: 'files to use with `--from` flag',
      type: 'array',
    })
  .options({
    'from': {
      description: 'build from an existing package',
      requiresArg: true,
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
  });

export const handler = async (argv) => {
  if (argv.from) {
    switch (argv.from) {
    case 'cask':
    case 'eldev':
    case 'keg':
    case 'source':
      await e_call(argv, 'init/' + argv.from
                        , def_flag(argv.from, '--from', argv.from)
                        , argv.files);
      break;
    default:
      console.warn(`Invalid argument, from: ${argv.from}`);
      break;
    }
  } else {
    await e_call(argv, 'core/init');
  }
};
