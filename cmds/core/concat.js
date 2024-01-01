/**
 * Copyright (C) 2022-2024 the Eask authors.
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

exports.command = ['concat [names..]', 'concatenate [names..]'];
exports.desc = UTIL.hide_cmd('Concatenate elisp files');
exports.builder = yargs => yargs
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

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/concat'
                    , argv.names
                    , UTIL.def_flag(argv.dest, '--dest', argv.dest)
                    , UTIL.def_flag(argv.output, '--output', argv.output));
};
