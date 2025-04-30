/**
 * Copyright (C) 2023-2025 the Eask authors.
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

exports.command = ['license <name>'];
exports.desc = 'Generate the LICENSE file';
exports.builder = yargs => yargs
  .positional(
    '<name>', {
      description: 'name of the license',
      type: 'string',
    })
  .options({
    'output': {
      description: 'output result to a file; the default is `LICENSE`',
      alias: 'o',
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
  });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'generate/license'
                    , argv.name
                    , UTIL.def_flag(argv.output, '--output', argv.output));
};
