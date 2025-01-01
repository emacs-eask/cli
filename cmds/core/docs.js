/**
 * Copyright (C) 2024-2025 the Eask authors.
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

exports.command = ['docs [names..]', 'doc [names..]'];
exports.desc = 'Build documentation';
exports.builder = yargs => yargs
  .positional(
    '[names..]', {
      description: 'specify source files to scan',
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
  });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/docs'
                    , argv.names
                    , UTIL.def_flag(argv.dest, '--dest', argv.dest));
};
