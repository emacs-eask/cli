/**
 * Copyright (C) 2023 Jen-Chieh Shen
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

exports.command = ['recipe [destination]'];
exports.desc = 'Generate recipe file';
exports.builder = yargs => yargs
  .positional(
    'destination', {
      description: 'destination path/folder',
      alias: 'dest',
      type: 'string',
    })
  .options({
    'yes': {
      description: 'assume the answer to all prompts is yes',
      alias: 'y',
      type: 'boolean',
      group: TITLE_CMD_OPTION,
    },
  });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'generate/recipe'
                    , argv.dest
                    , UTIL.def_flag(argv.yes, '--yes'));
};
