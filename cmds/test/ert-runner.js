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

exports.command = ['ert-runner [files..]'];
exports.desc = 'Run ert tests using ert-runner';
exports.builder = yargs => yargs
  .positional(
    '[files..]', {
      description: 'specify files to do ert tests',
      type: 'array',
    })
  .options({
    'pattern': {
      description: 'Run tests matching the pattern',
      alias: 'p',
      requiresArg: true,
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
    'tags': {
      description: 'Run tests matching the tags',
      alias: 't',
      requiresArg: true,
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
    'reporter': {
      description: 'Set the reporter to use (e.g. "dot", "ert")',
      requiresArg: true,
      type: 'string',
      group: TITLE_CMD_OPTION,
    },
  });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'test/ert-runner', argv.files
                    , UTIL.def_flag(argv.pattern, '--pattern', argv.pattern)
                    , UTIL.def_flag(argv.tags, '--tags', argv.tags)
                    , UTIL.def_flag(argv.reporter, '--reporter', argv.reporter));
};
