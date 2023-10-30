/**
 * Copyright (C) 2022-2023 the Eask authors.
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

exports.command = ['ert [files..]'];
exports.desc = 'Run ert tests';
exports.builder = yargs => yargs
  .positional(
    '[files..]', {
      description: 'specify files to do ert tests',
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'test/ert', argv.files);
};
