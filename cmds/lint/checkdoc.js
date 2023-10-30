/**
 * Copyright (C) 2022-2023 Jen-Chieh Shen
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

exports.command = ['checkdoc [files..]'];
exports.desc = 'Run checkdoc';
exports.builder = yargs => yargs
  .positional(
    '[files..]', {
      description: 'files you want checkdoc to run on',
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'lint/checkdoc', argv.files);
};
