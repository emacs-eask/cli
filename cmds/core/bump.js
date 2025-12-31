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

exports.command = ['bump [levels..]'];
exports.desc = UTIL.hide_cmd('Bump version for your project');
exports.builder = yargs => yargs
  .positional(
    '[levels..]', {
      description: "version level to bump; accept `major', `minor' or `patch'",
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/bump'
                    , argv.levels);
};
