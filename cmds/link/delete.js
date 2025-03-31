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

exports.command = ['delete [names..]', 'remove [names..]'];
exports.desc = 'Delete local linked packages';
exports.builder = yargs => yargs
  .positional(
    '[names..]', {
      description: 'name of the link, accept array',
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'link/delete', argv.names);
};
