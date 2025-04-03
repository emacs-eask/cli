/**
 * Copyright (C) 2025 the Eask authors.
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

exports.command = ['install-vc [specs..]'];
exports.desc = 'Install packages directly from the version control';
exports.builder = yargs => yargs
  .positional(
    '[specs..]', {
      description: 'vc specification to install as packages',
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/install-vc', argv.specs);
};
