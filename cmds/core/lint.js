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

exports.command = ['lint <type>'];
exports.desc = 'Run linter';
exports.builder = function (yargs) {
  yargs.usage(`${exports.desc}

Usage: eask lint <type> [options..]`)
    .commandDir('../lint/')
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (UTIL.cmd_count() == 1) {
    yargs.positional(
      '<type>', {
        description: 'type of the linter',
      });
  }
}

exports.handler = async (argv) => { };
