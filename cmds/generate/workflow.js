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

exports.command = ['workflow <type>'];
exports.desc = 'Generate yaml files for CI/CD';
exports.builder = function (yargs) {
  yargs.usage(`${exports.desc}

Usage: eask generate workflow <type> [options..]`)
    .commandDir('./workflow/')
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (UTIL.cmd_count() == 2) {
    yargs.positional(
      '<type>', {
        description: 'type of the workflow',
      });
  }
}

exports.handler = async (argv) => { };
