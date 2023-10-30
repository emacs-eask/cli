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

exports.command = ['create <type>'];
exports.desc = 'Create a new elisp project';
exports.builder = function (yargs) {
  yargs.usage(`${exports.desc}

Usage: eask create <type> [options..]`)
    .commandDir('../create/')
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (UTIL.cmd_count() == 1) {
    yargs.positional(
      '<type>', {
        description: 'type of the creation',
      });
  }
}

exports.handler = async (argv) => { };
