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

exports.command = ['link <action>'];
exports.desc = 'Manage links';
exports.builder = function (yargs) {
  yargs.usage(`${exports.desc}

Usage: eask link <action> [options..]`)
    .commandDir('../link/')
    .demandCommand();

  /* XXX: Configure only in the menu. */
  if (UTIL.cmd_count() == 1) {
    yargs.positional(
      '<action>', {
        description: 'type of link action',
      });
  }
}

exports.handler = async (argv) => { };
