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
 * along with GNU Emacs; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

"use strict";

exports.command = ['compile [names..]'];
exports.desc = 'Byte compile all Emacs Lisp files in the package';
exports.builder = {
  names: {
    description: 'specify files to byte-compile',
    requiresArg: false,
    type: 'array',
    group: TITLE_CMD_OPTION,
  },
};

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/compile', argv.names);
};
