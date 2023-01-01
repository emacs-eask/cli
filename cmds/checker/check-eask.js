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

exports.command = ['check-eask [files..]'];
exports.desc = 'Run eask checker';
exports.builder = {
  files: {
    description: 'specify Eask-files for checker to lint',
    requiresArg: false,
    type: 'array',
  },
  json: {
    description: 'Output lint result in JSON format',
    type: 'boolean',
  },
};

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'checker/check-eask'
                    , argv.files
                    , UTIL.def_flag(argv.json, '--json', argv.json));
};
