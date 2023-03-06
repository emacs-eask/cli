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

exports.command = ['search [queries..]'];
exports.desc = 'Search packages';
exports.builder = {
  query: {
    description: 'queries to search packages',
    requiresArg: false,
    type: 'array',
    group: TITLE_CMD_OPTION,
  },
  depth: {
    description: 'dependency depth level to print',
    requiresArg: true,
    type: 'number',
    group: TITLE_CMD_OPTION,
  },
};

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/search', argv.queries
                    , UTIL.def_flag(argv.depth, '--depth', argv.depth));
};
