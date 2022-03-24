/**
 * Copyright (C) 2022 Jen-Chieh Shen
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

const util = require("../src/util");

exports.command = "list-all";
exports.desc = "list all available packages";
exports.builder = {
  depth: {
    description: 'dependency depth level to print',
    requiresArg: true,
    type: 'number',
  },
};

exports.handler = async ({ global, depth }) => {
  await util.e_call('list-all'
                    , util.def_flag(global, '-g')
                    , util.def_flag(depth, '--depth', depth));
};
