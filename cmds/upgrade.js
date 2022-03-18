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

exports.command = 'upgrade [names..]';
exports.desc = 'upgrade packages';
exports.builder = {
  names: {
    description: 'packages to upgrade',
    requiresArg: false,
    type: 'array',
  },
  global: {
    description: 'upgrade globally `~/.emacs.d`, respect to variable `user-emacs-directory`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
  force: {
    description: 'force upgrade',
    alias: 'f',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ names, global, force }) => {
  await util.e_call('upgrade', names
                    , util.def_flag(global, '-g')
                    , util.def_flag(force, '-f'));
};
