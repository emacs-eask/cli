/**
 * $File: upgrade.js $
 * $Date: 2022-03-13 22:27:11 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = 'upgrade [name]';
exports.desc = 'Upgrade packages';
exports.builder = {
  name: {
    description: 'Name of the package to upgrade',
    requiresArg: false,
    type: 'string',
  },
  global: {
    description: 'Upgrade globally `~/.emacs.d`, respect to variable `user-emacs-directory`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
  force: {
    description: 'Force upgrade',
    alias: 'f',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ name, global, force }) => {
  await util.e_call('upgrade', name
                    , util.def_flag(global, '-g')
                    , util.def_flag(force, '-f'));
};
