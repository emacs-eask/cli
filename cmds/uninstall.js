/**
 * $File: uninstall.js $
 * $Date: 2022-03-15 21:08:11 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = 'uninstall <name>';
exports.desc = 'uninstall packages';
exports.builder = {
  name: {
    description: 'name of the package to uninstall',
    requiresArg: false,
    type: 'string',
  },
  global: {
    description: 'uninstall packages from default `.emacs.d`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
  force: {
    description: 'force to uninstall packages',
    alias: 'f',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ name, global, force }) => {
  await util.e_call('uninstall', name
                    , util.def_flag(global, '-g')
                    , util.def_flag(force, '-f'));
};
