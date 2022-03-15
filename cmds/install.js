/**
 * $File: install.js $
 * $Date: 2022-03-13 21:31:28 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = 'install [name]';
exports.desc = 'install packages';
exports.builder = {
  name: {
    description: 'name of the package to install',
    requiresArg: false,
    type: 'string',
  },
  global: {
    description: 'install packages from default `.emacs.d`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ name, global }) => {
  await util.e_call('install', name, util.def_flag(global, '-g'));
};
