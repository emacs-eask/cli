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
exports.desc = 'Install package';
exports.builder = {
  name: {
    description: 'Name of the package to install',
    requiresArg: false,
    type: 'string',
  },
  global: {
    description: 'Install to default `.emacs.d`, respect to variable `user-emacs-directory`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ name, global }) => {
  await util.call('install', name, (global) ? '-g' : undefined);
};
