/**
 * $File: list.js $
 * $Date: 2022-03-14 16:48:43 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = "list";
exports.desc = "List Emacs packages";
exports.builder = {
  global: {
    description: 'List it globally to `~/.emacs.d/`',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ global }) => {
  await util.e_call('list', util.def_flag(global, '-g'));
};
