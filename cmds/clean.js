/**
 * $File: clean.js $
 * $Date: 2022-03-14 03:04:26 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = "clean";
exports.desc = "clean up local .eask directory";
exports.builder = {
  global: {
    description: 'caution, this will kill your entire ~/.emacs.d/ directory',
    alias: 'g',
    requiresArg: false,
    type: 'boolean',
  },
};

exports.handler = async ({ global }) => {
  await util.e_call('clean', util.def_flag(global, '-g'));
};
