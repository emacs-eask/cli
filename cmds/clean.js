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
exports.desc = "Clean up local .eask directory";

exports.handler = async ({}) => {
  await util.e_call('clean');
};
