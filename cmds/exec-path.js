/**
 * $File: exec-path.js $
 * $Date: 2022-03-20 02:45:07 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = 'exec-path';
exports.desc = 'print the exec-path from workspace';

exports.handler = async ({}) => {
  await util.e_call('exec-path');
};
