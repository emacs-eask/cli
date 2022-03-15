/**
 * $File: info.js $
 * $Date: 2022-03-15 20:28:52 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../src/util");

exports.command = 'info';
exports.desc = 'Display environment information';

exports.handler = async ({ }) => {
  await util.e_call('info');
};
