/**
 * $File: upgrade.js $
 * $Date: 2022-03-13 22:27:11 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const util = require("../util");

exports.command = "upgrade";
exports.desc = "Upgrade all packages";

exports.handler = async ({}) => {
  await util.call('upgrade');
};
