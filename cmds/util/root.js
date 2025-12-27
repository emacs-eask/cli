/**
 * $File: root.js $
 * $Date: 2025-12-27 20:29:35 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2025 by Shen, Jen-Chieh $
 */

"use strict";

exports.command = ['root'];
exports.desc = 'Display the effective installation directory of your Emacs packages';

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'util/root');
};
