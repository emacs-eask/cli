/**
 * Copyright (C) 2022-2025 the Eask authors.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

"use strict";

const os = require('os');
const path = require('path');

global.UTIL = require('./util');
global.IS_PKG = path.basename(process.execPath).startsWith('eask');

/* Environment PATH */
global.EASK_HOMEDIR = os.homedir().replace(/\\/g, '/') + '/.eask/';

global.EASK_EMACS = process.env.EMACS || process.env.EASK_EMACS || "emacs";
process.env.EMACS = EASK_EMACS;  // Set back the value.

/* Titles */
global.TITLE_CMD_OPTION = 'Command Options:';
global.TITLE_PROXY_OPTION = 'Proxy Options:';

/* Exit Code
 *
 * This must match with the variable `eask--exit-code` in
 * the file `lisp/_prepare.el`.
 */
global.EXIT_SUCCESS = 0;
global.EXIT_FAILURE = 1;
global.EXIT_MISUSE  = 2;

/* CI */
global.GITHUB_ACTIONS = (process.env.GITHUB_WORKSPACE !== undefined);
