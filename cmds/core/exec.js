/**
 * Copyright (C) 2022-2023 Jen-Chieh Shen
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
 * along with GNU Emacs; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

"use strict";

const fs = require('fs');
const child_process = require("child_process");

exports.command = ['exec [args..]'];
exports.desc = 'Execute command with correct environment PATH set up';
exports.builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  //console.log(yargs.getOptions());
};

exports.handler = async (argv) => {
  // setup environment, so Emacs can receive it
  process.env.EASK_HOMEDIR = global.EASK_HOMEDIR;

  let cmd = process.argv.slice(3);

  await UTIL.e_call(argv, 'core/exec', '--', cmd);

  if (!fs.existsSync(global.EASK_HOMEDIR)) {
    return;
  }

  let epf = global.EASK_HOMEDIR + 'exec-path';
  let lpf = global.EASK_HOMEDIR + 'load-path';

  process.env.PATH = fs.readFileSync(epf, 'utf8');
  process.env.EMACSLOADPATH = fs.readFileSync(lpf, 'utf8');;

  let proc = child_process.spawn(UTIL.cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    throw 'Exit with code: ' + code;
  });
};
