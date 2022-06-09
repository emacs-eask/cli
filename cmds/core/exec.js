/**
 * Copyright (C) 2022 Jen-Chieh Shen
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

const os = require('os');
const fs = require('fs');
const path = require('path');
const child_process = require("child_process");

exports.command = ['exec [args..]'];
exports.desc = 'execute command with correct environment PATH set up';
exports.builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  //console.log(yargs.getOptions());
};

const EASK_HOMEDIR = os.homedir().replace(/\\/g, '/') + '/.eask/';

exports.handler = async (argv) => {
  process.env.EASK_HOMEDIR = EASK_HOMEDIR;  // setup environment, so Emacs can receive it

  let cmd = process.argv.slice(3);

  await UTIL.e_call(argv, 'core/exec', '--', cmd);

  if (!fs.existsSync(EASK_HOMEDIR)) {
    return;
  }

  let epf = EASK_HOMEDIR + 'exec-path';
  let lpf = EASK_HOMEDIR + 'load-path';

  process.env.PATH = fs.readFileSync(epf, 'utf8');
  process.env.EMACSLOADPATH = fs.readFileSync(lpf, 'utf8');;

  let program = cmd[0];
  let rest = cmd.slice(1);
  let proc = child_process.spawn(program, rest, { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    throw 'Exit with code: ' + code;
  });
};
