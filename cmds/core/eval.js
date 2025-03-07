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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

"use strict";

const fs = require('fs');
const child_process = require("child_process");

exports.command = ['eval [form]'];
exports.desc = 'Evaluate lisp form with a proper PATH';
exports.builder = yargs => yargs
  .positional(
    '[form]', {
      description: 'lisp form',
      type: 'string',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/eval', argv.form);

  if (!fs.existsSync(EASK_HOMEDIR)) {
    return;
  }

  let epf = EASK_HOMEDIR + 'exec-path';
  let lpf = EASK_HOMEDIR + 'load-path';

  if (!fs.existsSync(epf) || !fs.existsSync(lpf)) {
    return;
  }

  process.env.PATH = fs.readFileSync(epf, 'utf8');
  process.env.EMACSLOADPATH = fs.readFileSync(lpf, 'utf8');;

  let expressions = process.argv.slice(3);

  // XXX: Just replace `form` property, this is combined expression
  argv.form = expressions.join(' ');

  let cmd = ['emacs', '--batch', '--eval', argv.form];

  let proc = child_process.spawn(UTIL.cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });
};
