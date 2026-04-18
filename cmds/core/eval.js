/**
 * Copyright (C) 2022-2026 the Eask authors.
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

import fs from 'fs';
import child_process from "child_process";
import { EASK_HOMEDIR } from "../../src/env.js";
import { e_call, cli_args } from '../../src/util.js';

export const command = ['eval [form]'];
export const desc = 'Evaluate lisp form with a proper PATH';
export const builder = yargs => yargs
  .positional(
    '[form]', {
      description: 'lisp form',
      type: 'string',
    });

export const handler = async (argv) => {
  await e_call(argv, 'core/eval', argv.form);

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

  let proc = child_process.spawn(cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });
};
