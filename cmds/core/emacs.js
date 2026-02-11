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

import child_process from "child_process";
import { EASK_EMACS } from '../../src/env.js';
import { el_script, setup_env, cli_args } from '../../src/util.js';

export const command = ['emacs [args..]'];
export const desc = 'Execute emacs with the appropriate environment';
export const builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  yargs.strict(false);
};

export const handler = async (argv) => {
  let s_path = el_script('core/emacs');

  let default_cmd = [EASK_EMACS, '-Q', '-l', s_path];
  let rest = process.argv.slice(3);
  let cmd = default_cmd.concat(rest);

  setup_env();
  let proc = child_process.spawn(cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });
};
