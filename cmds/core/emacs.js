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

const os = require('os');
const fs = require('fs');
const path = require('path');
const child_process = require("child_process");

exports.command = ['emacs [args..]'];
exports.desc = 'Execute emacs with the appropriate environment';
exports.builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  yargs.strict(false);
};

exports.handler = async (argv) => {
  let s_path = UTIL.el_script('core/emacs');

  let default_cmd = [EASK_EMACS, '-Q', '-l', s_path];
  let rest = process.argv.slice(3);
  let cmd = default_cmd.concat(rest);

  UTIL.setup_env();
  let proc = child_process.spawn(UTIL.cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });
};
