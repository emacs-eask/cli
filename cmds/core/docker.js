/**
 * Copyright (C) 2023 Jen-Chieh Shen
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

const path = require('path');
const child_process = require("child_process");

exports.command = ['docker <version> [args..]'];
exports.desc = 'Launch specified Emacs version in a Docker container';
exports.builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  yargs.positional('<version>', {
    description: 'Emacs version to test',
    type: 'string',
  });
};

exports.handler = async (argv) => {
  if (!UTIL.which('docker')) {
    console.log("Docker is not installed (cannot find `docker' executable)");
    return;
  }

  let project_dir = convert_path(process.cwd());
  if (!project_dir.startsWith('/')) {
    project_dir = '/' + project_dir;
  }
  let container_dir = '/' + path.basename(project_dir);

  let container_arg = project_dir + ':' + container_dir;

  let default_cmd = ['docker', 'run', '--rm',
                     '-v', container_arg,
                     '-w', container_dir,
                     'silex/emacs:' + argv.version + '-ci-eask',
                     'eask'];
  let rest = process.argv.slice(4);
  let cmd = default_cmd.concat(rest);

  let proc = child_process.spawn(UTIL.cli_args(cmd), { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) return;
    process.exit(code);
  });
};

/**
 * Convert path so docker can recognize!
 * @param { String } path - Path to convert.
 */
function convert_path(path) {
  return UTIL.slash(path).replaceAll(':', '');
}
