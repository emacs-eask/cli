/**
 * Copyright (C) 2023 the Eask authors.
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
    console.warn("Docker is not installed (cannot find `docker' executable)");
    return;
  }

  let project_dir = convert_path(process.cwd());
  if (!project_dir.startsWith('/')) {  // XXX: Ensure compatible to Unix path!
    project_dir = '/' + project_dir;
  }
  let container_dir = '/' + path.basename(project_dir);

  let container_arg = project_dir + ':' + container_dir;

  let default_cmd = ['docker', 'run', '--rm',
                     '-v', container_arg,
                     '-w', container_dir,
                     'silex/emacs:' + argv.version + '-ci-eask',];
  let rest = process.argv.slice(4);

  // If no argument; we enter the container directly!
  if (rest.length == 0)
    default_cmd.splice(2, 0, '-it');
  else
    default_cmd.push('eask');

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
