/**
 * Copyright (C) 2023-2026 the Eask authors.
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

import path from 'path';
import child_process from "child_process";
import { which, cli_args, slash } from '../../src/util.js';

export const command = ['docker <version> [args..]'];
export const desc = 'Launch specified Emacs version in a Docker container';
export const builder = async (yargs) => {
  yargs.help(false);
  yargs.version(false);
  yargs.getOptions().narg = [];
  yargs.strict(false);
  yargs.positional('<version>', {
    description: 'Emacs version to test',
    type: 'string',
  });
};

export const handler = async (argv) => {
  if (!which('docker')) {
    console.warn("Docker is not installed (cannot find `docker' executable)");
    return;
  }

  let project_dir = convert_path(process.cwd());
  if (!project_dir.startsWith('/')) {  // XXX: Ensure compatible to Unix path!
    project_dir = '/' + project_dir;
  }
  let container_dir = '/' + path.basename(project_dir);

  let container_arg = project_dir + ':' + container_dir;

  // Find out the image name.
  let image = argv.version;

  if (valid_version_string(argv.version))
    image = 'silex/emacs:' + argv.version + '-eask';

  let default_cmd = ['docker', 'run', '--rm',
                     '-v', container_arg,
                     '-w', container_dir,
                     image,];
  let rest = process.argv.slice(4);

  // If no argument; we enter the container directly!
  if (rest.length == 0)
    default_cmd.splice(2, 0, '-it');
  else
    default_cmd.push('eask');

  let cmd = default_cmd.concat(rest);

  let proc = child_process.spawn(cli_args(cmd), { stdio: 'inherit', shell: true });

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
  return slash(path).replaceAll(':', '');
}

/**
 * Return true when string is a valid version string.
 * @param { String } str - The version string.
 */
function valid_version_string(str) {
  const ver_regex = /^\d+(\.\d+)*(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$/;
  return ver_regex.test(str);
}
