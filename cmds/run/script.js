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

exports.command = ['script [names..]'];
exports.desc = 'Run script nameds [names..]';
exports.builder = yargs => yargs
  .positional(
    '[names..]', {
      description: 'specify scripts to run',
      type: 'array',
    });

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'run/script', argv.names);

  if (!fs.existsSync(EASK_HOMEDIR)) {
    return;
  }

  let run = EASK_HOMEDIR + 'run';

  if (!fs.existsSync(run)) {
    return;
  }

  // this contain the full command!
  let instruction = fs.readFileSync(run, 'utf8');
  let commands = instruction.split('\n').filter(element => element);

  let count = 0;
  startCommand(commands, count);
};

/**
 * Recursive to execute commands in order.
 *
 * @param { array } commands - An array of commands to execute.
 * @param { integer } count - The current executing command's index.
 */
function startCommand(commands, count) {
  if (commands.length <= count)
    return;

  let command = commands[count];

  console.warn('[RUN]: ' + command);

  let proc = spawn(command, { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) {
      startCommand(commands, ++count);  // start next command!
      return;
    }
    process.exit(code);
  });
}

/**
 * Spawn process to avoid `MODULE_NOT_FOUND` not found error,
 * see https://github.com/vercel/pkg/issues/1356.
 *
 * @param { String } command - Command string.
 * @param { JSON } options - Process options.
 * @return Process object.
 */
function spawn(command, options) {
  if (IS_PKG && command.includes('eask ')) {
    let cmds = command.split(' ');
    cmds = replaceEaskExec(cmds);
    return child_process.spawn(process.execPath, cmds, options);
  }
  return child_process.spawn(command, options);
}

/**
 * Replace all possible eask/cli executable to snapshot executable.
 * @param { Array } cmds - Command array.
 * @return Return updated command array.
 */
function replaceEaskExec(cmds) {
  for (let index = 0; index < cmds.length; ++index) {
    if (cmds[index] == "eask") {
      cmds[index] = process.argv[1];  // XXX: This is `/snapshot/cli/eask`
    }
  }
  return cmds;
}
