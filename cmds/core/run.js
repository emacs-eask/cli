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

exports.command = ['run [names..]', 'run-script [names..]'];
exports.desc = 'Run the script named [names..]';
exports.builder = {
  names: {
    description: 'specify scripts to run',
    requiresArg: false,
    type: 'array',
    group: TITLE_CMD_OPTION,
  },
};

exports.handler = async (argv) => {
  // setup environment, so Emacs can receive it
  process.env.EASK_HOMEDIR = global.EASK_HOMEDIR;

  await UTIL.e_call(argv, 'core/run', argv.names);

  if (!fs.existsSync(global.EASK_HOMEDIR)) {
    return;
  }

  let run = global.EASK_HOMEDIR + 'run';

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

  console.log('[RUN]: ' + command);
  let proc = child_process.spawn(command, { stdio: 'inherit', shell: true });

  proc.on('close', function (code) {
    if (code == 0) {
      startCommand(commands, ++count);  // start next command!
      return;
    }
    throw 'Exit with code: ' + code;
  });
}
