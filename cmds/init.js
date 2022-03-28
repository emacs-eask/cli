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

const path = require('path');
const fs = require('fs');
const readline = require('readline');

const EASK_FILE = path.join(process.cwd(), '/Eask');

var instance;  /* `readline` instance */

exports.command = ['init'];
exports.desc = 'create new `Eask` file in current directory';

exports.handler = async ({}) => {
  let basename = path.basename(process.cwd());
  instance = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  if (fs.existsSync(EASK_FILE)) {
    console.log('Eask file is already exists');
    process.exit(0);
  }

  let name, version, description, entry_point;
  await ask(`package name: (${basename}) `, (answer) => { name = answer || basename; });
  await ask(`version: (1.0.0) `, (answer) => { version = answer || '1.0.0'; });
  await ask(`description: `, (answer) => { description = answer; });
  await ask(`entry-point: (${basename}.el) `, (answer) => { entry_point = answer || `${basename}.el`; });

  let content = `(source "gnu")

(package "${name}" "${version}" "${description}")

(package-file "${entry_point}")
`;

  await ask(`About to write to ${EASK_FILE}:

${content}

Is this OK? (yes) `,
            (answer) => {
              if (answer == '' || answer == 'yes') {
                fs.writeFile(EASK_FILE, content, (err) => { if (err) console.log(err); });
              }
            });
  instance.close();
};

/*
 * Ask question with callback
 * @param { string } question - question to ask for user input
 * @param { callback } callback - callback with user's answer
 */
function ask(question, callback) {
  return new Promise((resolve, reject) => {
    instance.question(question, (answer) => { callback(answer); resolve(); });
  });
}
