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

var EASK_FILE;

var instance;  /* `readline` instance */

exports.command = ['init'];
exports.desc = 'create new Eask file in current directory';

exports.handler = async ({}) => {
  await create_eask_file();
};

async function create_eask_file(dir) {
  let basename = path.basename(process.cwd());
  instance = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  EASK_FILE = path.join(process.cwd(), '/Eask');

  if (fs.existsSync(EASK_FILE) || fs.existsSync(EASK_FILE + 'file')) {
    console.log('Eask-file is already exists');
    process.exit(0);
  }

  let name, version, description, entry_point, emacs_version, website_url, keywords;
  await ask(`package name: (${basename}) `, (answer) => { name = answer || basename; });
  await ask(`version: (1.0.0) `, (answer) => { version = answer || '1.0.0'; });
  await ask(`description: `, (answer) => { description = answer; });
  await ask(`entry point: (${basename}.el) `, (answer) => { entry_point = answer || `${basename}.el`; });
  await ask(`emacs version: (26.1) `, (answer) => { emacs_version = answer || '26.1'; });
  await ask(`website: `, (answer) => { website_url = answer; });
  await ask(`keywords: `, (answer) => { keywords = answer; });

  keywords = keywords.split(/[, ]+/).join('" "');

  let content = `(package "${name}"
         "${version}"
         "${description}")

(website-url "${website_url}")
(keywords "${keywords}")

(package-file "${entry_point}")

(script "test" "echo \\\"Error: no test specified\\\" && exit 1")

(source "gnu")

(depends-on "emacs" "${emacs_version}")
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
}

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

module.exports.create_eask_file = create_eask_file;
