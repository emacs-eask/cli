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

import child_process from 'child_process';
import { e_call } from "../../src/util.js";

export const command = ['package <name>', 'pkg <name>'];
export const desc = 'Create a new package';
export const builder = yargs => yargs
  .positional(
    '<name>', {
      description: 'new package name',
      type: 'string',
    });

const TEMPLATE_URL = 'https://github.com/emacs-eask/template-elisp';

export const handler = async (argv) => {
  const project_name = argv.name;

  let proc = child_process.spawn('git', ['clone', TEMPLATE_URL, project_name],
                                 { stdio: 'inherit' });

  // You would just need to register the error event, or else it can't print
  // the help instruction below.
  proc.on('error', function () { });

  proc.on('close', function (code) {
    if (code == 0) {
      console.warn('✓ Done cloning the elisp package template');
      console.warn('');
      process.chdir(project_name);
      _cloned(argv);
      return;
    }
    // Help instruction here!
    console.warn('✗ Error while cloning template project');
    console.warn('');
    console.warn('  [1] Make sure you have git installed and has the right permission');
    process.stderr.write(`  [2] Failed because of the target directory isn't empty`);
  });
};

/* Operations after _cloned */
async function _cloned(argv) {
  console.warn('Initialize the Eask-file for your project...');
  await e_call(argv, 'core/init');
  await e_call(argv, 'create/package');
}
