/**
 * Copyright (C) 2022-2023 the Eask authors.
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

const child_process = require("child_process");

exports.command = ['upgrade-eask', 'upgrade-self'];
exports.desc = 'Upgrade Eask itself';

exports.handler = async (argv) => {
  process.chdir(UTIL.plugin_dir());
  let proc = child_process.spawn('git', ['pull'], { stdio: 'inherit' });

  // You would just need to register the error event, or else it can't print
  // the help instruction below.
  proc.on('error', function () { });

  proc.on('close', function (code) {
    if (code == 0) {
      process.stdout.write('✓ Done upgrading Eask to the latest version');
      return;
    }
    // Help instruction here!
    console.log('');
    console.log('');
    console.log('✗ Failed to upgrade Eask, possible causes are:');
    console.log('');
    console.log('  [1] Make sure you have git installed and has the right permission');
    console.log('  [2] You install Eask with other package management tool');
    console.log('');
    console.log('For example, if you have installed eask with npm:');
    console.log('');
    console.log('    $ npm install -g @emacs-eask/cli@latest');
    console.log('');
    process.stdout.write('Visit https://emacs-eask.github.io/Getting-Started/Install-Eask/ to see all available install options');
  });
};
