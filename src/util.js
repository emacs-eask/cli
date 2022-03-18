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
const child_process = require("child_process");

/*
 * Remove `undefined` item from the array
 * @param { Array } arr - target array
 */
function _remove_undefined(arr) {
  return arr.filter(elm => { return elm !== undefined; });
}

/**
 * Remove undefined item and insert space between arguments.
 * @param { ellipsis } ...arr - an array to insert space between each item.
 */
function _join_spc(...arr) {
  arr = _remove_undefined(arr);
  let _result = [];
  for (let index = 0; index < arr.length; ++index) {
    let _item = arr[index];
    if (Array.isArray(_item)) {
      _result.push(_join_spc(..._item));
    } else {
      _result.push(_item);
    }
  }
  return _result.join(' ');
}

/* Return plugin directory */
function _plugin_dir() { return path.join(__dirname, '..'); }

/**
 * Define flag with proper alias flag.
 * @param { boolean } arg - argument receive from yargs.
 * @param { string } name - the flag representation in alias.
 */
function def_flag(arg, name, val = undefined) {
  if (arg === undefined)
    return undefined;
  if (val === undefined)
    return '--eask' + name;
  return '--eask' + name + ' ' + val;
}

/* Send error code, and exit the program. */
function _exit_error(code) {
  process.exitCode = code;
  throw 'Uncaught exception error: ' + code;
}

/* Display all terminal output */
function _exec_out(error, stdout, stderr) {
  if (stdout) { console.log(stdout); }
  //if (error) { console.log(error); }  /* ignore node error */
  if (stderr) {
    console.log(stderr);
    // XXX The method here to detect error, and send exit code is fragile.
    // The better way should just grab it from Emacs program itself; but Emacs
    // return exit code immediately with `child_process.exec` call
    if (stderr.includes ('Error: ')) {
      _exit_error(1);
    }
  }
}

/**
 * Call emacs process
 * @param { string } script - name of the script from `../lisp`
 * @param { string } args - the rest of the arguments
 */
async function e_call(script, ...args) {
  let _script = 'lisp/' + script + '.el';
  let _path = path.join(_plugin_dir(), _script);
  let cmd = _join_spc('emacs', '-Q', '-nw', '--batch', '--script' , _path, args);
  console.log('Starting Eask...');
  console.log('~');
  console.log('~  $ ' + cmd);
  console.log('~');
  await child_process.exec(cmd, _exec_out);
}

/*
 * Module Exports
 */
module.exports.def_flag = def_flag;
module.exports.e_call = e_call;
