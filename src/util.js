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

/**
 * Handle global options
 *
 * @param { Object } argv - is a parsed object from yargs.
 */
function _global_options(argv) {
  let flags = [];
  flags.push(def_flag(argv.global, '-g'));
  flags.push(def_flag(argv.force, '-f'));
  flags.push(def_flag(argv.development, '--dev'));
  return flags;
}

/**
 * Call emacs process
 * @param { string } script - name of the script from `../lisp`
 * @param { string } args - the rest of the arguments
 */
async function e_call(argv, script, ...args) {
  let _script = 'lisp/' + script + '.el';
  let _path = path.join(_plugin_dir(), _script);

  let cmd_base = ['-Q', '--batch', '--script', _path];
  let cmd_args = args;
  let cmd_global = _global_options(argv);
  let cmd = cmd_base.concat(cmd_args).concat(cmd_global);
  cmd = _remove_undefined(cmd);
  console.log('Starting Eask...');
  console.log('~');
  console.log('~  $ ' + cmd.join(' '));
  console.log('~');
  let process = child_process.spawn('emacs', cmd, { stdio: 'inherit' });

  process.on('close', function (code) {
    if (code == 0) return;
    throw 'Exit with code: ' + code;
  });
}

/*
 * Module Exports
 */
module.exports.def_flag = def_flag;
module.exports.e_call = e_call;
