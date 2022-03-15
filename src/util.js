/**
 * $File: util.js $
 * $Date: 2022-03-13 22:04:21 $
 * $Revision: $
 * $Creator: Jen-Chieh Shen $
 * $Notice: See LICENSE.txt for modification and distribution information
 *                   Copyright © 2022 by Shen, Jen-Chieh $
 */

"use strict";

const path = require('path');
const { exec } = require("child_process");

/*
 * Remove `undefined` item from the array
 * @param { Array } arr - target array
 */
function _remove_undefined(arr) {
  return arr.filter(elm => { return elm !== undefined; });
}

/**
 *
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

/* Display all terminal output */
function _exec_out(error, stdout, stderr) {
  if (stdout) console.log(stdout);
  if (error) console.log(error);
  if (stderr) console.log(stderr);
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
  console.log("[Eask]: " + cmd);
  await exec(cmd, _exec_out);
}

/*
 * Module Exports
 */
module.exports.def_flag = def_flag;
module.exports.e_call = e_call;
