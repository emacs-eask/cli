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

const path = require('path');
const child_process = require("child_process");

/**
 * Form CLI arguments into a single string.
 * @param { Array } argv - Argument vector.
 */
function cli_args(argv) {
  let result = '';
  let first = true;
  argv.forEach(function (element) {
    // XXX: We wrap double quotes if the string contains spaces
    if (/\s/g.test(element)) {
      element = element.replaceAll('\"', '\\"');  // escape double quotes
      element = '\"' + element + '\"';
    }
    if (first)
      result += element;
    else
      result += ' ' + element;
    first = false;
  });
  return result;
}

/*
 * Remove `undefined` item from the array
 * @param { Array } arr - target array
 */
function _remove_undefined(arr) {
  return arr.filter(elm => { return elm !== undefined; });
}

/* Return plugin directory */
function plugin_dir() {
  let root = (IS_PKG) ? process.execPath : __dirname;
  return path.join(root, '..');
}

/**
 * Define flag with proper alias flag.
 * @param { boolean } arg - argument receive from yargs.
 * @param { string } name - the flag representation in alias.
 */
function def_flag(arg, name, val = undefined) {
  if (arg === undefined)
    return undefined;
  if (val === undefined)
    return ['--eask' + name];
  return ['--eask' + name, val];
}

/**
 * Setup the environment variables so Emacs could receive them.
 */
function setup_env() {
  if (GITHUB_ACTIONS) {
    /* XXX: isTTY flag will always be undefined in GitHub Actions; we will have
     * explicitly set environment variables.
     *
     * See https://github.com/actions/runner/issues/241
     */
    process.env.EASK_HASCOLORS = 'true';
  } else {
    if (process.stdout.isTTY !== undefined) {
      if (process.stdout.hasColors()) process.env.EASK_HASCOLORS = 'true';
    }
  }
}

/**
 * Handle global options
 *
 * @param { Object } argv - is a parsed object from yargs.
 */
function _global_options(argv) {
  let flags = [];
  /* Boolean type */
  flags.push(def_flag(argv.global, '-g'));
  flags.push(def_flag(argv.all, '-a'));
  flags.push(def_flag(argv.quick, '-q'));
  flags.push(def_flag(argv.force, '-f'));
  flags.push(def_flag(argv.development, '--dev'));
  flags.push(def_flag(argv.debug, '--debug'));
  flags.push(def_flag(argv.strict, '--strict'));
  flags.push(def_flag(argv['allow-error'], '--allow-error'));
  flags.push(def_flag(argv.insecure, '--insecure', argv.insecure));
  flags.push(def_flag(argv.timestamps, (argv.timestamps) ? '--timestamps' : '--no-timestamps'));
  flags.push(def_flag(argv['log-level'], (argv['log-level']) ? '--log-level' : '--no-log-level'));
  flags.push(def_flag(argv['log-file'], (argv['log-file']) ? '--log-file' : '--no-log-file'));
  flags.push(def_flag(argv['elapsed-time'], (argv['elapsed-time']) ? '--elapsed-time' : '--no-elapsed-time'));
  flags.push(def_flag(argv['color'], '--no-color'));
  /* Number type */
  flags.push(def_flag(argv.verbose, '--verbose', argv.verbose));
  /* String type */
  flags.push(def_flag(argv.proxy, '--proxy', argv.proxy));
  flags.push(def_flag(argv['http-proxy'], '--http-proxy', argv['http-proxy']));
  flags.push(def_flag(argv['https-proxy'], '--https-proxy', argv['https-proxy']));
  flags.push(def_flag(argv['no-proxy'], '--no-proxy', argv['no-proxy']));
  return flags;
}

/**
 * Form elisp script path.
 * @param { string } name - Name of the script without extension.
 */
function el_script(name) {
  let _script = 'lisp/' + name + '.el';
  let _path = path.join(plugin_dir(), _script);
  return _path;
}

/**
 * Call emacs process
 * @param { string } script - name of the script from `../lisp`
 * @param { string } args - the rest of the arguments
 */
async function e_call(argv, script, ...args) {
  return new Promise(resolve => {
    let _path = el_script(script);

    let cmd_base = ['emacs', '-Q', '--script', _path];
    let cmd_args = args.flat();
    let cmd_global = _global_options(argv).flat();
    let cmd = cmd_base.concat(cmd_args).concat(cmd_global);
    cmd = _remove_undefined(cmd);

    let env_status = (argv.global) ? 'global' : 'development';
    console.log(`Running Eask in the ${env_status} environment`);
    console.log('Press Ctrl+C to cancel.');
    console.log('');
    console.log('Executing script inside Emacs...');
    if (argv.verbose == 4)
      console.log('[DEBUG] emacs ' + cmd.join(' '));

    setup_env();
    let proc = child_process.spawn(cli_args(cmd), { stdio: 'inherit', shell: true });

    proc.on('close', function (code) {
      // if (code == 0) {
      //   resolve(code);
      //   return;
      // }
      throw new Error('Exit with code: ' + code);
    });
  });
}

/*
 * Module Exports
 */
module.exports.cli_args = cli_args;
module.exports.plugin_dir = plugin_dir;
module.exports.def_flag = def_flag;
module.exports.setup_env = setup_env;
module.exports.el_script = el_script;
module.exports.e_call = e_call;
