---
title: 🖥️ CLI & Yargs
weight: 700
---

The yargs command file is written in JavaScript, and located under the **cmds**
folder. Each file under, will be named with convention `[command_name].js`. This
file should define basic command-line parsing rules and correctly prepare data
to feed the Emacs session.

Let's look at the file `cmds/core/archives.js`:

```js
exports.command = ['archives', 'sources'];  // alias to sources
exports.desc = 'list out all package archives';

exports.handler = async (argv) => {
  await UTIL.e_call(argv, 'core/archives');
};
```

This is a standard yargs command file, which contains all the information we
need to pass it to the Emacs session.

* **exports.command** is the argument pattern, but it also accepts alias (array)
* **exports.desc** is the command description
* **exports.handler** is an asynchronous function that handles command execution
* **UTIL** is a global variable that points to the `src/util.js` module.
* **`'core/archives'`** is the elisp file under **lisp** folder (without `.el` extension).

`eask` is a JavaScript file that holds all our global options.

```js
yargs
  .usage('Usage: eask <command> [options..]')
  .help(
    'help',
    'Show usage instructions.'
  )
  .option('global', {
    description: 'change default workspace to ~/.emacs.d/',
    alias: 'g',
    type: 'boolean',
  })
..
```

For **local** options, please use `exports.builder` and specify under its
command file.

See
[yargs/docs/advanced.md](https://github.com/yargs/yargs/blob/main/docs/advanced.md),
the official documentation for more information and getting a better explanation
would help!
