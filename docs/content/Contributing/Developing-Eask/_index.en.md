---
title: 🔨 Developing Eask
weight: 20
---

{{< toc >}}

## 🚩 Prerequisites

To make changes to Eask, you should have:

1. [Node.js][] for the development environment.
2. [npm][] for the package manager.
3. [Emacs][], 26.1 or above!

## 📝 Building

To build the development environment, you would have to install Eask using
the [build from source][Build from source]
method. Make sure you have set up the environment PATH variable, so you can call
`eask` from the terminal.

After you have stepped through the installation, try:

```sh
eask locate
```

It should print out the location of the `eask` executable.
You should be able to identify the Eask executable's location,
even you have multiple Eask versions installed!

## 🧪 Testing

Local testing for Eask is done using the [Jest][] testing framework.
Jest is a mature and well supported testing framework written in Javascript.
Jest was chosen for much the [same reasons as Javascript][Why JS?] was chosen for this project.
In addition, Jest is easy to learn and has built in support for snapshot based testing.

### ⚗️ Running Tests

If you have not done so already, run `npm install --dev`

Always run from the project root (i.e. same directory as `package.json`)

- run all tests              `npm run test`
- run a single test          `npm run test path/to/test.js`
- run tests with full output `npm run test-debug`
- remove files created during test `npm run test-reset`

Since `npm run test` just runs Jest, you can also pass Jest options to the
commands above. For example:

- run tests whose names (in `test()` blocks) match `npm run test -t 'eask lint .*'`
- re-run failed tests `npm run test -f`

### 🌍 Environment Vars

| Name           | Type    | Default | Meaning                                                                                           |
|:---------------|:--------|:--------|:--------------------------------------------------------------------------------------------------|
| `ALLOW_UNSAFE` | `bool*` | false   | Run tests in `testUnsafe` blocks. These can **overwrite** your personal emacs config or settings. |
| `DEBUG`        | `bool*` | false   | Print full output from commands in test.                                                          |
| `EASK_COMMAND` | path    | "eask"  | Path to Eask. Usually either `eask` or `$PWD/bin/eask` to use local changes.                      |
| `TIMEOUT`      | number  | 25000   | Command timeout in ms. Note this is different than Jest's timeout, which should be greater.       |

{{< hint info >}}
💡 Node.js handles environment variables as strings. That means that `DEBUG=0`, `DEBUG=false` all _enable_ `DEBUG`.
The only setting which disables a boolean flag is null, for example `DEBUG=`.
{{< /hint >}}

### 🔬 How to Write a Test

**Folder structure**

Tests should be in `test/js`.
Related tests should be in the same file with the suffix `.test.js` and are usually named after the
feature or command that they test, for example `link.test.js` tests the `eask link` command.

If the test needs some specific project files, put them in a new folder within `test/js`
For example, files in `test/js/foo` would be expected to be for `foo.test.js`.

The exception is `test/js/empty`, which is simply an empty folder.
If you use it, make sure to run `eask clean all` before your tests.

**Test File structure**

``` javascript
const { TestContext } = require("./helpers");

describe("emacs", () => {
  const ctx = new TestContext("./test/jest/empty");

  beforeAll(async () => await ctx.runEask("clean all"));
  afterAll(() => ctx.cleanUp);

  test("eask emacs --version", async () => {
    await ctx.runEask("emacs --version");
  });

  test("eask emacs --batch --eval", async () => {
    await ctx.runEask(
      'emacs --batch --eval "(require (quote ert))" --eval "(ert-deftest mytest () (should-not (display-graphic-p)))" -f ert-run-tests-batch',
    );
  });
});
```

In Jest, you group related tests using `describe`. Tests in the same `describe` block can share setup/teardown code,
can be disabled as a group and are grouped under the same heading in output.

`describe` blocks can be nested within other `describe` blocks.
It's a good idea to add a nested `describe` when tests run in different directories, or to match a "given, when, then" style of testing.

For each test directory you should create a new `TestContext` object.
All `runEask` commands will use the `TestContext`'s working directory.

Jest's tests are in `test` blocks. Note that `it` is an alias for `test`.
Tests can be selectively disabled in code, like so:

- `test.only(name, fn)` runs only that test in the file
- `test.skip(name, fn)` skips running the test but still prints its name
- `test.failing(name, fn)` invert the meaning of the test: it *should* fail.

The `expect` API matches values in different ways and usually prints a diff as part of the failure report.
See Jest's [expect()](https://jestjs.io/docs/expect) API for more info.

Uncaught errors thrown in a `test` block will fail it and report the error.
That's why many tests don't have an `expect` call, they simply check that the command succeeds.

Output from `runEask` is wrapped in a helper class `CommandOutput` which provides some transformation methods.
For example, if you have `const out = await ctx.runEask("analyze");`, then

- `out.combined()` concatenates both stdout and stderr as a string,
- `out.raw()` returns a plain object with just `stdout` and `stderr` as properties,
- `out.sanitized()` replaces all absolute paths that match the context's path

Since the class wraps the output of Node's `exec()` method you can still access `stdout` and `stderr`:

``` javascript
const { stderr, stdout } = await ctx.runEask("analyze");
```

Some commands create files or directories which should be removed after the test runs.
For example, `eask generate ignore elisp` creates a `.gitignore` file.
You can use the context's `removeFiles` method to remove files and directories relative
to the context's path:

``` javascript
  describe("Generating", () => {
    beforeAll(async () => await ctx.removeFiles(".gitignore"));
    afterAll(async () => await ctx.removeFiles(".gitignore"));

    it("eask generate ignore elisp", async () => {
      await ctx.runEask("generate ignore elisp");
    });
  });
```

Note that `removeFiles()` will recursively remove directories, but does not accept patterns.
So, to remove all files in `./test` just call `ctx.remove("test")`.
You can pass multiple files or directory names in single call: `ctx.remove("test", ".gitignore")`.

Use `TestContext.cleanUp()` to immediately abort any still-running commands that were called in that context.
Use this if Jest reports "open handles were detected" after a test run.
Note that `cleanUp` sends a signal to *all* processes started using the context's `runEask` command.
If used in an `afterEach` hook (i.e. after every test) it may result in failures.

### 🪧 Snapshots

[Snapshot tests](https://jestjs.io/docs/snapshot-testing) match the output of a test against a saved copy of the expected output.
For example:

``` javascript
test("eask analyze", async () => {
  const res = await ctx.runEask("analyze");
  expect(res.raw()).toMatchSnapshot();
});
```

The first time you run this Jest will create a new snapshot saved in an adjacent `__snapshot__` directory.

You should check this file in to version control as it forms a critical part of the test.
If the snapshot changes, you can update the snapshot by running Jest with option `-u`, for example,
`npm run test -- -u` will update all changed snapshots.

Any type of output can be used for a snapshot test. You could snapshot the contents of a file after changing it

``` javascript
test("eask analyze", async () => {
  await ctx.runEask("foo");
  const file = ctx.fileContents("Easkfile"); // file as a string
  expect(file).toMatchSnapshot();
});
```

Often snapshots will include data that varies with time or environment, for example timestamps or file paths.
The snapshot of `eask analyze` contains absolute file paths that will be different on every machine.

Output from `runEask` is wrapped in a helper class `CommandOutput` which provides some transformation methods.
The simplest just removes the absolute file paths:

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res.sanitized() // a CommandOutput object with absolute paths replaced by "~"
                      .raw();      // an object { stderr, stdout } suitable for snapshotting
  expect(resClean).toMatchSnapshot();
});
```

You can include custom replacement functions. Here, numbers will be replaced by `"x"`.
Then strings `"x:x"` will be replaced by `"y"`.

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res
    .sanitized(
      (x) => x.replace(/[0-9]+/g, "x"),
      (x) => x.replaceAll("x:x", "y"),
    )
    .raw();
  expect(resClean).toMatchSnapshot();
});
```

It's important to use the `g` regex flag so all occurrences of the match are replaced, or you could use `replaceAll`.
User provided functions run in addition to the default sanitize function and run in the order they were given.

### ⏱️ Timeouts

There are two timeout settings, one for Jest and one for Node's `exec()`.
All timeout values are in milliseconds.

Since the `exec()` timeout immediately terminates the running command and reports output, it is much better to
use that instead of Jest's timeout.

To change a timeout for a single command

``` javascript
ctx.runEask("analyze", { timeout: 10000})
```

To change the global timeout for a single run, use the env var

``` shell
env TIMEOUT=30000 npm run test
```

To change the global timeout permanently, set the default in `./helpers.js`.

If you change either global timeout, **make sure the global Jest timeout is greater** by setting it in `package.json`

``` json
"jest": {
  "rootDir": "./test/jest",
  "testTimeout": 40000
}
```

### 📜 Patterns

Here are some common patterns for testing commands.
Each of these assumes that `ctx` is a `TestContext` object.

**Check a command succeeds:**

``` javascript
test("eask analyze", async () => {
  await ctx.runEask("analyze");
});
```

Uncaught errors thrown in a `test` block will fail the test and report the error.
Failed commands will include stderr and stdout.

**Check a command fails:**

``` javascript
test("eask analyze", async () => {
  await expect(ctx.runEask("analyze")).rejects.toThrow();
});
```

**Check a command fails with a specific code:**

``` javascript
test("eask link add should error", async () => {
  // the error object should have property code = 1
  await expect(ctx.runEask("link add")).rejects.toMatchObject({
    code: 1,
  });
});
```

**Check a command produces some output:**

``` javascript
test("eask analyze", async () => {
  const out = await ctx.runEask("analyze");
  expect(out.stderr).toMatch("success"); // should apppear as a substring
  // If you want to check both `stderr` and `stdout`, just concatenate them
  expect(out.stdout + "/n" + out.stderr).toMatch("success");
  // Same thing using helper methods
  expect(out.combined()).toMatch("success");
});
```

**Check command output against a snapshot:**

Simple output matching

``` javascript
test("eask analyze", async () => {
  const res = await ctx.runEask("analyze");
  expect(res).toMatchSnapshot();
});
```

Update all changed snapshots: `npm run test -- -u`

Remove absolute file paths from output:

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res.sanitized() // a CommandOutput object with absolute paths replaced by "~"
                      .raw();      // an object { stderr, stdout } suitable for snapshotting
  expect(resClean).toMatchSnapshot();
});
```

Apply custom transformations for sanitizing output:

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res
    .sanitized(
      (x) => x.replace(/[0-9]+/g, "x"),
      (x) => x.replace(/x:x/g, "y"),
    )
    .raw();
  expect(resClean).toMatchSnapshot();
});
```

User provided functions run in addition to the default sanitize function and run in the order they were given.

**Commands which modify the user's environment:**

For example, commands which use `-c` or `-g` options.

``` javascript
const { testUnsafe } = require('./helpers');

// this will only run if ALLOW_UNSAFE is != 0
testUnsafe("global install", async () => {
  // this installs in ~/.eask and changes ~/Eask
  await ctx.runEask("install -g foo");
});
```

### 🩺 Common Problems

- When using `runEask()`, pass only the Eask *arguments*, not the `eask` command itself.
- Always `await` any expressions that trigger commands.
- When using `expect(...).rejects` it should be awaited so that the promise rejects before the test completes.
- The folder argument to `TestContext` should be relative to project root, if it doesn't exist you may get an error `ENOENT`
- If you get an error from Jest reporting open handles, then try using `afterAll(() => ctx.cleanUp())`
- There are two timeout values: one used for Jest (set in `package.json`), and one used for `node.exec`, set via env var in `./helpers.js`.
The `node.exec` timeout is set lower than the Jest one, so changing timeout values for tests or by `jest.setTimeout` usually won't
have an effect. Instead set the timeout on the command itself `runEask("eask emacs", { timeout: 100000 })`


<!-- Links -->

[Build from source]: https://emacs-eask.github.io/Getting-Started/Install-Eask/#-build-from-source
[Why JS?]: https://emacs-eask.github.io/FAQ/#-why-javascript

[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/
[yargs]: https://github.com/yargs/yargs
[Emacs]: https://www.gnu.org/software/emacs/

[Jest]: https://jestjs.io
