---
title: Testing
weight: 20
---

{{< toc >}}

This describes how to run tests locally, and how to write new tests for Eask.

Local testing for Eask is done using the [Jest](https://jestjs.io) testing framework.

- TODO why jest

A custom `TestContext` class is used to manage the execution environment for each test suite.

- TODO perhaps not relevant here

### Running Tests

If you have not done so already, run `npm install --dev`

Always run from the project root (i.e. same directory as `package.json`)

- run all tests              `npm run test`
- run a single test          `npm run test path/to/test.js`
- run tests with full output `npm run test-debug`
- remove files created during test `npm run test-reset`

Since `npm run test` just runs Jest, you can also pass Jest options to the
commands above.
For example
- run tests whose names (in `test()` blocks) match `npm run test -t 'eask lint .*'`
- re-run failed tests `npm run test -f`

### Environment Vars

| Name           | Type   | Default | Meaning                                                                                           |
|:---------------|:-------|---------|:--------------------------------------------------------------------------------------------------|
| `ALLOW_UNSAFE` | bool   | 0       | Run tests in `testUnsafe` blocks. These can **overwrite** your personal emacs config or settings. |
| `DEBUG`        | bool   | 0       | Print full output from commands in test.                                                          |
| `EASK_COMMAND` | path   | "eask"  | Path to Eask. Usually either `eask` or `./bin/eask` (include local changes).                      |
| `TIMEOUT`      | number | 25000   | Command timeout in ms. Note this is different than Jest's timeout, which should be greater.        |

### How to Write a Test

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
  const ctx = new TestContext("./test-js/empty");

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

In Jest, you group related tests using `describe`. Tests in the same describe block can share setup/teardown code,
can be disabled as a group and are grouped under the same heading in output.

`describe` blocks can be nested within other `describe` blocks.
It's a good idea to add a nested `describe` when tests run in different directories, or to match a "given, when, then" style of testing.

For each test directory you should create a new `TestContext` object.
All `runEask` commands will use the `TestContext`'s working directory.

You can also use `TestContext.cleanUp()` to abort any still-running commands that were called in that context.
Note that it sends a signal to *all* processes started using the context's `runEask` command.
If used in an `afterEach` hook (i.e. after every test) it may result in failures.

Jest's tests are in `test` blocks. Note that `it` is an alias for `test`.
Tests can be selectively disabled in code, like so:
- `test.only(name, fn)` runs only that test in the file
- `test.skip(name, fn)` skips running the test but still prints its name
- `test.failing(name, fn)` invert the meaning of the test: it *should* fail.

The `expect` API matches values in different ways and usually prints a diff as part of the failure report.
See Jest's [expect()](https://jestjs.io/docs/expect) API for more info.

Uncaught errors thrown in a `test` block will fail it and report the error.
That's why many tests don't have an `expect` call, they simply check that the command succeeds.

### Patterns

Here are some common patterns for testing commands.
Each of these assumes that `ctx` is a `TestContext` object.

Check a command succeeds:
``` javascript
test("eask analyze", async () => {
  await ctx.runEask("analyze");
});
```

Check a command fails:
``` javascript
test("eask analyze", async () => {
  await expect(ctx.runEask("analyze")).rejects.toThrow();
});
```

Check a command fails with a specific code:
``` javascript
test("eask link add should error", async () => {
  // the error object should have property code = 1
  await expect(ctx.runEask("link add")).rejects.toMatchObject({
    code: 1,
  });
});
```

Check a command produces some output:
``` javascript
test("eask analyze", async () => {
  const { stdout, stderr } = await ctx.runEask("analyze");
  expect(stderr).toMatch("success"); // should apppear as a substring
  // If you want to check both `stderr` and `stdout`, just concatenate them
  expect(stdout + "/n" + stderr).toMatch("success");
});
```

Match command output against a snapshot:
``` javascript
test("eask analyze", async () => {
  const res = await ctx.runEask("analyze");
  expect(res).toMatchSnapshot();
});
```

The first time you run this Jest will create a new snapshot. You should check this in to version control.
If the snapshot changes, you can update the snapshot by running Jest with option `-u`, for example,
`npm run test -- -u` will update all changed snapshots.

Often snapshots will include data that varies with time or environment, for example timestamps or file paths.
The snapshot above includes absolute file paths that will be different on every machine.

Output from `runEask` is wrapped in a helper class `CommandOutput` which provides some transformations to help.
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
      (x) => x.replace(/x:x/g, "y"),
    )
    .raw();
  expect(resClean).toMatchSnapshot();
});
```

It's important to use the `g` regex flag so all occurrences of the match are replaced, or you could use `replaceAll`.
User provided functions run in addition to the default sanitize function and run in the order they were given.

Commands which modify global environment, for example with `-c` or `-g` options:
``` javascript
const { testUnsafe } = require('./helpers');

// this will only run if ALLOW_UNSAFE is != 0
testUnsafe("global install", async () => {
  // this installs in ~/.eask and changes ~/Eask
  await ctx.runEask("install -g foo");
});
```

### Common Problems

- When using `runEask()`, pass only the Eask *arguments*, not the `eask` command itself.
- When using `expect(...).rejects` it should be awaited so that the promise rejects before the test completes.
- The folder argument to `TestContext` should be relative to project root, if it doesn't exist you may get an error `ENOENT`
- If you get an error from Jest reporting open handles, then try using `afterAll(() => ctx.cleanUp())`
- There are two timeout values: one used for Jest (set in `package.json`), and one used for `node.exec`, set via env var in `./helpers.js`.
  The `node.exec` timeout is set lower than the Jest one, so changing timeout values for tests or by `jest.setTimeout` usually won't
  have an effect. Instead set the timeout on the command itself `runEask("eask emacs", { timeout: 100000 })`
