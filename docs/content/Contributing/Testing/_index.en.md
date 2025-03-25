---
title: Testing
weight: 20
---

{{< toc >}}

Local testing for Eask is done using the [Jest]() testing framework.

A custom `TestContext` class is used to manage the execution environment for each test suite.

### Running Tests

If you have not done so already, run `npm install --dev`

Always run from the project root (i.e. same directory as `package.json`)

- run all tests              `npm run test`
- run a single test          `npm run test path/to/test.js`
- run tests with full output `npm run test-debug`
- remove files created during test `npm run test-reset`

### Environment Vars

| Name           | Type   | Default | Meaning                                                                                    |
|:---------------|:-------|---------|:-------------------------------------------------------------------------------------------|
| `ALLOW_UNSAFE` | bool   | 0       | Run tests in `testUnsafe` blocks. These can affect your emacs config or settings.          |
| `DEBUG`        | bool   | 0       | Print full output from commands in test.                                                   |
| `EASK_COMMAND` | path   | "eask"  | Path to Eask. Usually either `eask` or `./bin/eask` (include local changes).               |
| `TIMEOUT`      | number | 25000   | Command timeout in ms. Note this is different than Jest's timeout, which should be larger. |

### How to Write a Test

**Folder structure**

Tests should be in `test/js`
Related tests should be in the same file with the suffix `.test.js`.

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

In Jest, group related tests using `describe`. Tests in the same describe block can share setup/teardown code, can be disabled as a group and are grouped under the same heading in output.

Describe blocks can be nested, it's a good idea to add a nested `describe` when tests run in different directories.

For each test directory you should create a new `TestContext` object. All `runEask` commands will use the `TestContext`'s working directory.

You can also use `TestContext.cleanUp()` to abort any still-running commands that were called in that context.

Jest's tests are in `test` blocks. Note that `it` is an alias for `test`.

The `expect` API matches values in different ways and usually prints a diff as part of the failure report.
See Jest's [expect()](https://jestjs.io/docs/expect) API for more info.

Errors thrown in a `test` block will fail it and report the error.
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
// TODO
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
`npm run test -u` will update all changed snapshots.

You can also match or ignore parts of an object to avoid time-varying or local data like usernames.

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

- Commands in `runEask` shouldn't include `eask`!
  This is an error: `ctx.runEask("eask emacs")`
- When using `expect(...).rejects` it should be awaited
- The folder argument to `TestContext` should be relative to project root, if it doesn't exist you may get an error `NO_ENT`
- If you get an error from Jest reporting open handles, then try using `afterAll(() => ctx.cleanUp())`
- There are two timeout values: one used for Jest (set in `package.json`), and one used for `node.exec`, set via env var in `./helpers.js`.
  The `node.exec` timeout is set lower than the Jest one, so changing timeout values for tests or by `jest.setTimeout` usually won't
  have an effect. Instead set the timeout on the command itself `runEask("eask emacs", { timeout: 100000 })`
