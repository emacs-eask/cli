const cmp = require('semver-compare');
const { emacsVersion, TestContext } = require("./helpers");

describe("outdated and upgrade", () => {
  const ctx = new TestContext("./test/jest/outdated-upgrade");

  beforeAll(async () => {
    await ctx.runEask(
      "install-deps", { timeout: 40000 },
      // See https://github.com/emacs-eask/cli/issues/11.
      cmp(await emacsVersion(), "28.1") == -1);
    await ctx.runEask("load make-outdate.el");
  });

  // these will run in sequence
  afterAll(async () => await ctx.runEask("clean all"));
  afterAll(() => ctx.cleanUp());

  test("list outdated", async () => {
    await ctx.runEask("outdated");
    await ctx.runEask("outdated --depth 0");
  });

  test("upgrade", async () => {
    await ctx.runEask("upgrade");
  });
});
