const { TestContext } = require("./helpers");

describe("outdated and upgrade", () => {
  const ctx = new TestContext("./test/jest/outdated-upgrade");

  // See https://github.com/emacs-eask/cli/issues/11.
  const avoid11 = (emacsVersion() < "28.1");

  beforeAll(async () => {
    await ctx.runEask("install-deps", { timeout: 40000 }, avoid11);
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
