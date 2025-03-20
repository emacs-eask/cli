const { testUnsafe, TestContext } = require("./helpers");

describe("config param", () => {
  const ctx = new TestContext();

  afterAll(() => ctx.cleanUp());

  // TODO timeout
  test("eask archives -c", async () => {
    await ctx.runEask("archives -c");
  });

  // TODO perhaps teardown the following two?
  // TODO check in docker
  testUnsafe("eask install -c", async () => {
    await ctx.runEask("install -c spinner ivy beacon company-fuzzy");
  });

  // TODO should test uninstalling multiple
  testUnsafe("eask uninstall -c", async () => {
    await ctx.runEask("uninstall -c ivy company-fuzzy");
  });

  // TODO timeout
  test("eask list -c", async () => {
    await ctx.runEask("list -c --depth=0");
  });

  // TODO timeout
  test("eask outdated -c", async () => {
    await ctx.runEask("outdated -c");
  });
});
