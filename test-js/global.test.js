const { testUnsafe, TestContext } = require("./helpers");

describe("global", () => {
  // global => install to ~/.eask
  // TODO may need to copy a global eask file?
  // or perhaps need --insecure option?
  // cp -R ./test/fixtures/home/Eask ~/Eask

  const ctx = new TestContext();

  describe("informational", () => {
    test("eask archives -g", async () => {
      await ctx.runEask("archives -g");
    });

    test("eask list -g --depth=0", async () => {
      await ctx.runEask("list -g --depth=0");
    });

    test("eask outdated -g", async () => {
      await ctx.runEask("outdated -g");
    });
  });

  describe("install", () => {
    /*
     * This test modifies ~/.eask
     */
    testUnsafe("eask install -g", async () => {
      await ctx.runEask("install -g spinner ivy beacon company fuzzy");
    });

    testUnsafe("eask uninstall -g", async () => {
      await ctx.runEask("uninstall -g ivy fuzzy");
    });
  });
});
