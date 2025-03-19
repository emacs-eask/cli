const { TestContext } = require("./helpers");

describe("link", () => {
  describe("normally", () => {
    const cwd = "./test-js/link";
    const ctx = new TestContext(cwd);
    it("adds a link", async () => {
      await ctx.runEask('link add "mini.pkg.1" "./test/mini.pkg.1/"');
    });

    it("lists links", async () => {
      await ctx.runEask("link list");
    });

    it("deletes links", async () => {
      await ctx.runEask("link delete mini.pkg.1-0.0.1");
    });
  });

  describe("in an empty directory", () => {
    const ctx = new TestContext("./test-js/empty");

    it("eask link add should error", async () => {
      await expect(ctx.runEask("link add")).rejects.toThrow();
    });

    it("adding an unknown package should error", async () => {
      await expect(ctx.runEask("link add foo .")).rejects.toThrow();
    });

    it("deleting an unknown package should error", async () => {
      await expect(ctx.runEask("link delete")).rejects.toThrow();
    });
  });
});
