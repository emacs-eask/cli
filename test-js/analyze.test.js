const { TestContext } = require("./helpers");

describe("analyze", () => {
  describe("dsl", () => {
    const cwd = "./test-js/dsl";
    const ctx = new TestContext(cwd);

    it("handles plain text", async () => {
      await ctx.runEask("analyze");
      await ctx.runEask("analyze Eask");
    });

    it("handles json", async () => {
      await ctx.runEask("analyze --json");
      await ctx.runEask("analyze Eask --json");
    });
  });

  describe("error", () => {
    const ctx = new TestContext("./test-js/empty");

    it("errors in an empty dir", async () => {
      await expect(ctx.runEask("analyze")).rejects.toThrow();
    });
  });

  describe("metadata", () => {
    const ctx = new TestContext("./test-js/metadata");

    it("handles plain text", async () => {
      await ctx.runEask("analyze");
      await ctx.runEask("analyze Eask");
    });

    it("handles json", async () => {
      await ctx.runEask("analyze --json");
      await ctx.runEask("analyze Eask --json");
    });
  });
});
