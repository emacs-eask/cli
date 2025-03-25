const { TestContext } = require("./helpers");

describe("analyze", () => {
  describe("in ./dsl", () => {
    const ctx = new TestContext("./test-js/dsl");

    it("handles plain text", async () => {
      await ctx.runEask("analyze");
      await ctx.runEask("analyze Eask");
    });

    it("handles json option", async () => {
      const { stderr } = await ctx.runEask("analyze --json");
      await ctx.runEask("analyze Eask --json");

      // try to parse output, errors if not valid
      JSON.parse(stderr);
    });

    it("matches snapshot", async () => {
      const res = await ctx.runEask("analyze");
      const resClean = res.sanitized().raw();
      expect(resClean).toMatchSnapshot();
    });

    it("should report multiple definitions", async () => {
      const { stderr } = await ctx.runEask("analyze");
      // expect this substring
      expect(stderr).toMatch("Multiple definition of `package'");
    });
  });

  describe("in ./empty", () => {
    const ctx = new TestContext("./test-js/empty");

    it("should error", async () => {
      await expect(ctx.runEask("analyze")).rejects.toThrow();
    });
  });

  describe("in ./metadata", () => {
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
