const { TestContext } = require("./helpers");

describe("analyze", () => {
  describe("in ./analyze/dsl", () => {
    const ctx = new TestContext("./test/jest/analyze/dsl");

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
    const ctx = new TestContext("./test/jest/empty");

    it("should error", async () => {
      await expect(ctx.runEask("analyze")).rejects.toThrow();
    });
  });

  describe("in ./analyze/metadata", () => {
    const ctx = new TestContext("./test/jest/analyze/metadata");

    it("handles plain text", async () => {
      await ctx.runEask("analyze");
      await ctx.runEask("analyze Eask");
    });

    it("handles json", async () => {
      await ctx.runEask("analyze --json");
      await ctx.runEask("analyze Eask --json");
    });
  });

  describe("in ./analyze/errors", () => {
    const ctx = new TestContext("./test/jest/analyze/errors");

    // Eask-normal - no errors or warnings
    // Eask-warn   - only warnings
    // Eask-error  - errors and  warnings

    it("should check Eask-normal", async () => {
      await ctx.runEask("analyze Eask-normal");
    });

    it("should check Eask-warn", async () => {
      await ctx.runEask("analyze Eask-warn");
    });

    it.failing("should error on Eask-errors", async () => {
      await expect(ctx.runEask("analyze Eask-error")).rejects.toThrow();
    });

    it.failing("should error when using --strict on Eask-warn", async () => {
      await expect(ctx.runEask("analyze --strict Eask-warn")).rejects.toThrow();
    });

    // sanity check: flag should not change behavior in this case
    // this is because the menaing of --allow-error is "continue to the end"
    it.failing("should error when --allow-error is set", async () => {
      await expect(
        ctx.runEask("analyze Eask-error --allow-error"),
      ).rejects.toThrow();
    });

    it.failing("should check all files when --allow-error is set", async () => {
      await expect(
        ctx.runEask("analyze --allow-error Eask-normal Eask-error"),
      ).rejects.toMatchObject({ stderr: "(Checked 2 files)" });
    });
  });
});
