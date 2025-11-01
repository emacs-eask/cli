const { TestContext } = require("./helpers");

/**
 * Clean output and attempt to parse as JSON.
 * Throws if failing.
 * @param {string} s
 * @returns {object} the parsed JSON.
 */
function tryJSON(s) {
  return JSON.parse(s.trim() || "{}");
}

describe("analyze", () => {
  describe("in ./analyze/dsl", () => {
    const ctx = new TestContext("./test/jest/analyze/dsl");

    it("handles plain text", async () => {
      await expect(ctx.runEask("analyze")).rejects.toThrow();
      await expect(ctx.runEask("analyze Eask")).rejects.toThrow();
    });

    it("handles json option", async () => {
      // alternate command order
      await expect(ctx.runEask("analyze Eask --json")).rejects.toThrow();
      try {
        await ctx.runEask("analyze --json");
      } catch (e) {
        tryJSON(e.stdout);
        expect(e.stdout).toMatchSnapshot();
      }
    });

    it("matches snapshot", async () => {
      try {
        await ctx.runEask("analyze");
        expect.failing();
      } catch (e) {
        expect(e.code).toBe(1);
        const res = ctx.errorToCommandOutput(e);
        const resClean = res.sanitized().raw();
        expect(resClean).toMatchSnapshot();
      }
    });

    it("should report multiple definitions", async () => {
      try {
        await ctx.runEask("analyze");
        expect.failing();
      } catch ({ stdout }) {
        // expect this substring
        expect(stdout).toMatch("Multiple definition of `package'");
      }
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
      await expect(ctx.runEask("analyze")).rejects.toThrow();
      await expect(ctx.runEask("analyze Eask")).rejects.toThrow(
        expect.objectContaining({
          code: 1,
          stderr: expect.stringContaining("(Checked 1 file)"),
        }),
      );
    });

    it("handles json", async () => {
      await expect(ctx.runEask("analyze --json")).rejects.toThrow();
      await expect(ctx.runEask("analyze Eask --json")).rejects.toMatchObject({
        code: 1,
        stderr: expect.stringContaining("(Checked 1 file)"),
      });
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

    it("should error on Eask-errors", async () => {
      await expect(ctx.runEask("analyze Eask-error")).rejects.toThrow();
    });

    // JSON
    it("handles json option when no errors", async () => {
      const { stdout } = await ctx.runEask("analyze --json Eask-normal");
      tryJSON(stdout);
    });

    it("handles json option when lexical binding warnings are present", async () => {
      const { stdout } = await ctx.runEask("analyze --json Eask-lexical");
      tryJSON(stdout);
    });

    // --strict and --allow-error
    it.failing("should error when using --strict on Eask-warn", async () => {
      await expect(ctx.runEask("analyze --strict Eask-warn")).rejects.toThrow();
    });

    // sanity check: flag should not change behavior in this case
    // this is because the menaing of --allow-error is "continue to the end"
    it("should error when --allow-error is set", async () => {
      await expect(
        ctx.runEask("analyze Eask-error --allow-error"),
      ).rejects.toThrow();
    });

    it("should print Checked when there are errors", async () => {
      await expect(ctx.runEask("analyze Eask-error")).rejects.toMatchObject({
        stderr: expect.stringContaining("(Checked 1 file)"),
      });
    });

    it("should check all files when --allow-error is set", async () => {
      // this is not a great test because when there is any error it doesn't print this note
      await expect(
        ctx.runEask("analyze --allow-error Eask-normal Eask-error"),
      ).rejects.toMatchObject({
        stderr: expect.stringContaining("(Checked 2 files)"),
      });
    });

    // although the output does match, it still doesn't exit with an error
    it("should have later warnings when --allow-error is set", async () => {
      await expect(
        ctx.runEask("analyze --allow-error Eask-error Eask-warn"),
        // this warning is specific to Eask-warn
      ).rejects.toMatchObject({
        stdout: expect.stringContaining("missing `none.el'"),
      });
    });
  });
});
