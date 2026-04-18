import { TestContext } from "./helpers.js";

/**
 * Clean output and attempt to parse as JSON.
 * Throws if failing.
 * @param {string} s
 * @returns {object} the parsed JSON.
 */
function tryJSON(s) {
  // The main use case of analyze --json is flycheck/flymake
  // both of which accept mixed JSON/text output.
  // So the test should filter non-JSON output in a similar way
  // See flycheck--json-parser for reference

  // This is a rough approximation of the greedy JSON parsing
  // that flycheck uses
  // It will yield a false negative if:
  // - a non-JSON output line begins with a space
  // - there is more that one JSON object in the output
  const lines = s.split("\n");
  const json = lines.filter((x) => x.match("^[{}\\[\\] ]"));
  if (json.length == 0) {
    return {};
  } else {
    const result = JSON.parse(json.join("\n"));
    if (typeof result == "string") {
      // this case is a single string prefixed with whitespace
      return {};
    }
    return result;
  }
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
        tryJSON(e.stderr);
        const res = ctx.errorToCommandOutput(e);
        const resClean = res.sanitized().raw();
        expect(resClean).toMatchSnapshot();
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
      } catch ({ stderr }) {
        // expect this substring
        expect(stderr).toMatch("Multiple definition of `package'");
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
      const { stderr } = await ctx.runEask("analyze --json Eask-normal");
      tryJSON(stderr);
    });

    it("handles json option when lexical binding warnings are present", async () => {
      const { stderr } = await ctx.runEask("analyze --json Eask-lexical");
      tryJSON(stderr);
    });

    // --strict and --allow-error
    it("should error when using --strict on Eask-warn", async () => {
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
        stderr: expect.stringContaining("missing `none.el'"),
      });
    });
  });
});
