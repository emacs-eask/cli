const { TestContext } = require("./helpers");

describe("lint", () => {
  const ctx = new TestContext("./test/jest/lint");

  beforeEach(async () => {
    await ctx.runEask("clean elc");
  });

  afterAll(() => {
    ctx.cleanUp();
  });

  describe("partial input", () => {
    it("eask lint should error", async () => {
      await expect(ctx.runEask("eask lint")).rejects.toMatchObject({ code: 1 });
    });
  });

  describe("checkdoc", () => {
    it("should work on declare-ok.el", async () => {
      await expect(
        ctx.runEask("lint checkdoc declare-ok.el"),
      ).resolves.toMatchObject({
        stderr: expect.stringContaining("No issues found"),
      });
    });

    it("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint checkdoc --strict checkdoc-fail.el"),
      ).rejects.toMatchObject({
        code: 1,
      });
    });

    it("should error given --strict and --allow-error", async () => {
      await expect(
        ctx.runEask("lint checkdoc --strict checkdoc-fail.el declare-ok.el"),
      ).rejects.toMatchObject({
        code: 1,
        // expect that declare-ok was checked too
        stderr: expect.stringContaining("`declare-ok.el` with checkdoc"),
      });
    });
  });

  describe("declare", () => {
    it("should work on declare-ok.el", async () => {
      await expect(
        ctx.runEask("lint declare declare-ok.el"),
      ).resolves.toMatchObject({
        stderr: expect.stringContaining("No issues found"),
      });
    });

    it("should work on declare-fail.el (warnings ignored)", async () => {
      await ctx.runEask("lint declare declare-fail.el");
    });

    it("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint declare --strict declare-fail.el"),
      ).rejects.toMatchObject({ code: 1 });
    });

    it("should error given --strict --allow-error", async () => {
      await expect(
        ctx.runEask("lint declare --strict --allow-error ./*.el"),
      ).rejects.toMatchObject({
        code: 1,
        // expect that declare-ok was checked too
        stderr: expect.stringContaining("`declare-ok.el` with check-declare"),
      });
    });
  });

  describe("elint", () => {
    it("should work on declare-ok.el", async () => {
      await ctx.runEask("lint elint declare-ok.el");
    });

    // TODO --strict has no effect on elint
    it.failing("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint elint --strict checkdoc-fail.el"),
      ).rejects.toMatchObject({ code: 1 });
    });
  });

  describe("elisp-lint", () => {
    it("should work on elisp-lint-ok.el", async () => {
      await ctx.runEask("lint elisp-lint --strict elisp-lint-ok.el");
    });

    it("should work on declare-ok.el (warnings)", async () => {
      await ctx.runEask("lint elisp-lint declare-ok.el");
    });

    it("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint elisp-lint --strict declare-ok.el"),
      ).rejects.toMatchObject({ code: 1 });
    });

    it("should work with --strict --allow-error", async () => {
      await expect(
        ctx.runEask(
          "lint elisp-lint --strict --allow-error checkdoc-fail.el elisp-lint-ok.el",
        ),
      ).rejects.toMatchObject({
        stderr: expect.stringContaining("2 files linted"),
      });
    });
  });

  describe("elsa", () => {
    // TODO prints a lot of garbage
    it("should work on elisp-lint-ok.el", async () => {
      await ctx.runEask("lint elsa elisp-lint-ok.el");
    });

    it("should work on elsa-warn.el (warnings)", async () => {
      await ctx.runEask("lint elsa elsa-warn.el");
    });

    // TODO prints out "error" but no exit code
    it("should error on declare-ok.el", async () => {
      await expect(
        ctx.runEask("lint elsa declare-ok.el"),
      ).rejects.toMatchObject({ code: 1 });
    });

    // TODO strict has no effect
    it.failing("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint elsa --strict elsa-warn.el"),
      ).rejects.toMatchObject({ code: 1 });
    });
  });

  describe("indent", () => {
    // TODO exits with 1
    // TODO prints "Don't call me!"
    it.failing("should work on indent-warn.el", async () => {
      await ctx.runEask("lint indent indent-warn.el");
    });

    it("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint indent --strict indent-warn.el"),
      ).rejects.toMatchObject({ code: 1 });
    });

    it("should work with --strict --allow-error", async () => {
      await expect(
        ctx.runEask(
          "lint indent --strict --allow-error indent-warn.el declare-ok.el",
        ),
      ).rejects.toMatchObject({
        code: 1,
        stderr: expect.stringContaining("2 files linted"),
      });
    });
  });

  describe("keywords", () => {
    // TODO seems like keywords in actual file are ignored
    // only those in Eask file are checked
    // but those interfere with --strict handling in other commands

    it.failing("should work", async () => {
      await ctx.runEask("lint keywords");
    });

    it.failing("should error given --strict", async () => {
      await expect(ctx.runEask("lint keywords --strict")).rejects.toMatchObject(
        { code: 1 },
      );
    });
  });

  describe("package", () => {
    it("should work on declare-ok.el", async () => {
      await ctx.runEask("lint package declare-ok.el");
    });

    // note that all files are checked anyway so --allow-error has no effect
    it.failing("should error given --strict", async () => {
      await expect(
        ctx.runEask("lint package --strict declare-ok.el"),
      ).rejects.toMatchObject({ code: 1 });
    });
  });

  describe("regexps", () => {
    it("should work on declare-ok.el", async () => {
      await ctx.runEask("lint regexps declare-ok.el");
    });

    // TODO this prints a stack trace
    it("should error on regexp-warn.el", async () => {
      await expect(
        ctx.runEask("lint regexps regexp-warn.el"),
      ).rejects.toThrow();
    });

    it.failing("should work with --allow-error", async () => {
      await expect(
        ctx.runEask("lint regexps --allow-error regexp-warn.el declare-ok.el"),
      ).rejects.toMatchObject({
        stderr: expect.stringContaining("2 files linted"),
      });
    });
  });
});
