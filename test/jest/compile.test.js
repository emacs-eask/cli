import { TestContext } from "./helpers.js";

describe("compile", () => {
  const ctx = new TestContext("./test/jest/compile");

  afterAll(() => {
    ctx.cleanUp();
  });

  describe("in an empty project", () => {
    const ctxEmpty = new TestContext("./test/jest/empty");

    it("should error on partial input", async () => {
      await expect(ctxEmpty.runEask("compile")).rejects.toThrow();
    });
  });

  describe("for compile/mock.el", () => {
    it("should compile with a warning", async () => {
      await ctx.runEask("compile mock.el");
    });

    it("should escalate warnings given --strict", async () => {
      await expect(
        ctx.runEask("compile --strict mock.el"),
      ).rejects.toMatchObject({ code: 1 });
    });
  });

  describe("for compile/fail.el", () => {
    beforeEach(async () => {
      await ctx.runEask("clean elc");
    });

    it("should error", async () => {
      await expect(ctx.runEask("compile fail.el")).rejects.toMatchObject({
        code: 1,
      });
    });

    it("should compile mock.el given --allow-error", async () => {
      await expect(
        ctx.runEask("compile --allow-error fail.el mock.el"),
      ).rejects.toMatchObject({
        code: 1,
        stderr: expect.stringContaining("1 file compiled, 1 skipped"),
      });
      expect(ctx.fileExists("./mock.elc"));
    });
  });
});
