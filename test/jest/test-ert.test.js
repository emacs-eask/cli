import { TestContext } from "./helpers.js";

describe("test-ert", () => {
  const ctx = new TestContext("./test/jest/ert");

  afterAll(() => ctx.cleanUp());

  test("eask test ert ./test/*.el", async () => {
    await ctx.runEask("test ert ./test/*.el");
  });

  test("nil message", async () => {
    await ctx.runEask("test ert ./test-nil-message/*.el");
  });

  test("no files", async () => {
    await expect(ctx.runEask("test ert")).rejects.toThrow();
  });
});
