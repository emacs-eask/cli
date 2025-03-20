const { TestContext } = require("./helpers");

jest.setTimeout(10000);

describe("buttercup", () => {
  const ctx = new TestContext("./test-js/buttercup");

  beforeAll(async () => await ctx.runEask("install-deps --dev"));

  test("run all tests", async () => {
    // this runs all tests, so should error
    await expect(ctx.runEask("test buttercup")).rejects.toThrow();
  });

  // buttercup takes directories as arguments
  test("run succeeding tests", async () => {
    await ctx.runEask("test buttercup ./test-ok");
  });

  test("run all tests explicitly", async () => {
    await expect(
      ctx.runEask("test buttercup ./test-ok ./test-fail"),
    ).rejects.toThrow();
  });

  test("does not take options", async () => {
    await ctx.runEask("test buttercup --no-color ./test-ok");
  });

  // Because load-path is manually set, cannot refer to parent directories.
  // Note this does work if you do ../buttercup/test-ok/, but not for any other directory.
  test("running paths in parent dir", async () => {
    await expect(ctx.runEask("test buttercup ../bin")).rejects.toThrow();
  });
});
