const { TestContext } = require("./helpers");

describe("exec", () => {
  const ctx = new TestContext("./test/jest/exec");

  beforeAll(async () => await ctx.runEask("install-deps"));

  afterAll(() => ctx.cleanUp());

  test("eask exec ert-runner", async () => {
    await ctx.runEask("exec ert-runner -h");
  });

  test("eask exec github-elpa", async () => {
    await ctx.runEask("exec github-elpa -h");
  });

  test("eask exec echo", async () => {
    await ctx.runEask("exec echo hello world");
  });

  test("eask exec buttercup -L .", async () => {
    await ctx.runEask("exec buttercup -L .");
  });

  test("eask exec buttercup -L . --pattern 'pattern 1'", async () => {
    await ctx.runEask('exec buttercup -L . --pattern "pattern 1"');
  });

  test("should error with no args", async () => {
    await expect(ctx.runEask("exec")).rejects.toThrow();
  });
});
