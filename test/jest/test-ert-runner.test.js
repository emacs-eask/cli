const { TestContext } = require("./helpers");

describe("test ert-runner", () => {
  const ctx = new TestContext("./test/jest/ert-runner");

  beforeAll(async () => await ctx.runEask("install-deps --dev"));

  afterAll(() => ctx.cleanUp());

  test("eask test ert-runner ./test/*.el", async () => {
    await ctx.runEask("test ert-runner ./test/*.el");
  });

  test("eask test ert-runner should accept the eask options", async () => {
    const output = await ctx.runEask("test ert-runner ./test/*.el -v 4");
    expect(output.combined()).toContain("Ran 1 test");
  });
});
