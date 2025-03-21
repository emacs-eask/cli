const { TestContext } = require("./helpers");

describe("test ert-runner", () => {
  const ctx = new TestContext("./test-js/ert-runner");

  beforeAll(async () => await ctx.runEask("install-deps --dev"), 10000);

  afterAll(() => ctx.cleanUp());

  test("eask test ert-runner ./test/*.el", async () => {
    await ctx.runEask("test ert-runner ./test/*.el");
  });
});
