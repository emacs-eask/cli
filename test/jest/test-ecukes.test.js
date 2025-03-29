const { TestContext } = require("./helpers");

describe("test_ecukes", () => {
  const ctx = new TestContext("./test/jest/ecukes");

  beforeAll(async () => await ctx.runEask("install-deps --dev"));
  afterAll(() => ctx.cleanUp());

  test("eask test ecukes", async () => {
    await ctx.runEask("test ecukes");
  });

  test("eask test ecukes ./features/foo.feature", async () => {
    await ctx.runEask("test ecukes ./features/foo.feature");
  });
});
