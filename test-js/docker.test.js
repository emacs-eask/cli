const { TestContext } = require("./helpers");

describe("docker", () => {
  const ctx = new TestContext("./test-js/docker");

  afterAll(() => ctx.cleanUp());

  test("eask docker 27.1 info", async () => {
    await ctx.runEask("docker 27.1 info");
  });
});
