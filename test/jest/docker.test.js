const { TestContext } = require("./helpers");

jest.setTimeout(1000 * 60);

describe("docker", () => {
  const ctx = new TestContext("./test/jest/docker");

  afterAll(() => ctx.cleanUp());

  test("eask docker 27.1 info", async () => {
    // this can take a long time to pull and build the image
    await ctx.runEask("docker 27.1 info", { timeout: 40000 });
  });
});
