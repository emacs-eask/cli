const { TestContext } = require("./helpers");

describe("docker", () => {
  const ctx = new TestContext("./test/jest/docker");

  afterAll(() => ctx.cleanUp());

  test("eask docker 27.1 info", async () => {
    // this can take a long time to pull and build the image
    await ctx.runEask("docker 27.1 info", { timeout: 40000 });
  });

  test("eask docker silex/emacs:27.1-eask info", async () => {
    // this can take a long time to pull and build the image
    await ctx.runEask("docker silex/emacs:27.1-eask info", { timeout: 40000 });
  });
});
