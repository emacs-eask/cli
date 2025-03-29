const { testUnsafe, TestContext } = require("./helpers");

describe("upgrade-eask", () => {
  const ctx = new TestContext();
  testUnsafe("eask upgrade-eask", async () => {
    await ctx.runEask("upgrade-eask");
  });
});
