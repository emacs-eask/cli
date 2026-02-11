import { testUnsafe, TestContext } from "./helpers.js";

describe("upgrade-eask", () => {
  const ctx = new TestContext();
  testUnsafe("eask upgrade-eask", async () => {
    await ctx.runEask("upgrade-eask");
  });
});
