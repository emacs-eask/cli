import { TestContext } from "./helpers.js";

describe("search", () => {
  const cwd = "./test/jest/search";
  const ctx = new TestContext(cwd);

  test("eask search company", async () => {
    await ctx.runEask("search company");
  });

  test("eask search company dash --depth 0", async () => {
    await ctx.runEask("search company dash --depth 0");
  });

  test("eask search company dash f s --depth 0 -g", async () => {
    await ctx.runEask("search company dash f s --depth 0 -g");
  });

  test("eask search should error", async () => {
    await expect(ctx.runEask("search")).rejects.toThrow();
  });
});
