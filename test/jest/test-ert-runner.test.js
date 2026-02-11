import { TestContext } from "./helpers.js";

describe("test ert-runner", () => {
  const ctx = new TestContext("./test/jest/ert-runner");

  beforeAll(async () => await ctx.runEask("install-deps --dev"));

  afterAll(() => ctx.cleanUp());

  test("eask test ert-runner ./test/*.el", async () => {
    await ctx.runEask("test ert-runner ./test/*.el");
  });
});
