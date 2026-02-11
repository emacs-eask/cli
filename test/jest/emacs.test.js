import { TestContext } from "./helpers.js";

describe("emacs", () => {
  const ctx = new TestContext("./test/jest/empty");

  beforeAll(async () => await ctx.runEask("clean all"));

  test("eask emacs --version", async () => {
    await ctx.runEask("emacs --version");
  });

  test("eask emacs --batch --eval", async () => {
    await ctx.runEask(
      'emacs --batch --eval "(require (quote ert))" --eval "(ert-deftest mytest () (should-not (display-graphic-p)))" -f ert-run-tests-batch',
    );
  });
});
