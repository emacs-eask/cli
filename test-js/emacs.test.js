const { TestContext } = require("./helpers");

describe("emacs", () => {
  const cwd = "./test-js/empty";
  const ctx = new TestContext(cwd);

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
