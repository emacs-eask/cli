const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;

describe("emacs", () => {
  const cwd = "./test-js/empty";

  beforeAll(async () => await exec("eask clean all", { cwd }));

  test("eask emacs --version", async () => {
    const res = await exec("eask emacs --version", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask emacs --batch --eval", async () => {
    const res = await exec(
      'eask emacs --batch --eval "(require (quote ert))" --eval "(ert-deftest mytest () (should-not (display-graphic-p)))" -f ert-run-tests-batch',
      { cwd },
    );
    expect(res).toBeTruthy();
  });
});
