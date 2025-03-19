const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;

describe("exec", () => {
  const cwd = "./test-js/exec";

  beforeAll(() => execSync("eask install-deps", { cwd }));

  test("eask exec ert-runner", async () => {
    const res = await exec("eask exec ert-runner -h", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask exec github-elpa", async () => {
    const res = await exec("eask exec github-elpa -h", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask exec echo", async () => {
    const res = await exec("eask exec echo hello world", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask exec buttercup -L .", async () => {
    const res = await exec("eask exec buttercup -L .", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask exec buttercup -L . --pattern 'pattern 1'", async () => {
    const res = await exec("eask exec buttercup -L . --pattern 'pattern 1'", {
      cwd,
    });
    expect(res).toBeTruthy();
  });

  test("should error with no args", async () => {
    await expect(exec("./bin/eask exec", { cwd })).rejects.toThrow();
  });
});
