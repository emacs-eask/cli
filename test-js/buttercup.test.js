const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;

jest.setTimeout(10000);

describe("buttercup", () => {
  const cwd = "./test-js/buttercup";
  beforeAll(async () => await exec("eask install-deps --dev", { cwd }));

  test("run all tests", async () => {
    // this runs all tests, so should error
    await expect(exec("eask test buttercup")).rejects.toThrow();
  });

  // buttercup takes directories as arguments
  test("run succeeding tests", async () => {
    const res = await exec("eask test buttercup ./test-ok", { cwd });
  });

  test("run all tests explicitly", async () => {
    await expect(
      exec("eask test buttercup ./test-ok ./test-fail"),
    ).rejects.toThrow();
  });

  test("does not take options", async () => {
    const res = await exec("eask test buttercup --no-color ./test-ok", { cwd });
    expect(res).toBeTruthy();
  });

  // Because load-path is manually set, cannot refer to parent directories.
  // Note this does work if you do ../buttercup/test-ok/, but not for any other directory.
  test("running paths in parent dir", async () => {
    await expect(exec("eask test buttercup ../bin", { cwd })).rejects.toThrow();
  });
});
