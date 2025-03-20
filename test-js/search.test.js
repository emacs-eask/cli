const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;

describe("search", () => {
  const cwd = "./test-js/search";

  test("eask search company", async () => {
    const res = await exec("eask search company", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask search company dash --depth 0", async () => {
    const res = await exec("eask search company dash --depth 0", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask search company dash f s --depth 0 -g", async () => {
    const res = await exec("eask search company dash f s --depth 0 -g", {
      cwd,
    });
    expect(res).toBeTruthy();
  });

  test("eask search should error", async () => {
    await expect(exec("./bin/eask search", { cwd })).rejects.toThrow();
  });
});
