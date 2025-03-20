const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;
const controller = new AbortController();
const { signal } = controller;

afterAll(() => controller.abort());

describe("outdated and upgrade", () => {
  const cwd = "./test-js/outdated_upgrade";

  beforeAll(async () => {
    await exec("eask install-deps", { cwd, signal });
    await exec("eask load make-outdate.el", { cwd, signal });
  }, 15000);

  afterAll(async () => await exec("eask clean all", { cwd, signal }));

  test("list outdated", async () => {
    await exec("eask outdated", { cwd, signal });
    await exec("eask outdated --depth 0", { cwd, signal });
  });

  test("upgrade", async () => {
    expect(await exec("eask upgrade", { cwd, signal })).toBeTruthy();
  });
});
