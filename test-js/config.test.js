const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const { testUnsafe } = require("./helpers");

describe("config param", () => {
  test("eask archives -c", async () => {
    const res = await exec("eask archives -c");
    expect(res).toBeTruthy();
  });

  // TODO perhaps teardown the following two?
  // TODO check in docker
  testUnsafe("eask install -c", async () => {
    const res = await exec("eask install -cp spinner ivy beacon company fuzzy");
    expect(res).toBeTruthy();
  });

  testUnsafe("eask uninstall -c", async () => {
    const res = await exec("eask uninstall -c ivy fuzzy");
    expect(res).toBeTruthy();
  });

  test("eask list -c", async () => {
    const res = await exec("eask list -c --depth=0");
    expect(res).toBeTruthy();
  });

  test("eask outdated -c", async () => {
    const res = await exec("eask outdated -c");
    expect(res).toBeTruthy();
  });
});
