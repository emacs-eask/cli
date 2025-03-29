const { testUnsafe, TestContext } = require("./helpers");
const fs = require("node:fs/promises");
const path = require("node:path");
const process = require("node:process");

describe("config param", () => {
  const ctx = new TestContext();

  afterAll(() => ctx.cleanUp());

  describe("informational", () => {
    /*
     * Note that these tests will run against your ~/.emacs.d if run locally.
     * If you have a complicated set up, they may timeout.
     */

    test("eask archives -c", async () => {
      await ctx.runEask("archives -c");
    });

    test("eask list -c", async () => {
      await ctx.runEask("list -c --depth=0");
    });

    test("eask outdated -c", async () => {
      await ctx.runEask("outdated -c");
    });
  });

  describe("install", () => {
    /*
     * This test modifies ~/.emacs.d/init.el and installs packages there when run with ALLOW_UNSAFE
     */
    testUnsafe("eask install -c", async () => {
      // create ~/.emacs.d
      await fs.mkdir(path.join(process.env.HOME, "/.emacs.d/"), {
        recursive: true,
      });
      // copy init.el from fixtures
      // it must enable sources for the installed packages
      await fs.copyFile(
        "./test/jest/config/init.el",
        path.join(process.env.HOME, "/.emacs.d/init.el"),
        fs.constants.COPYFILE_EXCL, // throw if init.el already exists
      );
      await ctx.runEask("install -c spinner ivy beacon company-fuzzy", {
        timeout: 35000,
      });
    });

    testUnsafe("eask uninstall -c", async () => {
      await ctx.runEask("uninstall -c ivy company-fuzzy");
    });
  });
});
