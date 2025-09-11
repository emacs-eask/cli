const fs = require("node:fs/promises");
const path = require("node:path");
const process = require("node:process");

const { testUnsafe, TestContext } = require("./helpers");

describe("global", () => {
  // global => install to ~/.eask

  const ctx = new TestContext();

  describe("informational", () => {
    test("eask archives -g", async () => {
      await ctx.runEask("archives -g");
    });

    test("eask list -g --depth=0", async () => {
      await ctx.runEask("list -g --depth=0");
    });

    test("eask outdated -g", async () => {
      await ctx.runEask("outdated -g");
    });
  });

  describe("install", () => {
    /*
     * This test modifies ~/.eask and ~/Eask
     */
    beforeAll(async () => {
      // add a global Easkfile
      if (process.env.ALLOW_UNSAFE) {
        await fs.copyFile(
          "./test/jest/global/Eask",
          path.join(process.env.HOME, "Eask"),
        );
      }
    });

    afterAll(async () => {
      if (process.env.ALLOW_UNSAFE) {
        // remove test changes
        await ctx.runEask("uninstall -g spinner ivy beacon company-fuzzy", , { timeout: 120000 });
        await fs.rm(path.join(process.env.HOME, "Eask"));
      }
    });

    testUnsafe("eask install -g", async () => {
      await ctx.runEask("install -g spinner ivy beacon company-fuzzy", , { timeout: 120000 });
    });

    testUnsafe("eask uninstall -g", async () => {
      await ctx.runEask("uninstall -g ivy company-fuzzy");
    });
  });
});
