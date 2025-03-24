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
     * This test modifies ~/.eask
     */
    testUnsafe(
      "eask install -g",
      async () => {
        // add a global Easkfile
        await fs.copyFile(
          "./test-js/global/Eask",
          path.join(process.env.HOME, "Eask"),
        );

        await ctx.runEask("install -g spinner ivy beacon company-fuzzy");
      },
      15000,
    );

    testUnsafe("eask uninstall -g", async () => {
      await ctx.runEask("uninstall -g ivy company-fuzzy");
    });
  });
});
