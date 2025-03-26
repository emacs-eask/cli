const { TestContext } = require("./helpers");

describe("link", () => {
  describe("in ./link", () => {
    const ctx = new TestContext("./test-js/link");

    beforeAll(async () => {
      await ctx.runEask("clean workspace");
      await ctx.runEask("refresh");
    });

    // check list command first since it is used in later tests
    it("eask link list", async () => {
      await ctx.runEask("link list");
    });

    const linkName = "link-to";
    const linkPath = "./link-to/";

    describe("eask link add", () => {
      let addResult; // undefined if the first test fails

      it("adds a link", async () => {
        addResult = await ctx.runEask(`link add "${linkName}" "${linkPath}"`);
        expect(addResult.stderr).toMatch("Created link");
      });

      it("should show the new link", async () => {
        expect(addResult).toBeTruthy(); // fail if previous test failed
        const { stdout, stderr } = await ctx.runEask("link list");
        // the name of the linked package should be included
        // FIXME: Some output is on stderr, some is on stdout
        const output = stderr + "/n" + stdout;
        expect(output).toMatch(linkName);
      });

      // side effects of linking
      it(`installs dependencies of ${linkName}`, async () => {
        const { stderr } = await ctx.runEask("list");
        expect(stderr).toMatch("json-mode");
      });

      it("generates a pkg.el in linked package", async () => {
        expect(await ctx.fileExists(`${linkPath}/${linkName}-pkg.el`)).toBe(
          true,
        );
      });

      it("can add the same link again", async () => {
        await ctx.runEask(`link add "${linkName}" "${linkPath}"`);
      });

      // FIXME: bug
      // - fails when there is a pkg-file in link-fail
      // - succeeds when no pkg-file
      // this means: 1st run it adds link-fail, generates new pkg.el
      //             2nd run it fails to add link-fail!
      it.skip("fails to add a link to uninstallable package", async () => {
        // mini.pkg.1 contains dependencies from melpa, but this package only has gnu
        await expect(
          ctx.runEask('link add "mini.pkg.1" "./link-fail/"'),
        ).rejects.toMatchObject({
          stderr: expect.stringContaining("Package not installable"),
        });
      });
    });

    describe("eask link delete", () => {
      beforeAll(async () => {
        await ctx.runEask("clean workspace");
        // this was tested in a previous command
        // redo it here so that this test will fail if no link was added
        await ctx.runEask(`link add "${linkName}" "${linkPath}"`);
      });

      it("deletes an added link", async () => {
        const { stderr } = await ctx.runEask(`link delete ${linkName}-1.0.0`);
        expect(stderr).toMatch("Package `link-to-1.0.0` unlinked");
      });

      it("should show no links", async () => {
        const { stderr } = await ctx.runEask("link list");
        expect(stderr).toMatch("No linked packages");
      });
    });
  });

  describe("in an empty directory", () => {
    const ctx = new TestContext("./test-js/empty");

    beforeAll(async () => {
      await ctx.runEask("clean all");
    });

    it("eask link list", async () => {
      const { stderr } = await ctx.runEask("link list");
      expect(stderr).toMatch("No linked packages");
    });

    describe("eask link add", () => {
      it("should error when called with no arguments", async () => {
        await expect(ctx.runEask("link add")).rejects.toMatchObject({
          code: 1,
        });
      });

      it("should error when linking a folder with no package file", async () => {
        await expect(ctx.runEask("link add foo .")).rejects.toThrow();
      });

      // FIXME: update this test when this bug is fixed
      test.failing(
        "should error when linking a non-existing folder",
        async () => {
          // currently this prints an error message but does not exit with a code
          await expect(
            ctx.runEask('link add bar "./nothing"'),
          ).rejects.toThrow();
        },
      );
    });

    describe("eask link delete", () => {
      it("should error when called with no arguments", async () => {
        await expect(ctx.runEask("link delete")).rejects.toThrow();
      });

      it("should do nothing when deleting an unknown package", async () => {
        const { stderr } = await ctx.runEask("link delete foo");
        expect(stderr).toMatch("No links present");
      });
    });
  });
});
