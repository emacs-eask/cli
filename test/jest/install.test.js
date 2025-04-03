const { emacsVersion, TestContext } = require("./helpers");

jest.setTimeout(1000 * 60 * 10000);

describe("install and uninstall", () => {
  describe("in ./install", () => {
    const ctx = new TestContext("./test/jest/install/");
    const packageName = "mini.pkg.1";

    beforeAll(async () => {
      await ctx.runEask("clean all");
    });

    afterAll(() => ctx.cleanUp());

    it("installs project package", async () => {
      await ctx.runEask("package"); // creates dist/<pkg>.tar
      await ctx.runEask("install"); // installs dependencies and generated package
      const { stderr } = await ctx.runEask("list");
      expect(stderr).toMatch(packageName);
    });

    it("installs specific packages", async () => {
      const { stderr } = await ctx.runEask(
        "install beacon company-fuzzy transwin",
      );
      expect(stderr).toMatch("beacon");
      expect(stderr).toMatch("company-fuzzy");
      expect(stderr).toMatch("transwin");
    });

    it("uninstalls specific packages", async () => {
      await ctx.runEask("install beacon transwin");
      await ctx.runEask("uninstall beacon transwin");
      const { stderr } = await ctx.runEask("list");
      expect(stderr).not.toMatch("beacon");
      expect(stderr).not.toMatch("transwin");
    });

    it("uninstalls project package", async () => {
      await ctx.runEask("uninstall");
      const { stderr } = await ctx.runEask("list");
      expect(stderr).not.toMatch(packageName);
    });

    it("installs dependencies", async () => {
      const { stderr } = await ctx.runEask("install-deps");
      expect(stderr).not.toMatch(packageName);
    });

    it("installs dev dependencies", async () => {
      const { stderr } = await ctx.runEask("install-deps --dev");
      expect(stderr).not.toMatch(packageName);
    });

    it("installs file directly", async () => {
      const { stderr } = await ctx.runEask("install-file ./mini.pkg.2");
      expect(stderr).toMatch("mini.pkg.2");
    });

    it("installs vc directly", async () => {
      if ((await emacsVersion()) >= "29.1") {
        const { stderr } = await ctx.runEask(
          "install-vc https://github.com/jcs-elpa/msgu"
        );
        expect(stderr).toMatch("msgu");
      }
    });
  });

  describe("in an empty project", () => {
    const ctx = new TestContext("./test/jest/empty");
    beforeAll(async () => await ctx.runEask("clean all"));
    afterAll(() => ctx.cleanUp());

    it("errors on install", async () => {
      await expect(ctx.runEask("install")).rejects.toThrow();
    });

    it("errors on uninstall", async () => {
      await expect(ctx.runEask("uninstall")).rejects.toThrow();
    });

    it("errors on reinstall", async () => {
      await expect(ctx.runEask("reinstall")).rejects.toThrow();
    });
  });
});
