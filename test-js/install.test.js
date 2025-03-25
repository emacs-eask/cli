const { TestContext } = require("./helpers");

describe("install and uninstall", () => {
  describe("in ./install", () => {
    const ctx = new TestContext("./test-js/install/");
    const packageName = "mini.pkg.1";

    beforeAll(async () => {
      await ctx.runEask("clean all");
      await ctx.runEask("install-deps");
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
  });

  describe("in an empty project", () => {
    const ctx = new TestContext("./test-js/empty");
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
