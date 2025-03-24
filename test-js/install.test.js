const { TestContext } = require("./helpers");

jest.setTimeout(15000);

describe("install and uninstall", () => {
  describe("in mini.pkg.1", () => {
    const ctx = new TestContext("./test-js/install/");
    afterAll(() => ctx.cleanUp());
    // beforeAll(async () => await exec("eask clean all", { cwd }));

    // TODO also teardown?

    // TODO this can take a really really long time!
    it("installs dependencies", async () => {
      await ctx.runEask("install-deps");
    }, 20000);

    it("installs project package", async () => {
      await ctx.runEask("package");
      await ctx.runEask("install");
    });

    it("installs specific packages", async () => {
      await ctx.runEask("install beacon company-fuzzy transwin");
    });

    // TODO stateful
    it("uninstalls specific packages", async () => {
      await ctx.runEask("uninstall beacon transwin");
    });

    it("uninstalls project package", async () => {
      await ctx.runEask("uninstall");
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
