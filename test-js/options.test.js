const { TestContext } = require("./helpers");

describe("options", () => {
  const ctx = new TestContext("./test-js/options");

  describe("eask info errors", () => {
    const ctx = new TestContext(process.env.HOME);
    test("eask info should error without an Easkfile", async () => {
      const hasGlobal =
        (await ctx.fileExists("Eask").catch(() => false)) ||
        (await ctx.fileExists("Easkfile").catch(() => false));
      // If there is a global Easkfile then just skip testing
      // Better to not test than to raise false errors
      if (!hasGlobal) {
        await expect(ctx.runEask("info -g")).rejects.toThrow();
        await expect(ctx.runEask("info --global")).rejects.toThrow();
      }
    });
  });

  test.each([
    "-a",
    "--all",
    "-q",
    "--quick",
    "-f",
    "--force",
    "--debug",
    "--strict",
    "--allow-error",
    "--insecure",
    "--timestamps",
    "--log-level",
    "--elapsed-time",
    "--et",
    "--no-color",
    "--proxy localhost:8080",
    "--http-proxy localhost:8080",
    "--https-proxy localhost:8080",
    "--no-proxy localhost:8080",
    "-v 4",
    "--verbose 4",
  ])("eask info %s", async (opt) => {
    await ctx.runEask(`info ${opt}`);
  });
});
