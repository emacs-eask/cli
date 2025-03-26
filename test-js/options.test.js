const { TestContext } = require("./helpers");

describe("options", () => {
  const ctx = new TestContext("./test-js/options");

  test("eask info -g should error", async () => {
    await expect(ctx.runEask("info -g")).rejects.toThrow();
    await expect(ctx.runEask("info -global")).rejects.toThrow();
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
