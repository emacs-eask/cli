const { TestContext } = require("./helpers");

describe("options", () => {
  const ctx = new TestContext("./test-js/options");

  test("eask info -g should error", async () => {
    await expect(ctx.runEask("info -g")).rejects.toThrow();
    await expect(ctx.runEask("info -global")).rejects.toThrow();
  });

  test("eask info -a", async () => {
    await ctx.runEask("info -a");
  });

  test("eask info --all", async () => {
    await ctx.runEask("info --all");
  });

  test("eask info -q", async () => {
    await ctx.runEask("info -q");
  });

  test("eask info --quick", async () => {
    await ctx.runEask("info --quick");
  });

  test("eask info -f", async () => {
    await ctx.runEask("info -f");
  });

  test("eask info --force", async () => {
    await ctx.runEask("info --force");
  });

  test("eask info --debug", async () => {
    await ctx.runEask("info --debug");
  });

  test("eask info --strict", async () => {
    await ctx.runEask("info --strict");
  });

  test("eask info --allow-error", async () => {
    await ctx.runEask("info --allow-error");
  });

  test("eask info --insecure", async () => {
    await ctx.runEask("info --insecure");
  });

  test("eask info --timestamps", async () => {
    await ctx.runEask("info --timestamps");
  });

  test("eask info --log-level", async () => {
    await ctx.runEask("info --log-level");
  });

  test("eask info --elapsed-time", async () => {
    await ctx.runEask("info --elapsed-time");
  });

  test("eask info --et", async () => {
    await ctx.runEask("info --et");
  });

  test("eask info --no-color", async () => {
    await ctx.runEask("info --no-color");
  });

  test("eask info --proxy localhost:8080", async () => {
    await ctx.runEask("info --proxy localhost:8080");
  });

  test("eask info --http-proxy localhost:8080", async () => {
    await ctx.runEask("info --http-proxy localhost:8080");
  });

  test("eask info --https-proxy localhost:8080", async () => {
    await ctx.runEask("info --https-proxy localhost:8080");
  });

  test("eask info --no-proxy localhost:8080", async () => {
    await ctx.runEask("info --no-proxy localhost:8080");
  });

  test("eask info -v 4", async () => {
    await ctx.runEask("info -v 4");
  });

  test("eask info --verbose 4", async () => {
    await ctx.runEask("info --verbose 4");
  });
});
