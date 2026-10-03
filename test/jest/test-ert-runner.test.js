const { TestContext } = require("./helpers");

describe("test ert-runner", () => {
  const ctx = new TestContext("./test/jest/ert-runner");

  beforeAll(async () => await ctx.runEask("install-deps --dev"));

  afterAll(() => ctx.cleanUp());

  test("eask test ert-runner ./test/*.el", async () => {
    const output = await ctx.runEask("test ert-runner ./test/*.el");
    expect(output.combined()).toContain("Ran 2 tests");
  });

  test("eask test ert-runner -p should run the matching tests", async () => {
    const output = await ctx.runEask(
      "test ert-runner ./test/*.el -p ert-runner-test-1");
    expect(output.combined()).toContain("Ran 1 test in");
  });

  test("eask test ert-runner --pattern should run the matching tests", async () => {
    const output = await ctx.runEask(
      "test ert-runner ./test/*.el --pattern ert-runner-test-2");
    expect(output.combined()).toContain("Ran 1 test in");
  });

  test("eask test ert-runner -p should run nothing when no test matches", async () => {
    const output = await ctx.runEask(
      "test ert-runner ./test/*.el -p no-such-test");
    expect(output.combined()).toContain("Ran 0 tests");
  });

  test("eask test ert-runner --reporter should set the reporter", async () => {
    const output = await ctx.runEask(
      "test ert-runner ./test/*.el --reporter ert");
    expect(output.combined()).toContain("Running 2 tests");
  });

  test("eask test ert-runner --tags should run the tagged tests", async () => {
    const output = await ctx.runEask(
      "test ert-runner ./test/*.el -t no-such-tag");
    expect(output.combined()).toContain("Ran 0 tests");
  });

  test("eask test ert-runner should accept the eask options", async () => {
    const output = await ctx.runEask("test ert-runner ./test/*.el -v 4");
    expect(output.combined()).toContain("Ran 2 tests");
  });
});
