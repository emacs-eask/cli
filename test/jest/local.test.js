// Here we test all local (workspace) commands by simulating a Emacs
// pacakge development environment!

// Notice, below we clone a random package (repo) that uses Eask as the
// dependencies management tool.

const { emacsVersion, TestContext } = require("./helpers");

describe("local", () => {
  const cwd = "./test/jest/local";
  const ctx = new TestContext(cwd);

  // NOTE: `install-deps` takes a long time in this package
  //       this is because of recipe dependencies triggering
  //       "temporary archives" build.
  beforeAll(async () => {
    await ctx.runEask("install-deps", { timeout: 40000 });
  });

  afterAll(() => ctx.cleanUp());

  describe("info commands", () => {
    it("info", async () => {
      await ctx.runEask("info");
    });
    it("status", async () => {
      await ctx.runEask("status");
    });
    it("archives", async () => {
      await ctx.runEask("archives");
    });
    it("archives --all", async () => {
      await ctx.runEask("archives --all");
    });
    it("list --depth=0", async () => {
      await ctx.runEask("list --depth=0");
    });
    // this alters the eask file
    it("bump", async () => {
      await ctx.runEask("bump major minor patch");
    });
    // NOTE: eask cat is a long running command
    it("cat", async () => {
      // cat requires e2ansi, installing it takes a while
      await ctx.runEask("cat package.json --insecure");
      await ctx.runEask("cat package.json --number --insecure");
    });
    it("concat", async () => {
      await ctx.runEask("concat");
    });
    // NOTE: eask loc is a long running command
    it("loc", async () => {
      // installs markdown mode -- depends on emacs 28.1
      if (cmp(await emacsVersion(), "28.1") == 1) {
        await ctx.runEask("loc");
        await ctx.runEask("loc Eask");
      }
    });
  });

  describe("PATH environment", () => {
    it("path", async () => {
      await ctx.runEask("path");
      await ctx.runEask("path bin");
    });
    it("load-path", async () => {
      await ctx.runEask("load-path");
      await ctx.runEask("load-path bin");
    });
  });

  describe("Preparation", () => {
    beforeAll(async () => await ctx.runEask("prepare --dev"));
    afterAll(async () => await ctx.runEask("clean dist"));

    it("package", async () => {
      await ctx.runEask("package");
    });
  });

  describe("Development", () => {
    beforeAll(async () => {
      await ctx.runEask("install-deps", { timeout: 40000 });
    });

    // this requires install-deps
    it("compile", async () => {
      await ctx.runEask("compile");
      await ctx.runEask("compile --clean");
      await ctx.runEask("recompile");
      await ctx.runEask("recompile --clean");
    });

    it("recipe", async () => {
      await ctx.runEask("recipe");
    });

    it("keywords", async () => {
      await ctx.runEask("keywords");
    });

    it("run script", async () => {
      await ctx.runEask("run script");
      await ctx.runEask("run script test");
      await ctx.runEask("run script extra -- Extra arguments!");
      await ctx.runEask("run script --all");
    });

    it("run command", async () => {
      await ctx.runEask("run command");
      await ctx.runEask("run command test");
      await ctx.runEask("run command mini-test-3 -- Extra arguments!");
      await ctx.runEask("run command --all");
    });
  });

  describe("Execution", () => {
    beforeAll(async () => {
      await ctx.runEask("install-deps", { timeout: 40000 });
    });

    test("eval", async () => {
      await ctx.runEask('eval "(progn (require \'mini.pkg.1))"');
    });
  });

  describe("Generating", () => {
    beforeAll(async () => {
      // from generate ignore elisp
      await ctx.removeFiles(".gitignore");
    });

    afterAll(async () => {
      await ctx.runEask("clean autoloads").catch(() => {});
      await ctx.runEask("clean pkg-file").catch(() => {});

      await ctx.removeFiles(
        "recipes",    // from generate recipes
        ".gitignore", // from generate ignore elisp
      );
    });

    it.each([
      "generate autoloads",
      "generate pkg-file",
      "generate recipe -y",
      "generate ignore elisp",
    ])("eask %s", async (cmd) => {
      await ctx.runEask(cmd);
    });

    it.skip("eask generate license", async () => {
      // XXX: Avoid API rate limit exceeded error
      await ctx.runEask("generate license gpl-3.0");
    });
  });

  describe("Generating tests", () => {
    // some tests fail if ./test already exists
    beforeEach(async () => await ctx.removeFiles("features", "test", "tests"));
    afterAll(async () => await ctx.removeFiles("features", "test", "tests"));

    it.each([
      "generate test ert",
      "generate test ert-runner",
      "generate test buttercup",
      "generate test ecukes",
    ])("eask %s", async (cmd) => {
      await ctx.runEask(cmd);
    });
  });

  describe("Generating workflow", () => {
    beforeEach(
      async () =>
      await ctx.removeFiles(
        ".circleci",
        ".gitlab-ci.yml",
        ".github",
        ".travis.yml",
      ),
    );
    afterAll(
      async () =>
      await ctx.removeFiles(
        ".circleci",
        ".gitlab-ci.yml",
        ".github",
        ".travis.yml",
      ),
    );

    it.each([
      "generate workflow circle-ci",
      "generate workflow github",
      "generate workflow gitlab",
      "generate workflow travis-ci",
    ])("eask %s", async (cmd) => {
      await ctx.runEask(cmd);
    });
  });

  describe("Linting", () => {
    // some lint commands may fail if packages are missing
    beforeAll(async () => {
      await ctx.runEask("install-deps", { timeout: 40000 });
    });

    it.each([
      "lint checkdoc",
      "lint declare",
      "lint elint",
      "lint elisp-lint",
      "lint indent",
      "lint keywords",
      "lint license",
      "lint package",
    ])("eask %s", async (cmd) => {
      await ctx.runEask(cmd);
    });

    // XXX: Elsa is not stable, ignore it for now
    test.skip("lint elsa", async () => {
      await ctx.runEask("lint elsa");
    });

    it("lint regexps", async () => {
      if (cmp(await emacsVersion(), "27.1") == 1) {
        await ctx.runEask("lint regexps");
      }
    });

    it("lint org *.org", async () => {
      await ctx.runEask("lint org *.org");
    });
  });

  describe("Testing", () => {
    it("test activate", async () => {
      await ctx.runEask("test activate");
    });
  });

  describe("Formatting", () => {
    // installs elisp-autofmt
    it("format elisp-autofmt", async () => {
      if (cmp(await emacsVersion(), "29.1") == 1) {
        await ctx.runEask("format elisp-autofmt");
      }
    });

    it("format elfmt", async () => {
      await ctx.runEask("format elfmt");
    });
  });

  describe("Cleaning", () => {
    it.each([
      "clean .eask",
      "clean elc",
      "clean dist",
      "clean autoloads",
      "clean pkg-file",
      "clean log-file",
      "clean all",
    ])("eask %s", async (cmd) => {
      await ctx.runEask(cmd);
    });
  });

  describe("Control DSL", () => {
    it("source add", async () => {
      await ctx.runEask('source add test "https://test.elpa.com"');
    });
    it("source delete test", async () => {
      await ctx.runEask("source delete test");
    });
    it("source list", async () => {
      await ctx.runEask("source list");
    });
  });

  describe("Util", () => {
    it("locate", async () => {
      await ctx.runEask("locate");
    });
    it("refresh", async () => {
      await ctx.runEask("refresh");
    });
  });
});
