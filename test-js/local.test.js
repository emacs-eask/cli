// Here we test all local (workspace) commands by simulating a Emacs
// pacakge development environment!

// Notice, below we clone a random package (repo) that uses Eask as the
// dependencies management tool.

const { emacsVersion, TestContext } = require("./helpers");

const fs = require("node:fs/promises");
const path = require("node:path");

describe("local", () => {
  const cwd = "./test-js/local";
  const ctx = new TestContext(cwd);

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
    it("cat", async () => {
      await ctx.runEask("cat package.json --insecure");
      await ctx.runEask("cat package.json --number --insecure");
    });
    it("concat", async () => {
      await ctx.runEask("concat");
    });
    it("loc", async () => {
      if ((await emacsVersion()) > "27.1") {
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
    afterAll(async () => await ctx.runEask("clean all"));

    // install-deps
    // TODO this times out
    it("prepare --dev", async () => {
      await ctx.runEask("prepare --dev");
    });

    it("package", async () => {
      await ctx.runEask("package");
    });
  });

  describe("Development", () => {
    // TODO takes a long time, organize-imports-java?
    // works when installed
    beforeAll(async () => await ctx.runEask("install-deps"));
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

  // TODO requires install-deps
  describe("Execution", () => {
    test("eval", async () => {
      await ctx.runEask('eval "(progn (require \'mini.pkg.1))"');
    });
  });

  describe("Generating", () => {
    it("generate autoloads", async () => {
      await ctx.runEask("generate autoloads");
    });
    it("generate pkg-file", async () => {
      await ctx.runEask("generate pkg-file");
    });
    it("generate recipe -y", async () => {
      await ctx.runEask("generate recipe -y");
    });
    // await ctx.runEask("generate license gpl-3.0"); # XXX: Avoid API rate limit exceeded error
    it("generate ignore elisp", async () => {
      await ctx.runEask("generate ignore elisp");
    });
  });

  describe("Generating tests", () => {
    it("generate test ert", async () => {
      await ctx.runEask("generate test ert");
      await fs.rm(path.join(cwd, "/test"), { recursive: true, retries: 1 });
    });
    it("generate test ert-runner", async () => {
      await ctx.runEask("generate test ert-runner");
    });
    it("generate test buttercup", async () => {
      await ctx.runEask("generate test buttercup");
    });
    it("generate test ecukes", async () => {
      await ctx.runEask("generate test ecukes");
    });
  });

  describe("Generating workflow", () => {
    it("generate workflow circle-ci", async () => {
      await ctx.runEask("generate workflow circle-ci");
    });
    it("generate workflow github", async () => {
      await ctx.runEask("generate workflow github");
    });
    it("generate workflow gitlab", async () => {
      await ctx.runEask("generate workflow gitlab");
    });
    it("generate workflow travis-ci", async () => {
      await ctx.runEask("generate workflow travis-ci");
    });
  });

  describe("Linting", () => {
    it("lint checkdoc", async () => {
      await ctx.runEask("lint checkdoc");
    });
    it("lint declare", async () => {
      await ctx.runEask("lint declare");
    });
    it("lint elint", async () => {
      await ctx.runEask("lint elint");
    });
    it("lint elisp-lint", async () => {
      await ctx.runEask("lint elisp-lint");
    });
    // XXX: Elsa is not stable, ignore it for now
    test.skip("lint elsa", async () => {
      await ctx.runEask("lint elsa");
    });
    it("lint indent", async () => {
      await ctx.runEask("lint indent");
    });
    it("lint keywords", async () => {
      await ctx.runEask("lint keywords");
    });
    it("lint license", async () => {
      await ctx.runEask("lint license");
    });
    it("lint package", async () => {
      await ctx.runEask("lint package");
    });
    it("lint regexps", async () => {
      if ((await emacsVersion()) > "27.1") {
        await ctx.runEask("lint regexps");
      }
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
      if ((await emacsVersion()) > "29.1") {
        await ctx.runEask("format elisp-autofmt");
      }
    });

    it("format elfmt", async () => {
      await ctx.runEask("format elfmt");
    });
  });

  describe("Cleaning", () => {
    it("clean .eask", async () => {
      await ctx.runEask("clean .eask");
    });
    it("clean elc", async () => {
      await ctx.runEask("clean elc");
    });
    it("clean dist", async () => {
      await ctx.runEask("clean dist");
    });
    it("clean autoloads", async () => {
      await ctx.runEask("clean autoloads");
    });
    it("clean pkg-file", async () => {
      await ctx.runEask("clean pkg-file");
    });
    it("clean log-file", async () => {
      await ctx.runEask("clean log-file");
    });
    it("clean all", async () => {
      await ctx.runEask("clean all");
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
    // TODO hmm, this might cause weird effects
    test.skip("upgrade-eask", async () => {
      await ctx.runEask("upgrade-eask");
    });

    it("refresh", async () => {
      await ctx.runEask("refresh");
    });
  });
});
