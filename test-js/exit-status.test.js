const { TestContext } = require("./helpers");

describe("exit-status", () => {
  const ctx = new TestContext("./test-js/empty");

  // beforeAll(async () => await ctx.runEask("install-deps"), 10000);

  afterAll(() => ctx.cleanUp());

  describe("bump", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("bump")).rejects.toThrow();
    });
  });

  describe("concat", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("concat")).rejects.toThrow();
    });
  });

  describe("docs", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("docs")).rejects.toThrow();
    });
  });

  describe("eval", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("eval")).rejects.toThrow();
    });
  });

  describe("format", () => {
    it("elfmt should error with no files", async () => {
      await expect(ctx.runEask("format elfmt")).rejects.toThrow();
    });

    it("elisp-autofmt should error with no files", async () => {
      await expect(ctx.runEask("format elisp-autofmt")).rejects.toThrow();
    });
  });

  describe("info", () => {
    it("should error with no files", async () => {
      // When run in the current directory this sees the eask file from the main project
      await expect(ctx.runEask("info -g")).rejects.toThrow();
    });
  });

  describe("init", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("concat")).rejects.toThrow();
    });
  });
  describe("concat", () => {
    it("--from cask should error with no files", async () => {
      await expect(ctx.runEask("init --from cask")).rejects.toThrow();
    });

    it("--from keg should error with no files", async () => {
      await expect(ctx.runEask("init --from keg")).rejects.toThrow();
    });

    it("--from eldev should error with no files", async () => {
      await expect(ctx.runEask("init --from eldev")).rejects.toThrow();
    });

    it("--from source should error with no files", async () => {
      await expect(ctx.runEask("init --from source")).rejects.toThrow();
    });
  });

  describe("lint", () => {
    it("checkdoc should error with no files", async () => {
      await expect(ctx.runEask("lint checkdoc")).rejects.toThrow();
    });

    it("declare should error with no files", async () => {
      await expect(ctx.runEask("lint declare")).rejects.toThrow();
    });

    it("elint should error with no files", async () => {
      await expect(ctx.runEask("lint elint")).rejects.toThrow();
    });

    it("elisp-lint should error with no files", async () => {
      await expect(ctx.runEask("lint elisp-lint")).rejects.toThrow();
    });

    // takes a while to install elsa
    it("elsa should error with no files", async () => {
      await expect(ctx.runEask("lint elsa")).rejects.toThrow();
    }, 10000);

    it("indent should error with no files", async () => {
      await expect(ctx.runEask("lint indent")).rejects.toThrow();
    });

    it("keywords should error with no files", async () => {
      await expect(ctx.runEask("lint keywords")).rejects.toThrow();
    });

    it("regexps should error with no files", async () => {
      await expect(ctx.runEask("lint regexps")).rejects.toThrow();
    });
  });

  describe("recipe", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("recipe")).rejects.toThrow();
    });
  });

  describe("run", () => {
    it("run command should error with no files", async () => {
      await expect(ctx.runEask("run command")).rejects.toThrow();
    });

    it("run script should error with no files", async () => {
      await expect(ctx.runEask("run script -g")).rejects.toThrow();
    });
  });

  describe("source", () => {
    it("should error with no files", async () => {
      await expect(ctx.runEask("source")).rejects.toThrow();
    });

    it("add should error with no files", async () => {
      await expect(ctx.runEask("source add")).rejects.toThrow();
    });

    it("add should error with unknown package", async () => {
      await expect(ctx.runEask("source add foo")).rejects.toThrow();
    });

    it("delete should error with no files", async () => {
      await expect(ctx.runEask("source delete")).rejects.toThrow();
    });

    it("delete should error with unknown package", async () => {
      await expect(ctx.runEask("source delete foo")).rejects.toThrow();
    });
  });
});
