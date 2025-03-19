const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);

describe("link", () => {
  describe("normally", () => {
    const cwd = "./test-js"; // TODO is this ok?
    it("adds a link", async () => {
      // TODO the first time this is run it initializes the local .eask folder,
      // this can take a while
      const res = await exec(
        'eask link add "mini.pkg.1" "./test/mini.pkg.1/"',
        { cwd },
      );
      expect(res).toBeTruthy();
    });

    it("lists links", async () => {
      const res = await exec("eask link list", { cwd });
      expect(res).toBeTruthy();
    });

    it("deletes links", async () => {
      const res = await exec("eask link delete mini.pkg.1-0.0.1", { cwd });
      expect(res).toBeTruthy();
    });
  });

  describe("in an empty directory", () => {
    const cwd = "./test-js/empty";

    it("eask link add should error", async () => {
      await expect(exec("eask link add", { cwd })).rejects.toThrow();
    });

    // TODO update with ./bin/eask?
    it("adding an unknown package should error", async () => {
      await expect(
        exec("./bin/eask link add foo .", { cwd }),
      ).rejects.toThrow();
    });

    it("deleting an unknown package should error", async () => {
      await expect(exec("./bin/eask link delete", { cwd })).rejects.toThrow();
    });
  });
});
