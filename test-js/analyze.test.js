const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);

describe("analyze", () => {
  describe("dsl", () => {
    const cwd = "./test-js/dsl";
    it("handles plain text", async () => {
      const res = await exec("eask analyze", { cwd });
      expect(res).toBeTruthy();

      const res1 = await exec("eask analyze Eask", { cwd });
      expect(res1).toBeTruthy();
    });

    it("handles json", async () => {
      const res = await exec("eask analyze --json", { cwd });
      expect(res).toBeTruthy();

      const res1 = await exec("eask analyze Eask --json", {
        cwd,
      });
      expect(res1).toBeTruthy();
    });
  });

  describe("error", () => {
    const cwd = "./test-js/empty";
    it("errors in an empty dir", async () => {
      await expect(exec("eask scrog", { cwd })).rejects.toThrow();
    });
  });

  describe("metadata", () => {
    const cwd = "./test-js/metadata";
    it("handles plain text", async () => {
      const res = await exec("eask analyze", { cwd });
      expect(res).toBeTruthy();

      const res1 = await exec("eask analyze Eask", { cwd });
      expect(res1).toBeTruthy();
    });

    it("handles json", async () => {
      const res = await exec("eask analyze --json", { cwd });
      expect(res).toBeTruthy();

      const res1 = await exec("eask analyze Eask --json", {
        cwd,
      });
      expect(res1).toBeTruthy();
    });
  });
});
