const { CommandOutput } = require("./helpers");
const path = require("node:path");

describe("CommandOutput", () => {
  describe("sanitizeString", () => {
    let out = new CommandOutput({}, "./test-js");

    it("replaces relative paths", () => {
      let res = out.sanitizeString("look at ./test-js/foo.js");
      expect(res).toBe("look at ~/foo.js");
    });

    it("replaces relative path literally", () => {
      let res = out.sanitizeString("look at a/test-js/foo.js");
      expect(res).toBe("look at a/test-js/foo.js");
    });

    it("replaces absolute paths", () => {
      let res = out.sanitizeString(
        `look at ${path.resolve("./test-js", "foo.js")}`,
      );
      expect(res).toBe(`look at ${path.join("~", "foo.js")}`);
    });

    it("replaces windows formatted paths", () => {
      let out = new CommandOutput({}, "./test-js/options");
      // force a windows style path
      out.cwdAbsolute = "D:\\a\\cli\\cli\\test-js\\options";
      // when on windows Eask translates those paths like
      let res = out.sanitizeString(
        "d:/a/cli/cli/test-js/options/Eask:7:18 Error: Multiple definition of `package'",
      );
      expect(res).toBe("~/Eask:7:18 Error: Multiple definition of `package'");
    });
  });
});
