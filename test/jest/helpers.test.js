import path from 'node:path';
import { CommandOutput } from "./helpers.js";

describe("CommandOutput", () => {
  describe("sanitizeString", () => {
    let out = new CommandOutput({}, "./test/jest");

    it("replaces relative paths", () => {
      let res = out.sanitizeString("look at ./test/jest/foo.js");
      expect(res).toBe("look at ~/foo.js");
    });

    it("replaces relative path literally", () => {
      let res = out.sanitizeString("look at a/test/jest/foo.js");
      expect(res).toBe("look at a/test/jest/foo.js");
    });

    it("replaces absolute paths", () => {
      let res = out.sanitizeString(
        `look at ${path.resolve("./test/jest", "foo.js")}`,
      );
      expect(res).toBe(`look at ${path.join("~", "foo.js")}`);
    });

    it("replaces windows formatted paths", () => {
      let out = new CommandOutput({}, "./test/jest/options");
      // force a windows style path
      out.cwdAbsolute = "D:\\a\\cli\\cli\\test/jest\\options";
      // when on windows Eask translates those paths like
      let res = out.sanitizeString(
        "d:/a/cli/cli/test/jest/options/Eask:7:18 Error: Multiple definition of `package'",
      );
      expect(res).toBe("~/Eask:7:18 Error: Multiple definition of `package'");
    });
  });
});
