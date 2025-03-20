const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);

/**
 * As for jest.test but skips running if the ALLOW_UNSAFE env var is set.
 * @param {string} name
 * @param {any} fn
 * @param {number} timeout
 */
function testUnsafe(name, fn, timeout) {
  if (process.env.ALLOW_UNSAFE) {
    return test(name, fn, timeout);
  } else {
    return test.skip(name, fn, timeout);
  }
}

/**
 * The version string for system emacs, e.g. "30.0.50".
 * You can compare lexicographically, e.g. if ((await emacsVersion()) > "27") { ... }
 * @returns {Promise.<string>} emacs version string.
 */
async function emacsVersion() {
  const text = await exec("emacs --version");
  const version = text.stdout.match("Emacs ([^ ]+)")?.[1];
  if (!version) {
    throw Error("Couldn't extract Emacs version. Text was:\n" + text);
  }
  return version;
}

class TestContext {
  /**
   * @param {string} cwd Current Working Directory, used for all commands.
   */
  constructor(cwd) {
    this.cwd = cwd;
    this.easkCommand = process.env.EASK_COMMAND || "eask";
    this.controller = new AbortController();
  }

  runEask(command, config) {
    return this.run(this.easkCommand + " " + command, config);
  }

  run(command, config) {
    return exec(command, {
      cwd: this.cwd,
      signal: this.controller.signal,
      ...config,
    }).then((obj) => {
      if (process.env.DEBUG) {
        console.log(command);
        console.log(obj.stdout);
        console.log(obj.stderr);
      }
      return obj;
    });
  }

  cleanUp() {
    this.controller.abort();
  }
}

module.exports = { testUnsafe, emacsVersion, TestContext };
