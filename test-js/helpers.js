const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const fs = require("node:fs/promises");
const path = require("node:path");

/*
 * This file uses JsDoc syntax.
 * Documentation for type helpers like @param: https://jsdoc.app/tags-type
 * Documentation for types in braces:
 *   https://github.com/google/closure-compiler/wiki/Types-in-the-Closure-Type-System
 */

/**
 * As for jest.test but skips running if the ALLOW_UNSAFE env var is set.
 * @param {string} name
 * @param {function({ fail: function }): void | function(): PromiseLike.<unknown>} fn
 * @param {number} [timeout]
 */
function testUnsafe(name, fn, timeout) {
  if (process.env.ALLOW_UNSAFE) {
    return test(name, fn, timeout);
  } else {
    return test.skip(name, fn, timeout);
  }
}

/**
 * Global timeout in ms, set by TIMEOUT env var.
 * This is used in TestContext to limit `exec`.
 * @returns {number}
 */
function getTimeout() {
  // parseInt returns NaN if it fails
  // TIMEOUT <= 0 will be ignored too
  return Number.parseInt(process.env.TIMEOUT) || 25000;
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

  /**
   * Runs command with "eask" prefix.
   * See https://nodejs.org/docs/latest-v20.x/api/child_process.html#child_processexeccommand-options-callback
   * for additional config options.
   * @param {string} command
   * @param {any} config
   * @returns {Promise.<{ stdout, stderr }>}
   */
  runEask(command, config) {
    return this.run(this.easkCommand + " " + command, config);
  }

  run(command, config) {
    return exec(command, {
      cwd: this.cwd,
      signal: this.controller.signal,
      timeout: getTimeout(),
      ...config,
    })
      .then((obj) => {
        if (process.env.DEBUG) {
          console.log(
            `--> ${command}\n` +
              (obj.stdout ? obj.stdout : "") +
              (obj.stderr ? obj.stderr : ""),
          );
        }
        return obj;
      })
      .catch((err) => {
        if (!err.code) err.message += "\nexec: TIMEOUT";
        throw err;
      });
  }

  cleanUp() {
    this.controller.abort();
  }

  /**
   * Test if a file exists using a path relative to this context's directory.
   * Throws if file does not exist, to provide an explanation when used with
   * expect().
   * @param {string} relativePath
   * @returns {Promise.<boolean>}
   */
  fileExists(relativePath) {
    const fullPath = path.resolve(this.cwd, relativePath);
    return fs
      .stat(fullPath)
      .then((_) => true)
      .catch((_) => {
        throw Error("File does not exist: " + fullPath);
      });
  }

  /**
   * @param {string} relativePath
   * @returns {Promise.<string>}
   */
  fileContents(relativePath) {
    const fullPath = path.resolve(this.cwd, relativePath);
    return fs.readFile(fullPath);
  }
}

module.exports = { testUnsafe, emacsVersion, TestContext, getTimeout };
