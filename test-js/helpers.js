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

/** Provides transformations on output of node.exec(). */
class CommandOutput {
  constructor(output, cwd) {
    this.stderr = output.stderr;
    this.stdout = output.stdout;
    this.cwd = cwd;
    this.cwdAbsolute = path.resolve(cwd || ".");
  }

  /**
   * Both stdout and stderr concatenated as a string.
   * @returns {string}
   */
  combined() {
    return this.stdout + "\n" + this.stderr;
  }

  /**
   * Output as a plain object.
   * @returns {{ sdout: string, stderr: string }}
   */
  raw() {
    return {
      stderr: this.stderr,
      stdout: this.stdout,
    };
  }

  /**
   * Attempt to make `s` safe for snapshotting by replacing local data.
   * @param {string} s
   * @returns {string}
   */
  sanitizeString(s) {
    let working = s;
    // windows paths
    if (this.cwdAbsolute.startsWith(":", 1)) {
      // cwdAbsolute is a windows path e.g. C:\foo\bar
      // Eask outputs windows paths like d:/a/cli/Eask
      let cwdTranslated = this.cwdAbsolute.replaceAll("\\", "/");
      // lowercase the drive letter
      cwdTranslated =
        cwdTranslated[0].toLowerCase() + cwdTranslated.substring(1);
      working = working.replaceAll(cwdTranslated, "~");
    }

    // replace absolute path
    working = working.replaceAll(this.cwdAbsolute, "~");
    // replace relative path
    working = working.replaceAll(this.cwd, "~");
    return working;
  }

  /**
   * Create a copy of this object with output safe for snapshotting.
   * @param {...function(string):string[]} sanitizeFns functions to apply to the output.
   * These apply after the default sanitize functions.
   * @returns {CommandOutput}
   */
  sanitized(...sanitizeFns) {
    let sani = (s) =>
      sanitizeFns.reduce((s1, f) => f.call({}, s1), this.sanitizeString(s));

    return new CommandOutput(
      {
        stdout: sani(this.stdout),
        stderr: sani(this.stderr),
      },
      this.cwd,
    );
  }
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
   * @returns {Promise.<CommandOutput>}
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
        return new CommandOutput(obj, this.cwd);
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
        throw Error("File does not exist, or is not readable: " + fullPath);
      });
  }

  /**
   * Get file contents as a string, relative to the context's directory.
   * @param {string} relativePath
   * @returns {Promise.<string>}
   */
  fileContents(relativePath) {
    const fullPath = path.resolve(this.cwd, relativePath);
    return fs.readFile(fullPath);
  }
}

module.exports = {
  testUnsafe,
  emacsVersion,
  TestContext,
  getTimeout,
  CommandOutput,
};
