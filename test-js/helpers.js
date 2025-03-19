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

module.exports = { testUnsafe };
