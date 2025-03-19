const util = require("node:util");
const exec = util.promisify(require("node:child_process").exec);
const execSync = require("node:child_process").execSync;

describe("options", () => {
  const cwd = "./test-js/options";

  test("eask info -g should error", async () => {
    await expect(exec("./bin/eask info -g", { cwd })).rejects.toThrow();
    await expect(exec("./bin/eask info -global", { cwd })).rejects.toThrow();
  });

  test("eask info -a", async () => {
    const res = await exec("eask info -a", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask info --all", async () => {
    const res = await exec("eask info --all", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask info -q", async () => {
    const res = await exec("eask info -q", { cwd });
    expect(res).toBeTruthy();
  });

  test("eask info --quick", async () => {
    const res = await exec("eask info --quick", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info -f", async () => {
    const res = await exec("eask info -f", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --force", async () => {
    const res = await exec("eask info --force", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --debug", async () => {
    const res = await exec("eask info --debug", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --strict", async () => {
    const res = await exec("eask info --strict", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --allow-error", async () => {
    const res = await exec("eask info --allow-error", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --insecure", async () => {
    const res = await exec("eask info --insecure", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --timestamps", async () => {
    const res = await exec("eask info --timestamps", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --log-level", async () => {
    const res = await exec("eask info --log-level", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --elapsed-time", async () => {
    const res = await exec("eask info --elapsed-time", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --et", async () => {
    const res = await exec("eask info --et", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --no-color", async () => {
    const res = await exec("eask info --no-color", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --proxy localhost:8080", async () => {
    const res = await exec("eask info --proxy localhost:8080", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --http-proxy localhost:8080", async () => {
    const res = await exec("eask info --http-proxy localhost:8080", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --https-proxy localhost:8080", async () => {
    const res = await exec("eask info --https-proxy localhost:8080", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --no-proxy localhost:8080", async () => {
    const res = await exec("eask info --no-proxy localhost:8080", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info -v 4", async () => {
    const res = await exec("eask info -v 4", { cwd });
    expect(res).toBeTruthy();
  });
  test("eask info --verbose 4", async () => {
    const res = await exec("eask info --verbose 4", { cwd });
    expect(res).toBeTruthy();
  });
});
