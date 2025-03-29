---
title: 🔨 開發 Eask
weight: 20
---

{{< toc >}}

### 🚩 必備條件

要更改 Eask，您應該：

1. [Node.js][] 開發環境。
2. [npm][] 包管理器。
3. [Emacs][], 26.1 以上！

### 📝 建構

要構建開發環境，您必須使用安裝 Eask [從源代碼建構][Build from source]
的方法。 確保你已經設置了環境 `PATH` 變量，這樣你就可以調用來自終端的`eask`。

完成安裝後，嘗試：

```sh
eask locate
```

它應該會列印出 `eask` 可執行檔的位置。即使您已安裝多個 Eask 版本，也應該可以辨識 Eask 可執行檔的位置！

### 🧪 測試

Eask 的本地測試使用 [Jest][] 測試框架完成。Jest 是用 Javascript 寫成的成熟且支援良好的測試框架。
選擇 Jest 的原因與本專案[選擇 Javascript 的原因大致相同][Why JS?]。此外，Jest 容易學習，而且內建了對快照測試的支援。

### ⚗️ 執行測試

如果尚未執行，請執行 `npm install --dev` 。

永遠從專案根目錄執行 (即與 `package.json` 相同的目錄)

- 執行所有測試        `npm run test`
- 執行單次測試        `npm run test path/to/test.js`
- 以完整輸出執行測試   `npm run test-debug`
- 移除測試時建立的檔案 `npm run test-reset`

由於 `npm run test` 只會執行 Jest，您也可以將 Jest 選項傳給上面的指令。例如

- 執行名稱 (在 `test()` 區塊中) 符合`npm run test -t 'eask lint .*'` 的測試
- 重新執行失敗的測試 `npm run test -f`

### 🌍 環境變數

| Name           | Type    | Default | Meaning                                                              |
|:---------------|:--------|:--------|:---------------------------------------------------------------------|
| `ALLOW_UNSAFE` | `bool*` | false   | 在 `testUnsafe` 區塊中執行測試。這些區塊可能**覆寫**您個人的 emacs 配置或設定。 |
| `DEBUG`        | `bool*` | false   | 列印測試中指令的完整輸出。                                                |
| `EASK_COMMAND` | path    | "eask"  | Eask 的路徑。通常是 `eask` 或 `$PWD/bin/eask` 來使用本機變更。              |
| `TIMEOUT`      | number  | 25000   | 命令超時，以毫秒為單位。請注意這與 Jest 的逾時時間不同，Jest 的逾時時間應該更長。   |

{{< hint info >}}
💡 Node.js 將環境變數當成字串來處理。這表示 `DEBUG=0`、`DEBUG=false` 都 _enable_ `DEBUG`。
唯一禁用布林旗標的設定是 null，例如 `DEBUG=`。
{{< /hint >}}

### 🔬 如何撰寫測試

**資料夾結構**

測試應該放在 `test/js` 中。
相關的測試應該在同一個檔案中，後綴為 `.test.js`，通常以測試的功能或指令命名，例如 `link.test.js` 測試 `eask link` 指令。

如果測試需要一些特定的專案檔案，請將它們放在 `test/js` 中的新資料夾。
例如，在 `test/js/foo` 中的檔案預計會用於 `foo.test.js`。

例外的是 `test/js/empty`，它只是一個空的資料夾。
如果您使用它，請務必在測試前執行 `eask clean all`。

** 測試檔案結構**

``` javascript
const { TestContext } = require("./helpers");

describe("emacs", () => {
  const ctx = new TestContext("./test/jest/empty");

  beforeAll(async () => await ctx.runEask("clean all"));
  afterAll(() => ctx.cleanUp);

  test("eask emacs --version", async () => {
    await ctx.runEask("emacs --version");
  });

  test("eask emacs --batch --eval", async () => {
    await ctx.runEask(
      'emacs --batch --eval "(require (quote ert))" --eval "(ert-deftest mytest () (should-not (display-graphic-p)))" -f ert-run-tests-batch',
    );
  });
});
```

在 Jest 中，您可以使用 `describe` 將相關的測試分組。在相同的 `describe` 區塊中的測試可以共用設定/停用程式碼，可以作為一個群組停用，並在輸出中被歸類在相同的標題下。

`describe` 區塊可以嵌套在其他的 `describe` 區塊中。
當測試在不同目錄中執行時，或配合 "given, when, then" 的測試風格時，加入巢狀的 `describe` 是個好主意。

您應該為每個測試目錄建立新的 `TestContext` 物件。
所有的 `runEask` 指令都會使用 `TestContext` 的工作目錄。

Jest 的測試在 `test` 區塊中。請注意 `it` 是 `test` 的別名。
測試可以在程式碼中選擇性地停用，就像這樣：

- `test.only(name,fn)` 只執行檔案中的測試
- `test.skip(name,fn)` 跳過執行測試，但仍會列印其名稱
- `test.failing(name, fn)` 反轉測試的意義：它*應該*失敗。

`expect` API 以不同的方式匹配值，通常會列印差異作為失敗報告的一部分。
更多資訊請參閱 Jest 的 [expect()](https://jestjs.io/docs/expect) API。

在 `test` 區塊中拋出的未捕獲錯誤會使它失敗並報告錯誤。
這就是為什麼許多測試沒有 `expect` 呼叫，它們只是檢查指令是否成功。

來自 `runEask` 的輸出會包裝在提供一些轉換方法的輔助類 `CommandOutput` 中。
例如，如果您有 `const out = await ctx.runEask("analyze");`，那麼

- `out.combined()` 將 stdout 和 stderr 匯整為一個字串、
- `out.raw()` 返回一個只有`stdout`和`stderr`屬性的純物件、
- `out.sanitized()` 會取代所有符合上下文路徑的絕對路徑。

由於這個類別包覆了 Node 的 `exec()` 方法的輸出，你仍然可以存取 `stdout` 和 `stderr`：

``` javascript
const { stderr, stdout } = await ctx.runEask("analyze");
```

有些指令會建立檔案或目錄，這些檔案或目錄應該在測試執行後移除。
例如，`eask generate ignore elisp` 會建立一個 `.gitignore` 檔案。
您可以使用 context 的 `removeFiles` 方法來移除相對於 context 路徑的檔案和目錄：

``` javascript
  describe("Generating", () => {
    beforeAll(async () => await ctx.removeFiles(".gitignore"));
    afterAll(async () => await ctx.removeFiles(".gitignore"));

    it("eask generate ignore elisp", async () => {
      await ctx.runEask("generate ignore elisp");
    });
  });
```

請注意，`removeFiles()` 會遞迴移除目錄，但不接受模式。
因此，要移除 `./test` 中的所有檔案，只需呼叫 `ctx.remove("test")`。
您可以在一次呼叫中傳入多個檔案或目錄名稱： `ctx.remove("test", ".gitignore")`.

使用 `TestContext.cleanUp()` 立即中止在該上下文中呼叫的任何仍在執行的指令。
如果 Jest 在測試執行後報告"偵測到開啟的句柄"，請使用此功能。
請注意，`cleanUp` 會傳送一個信號給所有使用上下文的 `runEask` 指令啟動的進程。
如果在 `afterEach` 鈎結中使用 (即每次測試後)，可能會導致失敗。

### 🪧 快照

[快照測試](https://jestjs.io/docs/snapshot-testing) 將測試的輸出與預期輸出的儲存副本進行比對。
例如:

``` javascript
test("eask analyze", async () => {
  const res = await ctx.runEask("analyze");
  expect(res.raw()).toMatchSnapshot();
});
```

第一次執行時，Jest 會建立一個新的快照，儲存在鄰近的 `__snapshot__` 目錄中。

您應該將此檔案加入版本控制，因為它是測試的關鍵部分。
如果快照變更，您可以使用選項 `-u` 執行 Jest 來更新快照，例如 `npm run test -- -u` 會更新所有變更的快照。

任何類型的輸出都可以用於快照測試。您可以在變更檔案後快照檔案內容

``` javascript
test("eask analyze", async () => {
  await ctx.runEask("foo");
  const file = ctx.fileContents("Easkfile"); // 檔案為字串
  expect(file).toMatchSnapshot();
});
```

通常快照會包含會隨時間或環境改變的資料，例如時間戳記或檔案路徑。
`eask analyze` 的快照包含絕對檔案路徑，這些路徑在每台機器上都會不同。

從 `runEask` 輸出的內容會包裝在提供一些轉換方法的輔助類 `CommandOutput` 中。
最簡單的只是移除絕對檔案路徑：

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res.sanitized() // 以 "~" 取代絕對路徑的 CommandOutput 物件
                      .raw();      // 適合快照的物件 { stderr、stdout }
  expect(resClean).toMatchSnapshot();
});
```

您可以加入自訂的取代函數。在這裡，數字會被 `"x"` 取代。
字串 `"x:x"` 將被 `"y"` 取代。

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res
    .sanitized(
      (x) => x.replace(/[0-9]+/g, "x"),
      (x) => x.replaceAll("x:x", "y"),
    )
    .raw();
  expect(resClean).toMatchSnapshot();
});
```

請務必使用 `g` regex 標記，這樣所有匹配的出現都會被取代，或者您也可以使用 `replaceAll`。
使用者提供的函數會在預設的 sanitize 函數之外執行，並且會依給定的順序執行。

### ⏱️ 超時

T這裡有兩個超時設定，一個用於 Jest，另一個用於 Node 的 `exec()`。
所有逾時值的單位都是毫秒。

由於 `exec()` 超時會立即終止執行中的指令並報告輸出，因此使用它來代替 Jest 的超時會更好。

若要變更單一指令的逾時時間

``` javascript
ctx.runEask("analyze", { timeout: 10000})
```

若要變更單次執行的全局逾時，請使用 env var

``` shell
env TIMEOUT=30000 npm run test
```

若要永久變更全局超時，請在 `./helpers.js` 中設定預設值。

如果您變更任一全域逾時時間，**請在 `package.json` 中設定全域逾時時間，以確保全域 Jest 逾時時間較長**。

``` json
"jest": {
  "rootDir": "./test/jest",
  "testTimeout": 40000
}
```

### 📜 樣式

以下是一些測試指令的常見模式。
每種模式都假設 `ctx` 是一個 `TestContext` 物件。

**檢查指令是否成功：**

``` javascript
test("eask analyze", async () => {
  await ctx.runEask("analyze");
});
```

在 `test` 區塊中拋出的未捕獲錯誤將使測試失敗，並會報告錯誤。
失敗的指令會包含 stderr 和 stdout。

**檢查命令失敗：**

``` javascript
test("eask analyze", async () => {
  await expect(ctx.runEask("analyze")).rejects.toThrow();
});
```

**檢查指令是否以特定代碼失敗：**

``` javascript
test("eask link add should error", async () => {
  // 錯誤物件的屬性代碼應該是 1
  await expect(ctx.runEask("link add")).rejects.toMatchObject({
    code: 1,
  });
});
```

** 檢查指令是否產生某些輸出：**

``` javascript
test("eask analyze", async () => {
  const out = await ctx.runEask("analyze");
  expect(out.stderr).toMatch("success"); // 應顯示為子字串
  // 如果您要同時檢查 `stderr` 和 `stdout`，只要將它們串連即可
  expect(out.stdout + "/n" + out.stderr).toMatch("success");
  // 使用輔助方法也是一樣
  expect(out.combined()).toMatch("success");
});
```

**根據快照檢查指令輸出：**

簡單的輸出比對

``` javascript
test("eask analyze", async () => {
  const res = await ctx.runEask("analyze");
  expect(res).toMatchSnapshot();
});
```

更新所有已變更的快照： `npm run test -- -u`

移除輸出中的絕對檔案路徑：

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res.sanitized() // 以 "~" 取代絕對路徑的 CommandOutput 物件
                      .raw();      // 適合快照的物件 { stderr、stdout }
  expect(resClean).toMatchSnapshot();
});
```

套用自訂的轉換來消毒輸出：

``` javascript
it("matches snapshot", async () => {
  const res = await ctx.runEask("analyze");
  const resClean = res
    .sanitized(
      (x) => x.replace(/[0-9]+/g, "x"),
      (x) => x.replace(/x:x/g, "y"),
    )
    .raw();
  expect(resClean).toMatchSnapshot();
});
```

使用者提供的函數會在預設的 sanitize 函數之外執行，並且會按照所給予的順序執行。

** 修改使用者環境的指令：**

例如，使用 `-c` 或 `-g` 選項的指令。

``` javascript
const { testUnsafe } = require('./helpers');

// 只有當 ALLOW_UNSAFE 為 != 0 時，才會執行此指令。
testUnsafe("global install", async () => {
  // 這會安裝在 ~/.eask 中，並變更 ~/Eask
  await ctx.runEask("install -g foo");
});
```

### 🩺 常見問題

- 使用 `runEask()` 時，只傳 Eask *arguments*，不傳 `eask` 指令本身。
- 總是 `await` 任何觸發指令的表達式。
- 當使用`expect(...).rejects`時，它應該被 awaited，以便承諾在測試完成前拒絕。
- `TestContext` 的資料夾參數應該相對於專案根目錄，如果它不存在，您可能會得到一個錯誤`ENOENT`。
- 如果您收到 Jest 回報打開句柄的錯誤，那麼請嘗試使用 `afterAll(() => ctx.cleanUp())`
- 有兩個逾時值：一個用於 Jest (在 `package.json` 中設定)，另一個用於 `node.exec`，在 `./helpers.js` 中透過 env var 設定。
`node.exec` 的逾時設定比 Jest 的低，所以改變測試的逾時值或 `jest.setTimeout` 通常不會有影響。
取而代之，在指令本身設定超時 `runEask("eask emacs", { timeout: 100000 })`。


<!-- Links -->

[Build from source]: https://emacs-eask.github.io/zh-tw/Getting-Started/Install-Eask/#-%e5%be%9e%e5%8e%9f%e5%a7%8b%e7%a2%bc%e6%a7%8b%e5%bb%ba
[Why JS?]: https://emacs-eask.github.io/zh-tw/FAQ/#-%E7%82%BA%E4%BB%80%E9%BA%BC%E9%81%B8%E6%93%87-javascript

[Node.js]: https://nodejs.org/en/
[npm]: https://www.npmjs.com/
[yargs]: https://github.com/yargs/yargs
[Emacs]: https://www.gnu.org/software/emacs/

[Jest]: https://jestjs.io