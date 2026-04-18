---
title: 🔱 代碼庫概述
weight: 10
---

Eask 由兩個組件組成：命令行工具（Eask CLI）和 Elisp 腳本。

CLI 用於查找相應的 lisp 文件並將其提供給 Emacs 可執行文件。 它會解析所有選項並在 lisp 腳本端將
它們轉換為 Emacs 可理解的選項。 它是用純 JavaScript 編寫的，主文件位於 **src/util.js** 中。

Elisp 腳本用於實際執行通過 CLI 的每個命令。 所有命令都拆分到它的文件中，並組織在 **lisp** 文件夾中。
它是用純 Emacs Lisp 編寫的，主要文件位於 **lisp/_prepare.el** 中。

{{< toc >}}

## 🖥️ CLI 和 Yargs

yargs 命令文件是用 JavaScript 編寫的，位於 **cmds** 文件夾下。 下面的每個文件都將按照約定命名為
`[command_name].js`。 此文件應定義基本的命令行解析規則並正確準備數據以提供給 Emacs session。

讓我們看一下文件 `cmds/core/archives.js` ：

```js
export const command = ['archives', 'sources'];  // 來源的別名
export const desc = 'List out all package archives';

export const handler = async (argv) => {
  await e_call(argv, 'core/archives');
};
```

這是一個標準的 yargs 命令文件，裡麵包含了我們所有的信息需要將它傳遞給 Emacs session。

- **export const command** 是參數模式，但它也接受別名（數組）
- **export const desc** 是命令說明
- **export const handler** 是處理命令執行的異步函數
- **`'core/archives'`** 是 **lisp** 文件夾下的 elisp 文件（沒有 .el 擴展名）。

`eask` 是一個包含我們所有全局選項的 JavaScript 文件。

```js
yargs
  .usage('Usage: eask <command> [options..]')
  .help(
    'help',
    'Show usage instructions.'
  )
  .options({
    'global': {
      description: `change default workspace to ~/.eask/`,
      alias: 'g',
      type: 'boolean',
    },
  })

...
```

對於 **local** 選項，請使用 `export const builder` 並在其下指定命令文件。

看
[yargs/docs/advanced.md](https://github.com/yargs/yargs/blob/main/docs/advanced.md),
官方文檔以獲取更多信息並獲得更好的解釋會有所幫助！

## 📜 Elisp 腳本

Elisp 腳本位於 **lisp** 文件夾下，將等待 CLI 調用。 所有 Elisp 腳本都是用 Emacs Lisp 編寫的，
並且應該具有以下類似的結構：

```elisp
(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (message "PWD is %s" default-directory))
```

請參閱 [開發 API](https://emacs-eask.github.io/Development-API/) 部分了解更多信息！

## 📂 項目結構

您需要研究**三個**地方：

1. `eask` 項目根目錄下的文件
2. `cmds` 包含所有可用命令的文件夾
3. `lisp` 包含所有 elisp 代碼的文件夾

`eask` 是節點入口，主要的 yargs 定義！ `cmds` 和 `lisp` 文件夾是一一對應的命令文件。

### ♻️ Eask 文件中的執行順序

Eask 是這樣執行的：

<p align="center">
<img src="images/execution_order.png" width="80%" />
</p>

- **Eask environment** 構建沙箱並讀取 Eask 文件信息
- **Emacs configuration** 僅在啟用 `-g` 選項時執行
- **before hooks** hook 在命令任務之前運行嗎
- **command execution** 是主要的命令任務
- **after hooks** 命令任務後是否運行 hook
