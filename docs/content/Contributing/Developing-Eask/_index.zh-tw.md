---
title: 🔨 開發 Eask
weight: 20
---

{{< toc >}}

### 🚩 必備條件

要更改 Eask，您應該：

1. [Node.js](https://nodejs.org/en/) 開發環境。
2. [npm](https://www.npmjs.com/) 包管理器。
3. [yargs](https://github.com/yargs/yargs) 用於 CLI 解析器。
4. [Emacs](https://www.gnu.org/software/emacs/), 26.1 以上！

### 📝 建構

要構建開發環境，您必須使用安裝 Eask [從源代碼建構](https://emacs-eask.github.io/Getting-Started/Install-Eask/#-build-from-source)
的方法。 確保你已經設置了環境 `PATH` 變量，這樣你就可以調用來自終端的`eask`。

完成安裝後，嘗試：

```sh
$ eask locate
```

它應該打印出您工作的 Eask 工作區的位置。 您應該能夠識別 Eask 工作區，即使您安裝
了多個 Eask 版本！

### 📈 測試

Eask 不提供本地測試，我們所有的測試都是使用 GitHub 完成的動作。 請 fork 我們的存儲庫
並將您的更改推送到您的 fork。 GitHub 行動應該為你拿起測試！

確保您的存儲庫（分叉）中啟用了 GitHub Actions。 必須 **設置** -> **操作** -> **常規**
-> **操作權限**； 確保您已經檢查了正確的選項。
