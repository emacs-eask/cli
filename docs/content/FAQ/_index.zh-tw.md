---
title: 常見問題
weight: 900
---

以下是一般常見問題列表。

{{< toc >}}

# 🔍 關於 Eask

## ❓ 您需要 Node.JS 才能使用 Eask 嗎？

答案是 **不**.

Eask 在每個版本上構建本機可執行文件，您可以從我們的[發布頁面](https://github.com/emacs-eask/cli/releases)
下載！ 但是，如果您要開發 Eask，那就需要需要 [Node.JS][]！

## ❓ 誰應該使用這個工具？

這是我們的建議； 如果您打算使用特定於操作系統的軟件包（從不去其他平台），去尋找其他工具。
另一方面，Eask 旨在提供每個操作系統之間的最佳一致性。 或者，如果你想學習一個隨處可用的
工具，Eask 是最好的選擇之一。

## ❓ 我在哪裡可以下載 Eask snapshot？

您可以在我們的網站下載最新的可執行文件 (snapshot) [emacs-eask/binaries](https://github.com/emacs-eask/binaries)
代碼庫!

# 🔍 技術選擇

## ❓ 為什麼選擇 Node.JS?

[Node][Node.js] 對各種終端應用程序有更好的支持（相比 shell 腳本）！ 比如豐富多彩的界面，整個 [npm][] 社區等等。
所以你可以更輕鬆地構建跨平台軟件！ 尤其是在微軟之後已經購買了 NPM 公司，並且可能會很好地支持他
們自己的系統。

在 `0.8.6` 版本之後，[Cask][] 似乎不支持 Windows（但支援 [WSL][]）。 在裡面早期版本，他們使用的是 [Python][]
但由於 [Python][] 在 Windows 上的支持只是不如 [Node.JS][]。

有關詳細信息，請參閱[問題 #140](https://github.com/emacs-eask/cli/issues/140)！

## ❓ 為什麼選擇 JavaScript?

我可以使用許多語言來建立 Eask，為什麼我選擇 [JavaScript][]？

我有三個理由

- [JavaScript][] 容易學習。
- 得益於 [Node.js][] runtime，它提供了極佳的跨平台兼容性。
- 我恰好了解 [JavaScript][]，而且用起來得心應手。

我也考慮過 [Rust][] 和 [Common Lisp][]。然而，當我開始這個專案時，[Rust][]
仍是比較新的語言，而 [Common Lisp][] 雖然功能強大，但學習曲線較陡，而且經常被視為有點落伍。因此，我選擇了 JavaScript。

## ❓ 為什麼選擇 yargs?

[yargs][] 擁有非常廣泛的社區； 和它已經被用在很多工具中。 它是跨平台的！
最重要的是，這是在 Linux、macOS 和 Windows 上運行良好的工具之一。

與 Eask 和其他替代方案相比，也存在主要差異。[Cask][]、[makem.sh][] 或 [Eldev][] 更依賴於
`batch` 和 `bash`。 我們選擇了一個路線不同，想把繁重的任務交給高層編程語言，[JavaScript][]。
開髮變得更加容易，因為我們不再需要關心不同類型的 shell！

缺點是 [Node.JS][] 的 runtime，但我們可以簡單地打包整個 CLI 程序變成可執行文件！ 這樣我們就不需要安裝
[Node][Node.js] 和 [npm][]！

# 🔍 用法

## ❓ 如何設定 Eask？

`Eask`-file 是一個 Elisp 檔案，類似於 `.emacs` 或 `init.el`。
正如 Emacs 允許您自訂您不喜歡的方面一樣，Eask 也遵循同樣的原則，讓您配置任何您不喜歡 Eask 的地方。

另一種配置工作區的方法與配置 Emacs 本身類似：

- 使用 .eask/VERSION_NO/early-init.el (僅限於 Emacs 27.1 以後)
- 使用 .eask/VERSION_NO/init.el

## ❓ 如何直接從套件庫安裝套件？

有幾種方法可以做到這一點，但標準的方法是在 Eask 檔案中定義一個 [recipe format][]。

```elisp
(depends-on "organize-imports-java"
            :repo "jcs-elpa/organize-imports-java"
            :fetcher 'github
            :files '(:defaults "sdk" "default"))
```

Eask 會一次建立套件，並寄存本機 ELPA，讓您稍後安裝時使用。這是安裝套件最安全的方式，因為它模擬了最實際的情況。

不過，由於 Eask 也是一個 Elisp 檔案，因此任何其他替代方式也同樣適用。

- [package-vc-install][]
- [quelpa][]
- [use-package][]
- [straight.el][]

# 🔍 故障排除

## ❓ 為什麼安裝時出現錯誤 package target `tar`/`el` not found？

示例錯誤消息，

```text
http://melpa.org/packages/lsp-mode-20220429.647.tar: Not found
```

該問題是由備份存檔不匹配引起的。 一般來說，Eask 將從源中獲取最新的 `archive-contents` 除非你已經
ping 源太多次。 然後來源可能會阻止您的 IP 一段時間分鐘。

您可以等待幾分鐘讓消息來源將您從他們的黑名單。 或者等待備份存檔更新到最新版本。 這
備份存檔存儲庫在 [此處](https://github.com/emacs-eask/archives)。

## ❓ 為什麼我收到錯誤包不可安裝？

示例錯誤消息，

```text
Package not installable `helm'; make sure package archives are included
```

你需要先問問自己； 包裹從哪裡來，是什麼具體來源保存此包信息。 從上面的示例消息中，
`helm` 列在 `melpa` 源代碼中。 您將不得不編輯您的 **Eask** 文件像這樣：

```elisp
...

(source "melpa")  ; <- add this line

(depends-on "helm")
```

## ❓ 為什麼我會看到錯誤？「Package emacs-XX.X' is unavailable」?

示例錯誤消息，

```text
Loading package information... done v
Installing 1 specified package...

  - [1/1] Installing markdown-mode (20250226.231)... Package `emacs-28.1' is unavailable
Wrong type argument: package-desc, nil
```

當 Emacs 嘗試安裝一個需要較新版本 Emacs 的套件時，就會發生這個錯誤。
在某些情況下，此需求並非直接來自套件本身，而是來自其依賴的其中一個套件。

您可以選擇不使用這個套件，或是將 Emacs 升級到所需的版本。

## ❓ 為什麼我會收到狀態為 2 的 git 錯誤？

如果您收到此示例錯誤消息:

```text
Loading package information... done ✓
  - Installing s (20210616.619)... Failed (status 2): git --no-pager remote get-url upstream .
...
```

您可能啟用了 `bug-reference-prog-mode` 。 它還不兼容 Eask, 運行 Eask 的任何命令時都應禁用。

參見[問題 #39](https://github.com/emacs-eask/cli/issues/39#issuecomment-1150770740)
了解更多信息。

## ❓ 為什麼我會以狀態 2 退出 tar？

如果您收到此示例錯誤消息:

```text
Created your-package-0.1.0.tar containing:
tar exited with status 2
Error: Process completed with exit code 1.
```

使用 BSD tar 時您可能會收到此錯誤。解決方法是使用 GNU tar 代替。

```elisp
(setq package-build-tar-executable "/path/to/gnu/tar")
```

在 Windows 中，默認是使用 BSD tar。如果安裝了 Git，則可以使用 Git 中的 tar
可執行文件；它使用 GNU tar。

將以下代碼片段添加到您的 Eask 文件中：

```elisp
;; 在Windows中使用 GNU tar
(when (memq system-type '(cygwin windows-nt ms-dos))
  (setq package-build-tar-executable "C:/Program Files/Git/usr/bin/tar.exe"))
```


<!-- Links -->

[emacs-eask/archives]: https://github.com/emacs-eask/archives

[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev

[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[yargs]: https://www.npmjs.com/package/yargs

[WSL]: https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux
[JavaScript]: https://simple.wikipedia.org/wiki/JavaScript
[Python]: https://www.python.org/
[Rust]: https://www.rust-lang.org/
[Common Lisp]: https://lisp-lang.org/

[recipe format]: https://github.com/melpa/melpa?tab=readme-ov-file#recipe-format

[package-vc-install]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Fetching-Package-Sources.html
[quelpa]: https://github.com/quelpa/quelpa
[use-package]: https://github.com/jwiegley/use-package
[straight.el]: https://github.com/radian-software/straight.el
