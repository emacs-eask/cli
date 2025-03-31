---
title: 🚪 介紹
weight: 0
---

Eask 原本是為 Elisp 專案設計的套件開發工具。然而，它後來擴展到支援廣泛的 Emacs Lisp 任務。
現在它可以用在三個主要方面：

1. 作為 Elisp 套件的開發工具。
2. 用於管理 Emacs 設定中的相依性。
3. 運行 Elisp 程式以達到各種目的 （本質上作為運行時）。

考慮到這些功能，Eask 與 [Cask][]、[makem.sh][] 和 [Eldev][] 等其他編譯工具有何不同？

很好的問題！Eask 已經不僅僅是一個編譯工具，它還具有多種用途！以下是 Eask 的目標：

- **一致性**： 在所有系統中提供可靠的沙箱環境。
- **多功能**： 包含常用的 Emacs 指令，如 `byte-compilation`、`checkdoc` 等。
- **可靠**： 即使發生使用者錯誤，也能提供有用的結果。
- **輕量級**： 可在任何平台上執行，無須依賴任何平台。

*📝 P.S. 詳細資訊請參閱 [為什麼選擇 Eask？](https://emacs-eask.github.io/zh-tw/Getting-Started/Introduction/#-%e7%82%ba%e4%bb%80%e9%ba%bc%e9%81%b8%e6%93%87-eask)。*

## ❓ 為什麼選擇 Eask？

`Eask` 遵循與 [Cask][] 相同的理念。若要瞭解為何要使用 `Eask` (或 [Cask][])，請查看其網站上的
[Why Cask?](https://cask.readthedocs.io/en/latest/guide/introduction.html#introduction-why-cask)。

許多工具，例如 [Cask][]、[makem.sh][] 和 [Eldev][]，並不完全支援 Windows。
`Cask` 已經放棄對傳統 Windows 的支援，`makem.sh` 則依賴 Bash，而 Eldev 雖然支援 Windows，
但其作者並未在該平台上積極使用，這意味著它缺乏完整的測試（從其 CI 工作流程中可以看出）。
相較之下，`Eask` 的設計可在所有主要平台上運作，包括 Linux、macOS 和 Windows 它優先處理跨平台相容性，
並確保不同作業系統之間的一致性。如果 `Eask` 在您的機器上運行，它將可靠地在任何平台上運行。

以下是我們的建議：如果您要開發一個永遠不需要跨平台支援的特定作業系統套件，其他工具可能更適合
。但是，如果您想要一個能夠確保跨不同作業系統的無縫一致性的工具，`Eask` 是一個絕佳的選擇。

`Eask` 的另一大優勢是它的透明性--沒有隱藏的工作流程，也沒有在背景中運行的隱晦進程。
此外，`Eask` 嚴格避免駭客或變通修正，確保解決方案乾淨、可維護且符合最佳實務。

## ⚖️ 比較

該表是通過閱讀這些項目的文檔和源代碼編制的，但作者不是這些工具的專家。 歡迎指正。

### 🔍 專案方面

該表顯示了作者選擇的技術以及項目的構建方式。 此外，他們做出了哪些技術決策？ 放棄支持？ 項目佈局？ 等等。

|            | Eask              | Cask                   | Eldev          | makem.sh               |
|------------|-------------------|------------------------|----------------|------------------------|
| bin 資料夾 | binary, bash, bat | bash, bat              | bash, bat, ps1 | bash                   |
| 跨平台     | ✅                | ❌, 不支援 [Windows][] | ✅             | ❌, 不支援 [Windows][] |
| Emacs 版本 | 26.1+             | 24.5+                  | 24.4+          | 26.1+                  |
| 檔案大小   | 9,000+ 行         | 3,000+ 行              | 8,000+ 行      | 1,200+ 行              |
| 執行檔     | ✅                | ❌                     | ❌             | ❌                     |
| 純 Elisp   | ❌, JavaScript    | ✅                     | ✅             | ✅                     |
| CLI 解析器 | [yargs][]         | [commander][]          | 內建           | 內建                   |

{{< hint info >}}
💡 **makem.sh** 也有很好的比較文檔，請訪問他們的[站點](https://github.com/alphapapa/makem.sh#comparisons)
{{< /hint >}}

### 🔍 功能方面

這是每個工具之間的功能比較。 每種工具都有其優點； 選擇適合您的工具！

如果這些功能沒有在下面列出，要么被遺忘，要么只是被認為太重要了，所以每個工具都有它； 因此我們不將它們添加到列表中。

|                         | Eask                                   | Cask                   | Eldev        | makem.sh |
|-------------------------|----------------------------------------|------------------------|--------------|----------|
| Elisp 配置              | ✅, [DSL][DSL-Eask] 是可選的           | ❌, 僅 [DSL][DSL-Cask] | ✅, 純 elisp | ❌       |
| 處理 `archives` 錯誤    | ✅, 看 [archives][emacs-eask/archives] | ❌                     | ❌           | ❌       |
| `create` 建立專案, 等等 | ✅                                     | ❌                     | ❌           | ❌       |
| `link` 本地依賴         | ✅                                     | ✅                     | ✅           | ❌       |
| `exec` 執行軟件         | ✅                                     | ✅                     | ❌           | ❌       |
| `eval` 表達式           | ✅                                     | ✅                     | ✅           | ❌       |
| `emacs` 執行            | ✅                                     | ✅                     | ❌           | ❌       |
| 支援 `docker`           | ✅                                     | ❌                     | ✅           | ❌       |
| 內建 `linters`          | ✅                                     | ❌                     | ✅           | ❌       |
| 內建 `tests`            | ✅                                     | ❌                     | ✅           | ❌       |
| 執行 `script`           | ✅                                     | ❌                     | ❌           | ❌       |
| 可自行建立指令          | ✅                                     | ❌                     | ✅           | ❌       |
| 子指令                  | ✅                                     | ❌                     | ❌           | ❌       |

## 📰 消息

請參考[這](https://emacs-eask.github.io/Getting-Started/Introduction/#-news).

## 📝 TODO 事項列表

請參考[這](https://emacs-eask.github.io/Getting-Started/Introduction/#-todo-list).

## 📂 基礎項目

Eask 的設計深受以下項目的影響：

- [Cask][] - Emacs 的項目管理工具
- [makem.sh][] -用於構建和測試 Emacs Lisp 包的類似 Makefile 的腳本
- [epm](https://github.com/xuchunyang/epm) - Emacs 包管理器
- [Eldev][] - Elisp 開發工具


<!-- Links -->

[emacs-eask/archives]: https://github.com/emacs-eask/archives
[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev

[yargs]: https://github.com/yargs/yargs
[commander]: https://github.com/rejeep/commander.el

[DSL-Eask]: https://emacs-eask.github.io/DSL/
[DSL-Cask]: https://cask.readthedocs.io/en/latest/guide/dsl.html

[Windows]: https://www.microsoft.com/en-us/windows?r=1
