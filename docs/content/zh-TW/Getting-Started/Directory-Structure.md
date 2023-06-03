---
title: 🏗️ 目錄結構
weight: 350
---

{{< toc >}}

從命令行運行 **`eask create package`** 生成器將創建
具有以下結構的目錄：

```
.
├── .gitignore
├── Makefile
├── Eask
├── README.md
└── {package-file}.el
```

## 目錄結構說明

以下是每個文件的高級概述。

`.gitignore`

Gitignore 文件，忽略您不想從存儲庫中包含的文件。 默認情況下，它已經排除了 Eask 生成的 `文件` 和 `目錄`。

`Makefile`

已經包含包的基本測試的 Makefile。 它現在默認具有以下任務：

* 測試構建（打包+安裝）
* 測試字節編譯
* 測試 checkdoc（樣式檢查器）
* 測試 lint（包 linter）

`README.md`

生成的文檔文件。 這用於顯示存儲庫中的主頁。

`{package-file}.el`

這是主要的包文件； 你應該在哪裡寫你的elisp代碼。 如果您嘗試創建多文件包； 您需要相應地編輯 `Eask` 文件。
