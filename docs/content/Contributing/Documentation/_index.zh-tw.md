---
title: ✒️ 文檔
weight: 30
---

{{< toc >}}

Eask 包含全面的用戶指南。 請嘗試相應地擴展它您實施新功能。

該文檔使用 [Hugo]() 和 GitHub Pages 以 [Markdown](https://gohugo.io/) 編寫。
前者是靜態站點生成器，後者是靜態網頁託管服務來自 GitHub。

{{< hint info >}}
💡 您可以在 **docs/content/** 文件夾下找到我們所有的文檔。
{{< /hint >}}

## 🚩 必備條件

要更改文檔，您應該：

- [hugo](https://gohugo.io/getting-started/quick-start/#step-1-install-hugo)
可執行； 靜態站點生成器。

## 📐 設置

要在本地設置網站，您需要先安裝主題：

```sh
# 克隆代碼庫和子模塊一起...
git clone https://github.com/emacs-eask/cli --recurse-submodules

# 導航到 `docs/theme/geekdoc` 文件夾
cd ./docs/theme/geekdoc/

# 構建主題
npm install && npm run build
```

然後運行 `hugo` 命令：

```sh
# 導航回 `docs` 文件夾
cd ./docs/

# 在本地運行 hugo 服務器
hugo server
```

您應該會看到類似以下畫面：

```console
Start building sites …
hugo v0.148.1-98ba786f2f5dca0866f47ab79f394370bcb77d2f windows/amd64 BuildDate=2025-07-11T12:56:21Z VendorInfo=gohugoio


                  │ EN  │ ZH - TW
──────────────────┼─────┼─────────
 Pages            │  36 │      34
 Paginator pages  │   0 │       0
 Non-page files   │   2 │       0
 Static files     │ 144 │     144
 Processed images │   0 │       0
 Aliases          │   2 │       1
 Cleaned          │   0 │       0

Built in 3987 ms
Environment: "development"
Serving pages from disk
Running in Fast Render Mode. For full rebuilds on change: hugo server --disableFastRender
Web Server is available at http://localhost:1313/ (bind address 127.0.0.1)
Press Ctrl+C to stop
```

就是這樣！ 現在您可以在瀏覽器裡面打開 `localhost:1313`。 🎉

{{< hint info >}}
💡 如果你考慮寫草稿，你可以指定 **-D** 選項。
{{< /hint >}}
