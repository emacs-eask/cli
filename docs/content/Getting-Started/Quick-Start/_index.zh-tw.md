---
title: 🔰 快速開始
weight: 100
---

{{< toc >}}

使用 Eask 作為您的 Emacs 包管理工具。

{{< hint info >}}
安裝是跨平台的，使用 [npm](https://www.npmjs.com/)。 關於如何使用其他方法安裝 Eask 的說明，
請參見[安裝](https://emacs-eask.github.io/Getting-Started/Install-Eask/)。

需要安裝 [Git](https://git-scm.com/downloads) 才能運行本教程。
{{< /hint >}}

## 步驟 1: 設置 NodeJS runtime 和 `npm`

請在 [此處](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm#using-a-node-installer-to-install-nodejs-and- npm)
並安裝 `NodeJS` 和 `npm` 對應你當前的操作系統

{{< hint ok >}}
💡 如果您不喜歡 **NodeJS** 和 **npm**，您可以使用 [binary](https://emacs-eask.github.io/Getting-Started/Install-Eask/#binary-cross -platform）
來自我們的 [release](https://github.com/emacs-eask/cli/releases) 頁面。
{{< /hint >}}

## 步驟 2: 安裝 Eask

```sh
$ npm install -g @emacs-eask/cli
```

要驗證您的新安裝：

```sh
$ eask --version
```

## 步驟 3: 導航到現有項目或創建新項目

如果您已有一個現有的 elisp 項目，請導航到項目根文件夾。

```sh
$ cd /path/to/project/dir/
```

創建一個：

```sh
$ eask create package project-name
```

它應該在您當前的工作目錄中創建一個名為 `project-name` 的文件夾。

## 步驟 4： 創建 `Eask` 文件

如果您選擇使用 **`eask create`** 創建項目，請跳過此步驟！

否則，在現有項目中創建 Eask 文件：

```sh
$ eask init
```

您將被問到一些關於您將要創建的包的問題：

```
package name: (your-project)
version: (1.0.0)
description: Your project description!
entry point: (your-project.el)
emacs version: (26.1)
website: https://example.com/project-url/
keywords: tools example
About to write to /path/to/project/Eask:

(package "your-project"
         "1.0.0"
         "Your project description!")

(website-url "https://example.com/project-url/")
(keywords "tools" "example")

(package-file "your-project.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(depends-on "emacs" "26.1")


Is this OK? (yes) yes ⏎
```

您應該能夠在項目文件夾中看到一個 `Eask` 文件。 🎉🎊

## 步驟 5: 開始包開發

要檢查您的包裹信息，請運行：

```sh
$ eask info
```

您應該能夠看到以下信息：

```
your-package (1.0.0) | deps: 0 | devDeps: 0
Your project description!
https://example.com/project-url/

keywords: tools, example

entry: your-package-file.el
kind: single

dist
.total-files: 0
.unpacked-size: 0
```

從一開始，您就不會有任何 `dependencies` 和 `devDependencies`（默認為 `0`）！

## 步驟 6: 管理包檔案

您可以使用 **Eask** 文件中的 `source` 指令來管理包存檔。

```elisp
(source "gnu")    ; 默認
(source "melpa")  ; 添加包 archive
```

{{< hint info >}}
💡 有關更多信息，請參閱 [DSL/source](https://emacs-eask.github.io/DSL/#-source-alias)！
{{< /hint >}}

## 步驟 7: 添加一些依賴

您可以在 **Eask** 文件中使用 `depends-on` 指令添加依賴項。

```elisp
...

(depends-on "f")
(depends-on "ht")
```

{{< hint danger >}}
💡 確保您添加的依賴項在包存檔中可用！

否則你會得到一個錯誤 **`package-name-' is unavailable**！
{{< /hint >}}

## 步驟 8: 安裝依賴

現在我們可以安裝我們在 **Eask** 文件中指定的依賴項：

```elisp
$ eask install-deps
```

您應該會看到 Eask 正確執行，輸出類似如下：

```
Loading package information... done
Installing 2 package dependencies...
  - Installing f (20220405.1534)... done
  - Installing ht (20210119.741)... done

(Total of 2 dependencies installed, 0 skipped)
```

## 也可以看看

- [Commands and options](https://emacs-eask.github.io/Getting-Started/Commands-and-options/)
- [Domain Specific Language](https://emacs-eask.github.io/DSL/)
- [Basic Usage](https://emacs-eask.github.io/Getting-Started/Basic-Usage/)
