---
title: 领域特定语言
weight: 200
---

本文檔是關於 [DSL] (https://en.wikipedia.org/wiki/Domain-specific_language)。

{{< toc >}}

# 🚩 包元資料

## 🔍 **package** (`name` `version` `description`)

使用給定的名稱、版本和描述聲明一個包：

```elisp
(package "ert-runner" "0.7.0" "Opinionated Ert testing workflow")
```

所有參數都是字符串。 該版本必須是 Emacs 內置的 `version-to-list` 可以理解的版本。

## 🔍 **website-url** (`url`)

聲明包網站。

```elisp
(website-url "https://github.com/owner/repo.git")
```

## 🔍 **keywords** (`&rest keywords`)

聲明包關鍵字。

```elisp
(keywords "tool" "utility" "emacs")
```

## 🔍 **author** (`name` &optional `email`)

聲明包的作者。

```elisp
(author "使用者名稱" "user.name@example.com")
```

## 🔍 **license** (`name`)

聲明包的作者。

```elisp
(license "GPLv3")
```

# 🚩 Package contents

## 🔍 **package-file** (`file` `version` `description`)

從文件的包頭定義此包及其運行時依賴項（僅用於包開發）。

```elisp
(package-file "foo.el")
```

## 🔍 **package-descriptor** (`pkg-file`)

通過指定文件中包含的包描述符直接聲明所有包元數據，名稱由文件給出。

```elisp
(package-descriptor "foo-pkg.el")
```

## 🔍 **files** (`&rest patterns`)

指定包含在此項目中的文件列表。

```elisp
(files "foo.el")
(files "*.el" "core/*.el")
```

# 🚩 測試

## 🔍 **script** (`name` `command` &rest `args`)

添加內置腳本及其預設的生命週期事件以及任意腳本。

```elisp
(script "test" "echo This is a test!")
```

# 🚩 依賴

## 🔍 **source** (`alias`)

## 🔍 **source** (`name` `url`)

添加包存檔以從中安裝依賴項。

```elisp
(source "gnu")
(source "gnu" "https://elpa.gnu.org/packages/")
```

可用別名：

* `gnu` ([https://elpa.gnu.org/packages/](https://elpa.gnu.org/packages/))
* `nongnu` ([https://elpa.nongnu.org/nongnu/](https://elpa.nongnu.org/nongnu/))
* `celpa` ([https://celpa.conao3.com/](https://celpa.conao3.com/))
* `jcs-elpa` ([https://jcs-emacs.github.io/jcs-elpa/packages/](https://jcs-emacs.github.io/jcs-elpa/packages/))
* `marmalade` ([https://marmalade-repo.org/packages/](https://marmalade-repo.org/packages/))
* `melpa` ([https://melpa.org/packages/](https://melpa.org/packages/))
* `melpa-stable` ([https://stable.melpa.org/packages/](https://stable.melpa.org/packages/))
* `org` ([https://orgmode.org/elpa/](https://orgmode.org/elpa/))
* `shmelpa` ([https://shmelpa.commandlinesystems.com/packages/](https://shmelpa.commandlinesystems.com/packages/))

{{< hint ok >}}
💡 使用**--insecure**讓**https**轉**http**，但不推薦!
{{< /hint >}}

## 🔍 **source-priority** (`name` `priority`)

設置 archive 優先級。

```elisp
(source-priority "gnu" 5)
```

## 🔍 **depends-on** (`package-name` `&optional minimum-version`)

## 🔍 **depends-on** (`package-name` `&rest recipe`)

指定此包的依賴項。

指定 **archives** 中列出的依賴項：

```elisp
(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "company")
```

以 **recipe** 格式指定依賴項：

```elisp
(depends-on "auto-rename-tag" 
            :repo "jcs-elpa/auto-rename-tag" 
            :fetcher 'github)

(depends-on "lsp-ui" 
            :repo "emacs-lsp/lsp-ui"
            :fetcher 'github
            :files '(:defaults "lsp-ui-doc.html" "resources"))
```

{{< hint ok >}}
💡 使用命令 **eask install-deps** 安裝依賴項！
{{< /hint >}}

## 🔍 **development** (`&rest body`)

將正文中所有 `depends-on` 表達式的範圍限定為開發依賴。

```elisp
(development
 (depends-on "ert-runner")
 (depends-on "elsa"))
```

{{< hint ok >}}
💡 您需要為開發依賴項指定 **--dev** 選項！
{{< /hint >}}

## 🔍 **load-paths** (`&rest paths`)

指定要添加到 `load-path` 的路徑。

```elisp
(load-paths "/lisp/")
```

## 🔍 **exec-paths** (`&rest paths`)

指定要添加到 `exec-path` 的路徑。

```elisp
(load-paths "/bin/")
```
