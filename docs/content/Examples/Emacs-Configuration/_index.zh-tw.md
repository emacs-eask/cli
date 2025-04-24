---
title: ⚙️ Emacs 配置
weight: 100
---

`Eask` 是魔法文件，`eask` 會將其讀取為 Emacs 中的初始化文件。語法類似於 `Cask` 文件，但有所不同。

```elisp
;; -*- mode: eask; lexical-binding: t -*-

(package "Emacs configuration's name"
         "0.1.0"
         "Your Emacs configuration's description")  ; optional

(website-url "https://github.com/owner/repo")
(keywords "config")

(package-file "init.el")  ; optional

(script "test" "echo \"Error: no test specified\" && exit 1")

(files "early-init.el" "init.el"
       "lisp/*.el"
       "site-lisp/*.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "26.1")
(depends-on "auto-complete")
(depends-on "dash")
(depends-on "f")
(depends-on "flycheck")
(depends-on "helm")
(depends-on "magit")
(depends-on "popup")
(depends-on "projectile")
(depends-on "s")
(depends-on "smartparens")
(depends-on "yasnippet")
```

{{< hint info >}}
💡 您需要使用 **-c** 或 **--config** 選項來管理您的配置包！
{{< /hint >}}
