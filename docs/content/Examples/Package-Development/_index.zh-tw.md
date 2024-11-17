---
title: 📦 Package 開發
weight: 200
---

`Eask` 是魔法文件，`eask` 會將其讀取為 Emacs 中的初始化文件。語法類似於 `Cask` 文件，但有所不同。

```elisp
(package "your-package"
         "0.1.0"
         "Your package description")

(website-url "https://github.com/owner/repo")
(keywords "example" "tool")

(package-file "your-package-file.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "f")
(depends-on "s")
```
