---
title: 📦 Package Development
weight: 200
---

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are similar to the `Cask` file, but different.

```elisp
(package "your-package"
         "0.1.0"
         "Your package description")

(package-file "your-package-file.el")

(source "gnu")

(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "f")
(depends-on "s")
```
