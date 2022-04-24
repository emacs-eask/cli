---
title: ⚙️ Emacs Configuration
weight: 100
---

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are similar to the `Cask` file, but different.

```elisp
(package "Emacs configuration's name"
         "0.1.0"
         "Your Emacs configuration's description")  ; optional

(package-file "init.el")  ; optional

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
