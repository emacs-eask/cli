---
title: section-3-usage.md
permalink: usage
---

# 🔨 Usage

## 📝 About Eask file

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are very similar to the `Cask` file, but different.

```el
(source "gnu")

(package "your-package" "1.0.0" "Your package description")

(package-file "your-package-file.el")
```

Remember, `Eask` is just the regular elisp file, and should be read it from
the Emacs itself!

```el
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

`eask` provides some hooks so you can define your own action before/after
each command. The name of the hook follows the rule of
`eask-{before/after}-command-{%name%}-hook`.

For example, to enable compile on warn on `byte-compile` command

```el
(add-hook 'eask-before-command-compile-hook 
          (lambda () (setq byte-compile-error-on-warn t)))
```
