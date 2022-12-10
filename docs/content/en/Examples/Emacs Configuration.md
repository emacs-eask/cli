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
💡 You would need to use **-g** or **--global** option to manage your configuration's packages!
{{< /hint >}}
