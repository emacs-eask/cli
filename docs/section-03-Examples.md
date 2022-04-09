---
title: Examples
permalink: examples
---

# Examples

Here are a few examples of how a `Eask`-file might look like.

## Emacs Configuration

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

## Package Development

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are similar to the `Cask` file, but different.

```elisp
(package "your-package"
         "0.1.0"
         "Your package description")

(package-file "your-package-file.el")

(source "gnu")
```

## Advanced Usage

`Eask` is just the regular Emacs Lisp file and should be read from the
Emacs itself! You can do:

```elisp
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

`eask` provides some hooks so you can make execution before/after each 
command. The name of the hook looks like,

```elisp
eask-{`before`/`after`}-{`command_name`}-hook
```

For example, to enable compile on warn on `compile` command

```elisp
(add-hook 'eask-before-compile-hook 
          (lambda () (setq byte-compile-error-on-warn t)))
```

This is also equivalent to option `--strict`:

```sh
$ eask compile [FILES..] --strict
```

Or hooks run on every command?

* `eask-before-command-hook`
* `eask-after-command-hook`

## Real project examples

These are some projects and configurations using Eask:

* [auto-highlight-symbol](https://github.com/elp-revive/auto-highlight-symbol)
* [emacs-dashboard](https://github.com/emacs-dashboard/emacs-dashboard)
* [flycheck-languagetool](https://github.com/emacs-languagetool/flycheck-languagetool)
* [grammarly](https://github.com/emacs-grammarly/grammarly)
* [popup-el](https://github.com/auto-complete/popup-el)
* [line-reminder](https://github.com/emacs-vs/line-reminder)
* [jcs-emacs](https://github.com/jcs-emacs/jcs-emacs)
* [jcs-elpa](https://github.com/jcs-emacs/jcs-elpa)
