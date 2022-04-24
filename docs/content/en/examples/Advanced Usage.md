---
title: Advanced Usage
weight: 300
---

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
