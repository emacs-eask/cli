---
title: 🔧 Advanced Usage
weight: 400
---

`Eask` is just a regular Emacs Lisp file and should be read from Emacs itself!
You can do:

```elisp
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Signal error if warning occurred
```

`eask` provides some hooks which enable you to execute code before and after
each command. The hooks look like so:

* `eask-before-COMMAND-hook`
* `eask-after-COMMAND-hook`

For example, to consider warnings as errors when byte-compiling with the command
`eask compile`:

```elisp
(add-hook 'eask-before-compile-hook
          (lambda () (setq byte-compile-error-on-warn t)))
```

This is also equivalent to option `--strict`:

```sh
$ eask compile [FILES..] --strict
```

Or hooks run on every command:

* `eask-before-command-hook`
* `eask-after-command-hook`
