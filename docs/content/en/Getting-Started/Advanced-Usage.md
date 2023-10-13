---
title: 🔧 Advanced Usage
weight: 400
---

{{< toc >}}

`Eask` is just a regular Emacs Lisp file and should be read from Emacs itself!
You can do:

```elisp
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Signal error if warning occurred
```

# 🪝 Hooks

`eask` provides some hooks which enable you to execute code before and after
each command. The hooks look like so:

- `eask-before-COMMAND-hook`
- `eask-after-COMMAND-hook`

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

Or hooks that run on every command:

- `eask-before-command-hook`
- `eask-after-command-hook`

```elisp
(add-hook 'eask-before-command-hook
           (lambda ()
             (message "%s" (eask-command))))  ; print the current command
```

For subcommands that contain spaces, will concatenate with `/`:

```sh
$ eask lint checkdoc     # lint/checkdoc
$ eask generate license  # generate/license
```

therefore,

```elisp
(add-hook 'eask-before-lint/checkdoc-hook
           (lambda ()
             ;; do stuff before checkdoc linting...
             ))
```

# 📇 Adding your own command

You can add your own command through our command interface:

```elisp
(eask-defcommand my-test-command
  "A test command that prints out useless message."
  (message "This is a test command!"))
```
