---
title: Development API
weight: 700
---

This document provides a reference to the public Eask API, which you may use in
your projects and extensions to Eask.

{{< toc >}}

# 🚩 Entry Point

## 🔍 Snippet: _prepare.el

Load `lisp/_prepare.el` to start using other Eask API.

```elisp
(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))
```

Each Elisp scripts should have this snippet at the very top of the file.

## 🔍 Macro: eask-start (&rest `body`)

Command entry point. Each command file should contain this macro somewhere in the file.

```elisp
(eask-start
  ;; TODO: design your command here!
  )
```

# 🚩 Environment

## 🔍 Variable: eask-has-colors

Return non-nil if the terminal supports colors.

```elisp
(when eask-has-colors ...
```

## 🔍 Variable: eask-homedir

Eask's home directory path.

```elisp
(message "%s" eask-homedir)
```

## 🔍 Variable: eask-invocation

Eask's invocation program path.

```elisp
(message "%s" eask-invocation)
```

It could be the `node` executable or the `eask` executable itself.

## 🔍 Variable: eask-is-pkg

Return non-nil if Eask is packaged.

```elisp
(when eask-is-pkg ...
```

## 🔍 Variable: eask-rest

Eask's arguments after command separator `--'; return a list.

```sh
eask <command> -- args0 args1
```

Output:

```elisp
(message "%s" eask-rest)  ; '(args0 args1)
```

## 🔍 Function: eask-rest ()

Eask's arguments after command separator `--'; return a string.

```sh
eask <command> -- args0 args1
```

Output:

```elisp
(message "%s" (eask-rest))  ; "args0 args1"
```

# 🚩 Core

## 🔍 Variable: eask-lisp-root

Points to `lisp` directory from the project root.

```elisp
(message "%s" eask-lisp-root)  ; path/to/eask/cli/lisp/
```

## 🔍 Function: eask-working-directory ()

Return the working directory of the program going to be executed.

```elisp
(message "%s" (eask-working-directory))  ; path/to/current/work/space/
```

## 🔍 Function: eask-command ()

Return the current command in string. Suppose the command is:

```sh
eask init
```

then,

```elisp
(message "%s" (eask-command))  ; init
```

## 🔍 Function: eask-command-check (`version`)

Report error if the current command requires minimum `version`.

```elisp
(eask-command-check "27.1")  ; The command requires 27.1 and above!
```

## 🔍 Function: eask-command-p (`commands`)

Return t if COMMANDS is the current command.

## 🔍 Function: eask-special-p ()

Return `t` if the command that can be run without Eask-file existence.

This allows some commands can still be executed without defining the user
directory. This can be handy when you want to do normal operations without
touching the user directory.

## 🔍 Function: eask-execution-p ()

Return `t` if the command is the execution command.

This is added because we don't want to pollute `error` and `warn` functions.

## 🔍 Function: eask-checker-p ()

Return `t` if running Eask as the checker.

Without this flag, the process will be terminated once the error is occurred.
This flag allows you to run through operations without reporting errors.

## 🔍 Function: eask-script (`script`)

Return full script filename.

```elisp
(eask-script "extern/pacakge")  ; {project-root}/lisp/extern/package.el
```

## 🔍 Function: eask-load (`script`)

Load another eask script.

```elisp
(eask-load "extern/ansi")  ; load {project-root}/lisp/extern/ansi.el file
```

## 🔍 Function: eask-call (`script`)

Call another eask script.

```elisp
(eask-call "clean/elc")  ; call command `eask clean-elc`
```

{{< hint info >}}
💡 We don't often call this since we don't wish to execute another command directly!
{{< /hint >}}

## 🔍 Function: eask-import (`url`)

Load and evaluate the script from the url.

```elisp
(eask-import "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/yes-no.el")

;; The script will be available after the call!
```

## 🔍 Macro: eask-defvc< (`version` &rest `body`)

Define the scope if the Emacs version is below a specific version.

`VERSION` is an integer and will be compared with `emacs-major-version`.

```elisp
(eask-defvc< 28
  ;; This is missing before Emacs 28; define it
  (defvar package-native-compile nil))
```

{{< hint info >}}
💡 This is used for Emacs compatibility!
{{< /hint >}}

## 🔍 Macro: eask--silent (&rest `body`)

Mute all messages from standard output inside the scope.

```elisp
(eask--unsilent (message "You can't hear me! :("))
```

## 🔍 Macro: eask--unsilent (&rest `body`)

Unmute all messages from standard output inside the scope.

```elisp
(eask--unsilent (message "You can hear me! :)"))
```

## 🔍 Function: eask-dependencies ()

Return a list of dependencies.

Elements should either be `(NAME . VERSION)` or `(NAME . RECIPE-FORMAT)`.

## 🔍 Function: eask-pkg-init (&optional `force`)

Initialize packages for use.

```elisp
(eask-start
  (eask-pkg-init)
  ;; Now you can use packages installed in `package-user-dir'
  )
```

{{< hint info >}}
💡 This is usually called after **eask-start**!
{{< /hint >}}

## 🔍 Macro: eask-with-archives (`archives` &rest `body`)

Scope that temporary makes archives available.

The argument `ARCHIVES` can either be a string or a list of strings.

```elisp
(eask-with-archives "melpa"
  (eask-package-install 'dash))  ; install packages that are only defined in MELPA
```

{{< hint info >}}
💡 This is handy when you need certain packages from certain archives.
{{< /hint >}}

## 🔍 Function: eask-archive-install-packages (`archives` `names`)

Install packages with archives setup.

The arugment `names` can be a symbol or list of symbols.

```elisp
(eask-archive-install-packages '("gnu" "melpa")
                               'el2org)  ; accept list
```

{{< hint info >}}
💡 This only installs packages if they are missing.
{{< /hint >}}

## 🔍 Function: eask-package-desc (`name` &optional `current`)

Build package descriptor for a package.

`CURRENT` means installed packages; otherwise it will return any available
packages from selected package archives.

## 🔍 Function: eask-argv (`index`)

Return a command-line argument by index.

## 🔍 Function: eask-args ()

Return a list that is extracted from command-line arguments.

```sh
eask info --verbose 4 foo bar
```

It will ignore `--verbose` and `4`, and only returns `foo`, and `bar`.

## 🔍 Variable: eask-file

Path to currently loaded Eask-file.

## 🔍 Variable: eask-file-root

Directory to currently loaded Eask-file.

## 🔍 Function: eask--match-file (`name`)

Check to see if NAME is our target Eask-file, then return it.

The following output is with Emacs 28.1:

```elisp
(eask--match-file "Eask")         ; t
(eask--match-file "Eask.28")      ; t
(eask--match-file "Eask.28.1")    ; t
(eask--match-file "Eask.29")      ; nil

(eask--match-file "Easkfile")     ; t
(eask--match-file "Easkfile.28")  ; t
(eask--match-file "Easkfile.29")  ; nil
```

## 🔍 Function: eask--all-files (&optional `dir`)

Return a list of Eask files from DIR.

Consider the following directory tree:

```text
. root
├── Eask
├── Eask.28
└── Eask.29
```

The following output is with Emacs 28.1:

```elisp
(eask--all-files "/root/")  ; '(Eask Eask.28)
```

## 🔍 Function: eask--find-files (`start-path`)

Find the Eask-file from START-PATH.

Consider the following directory tree:

```text
.project
├─ src
│ └── config.el
├── Eask
├── Eask.28
└── Eask.29
```

The following output is with Emacs 28.1:

```elisp
(eask--find-files "/project/src/config.el")  ; '(/project/Eask /project/Eask.28)
```

## 🔍 Function: eask-file-try-load (`start-path`)

Try load the Eask-file in START-PATH.

```elisp
(eask--find-files "/project/src/")  ; t
```

## 🔍 Function: eask-network-insecure-p ()

Return `t` if the current Emacs session allows insecure network connections.

# 🚩 Flags

## 🔍 Function: eask-global-p ()

Return `t` if the `global` option is enabled.

```elisp
(when (eask-global-p)
  user-emacs-directory)   ; ~/.eask/
```

## 🔍 Function: eask-config-p ()

Return `t` if the `config` option is enabled.

```elisp
(when (eask-config-p)
  user-emacs-directory)   ; ~/.emacs.d
```

{{< hint info >}}
💡 If both options `--config` and `--global` are on, the global space is chosen
over the config space.
{{< /hint >}}

## 🔍 Function: eask-local-p ()

This uses the current workspace, and this is the default.

```elisp
(when (eask-local-p)
  user-emacs-directory)   ; ./.eask/{emacs-version}/
```

{{< hint info >}}
💡 This function returns `t` only when `(eask-global-p)` and `(eask-config-p)`
are false!
{{< /hint >}}

## 🔍 Function: eask-all-p ()

Return `t` if the `all` option is enabled.

```elisp
(when (eask-all-p)
  ;; Run all tests
  ...)
```

## 🔍 Function: eask-quick-p ()

Return `t` if the `quick` option is enabled.

```elisp
(unless (eask-quick-p)
  (load user-init-file)
  ...)
```

## 🔍 Function: eask-force-p ()

Return `t` if the `force` option is enabled.

```elisp
(package-delete .. (eask-force-p))
```

## 🔍 Function: eask-dev-p ()

Return `t` if the `development` option is enabled.

```elisp
(when (eask-dev-p)
  (package-install 'ert-runner))  ; install development dependency
```

## 🔍 Function: eask-debug-p ()

Return `t` if the `debug` option is enabled.

```elisp
(when (eask-debug-p)
  (error "Executing in debug mode..."))
```

## 🔍 Function: eask-strict-p ()

Return `t` if the `strict` option is enabled.

```elisp
(setq byte-compile-error-on-warn (eask-strict-p))
```

## 🔍 Function: eask-timestamps-p ()

Return `t`/`nil` if the `timestamps` option is enabled/disabled.

These flags can't co-exist in the same command.

```elisp
(when (eask-timestamps-p)
  (message "Print log with timestamps!"))
```

## 🔍 Function: eask-log-level-p ()

Return `t`/`nil` if the `log-level` option is enabled/disabled.

These flags can't co-exist in the same command.

```elisp
(when (eask-log-level-p)
  (message "Print log with level prefix!"))
```

## 🔍 Function: eask-log-file-p ()

Return `t`/`nil` if the `log-file` option is enabled/disabled.

These flags can't co-exist in the same command.

```elisp
(when (eask-log-file-p)
  (message "Let's create a log file!"))
```

## 🔍 Function: eask-no-color-p ()

Return `t` if the `color` option is enabled.

```elisp
(unless (eask-no-color-p)
  (message "This string has no ansi code!"))
```

## 🔍 Function: eask-allow-error-p ()

Return `t` if the `allow-error` option is enabled.

```elisp
(unless (eask-allow-error-p)
  (error "Stop here."))
```

## 🔍 Function: eask-insecure-p ()

Return `t` if the `insecure` option is enabled.

```elisp
(when (eask-insecure-p)
  ;; Do some dangerous tasks?
  )
```

## 🔍 Function: eask-proxy ()
## 🔍 Function: eask-http-proxy ()
## 🔍 Function: eask-https-proxy ()
## 🔍 Function: eask-no-proxy ()

Return a **string** represents `hostname` + `port number`.

```sh
eask [command] --proxy "localhost:1000"
eask [command] --http-proxy "localhost:2000"
eask [command] --https-proxy "localhost:3000"
eask [command] --no-proxy "localhost:4000"
```

## 🔍 Function: eask-destination ()

Return a **string** represents the destination (output path).

```elisp
(write-file (or (eask-destination) "./dist"))  ; write file to destination
```

## 🔍 Function: eask-depth ()

Return an **integer** represents the depth of the current print level.

```elisp
(setq print-level (eask-depth))
```

## 🔍 Function: eask-verbose ()

Return an **integer** represents the verbosity level.

```elisp
(when (= (eask-verbose) 4)
  (setq byte-compile-verbose t))
```

# 🚩 `Eask`-file

These functions are the actual implementation of `Eask`-file DSL; and
have the word `eask-` as the function prefix.

See [DSL](https://emacs-eask.github.io/DSL/) section for more information.

## 🔍 Variable: eask-package

It holds package's `NAME`, `VERSION`, and `DESCRIPTION` in a plist.

```elisp
(plist-get eask-package :name)  ; return package name
```

Three functions that are extended from this variable:

- `(eask-package-name)`
- `(eask-package-version)`
- `(eask-package-description)`

## 🔍 Variable: eask-package-file

Points to package main file.

## 🔍 Variable: eask-package-desc

Package descriptor from the package main file.

```elisp
(package-desc-p eask-package-desc)  ; return t
```

{{< hint warning >}}
⚠ This can be **nil** if the package-descriptor cannot be constructed correctly!
{{< /hint >}}

## 🔍 Variable: eask-files

Holds a list of files pattern in wildcard specification.

## 🔍 Variable: eask-scripts

Holds a list of available scripts that can be executed by user using the
`eask run-script` command.

## 🔍 Variable: eask-depends-on-emacs

Holds information about Emacs minimum version.

```elisp
(depends-on "emacs" "26.1")
```

Function will return Emacs version in string.

- `(eask-depends-emacs-version)` - return `"26.1"`

## 🔍 Variable: eask-depends-on

Holds a list of dependencies.

## 🔍 Variable: eask-depends-on-dev

Holds a list of dependencies that are development used.

## 🔍 Function: eask-f-package (`name` `version` `description`)

Alias of `package`.

## 🔍 Function: eask-f-website-url (`url`)

Alias of `website-url`.

## 🔍 Function: eask-f-keywords (&rest `keywords`)

Alias of `keywords`.

## 🔍 Function: eask-f-author (`name` &optional `email`)

Alias of `author`.

## 🔍 Function: eask-f-license (`name`)

Alias of `license`.

## 🔍 Function: eask-f-package-file (`file`)

Alias of `package-file`.

## 🔍 Function: eask-f-files (`pkg` &rest `args`)

Alias of `files`.

## 🔍 Function: eask-f-script (`name` `command` &rest `args`)

Alias of `script`.

## 🔍 Function: eask-f-source (`name` &optional `location`)

Alias of `source`.

## 🔍 Function: eask-f-source-priority (`name` &optional `priority`)

Alias of `source-priority`.

## 🔍 Function: eask-f-depends-on (`pkg` &rest `args`)

Alias of `depends-on`.

## 🔍 Function: eask-f-development (&rest `dependencies`)

Alias of `development`.

## 🔍 Function: eask-f-exec-paths (&rest `dirs`)

Alias of `exec-paths`.

## 🔍 Function: eask-f-load-paths (&rest `dirs`)

Alias of `load-paths`.

# 🚩 Logging

Logger utility with timestamps and log level.

The log level value is defined in function `eask--verb2lvl`.

| Level   | Description                                                                                               | Value |
|:--------|:----------------------------------------------------------------------------------------------------------|:------|
| `debug` | Designates fine-grained informational events that are most useful to debug an application.                | 4     |
| `log`   | Designates normal messages.                                                                               | 3     |
| `info`  | Designates informational messages that highlight the progress of the application at coarse-grained level. | 2     |
| `warn`  | Designates potentially harmful situations.                                                                | 1     |
| `error` | Designates error events that might still allow the application to continue running.                       | 0     |

The default level is `log`.

## 🔍 Variable: eask-verbosity

The verbosity level is represented as an integer.

```elisp
(setq eask-verbosity 4)  ; you could set from 0 to 4
```

## 🔍 Variable: eask-timestamps

Log messages with timestamps.

```elisp
(setq eask-timestamps t)
```

Output:

```text
2022-04-14 13:44:46 This is a message with timestamps
```

## 🔍 Variable: eask-log-level

Log messages with level. (default: `nil`)

```elisp
(setq eask-log-level t)
```

Output:

```text
[DEBUG] This is a DEBUG message with log level
```

## 🔍 Variable: eask-log-file

Weather to generate log files. (default: `nil`)

```elisp
(setq eask-log-level t)
```

Use command `cat` to see the log,

```text
cat /.log/messages.log
```

## 🔍 Variable: eask-level-color

Define each log level color.

```elisp
(setq eask-level-color
      '((debug . ansi-blue)
        (log   . ansi-white)
        (info  . ansi-cyan)
        (warn  . ansi-yellow)
        (error . ansi-red)))
```

## 🔍 Function: eask-reach-verbosity-p (`symbol`)

Make execution when it reaches the verbosity level.

```elisp
(when (eask-reach-verbosity-p 'debug)
  ;; TODO: execution here..
  )
```

## 🔍 Macro: eask-with-verbosity (`symbol` &rest `body`)

Define verbosity scope.

```elisp
(eask-with-verbosity 'debug
  ;; TODO: execution here..
  )
```

Everything in the scope of this macro will be muted unless the verbosity
reaches. It will only be printed when you have specified `--verbose 4`
global option.

## 🔍 Macro: eask-with-verbosity-override (`symbol` &rest `body`)

Define override verbosity scope.

```elisp
(eask-with-verbosity 'debug
  (eask-with-verbosity-override 'log
    ;; TODO: execution here..
    )
  (eask-with-verbosity-override 'info
    ;; TODO: execution here..
    ))
```

Like macro `eask-with-verbosity`; but force display messages if it wasn't able
to display.

## 🔍 Function: eask-debug (`msg` &rest `args`)

```elisp
(eask-debug "This is DEBUG message")
```

```text
2022-04-14 17:31:54 [DEBUG] This is DEBUG message
```

## 🔍 Function: eask-log (`msg` &rest `args`)

```elisp
(eask-log "This is LOG message")
```

```text
2022-04-14 17:31:54 [LOG] This is LOG message
```

## 🔍 Function: eask-info (`msg` &rest `args`)

```elisp
(eask-info "This is INFO message")
```

```text
2022-04-14 17:31:54 [INFO] This is INFO message
```

## 🔍 Function: eask-warn (`msg` &rest `args`)

```elisp
(eask-warn "This is WARNING message")
```

```text
2022-04-14 17:31:54 [WARNING] This is WARNING message
```

## 🔍 Function: eask-error (`msg` &rest `args`)

```elisp
(eask-error "This is ERROR message")
```

```text
2022-04-14 17:31:54 [ERROR] This is ERROR message
```

## 🔍 Function: eask-print (`msg` &rest `args`)

Standard output printing without newline.

```elisp
(eask-println "Print to stdout!")
```

## 🔍 Function: eask-println (`msg` &rest `args`)

Like the function `eask-print` but contains the newline at the end.

```elisp
(eask-println "Print to stdout! (with newline)")
```

## 🔍 Function: eask-msg (`msg` &rest `args`)

Like the `message` function but will replace unicode with color.

```elisp
(eask-msg "Print this message with newline!")
```

## 🔍 Function: eask-write (`msg` &rest `args`)

Like the `eask-msg` function but without the newline at the end.

```elisp
(eask-write "Print this message without newline...")
```

## 🔍 Function: eask-report (&rest `args`)

Report error/warning depends on strict flag.

```elisp
(eask-report "This can be warning or error")
```

See option [--strict](https://emacs-eask.github.io/Getting-Started/Commands-and-options/#---strict).

# 🚩 Exit Code

## 🔍 Variable: eask--exit-code

Exit code specification.

## 🔍 Function: eask-exit-code (&optional `key`)

Return the exit code by the key symbol in the variable `eask--exit-code`.

```elisp
(eask-exit-code 'misuse)  ; by symbol
```

## 🔍 Function: eask--exit (&optional `exit-code` &rest `_`)

Send exit code.

This will kill Emacs process.

```elisp
(eask--exit 2)        ; by number
(eask--exit 'misuse)  ; by symbol
```

# 🚩 Error Handling

## 🔍 Variable: eask--ignore-error-p

Non-nil to prevent Emacs from being killed.

```elisp
(let ((eask--ignore-error-p t))
  (error "Emacs can't die! :P"))
```

## 🔍 Variable: eask-inhibit-error-message

Non-nil to stop error/warning message.

```elisp
(let ((eask-inhibit-error-message t))
  (error "This won't display at all!"))
```

## 🔍 Macro: eask-ignore-errors (&rest `body`)

Prevent Emacs from being killed.

```elisp
(eask-ignore-errors
  (error "Emacs can't die! :P"))
```

## 🔍 Macro: eask--silent-error (&rest `body`)

Inhibit display error/warning messages.

```elisp
(eask--silent-error
  (error "This won't display at all!"))
```

## 🔍 Macro: eask-ignore-errors-silent (&rest `body`)

Prevent Emacs from being killed and inhibit display error/warning messages.

```elisp
(eask-ignore-errors-silent
  (error "Nothing happens!"))
```

# 🚩 File

## 🔍 Function: eask-package-files ()

Return a list of package files.

## 🔍 Function: eask-package-el-files ()

Return a list of package files with `.el` extension.

## 🔍 Function: eask-package-elc-files ()

Return a list of package files with `.elc` extension.

## 🔍 Function: eask-package-multi-p ()

Return `nil` if single file package.

## 🔍 Function: eask-package-single-p ()

Return `t` if single file package.

## 🔍 Function: eask-unpacked-size ()

Return size of the current package.

{{< hint warning >}}
⚠️ This returns a string not bytes.
{{< /hint >}}

# 🚩 Progress

## 🔍 Macro: eask-with-progress (`msg-start` `body` `msg-end`)

Create execution with the responsive message output.

```elisp
(eask-with-progress
  "Downloading files... "
  (eask-with-verbosity 'debug  ; Often used with `eask-with-verbosity'
    ;; Execute some operations..
    )
  "done ✓")
```

Expect output:

```text
Downloading files... done ✓
```

## 🔍 Function: eask-print-log-buffer (&optional `buffer-or-name`)

Print buffer and highlight the `errors` and `warnings`.

```elisp
(eask-print-log-buffer "*Package-Lint*")
```

{{< hint info >}}
💡 This is handy for linters that create a buffer to display **errors** and **warnings**.
{{< /hint >}}

# 🚩 Help

## 🔍 Function: eask-help (`command` &optional `print-or-exit-code`)

Print help manual located under `lisp/help/` directory.

```elisp
(eask-help "core/search")  ; Exit code 1
```

Alternatively, you can specify the exit code using the second parameter,
`print-or-exit-code`, which defaults to 1.

```elisp
(eask-help "core/search" 4)  ; Exit code 4
```

To prevent the program from exiting with a code, pass any non-nil value.

```elisp
(eask-help "core/search" t)  ; Skip exiting
```

{{< hint info >}}
💡 This is used when a command fails, and would like to give users some tips!
{{< /hint >}}

# 🚩 Utilities

## 🔍 Function: eask-guess-package-name ()

Return the possible package name.

## 🔍 Function: eask-guess-entry-point ()

Return the possible package's entry point.
