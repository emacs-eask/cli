---
title: Domain Specific Language
permalink: dsl
---

# Domain Specific Language (DSL)

This document provides a reference on the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language).

## Package metadata

#### 🔎 **package** (`name` `version` `description`)

Declare a package with the given name, version, and description:

```elisp
(package "ert-runner" "0.7.0" "Opinionated Ert testing workflow")
```

All arguments are strings. The version must be a version understood by Emacs'
built-in `version-to-list`.

#### 🔎 **package-file** (`file` `version` `description`)

Define this package and its runtime dependencies from the package headers 
of a file (used only for package development).

```elisp
(package-file "foo.el")
```

## Package contents

#### 🔎 **files** (`&rest patterns`)

Specify list of files that are included in this project.

```elisp
(files "foo.el")
(files "*.el" "core/*.el")
```

## Dependencies

#### 🔎 **depends-on** (`package-name` `&optional minimum-version`)
#### 🔎 **depends-on** (`package-name` `&rest recipe`)

Specify a dependency of this package.

Specify dependencies that are listed in **archives**:

```elisp
(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "company")
```

Specify dependencies in **recipe** format:

```elisp
(depends-on "auto-rename-tag" 
            :repo "jcs-elpa/auto-rename-tag" 
            :fetcher 'github)

(depends-on "lsp-ui" 
            :repo "emacs-lsp/lsp-ui"
            :fetcher 'github
            :files '(:defaults "lsp-ui-doc.html" "resources"))
```

*💡 Tip: Install dependencies with `eask install-deps`!*

#### 🔎 **development** (`&rest body`)

Scope all `depends-on` expressions in body to development.

```elisp
(development
 (depends-on "ert-runner")
 (depends-on "elsa"))
```

*💡 You would need to specify the `--dev` option for development dependencies!*

#### 🔎 **source** (`alias`)
#### 🔎 **source** (`name` `url`)

Add a package archive to install dependencies from.

```elisp
(source "gnu")
(source "gnu" "https://elpa.gnu.org/packages/")
```

Available aliases:

* `gnu` ([https://elpa.gnu.org/packages/](https://elpa.gnu.org/packages/))
* `nongnu` ([https://elpa.nongnu.org/nongnu/](https://elpa.nongnu.org/nongnu/))
* `celpa` ([https://celpa.conao3.com/](https://celpa.conao3.com/))
* `jcs-elpa` ([https://jcs-emacs.github.io/jcs-elpa/packages/](https://jcs-emacs.github.io/jcs-elpa/packages/))
* `marmalade` ([https://marmalade-repo.org/packages/](https://marmalade-repo.org/packages/))
* `melpa` ([https://melpa.org/packages/](https://melpa.org/packages/))
* `melpa-stable` ([https://stable.melpa.org/packages/](https://stable.melpa.org/packages/))
* `org` ([https://orgmode.org/elpa/](https://orgmode.org/elpa/))
* `shmelpa` ([https://shmelpa.commandlinesystems.com/packages/](https://shmelpa.commandlinesystems.com/packages/))

*💡 You can use `--insecure` to make `https` to `http`, but not recommended*

#### 🔎 **source-priority** (`name` `priority`)

Set archive priority.

```elisp
(source-priority "gnu" 5)
```

#### 🔎 **load-paths** (`&rest paths`)

Specify paths to add to `load-path`

```elisp
(load-paths "/lisp/")
```

#### 🔎 **exec-paths** (`&rest paths`)

Specify paths to add to `exec-path`

```elisp
(load-paths "/bin/")
```
