---
title: Domain Specific Language
permalink: dsl
---

# Domain Specific Language (DSL)

This document provides a reference on the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language).

## Package metadata

#### 🔎 Function: **pacakge** `name` `version` `description`

Declare a package with the given name, version and description:

```elisp
(package "ert-runner" "0.7.0" "Opinionated Ert testing workflow")
```

All arguments are strings. The version must be a version understood by Emacs'
built-in `version-to-list`.

#### 🔎 Function: **pacakge-file** `file`

Package entry file.

## Package contents

#### 🔎 Function: **files** `&rest patterns`


## Dependencies

#### 🔎 Function: **depends-on** `package-name` `&optional minimum-version`
#### 🔎 Function: **depends-on** `package-name` `&rest recipe`

Specify a dependency of this package.

#### 🔎 Function: **development** `&rest body`

Scope all depends-on expressions in body to development.


#### 🔎 Function: **source** `alias`
#### 🔎 Function: **source** `name` `name`

Add a package archive to install dependencies from.

## Example

`Eask` is the magic file that `eask` will read it as the init file in Emacs.
The syntaxes are very similar to the `Cask` file, but different.

```elisp
(source "gnu")

(package "your-package" "1.0.0" "Your package description")

(package-file "your-package-file.el")
```

## Advanced Usage

Remember, `Eask` is just the regular elisp file, and should be read it from
the Emacs itself!

```elisp
; Regular Eask file content...

(setq byte-compile-error-on-warn t)  ; Singal error if warning occurred
```

`eask` provides some hooks so you can define your own action before/after
each command. The name of the hook follows the rule of
`eask-BEFORE/AFTER-command-NAME-hook`.

For example, to enable compile on warn on `byte-compile` command

```elisp
(add-hook 'eask-before-command-compile-hook 
          (lambda () (setq byte-compile-error-on-warn t)))
```
