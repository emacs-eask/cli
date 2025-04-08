---
title: Domain Specific Language
weight: 200
---

This document provides a reference on the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language).

{{< toc >}}

# 🚩 Package metadata

## 🔍 **package** (`name` `version` `description`)

Declare a package with the given name, version, and description:

```elisp
(package "ert-runner" "0.7.0" "Opinionated Ert testing workflow")
```

All arguments are strings. The version must be a version understood by Emacs'
built-in `version-to-list`.

## 🔍 **website-url** (`url`)

Declare the package website.

```elisp
(website-url "https://github.com/owner/repo.git")
```

## 🔍 **keywords** (`&rest keywords`)

Declare package keywords.

```elisp
(keywords "tool" "utility" "emacs")
```

## 🔍 **author** (`name` &optional `email`)

Declare package's author.

```elisp
(author "USER NAME" "user.name@example.com")
```

## 🔍 **license** (`name`)

Declare package's author.

```elisp
(license "GPLv3")
```

# 🚩 Package contents

## 🔍 **package-file** (`file` `version` `description`)

Define this package and its runtime dependencies from the package headers
of a file (used only for package development).

```elisp
(package-file "foo.el")
```

## 🔍 **package-descriptor** (`pkg-file`)

Declare all package metadata directly by specifying a package descriptor
contained in file with name given by file.

```elisp
(package-descriptor "foo-pkg.el")
```

## 🔍 **files** (`&rest patterns`)

Specify list of files that are included in this project.

```elisp
(files "foo.el")
(files "*.el" "core/*.el")
```

# 🚩 Tests

## 🔍 **script** (`name` `command` &rest `args`)

Add built-in scripts and their preset life cycle event as well as arbitrary
scripts.

```elisp
(script "test" "echo This is a test!")
```

# 🚩 Dependencies

## 🔍 **source** (`alias`)

## 🔍 **source** (`name` `url`)

Add a package archive to install dependencies from.

```elisp
(source "gnu")
(source "gnu" "https://elpa.gnu.org/packages/")
```

Available aliases:

- `gnu` (https://elpa.gnu.org/packages/)
- `nongnu` (https://elpa.nongnu.org/nongnu/)
- `celpa` (https://celpa.conao3.com/)
- `jcs-elpa` (https://jcs-emacs.github.io/jcs-elpa/packages/)
- `marmalade` (https://marmalade-repo.org/packages/)
- `melpa` (https://melpa.org/packages/)
- `melpa-stable` (https://stable.melpa.org/packages/)
- `org` (https://orgmode.org/elpa/)
- `shmelpa` (https://shmelpa.commandlinesystems.com/packages/)
- `ublt` (https://elpa.ubolonton.org/packages/)

Available `devel` aliases:

- `gnu-devel` (https://elpa.gnu.org/devel/)
- `nongnu-devel` (https://elpa.nongnu.org/nongnu-devel/)

{{< hint ok >}}
💡 Use **--insecure** to make **https** to **http**, but not recommended!
{{< /hint >}}

## 🔍 **source-priority** (`name` `priority`)

Set archive priority.

```elisp
(source-priority "gnu" 5)
```

## 🔍 **depends-on** (`package-name` `&optional minimum-version`)

## 🔍 **depends-on** (`package-name` `&rest recipe`)

Specify a dependency of this package.

Specify dependencies that are listed in **archives**:

```elisp
(depends-on "emacs" "26.1")
(depends-on "dash")
(depends-on "company")
```

Specify dependencies in **file** format:

```elisp
(depends-on "auto-rename-tag" :file "/path/to/auto-rename-tag")

(depends-on "lsp-ui" :file "/path/to/lsp-ui")
```

Specify dependencies in **vc** format:

```elisp
(depends-on "auto-rename-tag" :vc "jcs-elpa/auto-rename-tag")

(depends-on "lsp-ui" :vc "emacs-lsp/lsp-ui")
```

Specify dependencies in **try** format:

```elisp
(depends-on "auto-rename-tag" :try "https://github.com/emacs-vs/auto-rename-tag")

(depends-on "lsp-ui" :vc)  ; Try it, don't install it.
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

{{< hint ok >}}
💡 Install dependencies with command **eask install-deps**!
{{< /hint >}}

## 🔍 **development** (`&rest body`)

Scope all `depends-on` expressions in body to development.

```elisp
(development
 (depends-on "ert-runner")
 (depends-on "elsa"))
```

{{< hint ok >}}
💡 You would need to specify the **--dev** option for development dependencies!
{{< /hint >}}

## 🔍 **load-paths** (`&rest paths`)

Specify paths to add to `load-path`.

```elisp
(load-paths "/lisp/")
```

## 🔍 **exec-paths** (`&rest paths`)

Specify paths to add to `exec-path`.

```elisp
(load-paths "/bin/")
```
