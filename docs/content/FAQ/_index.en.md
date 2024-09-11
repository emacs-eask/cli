---
title: FAQ
weight: 900
---

Here is a list of general frequently asked questions.

{{< toc >}}

# 🔍 About Eask

## ❓ Do you need Node.JS to use Eask?

The answer is **NO**.

Eask builds native executable on every release, see our [release page](https://github.com/emacs-eask/cli/releases)
to download it! However, Node.JS is required if you are going to develop Eask!

## ❓ Who should use this tool?

Here are our suggestions; if you plan to work on an OS-specific package (never
going to other platforms), go for other tools. On the other hand, Eask aims to
provide the best consistency between each OS. Alternatively, if you want to
learn a tool that works everywhere, Eask is one of the best choices.

## ❓ Where can I download the Eask snapshot?

You can download the latest executable (snapshot) in our
[emacs-eask/binaries](https://github.com/emacs-eask/binaries)
repository!

# 🔍 Technology Choice

## ❓ Why Node.JS?

Node has better support on all kinds of terminal applications (compare to just
the shell script)! Like colorful interface, entire npm community, etc. So you
can build cross-platform software with fewer hassles! Especially after Microsoft
had bought the NPM inc, and would likely support their own system well.

Cask does not seem to support Windows (no WSL) after version `0.8.6`. In the
early versions, they have used Python, but due to the Python supports on Windows
are just not as good as Node.JS.

See [issue #140](https://github.com/emacs-eask/cli/issues/140) for more information!

## ❓ Why yargs?

[yargs](https://www.npmjs.com/package/yargs) has a very wide community; and it
has been used in many tools. It's cross-platform! And most importantly, this is
one of the tools that work well on Linux, macOS, and Windows.

There is also the major difference compared with Eask and other alternatives.
[Cask][], [makem.sh][], or [Eldev][] rely more on `batch` and `bash`. We chose a
different route and would like to hand over heavy tasks to a high-level
programming language, **`JavaScript`**. The development simply became easier,
since we don't need to care about different types of shells anymore!

The drawback is the NodeJS runtime, but we can simply pack the entire CLI
program into an executable! That way we would not need to install `Node` and
`npm` before using eask!

# 🔍 Usage

## ❓ Why am I getting the error package target `tar`/`el` not found while installing?

The example error message,

```
http://melpa.org/packages/lsp-mode-20220429.647.tar: Not found
```

The issue is caused by the mismatch from the backup archives. Generally, Eask
will pick up the latest `archive-contents` from sources unless you have been
pinging sources too many times. Then the source could block your IP for few
minutes.

You can either wait for few minutes for the source to remove you from their
black list. Or wait for the backup archives to update to the latest version. The
backup archives repository is [here](https://github.com/emacs-eask/archives).

## ❓ Why am I getting the error package is not installable?

The example error message,

```
Package not installable `helm'; make sure package archives are included
```

You would need to first ask yourself; where does the package came from and what
specific source holds this package information. From the above example message,
`helm` is listed on the `melpa` source. You would have to edit your **Eask**-file
like this:

```elisp
...

(source "melpa")  ; <- add this line

(depends-on "helm")
```

## ❓ Why am I getting git errors with status 2?

If you get this sample error message:

```
Loading package information... done ✓
  - Installing s (20210616.619)... Failed (status 2): git --no-pager remote get-url upstream .
...
```

You may have `bug-reference-prog-mode` enabled. It is not yet compatible with Eask and
should be disabled when running any of Eask’s commands.

See [this issue](https://github.com/emacs-eask/cli/issues/39#issuecomment-1150770740)
for more information.

## ❓ Why am I getting tar exited with status 2?

If you get this sample error message:

```
Created your-package-0.1.0.tar containing:
tar exited with status 2
Error: Process completed with exit code 1.
```

You might get this error while using the BSD tar. The workaround is to use
GNU tar instead.

```
(setq package-build-tar-executable "/path/to/gnu/tar")
```

In Windows, BSD tar is used by default. If you have Git installed, you can use
the tar executable from Git; it uses GNU tar.

Add the following code snippet to your Eask-file:

```
;; Use GNU tar in Windows
(when (memq system-type '(cygwin windows-nt ms-dos))
  (setq package-build-tar-executable "C:/Program Files/Git/usr/bin/tar.exe"))
```


<!-- Links -->

[emacs-eask/archives]: https://github.com/emacs-eask/archives
[Cask]: https://github.com/cask/cask
[makem.sh]: https://github.com/alphapapa/makem.sh
[Eldev]: https://github.com/doublep/eldev
