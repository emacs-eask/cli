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
to download it! However, [Node.JS][] is required if you are going to develop Eask!

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

[Node][Node.js] provides better support for various terminal applications compared to shell
scripts. It offers features like a rich, colorful interface and access to the
vast npm ecosystem, making cross-platform development more convenient. This is
especially true since Microsoft acquired [npm][] Inc., likely ensuring strong
support for their own systems.

[Cask][] seems to have dropped support for Windows (while still supporting [WSL][])
after version `0.8.6`. Earlier versions were built on [Python][], but [Python][]’s support
on Windows has traditionally been less reliable than [Node.js][].

See [issue #140](https://github.com/emacs-eask/cli/issues/140) for more information!

## ❓ Why JavaScript?

There are many languages I could use to build around Eask, so why did I choose [JavaScript][]?

I have three reasons:

- [JavaScript][] is easy to learn.
- It offers excellent cross-platform compatibility, thanks to the [Node.js][] runtime.
- I just happen to know [JavaScript][], and I’m comfortable with it.

I also considered [Rust][] and [Common Lisp][]. However, [Rust][] was still relatively
new when I started this project, and [Common Lisp][], while powerful, has a steeper
learning curve and is often seen as somewhat outdated. So, I went with [JavaScript][].

## ❓ Why yargs?

[yargs][] has a large and active community and is widely used in various tools.
It’s fully cross-platform and, most importantly, works seamlessly on Linux, macOS,
and Windows.

One key difference between Eask and other alternatives is how they handle scripting.
Tools like [Cask][], [makem.sh][], and [Eldev][] rely heavily on batch and bash scripts.
Instead, we took a different approach by leveraging a high-level programming
language-[JavaScript][]. This made development significantly easier, as we no longer
need to worry about shell compatibility.

The main drawback is the [Node.js][] runtime requirement, but we can mitigate this
by packaging the entire CLI program into an executable. This way, users won’t
need to install [Node][Node.js] or [npm][] beforehand to use Eask!

# 🔍 Usage

## ❓ How to configure Eask?

`Eask`-file is an Elisp file, similar to `.emacs` or `init.el`.
Just as Emacs allows you to customize aspects you don’t like,
Eask follows the same principle, letting you configure anything you dislike about Eask.

Another way to configure your workspace is similar to configuring Emacs itself:

- Use `.eask/VERSION_NO/early-init.el` (only after Emacs `27.1` onward)
- Use `.eask/VERSION_NO/init.el`

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

First, determine the origin of the package and the specific source that
provides its information. In the example above, `helm` is listed under the
`melpa` source. To properly include it, update your **Eask**-file as follows:

```elisp
...

(source "melpa")  ; <- add this line

(depends-on "helm")
```

## ❓ Why am I seeing the error: "Package `emacs-XX.X' is unavailable"?

The example error message,

```
Loading package information... done v
Installing 1 specified package...

  - [1/1] Installing markdown-mode (20250226.231)... Package `emacs-28.1' is unavailable
Wrong type argument: package-desc, nil
```

This error occurs when Emacs attempts to install a package that requires a newer
version of Emacs. In some cases, the requirement does not come directly from
the package itself but from one of its dependencies.

You can either refrain from using this package or upgrade Emacs to the
required version.

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

[Node.js]: https://nodejs.org/
[npm]: https://www.npmjs.com/
[yargs]: https://www.npmjs.com/package/yargs

[WSL]: https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux
[JavaScript]: https://simple.wikipedia.org/wiki/JavaScript
[Python]: https://www.python.org/
[Rust]: https://www.rust-lang.org/
[Common Lisp]: https://lisp-lang.org/
