---
title: FAQ
weight: 900
---

## ❓ Why Node.JS?

Node has better support on all kinds of terminal applications (compare to just
the shell script)! Like colorful interface, entire npm community, etc; so you
can build cross-platform software with fewer hassles! Especially after Microsoft
had bought the NPM inc, and would likely support their own system well.

Cask does not seem to support Windows (no WSL) after version `0.8.6`. In the
early versions, they have used Python, but due to the Python supports on Windows
are just not as good as Node.JS.

## ❓ Who should use this tool?

People who like to use Emacs on Windows (no WSL), and would like to keep their
Emacs configuration/packages consistent on every operating system!

## ❓ Why yargs?

[yargs](https://www.npmjs.com/package/yargs) has a vary wide community; and it
has been used in many tools. It's cross-platform! And most importantly, this is
one of the tools that work well on Linux, macOS, and Windows.

The is also the major difference compared with Eask and other alternatives. [Cask](),
[makem.sh](), or [Eldev]() rely more on `batch` and `bash`. We chose different
route and would like to hand over heavy tasks to a high-level programming
language, **`JavaScript`**. The development simply beame easier, since we would
not need to care about different types of shells anymore!

The drawback is the NodeJS runtime, but we can simply pack the entire CLI porgram
into an executable! That way we would not need to install `Node` and `npm` before
using eask!

## ❓ Why am I getting the error package target `tar`/`el` not found while installing?

The example error message,

```
http://melpa.org/packages/lsp-mode-20220429.647.tar: Not found
```

The issue is caused by the mismatch from the backup archives. Generally, Eask
will pick up the latest `archive-contents` from sources unless you have been
pinging sources too many times. Then the source could block your IP for few
minutes.

You can either wait for few minutes so the source has unlock you from their
black list. Or wait for the back up archives to update to the latest version.
The backup archives repository is [here](https://github.com/emacs-eask/archives).

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

If you get this sample error message
```
Loading package information... done ✓
  - Installing s (20210616.619)... Failed (status 2): git --no-pager remote get-url upstream .
...
```

You may have `bug-reference-prog-mode` enabled. It is not yet compatible with Eask and
should be disabled when running any of Eask’s commands.

See [this issue](https://github.com/emacs-eask/eask/issues/39#issuecomment-1150770740) for
more information.
