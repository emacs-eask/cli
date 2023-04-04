---
title: 🔭 Finding Emacs
weight: 150
---

By default, packages are installed for the default Emacs, i.e., the one behind
the `emacs` command. To pick a different Emacs, set the environment variable
`EMACS` to the command name or executable path of the Emacs to use:

```sh
$ EMACS="emacs26.1" eask command
```

Note that installed dependencies are scoped on the version of Emacs. So when
switching between versions you will have to install the dependencies for each:

```sh
$ EMACS="emacs26.3" eask install
```

There are, unfortunately, circumstances under which Emacs itself resets the
`EMACS` variable in a way which conflicts with **eask**, in which case you can
use the environment variable `EASK_EMACS` instead. Specifically, this problem
effects: Emacs-26, for `M-x compile`, `M-x shell` or `M-x term`, for Emacs-27
and Emacs-28 only for `M-x term`.
