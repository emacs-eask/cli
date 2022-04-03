---
title: Troubleshooting
permalink: troubleshooting
---

# Troubleshooting

This document help you troubleshoot Eask.

## 🚩 Possible Error Variables

Some potential variables can cause faulty Eask,

* Emacs installed, and setup with `PATH`
* Installed Eask correctly
* Node version should be `14.x` or above

## ⛔️ Error when running a Eask command

If you run a Eask command and get an error, there are a few things you can try
yourself:

* Make sure that you have the latest Eask version. You can determine the current
Eask version with `eask --version`.
* Upgrade Eask with `eask upgrade-eask`. Or `npm install -g @emacs-eask/eask@latest`
instead if chose to installed from `npm`.

---

> ⚠️ Warning
>
> If you installed Eask with `npm`, then you should probably upgrade it through
> `npm`. Otherwise you would just have to ensure the `git` is installed.

---

* If the error persists, try to reinstall Eask from scratch.

If Eask still does not work, please [report an issue](https://github.com/emacs-eask/eask/issues/new)
to the issue tracker.
Please include Eask output with the `--verbose 4` and `--debug` options enabled,
to give us as much information as possible.
