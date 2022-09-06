---
title: Troubleshooting
weight: 800
---

This document helps you troubleshoot Eask.

{{< toc >}}

## 🚩 Possible Error Variables

Some potential variables can cause faulty Eask, please check

* Emacs is installed and set up with `PATH`
* Eask is installed correctly
* Node version should be `14.x` or above

## ⛔️ Error when running an Eask command

If you run an Eask command and get an error, there are a few things you can try
yourself:

* Make sure that you have the latest Eask version. You can determine the current
Eask version with `eask --version`.
* Upgrade Eask with `eask upgrade-eask` or `npm install -g @emacs-eask/cli@latest`
if you chose to install from `npm`.

{{< hint warning >}}
**⚠ Warning**

If you installed Eask with **npm**, then you should probably upgrade it through
**npm**. Otherwise, you would just have to ensure the **git** is installed.
{{< /hint >}}

* If the error persists, try to reinstall Eask from scratch.

If Eask still does not work, please [report an
issue](https://github.com/emacs-eask/eask/issues/new) to the issue tracker.
Please include Eask output with the [--verbose 4] and [--debug] options enabled,
to give us as much information as possible.
