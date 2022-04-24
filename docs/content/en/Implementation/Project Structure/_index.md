---
title: 📂 Project Structure
weight: 0
---

There are **three** places you need to look into it:

1. `eask` file at the root of the project
2. `cmds` folder with all available commands
3. `lisp` folder with all elisp code
  - `lisp/extern` is the external modules/packages we used

`eask` is the node entry, and the main yargs definition! `cmds` and `lisp`
folders are command files that correspond to each other.

## Execution Order for Eask

Eask is executed this way:

![](flowchart/execution_order.png)

* **Eask environment** builds sandbox and reads Eask file information
* **Emacs configuration** is only being executed when `-g` option is enabled
* **before hooks** are hooks run before command task
* **command execution** is the primary command task
* **after hooks** are hooks run after command task
