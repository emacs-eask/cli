---
title: 📜 Elisp Script
weight: 800
---

Elisp scripts are located under **lisp** folder and will wait to get called
by the CLI. All Elisp scripts are written in Emacs Lisp and should have a
similar structure below:

```elisp
(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)
      
(eask-start
  (message "PWD is %s" default-directory))
```

See [Development API](https://emacs-eask.github.io/eask/api) section for
more information!
