---
title: 🔧 進階用法
weight: 400
---

{{< toc >}}

`Eask` 只是一個普通的 Emacs Lisp 文件，應該從 Emacs 本身讀取！ 你可以做：

```elisp
; 常規 Eask 文件內容...

(setq byte-compile-error-on-warn t)  ; 出現警告時信號錯誤
```

# 🪝 Hooks

`eask` 提供了一些 hooks，使您能夠在每個命令之前和之後執行代碼。 hook 看起來像這樣：

- `eask-before-COMMAND-hook`
- `eask-after-COMMAND-hook`

例如，在使用命令 `eask compile` 進行字節編譯時將警告視為錯誤：

```elisp
(add-hook 'eask-before-compile-hook
          (lambda () (setq byte-compile-error-on-warn t)))
```

這也等同於選項 `--strict`：

```sh
$ eask compile [FILES..] --strict
```

或者在每個命令上運行的 hooks：

- `eask-before-command-hook`
- `eask-after-command-hook`

```elisp
(add-hook 'eask-before-command-hook
           (lambda ()
             (message "%s" (eask-command))))  ; print the current command
```

對於包含空格的子命令，將與`/`連接：

```sh
$ eask lint checkdoc     # lint/checkdoc
$ eask generate license  # generate/license
```

所以，

```elisp
(add-hook 'eask-before-lint/checkdoc-hook
           (lambda ()
             ;; 在 checkdoc linting 之前做一些事情...
             ))
```

# 📇 加入你自己的指令

您可以透過我們的 command 介面添加自己的命令：

```elisp
(eask-defcommand my-test-command
  "測試指令印出無用的訊息。"
  (message "這是一個測試指令!"))
```
