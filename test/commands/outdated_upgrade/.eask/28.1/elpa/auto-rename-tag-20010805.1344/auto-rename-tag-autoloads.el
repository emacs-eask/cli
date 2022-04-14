;;; auto-rename-tag-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-rename-tag" "auto-rename-tag.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from auto-rename-tag.el

(autoload 'auto-rename-tag-mode "auto-rename-tag" "\
Minor mode 'auto-rename-tag' mode.

This is a minor mode.  If called interactively, toggle the
`Auto-Rename-Tag mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `auto-rename-tag-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "auto-rename-tag" '("auto-rename-tag-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; auto-rename-tag-autoloads.el ends here
