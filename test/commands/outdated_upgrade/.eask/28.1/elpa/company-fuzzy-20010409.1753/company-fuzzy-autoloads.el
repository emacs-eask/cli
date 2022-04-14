;;; company-fuzzy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-fuzzy" "company-fuzzy.el" (0 0 0 0))
;;; Generated autoloads from company-fuzzy.el

(autoload 'company-fuzzy-mode "company-fuzzy" "\
Minor mode 'company-fuzzy-mode'.

This is a minor mode.  If called interactively, toggle the
`Company-Fuzzy mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `company-fuzzy-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-company-fuzzy-mode 'globalized-minor-mode t)

(defvar global-company-fuzzy-mode nil "\
Non-nil if Global Company-Fuzzy mode is enabled.
See the `global-company-fuzzy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-company-fuzzy-mode'.")

(custom-autoload 'global-company-fuzzy-mode "company-fuzzy" nil)

(autoload 'global-company-fuzzy-mode "company-fuzzy" "\
Toggle Company-Fuzzy mode in all buffers.
With prefix ARG, enable Global Company-Fuzzy mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Company-Fuzzy mode is enabled in all buffers where
`company-fuzzy-turn-on-company-fuzzy-mode' would do it.

See `company-fuzzy-mode' for more information on Company-Fuzzy mode.

\(fn &optional ARG)" t nil)

(autoload 'company-fuzzy-backend-add "company-fuzzy" "\
Safe way to add BACKEND.

\(fn BACKEND)" nil nil)

(autoload 'company-fuzzy-backend-remove "company-fuzzy" "\
Safe way to remove BACKEND.

\(fn BACKEND)" nil nil)

(register-definition-prefixes "company-fuzzy" '("company-fuzzy-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8-emacs-unix
;; End:
;;; company-fuzzy-autoloads.el ends here
