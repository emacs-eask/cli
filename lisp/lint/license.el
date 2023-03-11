;;; lint/license.el --- Lint the package's license  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use to check license for the package,
;;
;;   $ eask lint license
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Flags

(advice-add #'eask-allow-error-p :override (lambda (&rest _) t))

;;
;;; Core

(defun eask--string-match-all (regexps)
  "Return t when every REGEXPS match the `buffer-string'."
  (cl-every (lambda (regexp)
              (string-match-p regexp (buffer-string)))
            regexps))

(defun eask--scan-license (file)
  "Scan the license FILE."
  (with-current-buffer (find-file file)
    ;; See https://api.github.com/licenses
    (cond
     ((eask--string-match-all '("GNU AFFERO GENERAL PUBLIC LICENSE"
                                "Version 3"
                                "You should have received a copy of the GNU Affero General Public License"))
      '("agpl-3.0" "GNU Affero General Public License v3.0" "AGPL-3.0"))
     ((eask--string-match-all '("Apache"
                                "http://www.apache.org/licenses/"))
      '("apache-2.0" "Apache License 2.0" "Apache-2.0"))
     ((eask--string-match-all '("BSD 2-Clause"))
      '("bsd-2-clause" "BSD 2-Clause \"Simplified\" License" "BSD-2-Clause"))
     ((eask--string-match-all '("BSD 3-Clause"))
      '("bsd-3-clause" "BSD 3-Clause \"New\" or \"Revised\" License" "BSD-3-Clause"))
     ((eask--string-match-all '("Boost Software License - Version 1.0"))
      '("bsl-1.0" "Boost Software License 1.0" "BSL-1.0"))
     ((eask--string-match-all '("CC0 1.0"))
      '("cc0-1.0" "Creative Commons Zero v1.0 Universal" "CC0-1.0"))
     ((eask--string-match-all '("Eclipse Public License - v 2.0"
                                "Eclipse Foundation"))
      '("epl-2.0" "Eclipse Public License 2.0" "EPL-2.0"))
     ((eask--string-match-all '("GNU General Public License"
                                "Version 2"))
      '("gpl-2.0" "GNU General Public License v2.0" "GPL-2.0"))
     ((eask--string-match-all '("GNU General Public License"
                                "version 3"
                                "You should have received a copy of the GNU General Public License"))
      '("gpl-3.0" "GNU General Public License v3.0" "GPL-3.0"))
     ((eask--string-match-all '("Lesser GPL"
                                "Version 2.1"))
      '("lgpl-2.1" "GNU Lesser General Public License v2.1" "LGPL-2.1"))
     ((eask--string-match-all '("Permission is hereby granted, free of charge, to any person obtaining a copy"))
      '("mit" "MIT License" "MIT"))
     ((eask--string-match-all '("Mozilla Public License Version 2.0"
                                "http://mozilla.org/MPL/2.0/"))
      '("mpl-2.0" "Mozilla Public License 2.0" "MPL-2.0"))
     ((eask--string-match-all '("This is free and unencumbered software released into the public domain"
                                "https://unlicense.org"))
      '("unlicense" "The Unlicense" "Unlicense"))
     (t
      '("unknown" "Unknown license" "unknown")))))

(defun eask--print-scanned-license (scanned)
  "Print all SCANNED license."
  (eask-msg "available via `eask lint license`")
  (eask-msg "")
  (let* ((names (mapcar #'car scanned))
         (offset (eask-seq-str-max names))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (data scanned)
      (eask-msg fmt (nth 0 data) (nth 3 data)))
    (eask-msg "")
    (eask-info "(Total of %s scanned license%s)" (length names)
               (eask--sinr names "" "s"))))
(eask-start
  (let* ((license-names '("copying" "copying.txt"
                          "license" "license.txt" "license.md"
                          "unlicense"))
         (files (eask-expand-file-specs license-names))
         (scanned))
    (cond
     ((null files)
      (eask-info "✗ No license found"))
     (t
      (when (<= 2 (length files))
        (eask-warn "!! Multi-licensing detected !!"))

      (eask-progress-seq "  - Sanning"  files "done! ✓"
                         (lambda (file)
                           (push (append (eask--scan-license file) `(,file)) scanned)))

      (eask-msg "")
      (eask--print-scanned-license (reverse scanned))))))

;;; lint/license.el ends here
