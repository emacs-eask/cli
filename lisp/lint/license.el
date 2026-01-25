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
;;; Externals

(require 'whitespace nil t)

;;
;;; Core

(defun eask-lint-license--s-match-all (contents)
  "Return t when every CONTENTS match the `buffer-string'."
  (cl-every (lambda (content)
              (string-match-p (eask-s-replace "\n" " " content) (buffer-string)))
            contents))

(defun eask-lint-license--scan (file)
  "Scan the license FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((whitespace-style '(trailing)))
      (whitespace-cleanup))
    (let ((content (eask-s-replace "\n" " " (buffer-string))))
      (erase-buffer)
      (insert content))
    ;; See https://api.github.com/licenses
    (cond
     ((eask-lint-license--s-match-all
       '("Licensed under the Academic Free License version 3.0"))
      '("afl-3.0" "Academic Free License v3.0" "AFL-3.0"))
     ((eask-lint-license--s-match-all
       '("You should have received a copy of the GNU Affero General Public License"))
      '("agpl-3.0" "GNU Affero General Public License v3.0" "AGPL-3.0"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim copies"
         "but changing it is not allowed."
         "artistic"))
      '("artistic-2.0" "Artistic License 2.0" "Artistic-2.0"))
     ((eask-lint-license--s-match-all
       '("Licensed under the Apache License, Version 2.0"))
      '("apache-2.0" "Apache License 2.0" "Apache-2.0"))
     ((eask-lint-license--s-match-all
       '("Redistribution and use in source and binary forms"
         "Redistributions in binary form must reproduce"))
      '("bsd-2-clause" "BSD 2-Clause \"Simplified\" License" "BSD-2-Clause"))
     ((eask-lint-license--s-match-all
       '("Redistribution and use in source and binary forms"
         "Neither the name of the copyright holder nor"))
      '("bsd-3-clause" "BSD 3-Clause \"New\" or \"Revised\" License" "BSD-3-Clause"))
     ((eask-lint-license--s-match-all
       '("Permission is hereby granted, free of charge, to any person or organization"))
      '("bsl-1.0" "Boost Software License 1.0" "BSL-1.0"))
     ((eask-lint-license--s-match-all
       '("The laws of most jurisdictions throughout the world automatically confer exclusive Copyright"))
      '("cc0-1.0" "Creative Commons Zero v1.0 Universal" "CC0-1.0"))
     ((eask-lint-license--s-match-all
       '("Eclipse Public License - v 2.0"
         "Eclipse Foundation"))
      '("epl-2.0" "Eclipse Public License 2.0" "EPL-2.0"))
     ((eask-lint-license--s-match-all
       '("Copying and distribution of this file, with or without"))
      '("fsfap" "FSF All Permissive License" "FSFAP"))
     ((eask-lint-license--s-match-all
       '("is free software." "you can redistribute it"
         "version 2 of the"))
      '("gpl-2.0" "GNU General Public License v2.0" "GPL-2.0"))
     ((eask-lint-license--s-match-all
       '("is free software." "you can redistribute it"
         "version 3 of the"))
      '("gpl-3.0" "GNU General Public License v3.0" "GPL-3.0"))
     ((eask-lint-license--s-match-all
       '("Permission to use, copy, modify, and/or distribute this"))
      '("isc" " Internet Systems Consortium" "ISC"))
     ((eask-lint-license--s-match-all
       '("Lesser"
         "GNU"
         "version 2"))
      '("lgpl-2.1" "GNU Lesser General Public License v2.1" "LGPL-2.1"))
     ((eask-lint-license--s-match-all
       '("This license governs use of the accompanying software."
         "If you use the software, you accept this license."
         "If you do not accept the license, do not use the software."))
      '("ms-pl" "Microsoft Public License" "MS-PL"))
     ((eask-lint-license--s-match-all
       '("Permission is hereby granted, free of charge, to any person"))
      '("mit" "MIT License" "MIT"))
     ((eask-lint-license--s-match-all
       '("http://mozilla.org/MPL/2.0/"))
      '("mpl-2.0" "Mozilla Public License 2.0" "MPL-2.0"))
     ((eask-lint-license--s-match-all
       '("Licensed under the Open Software License version 3.0"))
      '("osl-3.0" "Open Software License 3.0" "OSL-3.0"))
     ((eask-lint-license--s-match-all
       '("This is free and unencumbered software released into"))
      '("unlicense" "The Unlicense" "Unlicense"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim copies"))
      '("wtfpl-1" "Do What the Fuck You Want To Public License (Version 1)" "WTFPL-1"))
     ((eask-lint-license--s-match-all
       '("Everyone is permitted to copy and distribute verbatim or modified"))
      '("wtfpl-2" "Do What the Fuck You Want To Public License (Version 2)" "WTFPL-2"))
     ((eask-lint-license--s-match-all
       '("Permission is granted to anyone to use this software for any purpose,"))
      '("zlib" "zlib License" "Zlib"))
     (t
      '("unknown" "Unknown license" "unknown")))))

(defun eask-lint-license--print-scanned (scanned)
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
      (eask-info "(No license found)"))
     (t
      (when (<= 2 (length files))
        (eask-warn "!! Multi-licensing detected !!"))

      (eask-progress-seq "  - Sanning"  files "done! ✓"
                         (lambda (file)
                           (push (append (eask-lint-license--scan file) `(,file)) scanned)))

      (eask-msg "")
      (eask-lint-license--print-scanned (reverse scanned))))))

;;; lint/license.el ends here
