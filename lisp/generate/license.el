;;; generate/license.el --- Generate license file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command use generate LICENSE file,
;;
;;   $ eask generate license <name>
;;
;;
;;  Positionals:
;;
;;    <name>       Name of the license
;;
;;  Optional arguments:
;;
;;    --output     Output result to a file; the default is `LICENSE`
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

;;
;;; Externals

(defvar license-templates--data)

(declare-function license-templates-keys "ext:license-templates.el")

;;
;;; Core

(defun eask-generate-license--print-menu ()
  "Print all available license."
  (eask-msg "available via `eask generate license`")
  (eask-msg "")
  (let* ((names (license-templates-keys))
         (offset (eask-seq-str-max names))
         (fmt (concat "  %-" (eask-2str offset) "s  %s")))
    (dolist (data license-templates--data)
      (eask-msg fmt (plist-get data :key) (plist-get data :name)))
    (eask-msg "")
    (eask-info "(Total of %s available license%s)" (length names)
               (eask--sinr names "" "s"))))

(eask-start
  ;; Preparation
  (eask-archive-install-packages '("gnu" "melpa")
                                 'license-templates)

  ;; Start the task
  (require 'license-templates)
  (let* ((name (car (eask-args)))  ; type of the license
         (basename (or (eask-output) "LICENSE"))
         (filename (expand-file-name basename)))
    (license-templates-keys)  ; trigger request
    (eask-msg "")
    (cond ((file-exists-p filename)
           (eask-info "(The license file already exists `%s`)" filename))
          ((not (member name (license-templates-keys)))
           (eask-info "(Invalid license type: `%s`)" name)
           (eask-generate-license--print-menu))
          (t
           (eask-with-progress
             (format "  - [1/1] Generating license file in %s... " filename)
             (with-current-buffer (find-file filename)
               (license-templates-insert name)
               (save-buffer))
             "done ✓")
           (eask-msg "")
           (eask-info "(See created file in `%s`)" filename)))))

;;; generate/license.el ends here
