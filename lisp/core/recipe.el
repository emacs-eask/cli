;;; recipe.el --- Suggest a recipe format  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command would suggest you the recipe for current package:
;;
;;   $ eask recipe
;;

;;; Code:

(load (expand-file-name
       "../_prepare.el"
       (file-name-directory (nth 1 (member "-scriptload" command-line-args))))
      nil t)

(eask-start
  (if-let ((url (eask-package-desc-url)))
      (let* ((fetcher (cond ((string-match-p "github.com" url) 'github)
                            ((string-match-p "gitlab.com" url) 'gitlab)
                            (t 'git)))
             (url-regex (if (eq fetcher 'github)
                            "http[s]://github.com/"
                          "http[s]://gitlab.com/"))
             (repo (replace-regexp-in-string url-regex "" url))
             (recipe
              `(,(intern (eask-guess-package-name)) :fetcher ,fetcher)))
        (cond ((memq fetcher '(git hg))
               (nconc recipe `(:url ,url)))
              ((memq fetcher '(gitlab github))
               (nconc recipe `(:repo ,repo))))
        (when eask-files
          (nconc recipe `(:files ,(append '(:defaults) eask-files))))
        (eask-msg "")
        (eask-msg "Recipe: %s" (pp-to-string recipe))
        (eask-msg ""))
    (eask-info "(Repository URL is required to form a recipe)")
    (eask-help 'recipe)))

;;; recipe.el ends here
