;;; core/recipe.el --- Suggest a recipe format  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Command would suggest you the recipe for current package:
;;
;;   $ eask recipe
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(eask-start
  (if-let ((url (eask-package-desc-url)))
      (let* ((fetcher (cond ((string-match-p "github.com" url) 'github)
                            ((string-match-p "gitlab.com" url) 'gitlab)
                            (t 'git)))
             (url-regex (if (eq fetcher 'github)
                            "http[s]://github.com/"
                          "http[s]://gitlab.com/"))
             (repo (replace-regexp-in-string url-regex "" url))
             (name (eask-guess-package-name))
             (recipe
              `(,(intern name) :fetcher ,fetcher)))
        (cond ((memq fetcher '(git hg))
               (nconc recipe `(:url ,url)))
              ((memq fetcher '(gitlab github))
               (nconc recipe `(:repo ,repo))))
        (when eask-files
          (nconc recipe `(:files ,(append '(:defaults) eask-files))))
        (eask-msg "")
        (eask-msg "recipes/%s:" name)
        (eask-msg "")
        (eask-msg "%s" (pp-to-string recipe))
        (eask-msg ""))
    (eask-msg "")
    (eask-info "(Repository URL is required to form a recipe)")
    (eask-help "core/recipe")))

;;; core/recipe.el ends here
