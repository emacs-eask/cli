;;; source/add.el --- Add an archive source  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Commmand use to add an archive source
;;
;;   $ eask source add <name> <path>
;;
;;
;;  Positionals:
;;
;;    <name>     name of the archive
;;    <url>      link to the archive
;;

;;; Code:

(let ((dir (file-name-directory (nth 1 (member "-scriptload" command-line-args)))))
  (load (expand-file-name "_prepare.el"
                          (locate-dominating-file dir "_prepare.el"))
        nil t))

(defun eask--source-from-mapping (name url)
  "Return t if NAME and URL matched our database."
  (string= (eask-source-url name) url))

(defun eask--source-add (name url exists)
  "Add an archive source by NAME.

If argument URL is nil; ignore the insertion.

Arguments EXISTS is used to print the information."
  (let* ((style-sym (string-match "([ \t\n\r]*source[ \t\n\r]*[']+" (buffer-string)))
         (name-str (if style-sym (concat "'" name)
                     (concat "\"" name "\"")))
         (built-in (eask--source-from-mapping name url)))
    (if (and url (not built-in))
        (insert "(source " name-str " \"" url "\")\n")
      (insert "(source " name-str ")\n"))
    (eask-info "(New source `%s' added and points to `%s')" name (or url (cdr exists)))))

(defun eask--source-write (name url exists)
  "Write source construct by NAME and URL.

The argument URL can be nil.

The argument EXISTS is use to search for correct position to insert new source."
  (with-current-buffer (find-file eask-file)
    (goto-char (point-min))
    (cond
     (exists
      (if (re-search-forward (concat "([ \t\n\r]*source[ \t\n\r]*['\"]+" name)  nil t)
          (let ((start (point))
                (built-in (eask--source-from-mapping name url)))
            (when (string= "\"" (string (char-after)))
              (cl-incf start))
            (re-search-forward "[ \t\r\n\")]*" nil t)  ; Forward to non-space characters!
            (forward-char -1)
            (when (string= "\n" (string (char-after)))
              (forward-char -1))
            (pcase (string (char-after))
              (")"
               (unless built-in
                 (insert " \"" url "\"")))
              ("\""
               (if built-in
                   (delete-region start (save-window-excursion
                                          (search-forward ")" nil t)
                                          (1- (point))))
                 (let ((old (thing-at-point 'string))
                       (new (concat "\"" url "\"")))
                   (delete-region (point) (+ (point) (length old)))
                   (insert new)))))
            (eask-info "(Changed archive `%s''s location from `%s' to `%s')"
                       name (cdr exists) url))
        (goto-char (point-max))
        (eask--source-add name url exists)))
     (t
      (if (re-search-forward "([ \t\n\r]*source[ \t\n\r]*['\"]+"  nil t)
          (forward-paragraph)
        (goto-char (point-max)))
      (eask--source-add name url exists)))
    (save-buffer)))

(defun eask--source-ask-if-overwrite (name url)
  "Ask source overwrite if needed.

Arguments NAME and URL are main arguments for this command."
  (if-let ((exists (assoc name package-archives))
           (old-url (cdr exists)))
      (cond ((string= old-url url)
             (eask-info "(Nothing has changed due to the URLs are the same)"))
            ((yes-or-no-p
              (format
               (concat
                "The archive `%s' is already exists and currently points  to `%s'.

Do you want to overwrite it? ")
               name old-url))
             (eask--source-write name url exists))
            (t
             (eask-info "(Nothing has changed, aborted)")))
    (eask--source-write name url nil)))

(eask-start
  (let* ((args (eask-args))
         (name (nth 0 args))
         (url  (nth 1 args)))
    (cond
     (url (eask--source-ask-if-overwrite name url))
     (t
      (if-let* ((archive (assoc (eask-intern name) eask-source-mapping))
                (name (car archive))
                (name (eask-2str name))
                (url (eask-source-url name)))  ; Use the URL from our database!
          (eask--source-ask-if-overwrite name url)
        (eask-info "(Invalid source name, `%s')" name)
        (eask-help "source/add"))))))

;;; source/add.el ends here
