;;; autofix.el --- Autofixes for emacs packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/autofix
;; Keywords: convenience
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Annotate and autofix package headers


;;; Commands

;; M-x `autofix'

;; Apply all code and comments fixes.

;; Code fixes includes auto adding autoload comments before all interactive
;; commmands and removing unused (declare-function ...) forms.

;; Comments fixes includes fixes for file headers, package headers, footer etc

;;; Header fixes

;; M-x `autofix-header'
;;      Autofix comments header.
;; M-x `autofix-header-first-line'
;;      Fix or create the first comment line in the header.
;; M-x `autofix-copyright'
;;      Prompt and fix or make new copyright.
;; M-x `autofix-author'
;;      Add current user as new author to existing or new author section.
;; M-x `autofix-url'
;;      Return string with generated url.
;; M-x `autofix-keywords'
;;      Add or fix package keywords.
;; M-x `autofix-version'
;;      Add or fix package version.
;; M-x `autofix-header-body-comment'
;;      Add additional comments after package headers.
;;      Default vaiue is the comment starting with "This file is NOT part of GNU Emacs..."),
;;      To change it customize the variable `autofix-comment-section-body'.
;; M-x `autofix-package-requires'
;;      Add or fix package requires section.
;; M-x `autofix-commentary'
;;      Add Commentary section in current buffer if none.
;; M-x `autofix-code-comment'
;;      Add Code comment to the end of header block.
;; M-x `autofix-annotate-buffer'
;;      Add annotatations in header comment section.
;;      Annotations includes commands and custom variables.

;;; Code fixes

;; M-x `autofix-autoloads'
;;      Add autoload comments before all interactive functions in buffer.
;; M-x `autofix-remove-unused-declarations'
;;      Removed unused declared functions

;;; Footer

;; M-x `autofix-footer'
;;      Add of fix file footer (provide 'filename) with comment ends here.

;; M-x `autofix-copy-org-annotations'
;;      Copy org annotation of items in buffer.
;; M-x `autofix-show-org-annotations'
;;      Show annotations as org list items.
;; M-x `autofix-scan-extract-all-docs'
;;      Return string with all docs in all buffer.
;;      If called interactively also copy it.

;;; Customization

;; `autofix-ignored-file-patterns'
;;      List of file name bases to ignore.

;; `autofix-user-fullname'
;;      User email to add in header section.

;; `autofix-user-email'
;;      User email to add in header section.
;;      Can be string, variable or function.
;;      Function will be called without args and should return string.

;; `autofix-comment-section-body'
;;      Static text for adding in header comment section.
;;      It doesn't includes dynamic variables such author, year etc.

;;; Code:

(defcustom autofix-comment-section-body ";; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
  "Static text for adding in header comment section.
It doesn't includes dynamic variables such author, year etc."
  :type 'string
  :group 'autofix)

(defvar autofix-package-headers '("Author"
                                  "Maintainer"
                                  "Created"
                                  "Version"
                                  "Keywords"
                                  "URL"
                                  "Homepage"
                                  "Package-Version"
                                  "Version"
                                  "Package-Requires"))

(defvar autofix-package-header-re (concat "^;;\s"
                                          "\\("
                                          (string-join autofix-package-headers
                                                       "\\|")
                                          "\\):"))

(defun autofix-detect-user-email ()
  "Guess the user's email address. Return nil if none could be found."
  (when-let ((mail (or (shell-command-to-string
                        "git config --get user.email")
                       (shell-command-to-string
                        "git config --global --get user.email")
                       (getenv "EMAIL"))))
    (string-trim mail)))

(defcustom autofix-user-email 'autofix-detect-user-email
  "User email to add in header section.
Can be string, variable or function.
Function will be called without args and should return string."
  :type '(choice :tag "User email"
                 (string :tag "Type your email")
                 (const :tag "Autodetect" autofix-detect-user-email)
                 (function :tag "Custom function"))
  :group 'autofix)

(defun autofix-string-to-undescore (str)
  "Trasnfrom STR to underscored."
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string
               "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string
               "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))

(defun autofix-get-user-email ()
  "Get user email from variable `autofix-user-email'."
  (cond ((functionp autofix-user-email)
         (funcall autofix-user-email))
        (t autofix-user-email)))

(defun autofix-detect-user-full-name ()
  "Detect user full name from git config, git email or function `user-full-name'."
  (when-let ((str (or
                   (shell-command-to-string "git config --get user.name")
                   (autofix-get-user-email)
                   (user-full-name))))
    (mapconcat 'capitalize
               (split-string
                (autofix-string-to-undescore
                 (replace-regexp-in-string
                  "@.+" ""
                  (string-trim str)))
                "[^a-zZ-A]" t)
               "\s")))

(defcustom autofix-user-fullname 'user-full-name
  "User email to add in header section."
  :type '(choice :tag "User email"
                 (string :tag "Type your full name")
                 (const :tag "Autodetect"
                        autofix-detect-user-full-name)
                 (function :tag "Use value from `user-full-name'"
                           user-full-name)
                 (function :tag "Custom function"))
  :group 'autofix)

(defcustom autofix-ignored-file-patterns '("init.el"
                                           "custom.el"
                                           "early-init.el")
  "List of file name bases to ignore."
  :type '(repeat (regexp :tag "Regexp"))
  :group 'autofix)

(defmacro autofix-with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
     (progn
       ,@body)))

(defun autofix-overlay-prompt-region (beg end fn &rest args)
	"Highlight region from BEG to END while invoking FN with ARGS."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn
          (goto-char beg)
          (overlay-put overlay 'face 'diary)
          (apply fn args))
      (delete-overlay overlay))))

(defun autofix-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `elisp-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun autofix-elisp-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `autofix-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun autofix-git-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil '("remote" "-v")))
                  (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l) (let ((parts (split-string l)))
                      (cons (car parts)
                            (cadr parts))))
             (split-string remotes "\n" t)))))

(defun autofix-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'autofix-re-search-backward-inner)
               ((> count 0) #'autofix-elisp-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun autofix-get-user-fullname ()
  "Return user full name.
See `autofix-user-fullname'."
  (cond ((functionp autofix-user-fullname)
         (funcall autofix-user-fullname))
        (t autofix-user-fullname)))

(defun autofix-author-annotation ()
  "Return string with full name and <email>."
  (when-let ((author
              (delete
               nil
               (list (autofix-get-user-fullname)
                     (when-let ((email (autofix-get-user-email)))
                       (format "<%s>" email))))))
    (string-join author "\s")))

(defun autofix-jump-to-header-end ()
  "Jump to end of line of the header comment section."
  (let ((buff-str (buffer-string))
        (buff (current-buffer)))
    (when-let ((pos
                (with-temp-buffer
                  (insert buff-str)
                  (delay-mode-hooks
                    (emacs-lisp-mode)
                    (goto-char (point-min))
                    (when-let ((form-start
                                (progn
                                  (goto-char (point-min))
                                  (ignore-errors (forward-sexp 1))
                                  (if (equal (point) (point-min))
                                      (while (looking-at ";")
                                        (forward-line 1)
                                        (while (looking-at "\n+[;]")
                                          (forward-line 1)))
                                    (when (looking-back "[)]" 0)
                                      (forward-sexp -1))
                                    (or (re-search-backward
                                         "\\(;;;###\\)autoload[\s\t\n]" nil t 1)
                                        (point))))))
                      (when (re-search-backward ";" nil t 1)
                        (end-of-line)
                        (point)))))))
      (with-current-buffer buff
        (goto-char pos)
        pos))))

(defun autofix-header-get-regexp-info (regexp)
  "Search for REGEXP in header section.
Return list of (\"REGEXP MATCH ...\" start end)."
  (when-let ((max (save-excursion (autofix-jump-to-header-end))))
    (save-excursion
      (goto-char (point-min))
      (when
          (re-search-forward
           regexp
           max t 1)
        (list (match-string-no-properties 0)
              (match-beginning 0)
              (match-end 0))))))

(defun autofix-add-author-str (author-header new-author)
  "Add NEW-AUTHOR to AUTHOR-HEADER."
  (let ((parts (split-string author-header "\n" t)))
    (cond ((string-empty-p (string-trim (replace-regexp-in-string
                                         "^;;[\s]Author:[\s]*" ""
                                         (car (reverse parts)))))
           (concat ";; Author:\s" new-author "\n"))
          ((= 1 (length parts))
           (concat (string-trim author-header)
                   "\n" ";;"
                   (make-string (length "\sAuthor:\s") ?\ )
                   new-author
                   "\n"))
          (t (when-let ((col
                         (string-match-p
                          "[^;\s\t]"
                          (replace-regexp-in-string "^[;]+" ""
                                                    (car (reverse parts))))))
               (concat author-header ";;" (make-string col ?\ )
                       new-author "\n"))))))

;;;###autoload
(defun autofix-author ()
  "Add current user as new author to existing or new author section."
  (interactive)
  (when-let ((annotation (autofix-author-annotation)))
    (if-let ((author-header
              (autofix-header-get-regexp-info
               "^;;[\s]Author:\\(\\([^\n]*\\)\n\\(;;[\s][\s]+\\([^\n]+\\)[\n]\\)*\\)")))
        (unless (string-match-p (autofix-get-user-email) (car author-header))
          (let ((beg (nth 1 author-header))
                (end (nth 2 author-header))
                (rep (autofix-add-author-str (car author-header) annotation)))
            (let ((overlay (apply 'make-overlay (cdr author-header)))
                  (confirmed))
              (unwind-protect
                  (progn
                    (overlay-put overlay 'face 'error)
                    (goto-char beg)
                    (overlay-put overlay 'after-string
                                 (propertize rep 'face 'success))
                    (setq confirmed (yes-or-no-p "Relace?")))
                (delete-overlay overlay))
              (when confirmed
                (replace-region-contents beg end (lambda () rep))))))
      (when (autofix-jump-to-package-header-start)
        (insert
         (concat (if (looking-back "\n\n" 0) "" "\n")
                 ";; Author:\s" annotation "\n"
                 (if (or (looking-at autofix-package-header-re)
                         (looking-at "\n"))
                     "" "\n")))))))

;;;###autoload
(defun autofix-version ()
  "Add or fix package version."
  (interactive)
  (if-let ((info (autofix-header-get-regexp-info
                  ";;[\s]Version:\\([^\n]+\\)*")))
      (let* ((beg (nth 1 info))
             (end (nth 2 info))
             (current-version (string-trim
                               (replace-regexp-in-string "^;; Version:"
                                                         ""
                                                         (car info))))
             (rep (autofix-read-header-string
                   ";; Version: "
                   (if (string-empty-p current-version)
                       "0.1.0"
                     current-version))))
        (if rep
            (replace-region-contents beg end (lambda () rep))
          (delete-region beg end)))
    (autofix-jump-to-package-header-end)
    (insert
     (concat (if (looking-back "\n" 0) "" "\n")
             ";; Version: 0.1.0" "\n"
             (if (or (looking-at autofix-package-header-re)
                     (looking-at "\n"))
                 "" "\n")))))

(require 'package-lint)
(defun autofix-get-emacs-version ()
  "Return suitable Emacs version for current package."
  (when-let ((l
              (last
               (seq-sort-by
                'cadr
                'version<
                (seq-uniq
                 (mapcar
                  (lambda (it)
                    (with-temp-buffer
                      (save-excursion (insert (car (reverse it))))
                      (when
                          (re-search-forward
                           "\\(\\([-*_~$A-Za-z0-9:.\\+]+\\)[\s]\"\\([0-9]+\\.[0-9]+\\)\"\\)"
                           nil t 1)
                        (list
                         (match-string-no-properties 2)
                         (match-string-no-properties 3)))))
                  (seq-filter
                   (lambda (it) (string-match-p "You should depend on[\s][(]emacs[\s]"
                                           (car (reverse it))))
                   (package-lint--check-all))))))))
    (mapcar (lambda (i) (concat "(" (car i) " \"" (cadr i) "\"" ")"))
            l)))

;;;###autoload
(defun autofix-package-requires ()
  "Add or fix package requires section."
  (interactive)
  (if-let ((info (autofix-header-get-regexp-info
                  ";;[\s]Package-Requires:\\([^\n]+\\)?")))
      (when-let ((required-verison (autofix-get-emacs-version)))
        (let* ((beg (nth 1 info))
               (end (nth 2 info))
               (value (replace-regexp-in-string
                       "[(]emacs[\s\t]*\"[^\"]+\"[)]"
                       (car required-verison)
                       (string-trim
                        (replace-regexp-in-string
                         "^;; Package-Requires:"
                         ""
                         (car info)))))
               (rep (concat
                     ";; Package-Requires: " (if (string-match-p "[(][(]" value)
                                                 value
                                               (concat "(" value ")")))))
          (if rep
              (replace-region-contents beg end (lambda () rep))
            (delete-region beg end))))
    (when-let ((required-verison (autofix-get-emacs-version)))
      (autofix-jump-to-package-header-end)
      (insert (concat (if (and (looking-back "\n" 0)
                               (save-excursion
                                 (forward-line -1)
                                 (looking-at
                                  (concat "^;;" "[\s]\\("
                                          (string-join autofix-package-headers
                                                       "\\|")
                                          "\\)" ":"
                                          "\\(\\([^\n]*\\)\n\\(;;[\s][\s]+\\([^\n]+\\)[\n]\\)*\\)"
                                          ))))
                          ""
                        "\n"
                        )
                      ";; Package-Requires: " (format "%s" required-verison)
                      (if (looking-at "\n\n")  "" "\n"))))))

(defun autofix-jump-to-package-header-end ()
  "Jump to the end of package header end."
  (autofix-jump-to-package-header-start)
  (let ((skip-re (concat "^;;" "[\s]\\(" (string-join autofix-package-headers
                                                      "\\|")
                         "\\)" ":"
                         "\\(\\([^\n]*\\)\n\\(;;[\s][\s]+\\([^\n]+\\)[\n]\\)*\\)"
                         )))
    (while (looking-at skip-re)
      (re-search-forward skip-re nil t 1))))

;;;###autoload
(defun autofix-header-body-comment ()
  "Add additional comments after package headers.
Default vaiue is comment starting with \"This file is NOT part of
GNU Emacs...\"),
To change the value customize the variable `autofix-comment-section-body'."
  (interactive)
  (when autofix-comment-section-body
    (autofix-jump-to-package-header-end)
    (unless (re-search-forward autofix-comment-section-body nil t 1)
      (insert (concat (if (looking-back "\n\n" 0) "\n" "\n")
                      autofix-comment-section-body)))))

;;;###autoload
(defun autofix-keywords ()
  "Add or fix package keywords."
  (interactive)
  (if-let ((info (autofix-header-get-regexp-info
                  ";;[\s]Keywords:\\([^\n]+\\)?")))
      (unless (string-match-p "[0-9]" (car info))
        (let ((beg (nth 1 info))
              (end (nth 2 info))
              (rep (autofix-read-keyword
                    (split-string
                     (string-trim
                      (replace-regexp-in-string
                       ";; Keywords:"
                       ""
                       (car info)))
                     "[ \f\t\n\r\v,]+"))))
          (if rep
              (replace-region-contents beg end (lambda ()
                                                 (concat ";; Keywords: "
                                                         (string-join rep
                                                                      "\s"))))
            (delete-region beg end))))
    (when-let ((keywords (when (autofix-jump-to-package-header-start)
                           (autofix-read-keyword))))
      (autofix-jump-to-package-header-end)
      (insert (if (looking-back "\n" 0) "" "\n") ";; Keywords: "
              (string-join keywords "\s")
              (if (or (looking-at autofix-package-header-re)
                      (looking-at "\n"))
                  "" "\n")
              "\n"))))

(defun autofix-jump-to-package-header-start ()
  "Jump to the start of package header section or to place for new."
  (goto-char (point-min))
  (while (looking-at (concat ";;;\\|\\(;;[\s]Copyright[\s]\\)\\|\n"))
    (forward-line 1))
  (point))

(defvar finder-known-keywords)

;;;###autoload
(defun autofix-throw-done ()
  "Throw to the catch for done and return nil from it."
  (interactive)
  (throw 'done nil))

(defvar autofix-multi-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                'autofix-throw-done)
    (define-key map (kbd "C-M-j")
                'autofix-throw-done)
    map)
  "Keymap used in `autofix-read-keyword'.")

(defun autofix-read-keyword (&optional initial-values)
  "Read multiple standard Keywords headers with INITIAL-VALUES."
  (require 'finder nil t)
  (let* ((alist (when
                    (progn
                      (require 'finder nil t)
                      (bound-and-true-p finder-known-keywords))
                  (mapcar (lambda (it) (cons (symbol-name (car it)) (cdr it)))
                          finder-known-keywords)))
         (annotf (lambda (str) (format " (%s)" (cdr (assoc str alist)))))
         (choices)
         (curr))
    (dolist (it initial-values)
      (unless (yes-or-no-p (format "Remove keyword %s?" it))
        (setq choices (append (list it) choices))
        (setq alist (delete (assoc it alist) alist))))
    (catch 'done
      (while (setq curr
                   (minibuffer-with-setup-hook
                       (lambda ()
                         (use-local-map
                          (let ((map (copy-keymap
                                      autofix-multi-completion-map)))
                            (set-keymap-parent map
                                               (current-local-map))
                            map)))
                     (completing-read
                      (concat "Keywords\s" (substitute-command-keys "(`\\<autofix-multi-completion-map>\
\\[autofix-throw-done]' to finish)\s")
                              (if choices
                                  (concat
                                   "("
                                   (string-join
                                    choices
                                    ", ")
                                   ")")
                                ""))
                      (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (annotation-function . ,annotf))
                          (complete-with-action action alist str pred))))))
        (setq choices (append choices (list curr)))
        (setq alist (delete (assoc curr alist) alist))))
    choices))

(defun autofix-ssh-to-https (ssh-remote)
	"Convert SSH-REMOTE to https url."
  (with-temp-buffer
    (save-excursion (insert ssh-remote))
    (when (re-search-forward "@" nil t 1)
      (let* ((beg (point))
             (end (re-search-forward ":" nil t 1)))
        (string-trim
         (concat "https://"
                 (buffer-substring-no-properties
                  beg (1- end))
                 "/"
                 (buffer-substring-no-properties
                  end (point-max))))))))

(defun autofix-repo-urls ()
  "Return list of remotes converted to https urls."
  (let ((cell (with-temp-buffer
                (cons (= 0 (apply #'call-process
                                  "git" nil t nil
                                  '("remote" "-v")))
                      (string-trim (buffer-string))))))
    (when (car cell)
      (mapcar
       (lambda (url)
         (if (string-match-p "^git@" url)
             (replace-regexp-in-string "\\.git$" ""
                                       (autofix-ssh-to-https
                                        url))
           url))
       (seq-uniq
        (mapcar (lambda (l) (nth 1 (split-string l nil t)))
                (split-string (cdr cell) "\n" t)))))))

(defun autofix-goto-last-form ()
  "Jump to last Lisp form in buffer."
  (goto-char (point-max))
  (when-let ((pos
              (save-excursion
                (ignore-errors (forward-list -1))
                (when (looking-at "[(]")
                  (point)))))
    (goto-char pos)))

(defun autofix-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun autofix-get-current-file-header ()
  "Get list with first line of file header - (filename description bindings)."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at ";;;")
      (skip-chars-forward ";\s\t")
      (let ((parts)
            (beg (point))
            (filename)
            (description)
            (bindings))
        (when (and (not (eobp))
                   (looking-at "[^\n\r\f]"))
          (with-syntax-table emacs-lisp-mode-syntax-table
            (ignore-errors (forward-sexp 1))
            (setq filename (buffer-substring-no-properties beg (point)))))
        (unless (string-empty-p filename)
          (skip-chars-forward "\s\t")
          (when (looking-at "-\\{3\\}")
            (skip-chars-forward "-")
            (skip-chars-forward "\s\t")
            (setq parts (split-string
                         (buffer-substring-no-properties
                          (point)
                          (line-end-position))
                         "-\\*-"))
            (setq description (pop parts))
            (setq bindings (pop parts)))
          (seq-remove 'string-empty-p (mapcar 'string-trim
                                              (delete nil
                                                      (list
                                                       filename description
                                                       bindings)))))))))

(defun autofix-guess-feature-name ()
  "Return file name base from current file or buffer."
  (file-name-base (or (buffer-file-name) (format "%s" (current-buffer)))))

(defun autofix-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun autofix-function-p (symb)
  "Return t is SYMB can have arguments.
SYMB can be either symbol, either string."
  (member (if (symbolp symb)
              (symbol-name symb)
            symb)
          '("defun"
            "defmacro"
            "defun*"
            "defsubst"
            "cl-defun"
            "define-inline"
            "define-advice")))

(defvar autofix-group-annotation-alist
  '((:use-package . "Used Packages")
    (:interactive . "Commands")
    (:defcustom . "Customization")))

(defun autofix-parse-list-at-point ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"autofix-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                      (list-at-point)))
              (type (car sexp))
              (id (autofix-unquote (when (symbolp (nth 1 sexp))
                                       (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc (seq-find 'stringp (reverse (seq-take sexp 4))))
          (args (and (autofix-function-p type)
                     (nth 2 sexp))))
      (list name args doc
            (if (and (autofix-function-p type)
                     (eq 'interactive (car (nth (if doc 4 3) sexp))))
                'interactive
              type)))))

(defun autofix-annotate-with (prefix fn)
  "Return string of grouped annotations.

Each group is prefixed with PREFIX, and constists of
results of calling FN with list of (symbol-name args doc deftype)."
  (when-let ((items (autofix-scan-buffer)))
    (let ((blocks))
      (dolist (key (mapcar 'car autofix-group-annotation-alist))
        (when-let ((title (alist-get key autofix-group-annotation-alist))
                   (description (plist-get items key)))
          (setq description
                (concat prefix
                        title
                        "\n\n"
                        (funcall fn description)
                        "\n"))
          (push description blocks)))
      (when blocks
        (string-join (reverse blocks))))))

(defun autofix-annotate-list-item (item-list &optional mode)
  "Format ITEM-LIST to org list item or comment depending on MODE.
ITEM-LIST is a list of (NAME ARGS DOC-STRING DEFINITION-TYPE).
For example:
\(\"my-function\" (my-arg) \"Doc string.\" defun)"
  (let ((name (pcase mode
                ('org-mode (format "+ ~%s~" (car item-list)))
                (_ (format (if (member 'interactive item-list)
                               ";; M-x `%s'"
                             ";; `%s'")
                           (car item-list)))))
        (args (when (nth 1 item-list)
                (format " %s" (nth 1 item-list))))
        (doc (when (nth 2 item-list)
               (format "\n%s"
                       (mapconcat
                        (apply-partially
                         'format
                         (pcase mode
                           ('org-mode "%s" )
                           (_ ";;      %s" )))
                        (split-string (nth 2 item-list) "[\n]")
                        "\n")))))
    (format (if doc "%s\n" "%s\n")
            (string-join (delete nil (list name args doc))))))

(defun autofix-scan-buffer ()
  "Return plist of top level Lisp definitions.

Each key is definition type, converted to keyword (:defmacro, :defun etc),
except interactive functions, which holds under keyword :interactive.

The value of plist is a list of sublists of form (symbol-name args doc deftype).

See function `autofix-parse-list-at-point'."
  (save-excursion
    (let ((pl '()))
      (goto-char (point-max))
      (while (autofix-backward-list)
        (when-let ((sexp (autofix-parse-list-at-point)))
          (let ((keyword (intern (concat ":" (symbol-name (car
                                                           (reverse sexp)))))))
            (if-let ((group (plist-get pl keyword)))
                (setq pl (plist-put pl keyword (append group (list sexp))))
              (setq pl (plist-put pl keyword (list sexp)))))))
      pl)))

(defun autofix-annotate-to-org (item-list)
	"Format ITEM-LIST to org list item.
ITEM-LIST is a list of (NAME ARGS DOC-STRING DEFINITION-TYPE).
For example:
\(\"my-function\" (my-arg) \"Doc string.\" defun)"
  (let ((name (format "+ ~%s~" (car item-list)))
        (args (when (nth 1 item-list)
                (format " %s" (nth 1 item-list))))
        (doc (when (nth 2 item-list)
               (string-join (split-string
                             (with-temp-buffer
                               (save-excursion (insert
                                                (nth 2 item-list)))
                               (while (re-search-forward
                                       "`\\([a-zZ-A0-9-+]+\\)'" nil t 1)
                                 (replace-match
                                  (concat "~"
                                          (match-string-no-properties 1) "~")))
                               (buffer-string))
                             nil t)
                            "\s")))
        (title))
    (setq title (string-join (delete nil (list name args)) "\s"))
    (if doc
        (concat title "\n" doc "\n")
      (concat title "\n"))))

(defun autofix-annotate-as-org-list (sexps)
  "Return string with generetad from SEXPS annotations as org list items."
  (mapconcat
   (lambda (s) (autofix-annotate-to-org s))
   sexps "\n"))

(defun autofix-annotate-as-comments (sexps)
  "Return string with generetad from SEXPS annotations as comments."
  (mapconcat
   'autofix-annotate-list-item
   sexps "\n"))

(defun autofix-org-annotation ()
	"Return string with readme template in org mode format."
  (when-let* ((remote (cdr (car (autofix-git-remotes-alist))))
              (parts (reverse
                      (seq-take (reverse
                                 (split-string
                                  (replace-regexp-in-string
                                   "\\.git$"
                                   ""
                                   (cdr (car (autofix-git-remotes-alist))))
                                  "[:/]"
                                  t))
                                2)))
              (user (pop parts))
              (name (pop parts)))
    (let ((items (autofix-scan-buffer)))
      (string-join
       (list
        (format "* %s" name)
        "** Installation"
        "*** Manually"
        "Download repository and it to your load path in your init file:"
        "#+begin_src elisp :eval no"
        (format "(add-to-list 'load-path \"/path/to/%s/\")" name)
        (format "(require '%s)" name)
        "#+end_src"
        "*** With use-package and straight"
        "#+begin_src elisp :eval no"
        (autofix-with-temp-lisp-buffer
            (save-excursion
              (insert (concat "(" "use-package\s" name "\n" "\s\s" ":straight (:repo "
                              "\""
                              user
                              "/"
                              name
                              "\""
                              "\s:type git :host github)"
                              (when-let ((commands (plist-get items
                                                              :interactive)))
                                (concat "\n\s:commands\s(" (mapconcat 'car
                                                                      commands "\n")
                                        ")"))
                              ")")))
            (indent-sexp)
          (buffer-substring-no-properties (point-min) (point-max)))
        "#+end_src"
        (autofix-annotate-with
         "** "
         'autofix-annotate-as-org-list))
       "\n\n"))))

(defun autofix-read-header-string (prompt default-value)
  "Read a non-empty string from the minibuffer with PROMPT and DEFAULT-VALUE."
  (let ((str (string-trim (read-string prompt default-value))))
    (unless (string-empty-p str)
      (concat prompt str))))

(defun autofix-ensure-autoload ()
  "Insert autoload if list at point is a command without autoload comment."
  (when (eq 'interactive (car (reverse (autofix-parse-list-at-point))))
    (unless (looking-back ";;;###autoload[\s\t\n]+" 0)
      (insert ";;;###autoload\n")
      t)))

(defun autofix-make-short-annotation ()
  "Trim prefix from buffer file name base."
  (string-join
   (split-string
    (replace-regexp-in-string
     "^[a-z]+-" ""
     (or (if buffer-file-name
             (file-name-base buffer-file-name)
           (buffer-name (current-buffer)))))
    "[^a-z]" t)
   "\s"))

(defun autofix-non-directory-file-or-buff ()
  "Return file name sans its directory from current buffer."
  (if buffer-file-name
      (file-name-nondirectory buffer-file-name)
    (let ((name (buffer-name (current-buffer))))
      (if (string-suffix-p ".el" name)
          name
        (concat name ".el")))))

(defun autofix-get-current-copyright ()
  "Return line substring with Copyright header comment."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^;;[\s\t]+Copyright\\([\s\t]?+\\(©\\|[(][Cc][)]\\)\\)"
                             nil t 1)
      (skip-chars-forward "\s\t")
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun autofix-make-copyright ()
  "Return string with genereted Copyright."
  (concat ";; Copyright (C) " (format-time-string "%Y ")
          (read-string
           "Copyright author:\s"
           (autofix-author-annotation))))

;;;###autoload
(defun autofix-copyright ()
  "Prompt and fix or make new copyright."
  (interactive)
  (when-let ((author (autofix-author-annotation)))
    (if-let ((current (autofix-get-current-copyright)))
        (save-excursion
          (goto-char (point-min))
          (unless (string-match-p
                   (regexp-quote author)
                   current)
            (when-let ((line-end
                        (when
                            (re-search-forward
                             "^;;[\s\t]+Copyright\\([\s\t]?+\\(©\\|[(][Cc][)]\\)\\)"
                             nil t 1)
                          (line-end-position))))
              (when (yes-or-no-p
                     (format "Change copyright %s?"
                             (buffer-substring-no-properties
                              (point)
                              (line-end-position))))
                (let ((new-copyright (read-string "Copyright" author)))
                  (replace-region-contents (point) line-end
                                           (lambda () new-copyright)))))))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (insert "\n" (autofix-make-copyright)
                (if (looking-at "[^\n]")
                    "\n\n"
                  (if (looking-at "\n\n") "" "\n")))))))

;;;###autoload
(defun autofix-url ()
  "Return string with generated url."
  (interactive)
  (when-let ((remotes (autofix-repo-urls)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^;;[\s\t]+URL:" nil t 1)
          (let ((beg (point))
                (end (line-end-position)))
            (let ((curr-url (string-trim (buffer-substring-no-properties
                                          (point) end))))
              (when-let ((remotes (autofix-repo-urls)))
                (unless (member curr-url remotes)
                  (autofix-overlay-prompt-region
                   beg end
                   (lambda () (let ((url (if (= 1 (length remotes))
                                        (read-string ";; URL: " (car remotes))
                                      (completing-read ";; URL: " remotes))))
                           (replace-region-contents beg end (lambda () url)))))))))
        (autofix-jump-to-package-header-end)
        (insert (if (looking-back "\n" 0) "" "\n") ";; URL: "
                (if (= 1 (length remotes))
                    (read-string ";; URL: " (car remotes))
                  (completing-read ";; URL: " remotes))
                "\n")))))

;;;###autoload
(defun autofix-annotate-buffer ()
  "Add annotatations in header comment section.
Annotations includes commands, custom variables."
  (interactive)
  (save-excursion
    (when-let ((blocks (autofix-annotate-with
                        ";;; "
                        'autofix-annotate-as-comments)))
      (autofix-jump-to-header-end)
      (when (re-search-backward ";;; Code:" nil t 1)
        (let ((pos (point)))
          (dolist (title (mapcar 'cdr autofix-group-annotation-alist))
            (re-search-backward (concat "^;;;[\s\t]\\("
                                        (regexp-quote title) "\\)")
                                nil t 1))
          (when blocks
            (replace-region-contents
             pos (point)
             (lambda () blocks))))))))

;;;###autoload
(defun autofix-scan-extract-all-docs ()
  "Return string with all docs in all buffer.
If called interactively also copies it."
  (interactive)
  (let ((docs (string-join
               (mapcar
                (lambda (it)
                  (autofix-annotate-list-item it 'org-mode))
                (seq-reduce
                 (lambda (acc it) (setq acc (if (keywordp it)
                                           acc
                                         (append acc it))))
                 (autofix-scan-buffer)
                 '()))
               "\n")))
    (when (called-interactively-p 'any)
      (kill-new docs))
    docs))

;;;###autoload
(defun autofix-show-org-annotations ()
  "Show annotations as org list items."
  (interactive)
  (when-let ((buff-name (autofix-guess-feature-name))
             (description (autofix-org-annotation)))
    (with-current-buffer (get-buffer-create
                          (concat "*autofix-annotate*"))
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert description))
      (org-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun autofix-copy-org-annotations ()
  "Copy org annotation of items in buffer."
  (interactive)
  (kill-new (autofix-org-annotation)))

;;;###autoload
(defun autofix-header-first-line ()
  "Fix or create the first comment line in the header."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((filename (autofix-non-directory-file-or-buff)))
      (if-let ((current-header (autofix-get-current-file-header)))
          (let ((max-pos (line-end-position))
                (current-file (nth 0 current-header))
                (current-description (nth 1 current-header))
                (current-bindings (nth 2 current-header))
                (valid))
            (setq valid (equal filename current-file))
            (skip-chars-forward ";\s\t")
            (when (and current-file
                       (re-search-forward
                        (regexp-quote current-file) max-pos t 1))
              (unless valid
                (replace-match filename))
              (skip-chars-forward "\s\t")
              (if (null current-description)
                  (insert (read-string
                           "Description:\s"
                           (concat
                            "Configure "
                            (autofix-make-short-annotation)))
                          " -*- lexical-binding: t; -*-"
                          (if (looking-at "[^\n]")
                              "\n\n"
                            (if (looking-at "\n\n") "" "\n")))
                (let ((beg (progn (skip-chars-forward "-\s\t")
                                  (point)))
                      (end (or (re-search-forward (regexp-quote
                                                   current-description)
                                                  max-pos t 1)
                               max-pos))
                      (new-descr (unless valid (read-string
                                                "Description:\s"
                                                current-description))))
                  (when (and beg end new-descr)
                    (replace-region-contents beg end (lambda () new-descr)))
                  (unless current-bindings
                    (insert " -*- lexical-binding: t -*-"
                            (if (looking-at "[^\n]")
                                "\n\n"
                              (if (looking-at "\n\n") "" "\n"))))))))
        (insert (concat ";;; " filename "\s" "--- "
                        (read-string "Description:\s")
                        " -*- lexical-binding: t; -*-"
                        (if (looking-at "[^\n]")
                            "\n\n"
                          (if (looking-at "\n\n") "" "\n"))))))))

;;;###autoload
(defun autofix-code-comment ()
  "Add Code comment to the end of header block."
  (interactive)
  (save-excursion
    (autofix-jump-to-header-end)
    (unless
        (looking-back "^;;; Code:" 0)
      (insert
       (concat (if (looking-back "\n\n" 0) "" "\n\n")
               ";;; Code:" (if (looking-at "\n\n") ""
                             (if (looking-at "\n")
                                 "\n"
                               "\n\n")))))))

;;;###autoload
(defun autofix-commentary ()
  "Add Commentary section in current buffer if none."
  (interactive)
  (save-excursion
    (autofix-jump-to-header-end)
    (if (re-search-backward "^;;; Commentary:"
                            nil t 1)
        (unless (looking-back "\n\n" 0)
          (insert "\n"))
      (autofix-jump-to-package-header-start)
      (when autofix-comment-section-body
        (re-search-forward autofix-comment-section-body
                           nil t 1))
      (insert
       (concat (if (looking-back "\n\n" 0) "" "\n")
               ";;; Commentary:\n\n" ";; "
               (read-string
                "Commentary:"
                (nth 1 (autofix-get-current-file-header)))
               (if (looking-at "\n\n") "" "\n"))))))

;;;###autoload
(defun autofix-autoloads ()
  "Add autoload comments before all interactive functions in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (autofix-backward-list)
      (autofix-ensure-autoload))))

;;;###autoload
(defun autofix-header ()
  "Apply all header fixes for header comments."
  (interactive)
  (autofix-header-first-line)
  (autofix-copyright)
  (autofix-author)
  (autofix-url)
  (autofix-version)
  (autofix-keywords)
  (autofix-package-requires)
  (autofix-header-body-comment)
  (autofix-commentary)
  (autofix-code-comment))

;;;###autoload
(defun autofix-remove-unused-declarations ()
  "Removed unused declared functions."
  (interactive)
  (let* ((declarations-re (mapcar (lambda (it) (regexp-quote (car it)))
                                  (plist-get (autofix-scan-buffer)
                                             :declare-function)))
         (unused (seq-remove
                  (lambda (re) (save-excursion
                            (goto-char (point-min))
                            (autofix-re-search-forward re nil t 2)))
                  declarations-re)))
    (dolist (re unused)
      (save-excursion
        (goto-char (point-min))
        (autofix-re-search-forward re nil t 1)
        (backward-up-list 1)
        (let ((bounds (bounds-of-thing-at-point 'sexp)))
          (delete-region (car bounds) (cdr bounds)))))))

;;;###autoload
(defun autofix-footer ()
  "Add of fix file footer (provide 'filename) with comment ends here."
  (interactive)
  (save-excursion
    (let* ((name (autofix-guess-feature-name))
           (footer-end (concat ";;; " name ".el" " ends here")))
      (autofix-goto-last-form)
      (if-let* ((bounds (bounds-of-thing-at-point 'list))
                (l (list-at-point))
                (id (and
                     (eq (car l) 'provide)
                     (= 2 (length l))
                     (autofix-unquote (nth 1 l)))))
          (progn
            (save-excursion
              (forward-line 1)
              (when (looking-at ";;;")
                (end-of-line)
                (when (looking-back "ends[\s]here" 0)
                  (setcdr bounds (point)))))
            (if (not (equal name (symbol-name id)))
                (progn
                  (delete-region (car bounds) (cdr bounds))
                  (goto-char (point-max))
                  (insert (concat "(provide " "'" name ")"
                                  "\n"
                                  footer-end)))
              (forward-sexp 1)
              (unless (looking-at (concat "\n" (regexp-quote footer-end)))
                (delete-region (car bounds) (cdr bounds))
                (goto-char (point-max))
                (insert (concat (prin1-to-string l)
                                "\n"
                                footer-end)))))
        (goto-char (point-max))
        (insert (if (looking-back "\n" 0) "" "\n")
                (concat "(provide " "'" name ")" "\n" footer-end))))))

;;;###autoload
(defun autofix ()
  "Apply all code and comments fixes.

Code fixes includes auto adding autoload comments before all interactive
commmands and removing unused (declare-function ...) forms.

Comments fixes includes fixes for file headers, package headers, footer etc."
  (interactive)
  (autofix-autoloads)
  (autofix-header)
  (autofix-footer)
  (autofix-remove-unused-declarations))

;;;###autoload
(define-minor-mode autofix-mode
  "Runs `autofix' on file save when this mode is turned on."
  :lighter " autofix"
  :global nil
  (if (and autofix-mode
           (or (null buffer-file-name)
               (null (member (file-name-base
                              buffer-file-name)
                             autofix-ignored-file-patterns))))
      (add-hook 'before-save-hook 'autofix nil 'local)
    (remove-hook 'before-save-hook 'autofix 'local)))

(provide 'autofix)
;;; autofix.el ends here