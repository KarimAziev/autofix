;;; autofix.el --- Automatically fix common code style issues in Elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/autofix
;; Keywords: convenience, docs
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1") (package-lint "0.19"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Automatically fix common code style issues in Elisp

;; Usage

;; M-x `autofix-mode'
;; Runs `autofix' on file save when this mode is turned on.

;;; Minor mode

;; `autofix-mode'
;;      Runs `autofix' on file save when this mode is turned on.

;;; Keymaps

;; `autofix-multi-completion-map'

;;; Commands

;; M-x `autofix'
;;      Apply all functions from `autofix-functions'.
;;      Default code fixes includes auto adding auto load comments before all
;;      interactive commands and removing unused (declare-function ...) forms.
;;      Default comments fixes includes fixes for file headers,
;;      package headers, footer etc.
;;      File headers fixes can be customized via `autofix-header-functions'.

;; M-x `autofix-footer'
;;      Add of fix file footer (provide ='filename) with comment ends here.

;; M-x `autofix-remove-unused-declarations'
;;      Remove unused declared functions.

;; M-x `autofix-header'
;;      Apply all functions from `autofix-header-functions'.

;; M-x `autofix-autoloads'
;;      Add autoload comments before all interactive functions in buffer.

;; M-x `autofix-commentary'
;;      Add Commentary section in current buffer if none.

;; M-x `autofix-code-comment'
;;      Add Code comment to the end of header block.

;; M-x `autofix-header-first-line'
;;      Fix or create the first comment line in the header.

;; M-x `autofix-sharpquotes' (&optional no-prompt)
;;      Add sharpquotes according to `autofix-sharpquote-symbols-spec'.
;;      With optional argument NO-PROMPT replace occurrences without prompting.
;;      For example, such code:
;;      (mapcar \='car \='((a . 2) (b . 2) (c . 3)))
;;      Transforms to:
;;      (mapcar #\='car \='((a . 2) (b . 2) (c . 3))).
;;      To customize this behavior see variable `autofix-sharpquote-symbols-spec'.

;; M-x `autofix-scan-extract-all-docs'
;;      Return string with all docs in all buffer.
;;      If called interactively also copies it.

;; M-x `autofix-annotate-buffer'
;;      Add annotatations in header comment section.
;;      Annotations includes commands, custom variables.

;; M-x `autofix-url'
;;      Return string with generated url.

;; M-x `autofix-copyright'
;;      Prompt and fix or make new copyright.

;; M-x `autofix-license'
;;      Insert SPDX-License-Identifier if none.

;; M-x `autofix-add-fbound'
;;      Wrap function call in fbound.

;; M-x `autofix--throw-done'
;;      Throw to the catch for done and return nil from it.

;; M-x `autofix-keywords' (&optional force)
;;      Add or fix package keywords.
;;      With optional argument FORCE regenerate them even if valid.

;; M-x `autofix-header-body-comment'
;;      Add additional comments after package headers.

;; M-x `autofix-package-requires'
;;      Add or fix package requires section.

;; M-x `autofix-version'
;;      Add or fix package version.

;; M-x `autofix-update-version'
;;      Update or add new package version.

;; M-x `autofix-author'
;;      Add current user as new author to existing or new author section.


;;; Code:

(require 'transient)

(require 'subr-x)
(require 'package-lint)

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


(defcustom autofix-sharpquote-symbols-spec '(((mapconcat
                                               mapc
                                               mapcan
                                               funcall
                                               mapcar
                                               seq-map
                                               seq-map-indexed
                                               seq-mapcat
                                               seq-mapn
                                               seq-take-while
                                               seq-sort-by
                                               seq-sort
                                               seq-reduce
                                               seq-remove
                                               seq-drop-while
                                               seq-some
                                               seq-find
                                               seq-filter
                                               seq-do
                                               seq-do-indexed
                                               apply-on-rectangle
                                               apply-partially
                                               cl-assoc-if
                                               callf
                                               cl-callf
                                               transient-setup
                                               call-interactively
                                               apply
                                               cancel-function-timers
                                               funcall-interactively)
                                              . 1)
                                             ((add-hook
                                               remove-hook
                                               defalias
                                               local-set-key
                                               run-hook-wrapped
                                               global-set-key advice-remove
                                               seq-sort-by)
                                              . 2)
                                             ((run-with-idle-timer define-key
                                               advice-add
                                               run-with-timer
                                               run-at-time)
                                              . 3))
  "Alist of symbols and required sharpquote positions.

Each element is a cons which car is either symbol or list of symbols,
which cdr is a position of children element, that should be sharquoted."
  :group 'autofix
  :type '(alist
          :key-type (radio :tag "Symbol or symbols"
                     (symbol :tag "Symbol")
                     (repeat :tag "Symbols" symbol))
          :value-type integer))

(defcustom autofix-header-functions '(autofix-header-first-line
                                      autofix-copyright
                                      autofix-author
                                      autofix-url
                                      autofix-version
                                      autofix-keywords
                                      autofix-package-requires
                                      autofix-header-body-comment
                                      autofix-commentary
                                      autofix-code-comment
                                      autofix-license)
  "List of functions to fix header section:

`autofix-header-first-line' Fix or create the first comment line in the header.
`autofix-copyright' Prompt and fix or make new copyright.
`autofix-author' Add or fix author.
`autofix-url' Add of fix URL.
`autofix-version'Add or fix package version.
`autofix-keywords' Add or fix package keywords.
`autofix-package-requires' Add or fix package requires section.
`autofix-header-body-comment' Add `autofix-comment-section-body'.
`autofix-commentary' Add Commentary section in current buffer if none.
`autofix-code-comment' Add Code comment to the end of header block.
`autofix-license' Add SPDX short-form identifier."
  :group 'autofix
  :type '(repeat
          (radio
           (function-item :tag "First line" autofix-header-first-line)
           (function-item :tag "Copyright " autofix-copyright)
           (function-item :tag "Author" autofix-author)
           (function-item :tag "URL" autofix-url)
           (function-item :tag "Version" autofix-version)
           (function-item :tag "Keywords" autofix-keywords)
           (function-item :tag "Package requires" autofix-package-requires)
           (function-item :tag "Comments (`autofix-comment-section-body') "
                          autofix-header-body-comment)
           (function-item :tag "Commentary" autofix-commentary)
           (function-item :tag "Code" autofix-code-comment)
           (function-item :tag "SPDX-License-Identifier" autofix-license)
           (function :tag "Custom function"))))
(make-variable-buffer-local 'autofix-header-functions)


(defcustom autofix-spdx-license "GPL-3.0-or-later"
  "Default SPDX short-form identifier."
  :group 'autofix
  :type `(radio
          (const :tag "GNU General Public License v3.0" "GPL-3.0-or-later")
          (const :tag "MIT License" "MIT")
          (string :tag "Other")
          (const  :tag "None" nil)))



(defcustom autofix-functions '(autofix-autoloads
                               autofix-header
                               autofix-footer
                               autofix-function-name-quoting
                               autofix-remove-unused-declarations)
  "List of functions to apply with command `autofix'.

`autofix-autoloads'
`autofix-header'
`autofix-footer'
`autofix-function-name-quoting'
`autofix-remove-unused-declarations'"
  :group 'autofix
  :type '(repeat
          (choice
           (function-item :tag "Autoloads" autofix-autoloads)
           (function-item :tag "Header" autofix-header)
           (function-item :tag "Footer" autofix-footer)
           (function-item :tag "Quoting function names"
                          autofix-function-name-quoting)
           (function-item :tag "Remove unused declaration"
                          autofix-remove-unused-declarations)
           (function :tag "Custom function"))))

(make-variable-buffer-local 'autofix-functions)

(defvar autofix-package-headers '("Author"
                                  "Maintainer"
                                  "Created"
                                  "Version"
                                  "Keywords"
                                  "URL"
                                  "Homepage"
                                  "Package-Version"
                                  "Version"
                                  "Authors"
                                  "Package-Requires"
                                  "SPDX-License-Identifier"))

(defvar autofix-package-header-re  (concat "^;;[\s]" (regexp-opt
                                                      autofix-package-headers)
                                           ":"))
(defvar autofix-package-header-re-with-value
  (concat
   autofix-package-header-re
   "\\(\\([^\n]*\\)\n\\(;;[\s][\s]+\\([^\n]+\\)[\n]\\)*\\)"))

(defun autofix-detect-user-email ()
  "Guess the user's email address. Return nil if none could be found."
  (when-let* ((mail (or (shell-command-to-string
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
                 (const :tag "Auto-detect" autofix-detect-user-email)
                 (function :tag "Custom function"))
  :group 'autofix)

(defun autofix-string-to-undescore (str)
  "Transform STR to underscored."
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
  "Detect user full name from git config, email or function `user-full-name'."
  (when-let* ((str (or
                   (shell-command-to-string "git config --get user.name")
                   (autofix-get-user-email)
                   (user-full-name))))
    (mapconcat #'capitalize
               (split-string
                (autofix-string-to-undescore
                 (replace-regexp-in-string
                  "@.+" ""
                  (string-trim str)))
                "[^a-z]" t)
               "\s")))

(defcustom autofix-user-fullname 'user-full-name
  "User email to add in header section."
  :type '(choice :tag "User email"
                 (string :tag "Type your full name")
                 (const :tag "Auto-detect"
                        autofix-detect-user-full-name)
                 (function :tag "Use value from `user-full-name'"
                           user-full-name)
                 (function :tag "Custom function"))
  :group 'autofix)

(defcustom autofix-ignored-file-patterns '("init.el"
                                           "custom.el"
                                           "early-init.el"
                                           ".dir-locals.el")
  "List of file name bases to ignore."
  :type '(repeat (regexp :tag "Regexp"))
  :group 'autofix)

(defcustom autofix-extra-transient-suffixes '(("i" "Include undefined"
                                               elisp-bundle-include-undefined
                                               :if
                                               (lambda
                                                 ()
                                                 (require 'elisp-bundle
                                                  nil
                                                  t)))
                                              ("o" "generate docs"
                                               org-autodoc-async
                                               :if
                                               (lambda
                                                 ()
                                                 (require 'org-autodoc
                                                  nil
                                                  t))))
  "List of additional transient suffixes for autofix commands.

A list of additional transient suffixes to be included in the autofix menu. Each
element in the list is itself a list, specifying a transient suffix.

The elements of the inner list are as follows:

1. A single-character string that acts as the key for the transient suffix.

2. A string, function, or sexp that provides a description for the transient
suffix. If a function is provided, it should return a string when called.

3. A function that will be called when the transient suffix is invoked.

4. An optional list of conditions that determine when the transient suffix is
inappropriate, based on the current major mode. Each condition is a list
containing a keyword specifying the type of inappropriateness and the relevant
major mode symbol.

5. An optional list of conditions that determine when the transient suffix is
appropriate, based on the current major mode. Each condition is a list
containing a keyword specifying the type of appropriateness and the relevant
major mode symbol.

6. An optional list of conditions based on variable values that determine when
the transient suffix is appropriate. Each condition is a list containing a
keyword specifying the type of appropriateness and the variable to check.

7. An optional list of conditions based on variable values that determine when
the transient suffix is inappropriate. Each condition is a list containing a
keyword specifying the type of inappropriateness and the variable to check.

8. An optional list of conditions that determine when the transient suffix is
appropriate, based on the evaluation of a predicate. Each condition is a list
containing a keyword specifying the type of appropriateness and the predicate to
evaluate.

9. An optional list of conditions that determine when the transient suffix is
inappropriate, based on the evaluation of a predicate. Each condition is a list
containing a keyword specifying the type of inappropriateness and the predicate
to evaluate.

The default value is a list of two example suffixes, one for including undefined
elisp symbols and another for generating documentation asynchronously. These
examples include the necessary functions and conditions for their inclusion in
the autofix menu."
  :group 'autofix
  :type `(repeat
          (list
           :tag "Suffix"
           (string :tag "Key")
           (choice
            (string :tag "Description")
            (function :tag "Description Function")
            (sexp :tag "Description sexp"))
           (function :tag "Command")
           (repeat
            :tag "Inapt by modes"
            :inline t
            (list
             :tag "Inapt by modes"
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":inapt-if-mode"
               :doc
               "Inapt if major-mode matches value."
               :inapt-if-mode)
              (const
               :format "%v %d"
               :tag ":inapt-if-not-mode"
               :doc
               "Inapt if major-mode does not match value."
               :inapt-if-not-mode)
              (const
               :format "%v %d"
               :tag ":inapt-if-derived"
               :doc
               "Inapt if major-mode derives from value."
               :inapt-if-derived)
              (const
               :format "%v %d"
               :tag ":inapt-if-not-derived"
               :doc
               "Inapt if major-mode does not derive from value."
               :inapt-if-not-derived))
             (symbol
              :completions
              (lambda (string pred action)
                (let ((completion-ignore-case t))
                 (complete-with-action action
                  (remove 't
                   (seq-uniq
                    (seq-filter
                     #'symbolp
                     (flatten-list
                      auto-mode-alist))))
                  string pred))))))
           (repeat
            :tag "If mode"
            :inline t
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if-mode"
               :doc
               "Enable if major-mode matches value."
               :if-mode)
              (const
               :format "%v %d"
               :tag ":if-not-mode"
               :doc
               "Enable if major-mode does not match value."
               :if-not-mode)
              (const
               :format "%v %d"
               :tag ":if-derived"
               :doc
               "Enable if major-mode derives from value."
               :if-derived)
              (const
               :format "%v %d"
               :tag ":if-not-derived"
               :doc
               "Enable if major-mode does not derive from value."
               :if-not-derived))
             (symbol :completions
              (lambda (string pred action)
                (let ((completion-ignore-case t))
                 (complete-with-action action
                  (remove 't
                   (seq-uniq
                    (seq-filter
                     #'symbolp
                     (flatten-list
                      auto-mode-alist))))
                  string pred))))))
           (repeat
            :inline t
            :tag "If variable"
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if-non-nil"
               :doc
               "Enable if variable's value is non-nil."
               :if-non-nil)
              (const
               :format "%v %d"
               :tag ":if-nil"
               :doc "Enable if variable's value is nil."
               :if-nil))
             variable))
           (repeat
            :inline t
            :tag "Inapt if variable"
            (list
             :inline t
             (radio (const
                     :format "%v %d"
                     :tag ":inapt-if-non-nil"
                     :doc
                     "Inapt if variable's value is non-nil."
                     :inapt-if-non-nil)
              (const
               :format "%v %d"
               :tag ":inapt-if-nil"
               :doc
               "Inapt if variable's value is nil."
               :inapt-if-nil))
             variable))
           (repeat
            :tag "If"
            :inline t
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if"
               :doc "Enable if predicate returns non-nil."
               :if)
              (const
               :format "%v %d"
               :tag ":if-not"
               :doc "Enable if predicate returns nil."
               :if-not)
              (symbol :tag "other"))
             (choice (function :tag "Function")
              (symbol :tag "Symbol")
              (sexp :tag "Sexp"))))
           (repeat
            :tag "Inapt if "
            :inline t
            (list
             :inline t
             (radio (const
                     :format "%v %d"
                     :tag ":inapt-if"
                     :doc
                     "Inapt if predicate returns non-nil."
                     :inapt-if)
              (const
               :format "%v %d"
               :tag ":inapt-if-not"
               :doc
               "Inapt if predicate returns nil."
               :inapt-if-not)
              (symbol :tag "other"))
             (choice (function :tag "Function")
              (symbol :tag "Symbol")
              (sexp :tag "Sexp")))))))

(defun autofix-overlay-add-props (overlay props)
  "Add plist PROPS to OVERLAY."
  (dotimes (idx (length props))
    (when (eq (logand idx 1) 0)
      (let* ((prop-name (nth idx props))
             (val (plist-get props prop-name)))
        (overlay-put overlay prop-name val))))
  overlay)

(defun autofix-overlay-prompt-region (beg end overlay-props fn &rest args)
  "Highlight region from BEG to END while invoking FN with ARGS.
OVERLAY-PROPS is a plist to add in overlay."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn
          (goto-char beg)
          (autofix-overlay-add-props overlay overlay-props)
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
  (when-let* ((author
              (delete
               nil
               (list (autofix-get-user-fullname)
                     (when-let* ((email (autofix-get-user-email)))
                       (format "<%s>" email))))))
    (string-join author "\s")))

(defun autofix-jump-to-header-end ()
  "Jump to end of line of the header comment section."
  (let ((buff-str (buffer-string))
        (buff (current-buffer)))
    (when-let* ((pos
                (with-temp-buffer
                  (insert buff-str)
                  (delay-mode-hooks
                    (emacs-lisp-mode)
                    (goto-char (point-min))
                    (when-let* ((form-start
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
                                    (progn (while (re-search-backward "^\\(;;;###\\)autoload" nil t))
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
  (when-let* ((max (save-excursion (autofix-jump-to-header-end))))
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
          (t (when-let* ((col
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
  (when-let* ((annotation (autofix-author-annotation)))
    (if-let*
        ((author-header
          (autofix-header-get-regexp-info
           "^;;[\s]Author:\\(\\([^\n]*\\)\n\\(;;[\s][\s]+\\([^\n]+\\)[\n]\\)*\\)")))
        (unless (string-match-p (autofix-get-user-email)
                                (car author-header))
          (let ((beg (nth 1 author-header))
                (end (nth 2 author-header))
                (rep (autofix-add-author-str (car author-header) annotation)))
            (let ((overlay (apply #'make-overlay (cdr author-header)))
                  (confirmed))
              (unwind-protect
                  (progn
                    (overlay-put overlay 'face 'error)
                    (goto-char beg)
                    (overlay-put overlay 'after-string
                                 (propertize rep 'face 'success))
                    (setq confirmed (yes-or-no-p "Replace?")))
                (delete-overlay overlay))
              (when confirmed
                (replace-region-contents beg end (lambda () rep))))))
      (when (autofix-jump-to-package-header-end)
        (insert
         (concat (if (looking-back "\n\n" 0) "" "\n")
                 ";; Author:\s" annotation "\n"
                 (if
                     (or
                      (looking-at
                       autofix-package-header-re)
                      (looking-at
                       "\n"))
                     ""
                   "\n")))))))

(defun autofix-get-current-version ()
  "Return package header version as string."
  (when-let* ((info (autofix-header-get-regexp-info
                    ";;[\s\t]+Version:\\([^\n]+\\)?")))
    (let* ((current-version (string-trim
                             (match-string-no-properties 1))))
      (unless (string-empty-p current-version)
        current-version))))

;;;###autoload
(defun autofix-update-version ()
  "Update or add new package version."
  (interactive)
  (if-let* ((info (autofix-header-get-regexp-info
                  ";;[\s]Version:\\([^\n]+\\)?")))
      (let* ((beg (nth 1 info))
             (end (nth 2 info))
             (current-version (autofix-get-current-version))
             (rep (autofix-read-header-string
                   ";; Version: (empty if none)"
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

;;;###autoload
(defun autofix-version ()
  "Add or fix package version."
  (interactive)
  (unless (autofix-get-current-version)
    (autofix-update-version)))

(defun autofix-get-emacs-version ()
  "Return suitable Emacs version for current package."
  (when-let* ((l
              (last
               (seq-sort-by
                #'cadr
                #'version<
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
                   (ignore-errors (package-lint--check-all)))))))))
    (mapcar (lambda (it) (setcar it (intern (car it)))
              it)
            l)))

(defun autofix-elisp-find-symbol-library (sym)
  "Find symbol SYM and return cons where head is buffer and cdr is position."
  (require 'find-func)
  (when (stringp sym)
    (setq sym (intern sym)))
  (let ((found nil)
        (types
         '(lib nil defvar))
        (type))
    (while (setq type
                 (unless found (pop types)))
      (when-let* ((buff (or
                        (ignore-errors
                          (pcase type
                            ('lib
                             (when-let* ((file
                                         (find-library-name
                                          (prin1-to-string
                                           sym))))
                               (find-file-noselect file)))
                            (_ (car (find-definition-noselect
                                     sym type))))))))
        (setq found (with-current-buffer buff
                      (when-let* ((name (package-lint--provided-feature))
                                 (version (or
                                           (ignore-errors
                                             (replace-regexp-in-string
                                              "^[^0-9]+"
                                              ""
                                              (car (process-lines
                                                    "git"
                                                    "describe"
                                                    "--abbrev=0"
                                                    "--tags"))))
                                           (lm-version))))
                        (unless (string-empty-p version)
                          (list (intern name)
                                version)))))))
    (when (assq (car found) package-archive-contents)
      found)))

(defun autofix-format-sexp-to-require (sexp)
  "Return string with package name if SEXP is valid require call.
If package is optional, also add suffix (optional)."
  (pcase sexp
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote))))
     (cons (autofix-unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_)
     (cons (autofix-unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_
               ,(and optional (guard (not (eq optional nil)))))
     (cons (autofix-unquote name) t))))

(defmacro autofix-with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
     (progn
       ,@body)))

(defun autofix-parse-require ()
  "Parse and format s-expression to require statement."
  (when-let* ((sexp
              (unless (or (nth 4 (syntax-ppss (point)))
                          (nth 3 (syntax-ppss (point))))
                (sexp-at-point))))
    (when (listp sexp)
      (autofix-format-sexp-to-require sexp))))

(defun autofix-elisp-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (when-let* ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count (if (> n 0) n (- n))))
      (while
          (and (not (= count 0))
               (when-let* ((end (ignore-errors
                                 (funcall fn (if
                                                 (> n 0) 1
                                               -1))
                                 (point))))
                 (unless (or (= end
                                (or pos init-pos))
                             (nth 4 (syntax-ppss (point)))
                             (and (looking-at ";")
                                  (nth 4 (syntax-ppss (1+ (point))))))
                   (setq pos end))))
        (setq count
              (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun autofix-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (autofix-elisp-move-with 'backward-list n))

(defun autofix-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (autofix-elisp-move-with 'backward-up-list arg))

(defun autofix-get-require-calls (&optional top-level)
  "Return list of required libs in current buffer.
If TOP-LEVEL is non nil, return only top-levels calls."
  (let ((requires '())
        (deps))
    (save-excursion
      (goto-char (point-max))
      (while (autofix-backward-list)
        (when-let* ((sexp
                    (unless (nth 4 (syntax-ppss (point)))
                      (list-at-point))))
          (if-let* ((dep (autofix-format-sexp-to-require sexp)))
              (push dep requires)
            (when (and (not top-level)
                       (listp sexp))
              (push sexp deps))))))
    (when deps
      (autofix-with-temp-lisp-buffer
          (insert (prin1-to-string deps))
          (while (re-search-backward "[(]require[\s\t\n\r\f]+'" nil t 1)
            (when-let* ((found (autofix-parse-require)))
              (unless (member found requires)
                (push found requires))))))
    requires))

(defun autofix-get-required-libs ()
  "Return alist of required packages and versions."
  (delq nil (mapcar #'autofix-elisp-find-symbol-library
                    (mapcar #'car (seq-remove
                                   #'cdr
                                   (autofix-get-require-calls t))))))

(defun autofix-add-package-require-lib (entry)
  "Add ENTRY of form (symbol \"version\") to package requires section."
  (when-let* ((required entry))
    (let* ((info (autofix-header-get-regexp-info
                  ";;[\s]Package-Requires:\\([^\n]+\\)?"))
           (str
            (when info (match-string-no-properties 1)))
           (curr-requires
            (when str (car (read-from-string str))))
           (result (mapcar (lambda (it)
                             (if-let* ((repl
                                       (seq-find (lambda (c)
                                                   (eq
                                                    (car it)
                                                    (car c)))
                                                 required)))
                                 (cons (car it)
                                       (cdr repl))
                               it))
                           curr-requires))
           (rep))
      (setq result (seq-uniq (append result required)
                             (lambda (a b)
                               (eq (car a)
                                   (car b)))))
      (setq rep
            (unless (equal curr-requires result)
              (concat
               ";; Package-Requires: " (prin1-to-string result))))
      (when rep
        (if info
            (replace-region-contents
             (nth 1 info)
             (nth 2 info)
             (lambda () rep))
          (autofix-jump-to-package-header-end)
          (insert
           (concat
            (if
                (and
                 (looking-back "\n" 0)
                 (save-excursion
                   (forward-line -1)
                   (looking-at
                    autofix-package-header-re-with-value)))
                ""
              "\n")
            rep
            (if (looking-at
                 "\n\n")
                ""
              "\n"))))))))


;;;###autoload
(defun autofix-package-requires ()
  "Add or fix package requires section."
  (interactive)
  (when-let* ((required (delq nil
                             (append (autofix-get-emacs-version)
                                     (autofix-get-required-libs)))))
    (autofix-add-package-require-lib required)))

(defun autofix-jump-to-package-header-end ()
  "Jump to the end of package header end."
  (when-let* ((start (autofix-jump-to-package-header-start))
              (end (save-excursion
                     (autofix-jump-to-header-end)
                     (re-search-backward "^;;; Commentary:"
                                         nil t 1)
                     (max start (point)))))
    (while (re-search-forward
            autofix-package-header-re-with-value
            end t 1)
      (setq start (point)))
    (when (looking-at ";")
      (end-of-line)
      (newline))
    (point)))

(defun autofix-confirm-and-replace-region (beg end replacement)
  "Replace region between BEG and END with REPLACEMENT.
REPLACEMENT should be a string, or a function that returns string.
It will be called without arguments."
  (when-let* ((overlay (make-overlay beg end))
             (rep (if (functionp replacement)
                      (funcall replacement)
                    replacement)))
    (when (unwind-protect
              (progn (overlay-put overlay 'face 'error)
                     (overlay-put overlay 'after-string
                                  (concat
                                   "\s"
                                   (propertize rep
                                               'face 'success)))
                     (yes-or-no-p "Replace region?"))
            (delete-overlay overlay))
      (when (fboundp 'replace-region-contents)
        (if (equal beg end)
            (insert rep)
          (replace-region-contents beg end (lambda () rep))))
      rep)))

;;;###autoload
(defun autofix-header-body-comment ()
  "Add additional comments after package headers.
Default value is comment starting with \"This file is NOT part of
GNU Emacs...\"),
To change the value customize the variable `autofix-comment-section-body'."
  (interactive)
  (when autofix-comment-section-body
    (autofix-jump-to-package-header-end)
    (unless (re-search-forward autofix-comment-section-body nil t 1)
      (if-let* ((start (point))
               (commentary-start
                (save-excursion
                  (when (autofix-jump-to-header-end)
                    (re-search-backward "^[;]+[\s\t]+Commentary:[\s\t]*\n"
                                        nil
                                        t 1)))))
          (autofix-confirm-and-replace-region
           start commentary-start
           (concat
            "\n"
            (string-trim-left
             autofix-comment-section-body)))
        (insert (concat (if (looking-back "\n\n" 0)
                            "\n"
                          "\n\n")
                        autofix-comment-section-body))))))

;;;###autoload
(defun autofix-keywords (&optional force)
  "Add or fix package keywords.
With optional argument FORCE regenerate them even if valid."
  (interactive "P")
  (if-let* ((info (autofix-header-get-regexp-info
                  ";;[\s]Keywords:\\([^\n]+\\)?")))
      (when force
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
                                                                      ",\s"))))
            (delete-region beg end))))
    (when-let* ((keywords (when (autofix-jump-to-package-header-start)
                           (autofix-read-keyword))))
      (autofix-jump-to-package-header-end)
      (insert (if (looking-back "\n" 0) "" "\n")
              ";; Keywords: "
              (string-join keywords "\s")
              (if (or (looking-at autofix-package-header-re)
                      (looking-at "\n"))
                  "" "\n")
              "\n"))))

(defun autofix-jump-to-package-header-start ()
  "Jump to the start of package header section or to place for new."
  (goto-char (point-min))
  (while (and (looking-at ";;;\\|\\(;;[\s]Copyright[\s]\\)\\|\n;")
              (not (looking-at ";;;###autoload")))
    (forward-line 1))
  (when (looking-at ";;;###autoload")
    (forward-line -1))
  (point))

(defvar finder-known-keywords)


(defun autofix--throw-done ()
  "Throw to the catch for done and return nil from it."
  (interactive)
  (throw 'done nil))

(defvar autofix-multi-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                #'autofix--throw-done)
    (define-key map (kbd "C-M-j")
                #'autofix--throw-done)
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
\\[autofix--throw-done]' to finish)\s")
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
         (replace-regexp-in-string "\\.git$" "" (or (autofix-ssh-to-https url)
                                                    url)))
       (seq-uniq (mapcar (lambda (l)
                           (nth 1 (split-string l nil t)))
                         (split-string (cdr cell) "\n" t)))))))

(defun autofix-goto-last-form ()
  "Jump to last Lisp form in buffer."
  (goto-char (point-max))
  (when-let* ((pos
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
    (when (and (looking-at ";;;")
               (not (looking-at ";;;###autoload")))
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
          (seq-remove #'string-empty-p (mapcar
                                        #'string-trim
                                        (delete nil
                                                (list
                                                 filename description
                                                 bindings)))))))))

(defun autofix-guess-feature-name ()
  "Return file name base from current file or buffer."
  (file-name-base (or (buffer-file-name) (format "%s" (current-buffer)))))

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


(defvar autofix-docstring-positions
  (list (cons 'defun 3)
        (cons 'defmacro 3)
        (cons 'defsubst 3)
        (cons 'defalias 4)
        (cons 'defhydra 3)
        (cons 'transient-define-prefix 3)
        (cons 'transient-define-suffix 3)
        (cons 'transient-define-argument 3)
        (cons 'transient-define-infix 3)
        (cons 'cl-defun 3)
        (cons 'cl-defsubst 3)
        (cons 'cl-defmacro 3)
        (cons 'cl-defgeneric 3)
        (cons 'cl-defmethod 3)
        (cons 'define-minor-mode 2)
        (cons 'define-derived-mode 4)
        (cons 'define-generic-mode 8)
        (cons 'define-compilation-mode 3)
        (cons 'easy-mmode-define-minor-mode 2)))


(defun autofix-jump-to-defun-body ()
  "Jump to the body start in the current function."
  (let ((found nil))
    (when-let* ((start (car-safe (nth 9 (syntax-ppss (point))))))
      (goto-char start)
      (when-let* ((sexp (sexp-at-point))
                  (sexp-type (car-safe sexp)))
        (not (setq found (and (or
                               (eq sexp-type 'defun)
                               (eq sexp-type 'cl-defun))
                              (and (listp sexp)
                                   (symbolp (nth 1 sexp))
                                   (listp (nth 2 sexp))))))
        (when found
          (down-list)
          (forward-sexp 3)
          (skip-chars-forward "\s\t\n")
          (when (looking-at "\"")
            (forward-sexp 1)
            (skip-chars-forward "\s\t\n"))
          (when (looking-at "[(]interactive[^a-z]")
            (forward-sexp 1)
            (skip-chars-forward "\s\t\n"))
          (point))))))

;;;###autoload
(defun autofix-add-fbound ()
  "Wrap function call in fbound."
  (interactive)
  (require 'flymake)
  (require 'elisp-bundle)
  (when (bound-and-true-p flymake-mode)
    (when-let* ((diag (car (flymake-diagnostics (point)
                                                (line-end-position))))
                (text (when (fboundp 'flymake-diagnostic-text)
                        (flymake-diagnostic-text diag)))
                (beg (when (fboundp 'flymake-diagnostic-beg)
                       (flymake-diagnostic-beg diag)))
                (name (with-temp-buffer   (insert text)
                                          (when (re-search-backward
                                                 "‘\\([^’]+\\)’"
                                                 nil t 1)
                                            (match-string-no-properties 1)))))
      (let ((buffer (when (fboundp 'flymake-diagnostic-buffer)
                      (flymake-diagnostic-buffer diag))))
        (cond ((string-match-p
                "the function ‘\\([^’]+\\)’ might not be defined at runtime"
                text)
               (with-current-buffer buffer
                 (when-let* ((start (autofix-backward-up-list))
                             (end (progn (forward-sexp 1)
                                         (point)))
                             (content (buffer-substring-no-properties
                                       start end)))
                   (delete-region start end)
                   (insert (format "(when (fboundp '%s))"
                                   name))
                   (forward-char -1)
                   (newline-and-indent)
                   (insert content))))
              ((string-match-p
                "the function ‘\\([^’]+\\)’ is not known to be defined"
                text)
               (with-current-buffer buffer
                 (when-let* ((start (autofix-backward-up-list))
                             (end (progn (forward-sexp 1)
                                         (point)))
                             (content (buffer-substring-no-properties
                                       start end)))
                   (delete-region start end)
                   (insert (format "(when (fboundp '%s))"
                                   name))
                   (forward-char -1)
                   (newline-and-indent)
                   (insert content)
                   (when-let* ((lib
                               (ignore-errors
                                 (with-current-buffer (car-safe (find-definition-noselect
                                                                 (intern
                                                                  name)
                                                                 nil))
                                   (when (fboundp
                                          'elisp-bundle--provided-feature)
                                     (elisp-bundle--provided-feature))))))
                     (beginning-of-defun)
                     (let ((libs (mapcar #'symbol-name
                                         (autofix-get-requires-from-sexp
                                          (sexp-at-point)))))
                       (message "libs %s" libs)
                       (unless (member lib libs)
                         (forward-char 1)
                         (autofix-jump-to-defun-body)
                         (insert (format "(require '%s)" lib))
                         (newline-and-indent))))))))))))

(defun autofix-get-requires-from-sexp (sexp)
  "Return required symbols from SEXP."
  (let ((requires))
    (while (and (listp sexp)
                (setq sexp (memq 'require (flatten-list
                                           sexp))))
      (pop sexp)
      (when (and (eq (car-safe sexp) 'quote)
                 (symbolp (nth 1 sexp)))
        (push (nth 1 sexp) requires)))
    requires))

(defvar autofix-group-annotation-alist
  '((:define-derived-mode . "Major mode")
    (:define-generic-mode . "Major mode")
    (:define-compilation-mode . "Compilation mode")
    (:easy-mmode-define-minor-mode . "Minor mode")
    (:define-minor-mode . "Minor mode")
    (:define-generic-mode . "Generic mode")
    (:keymap . "Keymaps")
    (:defhydra . "Hydras")
    (:use-package . "Used Packages")
    (:interactive . "Commands")
    (:defcustom . "Customization")
    (:defun . "Functions")
    (:defalias . "Functions")
    (:cl-defun . "Functions")
    (:defmacro . "Macros")
    (:cl-defmacro . "Macros")
    (:defvar . "Variables")
    (:defvar-local . "Variables")
    (:cl-defmethod . "Method")
    (:cl-defstruct . "Structs")
    (:defsubst . "Inline Functions")
    (:cl-defsubst . "Inline Functions")))


(defun autofix-symbol-keymapp (sym)
  "Return t if value of symbol SYM is a keymap."
  (when-let* ((val (when (boundp sym)
                    (symbol-value sym))))
    (keymapp val)))

(defun autofix-symbol-sexp-keymapp (sexp)
  "Return t if SEXP look like keymap variable."
  (when-let* ((value (nth 2 sexp))
              (vals (and (listp value)
                         (symbolp (car value))
                         (memq (car value) '(let let*))
                         (car (seq-drop value 1)))))
    (when (and (listp vals)
               (listp (car vals)))
      (seq-find (lambda (it)
                (when-let* ((val (and (listp (cdr it))
                                     (listp (cadr it))
                                     (cadr it))))
                  (and
                   (= 1 (length val))
                   (symbolp (car val))
                   (memq (car val) '(make-sparse-keymap)))))
              vals))))

(defun autofix-parse-list-at-point ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"autofix-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                      (list-at-point)))
              (type (car sexp))
              (id (autofix-unquote (when (symbolp (nth 1 sexp))
                                     (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc (when-let* ((pos (cdr (assq type autofix-docstring-positions)))
                           (el (nth pos sexp)))
                 (when (stringp el)
                   el)))
          (args (and (autofix-function-p type)
                     (nth 2 sexp))))
      (list name args doc
            (cond ((and (autofix-function-p type)
                        (or (and
                             (nth 3 sexp)
                             (listp (nth 3 sexp))
                             (symbolp (car (nth 3 sexp)))
                             (eq 'interactive (car (nth 3 sexp))))
                            (and
                             (nth 4 sexp)
                             (listp (nth 4 sexp))
                             (symbolp (car (nth 4 sexp)))
                             (eq 'interactive (car (nth 4 sexp))))))
                   'interactive)
                  ((or (autofix-symbol-keymapp id)
                       (autofix-symbol-sexp-keymapp sexp))
                   'keymap)
                  (t type))))))

(defun autofix-annotate-with (prefix fn)
  "Return string of grouped annotations.

Each group is prefixed with PREFIX, and consists of
results of calling FN with list of (symbol-name args doc deftype)."
  (when-let* ((items (autofix-scan-buffer)))
    (let ((blocks))
      (dolist (key (mapcar #'car autofix-group-annotation-alist))
        (when-let* ((title (alist-get key autofix-group-annotation-alist))
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
  (let ((name
         (pcase mode
           ('org-mode (format "+ ~%s~" (car item-list)))
           (_ (format (if (member 'interactive item-list)
                          ";; M-x `%s'"
                        ";; `%s'")
                      (car item-list)))))
        (args
         (when (nth 1 item-list)
           (format " %s" (nth 1 item-list))))
        (doc
         (when (nth 2 item-list)
           (format "\n%s"
                   (mapconcat
                    (apply-partially
                     #'format
                     (pcase mode
                       ('org-mode "%s" )
                       (_ ";;      %s" )))
                    (split-string
                     (nth 2 item-list)
                     "[\n\"]"
                     t)
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
        (when-let* ((sexp (autofix-parse-list-at-point)))
          (let ((keyword (intern (concat ":" (symbol-name (car
                                                           (reverse sexp)))))))
            (if-let* ((group (plist-get pl keyword)))
                (setq pl (plist-put pl keyword (append group (list sexp))))
              (setq pl (plist-put pl keyword (list sexp)))))))
      pl)))

(defun autofix-annotate-as-comments (sexps)
  "Return string with generetad from SEXPS annotations as comments."
  (mapconcat
   #'autofix-annotate-list-item
   sexps "\n"))

(defun autofix-read-header-string (prompt default-value)
  "Read a non-empty string from the minibuffer with PROMPT and DEFAULT-VALUE."
  (let ((str (string-trim (read-string prompt default-value))))
    (unless (string-empty-p str)
      (concat prompt str))))


(defun autofix-ensure-autoload (&optional prompt-fn)
  "Ensure autoload comments are present or add them if missing.

Argument PROMPT-FN is an optional argument that represents a function or macro
used to prompt the user for input."
  (let ((l (autofix-parse-list-at-point)))
    (pcase l
      (`(,(and name (guard (stringp name))
               (guard (not (string-match-p "[-][-]" name))))
         ,_ ,_ interactive)
       (unless (looking-back ";;;###autoload[\s\t\n]+" 0)
         (when (or (not prompt-fn)
                   (funcall #'autofix-overlay-prompt-region
                            (point)
                            (point)
                            `(before-string ,(propertize ";;;###autoload\n"
                                                         'face
                                                         'success)
                                            face
                                            error)
                            prompt-fn))
           (insert ";;;###autoload\n"))
         t))
      (`(,(and name (guard (stringp name))
               (guard (not (string-match-p "[-][-]" name))))
         ,_ ,_ ,(or 'transient-define-prefix 'transient-define-suffix))
       (let ((rep (format
                   ";;;###autoload (autoload '%s %s nil t)"
                   (car l)
                   (prin1-to-string
                    (autofix-guess-feature-name)))))
         (when (save-excursion
                 (or (not (zerop (forward-line -1)))
                     (let ((beg (point))
                           (end))
                       (if (not (re-search-forward ";;;###autoload[\s\t]*"
                                                   (line-end-position)
                                                   t
                                                   1))
                           t
                         (when (or (not prompt-fn)
                                   (let ((curr))
                                     (setq end (line-end-position))
                                     (setq curr (buffer-substring-no-properties
                                                 beg
                                                 end))
                                     (and (not (string= curr rep))
                                          (funcall
                                           #'autofix-overlay-prompt-region
                                           beg
                                           end
                                           `(before-string
                                             ,(propertize rep
                                                          'face
                                                          'error)
                                             face
                                             error)
                                           prompt-fn))))
                           (replace-region-contents beg end
                                                    (lambda ()
                                                      rep)))
                         nil))))
           (when (or (not prompt-fn)
                     (funcall #'autofix-overlay-prompt-region
                              (point)
                              (point)
                              `(before-string ,(propertize (concat rep "\n")
                                                           'face
                                                           'success)
                                              face
                                              error)
                              prompt-fn))
             (insert (concat rep "\n")))))))))

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
    (when (re-search-forward
           "^;;[\s\t]+Copyright\\([\s\t]*\\(©\\|[(][Cc][)]\\)\\)"
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
(defun autofix-license ()
  "Insert SPDX-License-Identifier if none."
  (interactive)
  (unless (or (not autofix-spdx-license)
              (autofix-header-get-regexp-info
               "\\(;; SPDX-License-Identifier: \\([a-z][^\n]+\\)\\)"))
    (autofix-jump-to-package-header-end)
    (insert (concat ";; SPDX-License-Identifier: " autofix-spdx-license
                    (if (looking-at "[^\n]")
                        "\n\n"
                      (if (looking-at "\n\n") "" "\n"))))))

;;;###autoload
(defun autofix-copyright ()
  "Prompt and fix or make new copyright."
  (interactive)
  (when-let* ((author (autofix-author-annotation)))
    (if-let* ((current (autofix-get-current-copyright)))
        (save-excursion
          (goto-char (point-min))
          (unless (string-match-p
                   (regexp-quote author)
                   current)
            (when-let* ((line-end
                        (when (re-search-forward
                               "^;;[\s\t]+Copyright\\([\s\t]*\\(©\\|[(][Cc][)]\\)\\)"
                               nil t 1)
                          (line-end-position))))
              (when (yes-or-no-p
                     (format "Change copyright %s?"
                             (buffer-substring-no-properties
                              (point)
                              (line-end-position))))
                (let ((new-copyright (read-string "Copyright" author)))
                  (replace-region-contents (point) line-end
                                           (lambda ()
                                             (concat " "
                                                     new-copyright))))))))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (insert "\n" (autofix-make-copyright)
                (if (looking-at "[^\n]")
                    "\n\n"
                  (if (looking-at "\n\n") "" "\n")))))))

;;;###autoload
(defun autofix-url ()
  "Add package header with URL."
  (interactive)
  (when-let* ((remotes (autofix-repo-urls)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^;;[\s\t]+URL:" nil t 1)
          (let ((beg (point))
                (end (line-end-position)))
            (let ((curr-url (string-trim (buffer-substring-no-properties
                                          (point) end))))
              (when-let* ((remotes (autofix-repo-urls)))
                (unless (member curr-url remotes)
                  (autofix-overlay-prompt-region
                   beg end
                   '(face success)
                   (lambda ()
                     (let ((url (if (= 1 (length remotes))
                                    (read-string ";; URL: " (car remotes))
                                  (completing-read ";; URL:  " remotes))))
                       (replace-region-contents beg end (lambda ()
                                                          (concat
                                                           "\s"
                                                           url))))))))))
        (autofix-jump-to-package-header-end)
        (insert (if (looking-back "\n" 0) "" "\n")
                ";; URL: "
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
    (when-let* ((blocks (autofix-annotate-with
                        ";;; "
                        'autofix-annotate-as-comments)))
      (autofix-jump-to-header-end)
      (when (re-search-backward ";;; Code:" nil t 1)
        (let ((pos (point)))
          (dolist (title (mapcar #'cdr autofix-group-annotation-alist))
            (re-search-backward (concat "^;;;[\s\t]\\("
                                        (regexp-quote title) "\\)")
                                nil t 1))
          (when blocks
            (replace-region-contents
             pos (point)
             (lambda () blocks))))))))

(defun autofix-scan-top-all-top-forms ()
  "Parse all top level elisp forms with `autofix-parse-list-at-point'."
  (save-excursion
    (let ((l '()))
      (goto-char (point-max))
      (while (autofix-backward-list)
        (when-let* ((sexp (autofix-parse-list-at-point)))
          (push sexp l)))
      (nreverse l))))

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

(defun autofix-ensure-header-file (filename current-file)
  "Replace CURRENT-FILE header with FILENAME."
  (let* ((file-start (and
                      (point)))
         (file-end (and current-file
                        (re-search-forward
                         (regexp-quote current-file)
                         (line-end-position) t 1))))
    (if (and file-start
             file-end
             current-file)
        (progn (replace-region-contents file-start file-end (lambda () filename))
               (forward-char (apply #'-
                                    (seq-sort  #'>
                                               (mapcar
                                                #'length
                                                (delete nil
                                                        (list current-file
                                                              filename)))))))
      (if (equal current-file filename)
          (insert filename)
        (forward-char (length filename))))))

(defun autofix-read-summary (&optional initial-input)
  "Read a string from the minibuffer with INITIAL-INPUT."
  (autofix-trim-and-replace
   "\\."
   ""
   (read-string
    "Summary:\s"
    (or initial-input
        (when-let* ((annotation (autofix-make-short-annotation)))
          (concat "Configure " annotation))))))

(defun autofix--summary-period ()
  "Remove a period in the package summary."
  (save-excursion
    (goto-char (point-min))
    (goto-char (line-end-position))
    (when (and (looking-back "[-][*][-]" 0)
               (re-search-backward  "[-][*][-]" nil t 2))
      (let ((spaces (skip-chars-backward "\s\t")))
        (if (< spaces -1)
            (delete-char (1- (- spaces)))
          spaces))
      (when (looking-back "\\." 0)
        (forward-char -1)
        (let ((pos (point)))
          (when (autofix-overlay-prompt-region
                 pos
                 (1+ pos) '(face error) 'yes-or-no-p
                 "The package summary should not end with a period. Remove?")
            (delete-char 1)))))))

(defun autofix-sharpquote-sexp (symb-pos &optional prompt-fn)
  "Maybe add sharpquote to item at SYMB-POS in sexp at point.
Sharpquote will be added if item is a quoted symbol.
If PROMPT-FN is non nil, it should return nil to inhibit replacing."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let* ((sexp (sexp-at-point))
           (item
            (when (proper-list-p sexp)
              (nth symb-pos sexp)))
           (sym
            (when (eq (car-safe item) 'quote)
              (car-safe (cdr-safe item)))))
      (when (and sym
                 (symbolp sym))
        (let ((parse-sexp-ignore-comments t))
          (down-list 1)
          (forward-sexp (1+ symb-pos))
          (let ((end (point)))
            (forward-sexp -1)
            (when (and (looking-at "'")
                       (or (not prompt-fn)
                           (funcall #'autofix-overlay-prompt-region
                                    (point)
                                    end
                                    `(before-string ,(propertize "#" 'face
                                                                 'success)
                                                    face
                                                    error)
                                    prompt-fn)))
              (insert "#")
              t)))))))

;;;###autoload
(defalias 'autofix-function-name-quoting #'autofix-sharpquotes)

;;;###autoload
(defun autofix-sharpquotes (&optional no-prompt)
  "Add sharpquotes according to `autofix-sharpquote-symbols-spec'.

With optional argument NO-PROMPT replace occurrences without prompting.

For example, such code:

\(mapcar \\='car \\='((a . 2) (b . 2) (c . 3)))

Transforms to:

\(mapcar #\\='car \\='((a . 2) (b . 2) (c . 3))).

To customize this behavior see variable `autofix-sharpquote-symbols-spec'."
  (interactive "P")
  (let* ((confirmed)
         (prompt-fn
          (unless no-prompt
            (lambda ()
              (if (eq confirmed ?!)
                  t
                (setq confirmed
                      (car
                       (read-multiple-choice "Replace?"
                                             '((?y "yes"
                                                   "perform the action")
                                               (?n "no"
                                                   "skip to the next")
                                               (?! "all"
                                                   "accept all remaining without more questions")
                                               (?h "help"
                                                   "show help")
                                               (?q "quit" "exit")))))
                (pcase confirmed
                  (?q (keyboard-quit))
                  ((or ?y ?!)
                   t)))))))
    (autofix--sharpquotes prompt-fn)))

(defun autofix--sharpquotes (&optional prompt-fn)
  "Ensure sharpquotes according to `autofix-sharpquote-symbols-spec'.

If PROMPT-FN is non nil, it will be called without argument and should return
nil to to inhibit replacing."
  (pcase-dolist (`(,k . ,v) autofix-sharpquote-symbols-spec)
    (autofix-fix-sharpquotes k v prompt-fn)))

(defun autofix-fix-sharpquotes (parent-symb symb-pos &optional prompt-fn)
  "Ensure sharpquotes in lists which car is PARENT-SYMB with symbol at SYMB-POS.
Sharpquote will be added if item is a quoted symbol.
PARENT-SYMB should be either symbol or list of symbols.
If PROMPT-FN is non nil, it will be applied with ARGS and should return nil to
to inhibit replacing."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((regex (regexp-opt (if (symbolp parent-symb)
                                 (list (symbol-name parent-symb))
                               (mapcar #'symbol-name parent-symb))
                             'symbols))
          (results))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward regex nil t 1)
          (let ((stx (syntax-ppss (point))))
            (unless (or (nth 3 stx)
                        (nth 4 stx)
                        (autofix-elisp-move-with 'backward-sexp))
              (when (autofix-backward-up-list)
                (unless (looking-back "[`']" 0)
                  (save-excursion
                    (when (autofix-sharpquote-sexp symb-pos prompt-fn)
                      (push (point) results))))))))
        results))))




;;;###autoload
(defun autofix-header-first-line ()
  "Fix or create the first comment line in the header."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((filename (autofix-non-directory-file-or-buff)))
      (if-let* ((current-header (autofix-get-current-file-header)))
          (let ((current-file (nth 0 current-header))
                (current-bindings (seq-find
                                   (lambda (it)
                                     (member "lexical-binding:"
                                             (split-string it nil t)))
                                   current-header))
                (current-description))
            (setq current-description
                  (if (or (equal
                           current-bindings
                           (nth 1 current-header))
                          (when current-description
                            (string-empty-p
                             current-description)))
                      nil
                    (nth 1 current-header)))
            (skip-chars-forward ";\s\t")
            (autofix-ensure-header-file filename current-file)
            (skip-chars-forward "\s\t")
            (skip-chars-forward "-")
            (cond ((and (null current-description)
                        current-bindings)
                   (insert
                    (if (looking-back "\s" 0) "" "\s")
                    (autofix-read-summary)
                    (if (looking-at "\s" 0) "" "\s")))
                  ((and (null current-description)
                        (null current-bindings))
                   (let ((rep (autofix-read-summary)))
                     (replace-region-contents
                      (point)
                      (line-end-position)
                      (lambda ()
                        (concat
                         rep
                         " -*- lexical-binding: t; -*-")))))
                  ((and current-description
                        (null current-bindings))
                   (replace-region-contents (point)
                                            (line-end-position)
                                            (lambda ()
                                              (concat " "
                                                      (autofix-trim-and-replace
                                                       "\\.$"
                                                       ""
                                                       current-description)
                                                      " -*- lexical-binding: t; -*-"))))
                  (t (autofix--summary-period))))
        (insert (concat ";;; " filename "\s" "--- "
                        (autofix-trim-and-replace
                         "\\.$"
                         ""
                         (autofix-read-summary))
                        " -*- lexical-binding: t; -*-"
                        (if (looking-at "[^\n]")
                            "\n\n"
                          (if (looking-at "\n\n") "" "\n"))))))))

(defun autofix-trim-and-replace (regexp rep str)
  "Trim STR and replace all matches for REGEXP with REP."
  (replace-regexp-in-string regexp rep (string-trim str)))

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
  "Insert or fix the commentary section in the header of an Emacs Lisp file."
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
(defun autofix-autoloads (&optional no-prompt)
  "Fix autoloads interactively, with optional prompt to confirm each action.

Argument NO-PROMPT is a flag that determines whether or not to prompt the user
for confirmation before performing an action."
  (interactive "P")
  (let* ((confirmed)
         (prompt-fn
          (unless no-prompt
            (lambda ()
              (if (memq confirmed '(?! ?N))
                  (or (eq confirmed ?!)
                      (not (eq confirmed ?N)))
                (setq confirmed
                      (car
                       (read-multiple-choice "Replace?"
                                             '((?y "yes"
                                                   "perform the action")
                                               (?n "no"
                                                   "skip to the next")
                                               (?N "No, for all"
                                                   "No, decline all remaining without more questions")
                                               (?! "all"
                                                   "accept all remaining without more questions")
                                               (?h "help"
                                                   "show help")
                                               (?q "quit" "exit")))))
                (pcase confirmed
                  (?q (keyboard-quit))
                  ((or ?y ?!)
                   t)))))))
    (save-excursion
      (goto-char (point-max))
      (while (autofix-backward-list)
        (autofix-ensure-autoload prompt-fn)))))

;;;###autoload
(defun autofix-header ()
  "Apply all functions from `autofix-header-functions'."
  (interactive)
  (save-excursion
    (dolist (fn autofix-header-functions)
      (funcall fn))))

;;;###autoload
(defun autofix-remove-unused-declarations ()
  "Remove unused declared functions."
  (interactive)
  (let* ((declarations-re (mapcar (lambda (it)
                                    (regexp-quote (car it)))
                                  (plist-get (autofix-scan-buffer)
                                             :declare-function)))
         (unused (seq-remove
                  (lambda (re)
                    (save-excursion
                      (goto-char (point-min))
                      (autofix-re-search-forward re nil t 2)))
                  declarations-re)))
    (dolist (re unused)
      (save-excursion
        (goto-char (point-min))
        (autofix-re-search-forward re nil t 1)
        (backward-up-list 1)
        (let ((bounds (bounds-of-thing-at-point 'sexp)))
          (autofix-confirm-and-replace-region (save-excursion
                                                (goto-char (car bounds))
                                                (skip-chars-backward "\n")
                                                (if (looking-back ";" 0)
                                                    (car bounds)
                                                  (point)))
                                              (cdr bounds)
                                              ""))))))

;;;###autoload
(defun autofix-footer ()
  "Add of fix file footer (provide \\='filename) with comment ends here."
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
                  (delete-region (car bounds)
                                 (cdr bounds))
                  (goto-char (point-max))
                  (insert (concat "(provide " "'" name ")"
                                  "\n"
                                  footer-end)))
              (forward-sexp 1)
              (unless (looking-at (concat "\n" (regexp-quote footer-end)))
                (delete-region (car bounds)
                               (cdr bounds))
                (goto-char (point-max))
                (insert (concat (prin1-to-string l)
                                "\n"
                                footer-end)))))
        (goto-char (point-max))
        (insert (if (looking-back "\n" 0) "" "\n")
                (concat "(provide " "'" name ")" "\n" footer-end))))))

;;;###autoload (autoload 'autofix-menu "autofix" nil t)
(transient-define-prefix autofix-menu ()
  "Command dispatcher for autofix."
  [["Autofix"
    ("a" "all" autofix)
    ("f" "Add fbound" autofix-add-fbound)
    ("u" "Remove unused declarations"
     autofix-remove-unused-declarations)
    ("q" "sharp quotes" autofix-function-name-quoting)
    ("F" "footer" autofix-footer)
    ("h" "header" autofix-header)
    ("s" "add autoloads" autofix-autoloads)
    ("l" "first header line" autofix-header-first-line)]
   [("y" "copyright" autofix-copyright)
    ("r" "author" autofix-author)
    ("U" "url" autofix-url)
    ("v" "version" autofix-version)
    ("k" "keywords" autofix-keywords)
    ("R" "package-requires" autofix-package-requires)
    ("b" "body comment" autofix-header-body-comment)
    ("m" "commentary" autofix-commentary)
    ("C" "add Code" autofix-code-comment)]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (apply #'vector
              (mapcar (lambda (it)
                        (let ((description (nth 1 it)))
                          (if (stringp description)
                              it
                            (append (list (car it)
                                          (nth 2 it)
                                          :description
                                          description)
                                    (seq-drop it 3)))))
                      (seq-filter (lambda (l)
                                    (fboundp (nth 2 l)))
                                  autofix-extra-transient-suffixes)))))
    :class transient-column]])


;;;###autoload
(defun autofix ()
  "Apply all functions from `autofix-functions'.

Default code fixes includes auto adding auto load comments before all
interactive commands and removing unused (declare-function ...) forms.

Default comments fixes includes fixes for file headers,
package headers, footer etc.

File headers fixes can be customized via `autofix-header-functions'."
  (interactive)
  (let ((name (or (if buffer-file-name
                      (file-name-nondirectory buffer-file-name)
                    (buffer-name)))))
    (unless (member name autofix-ignored-file-patterns)
      (dolist (fn autofix-functions)
        (funcall fn)))))

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
      (add-hook 'before-save-hook #'autofix nil 'local)
    (remove-hook 'before-save-hook #'autofix 'local)))

(provide 'autofix)
;;; autofix.el ends here