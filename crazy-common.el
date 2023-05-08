;;; crazy-common.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; Last-Updated: 2022-12-18

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; common code shared with other files

;;; Code:


(defvar-local crazy-skip nil)
(defvar-local crazy-last-was-camel nil)

(defgroup crazy nil
  "Crazy ways to code."
  :group 'convenience
  :group 'editing
  :prefix "crazy-")

(defcustom crazy-auto-space t
  "Automatically insert space around operators.

Experimental, may be removed/disabled in the future"
  :type 'boolean
  :group 'crazy)
;;;###autoload
(put 'crazy-auto-space 'safe-local-variable #'booleanp)

(defcustom crazy-space-char nil
  "Character to be inserted on SPC, can be one of nil, ?_, ?- or ?\s."
  :type 'integer
  :group 'crazy)
;;;###autoload
(put 'crazy-space-char 'safe-local-variable (lambda (x) (and (integerp x) (memq x '(?_ ?- ?\s)))))

(defun crazy-preceding-point ()
  (max (1- (point)) (line-beginning-position)))

(defun crazy-insert-interactive (char &optional skip-override)
  "Interactively insert CHAR.

This will simply use `call-interactively' with the given character CHAR,
so that the default handlers when CHAR is inserted is run.
If SKIP-OVERRIDE is t, the default handlers run by `crazy-mode'
shall be ignored."
  (interactive)
  (setq crazy-skip skip-override
        last-command-event char
        this-command 'self-insert-command)
  (call-interactively #'self-insert-command)
  (setq crazy-skip nil))

(defun crazy-skip-or-insert (char &optional interactive skip-override)
  (if (eq (preceding-char) char)
      nil
    (if (eq (following-char) char)
        (forward-char)
      (if interactive
          (crazy-insert-interactive char skip-override)
        (insert-char char)))))

;; Copied from s.el
;; URL: https://github.com/magnars/s.el
;; GPL-3.0+ license
(defun crazy-s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (declare (pure t) (side-effect-free t))
  (mapconcat #'identity strings separator))

(defun crazy-s-split (separator s &optional omit-nulls)
  "Split S into substrings bounded by matches for regexp SEPARATOR.
If OMIT-NULLS is non-nil, zero-length substrings are omitted.
This is a simple wrapper around the built-in `split-string'."
  (declare (side-effect-free t))
  (save-match-data
    (split-string s separator omit-nulls)))

(defun crazy-s-split-words (s)
  "Split S into list of words."
  (declare (side-effect-free t))
  (crazy-s-split
   "[^[:word:]0-9]+"
   (let ((case-fold-search nil))
     (replace-regexp-in-string
      "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
      (replace-regexp-in-string "\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)" "\\1 \\2" s)))
   t))

(defun crazy-s-snake-case (s)
  "Convert S to snake_case."
  (declare (side-effect-free t))
  (crazy-s-join "_" (mapcar #'downcase (crazy-s-split-words s))))

(defun crazy-s-upper-snake-case (s)
  "Convert S to snake_case."
  (declare (side-effect-free t))
  (upcase (crazy-s-snake-case s)))

(defun crazy-s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (declare (side-effect-free t))
  (crazy-s-join "" (mapcar #'capitalize (crazy-s-split-words s))))

(defun crazy-s-dashed-words (s)
  "Convert S to dashed-words."
  (declare (side-effect-free t))
  (crazy-s-join "-" (mapcar #'downcase (crazy-s-split-words s))))

(defun crazy-s-get-style-case (s)
  "Returns the style case of S as a string, or nil if S contains no alphabet.

The value can be one of \"spaced\", \"snake\", \"upper-snake\", \"lisp\",
\"upper-camel\" or \"lower-camel\" (in the order of precedence).

So if S looks like a snake (have at least onelower case letter and a '-'), the
string \"snake\" shall be return regardless of whether it has a hump or not.

There is no one size fit solution to interpret style.  Read the source to get
heuristics used to interpret the style."
  (declare (side-effect-free t))
  (let ((case-fold-search nil))
    (cond
     ((string-match-p "^[^[:alpha:]]+$" s)
      nil)
     ((string-match-p "[ \t\n\r]" s)
      "spaced")
     ((string-match-p "\\([[:lower:]].*_\\)\\|\\([^_[:upper:]]_.*[[:lower:]]\\)\\|\\(^[^[:upper:]-]*$\\)" s)
      "snake")
     ((string-match-p "\\([[:upper:]].*_\\)\\|\\([^_[:lower:]]_.*[[:upper:]]\\)\\|\\(^[^[:lower:]-]*$\\)" s)
      "upper-snake")
     ((string-match-p "-" s)
      "lisp")
     ((string-match-p "^_?[[:upper:]]" s)
      "upper-camel")
     ((string-match-p "[[:upper:]]" s)
      "lower-camel"))))

(defun crazy-s-to-style (str style)
  (let* ((case-fold-search nil)
         (str (or str ""))
         (current-style (crazy-s-get-style-case str))
         (prefix nil)
         (suffix nil))

    ;; hack: So that we can keep using s.el API
    (setq str (replace-regexp-in-string ":" "അ" str))
    (setq str (replace-regexp-in-string "|" "ആ" str))

    ;; More cases to assume snake style
    (when (or (string-match-p "^[A-Z]\\{4,\\}_?[a-z]+[ _-]*$" str)
              (string-match-p "[A-Z]+_?[A-Z]+_?[A-Z]+_?[a-z]+[ _-]*$" str)
              (string-match-p "^\\([A-Z]+_+\\)\\{1,\\}\\([a-z]+_?\\)\\{1,\\}$" str))
      (setq str (downcase str))
      (setq current-style "snake"))

    (when (string-match "\\(^[ _.;-]+\\)" str)
      (setq prefix (match-string-no-properties 1 str)))
    (when (string-match "\\([a-zA-Z]\\)\\([ _.;-]+$\\)" str)
      (setq suffix (match-string-no-properties 2 str)))

    (if (or (string= current-style "upper-camel")
            (string= current-style "lower-camel"))
        (setq str (replace-regexp-in-string "\\([A-Z]\\)" "_\\1" str t)))
    (cond
     ((string= style "snake")
      (setq str (crazy-s-snake-case str)))
     ((string= style "upsnake")
      (setq str (crazy-s-upper-snake-case str)))
     ((string= style "upcamel")
      (setq str (crazy-s-upper-camel-case str)))
     ((string= style "lisp")
      (setq str (crazy-s-dashed-words str)))
     ((string= style "cycle")
      (cond
       ((string= current-style "snake")
        (setq str (crazy-s-upper-snake-case str)))
       ((string= current-style "upper-snake")
        (setq str (crazy-s-upper-camel-case str)))
       ((string= current-style "upper-camel")
        (setq str (crazy-s-snake-case str)))
       (t
        (setq str (crazy-s-snake-case str))))))

    (setq str (replace-regexp-in-string "അ" ":" str))
    (setq str (replace-regexp-in-string "ആ" "|" str))

    (concat prefix str suffix)))

(provide 'crazy-common)
;;; crazy-common.el ends here
