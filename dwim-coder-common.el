;;; dwim-coder-common.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0
;; Last-Updated: 2023-06-02

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


(defvar-local dwim-coder-skip nil)
(defvar-local dwim-coder-last-dot-point 0)
(defvar-local dwim-coder-last-space-point 0)

(defgroup dwim-coder nil
  "DWIM keybindings for programming modes."
  :group 'convenience
  :group 'editing
  :prefix "dwim-coder-")

(defcustom dwim-coder-auto-space t
  "Automatically insert space around operators.

Experimental, may be removed/disabled in the future"
  :type 'boolean
  :group 'dwim-coder)
;;;###autoload
(put 'dwim-coder-auto-space 'safe-local-variable #'booleanp)

(defcustom dwim-coder-space-char nil
  "Character to be inserted on SPC, can be one of nil, ?_, ?- or ?\s."
  :type 'integer
  :group 'dwim-coder)
;;;###autoload
(put 'dwim-coder-space-char 'safe-local-variable (lambda (x) (and (integerp x) (memq x '(?_ ?- ?\s)))))

(defun dwim-coder-preceding-point ()
  (max (1- (point)) (line-beginning-position)))

(defun dwim-coder-insert-interactive (char &optional skip-override)
  "Interactively insert CHAR.

This will simply use `call-interactively' with the given character CHAR,
so that the default handlers when CHAR is inserted is run.
If SKIP-OVERRIDE is t, the default handlers run by `dwim-coder-mode'
shall be ignored."
  (interactive)
  (setq dwim-coder-skip skip-override
        last-command-event char
        this-command 'self-insert-command)
  (call-interactively #'self-insert-command)
  (setq dwim-coder-skip nil))

(defun dwim-coder-skip-or-insert (char &optional interactive skip-override)
  (if (eq (preceding-char) char)
      nil
    (if (eq (following-char) char)
        (forward-char)
      (if interactive
          (dwim-coder-insert-interactive char skip-override)
        (insert-char char)))))

;; Copied from s.el
;; URL: https://github.com/magnars/s.el
;; GPL-3.0+ license
(defun dwim-coder-s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (declare (pure t) (side-effect-free t))
  (mapconcat #'identity strings separator))

(defun dwim-coder-s-split (separator s &optional omit-nulls)
  "Split S into substrings bounded by matches for regexp SEPARATOR.
If OMIT-NULLS is non-nil, zero-length substrings are omitted.
This is a simple wrapper around the built-in `split-string'."
  (declare (side-effect-free t))
  (save-match-data
    (split-string s separator omit-nulls)))

(defun dwim-coder-s-split-words (s)
  "Split S into list of words."
  (declare (side-effect-free t))
  (dwim-coder-s-split
   "[^[:word:]0-9]+"
   (let ((case-fold-search nil))
     (replace-regexp-in-string
      "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2"
      (replace-regexp-in-string "\\([[:upper:]]\\)\\([[:upper:]][0-9[:lower:]]\\)" "\\1 \\2" s)))
   t))

(defun dwim-coder-s-snake-case (s)
  "Convert S to snake_case."
  (declare (side-effect-free t))
  (dwim-coder-s-join "_" (mapcar #'downcase (dwim-coder-s-split-words s))))

(defun dwim-coder-s-upper-snake-case (s)
  "Convert S to snake_case."
  (declare (side-effect-free t))
  (upcase (dwim-coder-s-snake-case s)))

(defun dwim-coder-s-upper-camel-case (s)
  "Convert S to UpperCamelCase."
  (declare (side-effect-free t))
  (dwim-coder-s-join "" (mapcar #'capitalize (dwim-coder-s-split-words s))))

(defun dwim-coder-s-dashed-words (s)
  "Convert S to dashed-words."
  (declare (side-effect-free t))
  (dwim-coder-s-join "-" (mapcar #'downcase (dwim-coder-s-split-words s))))

(defun dwim-coder-s-get-style-case (s)
  "Return the style case of S as a string, or nil if S contain no alphabet.

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

(defun dwim-coder-s-to-style (str style)
  (let* ((case-fold-search nil)
         (str (or str ""))
         (current-style (dwim-coder-s-get-style-case str))
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
      (setq str (dwim-coder-s-snake-case str)))
     ((string= style "upsnake")
      (setq str (dwim-coder-s-upper-snake-case str)))
     ((string= style "upcamel")
      (setq str (dwim-coder-s-upper-camel-case str)))
     ((string= style "lisp")
      (setq str (dwim-coder-s-dashed-words str)))
     ((string= style "cycle")
      (cond
       ((string= current-style "snake")
        (setq str (dwim-coder-s-upper-snake-case str)))
       ((string= current-style "upper-snake")
        (setq str (dwim-coder-s-upper-camel-case str)))
       ((string= current-style "upper-camel")
        (setq str (dwim-coder-s-snake-case str)))
       (t
        (setq str (dwim-coder-s-snake-case str))))))

    (setq str (replace-regexp-in-string "അ" ":" str))
    (setq str (replace-regexp-in-string "ആ" "|" str))

    (concat prefix str suffix)))

(defun dwim-coder-common-dwim-op (char)
  (dwim-coder-insert-interactive char t)
  ;; electric-oprator inserts space after an operator even
  ;; if the character followed is another operator.
  (cond
   ((not (eq (preceding-char) ?\s))
    t)
   ((not (memq char '(?+ ?- ?* ?% ?^ ?& ?| ?< ?> ?=)))
    t)
   ((eq (following-char) ?=)
    (delete-char -1))
   ((and (eq char ?=)
         (eq (following-char) ?>))
    (delete-char -1))
   ((and (memq char '(?< ?>))
         (memq (following-char) '(?< ?>)))
    (delete-char -1))
   ((and (memq char '(?& ?|))
         (memq (following-char) '(?& ?|)))
    (delete-char -1)))
  t)

(defun dwim-coder-common-dwim-semi-colon ()
  (let ((value nil))
    (cond
     ;; goto end of string if inside one
     ((nth 3 (syntax-ppss))
      (while (and (not (eobp))
                  (nth 3 (syntax-ppss)))
        (if (next-property-change (point))
            (goto-char (next-property-change (point))))
        (if (nth 3 (syntax-ppss))
            (forward-char)))
      t)
     ;; On lines with _ only, delete the line and go to the end of last line
     ((and (looking-back "^ *[_-]$" (line-beginning-position))
           (looking-at-p " *$"))
      (delete-line)
      ;; Don't warn if we are at the beginning of the buffer
      (ignore-errors (backward-char))
      t)
     ((eq (following-char) ?\;)
      (forward-char)
      t)
     ;; Move up a list if list we contain ends in the same line.
     ;; Do a regex match first as `up-list' can be very slow if list is big
     ;; fixme: Use a better approach
     ((and (looking-at-p ".*[])}].")
           (setq value (save-excursion
                         (ignore-errors (up-list))
                         (point)))
           (> value (point))
           (< value (line-end-position)))
      (up-list)
      t)
     ((not (eolp))
      (end-of-line)
      (if (eq (preceding-char) ?\;)
          (backward-char))
      t)
     ((and (eolp)
           (looking-back ", ?" (line-beginning-position)))
      (undo-boundary)
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (dwim-coder-insert-interactive ?\n)
      t)
     ((eolp)
      (undo-boundary)
      ;; On empty lines, insert a newline
      (when (looking-back "^ *$" (line-beginning-position))
        (delete-line)
        (unless (bobp)
          (backward-char)
          (dwim-coder-insert-interactive ?\n)))
      (dwim-coder-insert-interactive ?\n)
      t))))

(provide 'dwim-coder-common)
;;; dwim-coder-common.el ends here
