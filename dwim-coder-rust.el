;;; dwim-coder-rust.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; dwim hacks implemeted for `rust-ts-mode'

;;; Code:

(require 'dwim-coder-common)
(require 'cl-lib)
(require 'treesit)

(defvar rust-ts-mode--operators)

(defun dwim-coder-rust-op-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p))
         (operators rust-ts-mode--operators)
         (can-append-equal nil)
         (type  nil))
    ;; Remove operators we don't care
    (setq operators (remove "." operators))
    (setq operators (remove "->" operators))
    (when (save-excursion
            (goto-char p)
            (looking-back "[<>%^&*/!+-]" (dwim-coder-preceding-point)))
      (setq node (treesit-node-at (dwim-coder-preceding-point))))
    (setq type (treesit-node-type node))
    (when (member type operators)
      (when (or (equal type "=")
                (and (not (string-suffix-p "=" type))
                     (not (member type '("&&" "||" "--" "++")))))
        (setq can-append-equal t))
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t) can-append-equal))))

(defun dwim-coder-rust-after-function-param-p (&optional p)
  (let ((p (or p (point)))
         (node nil))
    (when (save-excursion
            (skip-chars-backward " ")
            (and
             (eq (char-before) ?\))
             (progn
               (backward-sexp)
               (backward-char)
               (setq node (treesit-node-parent (treesit-node-at p))))))
      (or (equal (treesit-node-type node) "function_signature_item")
          (equal (treesit-node-type node) "function_item")))))

(defun dwim-coder-rust-identifier-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p)))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (dwim-coder-preceding-point)))
      (setq node (treesit-node-at (dwim-coder-preceding-point))))
    (when (member (treesit-node-type node)
                  '("identifier" "type_identifier" "field_identifier" "true" "false"))
      (when (and (> p (treesit-node-start node))
                 (<= p (treesit-node-end node)))
        (list (treesit-node-start node) (treesit-node-end node)
              (treesit-node-text node t))))))

(defun dwim-coder-rust-dwim-space ()
  (let ((value nil))
    (cond
     ((nth 3 (syntax-ppss))
      (if (and (looking-back ", " (line-beginning-position))
               (eq (following-char) ?\"))
          (progn
            (delete-char -2)
            (forward-char)
            (dwim-coder-skip-or-insert ?,)
            (if dwim-coder-auto-space
                (dwim-coder-skip-or-insert ?\s)))
        (dwim-coder-insert-interactive ?\s t))
      t)
     ;; Let SPC at start of line do '_'
     ((bolp)
      (insert "_")
      t)
     ((looking-back "super " (line-beginning-position))
      (delete-char -1)
      (insert "::")
      t)
     ((and (setq value (thing-at-point 'symbol t))
           (string-match-p "^\\(mut\\|impl\\|dyn\\)" value)
           (looking-back "mut\\|impl\\|dyn" (line-beginning-position)))
      (insert-char ?\s)
      t)
     ;; After function arguments let SPC SPC do SPC -> SPC
     ((and (eq (preceding-char) ?\s)
           (dwim-coder-rust-after-function-param-p))
      (dwim-coder-skip-or-insert ?\s)
      (if (looking-at "->")
          (forward-char 2)
        (insert "->"))
      (dwim-coder-skip-or-insert ?\s)
      (when (eq (following-char) ?\{)
        (dwim-coder-insert-interactive ?\s t)
        (backward-char))
      t)
     ;; ( SPC, (& SPC, and (* SPC
     ((looking-back "([&*]?" (line-beginning-position))
      (setq value (preceding-char))
      (if (memq (following-char) '(?& ?*))
          (forward-char)
        (if (eq value ?\()
            (dwim-coder-skip-or-insert ?& t)
          (delete-char -1)
          (if (eq value ?&)
              (dwim-coder-skip-or-insert ?* t)
            (dwim-coder-skip-or-insert ?& t))))
      t)
     ;; One identifier::SPC toggle 'identifier' style
     ((and
       (or (looking-back "[A-Za-z0-9_]::" (line-beginning-position))
           (looking-back "[A-Za-z0-9]__" (line-beginning-position)))
       (save-excursion
         (progn
           (backward-char 2)
           (setq value (dwim-coder-rust-identifier-at-point)))))
      ;; Remove :: or __
      (delete-char -2)
      (if (string-suffix-p "__" (caddr value))
          (insert "_::")
        (insert "__"))
      t)
     ;; Let .SPC give '_'
     ((and (eq (preceding-char) ?\.)
           (save-excursion
             (backward-char)
             (or (bolp)
                 (looking-back "[^a-z0-9]" (line-beginning-position)))))
      (delete-char -1)
      (insert "_")
      t)
     ;; change __ to ->
     ((or (setq value (dwim-coder-rust-identifier-at-point))
          (setq value (thing-at-point 'number t)))
      (if (looking-back "[a-zA-Z0-9]_" (line-beginning-position))
          (progn
            ;; Remove last '_'
            (delete-char -1)
            (insert "::"))
        (dwim-coder-insert-interactive ?_))
      t)
     ;; func (test, |) -> func (test), |
     ;; or {test, |} -> {test}, |
     ((save-excursion
        (and (not (eolp))
             (or (forward-char) t)
             (looking-back ", [)}]" (line-beginning-position))))
      (delete-char -2)
      (forward-char)
      (dwim-coder-insert-interactive ?,)
      t)
     ((eq (preceding-char) ?\s)
      (dwim-coder-insert-interactive ?_)
      t))))

(defun dwim-coder-rust-dwim-quote ()
  (let ((value nil)
        (bounds nil)
        (str nil))
    (cond
     ((and (setq str (thing-at-point 'symbol t))
           (setq bounds (bounds-of-thing-at-point 'symbol))
           (string-match-p "^[a-zA-Z_]" str))
      (delete-region (car bounds) (cdr bounds))
      (setq value (dwim-coder-s-get-style-case str))
      (if (equal value "snake")
          (insert (dwim-coder-s-to-style str "upcamel"))
        (if (equal value "upper-camel")
            (insert (dwim-coder-s-to-style str "upsnake"))
          (insert (dwim-coder-s-to-style str "snake"))))
      t))))

(defun dwim-coder-rust-dwim-dot ()
  (cond
   ((and (eq (preceding-char) ?.))
    (delete-char -1)
    (dwim-coder-insert-interactive ?\()
    t)
   ((dwim-coder-rust-after-function-param-p)
    (dwim-coder-insert-interactive ?{)
    t)))

(defun dwim-coder-rust-dwim-colon ()
  (cond
   ((looking-back ": ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (dwim-coder-insert-interactive ?: t)
    t)
   (t
    (dwim-coder-insert-interactive ?: t)
    (when dwim-coder-auto-space
      (dwim-coder-skip-or-insert ?\s)
      (when (eq (following-char) ?=)
        (dwim-coder-insert-interactive ?\s t)
        (backward-char)))
    t)))

(defun dwim-coder-rust-dwim-comma ()
  (let ((value nil)
        (case-fold-search nil))
    (cond
     ((nth 3 (syntax-ppss))
      (dwim-coder-skip-or-insert ?,)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ;; Replace , with # if in the beginning of line
     ((bolp)
      (dwim-coder-insert-interactive ?#)
      t)
     ((looking-back "^#" (line-beginning-position))
      (insert "!")
      t)
     ((and (setq value (treesit-node-at (point)))
           (equal (treesit-node-type value) "char_literal")
           (eq (following-char) ?')
           (eq (preceding-char) ?'))
      (insert-char ?,)
      t)
     ((looking-back ":[ ]?" (line-beginning-position))
      (when dwim-coder-auto-space
        (dwim-coder-skip-or-insert ?\s))
      (if (looking-at-p "&")
          (forward-char)
        (insert-char ?&))
      t)
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (eq (preceding-char) ?,))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      (dwim-coder-insert-interactive ?=)
      t)
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (memq (preceding-char) '(?+ ?- ?* ?& ?^ ?\% ?! ?~ ?< ?> ?=)))
      (dwim-coder-insert-interactive ?=)
      t)
     (t
      (if dwim-coder-auto-space
          (if (eq (preceding-char) ?\s)
              (delete-char -1))
        (if (eq (following-char) ?\s)
            (delete-char 1)))
      (dwim-coder-skip-or-insert ?,)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t))))

(defun dwim-coder-rust-dwim-brace ()
  (cond
   ((dwim-coder-rust-after-function-param-p)
    (dwim-coder-skip-or-insert ?\s)
    (dwim-coder-skip-or-insert ?{ t t)
    (insert "\n\n")
    (backward-char)
    (indent-according-to-mode)
    t)))

(defun dwim-coder-rust-dwim-semi-colon ()
  (cond
   ((eq (following-char) ?\;)
    (forward-char)
    t)
   ;; goto end of string if inside one
   ((nth 3 (syntax-ppss))
    (skip-syntax-forward "^\"")
    (forward-char)
    t)
   ;; Move forward if inside empty pairs
   ((and (not (bolp))
         (not (eolp))
         (save-excursion
           (backward-char)
           (looking-at-p "\\(()\\)\\|\\(\\[\\]\\)\\|\\({}\\)\\|\\(<>\\)")))
    (forward-char)
    t)
   ;; If not end of line, go to end of line
   ((not (eolp))
    (end-of-line)
    (if (eq (preceding-char) ?\;)
        (backward-char))
    t)
   ;; Skip to the end of the current statement if inside an argument list
   ((and (memq (preceding-char) '(?, ?\{ ?\[ ?\())
         (memq (char-after (nth 1 (syntax-ppss))) '(?\( ?\[ ?\{)))
    (goto-char (nth 1 (syntax-ppss)))
    (forward-sexp)
    t)))

(defun dwim-coder-rust-dwim-equal ()
  (cond
   ((looking-back "[]a-zA-Z0-9)}_] ?" (line-beginning-position))
    (when dwim-coder-auto-space
      (dwim-coder-skip-or-insert ?\s))
    (dwim-coder-insert-interactive ?= t)
    (when dwim-coder-auto-space
      (dwim-coder-skip-or-insert ?\s))
    t)
   (t
    (if (looking-back "[^ \t]* " (line-beginning-position))
        (backward-char))
    (when (memq (preceding-char) '(?> ?< ?- ?+ ?! ?& ?*))
      (backward-char)
      (when dwim-coder-auto-space
        (dwim-coder-skip-or-insert ?\s))
      (forward-char))
    (dwim-coder-insert-interactive ?= t)
    (when dwim-coder-auto-space
      (dwim-coder-skip-or-insert ?\s))
    t)))

(defun dwim-coder-rust-dwim-gt ()
  (cond
   ((looking-back "[-=>] ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (dwim-coder-insert-interactive ?> t)
    (when dwim-coder-auto-space
      (dwim-coder-skip-or-insert ?\s))
    t)))

(defun dwim-coder-rust-dwim-operator (char)
  (let ((value nil))
    (save-excursion
      (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
      (setq value (dwim-coder-rust-op-at-point (dwim-coder-preceding-point))))

    (if (and value
             (nth 3 value)
             (or (not (looking-back "= ?[-+!]*" (line-beginning-position)))
                 (memq char '(?, ?=))))
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point)))
    (dwim-coder-insert-interactive char t)
    (setq value (dwim-coder-rust-op-at-point (dwim-coder-preceding-point)))
    (cond
     ;; Handle =>
     ((and (eq char ?\>)
           (looking-back "=>" (line-beginning-position)))
      (dwim-coder-skip-or-insert ?\s)
      t)
     ((member (nth 2 value) '("--" "++"))
      (goto-char (nth 0 value))
      (if (eq (preceding-char) ?\s)
          (unless (or (looking-back "^[ \t]*" (line-beginning-position))
                      (looking-back "= *" (line-beginning-position)))
            (delete-char -1)))
      (indent-according-to-mode)
      (forward-char 2)
      (when (eq (following-char) ?\s)
        (delete-char 1)))
     ;; do nothing if preceding content is empty
     ((looking-back "^[ \t]*" (line-beginning-position))
      t)
     ;; Insert space before operator
     ((save-excursion
        (backward-char)
        (looking-back "[]a-zA-Z0-9_)]" (dwim-coder-preceding-point)))
      (goto-char (nth 0 value))
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      ;; update start and end point
      (setq value (dwim-coder-rust-op-at-point))
      (goto-char (nth 1 value))))
    (unless (or (member (nth 2 value) '("--" "++" "!"))
                (memq (char-before (nth 0 value)) '(?\( ?\[ ?{ ?<)))
      (if (and dwim-coder-auto-space
               (not (looking-back "= ?[-+!]" (line-beginning-position))))
          (dwim-coder-skip-or-insert ?\s)))
    t))

(defun dwim-coder-rust-dwim-rest (char)
  (cond
   ;; Convert x()y to x..y
   ((and (eq (preceding-char) ?\))
         (string-match-p "[a-zA-Z0-9_]" (string char))
         (looking-back "[a-z0-9A-Z_ ]()" (line-beginning-position)))
    (delete-char -2)
    (insert ".."))))

(defun dwim-coder-rust-override-self-insert (char)
  (cond
   ;; be sane with comments
   ((nth 4 (syntax-ppss))
    nil)
   ((eq char ?\;)
    (dwim-coder-rust-dwim-semi-colon))
   ((eq char ?,)
    (dwim-coder-rust-dwim-comma))
   ((eq char ?\s)
    (dwim-coder-rust-dwim-space))
   ;; be sane with string
   ((nth 3 (syntax-ppss))
    nil)
   ((eq char ?\:)
    (dwim-coder-rust-dwim-colon))
   ((eq char ?.)
    (dwim-coder-rust-dwim-dot))
   ((eq char ?\')
    (dwim-coder-rust-dwim-quote))
   ((eq char ?{)
    (dwim-coder-rust-dwim-brace))
   ((eq char ?=)
    (dwim-coder-rust-dwim-equal))
   ((eq char ?>)
    (dwim-coder-rust-dwim-gt))
   ((memq char '(?/ ?% ?- ?+ ?|))
    (dwim-coder-rust-dwim-operator char))
   (t
    (dwim-coder-rust-dwim-rest char))))

(provide 'dwim-coder-rust)
;;; dwim-coder-rust.el ends here
