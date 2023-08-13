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
                  '("identifier" "type_identifier" "field_identifier"
                    "shorthand_field_identifier" "true" "false"))
      (when (and (> p (treesit-node-start node))
                 (<= p (treesit-node-end node)))
        (list (treesit-node-start node) (treesit-node-end node)
              (treesit-node-text node t))))))

(defun dwim-coder-rust-dwim-space ()
  (let ((value nil))
    (cond
     ((nth 3 (syntax-ppss))
      nil)
     ;; Let SPC at start of line do '_'
     ((or (bolp)
          (looking-back "[([][&*]?" (line-beginning-position)))
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
    (if (looking-back "::" (line-beginning-position))
        (progn
          (insert "<>")
          (backward-char))
      (dwim-coder-insert-interactive ?\())
    t)
   (t
    (dwim-coder-common-dwim-dot))))

(defun dwim-coder-rust-dwim-colon ()
  (cond
   ((looking-back ": ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (dwim-coder-insert-interactive ?: t)
    t)
   (t
    (dwim-coder-insert-interactive ?: t)
    t)))

(defun dwim-coder-rust-dwim-comma ()
  (let ((value nil)
        (case-fold-search nil))
    (cond
     ((nth 3 (syntax-ppss))
      (dwim-coder-skip-or-insert ?,)
      t)
     ((and (eq (preceding-char) ?.)
           (not (eq (char-before (1- (point))) ?.)))
      (dwim-coder-insert-interactive ?. t)
      t)
     ;; Replace , with # if preceding content is blank
     ((looking-back "^ *" (line-beginning-position))
      (dwim-coder-insert-interactive ?#)
      t)
     ((looking-back "^ *#" (line-beginning-position))
      (insert "!")
      t)
     ;; (, (&, and (*,
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
     ((and (setq value (treesit-node-at (point)))
           (equal (treesit-node-type value) "char_literal")
           (eq (following-char) ?')
           (eq (preceding-char) ?'))
      (insert-char ?,)
      t)
     ((looking-back "::" (line-beginning-position))
      (dwim-coder-insert-interactive ?*)
      t)
     ((looking-back ":[ ]?" (line-beginning-position))
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
      t))))

(defun dwim-coder-rust-dwim-semi-colon ()
  (cond
   ((and (eolp)
         (looking-back "^ *#!?[[].*$" (line-beginning-position)))
    (dwim-coder-insert-interactive ?\n)
    t)
   ((and (looking-at-p " ?[])}]")
         (looking-back "[&|,+-] ?" (line-beginning-position)))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (dwim-coder-insert-interactive ?\n)
    t)
   ((and (eolp)
         (looking-back "[^[ \t;{(|&*^!~<>.,/:+=-]" (line-beginning-position))
         (not (looking-back "^ *_$" (line-beginning-position))))
    (insert ";")
    t)
   (t
    (dwim-coder-common-dwim-semi-colon))))

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
   ((memq char '(?+ ?- ?* ?% ?^ ?& ?| ?< ?> ?=))
    (dwim-coder-common-dwim-op char))))

(provide 'dwim-coder-rust)
;;; dwim-coder-rust.el ends here
