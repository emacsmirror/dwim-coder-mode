;;; crazy-python.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0
;; Created: 2023-01-26
;; Last-Updated: 2023-05-01

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; dwim hacks implemeted for `python-ts-mode'

;;; Code:


(require 'crazy-default)

(defun crazy-python-be-sane ()
  ;; situations to avoid being crazy
  (cond
   ;; comments and strings
   ((or (nth 4 (syntax-ppss))
	(nth 3 (syntax-ppss)))
    t)
   (t nil)))

(defun crazy-python-dwim-space ()
  (let ((node (treesit-node-at (crazy-preceding-point))))
    (cond
     ((and (nth 3 (syntax-ppss))
           (treesit-node-top-level (treesit-node-at (point)) "^interpolation$"))
      (crazy-insert-interactive ?_)
      t)
     ((and (nth 3 (syntax-ppss))
           (memq (following-char) '(?\" ?\'))
           (looking-back ", " (line-beginning-position)))
      (delete-char -2)
      (forward-char)
      (crazy-skip-or-insert ?\, t)
      t)
     ((crazy-python-be-sane)
      nil)
     ((and (eolp)
           (looking-back ") ?" (line-beginning-position))
           (save-excursion
             (skip-chars-backward "[ ]" (crazy-preceding-point))
             (backward-sexp)
             (and (setq node (treesit-node-at (crazy-preceding-point)))
                  (equal (treesit-node-type node) "identifier")
                  (goto-char (treesit-node-start node))
                  (or (backward-char) t)
                  (setq node (treesit-node-at (crazy-preceding-point)))
                  (equal (treesit-node-type node) "def"))))
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      (insert "->")
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ((and (looking-back ", " (line-beginning-position))
           (memq (following-char) '(?\) ?\] ?})))
      (delete-char -2)
      (forward-char)
      (crazy-skip-or-insert ?, t)
      t)
     ((or (bolp)
          (memq (preceding-char) '(?\s ?. ?\())
          (and node (equal (treesit-node-type node) "identifier"))
          (and node (equal (treesit-node-type node) "integer")))
      (crazy-insert-interactive ?_)
      t))))

(defun crazy-python-dwim-dot ()
  (cond
   ((eq (preceding-char) ?.)
    (delete-char -1)
    (crazy-insert-interactive ?\()
    t)
   ((looking-back "^ *" (line-beginning-position))
    (crazy-skip-or-insert ?@ t)
    t)
   ((and
     (crazy-python-be-sane)
     (eq (preceding-char) ?\()
     (eq (following-char) ?\)))
    (delete-char -1)
    (delete-char 1)
    (insert "...")
    t)
   ((looking-back "^ *@" (line-beginning-position))
    (delete-char -1)
    (crazy-insert-interactive ?\()
    t)))

(defun crazy-python-dwim-comma ()
  (let ((node (treesit-node-at (crazy-preceding-point))))
    (cond
     ((and (nth 3 (syntax-ppss))
           (setq node (treesit-node-top-level (treesit-node-at (point)) "^interpolation$")))
      (goto-char (treesit-node-end node))
      t)
     ((crazy-default-be-sane)
      (crazy-skip-or-insert ?\, t t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ((looking-back "^ *" (line-beginning-position))
      (crazy-skip-or-insert ?# t nil)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ((looking-back ", ?" (line-beginning-position))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      (crazy-skip-or-insert ?= t)
      t)
     ((save-excursion
        (skip-chars-backward "[ ]" (crazy-preceding-point))
        (memq (preceding-char) '(?= ?- ?+ ?* ?/ ?% ?< ?> ?! ?^ ?| ?&)))
      (crazy-insert-interactive ?=)
      t)
     (t
      (crazy-insert-interactive ?\, t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t))))

(defun crazy-python-dwim-semi ()
  (if (eolp)
      (progn
        (unless (looking-at-p "\n\n")
          (insert-char ?\n)
          (indent-according-to-mode)))
    (end-of-line))
  t)

(defun crazy-python-dwim-quote ()
  (let ((node (treesit-node-at (crazy-preceding-point)))
        (value nil)
        (style nil))
    (when (and node
               (equal (treesit-node-type node) "identifier")
               (setq value (treesit-node-text node))
               (> (length value) 1))
      (delete-region (treesit-node-start node) (treesit-node-end node))
      (setq style (crazy-s-get-style-case value))
      (if (equal style "snake")
          (insert (crazy-s-to-style value "upcamel"))
        (if (equal style "upper-camel")
            (insert (crazy-s-to-style value "upsnake"))
          (insert (crazy-s-to-style value "snake"))))
      t)))

(defun crazy-python-dwim-equal ()
  (let ((node nil))
    (cond
     ((treesit-node-top-level (treesit-node-at (crazy-preceding-point)) "^type$")
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      (crazy-insert-interactive ?\= t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ((and (setq node (treesit-node-parent (treesit-node-at (point))))
           (equal (treesit-node-type node) "argument_list"))
      (crazy-skip-or-insert ?= t t)
      t)
     ((and (setq node (treesit-node-parent (treesit-node-at (point))))
           (equal (treesit-node-type node) "parameters"))
      (crazy-skip-or-insert ?= t t)
      t)
     ((setq node (treesit-node-at (crazy-preceding-point)))
      (if (equal (treesit-node-type node) "identifier")
          (if crazy-auto-space
              (crazy-skip-or-insert ?\s))
        (skip-chars-backward "[ ]" (crazy-preceding-point)))
      (crazy-insert-interactive ?\= t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t))))

(defun crazy-python-dwim-op (char)
  (let ((node (treesit-node-at (crazy-preceding-point)))
        (value nil))
    (cond
     ((and (eq char ?/)
           (looking-back ", ?" (line-beginning-position)))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      t)
     ((and node
           (setq value (treesit-node-text node))
           (member value '(">" "<"))
           (memq char '(?> ?< ?=)))
      (skip-chars-backward "[ ]" (crazy-preceding-point))
      (crazy-insert-interactive char t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ((and (equal (treesit-node-type node) "identifier")
           (memq char '(?= ?- ?+ ?* ?/ ?% ?< ?> ?! ?^ ?| ?&)))
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      (crazy-skip-or-insert char t t)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t))))

(defun crazy-python-override-self-insert (char)
  (pcase char
    (?\s (crazy-python-dwim-space))
    (?. (crazy-python-dwim-dot))
    (?, (crazy-python-dwim-comma))
    (?\; (crazy-python-dwim-semi))
    ((guard (crazy-default-be-sane) nil))
    (?' (crazy-python-dwim-quote))
    (?= (crazy-python-dwim-equal))
    (_ (crazy-python-dwim-op char))))

(provide 'crazy-python)
;;; crazy-python.el ends here
