;;; dwim-coder-python.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0
;; Created: 2023-01-26
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
;; dwim hacks implemented for `python-ts-mode'

;;; Code:


(require 'dwim-coder-default)

(defun dwim-coder-python-be-sane ()
  ;; situations to avoid being crazy
  (cond
   ;; comments and strings
   ((or (nth 4 (syntax-ppss))
	(nth 3 (syntax-ppss)))
    t)
   (t nil)))

(defun dwim-coder-python-dwim-space ()
  (let ((node (treesit-node-at (dwim-coder-preceding-point))))
    (cond
     ((and (nth 3 (syntax-ppss))
           (treesit-node-top-level (treesit-node-at (point)) "^interpolation$"))
      (dwim-coder-insert-interactive ?_)
      t)
     ((and (nth 3 (syntax-ppss))
           (memq (following-char) '(?\" ?\'))
           (looking-back ", " (line-beginning-position)))
      (delete-char -2)
      (forward-char)
      (dwim-coder-skip-or-insert ?\, t)
      t)
     ((dwim-coder-python-be-sane)
      nil)
     ((and (eolp)
           (looking-back ") ?" (line-beginning-position))
           (save-excursion
             (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
             (backward-sexp)
             (and (setq node (treesit-node-at (dwim-coder-preceding-point)))
                  (equal (treesit-node-type node) "identifier")
                  (goto-char (treesit-node-start node))
                  (not (bolp))
                  (or (backward-char) t)
                  (setq node (treesit-node-at (dwim-coder-preceding-point)))
                  (equal (treesit-node-type node) "def"))))
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      (insert "->")
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ((and (looking-back ", " (line-beginning-position))
           (memq (following-char) '(?\) ?\] ?})))
      (delete-char -2)
      (forward-char)
      (dwim-coder-skip-or-insert ?, t)
      t)
     ((or (bolp)
          (memq (preceding-char) '(?\s ?. ?\())
          (and node (equal (treesit-node-type node) "identifier"))
          (and node (equal (treesit-node-type node) "integer")))
      (dwim-coder-insert-interactive ?_)
      t))))

(defun dwim-coder-python-dwim-dot ()
  (cond
   ((eq (preceding-char) ?.)
    (delete-char -1)
    (dwim-coder-insert-interactive ?\()
    t)
   ((looking-back "^ *" (line-beginning-position))
    (dwim-coder-skip-or-insert ?@ t)
    t)
   ((and
     (dwim-coder-python-be-sane)
     (eq (preceding-char) ?\()
     (eq (following-char) ?\)))
    (delete-char -1)
    (delete-char 1)
    (insert "...")
    t)
   ((looking-back "^ *@" (line-beginning-position))
    (delete-char -1)
    (dwim-coder-insert-interactive ?\()
    t)))

(defun dwim-coder-python-dwim-comma ()
  (let ((node (treesit-node-at (dwim-coder-preceding-point))))
    (cond
     ((and (nth 3 (syntax-ppss))
           (setq node (treesit-node-top-level (treesit-node-at (point)) "^interpolation$")))
      (goto-char (treesit-node-end node))
      t)
     ((dwim-coder-default-be-sane)
      (dwim-coder-skip-or-insert ?\, t t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ((looking-back "^ *" (line-beginning-position))
      (dwim-coder-skip-or-insert ?# t nil)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ((looking-back ", ?" (line-beginning-position))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      (dwim-coder-skip-or-insert ?= t)
      t)
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (memq (preceding-char) '(?= ?- ?+ ?* ?/ ?% ?< ?> ?! ?^ ?| ?&)))
      (dwim-coder-insert-interactive ?=)
      t)
     (t
      (dwim-coder-insert-interactive ?\, t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t))))

(defun dwim-coder-python-dwim-semi ()
  (cond
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
   ;; return nil, so that the default handlers are run
   ((eolp)
    nil)
   (t
    (end-of-line)
    t)))


(defun dwim-coder-python-dwim-quote ()
  (let ((node (treesit-node-at (dwim-coder-preceding-point)))
        (value nil)
        (style nil))
    (when (and node
               (equal (treesit-node-type node) "identifier")
               (setq value (treesit-node-text node))
               (> (length value) 1))
      (delete-region (treesit-node-start node) (treesit-node-end node))
      (setq style (dwim-coder-s-get-style-case value))
      (if (equal style "snake")
          (insert (dwim-coder-s-to-style value "upcamel"))
        (if (equal style "upper-camel")
            (insert (dwim-coder-s-to-style value "upsnake"))
          (insert (dwim-coder-s-to-style value "snake"))))
      t)))

(defun dwim-coder-python-dwim-equal ()
  (let ((node nil))
    (cond
     ((treesit-node-top-level (treesit-node-at (dwim-coder-preceding-point)) "^type$")
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      (dwim-coder-insert-interactive ?\= t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ((and (setq node (treesit-node-parent (treesit-node-at (point))))
           (equal (treesit-node-type node) "argument_list"))
      (dwim-coder-skip-or-insert ?= t t)
      t)
     ((and (setq node (treesit-node-parent (treesit-node-at (point))))
           (equal (treesit-node-type node) "parameters"))
      (dwim-coder-skip-or-insert ?= t t)
      t)
     ((setq node (treesit-node-at (dwim-coder-preceding-point)))
      (if (equal (treesit-node-type node) "identifier")
          (if dwim-coder-auto-space
              (dwim-coder-skip-or-insert ?\s))
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point)))
      (dwim-coder-insert-interactive ?\= t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t))))

(defun dwim-coder-python-dwim-op (char)
  (let ((node (treesit-node-at (dwim-coder-preceding-point)))
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
      (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
      (dwim-coder-insert-interactive char t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t)
     ((and (equal (treesit-node-type node) "identifier")
           (memq char '(?= ?- ?+ ?* ?/ ?% ?< ?> ?! ?^ ?| ?&)))
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      (dwim-coder-skip-or-insert char t t)
      (if dwim-coder-auto-space
          (dwim-coder-skip-or-insert ?\s))
      t))))

(defun dwim-coder-python-override-self-insert (char)
  (pcase char
    (?\s (dwim-coder-python-dwim-space))
    (?. (dwim-coder-python-dwim-dot))
    (?, (dwim-coder-python-dwim-comma))
    (?\; (dwim-coder-python-dwim-semi))
    ((guard (dwim-coder-default-be-sane) nil))
    (?' (dwim-coder-python-dwim-quote))
    (?= (dwim-coder-python-dwim-equal))
    (_ (dwim-coder-python-dwim-op char))))

(provide 'dwim-coder-python)
;;; dwim-coder-python.el ends here
