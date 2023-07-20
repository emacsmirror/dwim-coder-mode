;;; dwim-coder-python.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

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
      ;; do interactively so that modes like electric-operator shall act on it
      (dwim-coder-insert-interactive ?- t)
      (dwim-coder-insert-interactive ?> t)
      t)
     ((or (bolp)
          (memq (preceding-char) '(?\s ?. ?\())
          (and
           (setq node (treesit-node-at (dwim-coder-preceding-point)))
           (or
            (equal (treesit-node-type node) "identifier")
            (equal (treesit-node-type node) "integer"))))
      (dwim-coder-insert-interactive ?_)
      t))))

(defun dwim-coder-python-dwim-dot ()
  (cond
   ((and (> dwim-coder-last-dot-point 0)
         (= (point) dwim-coder-last-dot-point))
    (setq dwim-coder-last-dot-point 0)
    ;; Replace last inserted space with _()
    (delete-char -1)
    (dwim-coder-insert-interactive ?\s)
    (dwim-coder-insert-interactive ?\()
    t)
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
    t)
   ;; On _. do SPC
   ((looking-back "[A-Za-z0-9]_" (line-beginning-position))
    (delete-char -1)
    (dwim-coder-insert-interactive ?\s t)
    (setq dwim-coder-last-dot-point (point))
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
      t)
     ((looking-back "\\([([]\\)\\|\\(, \\)" (line-beginning-position))
      (dwim-coder-skip-or-insert ?* t t)
      t)
     ((looking-back "\\([([][*]+\\)\\|\\(, [*]+\\)" (line-beginning-position))
      (if (eq (following-char) ?*)
          (forward-char)
        (insert "*"))
      t)
     ((looking-back "^ *" (line-beginning-position))
      (dwim-coder-skip-or-insert ?# t nil)
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
      t))))

(defun dwim-coder-python-dwim-semi ()
  (let ((node nil)
        (value nil))
    (cond
     ;; goto end of string if inside one
     ((nth 3 (syntax-ppss))
      (setq node (treesit-node-parent (treesit-node-at (point))))
      (goto-char (treesit-node-end node))
      t)
     ;; On empty lines, delete the line and go to the end of last line
     ((save-excursion (beginning-of-line)
                      (looking-at-p "^ *$"))
      (delete-line)
      ;; Don't warn if we are at the beginning of the buffer
      (ignore-errors (backward-char))
      t)
     ((eolp)
      ;; On lines with _ only, convert it to an empty line
      (when (looking-back "^ *_$" (line-beginning-position))
        (delete-line)
        (backward-char)
        (dwim-coder-insert-interactive ?\n))
      (dwim-coder-insert-interactive ?\n)
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
     (t
      (end-of-line)
      t))))


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

(defun dwim-coder-python-override-self-insert (char)
  (let ((last-dot-point dwim-coder-last-dot-point)
        (status nil))
    (setq status
          (pcase char
            (?\s (dwim-coder-python-dwim-space))
            (?. (dwim-coder-python-dwim-dot))
            (?, (dwim-coder-python-dwim-comma))
            (?\; (dwim-coder-python-dwim-semi))
            ((guard (dwim-coder-default-be-sane) nil))
            (?' (dwim-coder-python-dwim-quote))
            (_ (dwim-coder-common-dwim-op char))))

    ;; Reset last dot point cache if we didn't update
    (when (= last-dot-point dwim-coder-last-dot-point)
      (setq dwim-coder-last-dot-point 0))
    status))

(provide 'dwim-coder-python)
;;; dwim-coder-python.el ends here
