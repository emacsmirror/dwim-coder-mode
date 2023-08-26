;;; dwim-coder-lisp.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

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
;; Generic dwim hacks implemented for all major modes derived from prog-mode

;;; Code:

(require 'dwim-coder-common)

(defun dwim-coder-lisp-be-sane ()
  "Check if we should act sane at `point'."
  (cond
   ;; comments
   ((nth 4 (syntax-ppss))
    t)
   (t nil)))

(defun dwim-coder-lisp-dwim-space ()
  (cond
   ((memq (preceding-char) '(?\) ?\"))
    (dwim-coder-insert-interactive ?\s t)
    t)
   ((looking-back " [.]" (line-beginning-position))
    (dwim-coder-insert-interactive ?\s t)
    t)
   (t
    (dwim-coder-insert-interactive ?-)
    (setq dwim-coder-last-space-point (point))
    t)))

(defun dwim-coder-lisp-dwim-dot ()
  (cond
   ((eq (preceding-char) ?-)
    (delete-char -1)
    (dwim-coder-insert-interactive ?\s t)
    t)
   ((eq (preceding-char) ?.)
    (delete-char -1)
    (when (looking-back "[a-zA-Z0-9-)\"]" (line-beginning-position))
      (dwim-coder-skip-or-insert ?\s))
    (dwim-coder-insert-interactive ?\()
    (indent-according-to-mode)
    t)))

(defun dwim-coder-lisp-dwim-comma ()
  (cond
   ((eq (preceding-char) ?\()
    (dwim-coder-skip-or-insert ?& t)
    t)
   ((looking-back "[a-zA-Z0-9]" (line-beginning-position))
    (dwim-coder-skip-or-insert ?\s t t)
    (dwim-coder-skip-or-insert ?& t)
    t)
   ((eq (preceding-char) ?\-)
    (delete-char -1)
    (dwim-coder-skip-or-insert ?\s)
    (dwim-coder-insert-interactive ?, t)
    t)))

(defun dwim-coder-lisp-dwim-semi-colon ()
  (let ((value nil))
    (cond
     ((and (bobp) (eolp))
      (dwim-coder-insert-interactive ?\; t)
      t)
     ((save-excursion
        (beginning-of-line)
        (and (looking-at-p "^[ \t]*$")
             (or (backward-char)
                 (nth 4 (syntax-ppss)))))
      (dwim-coder-insert-interactive ?\; t)
      t)
     ((and (looking-back "(1?-" (line-beginning-position))
           (looking-at-p "[ (]"))
      (if (and (setq value (save-excursion
                             (ignore-errors (up-list))
                             (point)))
               (> value (point))
               (< value (line-end-position)))
          (up-list)
        (end-of-line))
      t)
     ((and (looking-back ")[ -]" (line-beginning-position))
           (eq (following-char) ?\)))
      (delete-char -1)
      (dwim-coder-insert-interactive ?\n)
      t)
     ((and (eq (preceding-char) ?-)
           (looking-at "[) ]")
           (not (looking-back "^ *-" (line-beginning-position))))
      (delete-char -1)
      (dwim-coder-insert-interactive ?\n)
      t)
     ((and (setq value (save-excursion
                         (ignore-errors (up-list))
                         (point)))
           (> value (point))
           (< value (line-end-position)))
      (up-list)
      t)
     (t
      (dwim-coder-common-dwim-semi-colon)))))

(defun dwim-coder-lisp-dwim-op (char)
  (cond
   ((and (eq (preceding-char) ?\-)
         (memq char '(?\' ?\" ?,)))
    (delete-char -1)
    (dwim-coder-skip-or-insert ?\s)
    (dwim-coder-insert-interactive char t)
    t)))

(defun dwim-coder-lisp-override-self-insert (char)
  (pcase char
    ((guard (dwim-coder-lisp-be-sane) nil))
    (?\; (dwim-coder-lisp-dwim-semi-colon))
    ;; skip in strings
    ((guard (nth 3 (syntax-ppss)) nil))
    (?\s (dwim-coder-lisp-dwim-space))
    (?. (dwim-coder-lisp-dwim-dot))
    (?, (dwim-coder-lisp-dwim-comma))
    (_ (dwim-coder-lisp-dwim-op char))))

(provide 'dwim-coder-lisp)
;;; dwim-coder-lisp.el ends here
