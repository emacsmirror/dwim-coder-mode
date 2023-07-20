;;; dwim-coder-elisp.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

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


(defun dwim-coder-elisp-be-sane ()
  "Check if we should act sane at `point'."
  (cond
   ;; comments
   ((nth 4 (syntax-ppss))
    t)
   (t nil)))

(defun dwim-coder-elisp-dwim-space ()
  (cond
   ((memq (preceding-char) '(?\) ?\s))
    (dwim-coder-insert-interactive ?\s t)
    t)
   (t
    (dwim-coder-insert-interactive ?-)
    t) ))

(defun dwim-coder-elisp-dwim-dot ()
  (cond
   ((eq (preceding-char) ?-)
    (delete-char -1)
    (dwim-coder-insert-interactive ?\s t)
    t)
   ((eq (preceding-char) ?.)
    (delete-char -1)
    (unless (or (bolp)
                (memq (preceding-char) '(?\()))
      (dwim-coder-skip-or-insert ?\s))
    (dwim-coder-insert-interactive ?\()
    (indent-according-to-mode)
    t)))

(defun dwim-coder-elisp-dwim-semi-colon ()
  (cond
   ;; goto end of string if inside one
   ((nth 3 (syntax-ppss))
    (skip-syntax-forward "^\"")
    (forward-char)
    t)
   ((and (looking-back ")[ -]")
         (eq (following-char) ?\)))
    (delete-char -1)
    (dwim-coder-insert-interactive ?\n)
    t)
   ((and (eq (preceding-char) ?-)
         (looking-at "[) ]"))
    (delete-char -1)
    (dwim-coder-insert-interactive ?\n)
    t)
   ((and (looking-back "^ *" (line-beginning-position))
         (looking-at-p " *)"))
    (delete-indentation)
    t)
   ((and (setq value (save-excursion
                       (ignore-errors (up-list))
                       (point)))
         (> value (point))
         (< value (line-end-position)))
    (up-list)
    t)
   ((not (eolp))
    (end-of-line)
    t)
   ((save-excursion (beginning-of-line)
                    (looking-at-p "^ *$"))
    (delete-line)
    ;; Don't warn if we are at the beginning of the buffer
    (ignore-errors (backward-char))
    t)
   ((eolp)
    (if (looking-back "^ *-$" (line-beginning-position))
        (delete-region (line-beginning-position) (line-end-position))
      (if (looking-back "[a-zA-Z-]-" (line-beginning-position))
          (delete-char -1)))
    (dwim-coder-insert-interactive ?\n)
    t)))

(defun dwim-coder-elisp-override-self-insert (char)
  (pcase char
    ((guard (dwim-coder-elisp-be-sane) nil))
    (?\; (dwim-coder-elisp-dwim-semi-colon))
    ;; skip in strings
    ((guard (nth 3 (syntax-ppss)) nil))
    (?\s (dwim-coder-elisp-dwim-space))
    (?. (dwim-coder-elisp-dwim-dot))))

(provide 'dwim-coder-elisp)
;;; dwim-coder-elisp.el ends here
