;;; dwim-coder-xml.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

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
;; Generic dwim hacks implemented for html/xml mode

;;; Code:


(defun dwim-coder-xml-be-sane ()
  "Check if we should act sane at `point'."
  (cond
   ;; comments
   ((nth 4 (syntax-ppss))
    t)
   (t nil)))

(defun dwim-coder-xml-dwim-space ()
  (cond
   ((nth 3 (syntax-ppss))
    (dwim-coder-insert-interactive ?- t)
    t)
   ;; Mark the point so that if ; is immediately followed, we can insert \n
   ((looking-back "^ *$" (line-beginning-position))
    (dwim-coder-insert-interactive ?\s t)
    (setq dwim-coder-last-space-point (point))
    t)))

(defun dwim-coder-xml-dwim-dot ()
  (cond
   ((eq (preceding-char) ?-)
    (delete-char -1)
    (dwim-coder-insert-interactive ?\s t)
    t)
   ((eq (preceding-char) ?.)
    (delete-char -1)
    (dwim-coder-insert-interactive ?\<)
    (dwim-coder-insert-interactive ?\>)
    (indent-according-to-mode)
    (backward-char)
    t)
   ((and (eq (preceding-char) ?<)
         (eq (following-char) ?>))
    (delete-char -1)
    (delete-char 1)
    (dwim-coder-insert-interactive ?. t)
    (dwim-coder-insert-interactive ?. t)
    (dwim-coder-insert-interactive ?. t)
    t)))

(defun dwim-coder-xml-dwim-comma ()
  (cond
   ((eq (preceding-char) ?\,)
    (delete-char -1)
    (dwim-coder-skip-or-insert ?= t)
    ;; web-mode may auto insert quotes
    ;; fixme: This is not supposed to break though, but it does
    (unless (derived-mode-p 'web-mode)
      (dwim-coder-skip-or-insert ?\" t))
    t)))

(defun dwim-coder-xml-dwim-semi-colon ()
  (cond
   ;; goto end of string if inside one
   ((nth 3 (syntax-ppss))
    (skip-syntax-forward "^\"")
    (forward-char)
    t)
   ((not (eolp))
    (end-of-line)
    t)
   ((and (> dwim-coder-last-space-point 0)
         (= (point) dwim-coder-last-space-point)
         (looking-back "^ *" (line-beginning-position)))
    (dwim-coder-insert-interactive ?\n)
    (setq dwim-coder-last-space-point 0)
    t)
   ((save-excursion (beginning-of-line)
                    (looking-at-p "^ *$"))
    (delete-line)
    ;; Don't warn if we are at the beginning of the buffer
    (ignore-errors (backward-char))
    (indent-according-to-mode)
    t)
   ((eolp)
    (dwim-coder-insert-interactive ?\n)
    t)))

(defun dwim-coder-xml-override-self-insert (char)
  (pcase char
    (?\; (dwim-coder-xml-dwim-semi-colon))
    ((guard (dwim-coder-xml-be-sane) nil))
    (?\s (dwim-coder-xml-dwim-space))
    (?. (dwim-coder-xml-dwim-dot))
    ;; skip in strings
    ((guard (nth 3 (syntax-ppss)) nil))
    (?, (dwim-coder-xml-dwim-comma))))

(provide 'dwim-coder-xml)
;;; dwim-coder-xml.el ends here
