;;; crazy-default.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0
;; Created: 2022-12-22
;; Last-Updated: 2023-04-29

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; Generic dwim hacks implemeted for all major modes derived from prog-mode

;;; Code:


(defun crazy-default-be-sane ()
  "Check if we should act sane at `point'."
  (cond
   ;; comments
   ((nth 4 (syntax-ppss))
    t)
   (t nil)))

(defun crazy-default-is-xml-p ()
  (or (derived-mode-p 'sgml-mode 'nxml-mode)
      (string-match-p "\.\\(ui$\\)\\|\\(xml$\\)" (or (buffer-file-name) ""))))

(defun crazy-default-dwim-space ()
  (cond
   ;; On ", )]}" move forward and insert comma
   ((and (looking-back ", " (line-beginning-position))
         (memq (following-char) '(?\) ?} ?\] )))
    (delete-char -2)
    (forward-char)
    (crazy-insert-interactive ?,)
    t)
   ;; If folled by a valid variable replace SPC with _ or -
   ((save-excursion
      (and (< (skip-syntax-backward "w_" (pos-bol)) 0)
           (not (looking-at-p "[0-9.]"))))
    (cond
     (crazy-space-char
      (insert-char crazy-space-char))
     ((or (derived-mode-p 'css-mode 'lisp-data-mode)
          (crazy-default-is-xml-p))
      (insert-char ?\-))
     (t
      (insert-char ?\_)))
    t)))

(defun crazy-default-dwim-comma ()
  (cond
   ;; replace ,, with =
   ((looking-back ", ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    ;; In shell scripts ignore var,,|}
    (cond ((and (derived-mode-p 'sh-mode 'bash-ts-mode)
                (eq (following-char) ?})) nil)
          ;; don't insert space before = in xml
          ((crazy-default-is-xml-p)
           (delete-char -1)
           (crazy-insert-interactive ?=))
          (t
           (delete-char -1)
           (if crazy-auto-space
               (crazy-skip-or-insert ?\s))
           (crazy-insert-interactive ?=)
           (if crazy-auto-space
               (crazy-skip-or-insert ?\s))))
    t)
   ((looking-back "[=!+^&*%|<>/-] ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (crazy-insert-interactive ?=)
    (if crazy-auto-space
        (crazy-skip-or-insert ?\s))
    t)
   (t
    (crazy-insert-interactive ?, t)
    (if crazy-auto-space
        (crazy-skip-or-insert ?\s))
    t)))

(defun crazy-default-dwim-dot ()
  (cond
   ((and (eq (preceding-char) ?<)
         (eq (following-char) ?>)
         (crazy-default-is-xml-p))
    (delete-char -1)
    (delete-char 1)
    (insert "...")
    t)
   ;; replace ..| with (|) or <|>
   ((and (eq (preceding-char) ?.)
         (not (eq (char-before (1- (point))) ?.)))
    (delete-char -1)
    (cond ((crazy-default-is-xml-p)
           (crazy-insert-interactive ?<)
           (unless (eq (following-char) ?>)
             (crazy-insert-interactive ?>)
             (backward-char)))
          ((derived-mode-p 'lisp-data-mode)
           (unless (or (bolp)
                       (memq (preceding-char) '(?\s ?\( ?\' ?\` ?\, ?\@)))
             (if crazy-auto-space
                 (crazy-skip-or-insert ?\s)))
           (crazy-insert-interactive ?\())
          (t
           (crazy-insert-interactive ?\()))
    t)))

(defun crazy-default-dwim-semi-colon ()
  (cond
   ;; Move forward if inside empty pairs
   ((and (not (bolp))
         (not (eolp))
         (save-excursion
           (backward-char)
           (looking-at-p "\\(()\\)\\|\\(\\[\\]\\)\\|\\({}\\)\\|\\(<>\\)")))
    (forward-char)
    t)
   ((not (eolp))
    (end-of-line)
    t)
   ;; ignore further actions if in string
   ((nth 3 (syntax-ppss))
    nil)
     ;; Skip to the end of the current statement as possible
   ((and (memq (preceding-char) '(?, ?\{ ?\[ ?\())
         (memq (char-after (nth 1 (syntax-ppss))) '(?\( ?\[ ?\{)))
    (goto-char (nth 1 (syntax-ppss)))
    (forward-sexp)
    t)))

(defun crazy-default-override-self-insert (char)
  (pcase char
    ((guard (crazy-default-be-sane) nil))
    (?\; (crazy-default-dwim-semi-colon))
    ;; skip in strings
    ((guard (nth 3 (syntax-ppss)) nil))
    (?\s (crazy-default-dwim-space))
    (?\, (crazy-default-dwim-comma))
    (?. (crazy-default-dwim-dot))))

(provide 'crazy-default)
;;; crazy-default.el ends here
