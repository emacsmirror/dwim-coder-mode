;;; dwim-coder-default.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; SPDX-License-Identifier: CC0-1.0
;; Created: 2022-12-22
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
;; Generic dwim hacks implemented for all major modes derived from prog-mode

;;; Code:


(defun dwim-coder-default-be-sane ()
  "Check if we should act sane at `point'."
  (cond
   ;; comments
   ((nth 4 (syntax-ppss))
    t)
   (t nil)))

(defun dwim-coder-default-is-xml-p ()
  (or (derived-mode-p 'sgml-mode 'nxml-mode)
      (string-match-p "\.\\(ui$\\)\\|\\(xml$\\)" (or (buffer-file-name) ""))))

(defun dwim-coder-default-dwim-space ()
  (cond
   ;; On ", )]}" move forward and insert comma
   ((and (looking-back ", " (line-beginning-position))
         (memq (following-char) '(?\) ?} ?\] )))
    (delete-char -2)
    (forward-char)
    (dwim-coder-insert-interactive ?,)
    t)
   ;; If folled by a valid variable replace SPC with _ or -
   ((save-excursion
      (and (< (skip-syntax-backward "w_" (pos-bol)) 0)
           (not (looking-at-p "[0-9.]"))))
    (cond
     (dwim-coder-space-char
      (insert-char dwim-coder-space-char))
     ((or (derived-mode-p 'css-mode 'lisp-data-mode)
          (dwim-coder-default-is-xml-p))
      (insert-char ?\-))
     (t
      (insert-char ?\_)))
    t)))

(defun dwim-coder-default-dwim-comma ()
  (cond
   ;; replace ,, with =
   ((looking-back ", ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    ;; In shell scripts ignore var,,|}
    (cond ((and (derived-mode-p 'sh-mode 'bash-ts-mode)
                (eq (following-char) ?})) nil)
          ;; don't insert space before = in xml
          ((dwim-coder-default-is-xml-p)
           (delete-char -1)
           (dwim-coder-insert-interactive ?=))
          (t
           (delete-char -1)
           (if dwim-coder-auto-space
               (dwim-coder-skip-or-insert ?\s))
           (dwim-coder-insert-interactive ?=)
           (if dwim-coder-auto-space
               (dwim-coder-skip-or-insert ?\s))))
    t)
   ((looking-back "[=!+^&*%|<>/-] ?" (line-beginning-position))
    (if (eq (preceding-char) ?\s)
        (delete-char -1))
    (dwim-coder-insert-interactive ?=)
    (if dwim-coder-auto-space
        (dwim-coder-skip-or-insert ?\s))
    t)
   (t
    (dwim-coder-insert-interactive ?, t)
    (if dwim-coder-auto-space
        (dwim-coder-skip-or-insert ?\s))
    t)))

(defun dwim-coder-default-dwim-dot ()
  ;; good is bad
  (cond
   ((and (eq (preceding-char) ?<)
         (eq (following-char) ?>)
         (dwim-coder-default-is-xml-p))
    (delete-char -1)
    (delete-char 1)
    (insert "...")
    t)
   ;; replace ..| with (|) or <|>
   ((and (eq (preceding-char) ?.)
         (not (eq (char-before (1- (point))) ?.)))
    (delete-char -1)
    (cond ((dwim-coder-default-is-xml-p)
           (dwim-coder-insert-interactive ?<)
           (unless (eq (following-char) ?>)
             (dwim-coder-insert-interactive ?>)
             (backward-char)))
          ((derived-mode-p 'lisp-data-mode)
           (unless (or (bolp)
                       (memq (preceding-char) '(?\s ?\( ?\' ?\` ?\, ?\@)))
             (if dwim-coder-auto-space
                 (dwim-coder-skip-or-insert ?\s)))
           (dwim-coder-insert-interactive ?\())
          (t
           (dwim-coder-insert-interactive ?\()))
    t)))

(defun dwim-coder-default-dwim-semi-colon ()
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

(defun dwim-coder-default-override-self-insert (char)
  (pcase char
    ((guard (dwim-coder-default-be-sane) nil))
    (?\; (dwim-coder-default-dwim-semi-colon))
    ;; skip in strings
    ((guard (nth 3 (syntax-ppss)) nil))
    (?\s (dwim-coder-default-dwim-space))
    (?\, (dwim-coder-default-dwim-comma))
    (?. (dwim-coder-default-dwim-dot))))

(provide 'dwim-coder-default)
;;; dwim-coder-default.el ends here
