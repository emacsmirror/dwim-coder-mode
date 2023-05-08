;;; crazy-mode.el --- Crazy ways to code  -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>

;; Version: 0.0.1
;; SPDX-License-Identifier: CC0-1.0
;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; URL: https://sadiqpk.org/projects/crazy-mode.html
;; Package-Requires: ((emacs "29"))
;; Last-Updated: 2023-04-29
;; Keywords: convenience, hacks

;; This file is NOT part of GNU Emacs.

;; To the extent possible under law, the author(s) have dedicated all
;; copyright and related and neighboring rights to this software to
;; the public domain worldwide.  This software is distributed WITHOUT
;; ANY WARRANTY.

;; You should have received a copy of the CC0 Public Domain Dedication
;; along with this software.  If not, see
;; <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; This minor modes adds various dwim keyboard shortcuts to your Emacs.

;;; Code:

(require 'crazy-default)
(require 'crazy-c)
(require 'crazy-python)
(require 'crazy-rust)

(defun crazy-insert-space ()
  "Interactvely insert SPC."
  (interactive)
  (crazy-insert-interactive ?\s t))

(cl-defun crazy-pre-self-insert-function (&rest args)
  (let ((val nil)
        (last-was-camel crazy-last-was-camel))

    ;; Override only character inputs with no prefixes
    (unless (and (eq (car args) 1)
                 (eq (type-of (cdr args)) 'cons)
                 (eq (type-of (cadr args)) 'integer))
      (setq crazy-last-was-camel nil)
      (cl-return-from crazy-pre-self-insert-function nil))

    (cond
     (crazy-skip nil)
     ((not crazy-mode) nil)
     ((derived-mode-p 'c-ts-mode)
      (if (treesit-language-available-p 'c)
          (setq val (crazy-c-override-self-insert (cadr args)))
        (error "`treesitter' not available for C")))
     ((derived-mode-p 'rust-ts-mode)
      (if (treesit-language-available-p 'rust)
          (setq val (crazy-rust-override-self-insert (cadr args)))
        (error "`treesitter' not available for Rust")))
     ((derived-mode-p 'python-ts-mode)
      (if (treesit-language-available-p 'python)
          (setq val (crazy-python-override-self-insert (cadr args)))
        (error "`treesitter' not available for Python")))
     ((derived-mode-p 'prog-mode 'conf-mode 'text-mode)
      (setq val (crazy-default-override-self-insert (cadr args)))))
    ;; Reset only if the variable was set in some past call,
    ;; not on changes made in this call.
    (if last-was-camel
        (setq crazy-last-was-camel nil))
    val))

;;;###autoload
(define-minor-mode crazy-mode
  "Toggle Crazy ways for coding (Crazy mode).

With a prefix argument ARG, enable crazy mode if ARG is positive, and
disable it otherwise.  If called from Lisp, enable the mode if ARG is
omitted or nil.

This mode shall allow you to code in crazy ways.  Currently ‘c-ts-mode’,
`python-ts-mode' and `rust-ts-mode' are supported well.
Other modes may be supported, but not well tested.

Please note: If you don’t know what this mode does, this mode shall
drive you crazy, rather than helping you code crazy.

This is a local minor mode."
  :global nil
  :lighter " !"
  :group 'crazy
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-SPC") 'crazy-insert-space)
            map)
  (if crazy-mode
      (advice-add #'self-insert-command :before-until 'crazy-pre-self-insert-function)
    (advice-remove #'self-insert-command 'crazy-pre-self-insert-function)))

(provide 'crazy-mode)
;;; crazy-mode.el ends here
