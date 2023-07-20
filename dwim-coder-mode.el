;;; dwim-coder-mode.el --- DWIM keybindings for C, Python, Rust, and more -*- lexical-binding: t; -*-

;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>

;; Version: 0.0.1
;; SPDX-License-Identifier: CC0-1.0
;; Author: Mohammed Sadiq <sadiq@sadiqpk.org>
;; URL: https://sadiqpk.org/projects/dwim-coder-mode.html
;; Package-Requires: ((emacs "29"))
;; Last-Updated: 2023-07-13
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
;; Many keys like SPC, and punctuation keys are intercepted by this mode
;; to provide alternate actions as you type.
;;
;; Say for example, if you type `g clear object.. self  person;;' in `c-ts-mode',
;; you shall get `g_clear_object (&self->person);'.

;;; Code:

(require 'dwim-coder-default)
(require 'dwim-coder-c)
(require 'dwim-coder-elisp)
(require 'dwim-coder-python)
(require 'dwim-coder-rust)

(defun dwim-coder-insert-space ()
  "Interactvely insert SPC."
  (interactive)
  (dwim-coder-insert-interactive ?\s t))

(cl-defun dwim-coder-pre-self-insert-function (&rest args)
  (let ((val nil)
        (last-was-camel dwim-coder-last-was-camel))

    ;; Override only character inputs with no prefixes
    (unless (and (eq (car args) 1)
                 (eq (type-of (cdr args)) 'cons)
                 (eq (type-of (cadr args)) 'integer))
      (setq dwim-coder-last-was-camel nil)
      (cl-return-from dwim-coder-pre-self-insert-function nil))

    (cond
     (dwim-coder-skip nil)
     ((not dwim-coder-mode) nil)
     ((derived-mode-p 'c-ts-mode)
      (if (treesit-language-available-p 'c)
          (setq val (dwim-coder-c-override-self-insert (cadr args)))
        (error "`treesitter' not available for C")))
     ((derived-mode-p 'emacs-lisp-mode)
      (setq val (dwim-coder-elisp-override-self-insert (cadr args))))
     ((derived-mode-p 'rust-ts-mode)
      (if (treesit-language-available-p 'rust)
          (setq val (dwim-coder-rust-override-self-insert (cadr args)))
        (error "`treesitter' not available for Rust")))
     ((derived-mode-p 'python-ts-mode)
      (if (treesit-language-available-p 'python)
          (setq val (dwim-coder-python-override-self-insert (cadr args)))
        (error "`treesitter' not available for Python")))
     ((derived-mode-p 'prog-mode 'conf-mode 'text-mode)
      (setq val (dwim-coder-default-override-self-insert (cadr args)))))
    ;; Reset only if the variable was set in some past call,
    ;; not on changes made in this call.
    (if last-was-camel
        (setq dwim-coder-last-was-camel nil))
    val))

;;;###autoload
(define-minor-mode dwim-coder-mode
  "Toggle DWIM keybindings for C, Python, Rust and others.

With a prefix argument ARG, enable dwim-coder-mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode if ARG
is omitted or nil.

This mode shall allow you to code in crazy ways.  Currently ‘c-ts-mode’,
`python-ts-mode' and `rust-ts-mode' are supported well.
Other modes may be supported, but not well tested.

Please note: If you don’t know what this mode does, this mode shall
drive you crazy, rather than helping you code crazy.

This is a local minor mode."
  :global nil
  :lighter " !"
  :group 'dwim-coder
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-SPC") 'dwim-coder-insert-space)
            map)
  (if dwim-coder-mode
      (advice-add #'self-insert-command :before-until 'dwim-coder-pre-self-insert-function)
    (advice-remove #'self-insert-command 'dwim-coder-pre-self-insert-function)))

(provide 'dwim-coder-mode)
;;; dwim-coder-mode.el ends here
