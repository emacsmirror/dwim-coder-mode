;; -*- lexical-binding:t -*-

(add-to-list 'load-path ".")
(load "./tests/test-common.el")

(require 'buttercup)
(require 'cl-lib)
(require 'crazy-c)
(require 'crazy-mode)

(defun crazy-test-rust-mode ()
  (rust-ts-mode)
  (electric-pair-mode)
  (setq crazy-auto-space nil)
  (crazy-mode))

(defun crazy-test-auto-space-rust-mode ()
  (rust-ts-mode)
  (electric-pair-mode)
  (setq crazy-auto-space t)
  (crazy-mode))


(defun test-files (mode-func mode directory file-name-regex)
  (let ((files nil)
        (in-file nil) (expected-file nil)
        (in-content nil) (expected-content nil)
        (result nil)
        (current-point nil))
    (setq files (get-matching-files directory file-name-regex))
    (dolist (elt files)
      (it (concat mode " - " (file-name-base (cdr elt)))
        (setq in-file (car elt)
              expected-file (cdr elt))
        (if (file-exists-p (concat in-file ".skip"))
            (signal 'buttercup-pending "skipped")
          (setq in-content (get-file-contents in-file)
                expected-content (get-file-contents expected-file))

          (when (setq current-point (string-match "❚" expected-content))
            ;; the string match starts from 0, while (point) starts from 1.
            (setq current-point (1+ current-point))
            (setq expected-content (string-replace "❚" "" expected-content)))
          (setq result (test-buffer-with-content mode-func "test.rs" in-content))

          (expect (nth 0 result) :to-equal expected-content)
          (unless (null current-point)
            (expect (nth 1 result) :to-equal current-point)))))))

(describe "crazy-rust-all"
  (let* ((file (concat (file-name-directory load-file-name) "rust/basic-tests"))
         (content (get-file-contents file))
         (lines (split-string content "\n" t)))
    ;; skip the first line
    (dolist (line (cdr lines))
      (it line
        (setq items (split-string line "‖" t))
        (setq in (nth 0 items)
              expected (nth 1 items)
              expected-with-space (nth 2 items))
        (if (not (string-match-p "^‖" line))
            (signal 'buttercup-pending "skipped")
          (test-content in expected 'crazy-test-rust-mode "test.rs")
          (test-content in expected-with-space 'crazy-test-auto-space-rust-mode "test.rs"))))))
