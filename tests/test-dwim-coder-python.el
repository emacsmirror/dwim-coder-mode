;; -*- lexical-binding:t -*-

(add-to-list 'load-path ".")
(load "./tests/test-common.el")

(require 'buttercup)
(require 'cl-lib)
(require 'dwim-coder-python)
(require 'dwim-coder-mode)

(defun dwim-coder-test-python-mode ()
  (setq python-indent-guess-indent-offset nil)
  (python-ts-mode)
  (electric-pair-mode)
  (setq dwim-coder-auto-space nil)
  (dwim-coder-mode))

(defun dwim-coder-test-auto-space-python-mode ()
  (setq python-indent-guess-indent-offset nil)
  (python-ts-mode)
  (electric-pair-mode)
  (setq dwim-coder-auto-space t)
  (dwim-coder-mode))

(defun test-files (mode-func directory file-name-regex)
  (let ((files nil)
        (in-file nil) (expected-file nil)
        (in-content nil) (expected-content nil)
        (result nil)
        (current-point nil))
    (setq files (get-matching-files directory file-name-regex))
    (dolist (elt files)
      (it (file-name-base (cdr elt))
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
            (setq result (test-buffer-with-content mode-func "test.py" in-content))

            (expect (nth 0 result) :to-equal expected-content)
            (unless (null current-point)
              (expect (nth 1 result) :to-equal current-point)))))))

(describe "dwim-coder-python-all"
  (let* ((file (concat (file-name-directory load-file-name) "python/basic-tests"))
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
          (test-content in expected 'dwim-coder-test-python-mode "test.py")
          (test-content in expected-with-space 'dwim-coder-test-auto-space-python-mode "test.py"))))))
