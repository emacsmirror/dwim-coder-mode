;; -*- lexical-binding:t -*-

(add-to-list 'load-path ".")
(load "./tests/test-common.el")

(require 'buttercup)
(require 'cl-lib)
(require 'dwim-coder-xml)
(require 'dwim-coder-mode)

(defun dwim-coder-test-xml-mode ()
  (setq xml-indent-guess-indent-offset nil)
  (sgml-mode)
  (electric-pair-mode)
  (setq dwim-coder-auto-space nil)
  (dwim-coder-mode))

(defun dwim-coder-test-auto-space-xml-mode ()
  (setq xml-indent-guess-indent-offset nil)
  (sgml-mode)
  (electric-pair-mode)
  (setq dwim-coder-auto-space t)
  (dwim-coder-mode))

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
          (setq result (test-buffer-with-content mode-func "test.xml" in-content))

          (expect (nth 0 result) :to-equal expected-content)
          (unless (null current-point)
            (expect (nth 1 result) :to-equal current-point))
          )))
    ))

(describe "dwim-coder-xml-basic"
  (let* ((file (concat (file-name-directory load-file-name) "xml/basic-tests"))
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
          (when (and expected-with-space
                     (not (string-empty-p expected-with-space)))
            (test-content in expected-with-space 'dwim-coder-test-auto-space-xml-mode "test.xml"))
          (test-content in expected 'dwim-coder-test-xml-mode "test.xml"))))))

(describe "dwim-coder-xml-main"
  (let* ((file (concat (file-name-directory load-file-name) "xml/main-tests"))
         (content (get-file-contents file))
         (test-cases (split-string content "‖‖")))
    (dolist (test-case (cdr test-cases))
      (it (substring test-case 1 (string-match "\n" test-case))
        (setq items (split-string test-case "‖"))
        (setq in (substring (nth 0 items) (1+ (string-match "\n" (nth 0 items)))))
        ;; Remove trailing \n
        (setq in (substring in 0 -1))
        (if (string-match-p "^[ ]*-" test-case)
            (signal 'buttercup-pending "skipped")
          (dolist (item (cdr items))
            (cond
             ((string-match-p "^-" item)
              (setq func nil))
             ((string-match-p " *with-space" item)
              (setq func 'dwim-coder-test-auto-space-xml-mode))
             (t
              (error "Invalid test case")))
            (if (not (bound-and-true-p func))
                (signal 'buttercup-pending "skipped")
              (setq expected (substring item (1+ (string-match "\n" item))))
              (setq expected (substring expected 0 -1))
              (test-content in expected func "test.xml"))))))))
