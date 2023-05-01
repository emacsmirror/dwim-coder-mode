;; -*- lexical-binding:t -*-

(add-to-list 'load-path ".")
(load "./tests/test-common.el")

(require 'buttercup)
(require 'cl-lib)
(require 'crazy-c)
(require 'crazy-mode)

(defun crazy-test-c-mode (style &optional sub-style auto-space)
  (c-ts-mode)
  (setq c-ts-mode-indent-style style)
  (electric-pair-mode)
  (crazy-mode)
  (setq crazy-c-sub-style sub-style)
  (setq crazy-auto-space auto-space))

(defun crazy-test-gnu-mode ()
  (crazy-test-c-mode 'gnu))

(defun crazy-test-gnu-space-mode ()
  (crazy-test-c-mode 'gnu nil t))

(defun crazy-test-linux-mode ()
  (crazy-test-c-mode 'linux))

(defun crazy-test-gnu-gnome-mode ()
  (crazy-test-c-mode 'gnu 'gnome))

(defun crazy-test-linux-gnome-mode ()
  (crazy-test-c-mode 'linux 'gnome))


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
          (setq result (test-buffer-with-content mode-func "test.c" in-content))

          (expect (nth 0 result) :to-equal expected-content)
          (unless (null current-point)
            (expect (nth 1 result) :to-equal current-point))
          )))
    ))

(describe "crazy-c-basic"
  (let* ((file (concat (file-name-directory load-file-name) "c/basic-tests"))
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
          (test-content in expected 'crazy-test-gnu-mode "test.c")
          (test-content in expected-with-space 'crazy-test-gnu-space-mode "test.c")))
      )))

(describe "crazy-c-main"
  (let* ((file (concat (file-name-directory load-file-name) "c/main-tests"))
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
             ((string-match-p " *gnu-none" item)
              (setq func 'crazy-test-gnu-mode))
             ((string-match-p " *linux-none" item)
              (setq func 'crazy-test-linux-mode))
             ((string-match-p " *gnu-gnome" item)
              (setq func 'crazy-test-gnu-gnome-mode))
             ((string-match-p " *linux-gnome" item)
              (setq func 'crazy-test-linux-gnome-mode))
             (t
              (error "Invalid test case")))
            (if (not (bound-and-true-p func))
                (signal 'buttercup-pending "skipped")
              (setq expected (substring item (1+ (string-match "\n" item))))
              (setq expected (substring expected 0 -1))
              (test-content in expected func "test.c")))))
      ))
  )
