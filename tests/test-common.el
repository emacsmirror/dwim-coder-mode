;; -*- lexical-binding:t -*-

(defun create-file-cons (file-name)
  (cons (concat (replace-regexp-in-string "[^/]*/[^/-]*-\\([a-z-]*\\)[0-9]+$" "" file-name nil nil 1) ".in")
        (concat file-name ".expected")))

(defun get-matching-files (directory file-name-regex)
  (cl-assert (string-suffix-p ".expected$" file-name-regex))
  (let ((files (mapcar 'file-name-sans-extension
                       (directory-files directory t file-name-regex))))
    (mapcar 'create-file-cons files)))

(defun get-file-contents (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-buffer-with-content (mode filename in)
  (let ((buffer (generate-new-buffer filename))
        (str nil)
        (insert-point nil))
    (with-current-buffer buffer
      (funcall mode)
      (when (setq insert-point (string-match "❪\\(.*\\)❫" in))
        ;; the string match starts from 0, while (point) starts from 1.
        (setq insert-point (1+ insert-point))
        (setq str (match-string-no-properties 1 in))
        (setq in (replace-match "" t t in)))
      (if (null str)
          (mapc 'crazy-insert-interactive in)
        (insert in)
        ;; Set mode again, so that file variables, if any, shall be parsed
        (funcall mode)
        (goto-char insert-point)
        (mapc 'crazy-insert-interactive str))
      (list (buffer-substring-no-properties (point-min) (point-max)) (point)))))

(defun test-content (in expected mode-func buffer-name)
  (let ((result nil) (p nil))
    (when (setq p (string-match "❚" expected))
      ;; the string match starts from 0, while (point) starts from 1.
      (setq p (1+ p))
      (setq expected (string-replace "❚" "" expected)))
    (setq result (test-buffer-with-content mode-func buffer-name in))

    (expect (nth 0 result) :to-equal expected)
    (unless (null p)
      (expect (nth 1 result) :to-equal p))))
