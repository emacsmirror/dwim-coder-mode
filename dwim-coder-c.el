;;; dwim-coder-c.el --- DWIM keybindings for programming modes -*- lexical-binding: t; -*-

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
;; dwim hacks implemented for `c-ts-mode'

;;; Code:

(require 'dwim-coder-common)
(require 'cl-lib)
(require 'treesit)

(defvar c-ts-mode--operators)
(defvar c-ts-mode-indent-style)

(defcustom dwim-coder-c-sub-style nil
  "Sub style to be used spacing."
  :type '(choice (symbol :tag "None" nil)
                 (symbol :tag "GNOME" 'gnome))
  :group 'dwim-coder)


(defun dwim-coder-c-incomplete-for-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-top-level (treesit-node-at p) "ERROR")))
    (and (equal (treesit-node-type node) "ERROR")
         (setq node (treesit-node-at (treesit-node-start node)))
         (equal (treesit-node-type node) "for")
         ;; Skip if point is above "for"
         (if (eq (char-after p) ?\()
             (setq p (max 1 (1- p)))
           t)
         (setq node (treesit-node-at p))
         (not (equal (treesit-node-type node) "for")))))

(defun dwim-coder-c-can-paren-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p))
         (case-fold-search nil))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (dwim-coder-preceding-point)))
      (setq node (treesit-node-at (dwim-coder-preceding-point))))
    ;; allow parens everywhere except in function declarations
    (unless (treesit-node-top-level node "^function_declarator$")
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t)
            ;; Whether to enforce no-space before '('
            (or (equal (treesit-node-type node) "sizeof")
                (string-match-p "^[QCN]?[C]?_$" (treesit-node-text node t)))
            ;; Whether to enforce space before '('
            (member (treesit-node-type node)
                    '("if" "for" "while" "switch"))))))

(cl-defun dwim-coder-c-skip-semi ()
  (let ((node nil)
        (skip-semi nil))
    (when (eq (preceding-char) ?\})
      (save-excursion
        (backward-sexp)
        (skip-chars-backward "[ \n]")
        (setq node (treesit-node-at (dwim-coder-preceding-point)))
        (when (member (treesit-node-type node) '("=" "enum"))
          (cl-return-from dwim-coder-c-skip-semi nil))
        (setq node (treesit-node-parent (treesit-node-at (dwim-coder-preceding-point))))
        (when (equal (treesit-node-type node) "struct_specifier")
          (cl-return-from dwim-coder-c-skip-semi nil)))
        (setq skip-semi t))
    (when (eq (preceding-char) ?\))
      (save-excursion
        (backward-sexp)
        (skip-chars-backward "[ \n]")
        (setq node (treesit-node-at (dwim-coder-preceding-point)))
        (when (member (treesit-node-type node) '("if" "for"))
          (cl-return-from dwim-coder-c-skip-semi t))
        (setq node (treesit-node-parent (treesit-node-at (dwim-coder-preceding-point))))
        (when (equal (treesit-node-type node) "while_statement")
          (cl-return-from dwim-coder-c-skip-semi t))))
    skip-semi))

(defun dwim-coder-c-op-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p))
         (operators c-ts-mode--operators)
         (can-append-equal nil)
         (type  nil))
    ;; Remove operators we don't care
    (setq operators (remove "." operators))
    (setq operators (remove "->" operators))
    (when (save-excursion
            (goto-char p)
            (looking-back "[<>%^&*/!+-]" (dwim-coder-preceding-point)))
      (setq node (treesit-node-at (dwim-coder-preceding-point))))
    (setq type (treesit-node-type node))
    (when (member type operators)
      (when (or (equal type "=")
                (and (not (string-suffix-p "=" type))
                     (not (member type '("&&" "||" "--" "++")))))
        (setq can-append-equal t))
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t) can-append-equal))))

(defun dwim-coder-c-type-identifier-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p)))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (dwim-coder-preceding-point)))
      (setq node (treesit-node-at (dwim-coder-preceding-point)))
      (if (equal (treesit-node-type node) "primitive_type")
          (list (treesit-node-start node) (treesit-node-end node)
                (treesit-node-text node t))
        (if (and (equal (treesit-node-type node) "type_identifier")
                 (or (string-suffix-p "_" (treesit-node-text node t))
                     (string-suffix-p "_t" (treesit-node-text node t))))
            (if (or (dwim-coder-c-point-around-defun-decl)
                    (memq (char-after (nth 1 (syntax-ppss))) '(nil ?\{)))
                (list (treesit-node-start node) (treesit-node-end node)
                                (treesit-node-text node t))))))))

(defun dwim-coder-c-identifier-at-point (&optional p)
  (let* ((preceding-point p)
         (p (or p (point)))
         (node (treesit-node-at p)))
    (unless preceding-point
      (setq preceding-point (dwim-coder-preceding-point)))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" preceding-point))
      (setq node (treesit-node-at preceding-point)))
    (when (member (treesit-node-type node)
                  '("identifier" "type_identifier" "field_identifier" "true" "false"))
      (when (and (> p (treesit-node-start node))
                 (<= p (treesit-node-end node)))
        (list (treesit-node-start node) (treesit-node-end node)
              (treesit-node-text node t))))))

(defun dwim-coder-c-defun-arg-list ()
  (let ((node (treesit-node-at (point)))
        (value nil))
    (when (and (treesit-node-top-level node "^parameter_list$")
               (treesit-node-top-level node "^function_declarator$"))
      (setq node (treesit-node-top-level node "^parameter_list$"))
      (setq value (list (treesit-node-start node) (treesit-node-end node)))
      (if (and (> (point) (nth 0 value))
               (<= (point) (nth 1 value)))
          value))))

(defun dwim-coder-c-point-around-defun-decl ()
  (let ((node nil)
        (func-start nil)
        (func-name-start nil)
        (func-arg-start nil)
        (func-arg-end nil))
    (setq node (treesit-node-top-level
                (treesit-node-at (point)) "^function_definition$"))
    (if node
        (progn
          (setq func-start (treesit-node-start node))
          (setq node (treesit-node-child-by-field-name node "declarator")))
      (setq node (treesit-node-top-level (treesit-node-at (point)) "^declaration$"))
      (setq func-start (treesit-node-start node)))
    (when node
      (setq node (treesit-search-subtree node "function_declarator")))
    (when node
      (setq func-name-start (treesit-node-start node))
      (setq node (treesit-filter-child
                  node (lambda (n) (equal (treesit-node-type n) "parameter_list"))))
      (when node
        (setq node (car node))
        (setq func-arg-start (treesit-node-start node))
        (setq func-arg-end (treesit-node-end node))))
    (if (and func-start func-arg-start func-arg-end
             (>= (point) func-start)
             (<= (point) func-arg-end))
        (list func-start func-name-start func-arg-start func-arg-end))))

(defun dwim-coder-c-in-include-fname ()
  (let ((node (treesit-node-at (point))))
    (if (or (equal (treesit-node-type node) "\"")
            (equal (treesit-node-type node) "string_content"))
        (setq node (treesit-node-parent (treesit-node-at (point)))))
    (if (or (and (equal (treesit-node-type node) "string_literal")
                 (treesit-node-top-level node "^preproc_include$"))
            (equal (treesit-node-type node) "system_lib_string"))
        (if (or (and (> (point) (treesit-node-start node))
                     (< (point) (treesit-node-end node)))
                (nth 3 (syntax-ppss)))
            (list (treesit-node-start node) (treesit-node-end node))))))

(defun dwim-coder-c-get-char-literal ()
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (when (equal (treesit-node-type node) "char_literal")
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t)))))

(defun dwim-coder-c-dwim-space ()
  (let ((value nil)
        (node nil))
    (cond
     ;; In #include <|> insert '-'
     ((dwim-coder-c-in-include-fname)
      (if (eq (preceding-char) ?-)
          (progn
            ;; if it's already a '-' preceding, replace it with '_'
            (delete-char -1)
            (insert "_"))
        (insert "-"))
      t)
     ((looking-back ") " (line-beginning-position))
      (delete-char -1)
      (insert "->")
      t)
     ;; Let SPC at start of line do '_'
     ((bolp)
      (insert "_")
      (setq dwim-coder-last-space-point (point))
      t)
     ;; Let .SPC give '_'
     ((and (eq (preceding-char) ?\.)
           (save-excursion
             (backward-char)
             (or (bolp)
                 (looking-back "[^a-z0-9]" (line-beginning-position)))))
      (delete-char -1)
      (insert "_")
      t)
     ;; In var->| do var__
     ((and
       (eq (preceding-char) ?>)
       (setq node (treesit-node-at (dwim-coder-preceding-point)))
       (equal (treesit-node-type node) "->"))
      (delete-char -2)
      (insert "__")
      t)
     ;; Insert () if possible
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (setq value (dwim-coder-c-can-paren-at-point))
        ;; If we are after one of "if" "else" "for" "while" "switch"
        (and value (nth 4 value)))
      ;; insert a '()' or skip to it
      (goto-char (nth 1 value))
      (dwim-coder-skip-or-insert ?\( t)
      t)
     ((save-excursion
        (when (eq (preceding-char) ?\()
          (backward-char)
          (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
          (setq value (dwim-coder-c-can-paren-at-point))
          ;; If we are after one of "if" "for" "while" "switch"
          (and value (nth 4 value))))
      ;; insert a '()' or skip to it
      (dwim-coder-skip-or-insert ?! t)
      t)
     ;; Insert real space after type_t
     ((and (setq value (dwim-coder-c-identifier-at-point))
           (string-suffix-p "_t" (nth 2 value)))
      (dwim-coder-skip-or-insert ?\s)
      t)
     ;; change __ to ->
     ((setq value (dwim-coder-c-identifier-at-point))
      (if (and (eq (preceding-char) ?_)
               (not (equal (nth 2 value) "_")))
          (progn
            (delete-char -1)
            (insert "->"))
        (dwim-coder-insert-interactive ?_)
        (setq dwim-coder-last-space-point (point)))
      t)
     ;; If there are more than one space before, insert space again on SPC
     ((looking-back "[^ \t]+[ \t][ \t]+" (line-beginning-position))
      (insert-char ?\s)
      t)
     ((memq (preceding-char) '(?\s ?\( ?\[ ?\!))
      (dwim-coder-insert-interactive ?_)
      (setq dwim-coder-last-space-point (point))
      t)
     ((looking-back "[,([] ?[&*-]" (line-beginning-position))
      (dwim-coder-insert-interactive ?_)
      (setq dwim-coder-last-space-point (point))
      t))))

(defun dwim-coder-c-dwim-quote ()
  (let ((value nil)
        (bounds nil))
    (cond
     ((and (setq value (thing-at-point 'symbol t))
           (setq bounds (bounds-of-thing-at-point 'symbol))
           (string-match-p "^[a-zA-Z_]" value))
      (delete-region (car bounds) (cdr bounds))
      (insert (dwim-coder-s-to-style value "cycle"))
      t))))

(defun dwim-coder-c-dwim-dquote ()
  (let ((value nil)
        (str nil)
        (case-fold-search nil))
    (cond
     ;; insert '*' where it forms a valid pointer type
     ((and (looking-back "[a-z0-9A-Z_]" (line-beginning-position))
           (setq value (or (dwim-coder-c-identifier-at-point (dwim-coder-preceding-point))
                           (dwim-coder-c-type-identifier-at-point (dwim-coder-preceding-point))))
           ;; If the token is in upcase, don't handle (assume it is a macro call)
           (not (string-match-p "^[A-Z0-9_]*$" (caddr value))))
      (setq str (caddr value))
      (if (string-suffix-p "_t" str)
          nil
        (delete-region (car value) (cadr value))
        (if (string-suffix-p "_" str)
            (setq str (substring str 0 -1)))
        (if (string-match-p "_" str)
            (insert (dwim-coder-s-to-style str "upcamel"))
          (insert str)))
      (dwim-coder-skip-or-insert ?\s)
      (dwim-coder-skip-or-insert ?\*)
      t))))

(defun dwim-coder-c-dwim-dot ()
  (cond
   (t (dwim-coder-common-dwim-dot))
   ))

(defun dwim-coder-c-dwim-comma ()
  (let ((value nil)
        (start nil)
        (end nil)
        (p nil)
        (case-fold-search nil))
    (cond
     ;; Replace , with # if in the beginning of line
     ((bolp)
      (dwim-coder-insert-interactive ?#)
      t)
     ;; , after #include inserts <>
     ((save-excursion
        (setq value (treesit-node-at (dwim-coder-preceding-point)))
        (equal (treesit-node-type value) "#include"))
      (goto-char (treesit-node-end value))
      (dwim-coder-skip-or-insert ?\s)
      (save-excursion
        (dwim-coder-skip-or-insert ?<)
        (dwim-coder-skip-or-insert ?>))
      (forward-char)
      t)
     ;; if in #include swap  <|> or "|"
     ((setq value (dwim-coder-c-in-include-fname))
      (setq start (nth 0 value)
            end (nth 1 value))
      (setq value (char-after start))
      (setq p (point))
      (goto-char start)
      (delete-char 1)
      (if (eq value ?<)
          (insert "\"")
        (insert "<"))
      (goto-char end)
      (delete-char -1)
      (if (eq value ?<)
          (insert "\"")
        (insert ">"))
      (goto-char p)
      t)
     ;; ( SPC, (& SPC, and (* SPC
     ((looking-back "([&*]?" (line-beginning-position))
      (setq value (preceding-char))
      (if (memq (following-char) '(?& ?*))
          (forward-char)
        (if (eq value ?\()
            (dwim-coder-skip-or-insert ?& t)
          (delete-char -1)
          (if (eq value ?&)
              (dwim-coder-skip-or-insert ?* t)
            (dwim-coder-skip-or-insert ?& t))))
      t)
     ((and (setq value (treesit-node-at (point)))
           (equal (treesit-node-type value) "'")
           (eq (following-char) ?')
           (eq (preceding-char) ?'))
      (insert-char ?,)
      t)
     ;; if in a char_literal or in array, skip to end and insert , there
     ((and (setq value (treesit-node-parent (treesit-node-at (point))))
           (member (treesit-node-type value) '("subscript_expression" "array_declarator" "char_literal"))
           (> (point) (treesit-node-start value))
           (< (point) (treesit-node-end value)))
      (goto-char (treesit-node-end value))
      (dwim-coder-insert-interactive ?,)
      t)
     ;; Eg: Replace notify->use_underline., to "notify::use_underline",
     ((and (looking-back "[a-zA-Z_][.]" (line-beginning-position))
           (setq value (dwim-coder-c-identifier-at-point (- (point) 2))))
      ;; Delete the . first
      (delete-char -1)
      (setq start (nth 0 value))
      (setq end (nth 1 value))
      (save-excursion
        (goto-char start)
        (backward-char)
        (when (equal (treesit-node-type (treesit-node-at (point))) "->")
          (setq start (- start 2))
          (backward-char)
          (setq value (dwim-coder-c-identifier-at-point (dwim-coder-preceding-point)))
          (if value
              (setq start (nth 0 value)))))
      (replace-regexp-in-region "->" "::" start end)
      (setq value (buffer-substring-no-properties start end))
      (delete-region start end)
      (insert-char ?\")
      (insert (dwim-coder-s-to-style value "lisp"))
      (insert-char ?\")
      (dwim-coder-insert-interactive ?,)
      t)
     ;; Replace ,, with =
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (eq (preceding-char) ?,))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      (dwim-coder-insert-interactive ?=)
      t)
     ;; Don't do anything further if in function declarations
     ((dwim-coder-c-defun-arg-list)
      nil)
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (if (memq (preceding-char) '(?+ ?- ?* ?& ?^ ?\% ?! ?~ ?< ?> ?=))
            (backward-char))
        (setq value (dwim-coder-c-op-at-point))
        ;; Check if '=' is allowed here
        (and value (nth 3 value)))
      (goto-char (nth 1 value))
      (dwim-coder-insert-interactive ?=)
      t))))

(defun dwim-coder-c-dwim-paren ()
  (let ((value nil))
    (cond
     ((save-excursion
        (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
        (setq value (dwim-coder-c-can-paren-at-point))
        ;; If we are after one of "if" "else" "for" "while" "switch"
        (and value (nth 4 value)))
      (goto-char (nth 1 value))
      (dwim-coder-skip-or-insert ?\s t t)
      (dwim-coder-skip-or-insert ?\( t t)
      t)
     ;; don't add a space if no-space is enforced
     ;; don't remove one either as the user may have explicitly added it
     ((setq value (dwim-coder-c-can-paren-at-point))
      (if (nth 3 value)
          nil
        (if (or
             ;; if space is enforced
             (nth 4 value)
             ;; or is GNU style
             (eq c-ts-mode-indent-style 'gnu))
            ;; do nothing if we are already after a space
            (if (eq (preceding-char) ?\s)
                nil
              ;; else skip or insert a space
              (unless (eq (preceding-char) ?\()
                  (dwim-coder-skip-or-insert ?\s)))))
      (dwim-coder-insert-interactive ?\( t)
      t))))

(defun dwim-coder-c-dwim-brace ()
  (let ((value nil))
    (cond
     ;; Handle '{' in function declaration
     ((or (setq value (dwim-coder-c-point-around-defun-decl))
          (save-excursion
            (if (eq (preceding-char) ?\n)
                (backward-char))
            (skip-chars-backward "[ ]" (dwim-coder-preceding-point))
            (skip-chars-backward "[;]" (dwim-coder-preceding-point))
            (skip-chars-backward ")" (dwim-coder-preceding-point))
            (setq value (dwim-coder-c-point-around-defun-decl))))
      ;; Insert 'void' if the parameter list is empty
      (when (eq (- (nth 3 value) (nth 2 value)) 2)
        (goto-char (nth 2 value))
        (forward-char)
        (insert "void")
        ;; update values again since we changed the content
        (setq value (dwim-coder-c-point-around-defun-decl)))
      ;; Skip additional spaces between function return type and arguments
      (replace-regexp-in-region "  +" " " (nth 0 value) (nth 1 value))
      (goto-char (nth 0 value))
      (setq value (dwim-coder-c-point-around-defun-decl))
      (goto-char (nth 2 value))
      ;; Insert a space after function name ...
      (insert " ")
      (goto-char (nth 0 value))
      (setq value (dwim-coder-c-point-around-defun-decl))
      ;; Skip additional spaces between function name and paren start
      (replace-regexp-in-region "  +" " " (nth 1 value) (nth 2 value))
      (goto-char (nth 0 value))
      (setq value (dwim-coder-c-point-around-defun-decl))
      ;; If linux style, delete the space after function name
      (when (eq c-ts-mode-indent-style 'linux)
        (goto-char (nth 2 value))
        (delete-char -1)
        (setq value (dwim-coder-c-point-around-defun-decl)))
      ;; Add a \n before function name in gnu and gnome styles
      (when (or (eq c-ts-mode-indent-style 'gnu)
                (eq dwim-coder-c-sub-style 'gnome))
        (goto-char (nth 1 value))
        (dwim-coder-skip-or-insert ?\n)
        (setq value (dwim-coder-c-point-around-defun-decl)))
      ;; Remove ; (ie, change it to a definition from declaration)
      (goto-char (nth 3 value))
      (if (eq (following-char) ?\;)
          (delete-char 1))
      (indent-region (nth 0 value) (nth 3 value))
      (setq value (dwim-coder-c-point-around-defun-decl))
      ;; Align arguments in gnome style
      (when (eq dwim-coder-c-sub-style 'gnome)
        (goto-char (nth 2 value))
        (replace-regexp-in-region "  +" " " (nth 2 value)
                                  (save-excursion
                                    (re-search-forward "[,)]" (nth 3 value) t) (point)))
        (goto-char (nth 2 value))
        (setq value (dwim-coder-c-point-around-defun-decl))
        (align (nth 2 value) (nth 3 value))
        (setq value (dwim-coder-c-point-around-defun-decl)))
      (delete-trailing-whitespace (nth 0 value) (nth 3 value))
      (setq value (dwim-coder-c-point-around-defun-decl))
      (goto-char (nth 3 value))
      (dwim-coder-skip-or-insert ?\n)
      ;; insert or skip '{'
      (if (eq (following-char) ?{)
          (progn
            (forward-char)
            ;; Skip up to something non-comment
            (forward-comment 42)
            ;; If we went too far, move a bit back
            (if (and (bolp)
                     (eq (following-char) ?}))
                (backward-char)))
        (dwim-coder-insert-interactive ?{ t)
        (dwim-coder-skip-or-insert ?\})
        (if (looking-at-p "\n\n")
            nil
          (insert-char ?\n)
          (unless (eobp)
            (insert-char ?\n)
            (backward-char))
          (backward-char))
        (backward-char)
        (dwim-coder-insert-interactive ?\n))
      t))))

(defun dwim-coder-c-dwim-semi-colon ()
  (let ((value nil))
    (cond
     ;; in incomplete for(), insert ; at point
     ((dwim-coder-c-incomplete-for-at-point)
      ;; If inside an argument list, skip to the end
      (while (and (memq (char-after (nth 1 (syntax-ppss))) '(?\( ?\[))
                  (dwim-coder-c-incomplete-for-at-point (nth 1 (syntax-ppss))))
        (goto-char (nth 1 (syntax-ppss)))
        (setq value t))
      (if value
          (forward-sexp))
      (dwim-coder-insert-interactive ?\; t)
      t)
     ;; Insert ; at point if inside empty string literal
     ((and (eq (preceding-char) ?')
           (eq (following-char) ?')
           (not (dwim-coder-c-get-char-literal)))
      (dwim-coder-insert-interactive ?\; t)
      t)
     ;; Insert newline on ; on preprocessor lines
     ;; fixme: This won't work if it's multiline
     ((and (eolp)
           (looking-back "^ *#.*" (line-beginning-position)))
      (dwim-coder-insert-interactive ?\n)
      t)
     ;; Insert newlinew on ; after #include
     ((save-excursion
        (and (eolp)
             (looking-back "[>\"]" (line-beginning-position))
             (or (backward-char)
                 (dwim-coder-c-in-include-fname))))
      (end-of-line)
      (dwim-coder-insert-interactive ?\n)
      t)
     ((and (looking-at-p " ?[])}]")
           (looking-back "[&|,+-] ?" (line-beginning-position))
           (not (looking-back "\\(++\\|--\\) ?")))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (dwim-coder-insert-interactive ?\n)
      t)
     ((and (eolp)
           (looking-back "\\(++\\|--\\) ?" (line-beginning-position)))
      (dwim-coder-skip-or-insert ?\; t t)
      t)
     ((and (eolp)
           (dwim-coder-c-skip-semi))
      (dwim-coder-insert-interactive ?\n)
      t)
     ((and (eolp)
           (looking-back "[^[ \t;{(|&*^!~<>.,?/:+=-]" (line-beginning-position))
           (not (looking-back "^ *_$" (line-beginning-position))))
      (dwim-coder-skip-or-insert ?\; t t)
      t)
     (t
      (dwim-coder-common-dwim-semi-colon)))))

(defun dwim-coder-c-dwim-square-bracket (char)
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (cond
     ;; Handle '[' ']' in arrays
     ((member (treesit-node-type node) '("subscript_expression" "array_declarator"))
      (goto-char (treesit-node-end node))
      ;; insert closing ']' if missing
      (unless (eq (preceding-char) ?\])
        (insert-char ?\]))
      ;; if '[', insert a new subscript
      (when (eq char ?\[)
        (dwim-coder-insert-interactive ?\[ t))
      t))))

(defun dwim-coder-c-override-self-insert (char)
  (let ((node (treesit-node-at (dwim-coder-preceding-point))))
    (cond
     ;; be sane with comments
     ((nth 4 (syntax-ppss))
      nil)
     ((eq char ?\;)
      (dwim-coder-c-dwim-semi-colon))
     ((and (or (equal (treesit-node-type node) "\"")
               (equal (treesit-node-type node) "string_content"))
           (not (dwim-coder-c-in-include-fname)))
      nil)
     ((eq char ?,)
      (dwim-coder-c-dwim-comma))
     ((eq char ?\s)
      (dwim-coder-c-dwim-space))
     ((dwim-coder-c-in-include-fname)
      nil)
     ((eq char ?.)
      (dwim-coder-c-dwim-dot))
     ((eq char ?\')
      (dwim-coder-c-dwim-quote))
     ((eq char ?\")
      (dwim-coder-c-dwim-dquote))
     ((eq char ?\()
      (dwim-coder-c-dwim-paren))
     ((eq char ?{)
      (dwim-coder-c-dwim-brace))
     ((memq char '(?\[ ?\]))
      (dwim-coder-c-dwim-square-bracket char))
     ((memq char '(?+ ?- ?* ?% ?^ ?& ?| ?< ?> ?=))
      (dwim-coder-common-dwim-op char)))))

(provide 'dwim-coder-c)
;;; dwim-coder-c.el ends here
