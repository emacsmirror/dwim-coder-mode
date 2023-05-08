;;; crazy-c.el --- Crazy ways to code  -*- lexical-binding: t; -*-

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
;; dwim hacks implemeted for `c-ts-mode'

;;; Code:

(require 'crazy-common)
(require 'cl-lib)
(require 'treesit)

(defvar c-ts-mode--operators)
(defvar c-ts-mode-indent-style)

(defcustom crazy-c-sub-style nil
  "Sub style to be used spacing."
  :type '(choice (symbol :tag "None" nil)
                 (symbol :tag "GNOME" 'gnome))
  :group 'crazy)


(defun crazy-c-incomplete-for-at-point (&optional p)
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

(defun crazy-c-can-paren-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p))
         (case-fold-search nil))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (crazy-preceding-point)))
      (setq node (treesit-node-at (crazy-preceding-point))))
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

(defun crazy-c-op-at-point (&optional p)
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
            (looking-back "[<>%^&*/!+-]" (crazy-preceding-point)))
      (setq node (treesit-node-at (crazy-preceding-point))))
    (setq type (treesit-node-type node))
    (when (member type operators)
      (when (or (equal type "=")
                (and (not (string-suffix-p "=" type))
                     (not (member type '("&&" "||" "--" "++")))))
        (setq can-append-equal t))
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t) can-append-equal))))

(defun crazy-c-type-identifier-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p)))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (crazy-preceding-point)))
      (setq node (treesit-node-at (crazy-preceding-point)))
      (if (equal (treesit-node-type node) "primitive_type")
          (list (treesit-node-start node) (treesit-node-end node)
                (treesit-node-text node t))
        (if (and (equal (treesit-node-type node) "type_identifier")
                 (or (string-suffix-p "_" (treesit-node-text node t))
                     (string-suffix-p "_t" (treesit-node-text node t))))
            (if (or (crazy-c-point-around-defun-decl)
                    (memq (char-after (nth 1 (syntax-ppss))) '(nil ?\{)))
                (list (treesit-node-start node) (treesit-node-end node)
                                (treesit-node-text node t))))))))

(defun crazy-c-identifier-at-point (&optional p)
  (let* ((p (or p (point)))
         (node (treesit-node-at p)))
    (when (save-excursion
            (goto-char p)
            (looking-back "[a-zA-Z_0-9]" (crazy-preceding-point)))
      (setq node (treesit-node-at (crazy-preceding-point))))
    (when (member (treesit-node-type node)
                  '("identifier" "type_identifier" "field_identifier" "true" "false"))
      (when (and (> p (treesit-node-start node))
                 (<= p (treesit-node-end node)))
        (list (treesit-node-start node) (treesit-node-end node)
              (treesit-node-text node t))))))

(defun crazy-c-defun-arg-list ()
  (let ((node (treesit-node-at (point)))
        (value nil))
    (when (and (treesit-node-top-level node "^parameter_list$")
               (treesit-node-top-level node "^function_declarator$"))
      (setq node (treesit-node-top-level node "^parameter_list$"))
      (setq value (list (treesit-node-start node) (treesit-node-end node)))
      (if (and (> (point) (nth 0 value))
               (<= (point) (nth 1 value)))
          value))))

(defun crazy-c-point-around-defun-decl ()
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

(defun crazy-c-in-include-fname ()
  (let ((node (treesit-node-at (point))))
    (if (equal (treesit-node-type node) "\"")
        (setq node (treesit-node-parent (treesit-node-at (point)))))
    (if (or (and (equal (treesit-node-type node) "string_literal")
                 (treesit-node-top-level node "^preproc_include$"))
            (equal (treesit-node-type node) "system_lib_string"))
        (if (or (and (> (point) (treesit-node-start node))
                     (< (point) (treesit-node-end node)))
                (nth 3 (syntax-ppss)))
            (list (treesit-node-start node) (treesit-node-end node))))))

(defun crazy-c-in-enum-p ()
  (let ((node (treesit-node-at (point))))
    (and (not (eq (following-char) ?{))
         (treesit-node-top-level node "^enumerator_list$"))))

(defun crazy-c-get-char-literal ()
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (when (equal (treesit-node-type node) "char_literal")
      (list (treesit-node-start node) (treesit-node-end node)
            (treesit-node-text node t)))))

(defun crazy-c-dwim-in-string (char)
  (let ((node (treesit-node-parent (treesit-node-at (point)))))
    (cond
     ;; insert a space after ','
     ((eq char ?\,)
      (crazy-skip-or-insert ?\,)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t)
     ;; replace ", SPC" with "",SPC
     ((and (eq char ?\s)
           (eq (1+ (point)) (treesit-node-end node))
           (looking-back ", " (- (point) 2)))
      (delete-char -2)
      (forward-char)
      (crazy-insert-interactive ?\,)
      t))))

(defun crazy-c-dwim-space ()
  (let ((value nil)
        (node nil))
    (cond
     ;; In #include <|> insert '-'
     ((crazy-c-in-include-fname)
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
       (setq node (treesit-node-at (crazy-preceding-point)))
       (equal (treesit-node-type node) "->"))
      (delete-char -2)
      (insert "__")
      t)
     ;; Insert () if possible
     ((save-excursion
        (skip-chars-backward "[ ]" (crazy-preceding-point))
        (setq value (crazy-c-can-paren-at-point))
        ;; If we are after one of "if" "else" "for" "while" "switch"
        (and value (nth 4 value)))
      ;; insert a '()' or skip to it
      (goto-char (nth 1 value))
      (crazy-skip-or-insert ?\( t)
      t)
     ((save-excursion
        (when (eq (preceding-char) ?\()
          (backward-char)
          (skip-chars-backward "[ ]" (crazy-preceding-point))
          (setq value (crazy-c-can-paren-at-point))
          ;; If we are after one of "if" "for" "while" "switch"
          (and value (nth 4 value))))
      ;; insert a '()' or skip to it
      (crazy-skip-or-insert ?! t)
      t)
     ;; ( SPC, (& SPC, and (* SPC
     ((looking-back "([&*]?" (line-beginning-position))
      (setq value (preceding-char))
      (if (memq (following-char) '(?& ?*))
          (forward-char)
        (if (eq value ?\()
            (crazy-skip-or-insert ?& t)
          (delete-char -1)
          (if (eq value ?&)
              (crazy-skip-or-insert ?* t)
            (crazy-skip-or-insert ?& t))))
      t)
     ;; Insert real space after type_t
     ((and (setq value (crazy-c-identifier-at-point))
           (or (string-suffix-p "_t" (nth 2 value))
               crazy-last-was-camel))
      (crazy-skip-or-insert ?\s)
      (setq crazy-last-was-camel nil)
      t)
     ;; change __ to ->
     ((setq value (crazy-c-identifier-at-point))
      (if (and (eq (preceding-char) ?_)
               (not (equal (nth 2 value) "_")))
          (progn
            (delete-char -1)
            (insert "->"))
        (crazy-insert-interactive ?_))
      t)
     ;; func (test, |) -> func (test), |
     ;; or {test, |} -> {test}, |
     ((save-excursion
        (and (not (eolp))
             (or (forward-char) t)
             (looking-back ", [)}]" (line-beginning-position))))
      (delete-char -2)
      (forward-char)
      (crazy-insert-interactive ?,)
      t)
     ;; If there are more than one space before, insert space again on SPC
     ((looking-back "[^ \t]+[ \t][ \t]+" (line-beginning-position))
      (insert-char ?\s)
      t)
     ((eq (preceding-char) ?\s)
      (crazy-insert-interactive ?_)
      t))))

(defun crazy-c-dwim-quote ()
  (let ((value nil)
        (bounds nil))
    (cond
     ((and (setq value (thing-at-point 'symbol t))
           (setq bounds (bounds-of-thing-at-point 'symbol))
           (string-match-p "^[a-zA-Z_]" value))
      (delete-region (car bounds) (cdr bounds))
      (insert (crazy-s-to-style value "cycle"))
      (if (equal (crazy-s-get-style-case value) "upper-snake")
          (setq crazy-last-was-camel t))
      t))))

(defun crazy-c-dwim-dquote ()
  (let ((value nil)
        (str nil)
        (case-fold-search nil))
    (cond
     ;; insert '*' where it forms a valid pointer type
     ((and (looking-back "[a-z0-9A-Z_]" (line-beginning-position))
           (setq value (or (crazy-c-identifier-at-point (crazy-preceding-point))
                           (crazy-c-type-identifier-at-point (crazy-preceding-point))))
           ;; If the token is in upcase, don't handle (assume it is a macro call)
           (not (string-match-p "^[A-Z0-9_]*$" (caddr value))))
      (setq str (caddr value))
      (if (string-suffix-p "_t" str)
          nil
        (delete-region (car value) (cadr value))
        (if (string-suffix-p "_" str)
            (setq str (substring str 0 -1)))
        (if (string-match-p "_" str)
            (insert (crazy-s-to-style str "upcamel"))
          (insert str)))
      (crazy-skip-or-insert ?\s)
      (crazy-skip-or-insert ?\*)
      t))))

(defun crazy-c-dwim-dot ()
  (cond
   ((and (eq (preceding-char) ?.)
         (save-excursion
           (backward-char)
           (skip-chars-backward "[ ]" (crazy-preceding-point))
           (crazy-c-can-paren-at-point)))
    (delete-char -1)
    (crazy-insert-interactive ?\()
    t)))

(defun crazy-c-dwim-comma ()
  (let ((value nil)
        (start nil)
        (end nil)
        (p nil)
        (case-fold-search nil))
    (cond
     ;; Replace , with # if in the beginning of line
     ((bolp)
      (crazy-insert-interactive ?#)
      t)
     ;; , after #include inserts <>
     ((save-excursion
        (setq value (treesit-node-at (crazy-preceding-point)))
        (equal (treesit-node-type value) "#include"))
      (goto-char (treesit-node-end value))
      (crazy-skip-or-insert ?\s)
      (save-excursion
        (crazy-skip-or-insert ?<)
        (crazy-skip-or-insert ?>))
      (forward-char)
      t)
     ;; if in #include swap  <|> or "|"
     ((setq value (crazy-c-in-include-fname))
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
     ((save-excursion
        (and (not (eolp))
             (or (forward-char) t)
             (looking-back "[({[][])}]" (line-beginning-position))))
      (forward-char)
      (crazy-insert-interactive ?,)
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
      (crazy-insert-interactive ?,)
      t)
     ;; Eg: Replace notify->use_underline., to "notify::use_underline",
     ((and (eq (preceding-char) ?.)
           (setq value (crazy-c-identifier-at-point (crazy-preceding-point))))
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
          (setq value (crazy-c-identifier-at-point (crazy-preceding-point)))
          (if value
              (setq start (nth 0 value)))))
      (replace-regexp-in-region "->" "::" start end)
      (setq value (buffer-substring-no-properties start end))
      (delete-region start end)
      (insert-char ?\")
      (insert (crazy-s-to-style value "lisp"))
      (insert-char ?\")
      (crazy-insert-interactive ?,)
      t)
     ;; Replace ,, with =
     ((save-excursion
        (skip-chars-backward "[ ]" (crazy-preceding-point))
        (eq (preceding-char) ?,))
      (if (eq (preceding-char) ?\s)
          (delete-char -1))
      (delete-char -1)
      (crazy-insert-interactive ?=)
      t)
     ((and (eq crazy-c-sub-style 'gnome)
           (crazy-c-defun-arg-list))
      (crazy-skip-or-insert ?,)
      (insert "\n")
      (indent-according-to-mode)
      t)
     ;; Don't do anything further if in enum and function declarations
     ((or (crazy-c-in-enum-p)
          (crazy-c-defun-arg-list))
      nil)
     ((save-excursion
        (skip-chars-backward "[ ]" (crazy-preceding-point))
        (if (memq (preceding-char) '(?+ ?- ?* ?& ?^ ?\% ?! ?~ ?< ?> ?=))
            (backward-char))
        (setq value (crazy-c-op-at-point))
        ;; Check if '=' is allowed here
        (and value (nth 3 value)))
      (goto-char (nth 1 value))
      (crazy-insert-interactive ?=)
      t)
     (t
      (if crazy-auto-space
          (if (eq (preceding-char) ?\s)
              (delete-char -1))
        (if (eq (following-char) ?\s)
            (delete-char 1)))

      (crazy-skip-or-insert ?,)
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      t))))

(defun crazy-c-dwim-paren ()
  (let ((value nil))
    (cond
     ((save-excursion
        (skip-chars-backward "[ ]" (crazy-preceding-point))
        (setq value (crazy-c-can-paren-at-point))
        ;; If we are after one of "if" "else" "for" "while" "switch"
        (and value (nth 4 value)))
      (goto-char (nth 1 value))
      (crazy-skip-or-insert ?\s t t)
      (crazy-skip-or-insert ?\( t t)
      t)
     ;; don't add a space if no-space is enforced
     ;; don't remove one either as the user may have explicitly added it
     ((setq value (crazy-c-can-paren-at-point))
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
                  (crazy-skip-or-insert ?\s)))))
      (crazy-insert-interactive ?\( t)
      t))))

(defun crazy-c-dwim-brace ()
  (let ((value nil))
    (cond
     ;; Handle '{' in function declaration
     ((or (setq value (crazy-c-point-around-defun-decl))
          (save-excursion
            (if (eq (preceding-char) ?\n)
                (backward-char))
            (skip-chars-backward "[ ]" (crazy-preceding-point))
            (skip-chars-backward "[;]" (crazy-preceding-point))
            (skip-chars-backward ")" (crazy-preceding-point))
            (setq value (crazy-c-point-around-defun-decl))))
      ;; Insert 'void' if the parameter list is empty
      (when (eq (- (nth 3 value) (nth 2 value)) 2)
        (goto-char (nth 2 value))
        (forward-char)
        (insert "void")
        ;; update values again since we changed the content
        (setq value (crazy-c-point-around-defun-decl)))
      ;; Skip additional spaces between function return type and arguments
      (replace-regexp-in-region "  +" " " (nth 0 value) (nth 1 value))
      (goto-char (nth 0 value))
      (setq value (crazy-c-point-around-defun-decl))
      (goto-char (nth 2 value))
      ;; Insert a space after function name ...
      (insert " ")
      (goto-char (nth 0 value))
      (setq value (crazy-c-point-around-defun-decl))
      ;; Skip additional spaces between function name and paren start
      (replace-regexp-in-region "  +" " " (nth 1 value) (nth 2 value))
      (goto-char (nth 0 value))
      (setq value (crazy-c-point-around-defun-decl))
      ;; If linux style, delete the space after function name
      (when (eq c-ts-mode-indent-style 'linux)
        (goto-char (nth 2 value))
        (delete-char -1)
        (setq value (crazy-c-point-around-defun-decl)))
      ;; Add a \n before function name in gnu and gnome styles
      (when (or (eq c-ts-mode-indent-style 'gnu)
                (eq crazy-c-sub-style 'gnome))
        (goto-char (nth 1 value))
        (crazy-skip-or-insert ?\n)
        (setq value (crazy-c-point-around-defun-decl)))
      ;; Remove ; (ie, change it to a definition from declaration)
      (goto-char (nth 3 value))
      (if (eq (following-char) ?\;)
          (delete-char 1))
      (indent-region (nth 0 value) (nth 3 value))
      (setq value (crazy-c-point-around-defun-decl))
      ;; Align arguments in gnome style
      (when (eq crazy-c-sub-style 'gnome)
        (goto-char (nth 2 value))
        (replace-regexp-in-region "  +" " " (nth 2 value)
                                  (save-excursion
                                    (re-search-forward "[,)]" (nth 3 value) t) (point)))
        (goto-char (nth 2 value))
        (setq value (crazy-c-point-around-defun-decl))
        (align (nth 2 value) (nth 3 value))
        (setq value (crazy-c-point-around-defun-decl)))
      (delete-trailing-whitespace (nth 0 value) (nth 3 value))
      (setq value (crazy-c-point-around-defun-decl))
      (goto-char (nth 3 value))
      (crazy-skip-or-insert ?\n)
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
        (crazy-insert-interactive ?{ t)
        (crazy-skip-or-insert ?\})
        (if (looking-at-p "\n\n")
            nil
          (insert-char ?\n)
          (unless (eobp)
            (insert-char ?\n)
            (backward-char))
          (backward-char))
        (backward-char)
        (crazy-insert-interactive ?\n))
      t))))

(defun crazy-c-dwim-semi-colon ()
  (let ((value nil))
    (cond
     ;; in incomplete for(), insert ; at point
     ((crazy-c-incomplete-for-at-point)
      ;; If inside an argument list, skip to the end
      (while (and (memq (char-after (nth 1 (syntax-ppss))) '(?\( ?\[))
                  (crazy-c-incomplete-for-at-point (nth 1 (syntax-ppss))))
        (goto-char (nth 1 (syntax-ppss)))
        (setq value t))
      (if value
          (forward-sexp))
      (insert ";")
      t)
     ((eq (following-char) ?\;)
      (forward-char)
      t)
     ;; Insert ; at point if inside empty string literal
      ((and (eq (preceding-char) ?')
            (eq (following-char) ?')
            (not (crazy-c-get-char-literal)))
       (crazy-insert-interactive ?\; t)
       t)
      ;; goto end of string if inside one
      ((nth 3 (syntax-ppss))
       (skip-syntax-forward "^\"")
       (forward-char)
       t)
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
       (if (eq (preceding-char) ?\;)
           (backward-char))
       t)
      ;; Skip to the end of the current statement as possible
      ((and (memq (preceding-char) '(?, ?\{ ?\[ ?\())
            (memq (char-after (nth 1 (syntax-ppss))) '(?\( ?\[ ?\{)))
       (goto-char (nth 1 (syntax-ppss)))
       (forward-sexp)
       t))))

(defun crazy-c-dwim-square-bracket (char)
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
        (crazy-insert-interactive ?\[ t))
      t))))

(defun crazy-c-dwim-operator (char)
  (let ((value nil))
    (save-excursion
      (skip-chars-backward "[ ]" (crazy-preceding-point))
      (setq value (crazy-c-op-at-point (crazy-preceding-point))))

    (if (and value
             (nth 3 value)
             (or (not (looking-back "= ?[-+!]*" (line-beginning-position)))
                 (memq char '(?, ?=))))
        (skip-chars-backward "[ ]" (crazy-preceding-point)))
    (crazy-insert-interactive char t)
    (setq value (crazy-c-op-at-point (crazy-preceding-point)))
    (cond
     ((member (nth 2 value) '("--" "++"))
      (goto-char (nth 0 value))
      (if (eq (preceding-char) ?\s)
          (unless (or (looking-back "^[ \t]*" (line-beginning-position))
                      (looking-back "= *" (line-beginning-position)))
            (delete-char -1)))
      (indent-according-to-mode)
      (forward-char 2)
      (when (eq (following-char) ?\s)
        (delete-char 1)))
     ;; do nothing if preceding content is empty
     ((looking-back "^[ \t]*" (line-beginning-position))
      t)
     ;; Insert space before operator
     ((save-excursion
        (backward-char)
        (looking-back "[]a-zA-Z0-9_)]" (crazy-preceding-point)))
      (goto-char (nth 0 value))
      (if crazy-auto-space
          (crazy-skip-or-insert ?\s))
      ;; update start and end point
      (setq value (crazy-c-op-at-point))
      (goto-char (nth 1 value))))
    (unless (or (member (nth 2 value) '("--" "++" "!"))
                (memq (char-before (nth 0 value)) '(?\( ?\[ ?{ ?<)))
      (if (and crazy-auto-space
               (not (looking-back "= ?[-+!]" (line-beginning-position)))
               (not (looking-back ", ?[*&!]" (line-beginning-position))))
          (crazy-skip-or-insert ?\s)))
    t))

(defun crazy-c-override-self-insert (char)
  (let ((node (treesit-node-at (point))))
    (cond
     ;; be sane with comments
     ((nth 4 (syntax-ppss))
      nil)
     ((eq char ?\;)
      (crazy-c-dwim-semi-colon))
     ((and (equal (treesit-node-type node) "\"")
           (not (crazy-c-in-include-fname)))
      (crazy-c-dwim-in-string char))
     ;; upcase letters inside enum
     ((and (>= char ?a)
           (<= char ?z)
           (crazy-c-in-enum-p))
      (crazy-insert-interactive (upcase char))
      t)
     ((eq char ?,)
      (crazy-c-dwim-comma))
     ((eq char ?\s)
      (crazy-c-dwim-space))
     ((crazy-c-in-include-fname)
      nil)
     ((eq char ?.)
      (crazy-c-dwim-dot))
     ((eq char ?\')
      (crazy-c-dwim-quote))
     ((eq char ?\")
      (crazy-c-dwim-dquote))
     ((eq char ?\()
      (crazy-c-dwim-paren))
     ((eq char ?{)
      (crazy-c-dwim-brace))
     ((memq char '(?\[ ?\]))
      (crazy-c-dwim-square-bracket char))
     ((memq char '(?< ?> ?/ ?% ?* ?- ?+ ?= ?& ?| ?!))
      (crazy-c-dwim-operator char)))))

(provide 'crazy-c)
;;; crazy-c.el ends here
