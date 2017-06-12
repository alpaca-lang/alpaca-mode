;;; alpaca-font-lock.el --- Font locking module for Alpaca mode.

;; Copyright (C) 2013, 2014 Joseph Collard
;; Copyright (C) 2015 Bogdan Popa
;; Copyright (C) 2017 The Alpaca Community

;; Authors: Joseph Collard
;; URL: https://github.com/jcollard/elm-mode

;; Authors: Alpaca Community
;; URL: https://github.com/alpaca-lang/alpaca-mode

;; This file was adapted from `elm-indent.el'

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'font-lock)
(require 'rx)

(defgroup alpaca-font-lock nil
  "Font locking for Alpaca code."
  :group 'faces)

(defface alpaca-font-lock-operators
  '((t :inherit font-lock-builtin-face))
  "The default face used to highlight operators inside expressions."
  :group 'alpaca-font-lock)

(defcustom alpaca-font-lock-operators-face 'alpaca-font-lock-operators
  "The face used to highlight operators inside expressions.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'alpaca-font-lock)

(defface alpaca-font-lock-multiline-list-delimiters
  '((t :inherit font-lock-keyword-face))
  "The default face used to highlight brackets and commas in multiline lists."
  :group 'alpaca-font-lock)

(defcustom alpaca-font-lock-multiline-list-delimiters-face 'alpaca-font-lock-multiline-list-delimiters
  "The face used to highlight brackets and commas in multilist lists.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'alpaca-font-lock)

(defconst alpaca--keywords
  '("module" "export" "export_type" "let" "in"
    "type" "match" "with"
    "beam"
    "spawn" "send" "receive" "after"
    "test"
    "error" "exit" "throw"
    "true" "false"
    "unit" "size" "end" "sign"
    "big" "little" "native" "utf8")
  "Reserved keywords.")

(defconst alpaca--regexp-keywords
  (concat (regexp-opt alpaca--keywords 'words) "[^']")
  "A regular expression representing the reserved keywords.")

(defconst alpaca--font-lock-keywords
  (cons alpaca--regexp-keywords font-lock-keyword-face)
  "Highlighting for keywords.")

(defun alpaca--syntax-stringify ()
  "Syntax propertize triple quoted strings."
  (let* ((ppss (save-excursion
                 (backward-char 3)
                 (syntax-ppss)))
         (string-started (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (if (not string-started)
        (put-text-property quote-starting-pos (1+ quote-starting-pos)
                           'syntax-table (string-to-syntax "|"))
      (put-text-property (1- quote-ending-pos) quote-ending-pos
                           'syntax-table (string-to-syntax "|")))))

(defconst alpaca--syntax-propertize
  (syntax-propertize-rules
   ;;; Syntax rule for -- comments
   ((rx (and (0+ " ") (group "--")
             (0+ any) (group "\n")))
    (1 "< b")
    (2 "> b"))

   ;;; Syntax rule for char literals
   ((rx (and (1+ " ")
             (group "'")
             (optional "\\") any
             (group "'")))
    (1 "\"")
    (2 "\""))

   ((rx (and (or point
                 (not (any ?\\ ?\"))
                 (and (or (not (any ?\\)) point) ?\\ (* ?\\ ?\\) (any ?\")))
             (* ?\\ ?\\)
             "\"\"\""))
    (0 (ignore (alpaca--syntax-stringify))))))

(defun alpaca--syntax-propertize-function (begin end)
  "Mark special lexemes between BEGIN and END."
  (funcall alpaca--syntax-propertize begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "\\\\[({]" end t)
      (let ((open (match-beginning 0)))
        (add-text-properties open (1+ open) '(syntax-table (1 . nil)))))))

(defvar alpaca--syntax-table
  (let ((st (make-syntax-table)))
    ;;; Syntax entry for {- -} type comments.
    (modify-syntax-entry ?{ "(} 1n" st)
    (modify-syntax-entry ?- ". 23n" st)
    (modify-syntax-entry ?} "){ 4n" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    st))

;;; Name regexp is according to https://github.com/alpaca-lang/alpaca-compiler/blob/353930a474fee4d833f962100edde70417691bca/src/Parse/Helpers.hs#L65
(defconst alpaca--regexp-function
  "^\\([a-z_][0-9A-Za-z_']*\\|([^)]+)\\)"
  "A regular expression representing function names.")

(defconst alpaca--font-lock-functions
  (cons alpaca--regexp-function font-lock-function-name-face)
  "Highlighting for function names.")

(defconst alpaca--font-lock-data
    `(,(rx symbol-start
          (group (char upper) (1+ (or word ?_)))
          symbol-end)
     (1 font-lock-type-face))
    "Highlighting data constructors.")

(defconst alpaca--font-lock-number
    `(,(rx (or digit symbol-start)
          (group (or ?- ?+ ?%))
          (or digit symbol-end))
     (1 font-lock-variable-name-face))
    "Highlighting numbers.")

(defconst alpaca--font-lock-operator
    `(,(rx (not word) (group (or "->" "==" ?= "!=" ">=" "=<" ?> ?< ?=)))
     (1 font-lock-variable-name-face))
    "Highlighting operators.")

(defconst alpaca--font-lock-module
  `(,(rx symbol-start
            (group "module") (1+ space)
            (group (1+ (or word ?_))))
        (1 font-lock-keyword-face) (2 font-lock-type-face))
  "Highlighting for module names.")

(defconst alpaca--font-lock-export
  `(,(rx line-start
          (group "export") (1+ space)
          (group (: (1+ (or word ?_)) ?/ (1+ digit))
                 (* ?, (opt ?\n) (* space)
                    (: (1+ (or word ?_)) ?/ (1+ digit))))
          line-end)
    (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
  "Highlighting for export.")

(defconst alpaca--font-lock-export-type
  `(,(rx line-start
         (group "export_type") (1+ space)
         (group (1+ (or word ?_))
                (* ?, (opt ?\n) (* space)
                   (1+ (or word ?_))))
         line-end)
    (1 font-lock-keyword-face)
    (2 font-lock-variable-name-face))
  "Highlighting for export_type.")

(defconst alpaca--font-lock-type
  `(,(rx line-start
         (group "type") (1+ space) (group (1+ (or word ?_))) (1+ space)
         (optional (group (1+ ?' (1+ (or word ?_)) (1+ space)))) ?=)
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face))
  "Highlighting for types.")

(defconst alpaca--font-lock-builtin
  `(,(rx symbol-start
         (group "is_" (or "integer" "float" "atom" "bool"
                          "list" "string" "chars" "pid" "binary"))
         symbol-end)
    (1 font-lock-builtin-face))
  "Highlihgint for buitin functions.")

(defconst alpaca--regexp-operators
  (concat "\\(" "`[^`]+`"
          "\\|" "\\B\\\\"
          "\\|" "[-+*/\\\\|<>=:!@#$%^&,.]+"
          "\\)")
  "A regular expression representing operators inside expressions.")

(defconst alpaca--font-lock-operators
  (cons alpaca--regexp-operators '(1 alpaca-font-lock-operators-face))
  "Highlighting for operators inside expressions.")

(defconst alpaca--regexp-multiline-list-comma-closing-brackets
  (concat "^[[:space:]]*" (regexp-opt '("," "]" "}") t))
  "A regular expression representing commas and closing brackets in multiline lists and records.")

(defconst alpaca--font-lock-multiline-list-comma-closing-brackets
  (cons alpaca--regexp-multiline-list-comma-closing-brackets
        '(1 alpaca-font-lock-multiline-list-delimiters-face))
  "Highlighting for commas and closing brackets in multiline lists and records.")

(defun alpaca--match-multiline-list-opening-bracket (limit)
  "Highlighting search function for opening brackets in multiline lists and records.
Also highlights opening brackets without a matching bracket."
  (when (alpaca--search-forward-opening-bracket limit)
    (let ((opening (point))
          (eol (line-end-position))
          (closing (alpaca--search-forward-closing-bracket)))
      (if (or (= closing opening) (> closing eol))
          (progn
            (set-match-data (match-data))
            (goto-char (+ 1 opening))
            t)
        (alpaca--match-multiline-list-opening-bracket limit)))))

(defun alpaca--search-forward-opening-bracket (limit)
  "Go to the next opening bracket up to LIMIT."
  (if (search-forward-regexp (regexp-opt '("[" "{")) limit t)
      (progn
        (backward-char)
        t)))

(defun alpaca--search-forward-closing-bracket ()
  "Go to the next matching bracket, assuming that the cursor is on an opening bracket."
  (ignore-errors
    (save-match-data
      (forward-sexp)))
  (point))

(defconst alpaca--font-lock-multiline-list-opening-brackets
  '(alpaca--match-multiline-list-opening-bracket (0 alpaca-font-lock-multiline-list-delimiters-face))
  "Highlighting for opening brackets in multiline lists and records.")

(defconst alpaca--font-lock-highlighting
  (list (list alpaca--font-lock-keywords
              alpaca--font-lock-functions
              alpaca--font-lock-module
              alpaca--font-lock-data
              alpaca--font-lock-number
              alpaca--font-lock-operator
              alpaca--font-lock-type
              alpaca--font-lock-export
              alpaca--font-lock-export-type
              alpaca--font-lock-builtin
              alpaca--font-lock-multiline-list-comma-closing-brackets
              alpaca--font-lock-multiline-list-opening-brackets
              alpaca--font-lock-operators)
        nil nil))

(defun turn-on-alpaca-font-lock ()
  "Turn on Alpaca font lock."
  (setq font-lock-multiline t)
  (set-syntax-table alpaca--syntax-table)
  (set (make-local-variable 'syntax-propertize-function) #'alpaca--syntax-propertize-function)
  (set (make-local-variable 'font-lock-defaults) alpaca--font-lock-highlighting))

(provide 'alpaca-font-lock)
;;; alpaca-font-lock.el ends here
