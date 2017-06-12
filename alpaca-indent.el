;;; alpaca-indent.el --- "semi-intelligent" indentation module for Elm Mode

;; Copyright 2004, 2005, 2007, 2008, 2009  Free Software Foundation, Inc.
;; Copyright 1997-1998  Guy Lapalme
;; Copyright 2015  Bogdan Popa
;; Copyright (C) 2017 The Alpaca Community

;; Author: 1997-1998 Guy Lapalme <lapalme@iro.umontreal.ca>

;; Authors: Alpaca Community
;; URL: https://github.com/alpaca-lang/alpaca-mode

;; Keywords: indentation Alpaca layout-rule
;; Version: 1.2
;; URL: http://www.iro.umontreal.ca/~lapalme/layout/index.html

;; This file is not part of GNU Emacs.

;; This file was adapted from `elm-indent.el' which was adepted from `haskell-indent.el'.

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
(require 's)

(with-no-warnings
  (require 'cl))

;;; Customizations
(defgroup alpaca-indent nil
  "Alpaca indentation."
  :group 'alpaca
  :link '(custom-manual "(alpaca-mode)Indentation")
  :prefix "alpaca-indent-")

(defcustom alpaca-indent-offset 4
  "Indentation of Alpaca statements with respect to containing block."
  :type 'integer
  :group 'alpaca-indent)

(defcustom alpaca-indent-rhs-align-column 0
  "Column on which to align right-hand sides (use 0 for ad-hoc alignment)."
  :type 'integer
  :group 'alpaca-indent)

(defcustom alpaca-indent-look-past-empty-line t
  "If nil, indentation engine will not look past an empty line for layout points."
  :group 'alpaca-indent
  :type 'boolean)

(defcustom alpaca-indent-thenelse 0
  "If non-zero, \"then\" and \"else\" are indented by that amount."
  :group 'alpaca-indent
  :type 'integer)

(defcustom alpaca-indent-after-keywords
  `(("of" ,alpaca-indent-offset)
    ("in" ,alpaca-indent-offset 0)
    ("{" ,alpaca-indent-offset)
    "if"
    "then"
    "else"
    "let")
  "Keywords after which indentation should be indented by some offset.
Each keyword info can have the following forms:

   KEYWORD | (KEYWORD OFFSET [OFFSET-HANGING])

If absent OFFSET-HANGING defaults to OFFSET.
If absent OFFSET defaults to `alpaca-indent-offset'.

OFFSET-HANGING is the offset to use in the case where the keyword
is at the end of an otherwise-non-empty line."
  :set-after '(alpaca-indent-offset)
  :group 'alpaca-indent
  :type '(repeat (choice string
                         (cons :tag "" (string :tag "keyword:")
                               (cons :tag "" (integer :tag "offset")
                                     (choice (const nil)
                                             (list :tag ""
                                                   (integer :tag "offset-pending"))))))))

(defcustom alpaca-indent-dont-hang '("(")
  "Lexemes that should never be considered as hanging."
  :group 'alpaca-indent
  :type '(repeat string))


;;; Internals
(defconst alpaca-indent-start-keywords-re
  (concat "\\<"
          (regexp-opt '("module" "import" "type") t)
          "\\>")
  "Regexp for keywords to complete when standing at the first word of a line.")

(defvar alpaca-indent-off-side-keywords-re
  (concat "\\<"
          (regexp-opt '("let"))
          "\\>[ \t]*"))

(defvar alpaca-indent-last-info nil)
(defvar alpaca-indent-info)

(defvar alpaca-indent-current-line-first-ident ""
  "Global variable that keeps track of the first ident of the line to indent.")

(defvar alpaca-indent-inhibit-after-offset nil)

(defun alpaca-indent-point-to-col (apoint)
  "Return the column number of APOINT."
  (save-excursion
    (goto-char apoint)
    (current-column)))

(defun alpaca-indent-push-col (col &optional name)
  "Push indentation information for the column COL.
The info is followed by NAME (if present).
Makes sure that the same indentation info is not pushed twice.
Uses free var `alpaca-indent-info'."
  (let ((tmp (cons col name)))
    (if (member tmp alpaca-indent-info)
        alpaca-indent-info
      (push tmp alpaca-indent-info))))

(defun alpaca-indent-push-pos (pos &optional name)
  "Push indentation information for POS followed by NAME (if present)."
  (alpaca-indent-push-col (alpaca-indent-point-to-col pos) name))

(defun alpaca-indent-column+offset (column offset)
  (unless offset (setq offset alpaca-indent-offset))
  (setq column (+ column offset)))

(defun alpaca-indent-push-pos-offset (pos &optional offset)
  "Pushes indentation information for the column corresponding to POS
followed by an OFFSET (if present use its value otherwise use
`alpaca-indent-offset')."
  (alpaca-indent-push-col (alpaca-indent-column+offset
                        (alpaca-indent-point-to-col pos)
                        offset)))

(defun alpaca-indent-empty-line-p ()
  "Checks if the current line is empty; deals with Bird style scripts."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*$")))

(defun alpaca-indent-skip-blanks-and-newlines-forward (end)
  "Skip forward blanks, tabs and newlines until END."
  (skip-chars-forward " \t\n" end))

(defun alpaca-indent-skip-blanks-and-newlines-backward (start)
  "Skip backward blanks, tabs and newlines up to START."
  (skip-chars-backward " \t\n" start))

(defun alpaca-indent-start-of-def ()
  "Return the position of the start of a definition.
The start of a def is expected to be recognizable by starting in column 0,
unless `alpaca-indent-look-past-empty-line' is nil, in which case we
take a coarser approximation and stop at the first empty line."
  (save-excursion
    (let ((start-code nil)
          (top-col 0)
          (save-point (point)))
      ;; determine the starting point of the current piece of code
      (setq start-code (if start-code (1+ start-code) (point-min)))
      ;; go backward until the first preceding empty line
      (forward-line -1)
      (while (and (if alpaca-indent-look-past-empty-line
                      (or (> (current-indentation) top-col)
                          (alpaca-indent-empty-line-p))
                    (and (> (current-indentation) top-col)
                         (not (alpaca-indent-empty-line-p))))
                  (> (point) start-code)
                  (= 0 (forward-line -1))))
      ;; go forward after the empty line
      (if (alpaca-indent-empty-line-p)
          (forward-line 1))
      (setq start-code (point))
      ;; find the first line of code which is not a comment
      (forward-comment (point-max))
      (if (> (point) save-point)
          start-code
        (point)))))

(defun alpaca-indent-open-structure (start end)
  "If any structure (list or tuple) is not closed, between START and END,
returns the location of the opening symbol, nil otherwise."
  (save-excursion
    (nth 1 (parse-partial-sexp start end))))

(defun alpaca-indent-in-string (start end)
  "If a string is not closed , between START and END, returns the
location of the opening symbol, nil otherwise."
  (save-excursion
    (let ((pps (parse-partial-sexp start end)))
      (if (nth 3 pps) (nth 8 pps)))))

(defun alpaca-indent-in-comment (start end)
  "Check, starting from START, if END is at or within a comment.
Returns the location of the start of the comment, nil otherwise."
  (when (<= start end)
    (let (pps)
      (cond ((= start end) nil)
            ((nth 4 (save-excursion (setq pps (parse-partial-sexp start end))))
             (nth 8 pps))
            ;; We also want to say that we are *at* the beginning of a comment.
            ((and (not (nth 8 pps))
                  (>= (point-max) (+ end 2))
                  (nth 4 (save-excursion
                           (setq pps (parse-partial-sexp end (+ end 2))))))
             (nth 8 pps))))))

(defun alpaca-indent-type-at-point ()
  "Return the type of the line (also puts information in `match-data')."
  (cond
   ((alpaca-indent-empty-line-p) 'empty)
   ((alpaca-indent-in-comment (point-min) (point)) 'comment)
   ((looking-at "\\(\\([[:alpha:]]\\(\\sw\\|'\\)*\\)\\|_\\)[ \t\n]*") 'ident)
   ((looking-at "\\(|[^|]\\)[ \t\n]*") 'guard)
   ((looking-at "\\(=[^>=]\\|:[^:]\\|->\\|<-\\)[ \t\n]*") 'rhs)
   (t 'other)))

(defun alpaca-indent-contour-line (start end)
  "Generate contour information between START and END points."
  (if (< start end)
      (save-excursion
        (goto-char end)
        (alpaca-indent-skip-blanks-and-newlines-backward start)
        (let ((cur-col (current-column)) ; maximum column number
              (fl 0) ; number of lines that forward-line could not advance
              contour)
          (while (and (> cur-col 0) (= fl 0) (>= (point) start))
            (back-to-indentation)
            (if (< (point) start) (goto-char start))
            (and (not (member (alpaca-indent-type-at-point)
                              '(empty comment))) ; skip empty and comment lines
                 (< (current-column) cur-col) ; less indented column found
                 (push (point) contour) ; new contour point found
                 (setq cur-col (current-column)))
            (setq fl (forward-line -1)))
          contour))))

(defun alpaca-indent-next-symbol (end)
  "Move point to the next symbol."
  (skip-syntax-forward ")" end)
  (if (< (point) end)
      (progn
        (forward-sexp 1)
        (alpaca-indent-skip-blanks-and-newlines-forward end))))

(defun alpaca-indent-next-symbol-safe (end)
  "Puts point to the next following symbol, or to end if there are no more symbols in the sexp."
  (condition-case errlist (alpaca-indent-next-symbol end)
    (error (goto-char end))))

(defun alpaca-indent-separate-valdef (start end)
  "Return a list of positions for important parts of a valdef."
  (save-excursion
    (let (valname valname-string aft-valname
                  guard aft-guard
                  rhs-sign aft-rhs-sign
                  type)
      ;; "parse" a valdef separating important parts
      (goto-char start)
      (setq type (alpaca-indent-type-at-point))
      (if (or (memq type '(ident other))) ; possible start of a value def
          (progn
            (if (eq type 'ident)
                (progn
                  (setq valname (match-beginning 0))
                  (setq valname-string (match-string 0))
                  (goto-char (match-end 0)))
              (skip-chars-forward " \t" end)
              (setq valname (point))    ; type = other
              (alpaca-indent-next-symbol-safe end))
            (while (and (< (point) end)
                        (setq type (alpaca-indent-type-at-point))
                        (or (memq type '(ident other))))
              (if (null aft-valname)
                  (setq aft-valname (point)))
              (alpaca-indent-next-symbol-safe end))))
      (if (and (< (point) end) (eq type 'guard)) ; start of a guard
          (progn
            (setq guard (match-beginning 0))
            (goto-char (match-end 0))
            (while (and (< (point) end)
                        (setq type (alpaca-indent-type-at-point))
                        (not (eq type 'rhs)))
              (if (null aft-guard)
                  (setq aft-guard (point)))
              (alpaca-indent-next-symbol-safe end))))
      (if (and (< (point) end) (eq type 'rhs)) ; start of a rhs
          (progn
            (setq rhs-sign (match-beginning 0))
            (goto-char (match-end 0))
            (if (< (point) end)
                (setq aft-rhs-sign (point)))))
      (list valname valname-string aft-valname
            guard aft-guard rhs-sign aft-rhs-sign))))

(defsubst alpaca-indent-no-otherwise (guard)
  "Check if there is no otherwise at GUARD."
  (save-excursion
    (goto-char guard)
    (not (looking-at "|[ \t]*otherwise\\>"))))


(defun alpaca-indent-guard (start end end-visible indent-info)
  "Find indentation information for a line starting with a guard."
  (save-excursion
    (let* ((alpaca-indent-info indent-info)
           (sep (alpaca-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and guard (< guard end-visible) (alpaca-indent-no-otherwise guard))
          (alpaca-indent-push-pos guard)
        (if rhs-sign
            (alpaca-indent-push-pos rhs-sign) ; probably within a data definition...
          (if valname
              (alpaca-indent-push-pos-offset valname))))
      alpaca-indent-info)))

(defun alpaca-indent-rhs (start end end-visible indent-info)
  "Find indentation information for a line starting with a rhs."
  (save-excursion
    (let* ((alpaca-indent-info indent-info)
           (sep (alpaca-indent-separate-valdef start end))
           (valname (nth 0 sep))
           (guard (nth 3 sep))
           (rhs-sign (nth 5 sep)))
      ;; push information indentation for the visible part
      (if (and rhs-sign (< rhs-sign end-visible))
          (alpaca-indent-push-pos rhs-sign)
        (if (and guard (< guard end-visible))
            (alpaca-indent-push-pos-offset guard)
          (if valname                   ; always visible !!
              (alpaca-indent-push-pos-offset valname))))
      alpaca-indent-info)))

(defconst alpaca-indent-decision-table
  (let ((or "\\)\\|\\("))
    (concat "\\("
            "1.1.11" or                 ; 1= vn gd rh arh
            "1.1.10" or                 ; 2= vn gd rh
            "1.1100" or                 ; 3= vn gd agd
            "1.1000" or                 ; 4= vn gd
            "1.0011" or                 ; 5= vn rh arh
            "1.0010" or                 ; 6= vn rh
            "110000" or                 ; 7= vn avn
            "100000" or                 ; 8= vn
            "001.11" or                 ; 9= gd rh arh
            "001.10" or                 ;10= gd rh
            "001100" or                 ;11= gd agd
            "001000" or                 ;12= gd
            "000011" or                 ;13= rh arh
            "000010" or                 ;14= rh
            "000000"                    ;15=
            "\\)")))

(defun alpaca-indent-find-case (test)
  "Find the index that matches TEST in the decision table."
  (if (string-match alpaca-indent-decision-table test)
      ;; use the fact that the resulting match-data is a list of the form
      ;; (0 6 [2*(n-1) nil] 0 6) where n is the number of the matching regexp
      ;; so n= ((length match-data)/2)-1
      (- (/ (length (match-data 'integers)) 2) 1)
    (error "alpaca-indent-find-case: impossible case: %s" test)))

(defun alpaca-indent-after-list-item-p ()
  (with-no-warnings
    ;; HACK: we depend on open being dynamically bound while handling
    ;; indentation inside of a parenthesized expression.
    (when open
      (or (eq (char-after open) ?\()
          (eq (char-after open) ?\[)))))

(defun alpaca-indent-empty (start end end-visible indent-info)
  "Find indentation points for an empty line."
  (save-excursion
    (let* ((alpaca-indent-info indent-info)
           (sep (alpaca-indent-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match alpaca-indent-start-keywords-re valname-string))
          (progn
            (alpaca-indent-push-pos valname)
            ;; Special case for `type' declarations.
            (if (string-match "\\<type\\>" valname-string)
                (alpaca-indent-push-pos-offset valname)
              (alpaca-indent-push-pos-offset valname 0)))
        (case                           ; general case
            (alpaca-indent-find-case test)
          ;; "1.1.11"   1= vn gd rh arh
          (1 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos valname valname-string)
             (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "| "))
             (alpaca-indent-push-pos aft-rhs-sign))
          ;; "1.1.10"   2= vn gd rh
          (2 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos valname valname-string)
             (if last-line
                 (alpaca-indent-push-pos-offset guard)
               (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "| "))))
          ;; "1.1100"   3= vn gd agd
          (3 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos aft-guard)
             (if last-line (alpaca-indent-push-pos-offset valname)))
          ;; "1.1000"   4= vn gd
          (4 (alpaca-indent-push-pos valname)
             (if last-line (alpaca-indent-push-pos-offset guard 2)))
          ;; "1.0011"   5= vn rh arh
          (5 (alpaca-indent-push-pos valname)
             (if (or (and aft-valname (= (char-after rhs-sign) ?\=))
                     (= (char-after rhs-sign) ?\:))
                 (alpaca-indent-push-pos valname valname-string))
             (alpaca-indent-push-pos aft-rhs-sign))
          ;; "1.0010"   6= vn rh
          (6 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos valname valname-string)
             (if last-line (alpaca-indent-push-pos-offset valname)))
          ;; "110000"   7= vn avn
          (7 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos-offset valname))
          ;; "100000"   8= vn
          (8 (if (alpaca-indent-after-list-item-p)
                 (progn
                   (alpaca-indent-push-pos valname)
                   (alpaca-indent-push-pos-offset valname))

               (alpaca-indent-push-pos valname)
               (alpaca-indent-push-pos-offset valname)))
          ;; "001.11"   9= gd rh arh
          (9 (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "| "))
             (alpaca-indent-push-pos aft-rhs-sign))
          ;; "001.10"  10= gd rh
          (10 (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "| "))
              (if last-line (alpaca-indent-push-pos-offset guard)))
          ;; "001100"  11= gd agd
          (11 (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "|> "))
              (alpaca-indent-push-pos aft-guard))
          ;; "001000"  12= gd
          (12 (if (alpaca-indent-no-otherwise guard) (alpaca-indent-push-pos guard "|> "))
              (if last-line (alpaca-indent-push-pos-offset guard 2)))
          ;; "000011"  13= rh arh
          (13 (alpaca-indent-push-pos aft-rhs-sign))
          ;; "000010"  14= rh
          (14 (if last-line (alpaca-indent-push-pos-offset rhs-sign 2 )))
          ;; "000000"  15=
          (t (error "alpaca-indent-empty: %s impossible case" test ))))
      alpaca-indent-info)))

(defun alpaca-indent-ident (start end end-visible indent-info)
  "Find indentation points for a line starting with an identifier."
  (save-excursion
    (let*
        ((alpaca-indent-info indent-info)
         (sep (alpaca-indent-separate-valdef start end))
         (valname (pop sep))
         (valname-string (pop sep))
         (aft-valname (pop sep))
         (guard (pop sep))
         (aft-guard (pop sep))
         (rhs-sign (pop sep))
         (aft-rhs-sign (pop sep))
         (last-line (= end end-visible))
         (is-where
          (string-match "where[ \t]*" alpaca-indent-current-line-first-ident))
         (diff-first                 ; not a function def with the same name
          (or (null valname-string)
              (not (string= (s-trim valname-string)
                            (s-trim alpaca-indent-current-line-first-ident)))))

         ;; (is-type-def
         ;;  (and rhs-sign (eq (char-after rhs-sign) ?\:)))
         (test (string
                (if valname ?1 ?0)
                (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                (if (and guard (< guard end-visible)) ?1 ?0)
                (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match alpaca-indent-start-keywords-re valname-string))
          (progn
            (alpaca-indent-push-pos valname)
            (if (string-match "\\<type\\>" valname-string)
                ;; Special case for `type' declarations.
                (alpaca-indent-push-pos-offset valname)
              (if (not (string-match
                        alpaca-indent-start-keywords-re
                        alpaca-indent-current-line-first-ident))
                  (alpaca-indent-push-pos-offset valname))))
        (if (string= alpaca-indent-current-line-first-ident ":")
            (if valname (alpaca-indent-push-pos valname))
          (case                         ; general case
              (alpaca-indent-find-case test)
            ;; "1.1.11"   1= vn gd rh arh
            (1 (if is-where
                   (alpaca-indent-push-pos guard)
                 (alpaca-indent-push-pos valname)
                 (if diff-first (alpaca-indent-push-pos aft-rhs-sign))))
            ;; "1.1.10"   2= vn gd rh
            (2 (if is-where
                   (alpaca-indent-push-pos guard)
                 (alpaca-indent-push-pos valname)
                 (if last-line
                     (alpaca-indent-push-pos-offset guard))))
            ;; "1.1100"   3= vn gd agd
            (3 (if is-where
                   (alpaca-indent-push-pos-offset guard)
                 (alpaca-indent-push-pos valname)
                 (if diff-first
                     (alpaca-indent-push-pos aft-guard))))
            ;; "1.1000"   4= vn gd
            (4 (if is-where
                   (alpaca-indent-push-pos guard)
                 (alpaca-indent-push-pos valname)
                 (if last-line
                     (alpaca-indent-push-pos-offset guard 2))))
            ;; "1.0011"   5= vn rh arh
            (5 (if is-where
                   (alpaca-indent-push-pos-offset valname)
                 (alpaca-indent-push-pos valname)
                 (if diff-first
                     (alpaca-indent-push-pos aft-rhs-sign))))
            ;; "1.0010"   6= vn rh
            (6 (alpaca-indent-push-pos valname)
               (alpaca-indent-push-pos valname valname-string)
               (if last-line (alpaca-indent-push-pos-offset valname)))
            ;; "110000"   7= vn avn
            (7 (alpaca-indent-push-pos valname)
               (alpaca-indent-push-pos-offset valname))
            ;; "100000"   8= vn
            (8 (alpaca-indent-push-pos valname)
               (alpaca-indent-push-pos-offset valname))
            ;; "001.11"   9= gd rh arh
            (9 (if is-where
                   (alpaca-indent-push-pos guard)
                 (alpaca-indent-push-pos aft-rhs-sign)))
            ;; "001.10"  10= gd rh
            (10 (if is-where
                    (alpaca-indent-push-pos guard)
                  (if last-line
                      (alpaca-indent-push-pos-offset guard))))
            ;; "001100"  11= gd agd
            (11 (if is-where
                    (alpaca-indent-push-pos guard)
                  (if (alpaca-indent-no-otherwise guard)
                      (alpaca-indent-push-pos aft-guard))))
            ;; "001000"  12= gd
            (12 (if last-line (alpaca-indent-push-pos-offset guard 2)))
            ;; "000011"  13= rh arh
            (13 (alpaca-indent-push-pos aft-rhs-sign))
            ;; "000010"  14= rh
            (14 (if last-line (alpaca-indent-push-pos-offset rhs-sign 2)))
            ;; "000000"  15=
            (t (error "alpaca-indent-ident: %s impossible case" test )))))
      alpaca-indent-info)))

(defun alpaca-indent-other (start end end-visible indent-info)
  "Find indentation points for a non-empty line starting with something other
than an identifier, a guard or rhs."
  (save-excursion
    (let* ((alpaca-indent-info indent-info)
           (sep (alpaca-indent-separate-valdef start end))
           (valname (pop sep))
           (valname-string (pop sep))
           (aft-valname (pop sep))
           (guard (pop sep))
           (aft-guard (pop sep))
           (rhs-sign (pop sep))
           (aft-rhs-sign (pop sep))
           (last-line (= end end-visible))
           (test (string
                  (if valname ?1 ?0)
                  (if (and aft-valname (< aft-valname end-visible)) ?1 ?0)
                  (if (and guard (< guard end-visible)) ?1 ?0)
                  (if (and aft-guard (< aft-guard end-visible)) ?1 ?0)
                  (if (and rhs-sign (< rhs-sign end-visible)) ?1 ?0)
                  (if (and aft-rhs-sign (< aft-rhs-sign end-visible)) ?1 ?0))))
      (if (and valname-string           ; special case for start keywords
               (string-match alpaca-indent-start-keywords-re valname-string))
          (alpaca-indent-push-pos-offset valname)
        (case                           ; general case
            (alpaca-indent-find-case test)
          ;; "1.1.11"   1= vn gd rh arh
          (1 (alpaca-indent-push-pos aft-rhs-sign))
          ;; "1.1.10"   2= vn gd rh
          (2 (if last-line
                 (alpaca-indent-push-pos-offset guard)
               (alpaca-indent-push-pos-offset rhs-sign 2)))
          ;; "1.1100"   3= vn gd agd
          (3 (alpaca-indent-push-pos aft-guard))
          ;; "1.1000"   4= vn gd
          (4 (alpaca-indent-push-pos-offset guard 2))
          ;; "1.0011"   5= vn rh arh
          (5 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos aft-rhs-sign))
          ;; "1.0010"   6= vn rh
          (6 (if last-line
                 (alpaca-indent-push-pos-offset valname)
               (alpaca-indent-push-pos-offset rhs-sign 2)))
          ;; "110000"   7= vn avn
          (7 (alpaca-indent-push-pos valname)
             (alpaca-indent-push-pos-offset valname))
          ;; "100000"   8= vn
          (8 (alpaca-indent-push-pos valname))
          ;; "001.11"   9= gd rh arh
          (9 (alpaca-indent-push-pos aft-rhs-sign))
          ;; "001.10"  10= gd rh
          (10 (if last-line
                  (alpaca-indent-push-pos-offset guard)
                (alpaca-indent-push-pos-offset rhs-sign 2)))
          ;; "001100"  11= gd agd
          (11 (if (alpaca-indent-no-otherwise guard)
                  (alpaca-indent-push-pos aft-guard)))
          ;; "001000"  12= gd
          (12 (if last-line (alpaca-indent-push-pos-offset guard 2)))
          ;; "000011"  13= rh arh
          (13 (alpaca-indent-push-pos aft-rhs-sign))
          ;; "000010"  14= rh
          (14 (if last-line (alpaca-indent-push-pos-offset rhs-sign 2)))
          ;; "000000"  15=
          (t (error "alpaca-indent-other: %s impossible case" test ))))
      alpaca-indent-info)))

(defun alpaca-indent-valdef-indentation (start end end-visible curr-line-type
                                            indent-info)
  "Find indentation information for a value definition."
  (let ((alpaca-indent-info indent-info))
    (if (< start end-visible)
        (case curr-line-type
          (empty (alpaca-indent-empty start end end-visible indent-info))
          (ident (alpaca-indent-ident start end end-visible indent-info))
          (guard (alpaca-indent-guard start end end-visible indent-info))
          (rhs   (alpaca-indent-rhs start end end-visible indent-info))
          (comment (error "Comment indent should never happen"))
          (other (alpaca-indent-other start end end-visible indent-info)))
      alpaca-indent-info)))

(defun alpaca-indent-line-indentation (line-start line-end end-visible
                                               curr-line-type indent-info)
  "Compute indentation info between LINE-START and END-VISIBLE.
Separate a line of program into valdefs between offside keywords
and find indentation info for each part."
  (save-excursion
    ;; point is (already) at line-start
    (assert (eq (point) line-start))
    (let ((alpaca-indent-info indent-info)
          (start (or (alpaca-indent-in-comment line-start line-end)
                     (alpaca-indent-in-string line-start line-end))))
      (if start                         ; if comment at the end
          (setq line-end start))  ; end line before it
      ;; loop on all parts separated by off-side-keywords
      (while (and (re-search-forward alpaca-indent-off-side-keywords-re
                                     line-end t)
                  (not (or (alpaca-indent-in-comment line-start (point))
                           (alpaca-indent-in-string line-start (point)))))
        (let ((beg-match (match-beginning 0)) ; save beginning of match
              (end-match (match-end 0)))      ; save end of match
          ;; Do not try to find indentation points if off-side-keyword at
          ;; the start...
          (if (or (< line-start beg-match)
                  ;; Actually, if we're looking at a "let" inside a "do", we
                  ;; should add the corresponding indentation point.
                  (eq (char-after beg-match) ?l))
              (setq alpaca-indent-info
                    (alpaca-indent-valdef-indentation line-start beg-match
                                                   end-visible
                                                   curr-line-type
                                                   alpaca-indent-info)))
          ;; ...but keep the start of the line if keyword alone on the line
          (if (= line-end end-match)
              (alpaca-indent-push-pos beg-match))
          (setq line-start end-match)
          (goto-char line-start)))
      (alpaca-indent-valdef-indentation line-start line-end end-visible
                                     curr-line-type alpaca-indent-info))))


(defun alpaca-indent-layout-indent-info (start contour-line)
  (let ((alpaca-indent-info nil)
        (curr-line-type (alpaca-indent-type-at-point))
        line-start line-end end-visible)
    (save-excursion
      (if (eq curr-line-type 'ident)
          (let                          ; guess the type of line
              ((sep
                (alpaca-indent-separate-valdef
                 (point) (line-end-position))))
            ;; if the first ident is where or the start of a def
            ;; keep it in a global variable
            (setq alpaca-indent-current-line-first-ident
                  (if (string-match "where[ \t]*" (nth 1 sep))
                      (nth 1 sep)
                    (if (nth 5 sep)              ; is there a rhs-sign
                        (if (= (char-after (nth 5 sep)) ?\:) ;is it a typdef
                            ":" (nth 1 sep))
                      "")))))
      (while contour-line               ; explore the contour points
        (setq line-start (pop contour-line))
        (goto-char line-start)
        (setq line-end (line-end-position))
        (setq end-visible            ; visible until the column of the
              (if contour-line       ; next contour point
                  (save-excursion
                    (move-to-column
                     (alpaca-indent-point-to-col (car contour-line)))
                    (point))
                line-end))
        (unless (or (alpaca-indent-open-structure start line-start)
                    (alpaca-indent-in-comment start line-start))
          (setq alpaca-indent-info
                (alpaca-indent-line-indentation line-start line-end
                                             end-visible curr-line-type
                                             alpaca-indent-info)))))
    alpaca-indent-info))

(defun alpaca-indent-find-matching-start (regexp limit &optional pred start)
  (let ((open (alpaca-indent-open-structure limit (point))))
    (if open (setq limit (1+ open))))
  (unless start (setq start (point)))
  (when (re-search-backward regexp limit t)
    (let ((nestedcase (match-end 1))
          (outer (or (alpaca-indent-in-string limit (point))
                     (alpaca-indent-in-comment limit (point))
                     (alpaca-indent-open-structure limit (point))
                     (if (and pred (funcall pred start)) (point)))))
      (cond
       (outer
        (goto-char outer)
        (alpaca-indent-find-matching-start regexp limit pred start))
       (nestedcase
        ;; Nested case.
        (and (alpaca-indent-find-matching-start regexp limit pred)
             (alpaca-indent-find-matching-start regexp limit pred start)))
       (t (point))))))


(defun alpaca-indent-comment (open start)
  "Compute indent info for comments and text inside comments.
OPEN is the start position of the comment in which point is."
  ;; Ideally we'd want to guess whether it's commented out code or
  ;; whether it's text.  Instead, we'll assume it's text.
  (save-excursion
    (if (= open (point))
        ;; We're actually just in front of a comment: align with following
        ;; code or with comment on previous line.
        (let ((prev-line-info
               (cond
                ((eq (char-after) ?\{) nil) ;Align as if it were code.
                ((and (forward-comment -1)
                      (> (line-beginning-position 3) open))
                 ;; We're after another comment and there's no empty line
                 ;; between us.
                 (list (list (alpaca-indent-point-to-col (point)))))
                (t nil))))              ;Else align as if it were code
          ;; Align with following code.
          (forward-comment (point-max))
          ;; There are several possible indentation points for this code-line,
          ;; but the only valid indentation point for the comment is the one
          ;; that the user will select for the code-line.  Obviously we can't
          ;; know that, so we just assume that the code-line is already at its
          ;; proper place.
          ;; Strictly speaking "assume it's at its proper place" would mean
          ;; we'd just use (current-column), but since this is using info from
          ;; lines further down and it's common to reindent line-by-line,
          ;; we'll align not with the current indentation, but with the
          ;; one that auto-indentation "will" select.
          (append
           prev-line-info
           (let ((indent-info (save-excursion
                                (alpaca-indent-indentation-info start)))
                 (col (current-column)))
             ;; Sort the indent-info so that the current indentation comes
             ;; out first.
             (setq indent-info
                   (sort indent-info
                         (lambda (x y)
                           (<= (abs (- col (car x))) (abs (- col (car y)))))))
             indent-info)))

      ;; We really are inside a comment.
      (if (looking-at "-}")
          (progn
            (forward-char 2)
            (forward-comment -1)
            (list (list (1+ (alpaca-indent-point-to-col (point))))))
        (let ((offset (if (looking-at "--?")
                          (- (match-beginning 0) (match-end 0)))))
          (forward-line -1)             ;Go to previous line.
          (back-to-indentation)
          (if (< (point) start) (goto-char start))

          (list (list (if (and comment-start-skip (looking-at comment-start-skip))
                          (if offset
                              (+ 2 offset (alpaca-indent-point-to-col (point)))
                            (alpaca-indent-point-to-col (match-end 0)))
                        (alpaca-indent-point-to-col (point))))))))))

(defun alpaca-indent-closing-keyword (start)
  (let ((open (save-excursion
                (alpaca-indent-find-matching-start
                 (case (char-after)
                   (?i "\\<\\(?:\\(in\\)\\|let\\)\\>")
                   (?o "\\<\\(?:\\(of\\)\\|case\\)\\>")
                   (?t "\\<\\(?:\\(then\\)\\|if\\)\\>")
                   (?e "\\<\\(?:\\(else\\)\\|if\\)\\>"))
                 start))))
    ;; For a "hanging let/case/if at EOL" we should use a different
    ;; indentation scheme.
    (save-excursion
      (goto-char open)
      (if (alpaca-indent-hanging-p)
          (setq open (alpaca-indent-virtual-indentation start))))
    ;; FIXME: we should try and figure out if the `if' is in a `do' layout
    ;; before using alpaca-indent-thenelse.
    (list (list (+ (if (memq (char-after) '(?t ?e)) alpaca-indent-thenelse 0)
                   (alpaca-indent-point-to-col open))))))

(defun alpaca-indent-skip-lexeme-forward ()
  (and (zerop (skip-syntax-forward "w"))
       (skip-syntax-forward "_")
       (skip-syntax-forward "(")
       (skip-syntax-forward ")")))

(defun alpaca-indent-offset-after-info ()
  "Return the info from `alpaca-indent-after-keywords' for keyword at point."
  (let ((id (buffer-substring
             (point)
             (save-excursion
               (alpaca-indent-skip-lexeme-forward)
               (point)))))
    (or (assoc id alpaca-indent-after-keywords)
        (car (member id alpaca-indent-after-keywords)))))

(defun alpaca-indent-hanging-p ()
  ;; A Hanging keyword is one that's at the end of a line except it's not at
  ;; the beginning of a line.
  (not (or (= (current-column) (current-indentation))
           (save-excursion
             (let ((lexeme
                    (buffer-substring
                     (point)
                     (progn (alpaca-indent-skip-lexeme-forward) (point)))))
               (or (member lexeme alpaca-indent-dont-hang)
                   (> (line-end-position)
                      (progn (forward-comment (point-max)) (point)))))))))

(defun alpaca-indent-after-keyword-column (offset-info start &optional default)
  (unless offset-info
    (setq offset-info (alpaca-indent-offset-after-info)))
  (unless default (setq default alpaca-indent-offset))
  (setq offset-info
        (if alpaca-indent-inhibit-after-offset '(0) (cdr-safe offset-info)))
  (if (not (alpaca-indent-hanging-p))
      (alpaca-indent-column+offset (current-column)
                                (or (car offset-info) default))
    ;; The keyword is hanging at the end of the line.
    (alpaca-indent-column+offset
     (alpaca-indent-virtual-indentation start)
     (or (cadr offset-info) (car offset-info) default))))

(defun alpaca-indent-inside-paren-update-syntax-p (open)
  (let ((end (point)))
    (save-excursion
      (goto-char open)
      (and (not (looking-at ".*="))
           (progn
             (forward-line)
             (alpaca-indent-skip-blanks-and-newlines-forward end)
             (eq (char-after) ?\|))))))

(defun alpaca-indent-inside-paren (open)
  ;; there is an open structure to complete
  (if (looking-at "\\s)\\|[|;,]")
      ;; A close-paren or a , or ; can only correspond syntactically to
      ;; the open-paren at `open'.  So there is no ambiguity.
      (let* ((is-close-brace (eq (char-after) ?\}))
             (inside-update (alpaca-indent-inside-paren-update-syntax-p open)))
        (if (and (eq (char-after) ?\;) (eq (char-after open) ?\())
            (message "Mismatched punctuation: `%c' in %c...%c"
                     (char-after) (char-after open)
                     (if (eq (char-after open) ?\() ?\) ?\})))
        (save-excursion
          (goto-char open)
          (list (list
                 (if (alpaca-indent-hanging-p)
                     (alpaca-indent-virtual-indentation nil)
                   (if (and inside-update
                            (not is-close-brace)
                            (eq (char-after open) ?\{))
                       (alpaca-indent-point-to-col (+ alpaca-indent-offset open))
                     (alpaca-indent-point-to-col open)))))))
    ;; There might still be layout within the open structure.
    (let* ((end (point))
           (basic-indent-info
            ;; Anything else than a ) is subject to layout.
            (if (looking-at "\\s.\\|\\$ ")
                (alpaca-indent-point-to-col open) ; align a punct with (
              (let ((follow (save-excursion
                              (goto-char (1+ open))
                              (alpaca-indent-skip-blanks-and-newlines-forward end)
                              (point))))
                (if (= follow end)
                    (save-excursion
                      (goto-char open)
                      (alpaca-indent-after-keyword-column nil nil 1))
                  (alpaca-indent-point-to-col (+ alpaca-indent-offset follow))))))
           (open-column (alpaca-indent-point-to-col open))
           (contour-line (alpaca-indent-contour-line (1+ open) end)))
      (if (null contour-line)
          (list (list basic-indent-info))
        (let ((indent-info
               (alpaca-indent-layout-indent-info
                (1+ open) contour-line)))
          ;; Fix up indent info.
          (let ((base-elem (assoc open-column indent-info)))
            (if base-elem
                (progn (setcar base-elem basic-indent-info)
                       (setcdr base-elem nil))
              (if (not (eq basic-indent-info 0))
                  (setq indent-info (append (list (list basic-indent-info)) indent-info))
                (setq indent-info (append indent-info (list (list basic-indent-info))))))
            indent-info))))))

(defun alpaca-indent-virtual-indentation (start)
  "Compute the \"virtual indentation\" of text at point.
The \"virtual indentation\" is the indentation that text at point would have
had, if it had been placed on its own line."
  (let ((col (current-column))
        (alpaca-indent-inhibit-after-offset (alpaca-indent-hanging-p)))
    (if (save-excursion (skip-chars-backward " \t") (bolp))
        ;; If the text is indeed on its own line, than the virtual indent is
        ;; the current indentation.
        col
      ;; Else, compute the indentation that it would have had.
      (let ((info (alpaca-indent-indentation-info start))
            (max -1))
        ;; `info' is a list of possible indent points.  Each indent point is
        ;; assumed to correspond to a different parse.  So we need to find
        ;; the parse that corresponds to the case at hand (where there's no
        ;; line break), which is assumed to always be the
        ;; deepest indentation.
        (dolist (x info)
          (setq x (car x))
          ;; Sometimes `info' includes the current indentation (or yet
          ;; deeper) by mistake, because alpaca-indent-indentation-info
          ;; wasn't designed to be called on a piece of text that is not at
          ;; BOL.  So ignore points past `col'.
          (if (and (> x max) (not (>= x col)))
              (setq max x)))
        ;; In case all the indent points are past `col', just use `col'.
        (if (>= max 0) max col)))))

(defun alpaca-indent-indentation-info (&optional start)
  "Return a list of possible indentations for the current line.
These are then used by `alpaca-indent-cycle'.
START if non-nil is a presumed start pos of the current definition."
  (unless start (setq start (alpaca-indent-start-of-def)))
  (let (open contour-line)
    (cond
     ;; in string?
     ((setq open (alpaca-indent-in-string start (point)))
      (list (list (+ (alpaca-indent-point-to-col open)
                     (if (looking-at "\\\\") 0 1)))))

     ;; in comment ?
     ((setq open (alpaca-indent-in-comment start (point)))
      (alpaca-indent-comment open start))

     ;; Closing the declaration part of a `let' or the test exp part of a case.
     ((looking-at "\\(?:in\\|of\\|then\\|else\\)\\>")
      (alpaca-indent-closing-keyword start))

     ;; Right after a special keyword.
     ((save-excursion
        (forward-comment (- (point-max)))
        (when (and (not (zerop (skip-syntax-backward "w")))
                   (setq open (alpaca-indent-offset-after-info)))
          (list (list (alpaca-indent-after-keyword-column open start))))))

     ;; open structure? ie  ( { [
     ((setq open (alpaca-indent-open-structure start (point)))
      (alpaca-indent-inside-paren open))

     ;; full indentation
     ((setq contour-line (alpaca-indent-contour-line start (point)))
      (alpaca-indent-layout-indent-info start contour-line))

     (t
      ;; simple contour just one indentation at start
      (list (list (alpaca-indent-point-to-col start)))))))

(defun alpaca-indent-cycle ()
  "Indentation cycle.

We stay in the cycle as long as the TAB key is pressed."
  (interactive "*")
  (let ((marker (if (> (current-column)
                       (current-indentation))
                    (point-marker)))
        (bol (progn (beginning-of-line) (point))))
    (back-to-indentation)
    (unless (and (eq last-command this-command)
                 (eq bol (car alpaca-indent-last-info)))
      (save-excursion
        (setq alpaca-indent-last-info
              (list bol (alpaca-indent-indentation-info) 0 0))))

    (let* ((il (nth 1 alpaca-indent-last-info))
           (index (nth 2 alpaca-indent-last-info))
           (last-insert-length (nth 3 alpaca-indent-last-info))
           (indent-info (nth index il)))

      (indent-line-to (car indent-info)) ; insert indentation
      (delete-char last-insert-length)
      (setq last-insert-length 0)
      (let ((text (cdr indent-info)))
        (if text
            (progn
              (insert text)
              (setq last-insert-length (length text)))))

      (setq alpaca-indent-last-info
            (list bol il (% (1+ index) (length il)) last-insert-length))

      (if (= (length il) 1)
          (message "Sole indentation")
        (message "Indent cycle (%d)..." (length il)))

      (if marker
          (goto-char (marker-position marker))))))

(defun alpaca-indent-region (start end)
  "Apply `alpaca-indent-cycle' to every line between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (alpaca-indent-cycle)
      (forward-line))))

;;; Alignment functions
(defun alpaca-indent-shift-columns (dest-column region-stack)
  "Shift columns in REGION-STACK to go to DEST-COLUMN.
Elements of the stack are pairs of points giving the start and end
of the regions to move."
  (let (reg col diffcol reg-end)
    (while (setq reg (pop region-stack))
      (setq reg-end (copy-marker (cdr reg)))
      (goto-char (car reg))
      (setq col (current-column))
      (setq diffcol (- dest-column col))
      (if (not (zerop diffcol))
          (catch 'end-of-buffer
            (while (<= (point) (marker-position reg-end))
              (if (< diffcol 0)
                  (backward-delete-char-untabify (- diffcol) nil)
                (insert-char ?\  diffcol))
              (end-of-line 2)           ; should be (forward-line 1)
              (if (eobp)                ; but it adds line at the end...
                  (throw 'end-of-buffer nil))
              (move-to-column col)))))))

(defun alpaca-indent-align-def (p-arg type)
  "Align guards or rhs within the current definition before point.
If P-ARG is t align all defs up to the mark.
TYPE is either 'guard or 'rhs."
  (save-excursion
    (let (start-block end-block
                      (maxcol (if (eq type 'rhs) alpaca-indent-rhs-align-column 0))
                      contour sep defname defnamepos
                      defcol pos lastpos
                      regstack eqns-start start-found)
      ;; find the starting and ending boundary points for alignment
      (if p-arg
          (if (mark)                    ; aligning everything in the region
              (progn
                (when (> (mark) (point)) (exchange-point-and-mark))
                (setq start-block
                      (save-excursion
                        (goto-char (mark))
                        (line-beginning-position)))
                (setq end-block
                      (progn (if (bolp)
                                 (forward-line -1))
                             (line-end-position))))
            (error "The mark is not set for aligning definitions"))
        ;; aligning the current definition
        (setq start-block (alpaca-indent-start-of-def))
        (setq end-block (line-end-position)))
      ;; find the start of the current valdef using the contour line
      ;; in reverse order because we need the nearest one from the end
      (setq contour
            (reverse (alpaca-indent-contour-line start-block end-block)))
      (setq pos (car contour))          ; keep the start of the first contour
      ;; find the nearest start of a definition
      (while (and (not defname) contour)
        (goto-char (pop contour))
        (if (alpaca-indent-open-structure start-block (point))
            nil
          (setq sep (alpaca-indent-separate-valdef (point) end-block))
          (if (nth 5 sep)               ; is there a rhs?
              (progn (setq defnamepos (nth 0 sep))
                     (setq defname (nth 1 sep))))))
      ;; start building the region stack
      (if defnamepos
          (progn                        ; there is a valdef
            ;; find the start of each equation or guard
            (if p-arg      ; when indenting a region
                ;; accept any start of id or pattern as def name
                (setq defname "\\<\\|("))
            (setq defcol (alpaca-indent-point-to-col defnamepos))
            (goto-char pos)
            (setq end-block (line-end-position))
            (catch 'top-of-buffer
              (while (and (not start-found)
                          (>= (point) start-block))
                (if (<= (current-indentation) defcol)
                    (progn
                      (move-to-column defcol)
                      (if (and (looking-at defname) ; start of equation
                               (not (alpaca-indent-open-structure start-block (point))))
                          (push (cons (point) 'eqn) eqns-start)
                        ;; found a less indented point not starting an equation
                        (setq start-found t)))
                  ;; more indented line
                  (back-to-indentation)
                  (if (and (eq (alpaca-indent-type-at-point) 'guard) ; start of a guard
                           (not (alpaca-indent-open-structure start-block (point))))
                      (push (cons (point) 'gd) eqns-start)))
                (if (bobp)
                    (throw 'top-of-buffer nil)
                  (backward-to-indentation 1))))
            ;; remove the spurious guards before the first equation
            (while (and eqns-start (eq (cdar eqns-start) 'gd))
              (pop eqns-start))
            ;; go through each equation to find the region to indent
            (while eqns-start
              (let ((eqn (caar eqns-start)))
                (setq lastpos (if (cdr eqns-start)
                                  (save-excursion
                                    (goto-char (caadr eqns-start))
                                    (forward-line -1)
                                    (line-end-position))
                                end-block))
                (setq sep (alpaca-indent-separate-valdef eqn lastpos)))
              (if (eq type 'guard)
                  (setq pos (nth 3 sep))
                ;; check if what follows a rhs sign is more indented or not
                (let ((rhs (nth 5 sep))
                      (aft-rhs (nth 6 sep)))
                  (if (and rhs aft-rhs
                           (> (alpaca-indent-point-to-col rhs)
                              (alpaca-indent-point-to-col aft-rhs)))
                      (setq pos aft-rhs)
                    (setq pos rhs))))
              (if pos
                  (progn                ; update region stack
                    (push (cons pos (or lastpos pos)) regstack)
                    (setq maxcol        ; find the highest column number
                          (max maxcol
                               (progn   ;find the previous non-empty column
                                 (goto-char pos)
                                 (skip-chars-backward
                                  " \t"
                                  (line-beginning-position))
                                 (if (bolp)
                                     ;;if on an empty prefix
                                     (alpaca-indent-point-to-col pos) ;keep original indent
                                   (1+ (alpaca-indent-point-to-col (point)))))))))
              (pop eqns-start))
            ;; now shift according to the region stack
            (if regstack
                (alpaca-indent-shift-columns maxcol regstack)))))))


;;; Insertion functions
(defun alpaca-indent-insert-equal ()
  "Insert an = sign and align the previous rhs of the current function."
  (interactive "*")
  (if (or (bolp)
          (/= (preceding-char) ?\ ))
      (insert ?\ ))
  (insert "= ")
  (alpaca-indent-align-def mark-active 'rhs))


;;; alpaca-indent-mode
(defvar alpaca-indent-mode nil
  "Non-nil if the semi-intelligent Alpaca indentation mode is in effect.")
(make-variable-buffer-local 'alpaca-indent-mode)

(defvar alpaca-indent-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-=] 'alpaca-indent-insert-equal)
    map))

;;;###autoload
(defun turn-on-alpaca-indent ()
  "Turn on ``intelligent'' Alpaca indentation mode."
  (set (make-local-variable 'indent-line-function) 'alpaca-indent-cycle)
  (set (make-local-variable 'indent-region-function) 'alpaca-indent-region)
  (setq alpaca-indent-mode t)
  ;; Activate our keymap.
  (let ((map (current-local-map)))
    (while (and map (not (eq map alpaca-indent-map)))
      (setq map (keymap-parent map)))
    (if map
        ;; alpaca-indent-map is already active: nothing to do.
        nil
      ;; Put our keymap on top of the others.  We could also put it in
      ;; second place, or in a minor-mode.  The minor-mode approach would be
      ;; easier, but it's harder for the user to override it.  This approach
      ;; is the closest in behavior compared to the previous code that just
      ;; used a bunch of local-set-key.
      (set-keymap-parent alpaca-indent-map (current-local-map))
      ;; Protect our keymap.
      (setq map (make-sparse-keymap))
      (set-keymap-parent map alpaca-indent-map)
      (use-local-map map)))
  (run-hooks 'alpaca-indent-hook))

(defun turn-off-alpaca-indent ()
  "Turn off ``intelligent'' Alpaca indentation mode."
  (kill-local-variable 'indent-line-function)
  ;; Remove alpaca-indent-map from the local map.
  (let ((map (current-local-map)))
    (while map
      (let ((parent (keymap-parent map)))
        (if (eq alpaca-indent-map parent)
            (set-keymap-parent map (keymap-parent parent))
          (setq map parent)))))
  (setq alpaca-indent-mode nil))

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'alpaca-indent-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((alpaca-indent-mode " Alpaca Indent")))))

;;;###autoload
(defun alpaca-indent-mode (&optional arg)
  "``Intelligent'' Alpaca indentation mode.

This deals with the layout rules of Alpaca.

\\[alpaca-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.

Other special keys are:

    \\[alpaca-indent-insert-equal]
      inserts an =

Invokes `alpaca-indent-hook' if not nil."
  (interactive "P")
  (setq alpaca-indent-mode
        (if (null arg)
            (not alpaca-indent-mode)
          (> (prefix-numeric-value arg) 0)))
  (if alpaca-indent-mode
      (turn-on-alpaca-indent)
    (turn-off-alpaca-indent)))

(provide 'alpaca-indent)
;;; alpaca-indent.el ends here
