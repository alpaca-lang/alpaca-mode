;;; alpaca-mode.el --- A major mode for the Alpaca language.

;; Copyright (C) 2016-2017 The Alpaca Community

;; Author: Eric Bailey, Tyler Weir
;; Keywords: languages
;; Version: 0.1

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

;;

;;; Code:

(require 'alpaca-indent)
(require 'alpaca-font-lock)

(defgroup alpaca nil
  "Support for the alpaca programming language."
  :link '(url-link :tag "Github" "https://github.com/alpaca-lang/alpaca-mode")
  :group 'languages)

;;;###autoload
(define-derived-mode alpaca-mode prog-mode "Alpaca"
  "Major mode for editing Alpaca source code."
  (setq-local indent-tabs-mode nil)

  (when (boundp 'electric-indent-inhibit)
    (setq-local electric-indent-inhibit t))

  (setq-local comment-start "--")
  (setq-local comment-end "")

  (turn-on-alpaca-font-lock)
  (turn-on-alpaca-indent))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.alp\\'" . alpaca-mode))

(provide 'alpaca-mode)
;;; alpaca-mode.el ends here
