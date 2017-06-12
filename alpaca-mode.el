;;; alpaca-mode --- A major mode for the Alpaca language.

;; Copyright (C) 2016 The Alpaca Community
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Author: Eric Bailey, Tyler Weir
;; Keywords: languages alpaca

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
