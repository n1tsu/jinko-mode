;; jinko-mode.el --- Major mode for editing Jinko -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Augustin Thiercelin
;;
;; Author: Augustin Thiercelin <https://github.com/n1tsu>
;; Maintainer: Augustin Thiercelin <augustin.thiercelin@outlook.com>
;; Created: February 02, 2022
;; Modified: February 02, 2022
;; Version: 1.0.0
;; Keywords: languages jinko
;; Homepage: https://github.com/n1tsu/jinko-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode for Jinko
;;
;;; Code:

;; (setq jinko-keywords
;;       '("incl" "func" "mut" "for" "in" "if" "while" "test" "loop" "bool" "type"))

;; (setq jinko-types
;;       '("float" "int" "string" "void"))

;; (setq jinko-keywords-regexp
;;       (regexp-opt jinko-keywords 'words))

;; (setq jinko-types-regexp
;;       (regexp-opt jinko-types 'words))

(setq jinko-highlights
      '(("incl\\|func\\|mut\\|for\\|in\\|if\\|while\\|test\\|loop" . 'font-lock-keyword-face)
        ("float\\|int\\|string\\|void" . 'font-lock-type-face)))

(defvar jinko-mode-syntax-table nil "Syntax table for `jinko-mode'.")
(setq jinko-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        (modify-syntax-entry ?# "<" syn-table)
        syn-table))

(define-derived-mode jinko-mode fundamental-mode "Jinko"
  "major mode for editing jinko language code."
  (setq comment-start "// ") ;; FIXME Might actually be '# '
  (setq comment-end "")
  (setq font-lock-defaults '(jinko-highlights)))

(add-to-list 'auto-mode-alist '("\\.jk\\'" . jinko-mode))

(provide 'jinko-mode)

;;; jinko-mode.el ends here
