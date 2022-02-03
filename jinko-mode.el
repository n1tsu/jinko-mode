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

(defvar jinko-keywords nil "all keywords of jinko.")
(setq jinko-keywords
      '("if" "else"                                                    ;; conditional
        "for" "in" "while" "loop"                                      ;; repeat
        "func" "ext" "test" "mock" "mut" "return" "type" "incl" "as")) ;; statement

(defvar jinko-types nil "all types of jinko.")
(setq jinko-types
      '("int" "string" "float" "char" "bool"))

(setq jinko-highlights
      `((,(regexp-opt jinko-keywords 'word) . 'font-lock-keyword-face)
        (,(regexp-opt jinko-types 'word) . 'font-lock-type-face)))

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
