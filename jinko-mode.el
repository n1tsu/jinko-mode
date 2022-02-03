;;; jinko-mode.el --- Major mode for editing Jinko -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Augustin Thiercelin
;;
;; Author: Augustin Thiercelin <https://github.com/n1tsu>
;; Maintainer: Augustin Thiercelin <augustin.thiercelin@epita.fr>
;; Created: February 02, 2022
;; Modified: February 02, 2022
;; Version: 1.0.0
;; Keywords: languages jinko
;; Homepage: https://github.com/n1tsu/jinko-mode
;; Package-Requires: ((emacs "26.1"))
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
        "func" "ext" "test" "mock" "mut" "return" "type" "incl" "as"   ;; statements
        "true" "false"))                                               ;; boolean

(defvar jinko-types nil "all types of jinko.")
(setq jinko-types
      '("int" "string" "float" "char" "bool"))

(defvar jinko-fixme nil "fixme keyword.")
(setq jinko-fixme
      '("FIXME"))

(defvar jinko-todo nil "todo keyword.")
(setq jinko-todo
      '("TODO"))

(defface jinko-fixme-face
  '((t(:foreground "#FF4500" :weight bold)))
  "Face for FIXME.")

(defface jinko-todo-face
  '((t(:foreground "#FFD700" :weight bold)))
  "Face for TODO.")

(defface jinko-numbers-face
  '((t(:foreground "#E69055" :weight bold)))
  "Face for number.")

(setq jinko-highlights
      `((,(regexp-opt jinko-keywords 'words) . 'font-lock-keyword-face)
        (,(regexp-opt jinko-types 'words) . 'font-lock-type-face)
        (,(regexp-opt jinko-fixme 'words) 0 'jinko-fixme-face t)
        (,(regexp-opt jinko-todo 'words) 0 'jinko-todo-face t)
        ("\"\\.\\*\\?" . font-lock-string-face)
        ("func \\(\\sw+\\)" (1 font-lock-function-name-face))
        ("\\(\\sw+\\)::\\sw+" (1 font-lock-constant-face))
        ("\\([[:digit:]]+\\)" . 'jinko-numbers-face)))

(defvar jinko-mode-syntax-table nil "Syntax table for `jinko-mode'.")
(setq jinko-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?\/ ". 124" syn-table)
        (modify-syntax-entry ?* ". 23b" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        (modify-syntax-entry ?# "<" syn-table)
        (modify-syntax-entry ?_ "w" syn-table)
        syn-table))

(define-derived-mode jinko-mode fundamental-mode "Jinko"
  "major mode for editing jinko language code."
  (setq comment-start "// ") ;; FIXME Might actually be '# '
  (setq comment-end "")
  (setq font-lock-defaults '(jinko-highlights))
  (display-line-numbers-mode))

(add-to-list 'auto-mode-alist '("\\.jk\\'" . jinko-mode))

(provide 'jinko-mode)

;;; jinko-mode.el ends here
