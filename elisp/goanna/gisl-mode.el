;;; gisl-mode.el --- for Goanna Interprocedural Specification Language
;;
;; Filename: gisl-mode.el
;; Description: major mode for Goanna Interprocedural Specification Language
;; Author: Chunxiao Lin
;; Maintainer:
;; Created: Mon Jan 12 11:46:42 2015 (+1100)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Mon Jan 12 14:14:11 2015 (+1100)
;;           By: Chunxiao Lin
;;     Update #: 3
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; To use this, add
;;    (load "gisl-mode")
;; to your .emacs file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'generic)
(require 'font-lock)

(define-generic-mode 'gisl-mode
  (list ?# (cons "(*" "*)"))
  nil
  (list
   ;; effects
   (generic-make-keywords-list
    (list
     "true" "maybe"
     ) 'font-lock-constant-face)
   ;; keywords
   (generic-make-keywords-list
    (list
     "AFTER" "ALWAYS" "AND" "BEFORE" "CHANGE_CONTEXT" "CONCAT" "CTL" "CTLR" "DB" "ENDPROP"
     "EVENTUALLY" "EVERY" "EXISTS" "FIRST" "FIRSTST" "FORALL" "FUN" "HAVE" "HOLDS" "IN" "IS" "JOIN"
     "LABEL" "LET" "NOT" "LAST" "LASTST" "LINE" "NEVER" "ON" "OR" "SELECT" "SELECTREF" "SOME"
     "STRICTLY" "SYNTAX" "WARN" "WHERE" "WITH" "SIDE" "EFFECT" "PARAM" "PROP"
      ) 'font-lock-keyword-face)
   ;; check decl
   (list "^[@ \t]*PROP\s+\\([-a-zA-Z_0-9]*\\)\\(.*\\)\n"
	 (list 1 font-lock-function-name-face)
	 (list 2 font-lock-doc-face))
   ;; tag name
   (list " \\([-_+A-Z0-9]\+\\)=" 1 'font-lock-variable-name-face t)
   ;; operators
   (list "\\(!=\\|[<>]=\\|[<=>~^]\\)" 1 'font-lock-builtin-face t)
   ;; preprocess
   (list "%\\([-_+A-Z0-9]\+\\)%" 1 'font-lock-constant-face t)
   ;; variables
   (list "\\(FORALL\\|EXISTS\\)\s+\\([A-Za-z0-9_]+\\)\s+IN" 2 'font-lock-variable-name-face t)
   ;; comments
   (list "\\((\\*.*\\*)\\)" 1 'font-lock-comment-face t)
   (list "\\(#.*\\)\n" 1 'font-lock-comment-face t)
   )
  (list "\\.gisl\\'")
  nil
  "Major mode for editing gisl files "
  )

(provide 'gisl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gisl-mode.el ends here
