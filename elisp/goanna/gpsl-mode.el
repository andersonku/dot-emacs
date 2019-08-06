;;; gpsl-mode.el --- for Goanna Property Specification Language
;;
;; Filename: gpsl-mode.el
;; Description: major mode for Goanna Property Specification Language
;; Author: Chunxiao Lin
;; Maintainer:
;; Created: Mon Jan 12 14:11:36 2015 (+1100)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Mon Jan 12 14:57:54 2015 (+1100)
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
;;    (load "gpsl-mode")
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

(define-generic-mode 'gpsl-mode
  (list ?# (cons "(*" "*)"))
  nil
  (list
   ;; keywords
   (generic-make-keywords-list
    (list
      "AFTER" "ALWAYS" "AND" "BEFORE" "CHANGE_CONTEXT" "CONCAT" "CTL" "CTLR" "DB" "ENDPROP"
      "EVENTUALLY" "EVERY" "EXISTS" "FIRST" "FIRSTST" "FORALL" "FUN" "HAVE" "HOLDS" "IN" "IS" "JOIN"
      "LABEL" "LET" "NOT" "LAST" "LASTST" "LINE" "NEVER" "ON" "OR" "SELECT" "SELECTREF" "SOME"
      "STRICTLY" "SYNTAX" "WARN" "WHERE" "WITH" "CHECK"
      ) 'font-lock-keyword-face)
   ;; check decl
   (list "^[@ \t]*CHECK\s+\\([-a-zA-Z_0-9]*\\)" 1 'font-lock-function-name-face t)
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
  (list "\\.gpsl\\'")
  nil
  "Major mode for editing gpsl files "
  )

(provide 'gpsl-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gpsl-mode.el ends here
