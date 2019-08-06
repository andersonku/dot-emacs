;;; gxsl-mode.el --- for Goanna XPath Specification Language
;;
;; Filename: gxsl-mode.el
;; Description: major mode for Goanna XPath Specification Language
;; Author: Chunxiao Lin
;; Maintainer:
;; Created: Mon Jan 12 11:06:45 2015 (+1100)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Mon Jan 12 14:14:34 2015 (+1100)
;;           By: Chunxiao Lin
;;     Update #: 4
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
;;    (load "gxsl-mode")
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

(define-generic-mode 'gxsl-mode
  (list ?# (cons "(*" "*)"))
  nil
  (list
   ;; number
   (list "\\(\s+\\|\b\\)\\([-+]?[0-9]*\.?[0-9]+\\)\\(\s+\\|\b\\)" 2 'font-lock-constant-face t)
   ;; gxsl constans
   (generic-make-keywords-list
    (list
     "true" "fasle"
     ) 'font-lock-constant-face)
   ;; gxsl keywords
   (generic-make-keywords-list
    (list
     "after" "always" "and" "before" "change_context" "check" "concat" "ctl" "ctlr" "db" "endprop"
     "eventually" "every" "exists" "first" "firstst" "have" "holds" "in" "is" "join"
     "intersect" "label" "not" "last" "lastst" "never" "on" "or" "select" "selectref" "some"
     "strictly" "syntax" "warn" "where" "with"
     "forall" "let" "nextgroup=gxslIdentifiers" "skipwhite" "fun"
     ) 'font-lock-keyword-face)
   ;; function decl
   (list "^[@ \t]*fun\s+\\([-a-zA-Z_0-9]*\\)" 1 'font-lock-function-name-face t)
   ;; identifiers
   (list "\\(forall\\|let\\)\s+\\([-a-zA-Z_0-9]+\\)\s+in" 2 'font-lock-variable-name-face t)
   ;; operators
   (list "\\(!=\\|[<>]=\\|[<=>~^+-\*]\\)" 1 'font-lock-builtin-face t)
   ;; AST tags
   (list "\\(@[-a-zA-Z_0-9]+\\)" 1 'font-lock-builtin-face t)
   ;; EDG AST nodes
   (generic-make-keywords-list
    (list
     "Block" "Cast" "End" "Exp" "Field" "Float" "FunCall" "FunDecl" "FunProto"
     "ClassDecl" "FunRef" "Handler" "If" "IntConst" "Integer" "Member" "NullStmt"
     "Op1" "Op2" "Op2Builtin" "Op3" "Pointer" "Ref" "Return" "SizeofExpr" "AlignofExpr"
     "Struct" "Type" "TypeRef" "StructDecl" "UnionDecl" "Union" "Void" "VarDecl"
     "Ellipsis" "For" "While" "Do" "StringConst" "FloatConst" "Goto" "Label" "Try"
     "Throw" "Case" "Switch" "Asm" "Include" "TypeDef" "Enum" "Define" "Undef" "Comment"
     "SourceFile" "PPIf" "PPIfDef" "PPIfnDef" "PPElIf" "Array"
     "NonExistentNode" "Class"
     "Aggregate" "PtrDeref" "Subscript" "RefDeref" "Arrow" "Fun" "Bool" "Dot"
     "PtrAddr" "RefAddr"
     "Multiply" "Divide" "Modulo" "Add" "Subtract" "ShiftLeft" "ShiftRight" "RDiv"
     "Assign" "Init" "PostInc" "PostDec" "PreInc" "PreDec"
     "Lt" "Gt" "Le" "Ge" "Eq" "Ne"
     "BitAnd" "BitXor" "BitOr" "BitNot" "LogicalAnd" "LogicalOr" "LogicalNot"
     "AddAssign" "SubtractAssign" "MultiplyAssign" "DivideAssign" "ModuloAssign" "RDivAssign"
     "ShiftLeftAssign" "ShiftRightAssign" "BitAndAssign" "BitOrAssign" "BitXorAssign"
     ) 'font-lock-constant-face)
   ;; xpath keywords
   (generic-make-keywords-list
    (list
     "union" "intersect" "except" "div" "idiv" "mod" "eq" "ne" "lt" "le" "gt" "ge" "is" "and" "or" "not" "return" "for" "in" "if" "then" "else"
     "some" "every" "satisfies" "instance" "of" "cast" "as" "castable" "treat"
     ) 'font-lock-keyword-face)
   ;; xpath identifiers
   (generic-make-keywords-list
    (list
     "child" "descendant" "attribute" "self" "descendant-or-self" "following-sibling" "following" "namespace"
     "parent" "ancestor" "preceding-sibling" "preceding" "ancestor-or-self"
     ) 'font-lock-type-face)
   ;; comments
   (list "\\((\\*.*\\*)\\)" 1 'font-lock-comment-face t)
   (list "\\(#.*\\)\n" 1 'font-lock-comment-face t)
   )
  (list "\\.gxsl\\'")
  nil
  "Major mode for editing gxsl files "
  )

(add-hook 'gxsl-mode-hook
	  (lambda ()
	    (modify-syntax-entry ?/ ".")
	    (modify-syntax-entry ?< ".")
	    (modify-syntax-entry ?> ".")
	    )
	  )

(provide 'gxsl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gxsl-mode.el ends here
