;;;; IR - Intermediate Representation ADT
;;;;
;;;; Proper sum type for IR nodes with exhaustiveness checking.
;;;; Replaces the numeric ir-tag-* functions with type-safe constructors.
;;;;
;;;; Uses :habu package directly so ir-* functions are available
;;;; without package prefixes in habu0.lisp (build concatenates files).

(in-package :habu)

;;; IR ADT - all IR node types with :prefix ir
;;; Match patterns use short names: (match ir node (lit (v) ...) (add (l r) ...) ...)

(habu.types:deftype ir :prefix ir
  ;; === Literals and Variables ===
  (lit value)                    ; fixnum literal
  (var offset)                   ; variable reference by env offset
  (nil)                          ; nil literal (no fields)
  (t)                            ; t literal (no fields)
  (str-lit value)                ; string literal
  (kw-lit value)                 ; keyword literal
  (quote-sym value)              ; quoted symbol

  ;; === Arithmetic ===
  (add left right)
  (sub left right)
  (mul left right)
  (div left right)
  (mod left right)
  (neg value)                    ; unary negation

  ;; === Comparison ===
  (cmp-eq left right)            ; numeric ==
  (cmp-lt left right)            ; numeric <
  (cmp-gt left right)            ; numeric >
  (cmp-le left right)            ; numeric <=
  (cmp-ge left right)            ; numeric >=
  (eq left right)                ; pointer eq
  (eql left right)               ; eql
  (sym-eq left right)            ; symbol name equality

  ;; === Control Flow ===
  (if test then else)
  (let offset value body)        ; let binding
  (progn forms)                  ; sequence (forms is a list)
  (setq offset value)            ; variable assignment

  ;; === List Operations ===
  (cons car cdr)
  (car cell)
  (cdr cell)
  (null value)                   ; null predicate
  (length list)                  ; list length

  ;; === Type Predicates ===
  (consp value)
  (symbolp value)
  (numberp value)
  (stringp value)
  (keywordp value)

  ;; === Bitwise/Logical ===
  (logand left right)
  (logior left right)
  (lognot value)
  (ash value count)
  (not value)                    ; boolean not

  ;; === String Operations ===
  (str-len str)
  (str-ref str index)
  (string-eq left right)
  (symbol-name sym)
  (keyword-name kw)
  (make-string-from-vector vec)
  (make-symbol-from-string str)

  ;; === Vector Operations ===
  (make-vector size init)
  (vector-ref vec index)
  (vector-set vec index value)
  (vector-length vec)

  ;; === Tag Operations ===
  (get-tag value)
  (set-tag value tag)

  ;; === Functions ===
  (lambda params body free-vars free-offsets)
  (lambda-ref name free-offsets) ; reference to lifted lambda
  (funcall fn args)
  (call name args)               ; named function call

  ;; === Error ===
  (error message))

;;; Lambda Pipeline ADT
;;;
;;; Lift-lambdas extracts lambda-ir nodes and returns lambda-entry records.
;;; lambdas-to-defuns converts lambda-entry to defun-fn.
;;; This ADT makes the conversion explicit and type-safe.

(habu.types:deftype lambda-entry :prefix le
  "Lifted lambda before conversion to defun.
   Created by lift-lambdas, consumed by lambdas-to-defuns."
  (entry name params body free-vars free-offsets))

(habu.types:deftype defun-fn :prefix df
  "Function definition ready for codegen.
   param-base is the environment offset where params start
   (after captured vars for closures, 0 for regular functions)."
  (fn name params body param-base))
