;;;; IR - Intermediate Representation Types
;;;;
;;;; Each variant is a node in the IR tree.
;;;; Every pass that processes IR must handle ALL variants.
;;;; The match macro enforces this at compile time.

(defpackage :habu.ir
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :ir-node
           :ir-lit :ir-nil :ir-t :ir-str :ir-sym :ir-kw
           :ir-var :ir-setq :ir-global :ir-set-global
           :ir-add :ir-sub :ir-mul :ir-div :ir-mod :ir-neg
           :ir-eq :ir-eql :ir-lt :ir-gt :ir-le :ir-ge :ir-zerop
           :ir-not :ir-and :ir-or
           :ir-band :ir-bor :ir-bxor :ir-bsh :ir-bnot
           :ir-if :ir-progn :ir-while :ir-let
           :ir-call :ir-lambda :ir-funcall
           :ir-cons :ir-car :ir-cdr :ir-list
           :ir-null :ir-consp :ir-symbolp :ir-stringp :ir-numberp :ir-keywordp :ir-functionp
           :ir-string-length :ir-string-ref :ir-string-concat
           :ir-make-vector :ir-vector-ref :ir-vector-set :ir-vector-length
           :ir-make-symbol :ir-symbol-name :ir-intern
           :ir-keyword-name
           :ir-exit :ir-error))

(in-package :habu.ir)

(deftype ir-node
  ;; === Literals ===
  (ir-lit value)              ; integer literal (tagged fixnum)
  (ir-nil)                    ; nil literal
  (ir-t)                      ; t literal
  (ir-str string)             ; string literal
  (ir-sym name)               ; symbol literal (quoted)
  (ir-kw name)                ; keyword literal

  ;; === Variables ===
  (ir-var offset)             ; local variable reference
  (ir-setq offset value)      ; variable assignment
  (ir-global name)            ; global variable reference
  (ir-set-global name value)  ; global assignment

  ;; === Arithmetic ===
  (ir-add left right)
  (ir-sub left right)
  (ir-mul left right)
  (ir-div left right)
  (ir-mod left right)
  (ir-neg value)              ; unary negation

  ;; === Comparison ===
  (ir-eq left right)          ; pointer equality
  (ir-eql left right)         ; value equality
  (ir-lt left right)
  (ir-gt left right)
  (ir-le left right)
  (ir-ge left right)
  (ir-zerop value)

  ;; === Logical ===
  (ir-not value)
  (ir-and left right)         ; short-circuit and
  (ir-or left right)          ; short-circuit or

  ;; === Bitwise ===
  (ir-band left right)
  (ir-bor left right)
  (ir-bxor left right)
  (ir-bsh value shift)        ; bit shift
  (ir-bnot value)             ; bitwise not

  ;; === Control Flow ===
  (ir-if test then else)
  (ir-progn forms)            ; sequence, forms is a list
  (ir-while test body)
  (ir-let bindings body)      ; bindings is list of (offset . init)

  ;; === Functions ===
  (ir-call name args)         ; named function call, args is a list
  (ir-lambda params body captures) ; lambda with capture list
  (ir-funcall fn args)        ; indirect call through closure/function

  ;; === List Operations ===
  (ir-cons car cdr)
  (ir-car cell)
  (ir-cdr cell)
  (ir-list elems)             ; list constructor

  ;; === Type Predicates ===
  (ir-null value)
  (ir-consp value)
  (ir-symbolp value)
  (ir-stringp value)
  (ir-numberp value)
  (ir-keywordp value)
  (ir-functionp value)

  ;; === String Operations ===
  (ir-string-length str)
  (ir-string-ref str index)
  (ir-string-concat left right)

  ;; === Vector Operations ===
  (ir-make-vector size init)
  (ir-vector-ref vec index)
  (ir-vector-set vec index value)
  (ir-vector-length vec)

  ;; === Symbol Operations ===
  (ir-make-symbol name)
  (ir-symbol-name sym)
  (ir-intern str)

  ;; === Keyword Operations ===
  (ir-keyword-name kw)

  ;; === System ===
  (ir-exit code)
  (ir-error message))

;; Convenience: count variants for documentation
;; Current count: 60 variants
