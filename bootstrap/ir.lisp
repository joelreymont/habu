;;;; IR - Intermediate Representation Types
;;;;
;;;; Each variant is a node in the IR tree.
;;;; Every pass that processes IR must handle ALL variants.
;;;; The match macro enforces this at compile time.
;;;;
;;;; With :prefix ir, we write (lit value) and get ir-lit constructor.
;;;; Match patterns use short names: (match ir-node x (lit (v) ...) ...)

(defpackage :habu.ir
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :ir-node
           ;; Literals
           :ir-lit :ir-nil :ir-t :ir-str :ir-sym :ir-kw
           ;; Variables
           :ir-var :ir-setq :ir-global :ir-set-global
           ;; Arithmetic
           :ir-add :ir-sub :ir-mul :ir-div :ir-mod :ir-neg
           ;; Comparison
           :ir-eq :ir-eql :ir-lt :ir-gt :ir-le :ir-ge :ir-zerop
           ;; Logical
           :ir-not :ir-and :ir-or
           ;; Bitwise
           :ir-band :ir-bor :ir-bxor :ir-bsh :ir-bnot
           ;; Control flow
           :ir-if :ir-progn :ir-while :ir-let
           ;; Functions
           :ir-call :ir-lambda :ir-funcall
           ;; List operations
           :ir-cons :ir-car :ir-cdr :ir-list :ir-length
           ;; Type predicates
           :ir-null :ir-consp :ir-symbolp :ir-stringp :ir-numberp :ir-keywordp :ir-functionp
           ;; String operations
           :ir-string-length :ir-string-ref :ir-string-concat
           ;; Vector operations
           :ir-make-vector :ir-vector-ref :ir-vector-set :ir-vector-length
           ;; Symbol operations
           :ir-make-symbol :ir-symbol-name :ir-intern
           ;; Keyword operations
           :ir-keyword-name
           ;; System
           :ir-exit :ir-error))

(in-package :habu.ir)

(deftype ir-node :prefix ir
  ;; === Literals ===
  (lit value)              ; integer literal (tagged fixnum)
  (nil)                    ; nil literal
  (t)                      ; t literal
  (str string)             ; string literal
  (sym name)               ; symbol literal (quoted)
  (kw name)                ; keyword literal

  ;; === Variables ===
  (var offset)             ; local variable reference
  (setq offset value)      ; variable assignment
  (global name)            ; global variable reference
  (set-global name value)  ; global assignment

  ;; === Arithmetic ===
  (add left right)
  (sub left right)
  (mul left right)
  (div left right)
  (mod left right)
  (neg value)              ; unary negation

  ;; === Comparison ===
  (eq left right)          ; pointer equality
  (eql left right)         ; value equality
  (lt left right)
  (gt left right)
  (le left right)
  (ge left right)
  (zerop value)

  ;; === Logical ===
  (not value)
  (and left right)         ; short-circuit and
  (or left right)          ; short-circuit or

  ;; === Bitwise ===
  (band left right)
  (bor left right)
  (bxor left right)
  (bsh value shift)        ; bit shift
  (bnot value)             ; bitwise not

  ;; === Control Flow ===
  (if test then else)
  (progn forms)            ; sequence, forms is a list
  (while test body)
  (let bindings body)      ; bindings is list of (offset . init)

  ;; === Functions ===
  (call name args)         ; named function call, args is a list
  (lambda params body captures) ; lambda with capture list
  (funcall fn args)        ; indirect call through closure/function

  ;; === List Operations ===
  (cons car cdr)
  (car cell)
  (cdr cell)
  (list elems)             ; list constructor
  (length list)            ; list length

  ;; === Type Predicates ===
  (null value)
  (consp value)
  (symbolp value)
  (stringp value)
  (numberp value)
  (keywordp value)
  (functionp value)

  ;; === String Operations ===
  (string-length str)
  (string-ref str index)
  (string-concat left right)

  ;; === Vector Operations ===
  (make-vector size init)
  (vector-ref vec index)
  (vector-set vec index value)
  (vector-length vec)

  ;; === Symbol Operations ===
  (make-symbol name)
  (symbol-name sym)
  (intern str)

  ;; === Keyword Operations ===
  (keyword-name kw)

  ;; === System ===
  (exit code)
  (error message))

;; Convenience: count variants for documentation
;; Current count: 60 variants
