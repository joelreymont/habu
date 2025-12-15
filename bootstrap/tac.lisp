;;;; TAC - Three Address Code
;;;;
;;;; Linear instruction sequence with virtual registers.
;;;; Each instruction has at most 3 operands.
;;;; Virtual registers are integers starting from 0.
;;;;
;;;; With :prefix tac, we write (lit dest value) and get tac-lit constructor.
;;;; Match patterns use short names: (match tac-instr x (lit (d v) ...) ...)

(defpackage :habu.tac
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :tac-instr :tac-def :tac-use
           ;; Data movement
           :tac-lit :tac-nil :tac-t :tac-move :tac-var :tac-setvar
           :tac-global :tac-set-global
           ;; Arithmetic
           :tac-add :tac-sub :tac-mul :tac-div :tac-mod :tac-neg
           ;; Comparison
           :tac-eq :tac-eql :tac-lt :tac-gt :tac-le :tac-ge :tac-zerop
           ;; Logical/bitwise
           :tac-not :tac-band :tac-bor :tac-bxor :tac-bsh :tac-bnot
           ;; Control flow
           :tac-label :tac-goto :tac-if :tac-ifnot :tac-return
           ;; Function calls
           :tac-param :tac-arg :tac-call :tac-funcall
           ;; List operations
           :tac-cons :tac-car :tac-cdr :tac-list
           ;; Type predicates
           :tac-null :tac-consp :tac-symbolp :tac-stringp :tac-numberp
           :tac-keywordp :tac-functionp
           ;; String operations
           :tac-string-length :tac-string-ref :tac-string-concat :tac-string-lit
           ;; Vector operations
           :tac-make-vector :tac-vector-ref :tac-vector-set :tac-vector-length
           ;; Symbol operations
           :tac-make-symbol :tac-symbol-name :tac-intern :tac-symbol-lit
           ;; Keyword operations
           :tac-keyword-name :tac-keyword-lit
           ;; System
           :tac-exit :tac-error))

(in-package :habu.tac)

(deftype tac-instr :prefix tac
  ;; === Data Movement ===
  (lit dest value)         ; dest := literal
  (nil dest)               ; dest := nil
  (t dest)                 ; dest := t
  (move dest src)          ; dest := src
  (var dest offset)        ; dest := env[offset]
  (setvar offset src)      ; env[offset] := src
  (global dest name)       ; dest := global[name]
  (set-global name src)    ; global[name] := src

  ;; === Arithmetic (dest := left op right) ===
  (add dest left right)
  (sub dest left right)
  (mul dest left right)
  (div dest left right)
  (mod dest left right)
  (neg dest value)

  ;; === Comparison (dest := left cmp right) ===
  (eq dest left right)
  (eql dest left right)
  (lt dest left right)
  (gt dest left right)
  (le dest left right)
  (ge dest left right)
  (zerop dest value)

  ;; === Logical ===
  (not dest value)

  ;; === Bitwise ===
  (band dest left right)
  (bor dest left right)
  (bxor dest left right)
  (bsh dest value shift)
  (bnot dest value)

  ;; === Control Flow ===
  (label name)
  (goto target)
  (if cond then-label)     ; if cond goto then-label
  (ifnot cond else-label)  ; if not cond goto else-label
  (return value)

  ;; === Function Calls ===
  (param dest index)       ; dest := parameter[index]
  (arg index src)          ; set argument[index] := src before call
  (call dest name nargs)   ; dest := name(args...)
  (funcall dest fn nargs)  ; dest := fn(args...) indirect

  ;; === List Operations ===
  (cons dest car cdr)
  (car dest cell)
  (cdr dest cell)
  (list dest elems)        ; elems is list of vregs

  ;; === Type Predicates ===
  (null dest value)
  (consp dest value)
  (symbolp dest value)
  (stringp dest value)
  (numberp dest value)
  (keywordp dest value)
  (functionp dest value)

  ;; === String Operations ===
  (string-length dest str)
  (string-ref dest str index)
  (string-concat dest left right)
  (string-lit dest string) ; load string literal

  ;; === Vector Operations ===
  (make-vector dest size init)
  (vector-ref dest vec index)
  (vector-set vec index value)
  (vector-length dest vec)

  ;; === Symbol Operations ===
  (make-symbol dest name)
  (symbol-name dest sym)
  (intern dest str)
  (symbol-lit dest name)   ; load symbol literal

  ;; === Keyword Operations ===
  (keyword-name dest kw)
  (keyword-lit dest name)  ; load keyword literal

  ;; === System ===
  (exit code)
  (error message))

;; Virtual register operations for analysis

(defun tac-def (instr)
  "Return the vreg defined by this instruction, or nil"
  (match tac-instr instr
    (lit (dest value) dest)
    (nil (dest) dest)
    (t (dest) dest)
    (move (dest src) dest)
    (var (dest offset) dest)
    (setvar (offset src) nil)
    (global (dest name) dest)
    (set-global (name src) nil)
    (add (dest left right) dest)
    (sub (dest left right) dest)
    (mul (dest left right) dest)
    (div (dest left right) dest)
    (mod (dest left right) dest)
    (neg (dest value) dest)
    (eq (dest left right) dest)
    (eql (dest left right) dest)
    (lt (dest left right) dest)
    (gt (dest left right) dest)
    (le (dest left right) dest)
    (ge (dest left right) dest)
    (zerop (dest value) dest)
    (not (dest value) dest)
    (band (dest left right) dest)
    (bor (dest left right) dest)
    (bxor (dest left right) dest)
    (bsh (dest value shift) dest)
    (bnot (dest value) dest)
    (label (name) nil)
    (goto (target) nil)
    (if (cond then-label) nil)
    (ifnot (cond else-label) nil)
    (return (value) nil)
    (param (dest index) dest)
    (arg (index src) nil)
    (call (dest name nargs) dest)
    (funcall (dest fn nargs) dest)
    (cons (dest car cdr) dest)
    (car (dest cell) dest)
    (cdr (dest cell) dest)
    (list (dest elems) dest)
    (null (dest value) dest)
    (consp (dest value) dest)
    (symbolp (dest value) dest)
    (stringp (dest value) dest)
    (numberp (dest value) dest)
    (keywordp (dest value) dest)
    (functionp (dest value) dest)
    (string-length (dest str) dest)
    (string-ref (dest str index) dest)
    (string-concat (dest left right) dest)
    (string-lit (dest string) dest)
    (make-vector (dest size init) dest)
    (vector-ref (dest vec index) dest)
    (vector-set (vec index value) nil)
    (vector-length (dest vec) dest)
    (make-symbol (dest name) dest)
    (symbol-name (dest sym) dest)
    (intern (dest str) dest)
    (symbol-lit (dest name) dest)
    (keyword-name (dest kw) dest)
    (keyword-lit (dest name) dest)
    (exit (code) nil)
    (error (message) nil)))

(defun tac-use (instr)
  "Return list of vregs used by this instruction"
  (match tac-instr instr
    (lit (dest value) nil)
    (nil (dest) nil)
    (t (dest) nil)
    (move (dest src) (list src))
    (var (dest offset) nil)
    (setvar (offset src) (list src))
    (global (dest name) nil)
    (set-global (name src) (list src))
    (add (dest left right) (list left right))
    (sub (dest left right) (list left right))
    (mul (dest left right) (list left right))
    (div (dest left right) (list left right))
    (mod (dest left right) (list left right))
    (neg (dest value) (list value))
    (eq (dest left right) (list left right))
    (eql (dest left right) (list left right))
    (lt (dest left right) (list left right))
    (gt (dest left right) (list left right))
    (le (dest left right) (list left right))
    (ge (dest left right) (list left right))
    (zerop (dest value) (list value))
    (not (dest value) (list value))
    (band (dest left right) (list left right))
    (bor (dest left right) (list left right))
    (bxor (dest left right) (list left right))
    (bsh (dest value shift) (list value shift))
    (bnot (dest value) (list value))
    (label (name) nil)
    (goto (target) nil)
    (if (cond then-label) (list cond))
    (ifnot (cond else-label) (list cond))
    (return (value) (list value))
    (param (dest index) nil)
    (arg (index src) (list src))
    (call (dest name nargs) nil)  ; args passed via tac-arg
    (funcall (dest fn nargs) (list fn))
    (cons (dest car cdr) (list car cdr))
    (car (dest cell) (list cell))
    (cdr (dest cell) (list cell))
    (list (dest elems) elems)
    (null (dest value) (list value))
    (consp (dest value) (list value))
    (symbolp (dest value) (list value))
    (stringp (dest value) (list value))
    (numberp (dest value) (list value))
    (keywordp (dest value) (list value))
    (functionp (dest value) (list value))
    (string-length (dest str) (list str))
    (string-ref (dest str index) (list str index))
    (string-concat (dest left right) (list left right))
    (string-lit (dest string) nil)
    (make-vector (dest size init) (list size init))
    (vector-ref (dest vec index) (list vec index))
    (vector-set (vec index value) (list vec index value))
    (vector-length (dest vec) (list vec))
    (make-symbol (dest name) (list name))
    (symbol-name (dest sym) (list sym))
    (intern (dest str) (list str))
    (symbol-lit (dest name) nil)
    (keyword-name (dest kw) (list kw))
    (keyword-lit (dest name) nil)
    (exit (code) (list code))
    (error (message) (list message))))

;; Current count: 65 variants
