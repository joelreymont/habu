;;;; TAC - Three Address Code
;;;;
;;;; Linear instruction sequence with virtual registers.
;;;; Each instruction has at most 3 operands.
;;;; Virtual registers are integers starting from 0.

(defpackage :habu.tac
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:export :tac-instr :tac-def :tac-use
           :tac-lit :tac-nil :tac-t :tac-move :tac-var :tac-setvar
           :tac-global :tac-set-global
           :tac-add :tac-sub :tac-mul :tac-div :tac-mod :tac-neg
           :tac-eq :tac-eql :tac-lt :tac-gt :tac-le :tac-ge :tac-zerop
           :tac-not :tac-band :tac-bor :tac-bxor :tac-bsh :tac-bnot
           :tac-label :tac-goto :tac-if :tac-ifnot :tac-return
           :tac-param :tac-arg :tac-call :tac-funcall
           :tac-cons :tac-car :tac-cdr :tac-list
           :tac-null :tac-consp :tac-symbolp :tac-stringp :tac-numberp
           :tac-keywordp :tac-functionp
           :tac-string-length :tac-string-ref :tac-string-concat :tac-string-lit
           :tac-make-vector :tac-vector-ref :tac-vector-set :tac-vector-length
           :tac-make-symbol :tac-symbol-name :tac-intern :tac-symbol-lit
           :tac-keyword-name :tac-keyword-lit
           :tac-exit :tac-error))

(in-package :habu.tac)

(deftype tac-instr
  ;; === Data Movement ===
  (tac-lit dest value)         ; dest := literal
  (tac-nil dest)               ; dest := nil
  (tac-t dest)                 ; dest := t
  (tac-move dest src)          ; dest := src
  (tac-var dest offset)        ; dest := env[offset]
  (tac-setvar offset src)      ; env[offset] := src
  (tac-global dest name)       ; dest := global[name]
  (tac-set-global name src)    ; global[name] := src

  ;; === Arithmetic (dest := left op right) ===
  (tac-add dest left right)
  (tac-sub dest left right)
  (tac-mul dest left right)
  (tac-div dest left right)
  (tac-mod dest left right)
  (tac-neg dest value)

  ;; === Comparison (dest := left cmp right) ===
  (tac-eq dest left right)
  (tac-eql dest left right)
  (tac-lt dest left right)
  (tac-gt dest left right)
  (tac-le dest left right)
  (tac-ge dest left right)
  (tac-zerop dest value)

  ;; === Logical ===
  (tac-not dest value)

  ;; === Bitwise ===
  (tac-band dest left right)
  (tac-bor dest left right)
  (tac-bxor dest left right)
  (tac-bsh dest value shift)
  (tac-bnot dest value)

  ;; === Control Flow ===
  (tac-label name)
  (tac-goto target)
  (tac-if cond then-label)     ; if cond goto then-label
  (tac-ifnot cond else-label)  ; if not cond goto else-label
  (tac-return value)

  ;; === Function Calls ===
  (tac-param dest index)       ; dest := parameter[index]
  (tac-arg index src)          ; set argument[index] := src before call
  (tac-call dest name nargs)   ; dest := name(args...)
  (tac-funcall dest fn nargs)  ; dest := fn(args...) indirect

  ;; === List Operations ===
  (tac-cons dest car cdr)
  (tac-car dest cell)
  (tac-cdr dest cell)
  (tac-list dest elems)        ; elems is list of vregs

  ;; === Type Predicates ===
  (tac-null dest value)
  (tac-consp dest value)
  (tac-symbolp dest value)
  (tac-stringp dest value)
  (tac-numberp dest value)
  (tac-keywordp dest value)
  (tac-functionp dest value)

  ;; === String Operations ===
  (tac-string-length dest str)
  (tac-string-ref dest str index)
  (tac-string-concat dest left right)
  (tac-string-lit dest string) ; load string literal

  ;; === Vector Operations ===
  (tac-make-vector dest size init)
  (tac-vector-ref dest vec index)
  (tac-vector-set vec index value)
  (tac-vector-length dest vec)

  ;; === Symbol Operations ===
  (tac-make-symbol dest name)
  (tac-symbol-name dest sym)
  (tac-intern dest str)
  (tac-symbol-lit dest name)   ; load symbol literal

  ;; === Keyword Operations ===
  (tac-keyword-name dest kw)
  (tac-keyword-lit dest name)  ; load keyword literal

  ;; === System ===
  (tac-exit code)
  (tac-error message))

;; Virtual register operations for analysis

(defun tac-def (instr)
  "Return the vreg defined by this instruction, or nil"
  (match tac-instr instr
    (tac-lit (dest value) dest)
    (tac-nil (dest) dest)
    (tac-t (dest) dest)
    (tac-move (dest src) dest)
    (tac-var (dest offset) dest)
    (tac-setvar (offset src) nil)
    (tac-global (dest name) dest)
    (tac-set-global (name src) nil)
    (tac-add (dest left right) dest)
    (tac-sub (dest left right) dest)
    (tac-mul (dest left right) dest)
    (tac-div (dest left right) dest)
    (tac-mod (dest left right) dest)
    (tac-neg (dest value) dest)
    (tac-eq (dest left right) dest)
    (tac-eql (dest left right) dest)
    (tac-lt (dest left right) dest)
    (tac-gt (dest left right) dest)
    (tac-le (dest left right) dest)
    (tac-ge (dest left right) dest)
    (tac-zerop (dest value) dest)
    (tac-not (dest value) dest)
    (tac-band (dest left right) dest)
    (tac-bor (dest left right) dest)
    (tac-bxor (dest left right) dest)
    (tac-bsh (dest value shift) dest)
    (tac-bnot (dest value) dest)
    (tac-label (name) nil)
    (tac-goto (target) nil)
    (tac-if (cond then-label) nil)
    (tac-ifnot (cond else-label) nil)
    (tac-return (value) nil)
    (tac-param (dest index) dest)
    (tac-arg (index src) nil)
    (tac-call (dest name nargs) dest)
    (tac-funcall (dest fn nargs) dest)
    (tac-cons (dest car cdr) dest)
    (tac-car (dest cell) dest)
    (tac-cdr (dest cell) dest)
    (tac-list (dest elems) dest)
    (tac-null (dest value) dest)
    (tac-consp (dest value) dest)
    (tac-symbolp (dest value) dest)
    (tac-stringp (dest value) dest)
    (tac-numberp (dest value) dest)
    (tac-keywordp (dest value) dest)
    (tac-functionp (dest value) dest)
    (tac-string-length (dest str) dest)
    (tac-string-ref (dest str index) dest)
    (tac-string-concat (dest left right) dest)
    (tac-string-lit (dest string) dest)
    (tac-make-vector (dest size init) dest)
    (tac-vector-ref (dest vec index) dest)
    (tac-vector-set (vec index value) nil)
    (tac-vector-length (dest vec) dest)
    (tac-make-symbol (dest name) dest)
    (tac-symbol-name (dest sym) dest)
    (tac-intern (dest str) dest)
    (tac-symbol-lit (dest name) dest)
    (tac-keyword-name (dest kw) dest)
    (tac-keyword-lit (dest name) dest)
    (tac-exit (code) nil)
    (tac-error (message) nil)))

(defun tac-use (instr)
  "Return list of vregs used by this instruction"
  (match tac-instr instr
    (tac-lit (dest value) nil)
    (tac-nil (dest) nil)
    (tac-t (dest) nil)
    (tac-move (dest src) (list src))
    (tac-var (dest offset) nil)
    (tac-setvar (offset src) (list src))
    (tac-global (dest name) nil)
    (tac-set-global (name src) (list src))
    (tac-add (dest left right) (list left right))
    (tac-sub (dest left right) (list left right))
    (tac-mul (dest left right) (list left right))
    (tac-div (dest left right) (list left right))
    (tac-mod (dest left right) (list left right))
    (tac-neg (dest value) (list value))
    (tac-eq (dest left right) (list left right))
    (tac-eql (dest left right) (list left right))
    (tac-lt (dest left right) (list left right))
    (tac-gt (dest left right) (list left right))
    (tac-le (dest left right) (list left right))
    (tac-ge (dest left right) (list left right))
    (tac-zerop (dest value) (list value))
    (tac-not (dest value) (list value))
    (tac-band (dest left right) (list left right))
    (tac-bor (dest left right) (list left right))
    (tac-bxor (dest left right) (list left right))
    (tac-bsh (dest value shift) (list value shift))
    (tac-bnot (dest value) (list value))
    (tac-label (name) nil)
    (tac-goto (target) nil)
    (tac-if (cond then-label) (list cond))
    (tac-ifnot (cond else-label) (list cond))
    (tac-return (value) (list value))
    (tac-param (dest index) nil)
    (tac-arg (index src) (list src))
    (tac-call (dest name nargs) nil)  ; args passed via tac-arg
    (tac-funcall (dest fn nargs) (list fn))
    (tac-cons (dest car cdr) (list car cdr))
    (tac-car (dest cell) (list cell))
    (tac-cdr (dest cell) (list cell))
    (tac-list (dest elems) elems)
    (tac-null (dest value) (list value))
    (tac-consp (dest value) (list value))
    (tac-symbolp (dest value) (list value))
    (tac-stringp (dest value) (list value))
    (tac-numberp (dest value) (list value))
    (tac-keywordp (dest value) (list value))
    (tac-functionp (dest value) (list value))
    (tac-string-length (dest str) (list str))
    (tac-string-ref (dest str index) (list str index))
    (tac-string-concat (dest left right) (list left right))
    (tac-string-lit (dest string) nil)
    (tac-make-vector (dest size init) (list size init))
    (tac-vector-ref (dest vec index) (list vec index))
    (tac-vector-set (vec index value) (list vec index value))
    (tac-vector-length (dest vec) (list vec))
    (tac-make-symbol (dest name) (list name))
    (tac-symbol-name (dest sym) (list sym))
    (tac-intern (dest str) (list str))
    (tac-symbol-lit (dest name) nil)
    (tac-keyword-name (dest kw) (list kw))
    (tac-keyword-lit (dest name) nil)
    (tac-exit (code) (list code))
    (tac-error (message) (list message))))

;; Current count: 65 variants
