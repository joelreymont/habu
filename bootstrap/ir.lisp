;;;; IR - Intermediate Representation Types
;;;;
;;;; Complete IR ADT for the Habu compiler.
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
           :ir-block :ir-return-from :ir-loop :ir-continue
           :ir-dolist :ir-dotimes
           ;; Functions
           :ir-call :ir-lambda :ir-funcall :ir-lambda-ref :ir-tail-call
           ;; List operations
           :ir-cons :ir-car :ir-cdr :ir-list :ir-length
           :ir-setcar :ir-setcdr :ir-nthcdr
           ;; Type predicates
           :ir-null :ir-consp :ir-symbolp :ir-stringp :ir-numberp
           :ir-keywordp :ir-functionp
           :ir-get-tag :ir-set-tag
           ;; String operations
           :ir-string-length :ir-string-ref :ir-string-concat
           :ir-make-string :ir-make-string-from-vector
           :ir-string-equal :ir-string-set
           ;; Vector operations
           :ir-make-vector :ir-vector-ref :ir-vector-set :ir-vector-length
           :ir-buffer-byte-ref :ir-buffer-byte-set :ir-buffer-to-string
           ;; Symbol operations
           :ir-make-symbol :ir-make-symbol-from-string
           :ir-symbol-name :ir-intern
           ;; Keyword operations
           :ir-keyword-name
           ;; File I/O
           :ir-read-file :ir-write-file :ir-write-bytes :ir-println
           :ir-sys-read :ir-sys-read-byte :ir-sys-write :ir-sys-write-char
           :ir-sys-open :ir-sys-close
           ;; System/Low-level
           :ir-exit :ir-error :ir-system
           :ir-mmap :ir-mmap-jit :ir-munmap
           :ir-pthread-jit-write-protect :ir-sys-dcache-flush :ir-sys-icache-invalidate
           :ir-funcall-ptr :ir-mem-set-byte :ir-mem-load-64 :ir-mem-load-byte
           ;; Heap/Runtime access
           :ir-get-intern-table :ir-set-intern-table
           :ir-get-keyword-table :ir-set-keyword-table
           :ir-get-lambda-counter :ir-set-lambda-counter
           :ir-get-symbol-counter :ir-set-symbol-counter
           :ir-get-symbol-table :ir-set-symbol-table
           :ir-get-symtab-offset :ir-get-symtab-count
           :ir-get-frame-pointer :ir-get-code-base
           :ir-set-global-vars :ir-get-global-vars
           :ir-get-cmdline-args
           ;; Multiple values
           :ir-values :ir-mvb))

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
  (let bindings body count offsets)  ; bindings with metadata
  (block id body)          ; named block for return-from
  (return-from id value)   ; non-local return
  (loop body)              ; infinite loop (use return to exit)
  (continue)               ; continue to next iteration
  (dolist var-offset list-ir body end-label) ; iterate over list
  (dotimes var-offset count-ir body end-label) ; iterate n times

  ;; === Functions ===
  (call name args)         ; named function call, args is a list
  (lambda params body captures offsets) ; lambda with capture list
  (lambda-ref name captures) ; reference to lifted lambda
  (funcall fn args)        ; indirect call through closure/function
  (tail-call name args)    ; tail call optimization

  ;; === List Operations ===
  (cons car cdr)
  (car cell)
  (cdr cell)
  (list elems)             ; list constructor
  (length list)            ; list length
  (setcar cell value)      ; destructive car update
  (setcdr cell value)      ; destructive cdr update
  (nthcdr n list)          ; skip n cdrs

  ;; === Type Predicates ===
  (null value)
  (consp value)
  (symbolp value)
  (stringp value)
  (numberp value)
  (keywordp value)
  (functionp value)
  (get-tag value)          ; extract type tag
  (set-tag value tag)      ; set type tag

  ;; === String Operations ===
  (string-length str)
  (string-ref str index)
  (string-concat left right)
  (make-string length init)
  (make-string-from-vector vec)
  (string-equal left right)
  (string-set str index value)

  ;; === Vector Operations ===
  (make-vector size init)
  (vector-ref vec index)
  (vector-set vec index value)
  (vector-length vec)
  (buffer-byte-ref buf index)
  (buffer-byte-set buf index value)
  (buffer-to-string buf length)

  ;; === Symbol Operations ===
  (make-symbol name)
  (make-symbol-from-string str)
  (symbol-name sym)
  (intern str)

  ;; === Keyword Operations ===
  (keyword-name kw)

  ;; === File I/O ===
  (read-file path)
  (write-file path content)
  (write-bytes fd bytes)
  (println value)
  (sys-read fd buf count)
  (sys-read-byte fd)
  (sys-write fd buf count)
  (sys-write-char fd char)
  (sys-open path flags mode)
  (sys-close fd)

  ;; === System/Low-level ===
  (exit code)
  (error message)
  (system cmd)
  (mmap addr length prot flags fd offset)
  (mmap-jit length)
  (munmap addr length)
  (pthread-jit-write-protect enable)
  (sys-dcache-flush addr length)
  (sys-icache-invalidate addr length)
  (funcall-ptr ptr args)   ; call function at raw pointer
  (mem-set-byte addr value)
  (mem-load-64 addr)
  (mem-load-byte addr)

  ;; === Heap/Runtime Access ===
  (get-intern-table)
  (set-intern-table value)
  (get-keyword-table)
  (set-keyword-table value)
  (get-lambda-counter)
  (set-lambda-counter value)
  (get-symbol-counter)
  (set-symbol-counter value)
  (get-symbol-table)
  (set-symbol-table value)
  (get-symtab-offset)
  (get-symtab-count)
  (get-frame-pointer)
  (get-code-base)
  (set-global-vars value)
  (get-global-vars)
  (get-cmdline-args)

  ;; === Multiple Values ===
  (values vals)            ; return multiple values
  (mvb vars expr body))    ; multiple-value-bind

;; Total: ~105 variants (comprehensive for self-hosting)
