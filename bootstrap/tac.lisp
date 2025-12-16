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
  (:shadowing-import-from :habu.types :deftype :match)
  (:export ;; Literal value types
           :tac-literal :tac-literal-p
           :lit-fixnum :lit-fixnum-p :lit-fixnum-value
           :lit-raw :lit-raw-p :lit-raw-value
           ;; TAC instructions
           :tac-instr :tac-def :tac-use
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
           ;; List mutations
           :tac-setcar :tac-setcdr :tac-nthcdr :tac-length
           ;; Type tag operations
           :tac-get-tag :tac-set-tag
           ;; String mutations
           :tac-make-string :tac-make-string-from-vector :tac-string-equal :tac-string-set
           ;; Buffer operations
           :tac-buffer-byte-ref :tac-buffer-byte-set :tac-buffer-to-string
           ;; Symbol operations extended
           :tac-make-symbol-from-string
           ;; File I/O
           :tac-read-file :tac-write-file :tac-write-bytes :tac-println
           :tac-sys-read :tac-sys-read-byte :tac-sys-write :tac-sys-write-char
           :tac-sys-open :tac-sys-close
           ;; System/Low-level
           :tac-system :tac-mmap :tac-mmap-jit :tac-munmap
           :tac-pthread-jit-write-protect :tac-sys-dcache-flush :tac-sys-icache-invalidate
           :tac-funcall-ptr :tac-mem-set-byte :tac-mem-load-64 :tac-mem-load-byte
           ;; Heap/Runtime access
           :tac-get-intern-table :tac-set-intern-table
           :tac-get-keyword-table :tac-set-keyword-table
           :tac-get-lambda-counter :tac-set-lambda-counter
           :tac-get-symbol-counter :tac-set-symbol-counter
           :tac-get-symbol-table :tac-set-symbol-table
           :tac-get-symtab-offset :tac-get-symtab-count
           :tac-get-frame-pointer :tac-get-code-base
           :tac-set-global-vars :tac-get-global-vars
           :tac-get-cmdline-args
           ;; Control flow extended
           :tac-block-begin :tac-block-end :tac-return-from
           :tac-continue :tac-dolist-init :tac-dolist-next :tac-dotimes-init :tac-dotimes-next
           ;; Functions extended
           :tac-lambda :tac-lambda-ref :tac-tail-call
           ;; Multiple values
           :tac-values :tac-mvb
           ;; System
           :tac-exit :tac-error))

(in-package :habu.tac)

;;; === Literal Value Types ===
;;; Distinguishes Lisp values (tagged) from raw integers (untagged)
;;; This prevents the bug where literal 256 was loaded as-is instead of tagged 513

(deftype tac-literal :prefix lit
  (fixnum value)           ; Lisp fixnum - emit as (value << 1) | 1
  (raw value))             ; Raw integer - emit as-is (for internal use)

(deftype tac-instr :prefix tac
  ;; === Data Movement ===
  (lit dest literal)       ; dest := literal (tac-literal value)
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

  ;; === List Mutations ===
  (setcar cell value)
  (setcdr cell value)
  (nthcdr dest n list)
  (length dest list)

  ;; === Type Tag Operations ===
  (get-tag dest value)
  (set-tag dest value tag)

  ;; === String Mutations ===
  (make-string dest length init)
  (make-string-from-vector dest vec)
  (string-equal dest left right)
  (string-set str index value)

  ;; === Buffer Operations ===
  (buffer-byte-ref dest buf index)
  (buffer-byte-set buf index value)
  (buffer-to-string dest buf length)

  ;; === Symbol Operations Extended ===
  (make-symbol-from-string dest str)

  ;; === File I/O ===
  (read-file dest path)
  (write-file path content)
  (write-bytes fd bytes)
  (println value)
  (sys-read dest fd buf count)
  (sys-read-byte dest fd)
  (sys-write dest fd buf count)
  (sys-write-char fd char)
  (sys-open dest path flags mode)
  (sys-close dest fd)

  ;; === System/Low-level ===
  (system dest cmd)
  (mmap dest addr length prot flags fd offset)
  (mmap-jit dest length)
  (munmap dest addr length)
  (pthread-jit-write-protect enable)
  (sys-dcache-flush addr length)
  (sys-icache-invalidate addr length)
  (funcall-ptr dest ptr args)
  (mem-set-byte addr value)
  (mem-load-64 dest addr)
  (mem-load-byte dest addr)

  ;; === Heap/Runtime Access ===
  (get-intern-table dest)
  (set-intern-table value)
  (get-keyword-table dest)
  (set-keyword-table value)
  (get-lambda-counter dest)
  (set-lambda-counter value)
  (get-symbol-counter dest)
  (set-symbol-counter value)
  (get-symbol-table dest)
  (set-symbol-table value)
  (get-symtab-offset dest)
  (get-symtab-count dest)
  (get-frame-pointer dest)
  (get-code-base dest)
  (set-global-vars value)
  (get-global-vars dest)
  (get-cmdline-args dest)

  ;; === Control Flow Extended ===
  (block-begin id)           ; mark block start
  (block-end id)             ; mark block end
  (return-from id value)     ; return from named block
  (continue)                 ; continue to next iteration
  (dolist-init dest var-offset list)  ; init dolist iteration
  (dolist-next dest var-offset list end-label)  ; advance dolist
  (dotimes-init dest var-offset count) ; init dotimes iteration
  (dotimes-next dest var-offset count end-label) ; advance dotimes

  ;; === Functions Extended ===
  (lambda dest params body captures) ; create closure
  (lambda-ref dest name captures)    ; reference to lifted lambda
  (tail-call name args)              ; tail call optimization

  ;; === Multiple Values ===
  (values vals)              ; return multiple values
  (mvb vars expr body)       ; multiple-value-bind

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
    ;; List mutations
    (setcar (cell value) nil)
    (setcdr (cell value) nil)
    (nthcdr (dest n list) dest)
    (length (dest list) dest)
    ;; Type tags
    (get-tag (dest value) dest)
    (set-tag (dest value tag) dest)
    ;; String mutations
    (make-string (dest length init) dest)
    (make-string-from-vector (dest vec) dest)
    (string-equal (dest left right) dest)
    (string-set (str index value) nil)
    ;; Buffer ops
    (buffer-byte-ref (dest buf index) dest)
    (buffer-byte-set (buf index value) nil)
    (buffer-to-string (dest buf length) dest)
    ;; Symbol extended
    (make-symbol-from-string (dest str) dest)
    ;; File I/O
    (read-file (dest path) dest)
    (write-file (path content) nil)
    (write-bytes (fd bytes) nil)
    (println (value) nil)
    (sys-read (dest fd buf count) dest)
    (sys-read-byte (dest fd) dest)
    (sys-write (dest fd buf count) dest)
    (sys-write-char (fd char) nil)
    (sys-open (dest path flags mode) dest)
    (sys-close (dest fd) dest)
    ;; System/low-level
    (system (dest cmd) dest)
    (mmap (dest addr length prot flags fd offset) dest)
    (mmap-jit (dest length) dest)
    (munmap (dest addr length) dest)
    (pthread-jit-write-protect (enable) nil)
    (sys-dcache-flush (addr length) nil)
    (sys-icache-invalidate (addr length) nil)
    (funcall-ptr (dest ptr args) dest)
    (mem-set-byte (addr value) nil)
    (mem-load-64 (dest addr) dest)
    (mem-load-byte (dest addr) dest)
    ;; Heap access
    (get-intern-table (dest) dest)
    (set-intern-table (value) nil)
    (get-keyword-table (dest) dest)
    (set-keyword-table (value) nil)
    (get-lambda-counter (dest) dest)
    (set-lambda-counter (value) nil)
    (get-symbol-counter (dest) dest)
    (set-symbol-counter (value) nil)
    (get-symbol-table (dest) dest)
    (set-symbol-table (value) nil)
    (get-symtab-offset (dest) dest)
    (get-symtab-count (dest) dest)
    (get-frame-pointer (dest) dest)
    (get-code-base (dest) dest)
    (set-global-vars (value) nil)
    (get-global-vars (dest) dest)
    (get-cmdline-args (dest) dest)
    ;; Control flow extended
    (block-begin (id) nil)
    (block-end (id) nil)
    (return-from (id value) nil)
    (continue () nil)
    (dolist-init (dest var-offset list) dest)
    (dolist-next (dest var-offset list end-label) dest)
    (dotimes-init (dest var-offset count) dest)
    (dotimes-next (dest var-offset count end-label) dest)
    ;; Functions extended
    (lambda (dest params body captures) dest)
    (lambda-ref (dest name captures) dest)
    (tail-call (name args) nil)
    ;; Multiple values
    (values (vals) nil)
    (mvb (vars expr body) nil)
    ;; System
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
    ;; List mutations
    (setcar (cell value) (list cell value))
    (setcdr (cell value) (list cell value))
    (nthcdr (dest n list) (list n list))
    (length (dest list) (list list))
    ;; Type tags
    (get-tag (dest value) (list value))
    (set-tag (dest value tag) (list value tag))
    ;; String mutations
    (make-string (dest length init) (list length init))
    (make-string-from-vector (dest vec) (list vec))
    (string-equal (dest left right) (list left right))
    (string-set (str index value) (list str index value))
    ;; Buffer ops
    (buffer-byte-ref (dest buf index) (list buf index))
    (buffer-byte-set (buf index value) (list buf index value))
    (buffer-to-string (dest buf length) (list buf length))
    ;; Symbol extended
    (make-symbol-from-string (dest str) (list str))
    ;; File I/O
    (read-file (dest path) (list path))
    (write-file (path content) (list path content))
    (write-bytes (fd bytes) (list fd bytes))
    (println (value) (list value))
    (sys-read (dest fd buf count) (list fd buf count))
    (sys-read-byte (dest fd) (list fd))
    (sys-write (dest fd buf count) (list fd buf count))
    (sys-write-char (fd char) (list fd char))
    (sys-open (dest path flags mode) (list path flags mode))
    (sys-close (dest fd) (list fd))
    ;; System/low-level
    (system (dest cmd) (list cmd))
    (mmap (dest addr length prot flags fd offset) (list addr length prot flags fd offset))
    (mmap-jit (dest length) (list length))
    (munmap (dest addr length) (list addr length))
    (pthread-jit-write-protect (enable) (list enable))
    (sys-dcache-flush (addr length) (list addr length))
    (sys-icache-invalidate (addr length) (list addr length))
    (funcall-ptr (dest ptr args) (cons ptr args))
    (mem-set-byte (addr value) (list addr value))
    (mem-load-64 (dest addr) (list addr))
    (mem-load-byte (dest addr) (list addr))
    ;; Heap access
    (get-intern-table (dest) nil)
    (set-intern-table (value) (list value))
    (get-keyword-table (dest) nil)
    (set-keyword-table (value) (list value))
    (get-lambda-counter (dest) nil)
    (set-lambda-counter (value) (list value))
    (get-symbol-counter (dest) nil)
    (set-symbol-counter (value) (list value))
    (get-symbol-table (dest) nil)
    (set-symbol-table (value) (list value))
    (get-symtab-offset (dest) nil)
    (get-symtab-count (dest) nil)
    (get-frame-pointer (dest) nil)
    (get-code-base (dest) nil)
    (set-global-vars (value) (list value))
    (get-global-vars (dest) nil)
    (get-cmdline-args (dest) nil)
    ;; Control flow extended
    (block-begin (id) nil)
    (block-end (id) nil)
    (return-from (id value) (list value))
    (continue () nil)
    (dolist-init (dest var-offset list) (list list))
    (dolist-next (dest var-offset list end-label) (list list))
    (dotimes-init (dest var-offset count) (list count))
    (dotimes-next (dest var-offset count end-label) (list count))
    ;; Functions extended
    (lambda (dest params body captures) captures)
    (lambda-ref (dest name captures) captures)
    (tail-call (name args) args)
    ;; Multiple values
    (values (vals) vals)
    (mvb (vars expr body) nil)  ; expr/body are IR, not vregs
    ;; System
    (exit (code) (list code))
    (error (message) (list message))))

;; Total: ~130 variants (comprehensive for self-hosting)
