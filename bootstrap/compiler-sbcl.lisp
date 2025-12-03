;;; ============================================================
;;; Habu Native Compiler - Self-Hosting ARM64 Lisp Compiler
;;; ============================================================
;;;
;;; Components: ARM64 asm, utils, reader, codegen, IR compiler
;;;
;;; Package structure:
;;; - SYS: Internal compiler functions (ARM64 encoders, IR, codegen)
;;; - HABU: Public compiler API (deliver, compile-program, etc.)
;;;
;;; Internal functions use clean names within the SYS package

;;; ============================================================
;;; Part 0: Package Definitions
;;; ============================================================

;;; SYS: Internal compiler implementation
;;; ARM64 encoders, IR operations, codegen - all internal
(defpackage :sys
  (:use :cl)
  (:shadow #:read #:compile)  ; Shadow CL versions to define our own
  (:export
   ;; Reader and compiler (shadowed from CL)
   #:read #:compile
   ;; String primitives (for runtime)
   #:string-length #:string-ref #:make-string-from-vector
   #:string-concat #:number-to-string
   ;; Vector primitives
   #:make-vector #:vector-set))

;;; HABU: Public compiler API
;;; Use habu:deliver, habu:compile-program, etc.
(defpackage :habu
  (:use :cl :sys)
  (:shadowing-import-from :sys #:read #:compile)  ; Use SYS versions, not CL
  (:export
   ;; Public compiler API (clean names)
   #:read-all           ; Parse source string to forms
   #:compile-program    ; Compile forms to ARM64 bytecode
   #:deliver            ; Compile source to native executable
   #:deliver-file       ; Compile file to native executable
   ;; Disassembler
   #:disassemble-form
   #:disassemble-bytecode
   ;; Optimizer
   #:optimize-ir
   ;; Internal functions (for tests)
   #:eval-ir #:eval-forms #:codegen #:codegen-main
   #:eval-ir-with-fns #:compile-forms
   ;; Re-export system primitives for convenience
   #:string-length #:string-ref #:make-string-from-vector
   #:make-vector #:vector-set
   #:string-concat #:number-to-string))

(in-package :sys)

;;; Forward declarations for functions defined in other files
;;; These are loaded via ASDF after this file
(declaim (ftype (function (t t t t &optional t) t) write-macho-executable-with-imports-and-heap))
(declaim (ftype (function (t &key (:passes t)) t) habu:optimize-ir))
(declaim (ftype (function (t t t) t) wrap-bytecode-with-heap-for-imports))
(declaim (ftype (function () t) fn-epilogue))
(declaim (ftype (function (t) t) find-free-vars-simple))

;;; System primitives (SBCL compatibility shims)
;;; In self-hosted Habu, these are native runtime functions
(defun string-length (s) (cl:length s))
(defun string-ref (s i) (char-code (char s i)))
(defun make-vector (n) (make-array n))
(defun vector-set (v i x) (setf (aref v i) x))
(defun make-string-from-vector (v)
  (map 'string #'code-char v))

;; String concatenation - for replacing format nil patterns
(defun string-concat (&rest strings)
  (apply #'concatenate 'string strings))

;; Number to string conversion - for replacing format nil patterns
(defun number-to-string (n)
  (write-to-string n))

(in-package :habu)

;;; HABU package shims for file I/O (needed by deliver-file and link-fasls)
(defun native-read-file (path)
  "Read entire file into string"
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (when stream
      (let ((content (make-string (file-length stream))))
        (cl:read-sequence content stream)
        content))))

(defun collect-imports (code-bytes)
  "Collect import stubs from bytecode. Returns list of imported function names."
  (declare (ignore code-bytes))
  '("_exit"))

;;; Forward declarations for functions defined in macho.lisp (loaded later via ASDF)
;;; The actual implementations are in macho.lisp; declaims here for forward references

;;; Register :habu as a feature for #+habu / #-habu conditionals
;;; This works in SBCL during bootstrap; native reader has its own feature-present?
(pushnew :habu *features*)

;;; ============================================================
;;; Part 0a: Function Linking State
;;; ============================================================

;; Global state for function call fixups during codegen
;; *codegen-pos* tracks current byte position in output
;; *call-fixups* accumulates (byte-pos . fn-name) pairs for BL patching
(defparameter *codegen-pos* 0)
(defparameter *call-fixups* nil)

;; Symbol table for native executables (no runtime symbol interning)
;; Each unique symbol name gets a unique integer ID
;; Symbols are represented as (ID << 4) | 2 (tag 2 = symbol)
(defparameter *symbol-table* nil)
(defparameter *symbol-counter* 1)  ; Start at 1, 0 reserved for nil

;;; Forward declarations for functions used before defined
(declaim (ftype (function (list) list) append-all))
(declaim (ftype (function (integer) integer) temp-slot))
(declaim (ftype (function (list) integer) code-size))

(defun intern-symbol (name)
  "Get or create a symbol ID for NAME. Returns tagged symbol value."
  (let ((entry (assoc name *symbol-table* :test #'equal)))
    (if entry
        (cdr entry)
        (let ((id *symbol-counter*))
          (push (cons name id) *symbol-table*)
          (incf *symbol-counter*)
          id))))

;; reset-symbol-table defined in codegen.lisp

;;; ============================================================
;;; Part 1: ARM64 Instruction Encoders
;;; ============================================================

;; Most encoders now use arm64: package
;; cbz/cbnz wrappers defined in codegen.lisp

;;; Position tracking helpers for function linking
(defun emit-with-pos (code)
  "Emit code and update position counter. Returns the code."
  (let ((len (length code)))
    (incf *codegen-pos* len)
    code))

(defun record-call-fixup (fn-name)
  "Record that a BL instruction at current position needs fixup for fn-name."
  (push (cons *codegen-pos* fn-name) *call-fixups*))

(defun patch-bl-at (code pos rel-offset)
  "Patch a BL instruction at byte position pos with rel-offset."
  (let* ((off-s (ash rel-offset -2))
         (off-m (logand off-s #x3FFFFFF))
         (word (logior #x94000000 off-m))
         (b0 (logand word #xFF))
         (b1 (logand (ash word -8) #xFF))
         (b2 (logand (ash word -16) #xFF))
         (b3 (logand (ash word -24) #xFF)))
    (setf (nth pos code) b0)
    (setf (nth (+ pos 1) code) b1)
    (setf (nth (+ pos 2) code) b2)
    (setf (nth (+ pos 3) code) b3)
    code))

(defun apply-fixups (code fnoffs)
  "Apply all recorded call fixups to code."
  (dolist (fixup *call-fixups*)
    (let* ((bl-pos (car fixup))
           (fn-name (cdr fixup))
           (fn-entry (assoc fn-name fnoffs)))
      (when fn-entry
        (let* ((fn-pos (cdr fn-entry))
               (rel-offset (- fn-pos bl-pos)))
          (patch-bl-at code bl-pos rel-offset)))))
  code)

(defun resolve-calls (code fnoffs)
  "Resolve call and loop markers to branch instructions.
   Handles: (:call-fn name), (:tail-call-fn name), (:loop-start), (:loop-continue)
   Note: (:extern-call name) markers are left as-is for later resolution."
  (labels ((calc-size (item)
             ;; Calculate byte size of an item
             (cond ((and (consp item) (eq (car item) :call-fn)) 4)
                   ((and (consp item) (eq (car item) :tail-call-fn)) 4)
                   ((and (consp item) (eq (car item) :extern-call)) 4)
                   ((and (consp item) (eq (car item) :loop-start)) 0) ; marker only, no code
                   ((and (consp item) (eq (car item) :loop-continue)) 4) ; B instruction
                   (t 1)))
           (find-loop-start (items pos)
             ;; Find position of most recent :loop-start marker
             (labels ((scan (items pos last-start)
                        (if (null items)
                            last-start
                            (let ((item (car items)))
                              (cond
                                ((and (consp item) (eq (car item) :loop-start))
                                 (scan (cdr items) pos pos))
                                ((and (consp item) (eq (car item) :loop-continue))
                                 last-start) ; stop at continue
                                (t
                                 (scan (cdr items) (+ pos (calc-size item)) last-start)))))))
               (scan items pos nil)))
           (resolve-at (items pos acc loop-start-stack)
             ;; Iterate through items, tracking position, resolving markers
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Loop start - record position on stack, emit nothing
                     ((and (consp item) (eq (car item) :loop-start))
                      (resolve-at (cdr items) pos acc (cons pos loop-start-stack)))
                     ;; Loop continue - emit backward branch to loop start
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let* ((loop-start (car loop-start-stack))
                             (rel-offset (- loop-start pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-start-stack)))
                     ;; Internal call - resolve to BL
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-entry (assoc fn-name fnoffs))
                             (fn-pos (if fn-entry (cdr fn-entry) 0))
                             (rel-offset (- fn-pos pos))
                             (bl-bytes (arm64:bl (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse bl-bytes) acc)
                                    loop-start-stack)))
                     ;; Tail call - resolve to B (unconditional branch without link)
                     ((and (consp item) (eq (car item) :tail-call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-entry (assoc fn-name fnoffs))
                             (fn-pos (if fn-entry (cdr fn-entry) 0))
                             (rel-offset (- fn-pos pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-start-stack)))
                     ;; External call - emit 4 placeholder bytes + marker
                     ;; CRITICAL: Must emit 4 bytes to maintain position consistency
                     ;; Marker goes FIRST (at position), then 3 zero bytes follow
                     ((and (consp item) (eq (car item) :extern-call))
                      (resolve-at (cdr items)
                                  (+ pos 4)
                                  (list* 0 0 0 (list :extern-call (cadr item) pos) acc)
                                  loop-start-stack))
                     ;; Regular byte
                     (t
                      (resolve-at (cdr items)
                                  (+ pos 1)
                                  (cons item acc)
                                  loop-start-stack)))))))
    (resolve-at code 0 nil nil)))

(defun collect-extern-calls (code)
  "Collect all extern call markers from code.
   Returns list of (name . position) pairs."
  (let ((calls nil))
    (dolist (item code)
      (when (and (consp item) (eq (car item) :extern-call))
        (push (cons (cadr item) (caddr item)) calls)))
    (nreverse calls)))

(defun get-unique-imports (extern-calls)
  "Get unique import names from extern calls list."
  (let ((names nil))
    (dolist (call extern-calls)
      (let ((name (car call)))
        (unless (member name names :test #'equal)
          (push name names))))
    (nreverse names)))

(defun flatten-extern-calls (code &optional stub-map code-base-addr)
  "Replace extern call markers with BL instructions.
   If STUB-MAP and CODE-BASE-ADDR are provided, emits correct BL instructions.
   Otherwise emits placeholder BL instructions (for post-processing).
   Note: Markers are now followed by 4 zero bytes (BL instruction placeholder).
   Returns (cons flattened-code extern-call-positions)
   where extern-call-positions is ((name . byte-pos) ...)"
  (let ((result nil)
        (positions nil)
        (skip 0))  ; Number of items to skip
    (dolist (item code)
      (cond
        ;; Skip placeholder zeros after extern-call marker
        ((> skip 0)
         (decf skip))
        ;; Extern call marker - emit BL, skip next 4 placeholder zeros
        ((and (consp item) (eq (car item) :extern-call))
         (let ((name (cadr item))
               (pos (caddr item)))
           (push (cons name pos) positions)
           (if (and stub-map code-base-addr)
               ;; Emit correct BL instruction
               (let* ((bl-addr (+ code-base-addr pos))
                      (stub-addr (gethash name stub-map))
                      (rel-offset (- stub-addr bl-addr))
                      (off-s (ash rel-offset -2))
                      (off-m (logand off-s #x3FFFFFF))
                      (bl-instr (logior #x94000000 off-m)))
                 ;; Emit BL in little-endian
                 (push (logand bl-instr #xFF) result)
                 (push (logand (ash bl-instr -8) #xFF) result)
                 (push (logand (ash bl-instr -16) #xFF) result)
                 (push (logand (ash bl-instr -24) #xFF) result))
               ;; Emit placeholder BL (will be patched later)
               (progn
                 (push 0 result)
                 (push 0 result)
                 (push 0 result)
                 (push #x94 result)))  ; BL opcode high byte
           ;; Skip the 4 placeholder zeros that follow the marker
           (setf skip 4)))
        ;; Regular byte
        (t
         (push item result))))
    (cons (nreverse result) (nreverse positions))))

(defun load-addr (rd addr)
  "Load a 64-bit address into register using MOVZ + MOVK sequence."
  (let* ((lo16 (logand addr #xFFFF))
         (sh16 (ash addr -16))
         (hi16 (logand sh16 #xFFFF))
         (sh32 (ash addr -32))
         (hi32 (logand sh32 #xFFFF))
         (sh48 (ash addr -48))
         (hi48 (logand sh48 #xFFFF))
         (base (arm64:movz rd lo16))
         (p1 (if (> hi16 0) (arm64:movk rd hi16 :lsl 16) nil))
         (r1 (append base p1))
         (p2 (if (> hi32 0) (arm64:movk rd hi32 :lsl 32) nil))
         (r2 (append r1 p2))
         (p3 (if (> hi48 0) (arm64:movk rd hi48 :lsl 48) nil)))
    (append r2 p3)))

(defun load-addr-32 (rd addr)
  "Load a 32-bit address into register rd using exactly 8 bytes (MOVZ + MOVK).
   This is used for function offsets to ensure consistent code size during
   the two-pass compilation where fnoffs may be nil in the first pass."
  (let* ((lo16 (logand addr #xFFFF))
         (hi16 (logand (ash addr -16) #xFFFF)))
    (append (arm64:movz rd lo16)
            (arm64:movk rd hi16 :lsl 16))))

;; Condition code helpers (map to arm64:+*+ constants)
(defun cond-eq () arm64:+eq+)
(defun cond-ne () arm64:+ne+)
(defun cond-lt () arm64:+lt+)
(defun cond-le () arm64:+le+)
(defun cond-gt () arm64:+gt+)
(defun cond-ge () arm64:+ge+)

(defun string-to-char-codes (str)
  "Convert string to list of character codes"
  (labels ((iter (i acc)
             (if (>= i (length str))
                 (reverse acc)
                 (iter (+ i 1) (cons (char-code (char str i)) acc)))))
    (iter 0 nil)))

(defun codegen-string-from-chars (chars td)
  "Generate code to build a string from character codes.
   Returns code that leaves the string in x0."
  (let* ((len (length chars))
         (tagged-len (ash len 4))  ; Tag length as fixnum
         (vec-slot (temp-slot td))
         ;; Allocate vector: movz x0, tagged-len; ldr x11, [x19, #56]; blr x11
         ;; Runtime table index 7 = make_vector at offset 56
         (alloc (append-all
                 (list (if (< tagged-len #x10000)
                           (arm64:movz 0 tagged-len)
                           (load-addr 0 tagged-len))
                       (arm64:ldr 11 19 :offset 56)
                       (arm64:blr 11)
                       (arm64:str 0 31 :offset vec-slot)))))
    ;; Store each character: ldr x0, [sp, vec-slot]; movz x1, tagged-idx; movz x2, tagged-ch; ldr x11, [x19, #64]; blr x11
    ;; Runtime table index 8 = vector_set at offset 64
    (labels ((store-chars (chs idx acc)
               (if (null chs)
                   acc
                   (let* ((ch (car chs))
                          (tagged-idx (ash idx 4))    ; Tag index as fixnum
                          (tagged-ch (ash ch 4))      ; Tag character as fixnum
                          (store-code (append-all
                                       (list (arm64:ldr 0 31 :offset vec-slot)
                                             (if (< tagged-idx #x10000)
                                                 (arm64:movz 1 tagged-idx)
                                                 (load-addr 1 tagged-idx))
                                             (if (< tagged-ch #x10000)
                                                 (arm64:movz 2 tagged-ch)
                                                 (load-addr 2 tagged-ch))
                                             (arm64:ldr 11 19 :offset 64)
                                             (arm64:blr 11)))))
                     (store-chars (cdr chs) (+ idx 1) (append acc store-code))))))
      (let* ((stores (store-chars chars 0 nil))
             ;; Make string from vector: ldr x0, [sp, vec-slot]; ldr x9, [x19, #80]; blr x9
             ;; Runtime table index 10 = make_string_from_vector at offset 80
             (make-str (append-all
                        (list (arm64:ldr 0 31 :offset vec-slot)
                              (arm64:ldr 9 19 :offset 80)
                              (arm64:blr 9)))))
        (append-all (list alloc stores make-str))))))

(defun codegen-string-inline (chars)
  "Generate code to build a string inline on the heap using x28 bump pointer.
   String layout: [length (8 bytes)][char data (n bytes)]
   Returns code that leaves tagged string pointer in x0.
   All allocations are 16-byte aligned for 4-bit tagging scheme."
  (let* ((len (length chars))
         ;; Round up allocation to 16-byte alignment: (8 + len + 15) & ~15
         (alloc-size (logand (+ 8 len 15) (lognot 15))))
    (labels ((store-chars (chs idx acc)
               (if (null chs)
                   acc
                   (let* ((ch (car chs))
                          ;; Store char at x28 + 8 + idx
                          (offset (+ 8 idx))
                          (code (append-all
                                 (list (arm64:movz 1 ch)
                                       (arm64:strb 1 28 offset)))))
                     (store-chars (cdr chs) (+ idx 1) (append acc code))))))
      (let ((store-code (store-chars chars 0 nil)))
        (append-all
         (list
          ;; Store length at [x28+0]
          (arm64:movz 1 len)
          (arm64:str 1 28 :offset 0)
          ;; Store each char
          store-code
          ;; Return tagged pointer, bump heap
          (arm64:mov 0 28)                   ; x0 = current heap ptr
          (arm64:movz 1 alloc-size)
          (arm64:add 28 28 1)                ; x28 += alloc size
          ;; Tag with string tag (0x4)
          (arm64:movz 1 4)
          (arm64:orr 0 0 1)))))))

;;; ============================================================
;;; Part 2: Utility Functions (util-*)
;;; ============================================================

(defun has-tag (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun env-lookup (sym env)
  (if (null env)
      nil
      (if (eq (caar env) sym)
          (cdar env)
          (env-lookup sym (cdr env)))))

(defun env-extend (bindings env)
  ;; Use let* to sequence operations - avoid nested recursive calls in args
  (labels ((max-off (e acc)
             (if (null e) acc
                 (let ((o (cdar e)))
                   (max-off (cdr e) (if (> o acc) o acc)))))
           (add-bs (bs off acc)
             (if (null bs) acc
                 (let ((entry (cons (caar bs) off)))
                   (add-bs (cdr bs) (+ off 1) (cons entry acc))))))
    (let* ((mx (if env (max-off env -1) -1))
           (bs-result (add-bs bindings (+ mx 1) nil))
           (rev-result (reverse bs-result)))
      (append rev-result env))))

(defun count-instrs (code)
  (if (null code) 0 (ash (length code) -2)))

;; Append two lists - bind first, then append
(defun append2 (a b)
  (let ((ar a))
    (append ar b)))

;; Append list of lists using fold - avoiding nested calls
(defun append-all (lists)
  (labels ((iter (ls acc)
             (if (null ls) acc
                 (let* ((hd (car ls))
                        (tl (cdr ls))
                        (na (append acc hd)))
                   (iter tl na)))))
    (iter lists nil)))

;;; ============================================================
;;; Keyword Argument Support
;;; ============================================================

(defun parse-lambda-list (params)
  "Parse lambda list, splitting at &key.
   Returns (positional-params . keyword-specs) where keyword-specs is
   a list of (name default) pairs."
  (let ((positional nil)
        (keywords nil)
        (in-keys nil))
    (dolist (p params)
      (cond
        ((eq p '&key) (setq in-keys t))
        (in-keys
         ;; Keyword param: either SYMBOL or (SYMBOL DEFAULT)
         (if (consp p)
             (push (list (car p) (cadr p)) keywords)
             (push (list p nil) keywords)))
        (t (push p positional))))
    (cons (nreverse positional) (nreverse keywords))))

;; Note: cl:keywordp already checks if symbol is in keyword package

(defun keyword-to-param-name (kw)
  "Convert :FOO keyword to FOO param name string.
   In CL, (symbol-name :foo) already returns \"FOO\" without colon."
  (symbol-name kw))

(defun find-keyword-position (kw-name keyword-specs)
  "Find position of keyword with given name in keyword-specs list.
   kw-name is the param name (e.g., \"IMM\"), keyword-specs is ((name default) ...)"
  (let ((pos 0))
    (dolist (spec keyword-specs)
      (when (string-equal kw-name (symbol-name (car spec)))
        (return-from find-keyword-position pos))
      (incf pos))
    nil))

(defun rewrite-keyword-call (args n-positional keyword-specs)
  "Rewrite call args with keyword arguments to fully positional args.
   Returns list of args in positional order, with defaults for unspecified keywords."
  (let* ((n-keywords (length keyword-specs))
         (positional-args (subseq args 0 (min n-positional (length args))))
         (keyword-values (make-array n-keywords :initial-element nil))
         (rest-args (if (> (length args) n-positional)
                        (subseq args n-positional)
                        nil)))
    ;; Parse keyword/value pairs from rest-args
    (loop while rest-args do
      (let ((kw (car rest-args))
            (val (cadr rest-args)))
        (if (keywordp kw)
            (let* ((kw-name (keyword-to-param-name kw))
                   (pos (find-keyword-position kw-name keyword-specs)))
              (when pos
                (setf (aref keyword-values pos) val))
              (setq rest-args (cddr rest-args)))
            ;; Not a keyword - shouldn't happen in well-formed call
            (setq rest-args (cdr rest-args)))))
    ;; Fill in defaults for unspecified keywords
    (loop for i from 0 below n-keywords
          for spec in keyword-specs
          when (null (aref keyword-values i))
            do (setf (aref keyword-values i) (cadr spec)))
    ;; Return combined positional list
    (append positional-args (coerce keyword-values 'list))))

(defun call-has-keywords-p (args)
  "Check if call arguments contain keyword arguments (symbols starting with :)"
  (some #'keywordp args))

;;; ============================================================
;;; Part 3: Reader (read-*)
;;; ============================================================

(defun whitespace-p (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
(defun digit-p (ch) (and (>= ch #x30) (<= ch #x39)))
(defun hex-digit-p (ch) (or (digit-p ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
(defun alpha-p (ch) (or (and (>= ch #x41) (<= ch #x5A)) (and (>= ch #x61) (<= ch #x7A))))
(defun symbol-char-p (ch)
  (or (alpha-p ch) (digit-p ch) (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E) (= ch #x21) (= ch #x3F)
      (= ch #x26) (= ch #x25) (= ch #x3A) (= ch #x2E)))  ; #x2E = dot for symbols like arm64:b.lo

(defun char-at (s pos)
  (if (< pos (string-length s)) (string-ref s pos) 0))

(defun digit-val (ch) (- ch #x30))
(defun hex-val (ch)
  (cond ((digit-p ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) 10))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) 10))
        (t 0)))

(defun skip-line (s pos)
  (let ((ch (char-at s pos)))
    (if (or (= ch #x0A) (= ch 0)) (+ pos 1) (skip-line s (+ pos 1)))))

(defun skip-ws (s pos)
  (let ((ch (char-at s pos)))
    (cond ((whitespace-p ch) (skip-ws s (+ pos 1)))
          ((= ch #x3B) (skip-ws s (skip-line s (+ pos 1))))
          (t pos))))

(defun read-digits (s pos n)
  (let ((ch (char-at s pos)))
    (if (digit-p ch)
        (read-digits s (+ pos 1) (+ (* n 10) (digit-val ch)))
        (cons n pos))))

(defun read-int (s pos)
  (let ((neg nil) (start pos))
    (let ((ch (char-at s pos)))
      (cond ((= ch #x2D) (setq neg t) (setq start (+ pos 1)))
            ((= ch #x2B) (setq start (+ pos 1)))))
    (let* ((r (read-digits s start 0))
           (val (car r))
           (end (cdr r)))
      (cons (if neg (- 0 val) val) end))))

(defun read-hex-digits (s pos n)
  (let ((ch (char-at s pos)))
    (if (hex-digit-p ch)
        (read-hex-digits s (+ pos 1) (+ (* n 16) (hex-val ch)))
        (cons n pos))))

(defun read-hex (s pos)
  (read-hex-digits s pos 0))

(defun chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (dotimes (i len)
      (vector-set vec i (nth i chars)))
    (make-string-from-vector vec)))

(defun read-sym-chars (s pos chars)
  (let ((ch (char-at s pos)))
    (if (symbol-char-p ch)
        (read-sym-chars s (+ pos 1) (cons ch chars))
        (cons chars pos))))

(defun read-sym (s pos)
  (let* ((r (read-sym-chars s pos nil))
         (chars (car r))
         (end (cdr r))
         (name (chars-to-string (reverse chars)))
         (uname (string-upcase name)))
    (cons (cond ((string= uname "NIL") nil)
                ((string= uname "T") t)
                ;; Keywords start with ':' - intern into KEYWORD package
                ((and (> (length uname) 1) (char= (char uname 0) #\:))
                 (intern (subseq uname 1) "KEYWORD"))
                ;; Package-qualified symbols like ARM64:ADD
                ((position #\: uname)
                 (let* ((colon-pos (position #\: uname))
                        (pkg-name (subseq uname 0 colon-pos))
                        (sym-name (subseq uname (1+ colon-pos)))
                        (pkg (find-package pkg-name)))
                   ;; If package exists use it, otherwise intern full name in HABU
                   (if pkg
                       (intern sym-name pkg)
                       (intern uname (find-package :habu)))))
                ;; Regular symbols - intern into HABU package
                (t (intern uname (find-package :habu))))
          end)))

(defun read-str-chars (s pos chars)
  (let ((ch (char-at s pos)))
    (cond
      ((= ch #x22) (cons chars (+ pos 1)))
      ((= ch #x5C)
       (let* ((esc (char-at s (+ pos 1)))
              (ec (cond ((= esc #x6E) #x0A) ((= esc #x74) #x09) ((= esc #x72) #x0D) (t esc))))
         (read-str-chars s (+ pos 2) (cons ec chars))))
      ((= ch 0) (cons chars pos))
      (t (read-str-chars s (+ pos 1) (cons ch chars))))))

(defun read-str (s pos)
  (let* ((r (read-str-chars s (+ pos 1) nil))
         (chars (car r))
         (end (cdr r)))
    (cons (chars-to-string (reverse chars)) end)))

(defun sys:read (source pos)
  (labels
      ((read-list-elems (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (cond
             ((= ch #x29) (cons nil (+ p2 1)))
             ;; Dot - check if standalone (dotted pair) or part of symbol (like b.lo)
             ((= ch #x2E)
              (let ((next-ch (char-at source (+ p2 1))))
                ;; Only treat as dotted pair if followed by whitespace, ), or EOF
                (if (or (whitespace-p next-ch)
                        (= next-ch #x29)  ; )
                        (= next-ch 0))    ; EOF
                    ;; Standalone dot - dotted pair marker
                    (let* ((r (read-one (+ p2 1)))
                           (cdr-val (car r))
                           (p3 (cdr r))
                           (p4 (skip-ws source p3)))
                      (cons cdr-val (+ p4 1)))
                    ;; Dot followed by non-delimiter - part of symbol (like b.lo)
                    (let* ((er (read-one p2))
                           (el (car er))
                           (p3 (cdr er))
                           (rr (read-list-elems p3)))
                      (cons (cons el (car rr)) (cdr rr))))))
             ((= ch 0) (cons nil p2))
             (t (let* ((er (read-one p2))
                       (el (car er))
                       (p3 (cdr er)))
                  ;; Skip reader-skip markers from #+/- conditionals
                  (if (and (consp el) (eq (car el) :reader-skip))
                      ;; Skip this element, continue with rest
                      (read-list-elems p3)
                      ;; Normal element, include in list
                      (let ((rr (read-list-elems p3)))
                        (cons (cons el (car rr)) (cdr rr)))))))))
       (read-list (p) (read-list-elems (+ p 1)))
       ;; Feature check for building native code:
       ;; :habu is always present, :sbcl is ABSENT (target is native, not SBCL)
       ;; This ensures #-sbcl forms ARE included (native code) and #+sbcl are skipped
       (feature-present-p (feature-name)
         (let ((uname (string-upcase feature-name)))
           (string= uname "HABU")))
       (read-sharp (p)
         (let ((ch (char-at source (+ p 1))))
           (cond
             ;; #x or #X - hexadecimal number
             ((or (= ch #x78) (= ch #x58)) (read-hex source (+ p 2)))
             ;; #' - function quote
             ((= ch #x27)
              (let ((r (read-one (+ p 2))))
                (cons (list 'function (car r)) (cdr r))))
             ;; #\ - character literal
             ((= ch #x5C)
              (let ((ch2 (char-at source (+ p 2))))
                (if (alpha-p (char-at source (+ p 3)))
                    (let* ((r (read-sym-chars source (+ p 2) nil))
                           (nm (chars-to-string (reverse (car r)))))
                      (cons (cond ((string= nm "newline") #x0A) ((string= nm "space") #x20)
                                  ((string= nm "tab") #x09) (t ch2))
                            (cdr r)))
                    (cons ch2 (+ p 3)))))
             ;; #+ - read form only if feature is present
             ((= ch #x2B)  ; '+'
              (let* ((feat-result (read-sym source (+ p 2)))
                     (feature-sym (car feat-result))
                     (after-feat (cdr feat-result))
                     (feature-name (if (symbolp feature-sym) (symbol-name feature-sym) "")))
                (if (feature-present-p feature-name)
                    ;; Feature present: read and return the form
                    (read-one after-feat)
                    ;; Feature absent: skip the form, return marker
                    (let ((skipped (read-one after-feat)))
                      ;; Return (:reader-skip . t) as marker, read-all will filter
                      (cons (cons :reader-skip t) (cdr skipped))))))
             ;; #- - read form only if feature is absent
             ((= ch #x2D)  ; '-'
              ;; Check if this is a negative number: #-123 vs #-sbcl
              (let ((ch2 (char-at source (+ p 2))))
                (if (digit-p ch2)
                    ;; It's a negative number like #-123 (unusual but handle it)
                    (cons nil (+ p 2))
                    ;; It's a feature conditional
                    (let* ((feat-result (read-sym source (+ p 2)))
                           (feature-sym (car feat-result))
                           (after-feat (cdr feat-result))
                           (feature-name (if (symbolp feature-sym) (symbol-name feature-sym) "")))
                      (if (feature-present-p feature-name)
                          ;; Feature present: skip the form, return marker
                          (let ((skipped (read-one after-feat)))
                            ;; Return (:reader-skip . t) as marker, read-all will filter
                            (cons (cons :reader-skip t) (cdr skipped)))
                          ;; Feature absent: read and return the form
                          (read-one after-feat))))))
             (t (cons nil (+ p 2))))))
       (read-one (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (if (>= p2 (string-length source))
               (cons nil p2)
               (cond
                 ((= ch #x22) (read-str source p2))
                 ((= ch #x28) (read-list p2))
                 ((= ch #x27)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quote (car r)) (cdr r))))
                 ((= ch #x60)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quasiquote (car r)) (cdr r))))
                 ((= ch #x2C)
                  (if (= (char-at source (+ p2 1)) #x40)
                      (let ((r (read-one (+ p2 2))))
                        (cons (list 'unquote-splicing (car r)) (cdr r)))
                      (let ((r (read-one (+ p2 1))))
                        (cons (list 'unquote (car r)) (cdr r)))))
                 ((= ch #x23) (read-sharp p2))
                 ((or (digit-p ch)
                      (and (or (= ch #x2D) (= ch #x2B))
                           (digit-p (char-at source (+ p2 1)))))
                  (read-int source p2))
                 ((symbol-char-p ch) (read-sym source p2))
                 ((= ch #x29) (cons nil (+ p2 1)))
                 (t (read-one (+ p2 1))))))))
    (read-one pos)))

(defun parse-string (source)
  "Parse a single form from SOURCE string. Returns the parsed form."
  (car (sys:read source 0)))

(defun reader-skip-marker-p (form)
  "Check if form is a reader skip marker from #+/- conditionals"
  (and (consp form)
       (eq (car form) :reader-skip)))

(defun read-all (source)
  (let ((len (string-length source)))
    (labels ((ra (pos acc)
               (let ((p2 (skip-ws source pos)))
                 (if (>= p2 len)
                     (reverse acc)
                     (let* ((r (sys:read source p2))
                            (form (car r)))
                       ;; Skip reader conditional markers
                       (if (reader-skip-marker-p form)
                           (ra (cdr r) acc)
                           (ra (cdr r) (cons form acc))))))))
      (ra 0 nil))))

;;; ============================================================
;;; Part 4: Stack Frame Constants (inlined for delivery)
;;; ============================================================

;; Constants inlined directly to avoid global variable initialization issues
;; Frame size: #xFF0, Env base: #x180, Temp base: #x40
;; Temp guard: #x180, Spill base: #x200

(defun frame-size () #xFF0)
(defun env-base () #x180)
(defun temp-base () #x40)
(defun temp-guard () #x180)
(defun spill-base (td)
  "Calculate spill area base for temp depth td.
   Each nesting level gets 64 bytes (8 slots) of spill area."
  (+ #x240 (* td 64)))

;; Stack frame layout for user functions (512 bytes):
;;   [sp, #0-#15]:   Save area (x20, lr)
;;   [sp, #64-#191]: Temp slots (16 slots)
;;   [sp, #192-#319]: Arg spill area (16 args max)
;;   [sp, #320-#511]: Env variables (x20 = sp+320)
;; Note: Env grows downward from x20, so vars are at [x20-0], [x20-8], etc.

;;; Register-based temporary allocation
;;; Use registers x5-x15 (11 registers) for temporaries before spilling to stack
;;; x5-x15 are caller-saved, so they're clobbered by function calls
;;; When a temp may be live across a call, we must still use stack slots

(defparameter *temp-registers* '(5 6 7 8 9 10 11 12 13 14 15))
(defparameter *num-temp-registers* 11)

(defun temp-register (depth)
  "Return register number for temp depth, or nil if must spill to stack."
  (if (< depth *num-temp-registers*)
      (nth depth *temp-registers*)
      nil))

(defun temp-slot (depth)
  "Return stack offset for temp depth. Used when registers exhausted or across calls."
  (let ((off (+ #x40 (* depth 8))))  ; #x40 = temp base (64)
    (if (>= off #x2000)               ; #x2000 = temp guard (8192), allows 1016 slots
        (error "Too many temp slots: ~A" depth)
        off)))

(defun save-temp (depth)
  "Generate code to save x0 to temp location (register or stack)."
  (let ((reg (temp-register depth)))
    (if reg
        (arm64:mov reg 0)            ; MOV xN, x0
        (arm64:str 0 31 :offset (temp-slot depth)))))  ; STR x0, [sp, #off]

(defun load-temp (dest-reg depth)
  "Generate code to load temp location to dest-reg."
  (let ((reg (temp-register depth)))
    (if reg
        (if (= dest-reg reg)
            nil                        ; Already in correct register
            (arm64:mov dest-reg reg)) ; MOV dest, xN
        (arm64:ldr dest-reg 31 :offset (temp-slot depth)))))

(defun spill-slot (td idx)
  ;; Spill slots are depth-aware to handle nested function calls
  ;; Each call level gets 8 spill slots (8 args max per call)
  ;; td=0: slots 0-7 at #x240-#x278
  ;; td=1: slots 0-7 at #x280-#x2B8
  ;; etc.
  (let* ((slots-per-level 8)
         (base #x240)
         (off (+ (* td slots-per-level 8) (* idx 8))))
    (+ base off)))

;;; ============================================================
;;; Part 5: Prologue/Epilogue
;;; ============================================================

(defun prologue ()
  ;; Main entry prologue - x0 has runtime table pointer from C caller
  ;; Use 4KB frame to support deep nesting in large programs
  (append
   (arm64:sub 31 31 1 :imm t :shift12 t)   ; SUB sp, sp, #1, LSL #12 = #4096
   (arm64:stp 29 30 31 :offset 0)  ; STP x29, x30, [sp, #0]
   (arm64:stp 19 20 31 :offset 16) ; STP x19, x20, [sp, #16]
   (arm64:stp 21 22 31 :offset 32) ; STP x21, x22, [sp, #32]
   (arm64:stp 23 24 31 :offset 48) ; STP x23, x24, [sp, #48]
   (arm64:mov 19 0)           ; MOV x19, x0 (save runtime table)
   (arm64:add 20 31 #x180 :imm t)))  ; ADD x20, sp, #384 (env-base)

(defun epilogue ()
  (append
   (arm64:ldp 23 24 31 :offset 48) ; LDP x23, x24, [sp, #48]
   (arm64:ldp 21 22 31 :offset 32) ; LDP x21, x22, [sp, #32]
   (arm64:ldp 19 20 31 :offset 16) ; LDP x19, x20, [sp, #16]
   (arm64:ldp 29 30 31 :offset 0)  ; LDP x29, x30, [sp, #0]
   (arm64:add 31 31 1 :imm t :shift12 t)    ; ADD sp, sp, #1, LSL #12 = #4096
   (arm64:ret)))

;;; ============================================================
;;; Part 5b: Free Variable Analysis
;;; ============================================================

(defun find-free-vars (expr bound env)
  "Find variables referenced in expr that are in env but not in bound"
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((symbolp e)
                ;; Check if it's a variable reference (in env but not bound)
                (if (and (env-lookup e env)
                         (not (member e bnd)))
                    (if (member e acc) acc (cons e acc))
                    acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)  ; Don't look inside quotes
               ((eq (car e) 'lambda)
                ;; Lambda binds its params - add to bound
                (let ((params (cadr e))
                      (body (caddr e)))
                  (collect body (append params bnd) acc)))
               ((eq (car e) 'LET)
                ;; Let binds variables
                (let* ((bindings (cadr e))
                       (body (caddr e))
                       (names (mapcar #'car bindings))
                       (vals (mapcar #'cadr bindings))
                       ;; Collect from values first
                       (acc2 (collect-list vals bnd acc))
                       ;; Then body with new bindings
                       (new-bnd (append names bnd)))
                  (collect body new-bnd acc2)))
               ((eq (car e) 'LET*)
                (let* ((bindings (cadr e))
                       (body (caddr e)))
                  (labels ((do-bindings (bs bnd acc)
                             (if (null bs)
                                 (collect body bnd acc)
                                 (let* ((b (car bs))
                                        (nm (car b))
                                        (vl (cadr b))
                                        (acc2 (collect vl bnd acc)))
                                   (do-bindings (cdr bs) (cons nm bnd) acc2)))))
                    (do-bindings bindings bnd acc))))
               (t
                ;; General case: collect from all subexpressions
                (collect-list e bnd acc))))
           (collect-list (lst bnd acc)
             (if (null lst)
                 acc
                 (collect-list (cdr lst) bnd (collect (car lst) bnd acc)))))
    (collect expr bound nil)))

;;; ============================================================
;;; Part 5c: Mutable Closure Boxing
;;; ============================================================

(defun find-setq-targets (expr bound)
  "Find all variables that are targets of setq in expr, respecting bindings.
   Returns list of variable names that are setq'd and in scope via bound."
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)  ; Don't look inside quotes
               ((eq (car e) 'setq)
                ;; (setq var val) - collect var if it's in bound
                (let ((var (cadr e))
                      (val (caddr e)))
                  (if (member var bnd)
                      (collect val bnd (if (member var acc) acc (cons var acc)))
                      (collect val bnd acc))))
               ((eq (car e) 'lambda)
                ;; Lambda binds its params
                (let ((params (cadr e))
                      (body (caddr e)))
                  (collect body (append params bnd) acc)))
               ((or (eq (car e) 'LET) (eq (car e) 'let))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e)))
                       (names (mapcar #'car bindings))
                       (vals (mapcar #'cadr bindings))
                       (acc2 (collect-list vals bnd acc))
                       (new-bnd (append names bnd)))
                  (collect body new-bnd acc2)))
               ((or (eq (car e) 'LET*) (eq (car e) 'let*))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e))))
                  (labels ((do-bindings (bs bnd acc)
                             (if (null bs)
                                 (collect body bnd acc)
                                 (let* ((b (car bs))
                                        (nm (car b))
                                        (vl (cadr b))
                                        (acc2 (collect vl bnd acc)))
                                   (do-bindings (cdr bs) (cons nm bnd) acc2)))))
                    (do-bindings bindings bnd acc))))
               (t (collect-list e bnd acc))))
           (collect-list (lst bnd acc)
             (if (null lst) acc
                 (collect-list (cdr lst) bnd (collect (car lst) bnd acc)))))
    (collect expr bound nil)))

(defun find-captured-vars (expr bound)
  "Find all variables that are captured by lambdas (free in lambda bodies).
   Returns list of variable names that appear free in any inner lambda."
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)
               ((eq (car e) 'lambda)
                ;; Find free vars in this lambda
                (let* ((params (cadr e))
                       (body (caddr e))
                       (new-bnd (append params bnd))
                       ;; Collect from lambda body for nested lambdas
                       (acc2 (collect body new-bnd acc)))
                  ;; Add vars free in this lambda
                  (labels ((add-free (vars acc)
                             (if (null vars) acc
                                 (let ((v (car vars)))
                                   (add-free (cdr vars)
                                             (if (and (member v bnd)
                                                      (not (member v acc)))
                                                 (cons v acc) acc))))))
                    (add-free (find-free-vars-simple body params) acc2))))
               ((or (eq (car e) 'LET) (eq (car e) 'let))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e)))
                       (names (mapcar #'car bindings))
                       (vals (mapcar #'cadr bindings))
                       (acc2 (collect-list vals bnd acc))
                       (new-bnd (append names bnd)))
                  (collect body new-bnd acc2)))
               ((or (eq (car e) 'LET*) (eq (car e) 'let*))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e))))
                  (labels ((do-bindings (bs bnd acc)
                             (if (null bs)
                                 (collect body bnd acc)
                                 (let* ((b (car bs))
                                        (nm (car b))
                                        (vl (cadr b))
                                        (acc2 (collect vl bnd acc)))
                                   (do-bindings (cdr bs) (cons nm bnd) acc2)))))
                    (do-bindings bindings bnd acc))))
               (t (collect-list e bnd acc))))
           (collect-list (lst bnd acc)
             (if (null lst) acc
                 (collect-list (cdr lst) bnd (collect (car lst) bnd acc)))))
    (collect expr bound nil)))

(defun find-free-vars-simple (expr bound)
  "Simple free variable finder - just symbols in expr not in bound."
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((symbolp e)
                (if (and (not (member e bnd))
                         (not (member e acc))
                         (not (eq e t))
                         (not (eq e nil)))
                    (cons e acc) acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)
               ((eq (car e) 'lambda)
                (let ((params (cadr e)) (body (caddr e)))
                  (collect body (append params bnd) acc)))
               ((or (eq (car e) 'LET) (eq (car e) 'let))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e)))
                       (names (mapcar #'car bindings))
                       (vals (mapcar #'cadr bindings))
                       (acc2 (collect-list vals bnd acc)))
                  (collect body (append names bnd) acc2)))
               ((or (eq (car e) 'LET*) (eq (car e) 'let*))
                (let* ((bindings (cadr e))
                       (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e))))
                  (labels ((do-bs (bs bnd acc)
                             (if (null bs) (collect body bnd acc)
                                 (let* ((b (car bs)) (nm (car b)) (vl (cadr b)))
                                   (do-bs (cdr bs) (cons nm bnd) (collect vl bnd acc))))))
                    (do-bs bindings bnd acc))))
               (t (collect-list e bnd acc))))
           (collect-list (lst bnd acc)
             (if (null lst) acc
                 (collect-list (cdr lst) bnd (collect (car lst) bnd acc)))))
    (collect expr bound nil)))

(defun box-mutable-captures (expr)
  "Transform expr to box variables that are both captured and mutated.
   - Wraps mutable captured vars in (cons val nil) at binding site
   - Transforms reads of boxed vars to (car var)
   - Transforms (setq var val) to (setcar var val)"
  (labels ((transform (e boxed)
             ;; boxed = list of currently boxed variable names
             (cond
               ((null e) e)
               ((symbolp e)
                ;; If this var is boxed, transform to (car var)
                (if (member e boxed) (list 'car e) e))
               ((not (consp e)) e)
               ((eq (car e) 'quote) e)
               ((eq (car e) 'setq)
                (let ((var (cadr e))
                      (val (caddr e)))
                  (if (member var boxed)
                      ;; Transform to (setcar var val)
                      (list 'setcar var (transform val boxed))
                      (list 'setq var (transform val boxed)))))
               ((eq (car e) 'lambda)
                (let* ((params (cadr e))
                       (body-forms (cddr e))
                       (body (if (cdr body-forms) (cons 'progn body-forms) (car body-forms)))
                       ;; Don't transform params, they shadow boxed vars
                       (new-boxed (remove-if (lambda (v) (member v params)) boxed))
                       (transformed (transform body new-boxed)))
                  ;; Unwrap single-form progn
                  (if (and (consp transformed) (eq (car transformed) 'progn))
                      (cons 'lambda (cons params (cdr transformed)))
                      (list 'lambda params transformed))))
               ((or (eq (car e) 'LET) (eq (car e) 'let))
                (transform-let e boxed))
               ((or (eq (car e) 'LET*) (eq (car e) 'let*))
                (transform-let* e boxed))
               (t (mapcar (lambda (x) (transform x boxed)) e))))

           (transform-let (e boxed)
             (let* ((bindings (cadr e))
                    (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e)))
                    (names (mapcar #'car bindings))
                    ;; Find which new bindings need to be boxed
                    (setq-targets (find-setq-targets body names))
                    (captured (find-captured-vars body names))
                    (to-box (intersection setq-targets captured))
                    ;; Transform binding values and box if needed
                    (new-bindings
                     (mapcar (lambda (b)
                               (let ((nm (car b))
                                     (vl (transform (cadr b) boxed)))
                                 (if (member nm to-box)
                                     (list nm (list 'cons vl nil))
                                     (list nm vl))))
                             bindings))
                    ;; Add new boxed vars to the set
                    (new-boxed (append to-box (remove-if (lambda (v) (member v names)) boxed))))
               (list 'let new-bindings (transform body new-boxed))))

           (transform-let* (e boxed)
             (let* ((bindings (cadr e))
                    (body (if (cdddr e) (cons 'progn (cddr e)) (caddr e)))
                    (names (mapcar #'car bindings))
                    ;; Find which new bindings need to be boxed
                    (setq-targets (find-setq-targets body names))
                    (captured (find-captured-vars body names))
                    (to-box (intersection setq-targets captured)))
               (labels ((do-bindings (bs current-boxed acc-bindings)
                          (if (null bs)
                              (list 'let* (reverse acc-bindings)
                                    (transform body current-boxed))
                              (let* ((b (car bs))
                                     (nm (car b))
                                     (vl (transform (cadr b) current-boxed))
                                     (is-boxed (member nm to-box))
                                     (new-val (if is-boxed (list 'cons vl nil) vl))
                                     (new-binding (list nm new-val))
                                     (new-boxed (if is-boxed
                                                    (cons nm current-boxed)
                                                    (remove nm current-boxed))))
                                (do-bindings (cdr bs) new-boxed
                                             (cons new-binding acc-bindings))))))
                 (do-bindings bindings boxed nil)))))
    (transform expr nil)))

;;; ============================================================
;;; Part 6: IR Compiler (compile-*)
;;; ============================================================

(defun rewrite-labels-calls (expr fn-names)
  "Rewrite calls to functions in fn-names to use funcall instead"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; If calling a labels function, rewrite as funcall
         ((and (symbolp op) (member op fn-names))
          (cons 'funcall (cons op (mapcar (lambda (e) (rewrite-labels-calls e fn-names)) (cdr expr)))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; LABELS/FLET - don't descend (BUG #20 FIX: let sys:compile handle nested labels)
         ((or (eq op 'LABELS) (eq op 'FLET) (eq op 'labels) (eq op 'flet)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-calls (caddr expr) fn-names)))
         ;; let/let* - rewrite values and body, not binding names
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (rewrite-labels-calls (cadr b) fn-names))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (rewrite-labels-calls e fn-names)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (rewrite-labels-calls e fn-names)) expr)))))
    (t expr)))

(defun rewrite-labels-body (expr fn-names fntab-var)
  "Inside labels body: rewrite function calls.
   All calls to labels fns: (fn args) -> (funcall fn FNTAB args)"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; If calling a labels function, rewrite to pass FNTAB
         ((and (symbolp op) (member op fn-names))
          (cons 'funcall (cons op (cons fntab-var
                          (mapcar (lambda (e) (rewrite-labels-body e fn-names fntab-var)) (cdr expr))))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; LABELS/FLET - don't descend (BUG #20 FIX: let sys:compile handle nested labels)
         ((or (eq op 'LABELS) (eq op 'FLET) (eq op 'labels) (eq op 'flet)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-body (caddr expr) fn-names fntab-var)))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (rewrite-labels-body (cadr b) fn-names fntab-var))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (rewrite-labels-body e fn-names fntab-var)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (rewrite-labels-body e fn-names fntab-var)) expr)))))
    (t expr)))

(defun rewrite-labels-main (expr fn-names)
  "In main body: rewrite (fn args) -> (funcall fn fn args)"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; If calling a labels function, rewrite as (funcall fn fn args)
         ((and (symbolp op) (member op fn-names))
          (cons 'funcall (cons op (cons op
                          (mapcar (lambda (e) (rewrite-labels-main e fn-names)) (cdr expr))))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; LABELS/FLET - don't descend (BUG #20 FIX: let sys:compile handle nested labels)
         ((or (eq op 'LABELS) (eq op 'FLET) (eq op 'labels) (eq op 'flet)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-main (caddr expr) fn-names)))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (rewrite-labels-main (cadr b) fn-names))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (rewrite-labels-main e fn-names)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (rewrite-labels-main e fn-names)) expr)))))
    (t expr)))

(defun quote-ir (obj)
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'nil-ir))  ;; Use nil-ir for proper nil, not (lit 0)
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (quote-ir (car obj)) (quote-ir (cdr obj))))
    (t (list 'lit 0))))

(defun sys:compile (expr env fenv)
  (cond
    ((numberp expr) (list 'lit expr))
    ((stringp expr) (list 'str-lit expr))
    ((symbolp expr)
     ;; Handle special symbols first
     (cond
       ;; t compiles to non-zero literal for native executables without runtime
       ;; In boolean context, any non-zero value is truthy
       ((eq expr 't) (list 'lit 1))           ; t = 1 (truthy)
       ((eq expr 'nil) (list 'nil-ir))          ; nil is 0x06 (tag 6)
       (t
        ;; Use numberp since offset 0 is falsey in Habu
        (let ((off (env-lookup expr env)))
          (if (numberp off)
              (list 'var off)
              ;; Check if it's a known function name - return as lambda-ref
              ;; This creates a closure pointing to the function (no captures)
              (if (and fenv (assoc expr fenv))
                  (list 'lambda-ref expr nil)
                  (list 'lit 0)))))))
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ((eq op '+)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (+ (car args) (cadr args)))
                            ;; Also fold if compiled results are both literals
                            (let ((left-ir (sys:compile (car args) env fenv))
                                  (right-ir (sys:compile (cadr args) env fenv)))
                              (if (and (has-tag left-ir 'lit) (has-tag right-ir 'lit))
                                  (list 'lit (+ (cadr left-ir) (cadr right-ir)))
                                  (list 'add left-ir right-ir))))
                        (sys:compile (cons '+ (cons (list '+ (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '-)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args))
                    ;; Constant folding for unary minus
                    (if (numberp (car args))
                        (list 'lit (- (car args)))
                        (let ((arg-ir (sys:compile (car args) env fenv)))
                          (if (has-tag arg-ir 'lit)
                              (list 'lit (- (cadr arg-ir)))
                              (list 'sub (list 'lit 0) arg-ir))))
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (- (car args) (cadr args)))
                            (let ((left-ir (sys:compile (car args) env fenv))
                                  (right-ir (sys:compile (cadr args) env fenv)))
                              (if (and (has-tag left-ir 'lit) (has-tag right-ir 'lit))
                                  (list 'lit (- (cadr left-ir) (cadr right-ir)))
                                  (list 'sub left-ir right-ir))))
                        (sys:compile (cons '- (cons (list '- (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '*)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 1)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (* (car args) (cadr args)))
                            (let ((left-ir (sys:compile (car args) env fenv))
                                  (right-ir (sys:compile (cadr args) env fenv)))
                              (if (and (has-tag left-ir 'lit) (has-tag right-ir 'lit))
                                  (list 'lit (* (cadr left-ir) (cadr right-ir)))
                                  (list 'mul left-ir right-ir))))
                        (sys:compile (cons '* (cons (list '* (car args) (cadr args)) (cddr args))) env fenv))))))
         ;; division with constant folding
         ((eq op '/)
          (if (and (numberp (cadr expr)) (numberp (caddr expr)) (not (zerop (caddr expr))))
              (list 'lit (truncate (cadr expr) (caddr expr)))
              (list 'div (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv))))
         ;; modulo
         ((eq op 'mod)
          (list 'mod-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'rem)
          (list 'mod-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; 1+ and 1-
         ((eq op '1+)
          (list 'add (sys:compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op '1-)
          (list 'sub (sys:compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op 'logand)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit -1)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'band (sys:compile (car args) env fenv) (sys:compile (cadr args) env fenv))
                        (sys:compile (list 'logand (list 'logand (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'logior)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bor (sys:compile (car args) env fenv) (sys:compile (cadr args) env fenv))
                        (sys:compile (list 'logior (list 'logior (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'logxor)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bxor (sys:compile (car args) env fenv) (sys:compile (cadr args) env fenv))
                        (sys:compile (list 'logxor (list 'logxor (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'ash)
          (list 'bsh (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '=)
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'eq)
          ;; eq is pointer equality - same as = for our tagged values
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '<)
          (list 'cmp-lt (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '>)
          (list 'cmp-gt (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '<=)
          (list 'cmp-le (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '>=)
          (list 'cmp-ge (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'if)
          (list 'if-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)
                (if (cdddr expr) (sys:compile (cadddr expr) env fenv) (list 'nil-ir))))
         ;; cond - multi-branch conditional
         ((eq op 'cond)
          (let ((clauses (cdr expr)))
            (if (null clauses)
                (list 'nil-ir)
                (let* ((clause (car clauses))
                       (test (car clause))
                       (body (cdr clause)))
                  (if (eq test 't)
                      ;; t clause - always execute
                      (if (null body)
                          (list 'lit 1)
                          (if (null (cdr body))
                              (sys:compile (car body) env fenv)
                              (sys:compile (cons 'progn body) env fenv)))
                      (list 'if-ir
                            (sys:compile test env fenv)
                            (if (null body)
                                (sys:compile test env fenv)
                                (if (null (cdr body))
                                    (sys:compile (car body) env fenv)
                                    (sys:compile (cons 'progn body) env fenv)))
                            (sys:compile (cons 'cond (cdr clauses)) env fenv)))))))
         ;; when - if with implicit progn (no else branch)
         ((eq op 'when)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (sys:compile test env fenv)
                  (if (null (cdr body))
                      (sys:compile (car body) env fenv)
                      (sys:compile (cons 'progn body) env fenv))
                  (list 'nil-ir))))
         ;; unless - negated when
         ((eq op 'unless)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (sys:compile test env fenv)
                  (list 'nil-ir)
                  (if (null (cdr body))
                      (sys:compile (car body) env fenv)
                      (sys:compile (cons 'progn body) env fenv)))))
         ;; while - iterative loop
         ((eq op 'while)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'while-ir
                  (sys:compile test env fenv)
                  (if (null (cdr body))
                      (sys:compile (car body) env fenv)
                      (sys:compile (cons 'progn body) env fenv)))))
         ;; dotimes - counted iteration
         ((eq op 'dotimes)
          ;; (dotimes (var count [result]) body...)
          (let* ((spec (cadr expr))
                 (var (car spec))
                 (count-form (cadr spec))
                 (result-form (if (cddr spec) (caddr spec) 0))
                 (body (cddr expr))
                 ;; Compile body with extended env that includes loop var
                 (new-env (env-extend (list (list var)) env))
                 (body-ir (if (null (cdr body))
                              (sys:compile (car body) new-env fenv)
                              (sys:compile (cons 'progn body) new-env fenv)))
                 (result-ir (sys:compile result-form new-env fenv)))
            ;; Create a dotimes-ir node with compiled body
            (list 'dotimes-ir
                  var
                  (sys:compile count-form env fenv)
                  body-ir     ; Compiled body IR
                  result-ir   ; Compiled result IR
                  env)))      ; Original env for var offset calculation
         ;; dolist - list iteration
         ((eq op 'dolist)
          ;; (dolist (var list [result]) body...)
          (let* ((spec (cadr expr))
                 (var (car spec))
                 (list-form (cadr spec))
                 (result-form (if (cddr spec) (caddr spec) nil))
                 (body (cddr expr))
                 ;; Compile body with extended env that includes loop var
                 (new-env (env-extend (list (list var)) env))
                 (body-ir (if (null (cdr body))
                              (sys:compile (car body) new-env fenv)
                              (sys:compile (cons 'progn body) new-env fenv)))
                 (result-ir (if result-form
                                (sys:compile result-form new-env fenv)
                                (list 'nil-ir))))
            ;; Create a dolist-ir node with compiled body
            (list 'dolist-ir
                  var
                  (sys:compile list-form env fenv)
                  body-ir     ; Compiled body IR
                  result-ir   ; Compiled result IR
                  env)))
         ((eq op 'LET)  ; Changed to uppercase
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr)))
            (labels ((proc (bs eacc vals names)
                       (if (null bs)
                           (list eacc (reverse vals) (reverse names))
                           (let* ((b (car bs))
                                  (nm (if (consp b) (car b) b))
                                  (vl (if (consp b) (cadr b) 0))
                                  (vi (sys:compile vl env fenv))
                                  (ne (env-extend (list (list nm)) eacc)))
                             (proc (cdr bs) ne (cons vi vals) (cons nm names)))))
                     ;; Avoid mapcar - use labels recursion instead
                     (get-offs (ns e acc)
                       (if (null ns)
                           (reverse acc)
                           (get-offs (cdr ns) e (cons (env-lookup (car ns) e) acc)))))
              (let* ((r (proc bindings env nil nil))
                     (nenv (car r))
                     (vals (cadr r))
                     (names (caddr r))
                     (offs (get-offs names nenv nil))
                     ;; Wrap multiple body forms in progn
                     (body (if (null (cdr body-forms))
                               (car body-forms)
                               (cons 'progn body-forms)))
                     (bir (sys:compile body nenv fenv)))
                (list 'let-ir vals bir (length bindings) offs)))))
         ((eq op 'LET*)  ; Changed to uppercase
          (let* ((bs (cadr expr))
                 (body-forms (cddr expr))
                 (body (if (null (cdr body-forms))
                           (car body-forms)
                           (cons 'progn body-forms))))
            (if (null bs)
                (sys:compile body env fenv)
                (sys:compile (list 'LET (list (car bs)) (cons 'LET* (cons (cdr bs) body-forms))) env fenv))))
         ((eq op 'quote) (quote-ir (cadr expr)))
         ;; function - return a reference to the named function (for funcall)
         ((eq op 'function)
          (let ((fn-name (cadr expr)))
            ;; Create a lambda-ref pointing to the function (no captures)
            (list 'lambda-ref fn-name nil)))
         ;; lambda - anonymous function (closure)
         ((eq op 'lambda)
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 ;; Find free variables (referenced but not in params)
                 (free-vars (find-free-vars body params env))
                 ;; Get the offsets for each free var in current env
                 (free-offsets (mapcar (lambda (v) (env-lookup v env)) free-vars))
                 ;; Build environment for body: params + free vars
                 ;; Free vars come first (as captured in closure env), then params
                 (param-bindings (mapcar #'list params))
                 (body-env (env-extend param-bindings
                              (env-extend (mapcar #'list free-vars) nil)))
                 ;; Compile body to IR
                 (body-ir (sys:compile body body-env fenv)))
            (list 'lambda-ir params body-ir free-vars free-offsets)))
         ((eq op 'cons)
          (list 'cons-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'car) (list 'car-ir (sys:compile (cadr expr) env fenv)))
         ((eq op 'cdr) (list 'cdr-ir (sys:compile (cadr expr) env fenv)))
         ;; caar, cdar - missing accessors that were causing crashes
         ((eq op 'caar) (sys:compile `(car (car ,(cadr expr))) env fenv))
         ((eq op 'cdar) (sys:compile `(cdr (car ,(cadr expr))) env fenv))
         ;; cadr, caddr, cadddr, cddr, cdddr - common accessor chains
         ((eq op 'cadr) (sys:compile `(car (cdr ,(cadr expr))) env fenv))
         ((eq op 'caddr) (sys:compile `(car (cdr (cdr ,(cadr expr)))) env fenv))
         ((eq op 'cadddr) (sys:compile `(car (cdr (cdr (cdr ,(cadr expr))))) env fenv))
         ((eq op 'cddr) (sys:compile `(cdr (cdr ,(cadr expr))) env fenv))
         ((eq op 'cdddr) (sys:compile `(cdr (cdr (cdr ,(cadr expr)))) env fenv))
         ;; first, second, third, fourth - list accessors
         ((eq op 'first) (sys:compile `(car ,(cadr expr)) env fenv))
         ((eq op 'second) (sys:compile `(cadr ,(cadr expr)) env fenv))
         ((eq op 'third) (sys:compile `(caddr ,(cadr expr)) env fenv))
         ((eq op 'fourth) (sys:compile `(cadddr ,(cadr expr)) env fenv))
         ;; rest - same as cdr
         ((eq op 'rest) (sys:compile `(cdr ,(cadr expr)) env fenv))
         ;; nth - get nth element
         ((eq op 'nth)
          (let ((n (cadr expr))
                (lst (caddr expr)))
            (if (numberp n)
                ;; Constant index - expand to car/cdr chain
                (if (= n 0)
                    (sys:compile `(car ,lst) env fenv)
                    (sys:compile `(nth ,(- n 1) (cdr ,lst)) env fenv))
                ;; Variable index - use labels recursion
                (let ((nth-iter-fn (gensym "NTH-ITER"))
                      (n-var (gensym "N"))
                      (lst-var (gensym "LST")))
                  (sys:compile
                   `(labels ((,nth-iter-fn (,n-var ,lst-var)
                               (if (= ,n-var 0)
                                   (car ,lst-var)
                                   (,nth-iter-fn (- ,n-var 1) (cdr ,lst-var)))))
                      (,nth-iter-fn ,n ,lst))
                   env fenv)))))
         ;; count - count occurrences
         ((eq op 'count)
          (let ((count-iter-fn (gensym "COUNT-ITER"))
                (item-var (gensym "ITEM"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC")))
            (sys:compile
             `(labels ((,count-iter-fn (,item-var ,lst-var ,acc-var)
                         (if (null ,lst-var)
                             ,acc-var
                             (,count-iter-fn ,item-var (cdr ,lst-var)
                                             (if (eq ,item-var (car ,lst-var))
                                                 (+ ,acc-var 1)
                                                 ,acc-var)))))
                (,count-iter-fn ,(cadr expr) ,(caddr expr) 0))
             env fenv)))
         ((eq op 'list)
          (labels ((bl (args)
                     (if (null args) (list 'nil-ir)
                         (list 'cons-ir (sys:compile (car args) env fenv) (bl (cdr args))))))
            (bl (cdr expr))))
         ((eq op 'null)
          ;; nil is 0x06 (tag 6), so compare directly against that value
          ;; Using nil-ir which generates 0x06
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) '(nil-ir)))
         ((eq op 'numberp)
          ;; get-tag returns tagged fixnum (tag << 4), lit also tags its value
          ;; so to compare tag=0, use (lit 0) -> becomes 0
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 0)))
         ((eq op 'consp)
          ;; get-tag returns tagged fixnum (tag << 4), lit also tags its value
          ;; so to compare tag=1, use (lit 1) -> becomes 1<<4=16
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 1)))
         ((eq op 'symbolp)
          ;; Symbol tag is 2, so compare with (lit 2) -> becomes 2<<4=32
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 2)))
         ((eq op 'stringp)
          ;; String tag is 4, so compare with (lit 4) -> becomes 4<<4=64
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 4)))
         ((eq op 'vectorp)
          ;; Vector tag is 3, so compare with (lit 3) -> becomes 3<<4=48
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 3)))
         ;; length - list length via recursion
         ((eq op 'length)
          (let ((len-iter-fn (gensym "LEN-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC")))
            (sys:compile
             `(labels ((,len-iter-fn (,lst-var ,acc-var)
                         (if (null ,lst-var)
                             ,acc-var
                             (,len-iter-fn (cdr ,lst-var) (+ ,acc-var 1)))))
                (,len-iter-fn ,(cadr expr) 0))
             env fenv)))
         ;; reverse - reverse list via recursion
         ((eq op 'reverse)
          (let ((rev-iter-fn (gensym "REV-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC"))
                (next-acc-var (gensym "NEXT-ACC")))
            (sys:compile
             `(labels ((,rev-iter-fn (,lst-var ,acc-var)
                         (if (null ,lst-var)
                             ,acc-var
                             ;; BUG #20 WORKAROUND: Evaluate cons in let before recursive call
                             (let ((,next-acc-var (cons (car ,lst-var) ,acc-var)))
                               (,rev-iter-fn (cdr ,lst-var) ,next-acc-var)))))
                (,rev-iter-fn ,(cadr expr) nil))
             env fenv)))
         ;; append - append two lists
         ((eq op 'append)
          (let ((args (cdr expr)))
            (if (null args)
                (sys:compile nil env fenv)
                (if (null (cdr args))
                    (sys:compile (car args) env fenv)
                    ;; Two-arg append: copy first list, point to second
                    (let ((app-iter-fn (gensym "APP-ITER"))
                          (lst-var (gensym "LST"))
                          (tail-var (gensym "TAIL")))
                      (sys:compile
                       `(labels ((,app-iter-fn (,lst-var ,tail-var)
                                   (if (null ,lst-var)
                                       ,tail-var
                                       (cons (car ,lst-var) (,app-iter-fn (cdr ,lst-var) ,tail-var)))))
                          (,app-iter-fn ,(car args) (append ,@(cdr args))))
                       env fenv))))))
         ;; mapcar - map function over list
         ((eq op 'mapcar)
          (let ((map-iter-fn (gensym "MAP-ITER"))
                (fn-var (gensym "FN"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC"))
                (next-acc-var (gensym "NEXT-ACC")))
            (sys:compile
             `(labels ((,map-iter-fn (,fn-var ,lst-var ,acc-var)
                         (if (null ,lst-var)
                             (reverse ,acc-var)
                             ;; BUG #20 WORKAROUND: Evaluate cons in let before recursive call
                             (let ((,next-acc-var (cons (funcall ,fn-var (car ,lst-var)) ,acc-var)))
                               (,map-iter-fn ,fn-var (cdr ,lst-var) ,next-acc-var)))))
                (,map-iter-fn ,(cadr expr) ,(caddr expr) nil))
             env fenv)))
         ;; member - find element in list
         ((eq op 'member)
          (let ((mem-iter-fn (gensym "MEM-ITER"))
                (item-var (gensym "ITEM"))
                (lst-var (gensym "LST")))
            (sys:compile
             `(labels ((,mem-iter-fn (,item-var ,lst-var)
                         (if (null ,lst-var)
                             nil
                             (if (eq ,item-var (car ,lst-var))
                                 ,lst-var
                                 (,mem-iter-fn ,item-var (cdr ,lst-var))))))
                (,mem-iter-fn ,(cadr expr) ,(caddr expr)))
             env fenv)))
         ;; assoc - find association in alist
         ((eq op 'assoc)
          (let ((assoc-iter-fn (gensym "ASSOC-ITER"))
                (key-var (gensym "KEY"))
                (lst-var (gensym "LST")))
            (sys:compile
             `(labels ((,assoc-iter-fn (,key-var ,lst-var)
                         (if (null ,lst-var)
                             nil
                             (if (eq ,key-var (car (car ,lst-var)))
                                 (car ,lst-var)
                                 (,assoc-iter-fn ,key-var (cdr ,lst-var))))))
                (,assoc-iter-fn ,(cadr expr) ,(caddr expr)))
             env fenv)))
         ;; progn - evaluate forms in sequence, return last
         ((eq op 'progn)
          (let ((forms (cdr expr)))
            (if (null forms)
                (list 'lit 0)
                (if (null (cdr forms))
                    (sys:compile (car forms) env fenv)
                    (list 'progn-ir
                          (mapcar (lambda (f) (sys:compile f env fenv)) forms))))))
         ;; and - short-circuit and (returns nil when false, not 0 - while checks for nil)
         ((eq op 'and)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 1)
                (if (null (cdr args))
                    (sys:compile (car args) env fenv)
                    (list 'if-ir
                          (sys:compile (car args) env fenv)
                          (sys:compile (cons 'and (cdr args)) env fenv)
                          '(nil-ir))))))
         ;; or - short-circuit or (returns first truthy value, nil when all false)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (if (null args)
                '(nil-ir)
                (if (null (cdr args))
                    (sys:compile (car args) env fenv)
                    ;; Need to evaluate first arg, check if truthy, return it or continue
                    ;; Use a let to bind the value, then check and return
                    (let ((tmp (gensym "OR")))
                      (sys:compile
                       (list 'LET (list (list tmp (car args)))
                             (list 'if tmp tmp (cons 'or (cdr args))))
                       env fenv))))))
         ;; not - logical not (nil is 0x06, not 0 - use nil-ir)
         ((eq op 'not)
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) '(nil-ir)))
         ;; funcall - call function by value
         ((eq op 'funcall)
          (list 'funcall-ir
                (sys:compile (cadr expr) env fenv)
                (mapcar (lambda (a) (sys:compile a env fenv)) (cddr expr))))
         ;; setq - assign to variable
         ((eq op 'setq)
          (let* ((var (cadr expr))
                 (val (caddr expr))
                 (off (env-lookup var env)))
            (if (numberp off)
                (list 'setq-ir off (sys:compile val env fenv))
                (list 'lit 0))))
         ;; setcar - mutate car of cons cell
         ((eq op 'setcar)
          (list 'setcar-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; setcdr - mutate cdr of cons cell
         ((eq op 'setcdr)
          (list 'setcdr-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; read-file - read entire file contents as string
         ((eq op 'read-file)
          (list 'read-file-ir (sys:compile (cadr expr) env fenv)))
         ;; write-file - write string to file
         ((eq op 'write-file)
          (list 'write-file-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; println - print value with newline
         ((eq op 'println)
          (list 'println-ir (sys:compile (cadr expr) env fenv)))
         ;; string-length - get length of string
         ((eq op 'string-length)
          (list 'string-length-ir (sys:compile (cadr expr) env fenv)))
         ;; string-ref - get character at index
         ((eq op 'string-ref)
          (list 'string-ref-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; char-code - in Habu, characters ARE fixnums, so this is identity
         ((eq op 'char-code)
          (sys:compile (cadr expr) env fenv))
         ;; string-append - concatenate two strings
         ;; Expands to: (let* ((s1 str1) (s2 str2)
         ;;                     (len1 (string-length s1)) (len2 (string-length s2))
         ;;                     (total (+ len1 len2))
         ;;                     (vec (make-vector total)))
         ;;               (labels ((copy1 (i) (if (< i len1)
         ;;                                       (progn (vector-set vec i (string-ref s1 i))
         ;;                                              (copy1 (+ i 1)))))
         ;;                        (copy2 (i) (if (< i len2)
         ;;                                       (progn (vector-set vec (+ len1 i) (string-ref s2 i))
         ;;                                              (copy2 (+ i 1))))))
         ;;                 (copy1 0)
         ;;                 (copy2 0)
         ;;                 (make-string-from-vector vec)))
         ((eq op 'string-append)
          ;; BUG FIX: Use gensyms for ALL variables to avoid shadowing in nested calls
          (let ((copy1-fn (gensym "COPY1"))
                (copy2-fn (gensym "COPY2"))
                (s1-var (gensym "S1"))
                (s2-var (gensym "S2"))
                (len1-var (gensym "LEN1"))
                (len2-var (gensym "LEN2"))
                (total-var (gensym "TOTAL"))
                (vec-var (gensym "VEC"))
                (i-var (gensym "I")))
            (sys:compile
             (list 'let* (list (list s1-var (cadr expr))
                               (list s2-var (caddr expr))
                               (list len1-var (list 'string-length s1-var))
                               (list len2-var (list 'string-length s2-var))
                               (list total-var (list '+ len1-var len2-var))
                               (list vec-var (list 'make-vector total-var)))
                   (list 'labels (list (list copy1-fn (list i-var)
                                             (list 'if (list '< i-var len1-var)
                                                   (list 'progn
                                                         (list 'vector-set vec-var i-var (list 'string-ref s1-var i-var))
                                                         (list copy1-fn (list '+ i-var 1)))))
                                       (list copy2-fn (list i-var)
                                             (list 'if (list '< i-var len2-var)
                                                   (list 'progn
                                                         (list 'vector-set vec-var (list '+ len1-var i-var) (list 'string-ref s2-var i-var))
                                                         (list copy2-fn (list '+ i-var 1))))))
                         (list copy1-fn 0)
                         (list copy2-fn 0)
                         (list 'make-string-from-vector vec-var)))
             env fenv)))
         ;; number-to-string - convert fixnum to string
         ;; Simplified implementation: handles 0-99999
         ;; CRITICAL: Use gensyms for ALL variables to prevent shadowing in nested calls
         ((eq op 'number-to-string)
          (let ((n-var (gensym "N"))
                (vec-var (gensym "VEC"))
                (d1-var (gensym "D1"))
                (d2-var (gensym "D2"))
                (d3-var (gensym "D3"))
                (d4-var (gensym "D4"))
                (d5-var (gensym "D5"))
                (rem-var (gensym "REM"))
                (rem2-var (gensym "REM2"))
                (rem3-var (gensym "REM3")))
            (sys:compile
             `(let ((,n-var ,(cadr expr)))
                (if (= ,n-var 0)
                    "0"
                    (if (< ,n-var 10)
                        (let ((,vec-var (make-vector 1)))
                          (vector-set ,vec-var 0 (+ 48 ,n-var))
                          (make-string-from-vector ,vec-var))
                        (if (< ,n-var 100)
                            (let* ((,d1-var (/ ,n-var 10))
                                   (,d2-var (mod ,n-var 10))
                                   (,vec-var (make-vector 2)))
                              (vector-set ,vec-var 0 (+ 48 ,d1-var))
                              (vector-set ,vec-var 1 (+ 48 ,d2-var))
                              (make-string-from-vector ,vec-var))
                            (if (< ,n-var 1000)
                                (let* ((,d1-var (/ ,n-var 100))
                                       (,rem-var (mod ,n-var 100))
                                       (,d2-var (/ ,rem-var 10))
                                       (,d3-var (mod ,rem-var 10))
                                       (,vec-var (make-vector 3)))
                                  (vector-set ,vec-var 0 (+ 48 ,d1-var))
                                  (vector-set ,vec-var 1 (+ 48 ,d2-var))
                                  (vector-set ,vec-var 2 (+ 48 ,d3-var))
                                  (make-string-from-vector ,vec-var))
                                (if (< ,n-var 10000)
                                    (let* ((,d1-var (/ ,n-var 1000))
                                           (,rem-var (mod ,n-var 1000))
                                           (,d2-var (/ ,rem-var 100))
                                           (,rem2-var (mod ,rem-var 100))
                                           (,d3-var (/ ,rem2-var 10))
                                           (,d4-var (mod ,rem2-var 10))
                                           (,vec-var (make-vector 4)))
                                      (vector-set ,vec-var 0 (+ 48 ,d1-var))
                                      (vector-set ,vec-var 1 (+ 48 ,d2-var))
                                      (vector-set ,vec-var 2 (+ 48 ,d3-var))
                                      (vector-set ,vec-var 3 (+ 48 ,d4-var))
                                      (make-string-from-vector ,vec-var))
                                    (let* ((,d1-var (/ ,n-var 10000))
                                           (,rem-var (mod ,n-var 10000))
                                           (,d2-var (/ ,rem-var 1000))
                                           (,rem2-var (mod ,rem-var 1000))
                                           (,d3-var (/ ,rem2-var 100))
                                           (,rem3-var (mod ,rem2-var 100))
                                           (,d4-var (/ ,rem3-var 10))
                                           (,d5-var (mod ,rem3-var 10))
                                           (,vec-var (make-vector 5)))
                                      (vector-set ,vec-var 0 (+ 48 ,d1-var))
                                      (vector-set ,vec-var 1 (+ 48 ,d2-var))
                                      (vector-set ,vec-var 2 (+ 48 ,d3-var))
                                      (vector-set ,vec-var 3 (+ 48 ,d4-var))
                                      (vector-set ,vec-var 4 (+ 48 ,d5-var))
                                      (make-string-from-vector ,vec-var))))))))
             env fenv)))
         ;; system - execute shell command
         ((eq op 'system)
          (list 'system-ir (sys:compile (cadr expr) env fenv)))
         ;; string= - compare two strings (via runtime)
         ((eq op 'string=)
          (list 'string-equal-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; make-vector - allocate a vector of size n
         ((eq op 'make-vector)
          (list 'make-vector-ir (sys:compile (cadr expr) env fenv)))
         ;; vector-set - set element at index
         ((eq op 'vector-set)
          (list 'vector-set-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)
                (sys:compile (cadddr expr) env fenv)))
         ;; vector-ref - get element at index
         ((eq op 'vector-ref)
          (list 'vector-ref-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; aref - same as vector-ref for now
         ((eq op 'aref)
          (list 'vector-ref-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; vector-length - get vector size
         ((eq op 'vector-length)
          (list 'vector-length-ir (sys:compile (cadr expr) env fenv)))
         ;; buffer-byte-ref - get raw byte at index from vector data area
         ;; Used for reading file data written by sys-read
         ((eq op 'buffer-byte-ref)
          (list 'buffer-byte-ref-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; make-string-from-vector - convert vector of char codes to string
         ((eq op 'make-string-from-vector)
          (list 'make-string-from-vector-ir (sys:compile (cadr expr) env fenv)))
         ;; buffer-to-string - convert raw byte buffer to string (for sys-read data)
         ((eq op 'buffer-to-string)
          (list 'buffer-to-string-ir
                (sys:compile (cadr expr) env fenv)    ; buffer
                (sys:compile (caddr expr) env fenv))) ; length
         ;; make-symbol-from-string - intern a string as symbol
         ((eq op 'make-symbol-from-string)
          (list 'make-symbol-from-string-ir (sys:compile (cadr expr) env fenv)))
         ;; intern - same as make-symbol-from-string
         ((eq op 'intern)
          (list 'make-symbol-from-string-ir (sys:compile (cadr expr) env fenv)))
         ;; symbol-name - get the name string of a symbol
         ((eq op 'symbol-name)
          (list 'symbol-name-ir (sys:compile (cadr expr) env fenv)))
         ;; write-bytes - write vector of bytes to file
         ((eq op 'write-bytes)
          (list 'write-bytes-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; === libSystem calls (for native executables) ===
         ;; sys-write - write(fd, buf, len) -> returns bytes written
         ((eq op 'sys-write)
          (list 'sys-write-ir
                (sys:compile (cadr expr) env fenv)    ; fd
                (sys:compile (caddr expr) env fenv)   ; buf (string)
                (sys:compile (cadddr expr) env fenv))) ; len
         ;; sys-read - read(fd, buf, len) -> returns bytes read
         ((eq op 'sys-read)
          (list 'sys-read-ir
                (sys:compile (cadr expr) env fenv)    ; fd
                (sys:compile (caddr expr) env fenv)   ; buf (vector)
                (sys:compile (cadddr expr) env fenv))) ; len
         ;; sys-open - open(path, flags, mode) -> returns fd
         ((eq op 'sys-open)
          (list 'sys-open-ir
                (sys:compile (cadr expr) env fenv)    ; path (string)
                (sys:compile (caddr expr) env fenv)   ; flags
                (sys:compile (cadddr expr) env fenv))) ; mode
         ;; sys-close - close(fd) -> returns 0 on success
         ((eq op 'sys-close)
          (list 'sys-close-ir
                (sys:compile (cadr expr) env fenv)))  ; fd
         ;; sys-exit - exit(code) -> does not return
         ((eq op 'sys-exit)
          (list 'sys-exit-ir
                (sys:compile (cadr expr) env fenv)))  ; exit code
         ;; get-intern-table - get the global intern table (for symbol interning)
         ((eq op 'get-intern-table)
          (list 'get-intern-table-ir))
         ;; set-intern-table - set the global intern table
         ((eq op 'set-intern-table)
          (list 'set-intern-table-ir
                (sys:compile (cadr expr) env fenv)))
         ;; get-lambda-counter - get the global lambda counter
         ((eq op 'get-lambda-counter)
          (list 'get-lambda-counter-ir))
         ;; set-lambda-counter - set the global lambda counter
         ((eq op 'set-lambda-counter)
          (list 'set-lambda-counter-ir
                (sys:compile (cadr expr) env fenv)))
         ;; === High-level file I/O (using sys-* primitives) ===
         ;; native-read-file - read entire file to string
         ;; Expands to: (let* ((fd (sys-open path O_RDONLY 0))
         ;;                     (buf (make-vector 524288))  ; 512KB buffer for combined sources
         ;;                     (n (sys-read fd buf 524288)))
         ;;               (sys-close fd)
         ;;               (buffer-to-string buf n))
         ((eq op 'native-read-file)
          ;; Uses 65536 element vector = 512KB storage, reads up to 65536 bytes
          ;; NOTE: make-vector allocates 8 bytes per element, so vector size must be
          ;; chosen carefully. 65536 elements = 512KB vector, allows reading 65KB files.
          ;; For larger files, use native-read-file-large which reads in chunks.
          (let ((path-var (gensym "PATH"))
                (fd-var (gensym "FD"))
                (buf-var (gensym "BUF"))
                (n-var (gensym "N")))
            (sys:compile
             (list 'LET* (list (list path-var (cadr expr))
                               (list fd-var (list 'sys-open path-var #x0 0))  ; O_RDONLY = 0
                               (list buf-var (list 'make-vector 65536))
                               (list n-var (list 'sys-read fd-var buf-var 65536)))
                   (list 'sys-close fd-var)
                   (list 'buffer-to-string buf-var n-var))
             env fenv)))
         ;; native-write-file - write string to file
         ;; Expands to: (let* ((fd (sys-open path O_WRONLY|O_CREAT|O_TRUNC 0644))
         ;;                     (n (sys-write fd str (string-length str))))
         ;;               (sys-close fd)
         ;;               n)
         ((eq op 'native-write-file)
          (let ((path-var (gensym "PATH"))
                (str-var (gensym "STR"))
                (fd-var (gensym "FD"))
                (len-var (gensym "LEN"))
                (n-var (gensym "N")))
            (sys:compile
             (list 'LET* (list (list path-var (cadr expr))
                               (list str-var (caddr expr))
                               ;; O_WRONLY|O_CREAT|O_TRUNC = 0x1|0x200|0x400 = 0x601
                               (list fd-var (list 'sys-open path-var #x601 #o644))
                               (list len-var (list 'string-length str-var))
                               (list n-var (list 'sys-write fd-var str-var len-var)))
                   (list 'sys-close fd-var)
                   n-var)
             env fenv)))
         ;; native-write-bytes - write byte vector to file
         ;; Expands to: (let* ((fd (sys-open path O_WRONLY|O_CREAT|O_TRUNC 0644))
         ;;                     (len (vector-length vec))
         ;;                     (n (sys-write fd vec len)))
         ;;               (sys-close fd)
         ;;               n)
         ;; Note: sys-write can write from vectors too, not just strings
         ((eq op 'native-write-bytes)
          (let ((path-var (gensym "PATH"))
                (vec-var (gensym "VEC"))
                (fd-var (gensym "FD"))
                (len-var (gensym "LEN"))
                (n-var (gensym "N")))
            (sys:compile
             (list 'LET* (list (list path-var (cadr expr))
                               (list vec-var (caddr expr))
                               ;; O_WRONLY|O_CREAT|O_TRUNC = 0x601
                               (list fd-var (list 'sys-open path-var #x601 #o644))
                               (list len-var (list 'vector-length vec-var))
                               (list n-var (list 'sys-write fd-var vec-var len-var)))
                   (list 'sys-close fd-var)
                   n-var)
             env fenv)))
         ;; native-read-file-large - read file in chunks, collect in list, then concatenate
         ;; Expands to: (let* ((fd (sys-open path O_RDONLY 0))
         ;;                     (buf (make-vector 65536)))
         ;;               (labels ((read-chunks (chunks total-len)
         ;;                          (let ((n (sys-read fd buf 65536)))
         ;;                            (if (= n 0)
         ;;                                (list chunks total-len)
         ;;                                (let* ((chunk (buffer-to-string buf n))
         ;;                                       ;; BUG #20 WORKAROUND: Evaluate cons before recursive call
         ;;                                       (next-chunks (cons chunk chunks))
         ;;                                       (next-total (+ total-len n)))
         ;;                                  (read-chunks next-chunks next-total))))))
         ;;                 (let* ((result-list (read-chunks nil 0))
         ;;                        (chunks (car result-list))
         ;;                        (total (car (cdr result-list))))
         ;;                   (sys-close fd)
         ;;                   (concat-string-list chunks total))))
         ((eq op 'native-read-file-large)
          ;; BUG #22 FIX: Use iterative while loop instead of recursive labels
          ;; This avoids stack overflow for large files (74KB+ would be 19+ recursive calls)
          ;; Strategy: Use single reusable buffer, accumulate chunks in list, then concat
          ;; Expands to:
          ;; (let* ((path <path-expr>)
          ;;        (fd (sys-open path 0 0))
          ;;        (buf (make-vector 4096))  ; reused buffer
          ;;        (chunks nil)
          ;;        (total 0)
          ;;        (n 0))
          ;;   (while (progn (setq n (sys-read fd buf 4096)) (> n 0))
          ;;     (setq chunks (cons (buffer-to-string buf n) chunks))
          ;;     (setq total (+ total n)))
          ;;   (sys-close fd)
          ;;   (concat-string-list-iter chunks total))
          (let ((path-var (gensym "PATH"))
                (fd-var (gensym "FD"))
                (buf-var (gensym "BUF"))
                (chunks-var (gensym "CHUNKS"))
                (total-var (gensym "TOTAL"))
                (n-var (gensym "N")))
            (sys:compile
             (list 'let* (list (list path-var (cadr expr))
                               (list fd-var (list 'sys-open path-var #x0 0))
                               (list buf-var (list 'make-vector 4096))
                               (list chunks-var nil)
                               (list total-var 0)
                               (list n-var 0))
                   (list 'while (list 'progn
                                     (list 'setq n-var (list 'sys-read fd-var buf-var 4096))
                                     (list '> n-var 0))
                         (list 'setq chunks-var (list 'cons (list 'buffer-to-string buf-var n-var) chunks-var))
                         (list 'setq total-var (list '+ total-var n-var)))
                   (list 'sys-close fd-var)
                   (list 'concat-string-list-iter chunks-var total-var))
             env fenv)))
         ;; concat-string-list - concatenate list of strings (in reverse order) into single string
         ;; Expands to: (let* ((vec (make-vector total-len))
         ;;                     (offset 0))
         ;;               (labels ((copy-chunk (chunks offset)
         ;;                          (if (null chunks)
         ;;                              vec
         ;;                              (let* ((chunk (car chunks))
         ;;                                     (len (string-length chunk)))
         ;;                                (labels ((copy-chars (i)
         ;;                                           (if (< i len)
         ;;                                               (progn (vector-set vec (+ offset i) (string-ref chunk i))
         ;;                                                      (copy-chars (+ i 1))))))
         ;;                                  (copy-chars 0)
         ;;                                  ;; BUG #20 WORKAROUND: Evaluate complex expressions in let before recursive call
         ;;                                  (let ((next-chunks (cdr chunks))
         ;;                                        (next-offset (+ offset len)))
         ;;                                    (copy-chunk next-chunks next-offset)))))))
         ;;                 (make-string-from-vector (copy-chunk (reverse chunks) 0))))
         ((eq op 'concat-string-list)
          ;; BUG #20 FIX: Pass env (for correct variable offsets) but nil fenv
          ;; This allows variable lookups while avoiding outer function contamination
          (let* ((chunks-var (gensym "CHUNKS"))
                 (total-var (gensym "TOTAL"))
                 (vec-var (gensym "VEC"))
                 (concat-fn (gensym "CONCAT-LOOP"))
                 (chunks-param (gensym "CHUNKS"))
                 (offset-param (gensym "OFFSET"))
                 (idx-param (gensym "IDX"))
                 (chunk-var (gensym "CHUNK"))
                 (len-var (gensym "LEN")))
            (sys:compile
             (list 'let* (list (list chunks-var (cadr expr))
                              (list total-var (caddr expr))
                              (list vec-var (list 'make-vector total-var)))
                   (list 'labels (list (list concat-fn
                                             (list chunks-param offset-param idx-param)
                                             (list 'if (list 'null chunks-param)
                                                   vec-var
                                                   (list 'let* (list (list chunk-var (list 'car chunks-param))
                                                                     (list len-var (list 'string-length chunk-var)))
                                                         (list 'if (list '< idx-param len-var)
                                                               ;; Copy current character and recur with idx+1
                                                               (list 'progn
                                                                     (list 'vector-set vec-var
                                                                           (list '+ offset-param idx-param)
                                                                           (list 'string-ref chunk-var idx-param))
                                                                     (list concat-fn chunks-param offset-param (list '+ idx-param 1)))
                                                               ;; Move to next chunk
                                                               (list concat-fn
                                                                     (list 'cdr chunks-param)
                                                                     (list '+ offset-param len-var)
                                                                     0))))))
                         (list 'make-string-from-vector
                               (list concat-fn (list 'reverse chunks-var) 0 0))))
             env
             nil)))  ; nil fenv
         ;; concat-string-list-iter - ITERATIVE version to avoid stack overflow
         ;; Uses two nested while loops instead of recursion
         ;; Expands to:
         ;; (let* ((chunks <chunks-expr>)
         ;;        (total <total-expr>)
         ;;        (vec (make-vector total))
         ;;        (rev-chunks (reverse chunks))
         ;;        (offset 0))
         ;;   (while rev-chunks
         ;;     (let* ((chunk (car rev-chunks))
         ;;            (len (string-length chunk))
         ;;            (i 0))
         ;;       (while (< i len)
         ;;         (vector-set vec (+ offset i) (string-ref chunk i))
         ;;         (setq i (+ i 1)))
         ;;       (setq offset (+ offset len))
         ;;       (setq rev-chunks (cdr rev-chunks))))
         ;;   (make-string-from-vector vec))
         ((eq op 'concat-string-list-iter)
          (let* ((chunks-var (gensym "CHUNKS"))
                 (total-var (gensym "TOTAL"))
                 (vec-var (gensym "VEC"))
                 (rev-chunks-var (gensym "REV-CHUNKS"))
                 (offset-var (gensym "OFFSET"))
                 (chunk-var (gensym "CHUNK"))
                 (len-var (gensym "LEN"))
                 (i-var (gensym "I")))
            (sys:compile
             (list 'let* (list (list chunks-var (cadr expr))
                               (list total-var (caddr expr))
                               (list vec-var (list 'make-vector total-var))
                               (list rev-chunks-var (list 'reverse chunks-var))
                               (list offset-var 0))
                   (list 'while rev-chunks-var
                         (list 'let* (list (list chunk-var (list 'car rev-chunks-var))
                                           (list len-var (list 'string-length chunk-var))
                                           (list i-var 0))
                               (list 'while (list '< i-var len-var)
                                     (list 'vector-set vec-var
                                           (list '+ offset-var i-var)
                                           (list 'string-ref chunk-var i-var))
                                     (list 'setq i-var (list '+ i-var 1)))
                               (list 'setq offset-var (list '+ offset-var len-var))
                               (list 'setq rev-chunks-var (list 'cdr rev-chunks-var))))
                   (list 'make-string-from-vector vec-var))
             env fenv)))
         ;; char-upcase - convert lowercase char code to uppercase
         ;; Transform to: (if (and (>= ch #x61) (<= ch #x7A)) (- ch #x20) ch)
         ((eq op 'char-upcase)
          (let ((ch-var (gensym "CH")))
            (sys:compile
             (list 'LET* (list (list ch-var (cadr expr)))
                   (list 'if (list 'and (list '>= ch-var #x61) (list '<= ch-var #x7A))
                         (list '- ch-var #x20)
                         ch-var))
             env fenv)))
         ;; string-upcase - convert string to uppercase
         ;; Transform to: build new string with uppercased chars using dotimes
         ((eq op 'string-upcase)
          (let ((str-var (gensym "STR"))
                (len-var (gensym "LEN"))
                (vec-var (gensym "VEC"))
                (i-var (gensym "I")))
            (sys:compile
             (list 'LET* (list (list str-var (cadr expr))
                               (list len-var (list 'string-length str-var))
                               (list vec-var (list 'make-vector len-var)))
                   (list 'dotimes (list i-var len-var vec-var)
                         (list 'vector-set vec-var i-var
                               (list 'char-upcase (list 'string-ref str-var i-var))))
                   (list 'make-string-from-vector vec-var))
             env fenv)))
         ;; incf - increment variable
         ((eq op 'incf)
          (let* ((place (cadr expr))
                 (delta (if (cddr expr) (caddr expr) 1)))
            (sys:compile (list 'setq place (list '+ place delta)) env fenv)))
         ;; push - push item onto list variable
         ((eq op 'push)
          (let* ((item (cadr expr))
                 (place (caddr expr)))
            (sys:compile (list 'setq place (list 'cons item place)) env fenv)))
         ;; setf - generalized assignment
         ((eq op 'setf)
          (let* ((place (cadr expr))
                 (val (caddr expr)))
            (if (symbolp place)
                ;; Simple variable assignment
                (sys:compile (list 'setq place val) env fenv)
                (if (consp place)
                    (let ((place-op (car place)))
                      (cond ((eq place-op 'car)
                             (sys:compile (list 'setcar (cadr place) val) env fenv))
                            ((eq place-op 'cdr)
                             (sys:compile (list 'setcdr (cadr place) val) env fenv))
                            ((eq place-op 'aref)
                             (sys:compile (list 'vector-set (cadr place) (caddr place) val) env fenv))
                            ((eq place-op 'nth)
                             ;; (setf (nth n lst) val) -> setcar on nthcdr
                             (sys:compile (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val) env fenv))
                            (t (list 'lit 0))))
                    (list 'lit 0)))))
         ;; nthcdr - get nth cdr of list
         ((eq op 'nthcdr)
          (list 'nthcdr-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; values - return multiple values
         ((eq op 'values)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'values-ir nil)
                (list 'values-ir (mapcar (lambda (a) (sys:compile a env fenv)) args)))))
         ;; multiple-value-bind - bind multiple values from form
         ((eq op 'multiple-value-bind)
          (let* ((vars (cadr expr))
                 (form (caddr expr))
                 (body (cdddr expr))
                 (nvars (length vars)))
            (list 'mvb-ir vars (sys:compile form env fenv) nvars
                  (sys:compile (if (null (cdr body)) (car body) (cons 'progn body))
                              (env-extend (mapcar (lambda (v) (cons v nil)) vars) env)
                              fenv))))
         ;; labels - local recursive functions
         ;; Uses Z combinator approach: each fn gets SELF as first param
         ;; Transform: (labels ((fn (params...) body)) main)
         ;; Into: (let ((fn nil))
         ;;         (setq fn (lambda (self params...) body'))
         ;;         main')
         ;; where body' rewrites (fn args) as (funcall self self args)
         ;; and main' rewrites (fn args) as (funcall fn fn args)
         ((eq op 'LABELS)
          ;; Transform using function table (FNTAB) approach for proper mutual recursion:
          ;; BUG FIX: Use gensym for FNTAB to avoid shadowing in nested labels
          ;; (labels ((f1 (a) ...) (f2 (b) ...)) body)
          ;; =>
          ;; (let ((f1 nil) (f2 nil))
          ;;   (setq f1 (lambda (FNTAB123 a) (let ((f1 (car FNTAB123)) (f2 (car (cdr FNTAB123)))) ...)))
          ;;   (setq f2 (lambda (FNTAB123 b) (let ((f1 (car FNTAB123)) (f2 (car (cdr FNTAB123)))) ...)))
          ;;   (let ((FNTAB123 (cons f1 (cons f2 nil))))
          ;;     body-rewritten))
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr))
                 (fn-names (mapcar #'car bindings))
                 (fntab-var (gensym "FNTAB"))  ;; BUG FIX: gensym FNTAB to avoid nested shadowing
                 ;; Build let bindings: ((fn1 nil) (fn2 nil) ...)
                 (let-bindings (mapcar (lambda (n) (list n nil)) fn-names))
                 ;; Build car/cdr chain bindings for unpacking FNTAB inside each lambda
                 ;; ((f1 (car FNTAB123)) (f2 (car (cdr FNTAB123))) (f3 (car (cdr (cdr FNTAB123)))) ...)
                 (fntab-unpack (labels ((build (names depth acc)
                                          (if (null names) (reverse acc)
                                              (let ((accessor (labels ((wrap-cdr (n base)
                                                                          (if (= n 0) base
                                                                              (wrap-cdr (1- n) (list 'cdr base)))))
                                                               (list 'car (wrap-cdr depth fntab-var)))))
                                                (build (cdr names) (1+ depth)
                                                       (cons (list (car names) accessor) acc))))))
                                 (build fn-names 0 nil)))
                 ;; Transform each function: add FNTAB param, unpack functions, rewrite calls
                 (setq-forms (mapcar (lambda (b)
                                       (let* ((fn-name (car b))
                                              (params (cadr b))
                                              (fn-body (cddr b))
                                              (fn-body-expr (if (null (cdr fn-body))
                                                                (car fn-body)
                                                                (cons 'progn fn-body)))
                                              ;; Rewrite calls: (fn args) -> (funcall fn FNTAB123 args)
                                              (rewritten (rewrite-labels-body fn-body-expr fn-names fntab-var))
                                              ;; Wrap body in let that unpacks FNTAB123
                                              (wrapped-body (list 'LET fntab-unpack rewritten)))
                                         ;; Lambda gets FNTAB123 as first param
                                         (list 'setq fn-name (list 'lambda (cons fntab-var params) wrapped-body))))
                                     bindings))
                 ;; Build the FNTAB list: (cons f1 (cons f2 ... nil))
                 (fntab-init (labels ((build-list (names)
                                        (if (null names) 'nil
                                            (list 'cons (car names) (build-list (cdr names))))))
                               (build-list fn-names)))
                 ;; Rewrite main body: (fn args) -> (funcall fn FNTAB123 args)
                 (main-body (if (null (cdr body-forms))
                                (car body-forms)
                                (cons 'progn body-forms)))
                 (rewritten-main (rewrite-labels-body main-body fn-names fntab-var))
                 ;; Build: (let bindings (setq ...) (let ((FNTAB123 (cons ...))) main))
                 (inner-let (list 'LET (list (list fntab-var fntab-init)) rewritten-main))
                 (full-expr (list 'LET let-bindings
                                  (cons 'progn (append setq-forms (list inner-let))))))
            (sys:compile full-expr env fenv)))
         ;; flet - local non-recursive functions (same transform but no rewriting)
         ((eq op 'FLET)
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr))
                 (fn-names (mapcar #'car bindings))
                 (let-bindings (mapcar (lambda (b)
                                         (let* ((fn-name (car b))
                                                (params (cadr b))
                                                (fn-body (cddr b))
                                                (fn-body-expr (if (null (cdr fn-body))
                                                                  (car fn-body)
                                                                  (cons 'progn fn-body))))
                                           (list fn-name (list 'lambda params fn-body-expr))))
                                       bindings))
                 (main-body (if (null (cdr body-forms))
                                (car body-forms)
                                (cons 'progn body-forms)))
                 (rewritten-main (rewrite-labels-calls main-body fn-names)))
            (sys:compile (list 'LET let-bindings rewritten-main) env fenv)))
         ;; User function call or call via variable
         (t
          (cond
           ;; op is a lambda expression: ((lambda (x) body) args...)
           ((and (consp op) (eq (car op) 'lambda))
            (list 'funcall-ir
                  (sys:compile op env fenv)
                  (mapcar (lambda (a) (sys:compile a env fenv)) (cdr expr))))
           ;; op is a known function name
           ((and fenv (assoc op fenv))
            (let* ((fn-info (cdr (assoc op fenv)))
                   (args (cdr expr))
                   ;; fn-info is (positional-params . keyword-specs)
                   (positional-params (car fn-info))
                   (keyword-specs (cdr fn-info))
                   ;; Rewrite call if function has &key (even if call doesn't use them)
                   ;; This appends defaults for any unspecified keyword params
                   (final-args (if keyword-specs
                                   (rewrite-keyword-call args
                                                          (length positional-params)
                                                          keyword-specs)
                                   args)))
              (list 'call-fn op (mapcar (lambda (a) (sys:compile a env fenv)) final-args))))
           ;; op is a variable (parameter) - compile as funcall
           (t
            (let ((off (env-lookup op env)))
              (if (numberp off)
                  (list 'funcall-ir (list 'var off) (mapcar (lambda (a) (sys:compile a env fenv)) (cdr expr)))
                  (list 'lit 0)))))))))
    (t (list 'lit 0))))

;;; ============================================================
;;; Part 6b: IR Evaluator (eval-*)
;;; ============================================================

(defun eval-ir (ir env)
  "Evaluate IR and return tagged value"
  (cond
    ((has-tag ir 'lit) (cadr ir))
    ((has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((has-tag ir 'add)
     (+ (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'sub)
     (- (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'mul)
     (* (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'band)
     (logand (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'bor)
     (logior (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'bxor)
     (logxor (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'bsh)
     (ash (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)))
    ((has-tag ir 'cmp-eq)
     (if (= (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)) 1 0))
    ((has-tag ir 'cmp-lt)
     (if (< (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)) 1 0))
    ((has-tag ir 'cmp-gt)
     (if (> (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)) 1 0))
    ((has-tag ir 'cmp-le)
     (if (<= (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)) 1 0))
    ((has-tag ir 'cmp-ge)
     (if (>= (eval-ir (cadr ir) env) (eval-ir (caddr ir) env)) 1 0))
    ((has-tag ir 'if-ir)
     (if (not (= (eval-ir (cadr ir) env) 0))
         (eval-ir (caddr ir) env)
         (eval-ir (cadddr ir) env)))
    ((has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     ;; offs is at index 4, which is (nth 3 (cdr ir))
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))  ; Fixed: was (nth 4 ...)
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (eval-ir (car vs) env)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (eval-ir bir (bind vals offs env)))))
    (t 0)))

;; Global function environment for IR evaluation
(defvar *fenv* nil)

(defun eval-ir-with-fns (ir env fenv)
  "Evaluate IR with function environment"
  (cond
    ((has-tag ir 'lit) (cadr ir))
    ((has-tag ir 'nil-ir) nil)  ;; Evaluate to proper SBCL nil
    ((has-tag ir 'sym-lit)
     ;; Return the symbol itself (interned)
     (intern (cadr ir)))
    ((has-tag ir 'str-lit)
     ;; Return the string literal directly
     (cadr ir))
    ((has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((has-tag ir 'add)
     (+ (eval-ir-with-fns (cadr ir) env fenv)
        (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'sub)
     (- (eval-ir-with-fns (cadr ir) env fenv)
        (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'mul)
     (* (eval-ir-with-fns (cadr ir) env fenv)
        (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'div)
     (truncate (eval-ir-with-fns (cadr ir) env fenv)
               (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'mod-ir)
     (mod (eval-ir-with-fns (cadr ir) env fenv)
          (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'band)
     (logand (eval-ir-with-fns (cadr ir) env fenv)
             (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'bor)
     (logior (eval-ir-with-fns (cadr ir) env fenv)
             (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'bxor)
     (logxor (eval-ir-with-fns (cadr ir) env fenv)
             (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'bsh)
     (ash (eval-ir-with-fns (cadr ir) env fenv)
          (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'cmp-eq)
     (if (= (eval-ir-with-fns (cadr ir) env fenv)
            (eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((has-tag ir 'cmp-lt)
     (if (< (eval-ir-with-fns (cadr ir) env fenv)
            (eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((has-tag ir 'cmp-gt)
     (if (> (eval-ir-with-fns (cadr ir) env fenv)
            (eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((has-tag ir 'cmp-le)
     (if (<= (eval-ir-with-fns (cadr ir) env fenv)
             (eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((has-tag ir 'cmp-ge)
     (if (>= (eval-ir-with-fns (cadr ir) env fenv)
             (eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((has-tag ir 'cons-ir)
     (cons (eval-ir-with-fns (cadr ir) env fenv)
           (eval-ir-with-fns (caddr ir) env fenv)))
    ((has-tag ir 'car-ir)
     (car (eval-ir-with-fns (cadr ir) env fenv)))
    ((has-tag ir 'cdr-ir)
     (cdr (eval-ir-with-fns (cadr ir) env fenv)))
    ((has-tag ir 'if-ir)
     (if (not (= (eval-ir-with-fns (cadr ir) env fenv) 0))
         (eval-ir-with-fns (caddr ir) env fenv)
         (eval-ir-with-fns (cadddr ir) env fenv)))
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (eval-ir-with-fns (car vs) env fenv)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (eval-ir-with-fns bir (bind vals offs env) fenv))))
    ((has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     (let ((forms-ir (cadr ir)))
       (labels ((eval-seq (fs)
                  (if (null fs)
                      0
                      (let ((v (eval-ir-with-fns (car fs) env fenv)))
                        (if (null (cdr fs))
                            v
                            (eval-seq (cdr fs)))))))
         (eval-seq forms-ir))))
    ((has-tag ir 'call-fn)
     ;; call-fn = (call-fn name args-ir-list)
     (let* ((fnm (cadr ir))
            (args-ir (caddr ir))
            (fn-def (cdr (assoc fnm fenv))))
       (if fn-def
           ;; fn-def = (name params body-ir param-base)
           (let* ((body-ir (caddr fn-def)))
             ;; Evaluate arguments
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
               (let ((arg-vals (eval-args args-ir nil)))
                 ;; Call with new env containing args
                 (eval-ir-with-fns body-ir arg-vals fenv))))
           0)))
    ((has-tag ir 'funcall-ir)
     ;; funcall-ir = (funcall-ir fn-ir args-ir-list)
     ;; fn-ir evaluates to a function name (symbol) or closure
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (fn-val (eval-ir-with-fns fn-ir env fenv)))
       ;; Check if fn-val is a closure (list starting with :closure)
       (if (and (consp fn-val) (eq (car fn-val) :closure))
           ;; Closure: (:closure params body-ir free-vars captured-vals)
           ;; body is now pre-compiled IR
           (let* ((body-ir (caddr fn-val))
                  (captured-vals (nth 4 fn-val)))
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
               (let* ((arg-vals (eval-args args-ir nil))
                      ;; Build value list: free vars (captured) come first, then args
                      (all-vals (append captured-vals arg-vals)))
                 (eval-ir-with-fns body-ir all-vals fenv))))
           ;; Named function: look up in fenv
           (let ((fn-def (cdr (assoc fn-val fenv))))
             (if fn-def
                 (let* ((body-ir (caddr fn-def)))
                   (labels ((eval-args (airs acc)
                              (if (null airs) (reverse acc)
                                  (eval-args (cdr airs)
                                             (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
                     (let ((arg-vals (eval-args args-ir nil)))
                       (eval-ir-with-fns body-ir arg-vals fenv))))
                 0)))))
    ((has-tag ir 'lambda-ref)
     ;; lambda-ref = (lambda-ref fn-name free-var-offsets)
     ;; Returns the function name as a symbol for lookup in funcall
     (cadr ir))
    ((has-tag ir 'lambda-ir)
     ;; lambda-ir = (lambda-ir params body free-vars free-var-offsets)
     ;; Create a closure: capture the values of free variables using offsets
     (let* ((params (cadr ir))
            (body (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth 4 ir)))  ; The offsets computed at compile time
       ;; Capture current values using the pre-computed offsets
       (labels ((capture (offs acc)
                  (if (null offs)
                      (reverse acc)
                      (let ((val (nth (car offs) env)))
                        (capture (cdr offs) (cons val acc))))))
         ;; Return: (:closure params body free-vars captured-vals)
         (list :closure params body free-vars (capture free-offsets nil)))))
    ((has-tag ir 'dotimes-ir)
     ;; dotimes-ir = (dotimes-ir var count-ir body-ir result-ir compile-env)
     (let* ((count-ir (caddr ir))
            (body-ir (cadddr ir))
            (result-ir (nth 4 ir))
            (count (eval-ir-with-fns count-ir env fenv)))
       ;; Iterative loop
       (labels ((iter (i)
                  (if (>= i count)
                      (eval-ir-with-fns result-ir (append env (list i)) fenv)
                      (progn
                        (eval-ir-with-fns body-ir (append env (list i)) fenv)
                        (iter (+ i 1))))))
         (iter 0))))
    ((has-tag ir 'dolist-ir)
     ;; dolist-ir = (dolist-ir var list-ir body-ir result-ir compile-env)
     (let* ((list-ir (caddr ir))
            (body-ir (cadddr ir))
            (result-ir (nth 4 ir))
            (lst (eval-ir-with-fns list-ir env fenv)))
       ;; Iterative loop over list
       (labels ((iter (remaining)
                  (if (null remaining)
                      (eval-ir-with-fns result-ir (append env (list nil)) fenv)
                      (let ((elem (car remaining)))
                        (eval-ir-with-fns body-ir (append env (list elem)) fenv)
                        (iter (cdr remaining))))))
         (iter lst))))
    ;; setq-ir - assign to variable in env
    ((has-tag ir 'setq-ir)
     ;; setq-ir = (setq-ir offset value-ir)
     ;; Note: env is immutable in evaluator, so we simulate via setf on nth
     (let* ((off (cadr ir))
            (val (eval-ir-with-fns (caddr ir) env fenv)))
       (setf (nth off env) val)
       val))
    ;; setcar-ir - mutate car of cons cell
    ((has-tag ir 'setcar-ir)
     ;; setcar-ir = (setcar-ir cons-ir value-ir)
     (let* ((cell (eval-ir-with-fns (cadr ir) env fenv))
            (val (eval-ir-with-fns (caddr ir) env fenv)))
       (setf (car cell) val)
       val))
    ;; setcdr-ir - mutate cdr of cons cell
    ((has-tag ir 'setcdr-ir)
     ;; setcdr-ir = (setcdr-ir cons-ir value-ir)
     (let* ((cell (eval-ir-with-fns (cadr ir) env fenv))
            (val (eval-ir-with-fns (caddr ir) env fenv)))
       (setf (cdr cell) val)
       val))
    ;; read-file-ir - read entire file as string
    ((has-tag ir 'read-file-ir)
     (let ((path (eval-ir-with-fns (cadr ir) env fenv)))
       (with-open-file (in path :direction :input)
         (let ((contents (make-string (file-length in))))
           (read-sequence contents in)
           contents))))
    ;; write-file-ir - write string to file
    ((has-tag ir 'write-file-ir)
     (let ((path (eval-ir-with-fns (cadr ir) env fenv))
           (contents (eval-ir-with-fns (caddr ir) env fenv)))
       (with-open-file (out path :direction :output :if-exists :supersede)
         (write-string contents out))
       contents))
    ;; println-ir - print value with newline
    ((has-tag ir 'println-ir)
     (let ((val (eval-ir-with-fns (cadr ir) env fenv)))
       (format t "~A~%" val)
       val))
    ;; string-length-ir - get length of string
    ((has-tag ir 'string-length-ir)
     (length (eval-ir-with-fns (cadr ir) env fenv)))
    ;; string-ref-ir - get character at index
    ((has-tag ir 'string-ref-ir)
     (char-code (char (eval-ir-with-fns (cadr ir) env fenv)
                      (eval-ir-with-fns (caddr ir) env fenv))))
    ;; system-ir - execute shell command (evaluator uses SBCL's system)
    ((has-tag ir 'system-ir)
     (let ((cmd (eval-ir-with-fns (cadr ir) env fenv)))
       #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output t :wait t)
       0))
    ;; string-equal-ir - compare two strings
    ((has-tag ir 'string-equal-ir)
     (let ((s1 (eval-ir-with-fns (cadr ir) env fenv))
           (s2 (eval-ir-with-fns (caddr ir) env fenv)))
       (if (string= s1 s2) 1 0)))
    ;; make-vector-ir - allocate vector
    ((has-tag ir 'make-vector-ir)
     (make-array (eval-ir-with-fns (cadr ir) env fenv)))
    ;; vector-set-ir - set element at index
    ((has-tag ir 'vector-set-ir)
     (let ((vec (eval-ir-with-fns (cadr ir) env fenv))
           (idx (eval-ir-with-fns (caddr ir) env fenv))
           (val (eval-ir-with-fns (cadddr ir) env fenv)))
       (setf (aref vec idx) val)
       val))
    ;; vector-ref-ir - get element at index
    ((has-tag ir 'vector-ref-ir)
     (let ((vec (eval-ir-with-fns (cadr ir) env fenv))
           (idx (eval-ir-with-fns (caddr ir) env fenv)))
       (aref vec idx)))
    ;; buffer-byte-ref-ir - get raw byte at index (for evaluator, same as aref)
    ((has-tag ir 'buffer-byte-ref-ir)
     (let ((vec (eval-ir-with-fns (cadr ir) env fenv))
           (idx (eval-ir-with-fns (caddr ir) env fenv)))
       (aref vec idx)))
    ;; make-string-from-vector-ir - convert vector to string
    ((has-tag ir 'make-string-from-vector-ir)
     (let ((vec (eval-ir-with-fns (cadr ir) env fenv)))
       (map 'string #'code-char vec)))
    ;; buffer-to-string-ir - convert raw byte buffer to string
    ((has-tag ir 'buffer-to-string-ir)
     (let ((buf (eval-ir-with-fns (cadr ir) env fenv))
           (len (eval-ir-with-fns (caddr ir) env fenv)))
       ;; For evaluator (SBCL), assume buf is a vector of bytes
       (map 'string #'code-char (subseq buf 0 len))))
    ;; make-symbol-from-string-ir - intern string as symbol
    ((has-tag ir 'make-symbol-from-string-ir)
     (let ((str (eval-ir-with-fns (cadr ir) env fenv)))
       (intern str)))
    ;; symbol-name-ir - get symbol's name string
    ((has-tag ir 'symbol-name-ir)
     (let ((sym (eval-ir-with-fns (cadr ir) env fenv)))
       (symbol-name sym)))
    ;; write-bytes-ir - write vector of bytes to file (for evaluator, use SBCL)
    ((has-tag ir 'write-bytes-ir)
     (let ((path (eval-ir-with-fns (cadr ir) env fenv))
           (vec (eval-ir-with-fns (caddr ir) env fenv)))
       (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
         (dotimes (i (length vec))
           (write-byte (aref vec i) out)))
       0))
    ;; nthcdr-ir - get nth cdr of list
    ((has-tag ir 'nthcdr-ir)
     ;; nthcdr-ir = (nthcdr-ir n-ir list-ir)
     (let* ((n (eval-ir-with-fns (cadr ir) env fenv))
            (lst (eval-ir-with-fns (caddr ir) env fenv)))
       (labels ((drop (cnt l)
                  (if (or (<= cnt 0) (null l))
                      l
                      (drop (- cnt 1) (cdr l)))))
         (drop n lst))))
    ;; values-ir - return multiple values
    ((has-tag ir 'values-ir)
     ;; values-ir = (values-ir (ir1 ir2 ...))
     (let ((irs (cadr ir)))
       (if (null irs)
           nil  ; no values
           (if (null (cdr irs))
               ;; single value - just return it
               (eval-ir-with-fns (car irs) env fenv)
               ;; multiple values - return as list for evaluator
               (labels ((eval-all (vs acc)
                          (if (null vs)
                              (reverse acc)
                              (eval-all (cdr vs)
                                       (cons (eval-ir-with-fns (car vs) env fenv) acc)))))
                 (eval-all irs nil))))))
    ;; mvb-ir - multiple-value-bind
    ((has-tag ir 'mvb-ir)
     ;; mvb-ir = (mvb-ir vars form-ir nvars body-ir)
     (let* ((form-ir (caddr ir))
            (nvars (cadddr ir))
            (body-ir (nth 4 ir))
            (result (eval-ir-with-fns form-ir env fenv)))
       ;; Result may be single value or list of values
       (let ((vals (if (consp result)
                       result
                       (list result))))
         ;; Pad with nils if needed
         (labels ((pad (vs n acc)
                    (if (<= n 0)
                        (reverse acc)
                        (pad (cdr vs) (- n 1) (cons (car vs) acc)))))
           (let ((padded-vals (pad vals nvars nil)))
             (eval-ir-with-fns body-ir (append env padded-vals) fenv))))))
    (t 0)))

;;; ============================================================
;;; Part 7: Code Generator (codegen-*)
;;; ============================================================

(defun ir-may-call? (ir)
  "Returns t if evaluating IR might make function calls that could clobber x24.
   This is used to optimize away unnecessary x24 save/restore in binary ops."
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((has-tag ir 'lit) nil)
    ((has-tag ir 'nil-ir) nil)
    ((has-tag ir 'sym-lit) nil)
    ((has-tag ir 'str-lit) nil)
    ((has-tag ir 'var) nil)
    ;; Function calls definitely clobber x24
    ((has-tag ir 'call-fn) t)
    ((has-tag ir 'tail-call-fn) t)
    ((has-tag ir 'funcall-ir) t)
    ((has-tag ir 'call-closure) t)
    ;; Runtime calls also clobber x24
    ((has-tag ir 'runtime-call) t)
    ((has-tag ir 'format-call) t)
    ((has-tag ir 'gensym-call) t)
    ((has-tag ir 'open-file-call) t)
    ((has-tag ir 'close-file-call) t)
    ((has-tag ir 'read-line-call) t)
    ((has-tag ir 'write-string-call) t)
    ((has-tag ir 'read-file-call) t)
    ((has-tag ir 'write-file-call) t)
    ((has-tag ir 'values-call) t)
    ((has-tag ir 'values-get-call) t)
    ((has-tag ir 'values-count-call) t)
    ((has-tag ir 'print-call) t)
    ((has-tag ir 'profile-time-call) t)
    ((has-tag ir 'sys-write-call) t)
    ((has-tag ir 'sys-read-call) t)
    ((has-tag ir 'sys-open-call) t)
    ((has-tag ir 'sys-close-call) t)
    ;; Binary/unary ops: check children
    ((has-tag ir 'add) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'sub) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'mul) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'div) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'mod-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'band) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'bor) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'bxor) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'bsh) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'cmp-eq) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'cmp-lt) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'cmp-gt) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'cmp-le) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'cmp-ge) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ;; Unary ops
    ((has-tag ir 'bnot) (ir-may-call? (cadr ir)))
    ((has-tag ir 'neg) (ir-may-call? (cadr ir)))
    ;; Cons/car/cdr: check children
    ((has-tag ir 'cons-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'car-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'cdr-ir) (ir-may-call? (cadr ir)))
    ;; Control flow: check all branches
    ((has-tag ir 'if-ir)
     (or (ir-may-call? (cadr ir))
         (ir-may-call? (caddr ir))
         (ir-may-call? (cadddr ir))))
    ;; Progn: check all forms
    ((has-tag ir 'progn-ir)
     (some #'ir-may-call? (cdr ir)))
    ;; Let: check bindings and body
    ((has-tag ir 'let-ir)
     (let ((bindings (cadr ir))
           (body (caddr ir)))
       (or (some #'ir-may-call? bindings)
           (ir-may-call? body))))
    ;; Vector/string operations are simple (inline)
    ((has-tag ir 'make-vector-call) (ir-may-call? (cadr ir)))
    ((has-tag ir 'vector-set-ir) (or (ir-may-call? (cadr ir))
                                         (ir-may-call? (caddr ir))
                                         (ir-may-call? (cadddr ir))))
    ((has-tag ir 'vector-ref-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'vector-length-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'string-length-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'string-ref-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ;; Type predicates are simple
    ((has-tag ir 'consp-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'null-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'numberp-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'symbolp-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'stringp-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'vectorp-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'eq-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ;; Lambda-ref is just loading an address
    ((has-tag ir 'lambda-ref) nil)
    ;; Setq: check value
    ((has-tag ir 'setq-ir) (ir-may-call? (caddr ir)))
    ((has-tag ir 'setcar-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ((has-tag ir 'setcdr-ir) (or (ir-may-call? (cadr ir)) (ir-may-call? (caddr ir))))
    ;; Loop constructs
    ((has-tag ir 'dotimes-ir) t)
    ((has-tag ir 'dolist-ir) t)
    ;; Self-TCO loop constructs: check body for calls
    ((has-tag ir 'loop-ir) (ir-may-call? (cadr ir)))
    ((has-tag ir 'continue-ir) (some #'ir-may-call? (cadr ir)))
    ;; Default: assume it might call to be safe
    (t t)))

(defun ir-is-simple? (ir)
  "Returns t if IR is simple (var or lit) and doesn't use any registers."
  (or (has-tag ir 'var)
      (has-tag ir 'lit)
      (has-tag ir 'nil-ir)))

(defun cmp-result-to-bool (cond-code)
  "Generate code to convert comparison result to t (0x10) or nil (0x06).
   Input: condition code (e.g., (cond-eq), (cond-lt))
   Output: code that sets x0 = 0x10 (true) or 0x06 (nil)"
  (append-all (list (arm64:cset 0 cond-code)
                    (arm64:lsl 0 0 4 :imm t)   ; x0 = 0x00 or 0x10
                    (arm64:cbnz 0 2)            ; if true (0x10), skip movz
                    (arm64:movz 0 6))))         ; false: x0 = nil (6)

(defun codegen-binop (left-ir right-ir op-instrs rtaddrs fnoffs td)
  "Generate code for binary operation with register-based temps.
   Uses temp registers when safe, falls back to stack when needed."
  (let* ((nd (+ td 1))
         (left-simple (ir-is-simple? left-ir))
         (right-simple (ir-is-simple? right-ir))
         (right-may-call (ir-may-call? right-ir))
         (left-may-call (ir-may-call? left-ir)))
    (cond
      ;; Optimal case: both operands are simple (var/lit)
      ;; Use temp register, no spill needed
      ((and left-simple right-simple)
       (let ((lc (codegen left-ir rtaddrs fnoffs nd))
             (rc (codegen right-ir rtaddrs fnoffs nd)))
         (append-all
          (list lc                          ; eval left -> x0
                (save-temp td)           ; save left in temp reg/slot
                rc                          ; eval right -> x0
                (arm64:mov 1 0)            ; x1 = right
                (load-temp 0 td)         ; x0 = left
                op-instrs))))
      ;; Left may call - need stack spill (caller-saved regs clobbered)
      (left-may-call
       (let* ((_xs (temp-slot td))
              (ls (temp-slot (+ td 1)))
              (lc (codegen left-ir rtaddrs fnoffs (+ td 2)))
              (rc (codegen right-ir rtaddrs fnoffs (+ td 2))))
         (append-all
          (list (arm64:str 24 31 :offset _xs)   ; save x24
                lc                          ; eval left -> x0
                (arm64:str 0 31 :offset ls)    ; save left value (must use stack)
                (arm64:ldr 24 31 :offset _xs)   ; restore x24
                rc                          ; eval right -> x0
                (arm64:mov 1 0)           ; x1 = right
                (arm64:ldr 0 31 :offset ls)    ; x0 = left
                op-instrs))))
      ;; Left doesn't call but right does - still need stack for left
      (right-may-call
       (let* ((ls (temp-slot td))
              (lc (codegen left-ir rtaddrs fnoffs (+ td 1)))
              (rc (codegen right-ir rtaddrs fnoffs (+ td 2))))  ; FIX: use td+2 to avoid clobbering temp[td]
         (append-all
          (list lc                          ; eval left -> x0
                (arm64:str 0 31 :offset ls)    ; save left value at temp[td]
                rc                          ; eval right -> x0 (uses temp[td+2]+ only)
                (arm64:mov 1 0)           ; x1 = right
                (arm64:ldr 0 31 :offset ls)    ; x0 = left
                op-instrs))))
      ;; Neither calls - can use temp registers
      (t
       (let ((lc (codegen left-ir rtaddrs fnoffs nd))
             (rc (codegen right-ir rtaddrs fnoffs nd)))
         (append-all
          (list lc                          ; eval left -> x0
                (save-temp td)           ; save left in temp reg
                rc                          ; eval right -> x0
                (arm64:mov 1 0)            ; x1 = right
                (load-temp 0 td)         ; x0 = left
                op-instrs)))))))

(defun codegen (ir rtaddrs fnoffs td)
  (cond
    ((has-tag ir 'lit)
     (let* ((v (cadr ir))
            (tg (ash v 4)))
       (if (and (>= tg 0) (< tg #x10000))
           (arm64:movz 0 tg)
           (load-addr 0 tg))))
    ((has-tag ir 'nil-ir)
     ;; nil is represented as 0x06 (tag 6) - distinct from fixnum 0
     (arm64:movz 0 6))
    ((has-tag ir 'sym-lit)
     ;; Symbol literal: use compile-time symbol table
     ;; Each unique symbol gets a unique ID, tagged with symbol tag (2)
     ;; Tagged value = (ID << 4) | 2
     (let* ((name (cadr ir))
            (id (intern-symbol name))
            (tagged (logior (ash id 4) 2)))  ; tag 2 = symbol
       (if (< tagged #x10000)
           (arm64:movz 0 tagged)
           (load-addr 0 tagged))))
    ((has-tag ir 'str-lit)
     ;; String literal: build string inline on heap using x28 bump pointer
     (let* ((s (cadr ir))
            (chars (string-to-char-codes s)))
       (codegen-string-inline chars)))
    ((has-tag ir 'var)
     (let* ((off (cadr ir))
            (off8 (* off 8))
            (i1 (arm64:sub 1 20 off8 :imm t))
            (i2 (arm64:ldr 0 1 :offset 0)))
       (append-all (list i1 i2))))
    ((has-tag ir 'get-tag)
     (let* ((ac (codegen (cadr ir) rtaddrs fnoffs td))
            (i1 (arm64:movz 1 #xF))
            (i2 (arm64:and* 0 0 1))
            (i3 (arm64:lsl 0 0 4 :imm t)))
       (append-all (list ac i1 i2 i3))))
    ((has-tag ir 'add)
     ;; Fast path: (add (var n) (lit k)) or (add (lit k) (var n)) -> single ADD imm
     (let ((left (cadr ir))
           (right (caddr ir)))
       (cond
         ;; (add var lit) where lit fits in 12-bit immediate
         ((and (has-tag left 'var) (has-tag right 'lit)
               (< (ash (cadr right) 4) #x1000))
          (let ((var-code (codegen left rtaddrs fnoffs td))
                (imm (ash (cadr right) 4)))
            (append var-code (arm64:add 0 0 imm :imm t))))
         ;; (add lit var) - swap operands
         ((and (has-tag left 'lit) (has-tag right 'var)
               (< (ash (cadr left) 4) #x1000))
          (let ((var-code (codegen right rtaddrs fnoffs td))
                (imm (ash (cadr left) 4)))
            (append var-code (arm64:add 0 0 imm :imm t))))
         ;; General case
         (t (codegen-binop left right (arm64:add 0 0 1) rtaddrs fnoffs td)))))
    ((has-tag ir 'sub)
     ;; Fast path: (sub (var n) (lit k)) -> single SUB imm
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (has-tag left 'var) (has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code (arm64:sub 0 0 imm :imm t)))
           (codegen-binop left right (arm64:sub 0 0 1) rtaddrs fnoffs td))))
    ((has-tag ir 'mul)
     ;; Optimized multiplication: untag only ONE operand
     ;; (a<<4) * (b>>4) = (a*b)<<4 -- correctly tagged result!
     ;; Saves 2 instructions vs untagging both and retagging
     (codegen-binop (cadr ir) (caddr ir)
                       (append-all (list (arm64:lsr 1 1 4 :imm t)    ; untag right only
                                            (arm64:mul 0 0 1)))  ; (left<<4) * right = result<<4
                       rtaddrs fnoffs td))
    ((has-tag ir 'band)
     (codegen-binop (cadr ir) (caddr ir) (arm64:and* 0 0 1) rtaddrs fnoffs td))
    ((has-tag ir 'bor)
     (codegen-binop (cadr ir) (caddr ir) (arm64:orr 0 0 1) rtaddrs fnoffs td))
    ((has-tag ir 'bxor)
     (codegen-binop (cadr ir) (caddr ir) (arm64:eor 0 0 1) rtaddrs fnoffs td))
    ((has-tag ir 'bsh)
     ;; Shift: optimized x24 save/restore
     (let* ((val-ir (cadr ir))
            (amt-ir (caddr ir))
            (_xs (temp-slot td))
            (vs (temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (codegen val-ir rtaddrs fnoffs nd))
            (ac (codegen amt-ir rtaddrs fnoffs nd))
            (may-call (ir-may-call? val-ir))
            (shift-code (append-all
                         (list (arm64:asr 1 0 4 :imm t)
                               (arm64:ldr 0 31 :offset vs)
                               (arm64:cmp 1 0 :imm t)
                               (arm64:b.ge (ash 16 -2))
                               (arm64:neg 2 1)
                               (arm64:asr 0 0 2)
                               (arm64:b (ash 8 -2))
                               (arm64:lsl 0 0 1)
                               (arm64:lsl 0 0 4 :imm t)))))
       (if may-call
           (append-all (list (arm64:str 24 31 :offset _xs) vc (arm64:lsr 0 0 4 :imm t)
                                (arm64:str 0 31 :offset vs) (arm64:ldr 24 31 :offset _xs)
                                ac shift-code))
           (append-all (list vc (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset vs)
                                ac shift-code)))))
    ((has-tag ir 'cmp-eq)
     (codegen-binop (cadr ir) (caddr ir)
                       (append (arm64:cmp 0 1)
                               (cmp-result-to-bool (cond-eq)))
                       rtaddrs fnoffs td))
    ((has-tag ir 'cmp-lt)
     ;; Fast path: (cmp-lt (var n) (lit k)) -> CMP x0, #imm; CSET
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (has-tag left 'var) (has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code
                    (arm64:cmp 0 imm :imm t)
                    (cmp-result-to-bool (cond-lt))))
           (codegen-binop left right
                             (append (arm64:cmp 0 1)
                                     (cmp-result-to-bool (cond-lt)))
                             rtaddrs fnoffs td))))
    ((has-tag ir 'cmp-gt)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (has-tag left 'var) (has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code
                    (arm64:cmp 0 imm :imm t)
                    (cmp-result-to-bool (cond-gt))))
           (codegen-binop left right
                             (append (arm64:cmp 0 1)
                                     (cmp-result-to-bool (cond-gt)))
                             rtaddrs fnoffs td))))
    ((has-tag ir 'cmp-le)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (has-tag left 'var) (has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code
                    (arm64:cmp 0 imm :imm t)
                    (cmp-result-to-bool (cond-le))))
           (codegen-binop left right
                             (append (arm64:cmp 0 1)
                                     (cmp-result-to-bool (cond-le)))
                             rtaddrs fnoffs td))))
    ((has-tag ir 'cmp-ge)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (has-tag left 'var) (has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code
                    (arm64:cmp 0 imm :imm t)
                    (cmp-result-to-bool (cond-ge))))
           (codegen-binop left right
                             (append (arm64:cmp 0 1)
                                     (cmp-result-to-bool (cond-ge)))
                             rtaddrs fnoffs td))))
    ((has-tag ir 'cons-ir)
     ;; Inline cons: allocate 16 bytes from heap (x28), store car/cdr, return tagged ptr
     ;; x28 is the heap bump pointer, initialized at startup
     ;; Cons cell: [car at offset 0, cdr at offset 8], tagged with 1
     (let* ((car-ir (cadr ir))
            (cdr-ir (caddr ir))
            (_xs (temp-slot td))
            (cs (temp-slot (+ td 1)))
            (nd (+ td 2))
            (cc (codegen car-ir rtaddrs fnoffs nd))
            (dc (codegen cdr-ir rtaddrs fnoffs nd))
            (may-call (ir-may-call? car-ir))
            (alloc-code (append-all
                         (list (arm64:mov 1 0)             ; x1 = cdr value
                               (arm64:ldr 0 31 :offset cs)      ; x0 = car value
                               (arm64:str 0 28 :offset 0)       ; [x28+0] = car
                               (arm64:str 1 28 :offset 8)       ; [x28+8] = cdr
                               (arm64:mov 0 28)            ; x0 = untagged ptr
                               (arm64:add 28 28 16 :imm t)        ; bump heap by 16
                               ;; GC trigger check: if x28 >= from_end, call GC
                               (arm64:ldr 9 27 :offset 16)       ; x9 = from_end [x27+16]
                               (arm64:cmp 28 9)                  ; compare x28, from_end
                               (arm64:b.lo 2)                    ; skip if x28 < from_end
                               (list '(:call-fn GC-COLLECT))     ; bl gc_collect
                               (arm64:movz 1 1)                ; x1 = 1
                               (arm64:orr 0 0 1)))))       ; x0 = ptr | 1
       (if may-call
           (append-all (list (arm64:str 24 31 :offset _xs) cc (arm64:str 0 31 :offset cs)
                                (arm64:ldr 24 31 :offset _xs) dc alloc-code))
           (append-all (list cc (arm64:str 0 31 :offset cs) dc alloc-code)))))
    ((has-tag ir 'car-ir)
     ;; Inline car: clear tag bits, load from offset 0
     ;; (car nil) returns nil - check for nil first
     (let ((arg-ir (cadr ir)))
       (let ((ac (codegen arg-ir rtaddrs fnoffs td)))
         (append-all
          (list ac
                ;; Check for nil: if x0 == 0, skip load (return 0)
                (arm64:cbz 0 7)                ; if x0 == 0, skip 7 instructions
                ;; Clear low 4 bits to get pointer
                (arm64:movz 1 #xFFF0)                ; x1 = mask (keep upper bits)
                (arm64:movk 1 #xFFFF :lsl 16)  ; complete mask
                (arm64:movk 1 #xFFFF :lsl 32)
                (arm64:movk 1 #xFFFF :lsl 48)
                (arm64:and* 0 0 1)                ; x0 = ptr with tag cleared
                (arm64:ldr 0 0 :offset 0))))))        ; x0 = [ptr+0] = car
    ((has-tag ir 'cdr-ir)
     ;; Inline cdr: clear tag bits, load from offset 8
     ;; (cdr nil) returns nil - check for nil first
     (let ((arg-ir (cadr ir)))
       (let ((ac (codegen arg-ir rtaddrs fnoffs td)))
         (append-all
          (list ac
                ;; Check for nil: if x0 == 0, skip load (return 0)
                (arm64:cbz 0 7)                ; if x0 == 0, skip 7 instructions
                ;; Clear low 4 bits to get pointer
                (arm64:movz 1 #xFFF0)                ; x1 = mask (keep upper bits)
                (arm64:movk 1 #xFFFF :lsl 16)  ; complete mask
                (arm64:movk 1 #xFFFF :lsl 32)
                (arm64:movk 1 #xFFFF :lsl 48)
                (arm64:and* 0 0 1)                ; x0 = ptr with tag cleared
                (arm64:ldr 0 0 :offset 8))))))
    ;; setq-ir - assign to variable
    ((has-tag ir 'setq-ir)
     ;; setq-ir = (setq-ir offset value-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (vc (codegen val-ir rtaddrs fnoffs td))
            (off8 (* off 8))
            (s1 (arm64:sub 1 20 off8 :imm t))
            (s2 (arm64:str 0 1 :offset 0)))
       (append-all (list vc s1 s2))))
    ;; setcar-ir - mutate car of cons cell
    ((has-tag ir 'setcar-ir)
     ;; setcar-ir = (setcar-ir cons-ir value-ir)
     ;; Inline implementation: clear tag, store value at offset 0
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot td))
            (val-slot (temp-slot (+ td 1)))
            (nd (+ td 2))
            (cons-code (codegen cons-ir rtaddrs fnoffs nd))
            (save-cons (arm64:str 0 31 :offset cons-slot))
            (val-code (codegen val-ir rtaddrs fnoffs nd))
            (save-val (arm64:str 0 31 :offset val-slot))
            ;; Get cons pointer back and clear tag
            (load-cons (arm64:ldr 1 31 :offset cons-slot))
            ;; Clear low 4 bits to get raw pointer
            (clear-tag (append-all
                        (list (arm64:movz 9 #xFFF0)
                              (arm64:movk 9 #xFFFF :lsl 16)
                              (arm64:movk 9 #xFFFF :lsl 32)
                              (arm64:movk 9 #xFFFF :lsl 48)
                              (arm64:and* 1 1 9))))
            ;; Get value back
            (load-val (arm64:ldr 0 31 :offset val-slot))
            ;; Store value at car position
            (store-car (arm64:str 0 1 :offset 0)))
       (append-all (list cons-code save-cons val-code save-val
                         load-cons clear-tag load-val store-car))))
    ;; setcdr-ir - mutate cdr of cons cell
    ((has-tag ir 'setcdr-ir)
     ;; setcdr-ir = (setcdr-ir cons-ir value-ir)
     ;; Inline implementation: clear tag, store value at offset 8
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot td))
            (val-slot (temp-slot (+ td 1)))
            (nd (+ td 2))
            (cons-code (codegen cons-ir rtaddrs fnoffs nd))
            (save-cons (arm64:str 0 31 :offset cons-slot))
            (val-code (codegen val-ir rtaddrs fnoffs nd))
            (save-val (arm64:str 0 31 :offset val-slot))
            ;; Get cons pointer back and clear tag
            (load-cons (arm64:ldr 1 31 :offset cons-slot))
            ;; Clear low 4 bits to get raw pointer
            (clear-tag (append-all
                        (list (arm64:movz 9 #xFFF0)
                              (arm64:movk 9 #xFFFF :lsl 16)
                              (arm64:movk 9 #xFFFF :lsl 32)
                              (arm64:movk 9 #xFFFF :lsl 48)
                              (arm64:and* 1 1 9))))
            ;; Get value back
            (load-val (arm64:ldr 0 31 :offset val-slot))
            ;; Store value at cdr position (offset 8)
            (store-cdr (arm64:str 0 1 :offset 8)))
       (append-all (list cons-code save-cons val-code save-val
                         load-cons clear-tag load-val store-cdr))))
    ;; read-file-ir - read entire file as string
    ((has-tag ir 'read-file-ir)
     ;; read-file-ir = (read-file-ir path-ir)
     ;; Runtime index 46 = habu_read_file at offset 368
     (let* ((path-ir (cadr ir))
            (pc (codegen path-ir rtaddrs fnoffs td))
            (lf (arm64:ldr 9 19 :offset 368))
            (bl (arm64:blr 9)))
       (append-all (list pc lf bl))))
    ;; write-file-ir - write string to file
    ((has-tag ir 'write-file-ir)
     ;; write-file-ir = (write-file-ir path-ir contents-ir)
     ;; Runtime index 47 = habu_write_file at offset 376
     (let* ((path-ir (cadr ir))
            (contents-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (pc (codegen path-ir rtaddrs fnoffs nd))
            (sp (arm64:str 0 31 :offset _xs))
            (cc (codegen contents-ir rtaddrs fnoffs nd))
            (m1 (arm64:mov 1 0))
            (lp (arm64:ldr 0 31 :offset _xs))
            (lf (arm64:ldr 9 19 :offset 376))
            (bl (arm64:blr 9)))
       (append-all (list pc sp cc m1 lp lf bl))))
    ;; println-ir - print value with newline
    ((has-tag ir 'println-ir)
     ;; println-ir = (println-ir value-ir)
     ;; Runtime index 49 = habu_println_value at offset 392
     (let* ((val-ir (cadr ir))
            (vc (codegen val-ir rtaddrs fnoffs td))
            (lf (arm64:ldr 9 19 :offset 392))
            (bl (arm64:blr 9)))
       (append-all (list vc lf bl))))
    ;; string-length-ir - get length of string (inline)
    ((has-tag ir 'string-length-ir)
     ;; string-length-ir = (string-length-ir str-ir)
     ;; String layout: [length (8 bytes)] [char data]
     ;; Clear tag, load length, tag as fixnum
     (let* ((str-ir (cadr ir))
            (sc (codegen str-ir rtaddrs fnoffs td)))
       (append-all
        (list sc
              ;; Clear low 4 bits to get pointer (same approach as car-ir)
              (arm64:movz 1 #xFFF0)              ; x1 = mask (keep upper bits)
              (arm64:movk 1 #xFFFF :lsl 16)  ; complete mask
              (arm64:movk 1 #xFFFF :lsl 32)
              (arm64:movk 1 #xFFFF :lsl 48)
              (arm64:and* 0 0 1)              ; x0 = str_ptr (untagged)
              ;; Load length from [x0+0]
              (arm64:ldr 0 0 :offset 0)           ; x0 = raw length
              ;; Tag as fixnum: x0 = x0 << 4
              (arm64:lsl 0 0 4 :imm t)))))
    ;; string-ref-ir - get character at index (inline)
    ((has-tag ir 'string-ref-ir)
     ;; string-ref-ir = (string-ref-ir str-ir idx-ir)
     ;; String layout: [length (8 bytes)] [char data]
     ;; Address = (str & ~0xF) + 8 + (idx >> 4)
     (let* ((str-ir (cadr ir))
            (idx-ir (caddr ir))
            (_xs (temp-slot td))
            (is (temp-slot (+ td 1)))
            (nd (+ td 2))
            (sc (codegen str-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset _xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd))
            (si (arm64:str 0 31 :offset is)))
       ;; After codegen: idx saved at [sp+is], str at [sp+_xs]
       (append-all
        (list sc sv ic si
              ;; Load str -> x1
              (arm64:ldr 1 31 :offset _xs)         ; x1 = str (tagged)
              ;; Clear tag: x1 = x1 & ~0xF (same approach as car-ir)
              (arm64:movz 2 #xFFF0)              ; x2 = mask (keep upper bits)
              (arm64:movk 2 #xFFFF :lsl 16)  ; complete mask
              (arm64:movk 2 #xFFFF :lsl 32)
              (arm64:movk 2 #xFFFF :lsl 48)
              (arm64:and* 1 1 2)              ; x1 = str_ptr (untagged)
              ;; Load idx -> x0
              (arm64:ldr 0 31 :offset is)         ; x0 = idx (tagged)
              ;; Calculate offset: x0 = (idx >> 4) + 8
              (arm64:lsr 0 0 4 :imm t)              ; x0 = untagged idx
              (arm64:add 0 0 8 :imm t)              ; x0 = offset = 8 + idx
              ;; Load byte from str_ptr + offset
              (arm64:ldrb 0 1 0 :reg t)             ; x0 = byte value (zero-extended)
              ;; Tag as fixnum: x0 = x0 << 4
              (arm64:lsl 0 0 4 :imm t)))))
    ;; system-ir - execute shell command
    ((has-tag ir 'system-ir)
     ;; system-ir = (system-ir cmd-ir)
     ;; Runtime index 51 = habu_system at offset 408
     (let* ((cmd-ir (cadr ir))
            (cc (codegen cmd-ir rtaddrs fnoffs td))
            (lf (arm64:ldr 9 19 :offset 408))
            (bl (arm64:blr 9)))
       (append-all (list cc lf bl))))
    ;; string-equal-ir - compare two strings (inline)
    ((has-tag ir 'string-equal-ir)
     ;; string-equal-ir = (string-equal-ir str1-ir str2-ir)
     ;; Inline implementation: compare lengths, then byte-by-byte
     ;; String layout: [length (8 bytes)][char data (n bytes)]
     ;; Returns: tagged fixnum 16 (true=1) or 0 (false)
     ;; Register usage:
     ;;   x0: result (0 or 16)
     ;;   x1: str1 base (untagged)
     ;;   x2: str2 base (untagged)
     ;;   x3: len1
     ;;   x4: len2 / loop counter
     ;;   x5: char from str1
     ;;   x6: char from str2
     (let* ((str1-ir (cadr ir))
            (str2-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (s1 (codegen str1-ir rtaddrs fnoffs nd))
            (sp (arm64:str 0 31 :offset _xs))
            (s2 (codegen str2-ir rtaddrs fnoffs nd)))
       (append-all
        (list s1 sp s2
              ;; x2 = str2 base (untagged)
              (arm64:and* 2 0 -16 :imm t)        ; x2 = str2 & ~0xF
              ;; x1 = str1 base (untagged)
              (arm64:ldr 0 31 :offset _xs)         ; x0 = str1 (tagged)
              (arm64:and* 1 0 -16 :imm t)        ; x1 = str1 & ~0xF
              ;; Load lengths
              (arm64:ldr 3 1 :offset 0)           ; x3 = len1
              (arm64:ldr 4 2 :offset 0)           ; x4 = len2
              ;; Compare lengths
              (arm64:cmp 3 4)                ; cmp len1, len2
              (arm64:b.ne (ash 56 -2))     ; if len1 != len2, jump to return_false (+14 instructions = 56 bytes)
              ;; Lengths equal, setup for loop
              ;; x1 = str1 data = x1 + 8
              (arm64:add 1 1 8 :imm t)              ; x1 = str1 data start
              ;; x2 = str2 data = x2 + 8
              (arm64:add 2 2 8 :imm t)              ; x2 = str2 data start
              ;; x4 = loop counter = 0
              (arm64:movz 4 0)                   ; x4 = 0
              ;; loop_start: (offset here, instruction 5)
              (arm64:cmp 4 3)                ; cmp counter, len
              (arm64:b.ge (ash 28 -2))     ; if counter >= len, jump to return_true (+7 instructions = 28 bytes)
              ;; Load bytes from both strings
              (arm64:ldrb 5 1 4 :reg t)             ; x5 = str1[counter]
              (arm64:ldrb 6 2 4 :reg t)             ; x6 = str2[counter]
              ;; Compare bytes
              (arm64:cmp 5 6)                ; cmp char1, char2
              (arm64:b.ne (ash 20 -2))     ; if char1 != char2, jump to return_false (+5 instructions = 20 bytes)
              ;; Increment counter
              (arm64:add 4 4 1 :imm t)              ; x4++
              ;; Loop back to cmp at instruction 5
              (arm64:b (ash -24 -2))               ; back 6 instructions = -24 bytes
              ;; return_true: (instruction 13)
              (arm64:movz 0 16)                  ; x0 = 16 (tagged 1)
              (arm64:b (ash 8 -2))                 ; skip return_false (+2 instructions = 8 bytes)
              ;; return_false: (instruction 15)
              (arm64:movz 0 0)))))
    ;; make-vector-ir - allocate vector (inline)
    ((has-tag ir 'make-vector-ir)
     ;; make-vector-ir = (make-vector-ir size-ir)
     ;; Inline allocation: size in x0 is tagged fixnum
     ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
     ;; Total size = 8 + (untagged_size * 8), rounded to 16 for tagging
     (let* ((size-ir (cadr ir))
            (sc (codegen size-ir rtaddrs fnoffs td)))
       (append-all
        (list sc
              ;; Store untagged length at [x28+0]
              (arm64:lsr 1 0 4 :imm t)           ; x1 = untagged length
              (arm64:str 1 28 :offset 0)       ; [x28+0] = length
              ;; Calculate allocation size: 8 + (x0 >> 1)
              (arm64:lsr 1 0 1 :imm t)           ; x1 = x0 >> 1 = untagged_size * 8
              (arm64:add 1 1 8 :imm t)           ; x1 = 8 + data_size = total size
              ;; Round to 16-byte alignment: (x1 + 15) & ~15
              (arm64:add 1 1 15 :imm t)          ; x1 = total + 15
              (arm64:and* 1 1 -16 :imm t)     ; x1 = x1 & ~15 (clear low 4 bits)
              ;; Return tagged pointer, bump heap
              (arm64:mov 0 28)            ; x0 = current heap ptr
              (arm64:add 28 28 1)         ; x28 += total size (now 16-aligned)
              ;; GC trigger check: if x28 >= from_end, call GC
              (arm64:ldr 9 27 :offset 16)       ; x9 = from_end [x27+16]
              (arm64:cmp 28 9)                  ; compare x28, from_end
              (arm64:b.lo 2)                    ; skip if x28 < from_end
              (list '(:call-fn GC-COLLECT))    ; bl gc_collect
              ;; Tag with vector tag (0x3)
              (arm64:movz 1 3)
              (arm64:orr 0 0 1)))))
    ;; vector-set-ir - set element at index (inline)
    ((has-tag ir 'vector-set-ir)
     ;; vector-set-ir = (vector-set-ir vec-ir idx-ir val-ir)
     ;; Inline store: compute address and store directly
     ;; Vector layout: [length (8 bytes)] [data[0] ... data[n-1]]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4) * 8 = (vec & ~0xF) + 8 + (idx >> 1)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (val-ir (cadddr ir))
            (_xs (temp-slot td))
            (xs2 (temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset _xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd))
            (si (arm64:str 0 31 :offset xs2))
            (vlc (codegen val-ir rtaddrs fnoffs nd)))
       ;; After codegen: val in x0, vec at [sp+_xs], idx at [sp+xs2]
       (append-all
        (list vc sv ic si vlc
              ;; x0 = val, load vec -> x1, idx -> x2
              (arm64:ldr 1 31 :offset _xs)         ; x1 = vec (tagged)
              (arm64:ldr 2 31 :offset xs2)        ; x2 = idx (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (arm64:and* 1 1 -16 :imm t)        ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate offset: x2 = (idx >> 1) + 8
              (arm64:lsr 2 2 1 :imm t)              ; x2 = idx >> 1 = idx_untagged * 8
              (arm64:add 2 2 8 :imm t)              ; x2 = offset = 8 + idx_untagged * 8
              ;; Store val at vec_ptr + offset
              (arm64:add 1 1 2)              ; x1 = address
              (arm64:str 0 1 :offset 0)           ; [x1] = val
              ))))
    ;; vector-ref-ir - get element at index (inline)
    ((has-tag ir 'vector-ref-ir)
     ;; vector-ref-ir = (vector-ref-ir vec-ir idx-ir)
     ;; Inline load: compute address and load directly
     ;; Vector layout: [length (8 bytes)] [data[0] ... data[n-1]]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4) * 8 = (vec & ~0xF) + 8 + (idx >> 1)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset _xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+_xs]
       (append-all
        (list vc sv ic
              ;; x0 = idx, load vec -> x1
              (arm64:ldr 1 31 :offset _xs)         ; x1 = vec (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (arm64:and* 1 1 -16 :imm t)        ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate offset: x0 = (idx >> 1) + 8
              (arm64:lsr 0 0 1 :imm t)              ; x0 = idx >> 1 = idx_untagged * 8
              (arm64:add 0 0 8 :imm t)              ; x0 = offset = 8 + idx_untagged * 8
              ;; Load element from vec_ptr + offset
              (arm64:add 1 1 0)              ; x1 = address
              (arm64:ldr 0 1 :offset 0)           ; x0 = [x1] = element (already tagged)
              ))))
    ;; vector-length-ir - get vector size (inline)
    ((has-tag ir 'vector-length-ir)
     ;; vector-length-ir = (vector-length-ir vec-ir)
     ;; Vector layout: [length (8 bytes)][data...]
     ;; Just load the length field and tag it
     (let* ((vec-ir (cadr ir))
            (vc (codegen vec-ir rtaddrs fnoffs td)))
       (append-all
        (list vc
              ;; x0 = vec (tagged)
              ;; Clear tag: x0 = x0 & ~0xF
              (arm64:and* 0 0 -16 :imm t)        ; x0 = vec_ptr (untagged)
              ;; Load length: x0 = [x0+0]
              (arm64:ldr 0 0 :offset 0)           ; x0 = raw length (untagged integer)
              ;; Tag as fixnum: x0 = x0 << 4
              (arm64:lsl 0 0 4 :imm t)))))          ; x0 = tagged fixnum length
    ;; buffer-byte-ref-ir - get raw byte at index (inline)
    ((has-tag ir 'buffer-byte-ref-ir)
     ;; buffer-byte-ref-ir = (buffer-byte-ref-ir vec-ir idx-ir)
     ;; Reads a single byte from vector data area (used after sys-read fills buffer)
     ;; Vector layout: [length (8 bytes)][raw bytes...]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset _xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+_xs]
       (append-all
        (list vc sv ic
              ;; x0 = idx (tagged), load vec -> x1
              (arm64:ldr 1 31 :offset _xs)         ; x1 = vec (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (arm64:and* 1 1 -16 :imm t)        ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate byte offset: x0 = idx >> 4 (untag) + 8 (skip length)
              (arm64:lsr 0 0 4 :imm t)              ; x0 = idx_untagged (byte offset)
              (arm64:add 0 0 8 :imm t)              ; x0 = offset = 8 + byte_index
              ;; Load byte from vec_ptr + offset
              (arm64:add 1 1 0)              ; x1 = address
              (arm64:ldrb 0 1 0)          ; x0 = byte (zero-extended to 64-bit)
              ;; Tag as fixnum
              (arm64:lsl 0 0 4 :imm t)              ; x0 = tagged fixnum
              ))))
    ;; make-string-from-vector-ir - convert vector to string (inline)
    ((has-tag ir 'make-string-from-vector-ir)
     ;; make-string-from-vector-ir = (make-string-from-vector-ir vec-ir)
     ;; Inline implementation: allocate string on heap, copy bytes from vector
     ;; Vector layout: [length (8 bytes)][data[0] ... data[n-1]] (8-byte tagged elements)
     ;; String layout: [length (8 bytes)][char data (n bytes)]
     ;; Register usage:
     ;;   x0: tagged vec input, then tagged string result
     ;;   x1: untagged vec base
     ;;   x2: string data base (untagged string ptr + 8)
     ;;   x3: loop counter (0 to len-1)
     ;;   x4: temp for loading/storing bytes
     ;;   x5: length
     (let* ((vec-ir (cadr ir))
            (vc (codegen vec-ir rtaddrs fnoffs td)))
       (append-all
        (list vc
              ;; x1 = untagged vec base
              (arm64:and* 1 0 -16 :imm t)        ; x1 = vec & ~0xF
              ;; x5 = vec length (raw)
              (arm64:ldr 5 1 :offset 0)           ; x5 = [x1+0] = length
              ;; Allocate string: store length at [x28], compute alloc size
              (arm64:str 5 28 :offset 0)          ; [x28+0] = length
              ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
              (arm64:add 4 5 23 :imm t)             ; x4 = len + 23 (= len + 8 + 15)
              (arm64:and* 4 4 -16 :imm t)        ; x4 = (len + 23) & ~15 (clear low 4 bits)
              ;; Save string ptr (will be result), bump heap
              (arm64:mov 0 28)               ; x0 = string base (untagged)
              (arm64:add 28 28 4)            ; x28 += alloc_size
              ;; GC trigger check: if x28 >= from_end, call GC
              (arm64:ldr 9 27 :offset 16)       ; x9 = from_end [x27+16]
              (arm64:cmp 28 9)                  ; compare x28, from_end
              (arm64:b.lo 2)                    ; skip if x28 < from_end
              (list '(:call-fn GC-COLLECT))    ; bl gc_collect
              ;; x2 = string data base = x0 + 8
              (arm64:add 2 0 8 :imm t)              ; x2 = string data start
              ;; x3 = loop counter = 0
              (arm64:movz 3 0)                   ; x3 = 0
              ;; Loop: while x3 < x5
              ;; loop_start: (offset 0 from here)
              (arm64:cmp 3 5)                ; cmp x3, x5
              (arm64:b.ge (ash 36 -2))     ; if x3 >= x5, jump to loop_end (+9 instructions = 36 bytes)
              ;; Load vec[x3]: address = x1 + 8 + x3*8
              (arm64:lsl 4 3 3 :imm t)              ; x4 = x3 * 8
              (arm64:add 4 4 8 :imm t)              ; x4 = 8 + x3*8 (offset in vec)
              (arm64:add 4 1 4)              ; x4 = vec_base + offset
              (arm64:ldr 4 4 :offset 0)           ; x4 = [x4] = tagged fixnum
              ;; Untag: x4 = x4 >> 4
              (arm64:lsr 4 4 4 :imm t)              ; x4 = char value (untagged)
              ;; Store byte: str_data[x3] = x4
              (arm64:strb 4 2 3 :reg t)             ; [x2 + x3] = x4 (byte)
              ;; x3++
              (arm64:add 3 3 1 :imm t)              ; x3++
              ;; Jump back to loop_start (cmp instruction)
              (arm64:b (ash -36 -2))               ; back 9 instructions = -36 bytes
              ;; loop_end:
              ;; Tag result with string tag (0x4)
              (arm64:movz 4 4)                   ; x4 = 4
              (arm64:orr 0 0 4)))))
    ;; buffer-to-string-ir - convert raw byte buffer to string (inline)
    ((has-tag ir 'buffer-to-string-ir)
     ;; buffer-to-string-ir = (buffer-to-string-ir buf-ir len-ir)
     ;; Inline implementation: allocate string on heap, copy raw bytes from buffer
     ;; Buffer layout: [length (8 bytes)][raw bytes...] (sys-read writes raw bytes)
     ;; String layout: [length (8 bytes)][char data (n bytes)]
     ;; Register usage:
     ;;   x0: result (tagged string)
     ;;   x1: untagged buf base + 8 (raw data start)
     ;;   x2: string data base (untagged string ptr + 8)
     ;;   x3: loop counter (0 to len-1)
     ;;   x4: temp for loading/storing bytes
     ;;   x5: length (untagged)
     (let* ((buf-ir (cadr ir))
            (len-ir (caddr ir))
            (buf-slot (temp-slot td))
            (nd (+ td 1))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (len-code (codegen len-ir rtaddrs fnoffs nd)))
       (append-all
        (list
         ;; Evaluate buf, save to slot
         buf-code
         (arm64:str 0 31 :offset buf-slot)
         ;; Evaluate len
         len-code
         ;; x5 = length (untagged)
         (arm64:lsr 5 0 4 :imm t)                 ; x5 = len >> 4 (untag)
         ;; x1 = buf data start (untagged buf base + 8)
         (arm64:ldr 1 31 :offset buf-slot)      ; x1 = buf (tagged)
         (arm64:and* 1 1 -16 :imm t)           ; x1 = buf & ~0xF (clear tag)
         (arm64:add 1 1 8 :imm t)                 ; x1 = buf + 8 (skip length header)
         ;; Allocate string: store length at [x28]
         (arm64:str 5 28 :offset 0)             ; [x28+0] = length
         ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
         (arm64:add 4 5 23 :imm t)                ; x4 = len + 23 (= len + 8 + 15)
         (arm64:and* 4 4 -16 :imm t)           ; x4 = (len + 23) & ~15
         ;; Save string ptr (will be result), bump heap
         (arm64:mov 0 28)                  ; x0 = string base (untagged)
         (arm64:add 28 28 4)               ; x28 += alloc_size
         ;; x2 = string data base = x0 + 8
         (arm64:add 2 0 8 :imm t)                 ; x2 = string data start
         ;; x3 = loop counter = 0
         (arm64:movz 3 0)                      ; x3 = 0
         ;; Loop: while x3 < x5
         ;; loop_start: (offset 0 from here)
         (arm64:cmp 3 5)                   ; cmp x3, x5
         (arm64:b.ge (ash 24 -2))        ; if x3 >= x5, jump to loop_end (+6 instructions = 24 bytes)
         ;; Load buf[x3] - raw byte
         (arm64:add 4 1 3)                 ; x4 = buf_data + x3
         (arm64:ldrb 4 4 0)             ; x4 = byte at [x4]
         ;; Store byte: str_data[x3] = x4
         (arm64:strb 4 2 3 :reg t)                ; [x2 + x3] = x4 (byte)
         ;; x3++
         (arm64:add 3 3 1 :imm t)                 ; x3++
         ;; Jump back to loop_start (cmp instruction)
         (arm64:b (ash -24 -2))                  ; back 6 instructions = -24 bytes
         ;; loop_end:
         ;; Tag result with string tag (0x4)
         (arm64:movz 4 4)                      ; x4 = 4
         (arm64:orr 0 0 4)))))
    ;; make-symbol-from-string-ir - intern string as symbol
    ((has-tag ir 'make-symbol-from-string-ir)
     ;; make-symbol-from-string-ir = (make-symbol-from-string-ir str-ir)
     ;; For native (no runtime): inline intern using x27 as symbol table base
     ;; Symbol table layout: x27[0] = next-id, x27[8] = table-ptr (list)
     ;; Table is list of (name . (id . next)) entries
     ;; String layout: [length (8 bytes)][char data] - ptr points to start
     ;; Result is symbol tagged as (id << 4) | 2
     ;;
     ;; Algorithm (simplified - always creates new symbol for now):
     ;; TODO: Add table search to deduplicate symbols
     ;; 1. Evaluate string, save to slot
     ;; 2. Get next-id from x27[0]
     ;; 3. Create symbol entry in table
     ;; 4. Return symbol with ID tagged as symbol
     (let* ((str-ir (cadr ir))
            (str-code (codegen str-ir rtaddrs fnoffs (+ td 5)))
            (str-slot (temp-slot td)))
       (append-all
        (list
         ;; Evaluate and save input string
         str-code
         (arm64:str 0 31 :offset str-slot)

         ;; Get next-id from x27[0]
         (arm64:ldr 3 27 :offset 0)  ; x3 = next-id (untagged)

         ;; Create (id . table) cons
         ;; id = x3 << 4 (tag as fixnum)
         (arm64:lsl 4 3 4 :imm t)      ; x4 = id as fixnum
         ;; table = [x27+8]
         (arm64:ldr 5 27 :offset 8)  ; x5 = current table
         ;; Allocate cons: [x28+0] = id, [x28+8] = table
         (arm64:str 4 28 :offset 0)
         (arm64:str 5 28 :offset 8)
         ;; Tag as cons
         (arm64:mov 6 28)
         (arm64:movz 9 1)
         (arm64:orr 6 6 9)      ; x6 = id-next cons
         (arm64:add 28 28 16 :imm t)   ; bump heap

         ;; Create outer cons: (name . id-next)
         ;; name = input string
         (arm64:ldr 0 31 :offset str-slot)
         (arm64:str 0 28 :offset 0)  ; [x28+0] = name
         (arm64:str 6 28 :offset 8)  ; [x28+8] = id-next cons
         ;; Tag as cons
         (arm64:mov 7 28)
         (arm64:orr 7 7 9)      ; x7 = new entry cons
         (arm64:add 28 28 16 :imm t)   ; bump heap

         ;; Update table: x27[8] = new entry
         (arm64:str 7 27 :offset 8)
         ;; Increment next-id: x27[0] = x3 + 1
         (arm64:add 3 3 1 :imm t)
         (arm64:str 3 27 :offset 0)

         ;; Return id as symbol: (id << 4) | 2
         ;; x4 already has id << 4 (as fixnum)
         (arm64:movz 11 #xF)
         (arm64:bic 0 4 11)     ; clear fixnum tag
         (arm64:movz 9 2)
         (arm64:orr 0 0 9)))))  ; tag as symbol
    ;; symbol-name-ir - get symbol's name by looking up in symbol table
    ((has-tag ir 'symbol-name-ir)
     ;; symbol-name-ir = (symbol-name-ir sym-ir)
     ;; Symbol table at x27[8] is list of (name . (id . rest)) entries
     ;; Symbol value is (id << 4) | 2
     ;; Algorithm:
     ;; 1. Get symbol ID: sym >> 4 (clear tag)
     ;; 2. Walk table until find entry where (car (cdr entry)) >> 4 == id
     ;; 3. Return (car entry) (the name string)
     (let* ((sym-ir (cadr ir))
            (sym-code (codegen sym-ir rtaddrs fnoffs (+ td 5))))
       (append-all
        (list
         ;; Evaluate symbol
         sym-code
         ;; Get ID: x1 = sym >> 4 (already properly shifted since tag is 2)
         (arm64:lsr 1 0 4 :imm t)           ; x1 = symbol ID (untagged)
         ;; Get table: x2 = x27[8]
         (arm64:ldr 2 27 :offset 8)       ; x2 = table (list of entries)
         ;; Load mask for clearing tag bits
         (arm64:movz 11 #xF)             ; x11 = 0xF (tag mask)
         ;; loop:
         ;; Check if nil (x2 == 0)
         (arm64:cmp 2 0 :imm t)
         (arm64:b.eq (ash 48 -2))  ; if nil, jump to end (+12 instructions = 48 bytes)
         ;; Get entry: x2 is cons (entry . rest), untag to get pointer
         (arm64:bic 3 2 11)          ; x3 = entry pointer (untagged)
         ;; Get id-next: (cdr entry) = [x3+8]
         (arm64:ldr 4 3 :offset 8)        ; x4 = (id . rest) cons
         ;; Untag and get id: (car x4) = [x4-1] after untagging
         (arm64:bic 4 4 11)          ; x4 = pointer to (id . rest)
         (arm64:ldr 5 4 :offset 0)        ; x5 = id (as fixnum, so id << 4)
         (arm64:lsr 5 5 4 :imm t)           ; x5 = id (untagged)
         ;; Compare: x5 == x1?
         (arm64:cmp 5 1)
         (arm64:b.eq (ash 12 -2))  ; if match, jump to found (+3 instructions = 12 bytes)
         ;; Not match, advance: x2 = (cdr entry) = [x3+8], then cdr of that = [x4+8]
         (arm64:ldr 2 4 :offset 8)        ; x2 = rest of table
         (arm64:b (ash -44 -2))            ; back to loop start (11 instructions = -44 bytes)
         ;; found: return (car entry) = [x3+0] (the name string)
         (arm64:ldr 0 3 :offset 0)        ; x0 = name string
         ;; skip to end (past the nil case)
         (arm64:b (ash 8 -2))              ; skip past nil case (branch + movz = 8 bytes)
         ;; end (nil case): return nil
         (arm64:movz 0 0)))))            ; x0 = nil
    ;; write-bytes-ir - write vector of bytes to file
    ((has-tag ir 'write-bytes-ir)
     ;; write-bytes-ir = (write-bytes-ir path-ir vec-ir)
     ;; Runtime index 53 = habu_write_bytes at offset 424
     ;; Takes path string in x0, byte vector in x1
     (let* ((path-ir (cadr ir))
            (vec-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (pc (codegen path-ir rtaddrs fnoffs nd))
            (sp (arm64:str 0 31 :offset _xs))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (mv (arm64:mov 1 0))
            (lp (arm64:ldr 0 31 :offset _xs))
            (lf (arm64:ldr 9 19 :offset 424))
            (bl (arm64:blr 9)))
       (append-all (list pc sp vc mv lp lf bl))))
    ;; nthcdr-ir - get nth cdr of list
    ((has-tag ir 'nthcdr-ir)
     ;; nthcdr-ir = (nthcdr-ir n-ir list-ir)
     ;; Loop: while n > 0 do x0 = cdr(x0), n = n - 1
     (let* ((n-ir (cadr ir))
            (list-ir (caddr ir))
            (_xs (temp-slot td))
            (nd (+ td 1))
            (nc (codegen n-ir rtaddrs fnoffs nd))
            (sn (arm64:str 0 31 :offset _xs))
            (lc (codegen list-ir rtaddrs fnoffs nd))
            (ml (arm64:mov 1 0))
            (ln (arm64:ldr 2 31 :offset _xs))
            (asr (arm64:asr 2 2 4 :imm t))
            (cm (arm64:cmp 2 0 :imm t))
            (be (arm64:b.le (ash 28 -2)))
            (m0 (arm64:mov 0 1))
            (lf (arm64:ldr 9 19 :offset 16))
            (bl (arm64:blr 9))
            (m1 (arm64:mov 1 0))
            (si (arm64:sub 2 2 1 :imm t))
            (bk (arm64:b (ash -20 -2)))
            (mr (arm64:mov 0 1)))
       (append-all (list nc sn lc ml ln asr cm be m0 lf bl m1 si bk mr))))
    ;; values-ir - return multiple values
    ((has-tag ir 'values-ir)
     ;; values-ir = (values-ir (ir1 ir2 ...))
     ;; Runtime index 17 = habu_values_set at offset 136
     ;; habu_values_set(count, v0, v1, v2, v3)
     (let* ((irs (cadr ir)))
       (if (null irs)
           ;; No values - call values_set(0, 0, 0, 0, 0)
           (append-all
            (list (arm64:movz 0 0)
                  (arm64:movz 1 0)
                  (arm64:movz 2 0)
                  (arm64:movz 3 0)
                  (arm64:movz 4 0)
                  (arm64:ldr 9 19 :offset 136)
                  (arm64:blr 9)))
           (if (null (cdr irs))
               ;; Single value - just return it
               (codegen (car irs) rtaddrs fnoffs td)
               ;; Multiple values - evaluate all and call values_set
               (let* ((cnt (length irs))
                      (_xs (temp-slot td))
                      (nd (+ td 4)))
                 (declare (ignore _xs))
                 (labels ((eval-vals (vs idx acc)
                            (if (null vs)
                                acc
                                (let* ((vc (codegen (car vs) rtaddrs fnoffs nd))
                                       (slot (temp-slot (+ td idx)))
                                       (sv (arm64:str 0 31 :offset slot)))
                                  (eval-vals (cdr vs) (+ idx 1)
                                             (append-all (list acc vc sv)))))))
                   (let* ((evc (eval-vals irs 0 nil))
                          (l0 (if (> cnt 0) (arm64:ldr 1 31 :offset (temp-slot td)) (arm64:movz 1 0)))
                          (l1 (if (> cnt 1) (arm64:ldr 2 31 :offset (temp-slot (+ td 1))) (arm64:movz 2 0)))
                          (l2 (if (> cnt 2) (arm64:ldr 3 31 :offset (temp-slot (+ td 2))) (arm64:movz 3 0)))
                          (l3 (if (> cnt 3) (arm64:ldr 4 31 :offset (temp-slot (+ td 3))) (arm64:movz 4 0)))
                          (ct (ash cnt 4))
                          (mc (arm64:movz 0 ct))
                          (lf (arm64:ldr 9 19 :offset 136))
                          (bl (arm64:blr 9))
                          (lv (arm64:ldr 0 31 :offset (temp-slot td))))
                     (append-all (list evc l0 l1 l2 l3 mc lf bl lv)))))))))
    ;; mvb-ir - multiple-value-bind
    ((has-tag ir 'mvb-ir)
     ;; mvb-ir = (mvb-ir vars form-ir nvars body-ir)
     ;; Runtime index 18 = habu_values_get at offset 144
     ;; habu_values_get(index, primary) returns value at index
     ;; Values must be stored in env frame (x20-based) so body VAR refs work
     (let* ((form-ir (caddr ir))
            (nvars (cadddr ir))
            (body-ir (nth 4 ir))
            (xs (temp-slot td))
            (nd (+ td 1))
            (fc (codegen form-ir rtaddrs fnoffs nd))
            (sp (arm64:str 0 31 :offset xs)))
       ;; Evaluate form, save primary, then get each value and store in env frame
       (labels ((bind-vars (idx acc)
                  (if (>= idx nvars)
                      acc
                      ;; habu_values_get expects untagged index (0, 1, 2, ...)
                      (let* ((mi (arm64:movz 0 idx))
                             (lp (arm64:ldr 1 31 :offset xs))
                             (lf (arm64:ldr 9 19 :offset 144))
                             (bl (arm64:blr 9))
                             ;; Store in env frame: sub x1, x20, offset; str x0, [x1]
                             (env-off (* idx 8))
                             (s1 (arm64:sub 1 20 env-off :imm t))
                             (sv (arm64:str 0 1 :offset 0)))
                        (bind-vars (+ idx 1)
                                   (append-all (list acc mi lp lf bl s1 sv)))))))
         (let* ((bc (bind-vars 0 nil))
                (body-code (codegen body-ir rtaddrs fnoffs nd)))
           (append-all (list fc sp bc body-code))))))
    ((has-tag ir 'if-ir)
     (let ((test-ir (cadr ir)))
       (let ((then-ir (caddr ir)))
         (let ((else-ir (cadddr ir)))
           (let ((tc (codegen test-ir rtaddrs fnoffs td)))
             (let ((thc (codegen then-ir rtaddrs fnoffs td)))
               (let ((elc (codegen else-ir rtaddrs fnoffs td)))
                 ;; Use code-size to correctly account for :call-fn markers
                 ;; Layout: B.EQ | then-code | B | else-code
                 ;; B.EQ skips to else-code: then-code + B (4) + self (4) = then_bytes + 8
                 ;; B skips past else-code: else-code + self (4) = else_bytes + 4
                 (let ((then-bytes (code-size thc)))
                   (let ((else-bytes (code-size elc)))
                     (append-all
                      (list tc
                            (arm64:movz 1 6)   ; x1 = 6 (nil)
                            (arm64:cmp 0 1)    ; if x0 == nil, take else branch
                            (arm64:b.eq (ash (+ then-bytes 8) -2))  ; Skip then + B + self
                            thc
                            (arm64:b (ash (+ else-bytes 4) -2))  ; Skip else + landing
                            elc)))))))))))
    ((has-tag ir 'while-ir)
     ;; while-ir = (while-ir test body)
     ;; Layout: test-code, cmp, b.eq(exit), body-code, b(back-to-test)
     (let* ((test-ir (cadr ir))
            (body-ir (caddr ir))
            (test-code (codegen test-ir rtaddrs fnoffs td))
            (body-code (codegen body-ir rtaddrs fnoffs td))
            (test-size (code-size test-code))
            (body-size (code-size body-code)))
       ;; From b-cond at X: body starts at X+4, backward-b at X+4+body_size,
       ;; exit at X+4+body_size+4. So skip offset = 4+body_size+4 = body_size+8
       (append-all
        (list test-code
              (arm64:cmp 0 6 :imm t)   ; cmp x0, #6 (nil)
              ;; If test is false (x0==nil), skip body and back-branch
              (arm64:b.eq (ash (+ body-size 8) -2))
              body-code
              ;; Jump back to start of test
              (arm64:b (ash (- 0 (+ test-size 8 body-size)) -2))))))
    ;; get-intern-table-ir - load intern table from [x27 + 0]
    ((has-tag ir 'get-intern-table-ir)
     (arm64:ldr 0 27 :offset 0))
    ;; set-intern-table-ir - store value to [x27 + 0], return value
    ((has-tag ir 'set-intern-table-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 0))))
    ;; get-lambda-counter-ir - load counter from [x27 + 8]
    ((has-tag ir 'get-lambda-counter-ir)
     (arm64:ldr 0 27 :offset 8))
    ;; set-lambda-counter-ir - store value to [x27 + 8], return value
    ((has-tag ir 'set-lambda-counter-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 8))))
    ((has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir)))  ;; offs is at index 3
            (xs (temp-slot td))
            (nd (+ td 1))
            (acc (arm64:str 24 31 :offset xs)))
       (labels ((gb (vs os a)
                  (if (null vs) a
                      (let* ((vc (codegen (car vs) rtaddrs fnoffs nd))
                             (s1 (arm64:sub 1 20 (* (car os) 8) :imm t))
                             (s2 (arm64:str 0 1 :offset 0))
                             (st (append s1 s2))
                             (ld (arm64:ldr 24 31 :offset xs))
                             (t1 (append a ld))
                             (t2 (append t1 vc))
                             (t3 (append t2 st)))
                        (gb (cdr vs) (cdr os) t3)))))
         (let* ((body-code (gb vals offs nil))
                (final-ld (arm64:ldr 24 31 :offset xs))
                (bc (codegen bir rtaddrs fnoffs nd))
                (r1 (append acc body-code))
                (r2 (append r1 final-ld)))
           (append r2 bc)))))
    ((has-tag ir 'call-fn)
     ;; Function call with support for >8 arguments
     ;; Args 0-7 go in x0-x7, args 8+ go on stack per ARM64 ABI
     ;; IMPORTANT: Stack must be 16-byte aligned per AAPCS64
     (let* ((fnm (cadr ir))
            (airs (caddr ir))
            (na (length airs))
            (stack-args (max 0 (- na 8)))          ; How many args go on stack
            (stack-bytes (* stack-args 8))         ; Raw bytes needed
            (stack-space (if (> stack-bytes 0)     ; Round up to 16-byte alignment
                             (* (ceiling stack-bytes 16) 16)
                             0))
            (xs (temp-slot td))
            (nd (+ td 1)))
       (labels ((ga (as i a)
                  ;; Evaluate all args to spill slots
                  (if (null as) a
                      (let* ((rs (if (> i 0) (arm64:ldr 24 31 :offset xs) nil))
                             (ac (codegen (car as) rtaddrs fnoffs nd))
                             (st (arm64:str 0 31 :offset (spill-slot td i)))
                             (t1 (append a rs))
                             (t2 (append t1 ac))
                             (t3 (append t2 st)))
                        (ga (cdr as) (+ i 1) t3))))
                (gl-reg (i a)
                  ;; Load args 0-7 into registers x0-x7
                  ;; After alloc-stack, sp moved down by stack-space, so adjust offset
                  (if (>= i (min na 8)) a
                      (let* ((adjusted-off (+ (spill-slot td i) stack-space))
                             (ld (arm64:ldr i 31 :offset adjusted-off))
                             (t1 (append a ld)))
                        (gl-reg (+ i 1) t1))))
                (store-stack-args (i a)
                  ;; Store args 8+ to stack: arg i goes to [sp + (i-8)*8]
                  ;; After alloc-stack, sp moved down by stack-space, so adjust offset
                  (if (>= i na) a
                      (let* ((adjusted-off (+ (spill-slot td i) stack-space))
                             (ld (arm64:ldr 0 31 :offset adjusted-off))
                             (stack-off (* (- i 8) 8))
                             (st (arm64:str 0 31 :offset stack-off))
                             (t1 (append a ld))
                             (t2 (append t1 st)))
                        (store-stack-args (+ i 1) t2)))))
         (let* ((save-x24 (arm64:str 24 31 :offset xs))
                (args-code (ga airs 0 nil))
                (restore-x24 (arm64:ldr 24 31 :offset xs))
                ;; Allocate stack space for args 8+ (if any)
                (alloc-stack (if (> stack-args 0)
                                 (arm64:sub 31 31 stack-space :imm t)
                                 nil))
                ;; Store args 8+ to stack
                (stack-code (store-stack-args 8 nil))
                ;; Load args 0-7 into registers
                (load-args (gl-reg 0 nil))
                (set-argc (arm64:movz 23 na))
                ;; Emit special marker instead of BL: (:call-fn name)
                ;; This will be resolved to actual BL in resolve-calls
                (call-marker (list (list :call-fn fnm)))
                ;; Deallocate stack space after call returns
                (dealloc-stack (if (> stack-args 0)
                                   (arm64:add 31 31 stack-space :imm t)
                                   nil)))
           (append-all (list save-x24 args-code restore-x24
                                alloc-stack stack-code load-args
                                set-argc call-marker dealloc-stack))))))
    ((has-tag ir 'tail-call-fn)
     ;; Tail call optimization: evaluate args, run epilogue, then jump (B) instead of call (BL)
     ;; The callee will set up its own frame, so we tear down ours first
     ;; NOTE: Tail calls currently limited to 8 args (x0-x7) because epilogue deallocates
     ;; our frame before we can set up stack args. >8 args requires saving to callee-saved
     ;; registers or converting to regular call.
     (let* ((fnm (cadr ir))
            (airs (caddr ir))
            (na (length airs))
            (xs (temp-slot td))
            (nd (+ td 1)))
       (labels ((ga (as i a)
                  (if (null as) a
                      (let* ((rs (if (> i 0) (arm64:ldr 24 31 :offset xs) nil))
                             (ac (codegen (car as) rtaddrs fnoffs nd))
                             (st (arm64:str 0 31 :offset (spill-slot td i)))
                             (t1 (append a rs))
                             (t2 (append t1 ac))
                             (t3 (append t2 st)))
                        (ga (cdr as) (+ i 1) t3))))
                (gl-reg (i a)
                  ;; Only load args 0-7 into registers for tail calls
                  (if (>= i (min na 8)) a
                      (let* ((ld (arm64:ldr i 31 :offset (spill-slot td i)))
                             (t1 (append a ld)))
                        (gl-reg (+ i 1) t1)))))
         (let* ((save-x24 (arm64:str 24 31 :offset xs))
                (args-code (ga airs 0 nil))
                (restore-x24 (arm64:ldr 24 31 :offset xs))
                (load-args (gl-reg 0 nil))
                (set-argc (arm64:movz 23 na))
                ;; Run epilogue to restore caller's registers and pop our frame
                ;; Use conservative max frame size for tail calls (actual size set by caller)
                (epilogue (fn-epilogue #x2000))
                ;; Emit tail call marker (resolved to B instead of BL)
                (call-marker (list (list :tail-call-fn fnm))))
           (append save-x24 args-code restore-x24 load-args set-argc epilogue call-marker)))))
    ((has-tag ir 'loop-ir)
     ;; loop-ir = (loop-ir body-ir)
     ;; Generate loop marker followed by body code
     ;; The marker records position for continue-ir to jump back to
     (let ((body-ir (cadr ir)))
       (append (list (list :loop-start))
               (codegen body-ir rtaddrs fnoffs td))))
    ((has-tag ir 'continue-ir)
     ;; continue-ir = (continue-ir (new-arg-ir ...))
     ;; Evaluate new args to temp slots, copy to param slots, jump back to loop start
     ;; Note: We must evaluate ALL args before storing ANY to handle (f (- n 1) (+ acc n))
     (let* ((new-args-ir (cadr ir))
            (nargs (length new-args-ir))
            (xs (temp-slot td))
            (nd (+ td 1)))
       ;; Generate code to evaluate all new args and store to temp slots
       (labels ((eval-args (args idx acc)
                  (if (null args)
                      acc
                      (let* ((arg-code (codegen (car args) rtaddrs fnoffs nd))
                             (store (arm64:str 0 31 :offset (spill-slot td idx))))
                        (eval-args (cdr args) (+ idx 1) (append acc arg-code store)))))
                (copy-to-params (idx acc)
                  ;; Copy from temp slots to param slots (offsets 0, 8, 16, ...)
                  (if (>= idx nargs)
                      acc
                      (let* ((load (arm64:ldr 0 31 :offset (spill-slot td idx)))
                             (param-addr (arm64:sub 1 20 (* idx 8) :imm t))
                             (store (arm64:str 0 1 :offset 0)))
                        (copy-to-params (+ idx 1) (append acc load param-addr store))))))
         (let* ((save-x24 (arm64:str 24 31 :offset xs))
                (eval-code (eval-args new-args-ir 0 nil))
                (restore-x24 (arm64:ldr 24 31 :offset xs))
                (copy-code (copy-to-params 0 nil))
                (jump-marker (list (list :loop-continue))))
           (append save-x24 eval-code restore-x24 copy-code jump-marker)))))
    ((has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     ;; Generate code for each form, keep result of last
     (let ((forms-ir (cadr ir)))
       (labels ((gen-seq (fs acc)
                  (if (null fs)
                      acc
                      (let ((fc (codegen (car fs) rtaddrs fnoffs td)))
                        (gen-seq (cdr fs) (append acc fc))))))
         (gen-seq forms-ir nil))))
    ((has-tag ir 'div)
     ;; Division: both operands untagged, divide, re-tag
     (codegen-binop (cadr ir) (caddr ir)
                       (append-all (list (arm64:lsr 0 0 4 :imm t)
                                            (arm64:lsr 1 1 4 :imm t)
                                            (arm64:sdiv 0 0 1)
                                            (arm64:lsl 0 0 4 :imm t)))
                       rtaddrs fnoffs td))
    ((or (has-tag ir 'mod) (has-tag ir 'mod-ir))
     ;; Modulo: a mod b = a - (a / b) * b
     (codegen-binop (cadr ir) (caddr ir)
                       (append-all (list (arm64:lsr 0 0 4 :imm t)
                                            (arm64:lsr 1 1 4 :imm t)
                                            (arm64:sdiv 2 0 1)
                                            (arm64:mul 2 2 1)
                                            (arm64:sub 0 0 2)
                                            (arm64:lsl 0 0 4 :imm t)))
                       rtaddrs fnoffs td))
    ((has-tag ir 'lambda-ir)
     ;; lambda-ir should be lifted to lambda-ref before codegen
     ;; If we encounter it directly, it's an error - return 0
     (arm64:movz 0 0))
    ((has-tag ir 'lambda-ref)
     ;; lambda-ref = (lambda-ref name free-var-offsets)
     ;; Create closure as inline heap cons cell (no runtime call):
     ;; car = fn-offset (as tagged fixnum) - relative offset in bytes
     ;; cdr = env (cons list of captures, or nil)
     ;; Result = heap ptr | tag 5 (closure tag)
     (let* ((name (cadr ir))
            (free-offsets (caddr ir))
            (capture-count (length free-offsets))
            (fn-entry (assoc name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0))
            (offset-bytes fn-offset))
       (if (= capture-count 0)
           ;; No captures - inline closure cons: (fn-offset . nil)
           ;; Store fn-offset (tagged fixnum) in car, nil in cdr
           ;; Allocate 16 bytes on heap (x28 = bump pointer)
           (let ((tagged-offset (ash offset-bytes 4)))  ; tag as fixnum
             (append-all
              (list
               ;; Store fn-offset (tagged) in [x28]
               ;; Use load-addr-32 to ensure consistent size during two-pass compilation
               (load-addr-32 9 tagged-offset)   ; x9 = tagged offset
               (arm64:str 9 28 :offset 0)           ; [x28+0] = car = fn-offset
               ;; Store nil in [x28+8]
               (arm64:movz 10 0)                   ; x10 = nil
               (arm64:str 10 28 :offset 8)          ; [x28+8] = cdr = nil
               ;; Result = x28 | 5 (closure tag)
               (arm64:mov 0 28)                ; x0 = x28
               (arm64:movz 9 5)                    ; x9 = closure tag
               (arm64:orr 0 0 9)                   ; x0 = x28 | 5
               ;; Bump heap pointer by 16
               (arm64:add 28 28 16 :imm t))))
           ;; Has captures - build env as cons list, then make closure cons
           ;; First build env cons list (capture-count cells)
           ;; Then allocate closure cons
           (let ((capture-code
                  (labels ((build-captures (offs acc env-acc)
                             (if (null offs)
                                 (list acc env-acc)  ; return (code . result-slot)
                                 (let* ((off (car offs))
                                        (val-slot (temp-slot (+ td 2 (* 2 (length offs)))))
                                        (pair-slot (temp-slot (+ td 3 (* 2 (length offs)))))
                                        ;; Load captured value
                                        (load-cap
                                         (append-all
                                          (list
                                           (arm64:sub 1 20 (* off 8) :imm t) ; x1 = &captured
                                           (arm64:ldr 0 1 :offset 0)       ; x0 = captured value
                                           (arm64:str 0 31 :offset val-slot)))) ; save value
                                        ;; Allocate cons: (value . prev-env)
                                        (alloc-cons
                                         (append-all
                                          (list
                                           (arm64:ldr 9 31 :offset val-slot)  ; car = captured value
                                           (arm64:str 9 28 :offset 0)         ; [x28+0] = car
                                           ;; cdr = previous env acc
                                           (if (null env-acc)
                                               (arm64:movz 9 0)              ; first: cdr = nil
                                               (arm64:ldr 9 31 :offset env-acc)) ; else: load prev env
                                           (arm64:str 9 28 :offset 8)         ; [x28+8] = cdr
                                           ;; Result = x28 | 1 (cons tag)
                                           (arm64:mov 0 28)
                                           (arm64:movz 9 1)
                                           (arm64:orr 0 0 9)                 ; x0 = cons ptr
                                           ;; Save and bump
                                           (arm64:str 0 31 :offset pair-slot)
                                           (arm64:add 28 28 16 :imm t)))))
                                   (build-captures (cdr offs)
                                                   (append-all (list acc load-cap alloc-cons))
                                                   pair-slot)))))
                    ;; Reverse free-offsets so first captured var ends up at car of env list
                   ;; This matches gen-capture-copies which stores car at slot 0, etc.
                   (build-captures (reverse free-offsets) nil nil))))
             (let* ((env-code (car capture-code))
                    (env-result-slot (cadr capture-code))
                    (tagged-offset (ash offset-bytes 4)))
               (append-all
                (list
                 ;; Build env cons list
                 env-code
                 ;; Now allocate closure cons: (fn-offset . env)
                 ;; Use load-addr-32 to ensure consistent size during two-pass compilation
                 (load-addr-32 9 tagged-offset)     ; car = fn-offset (tagged)
                 (arm64:str 9 28 :offset 0)             ; [x28+0] = car
                 (arm64:ldr 9 31 :offset env-result-slot) ; cdr = env cons list
                 (arm64:str 9 28 :offset 8)             ; [x28+8] = cdr
                 ;; Result = x28 | 5 (closure tag)
                 (arm64:mov 0 28)
                 (arm64:movz 9 5)
                 (arm64:orr 0 0 9)
                 ;; Bump heap
                 (arm64:add 28 28 16 :imm t))))))))
    ((has-tag ir 'funcall-ir)
     ;; funcall-ir = (funcall-ir fn-ir args-ir-list)
     ;; Inline closure access (no runtime calls):
     ;; 1. Evaluate fn-ir to get closure (cons cell with tag 5)
     ;; 2. Extract fn-offset from car, env from cdr
     ;; 3. Compute code address: x26 (code base) + fn-offset
     ;; 4. Set up args and call (args 0-7 in registers, 8+ on stack)
     ;; Closure layout: car = fn-offset (tagged fixnum), cdr = env (cons or nil)
     ;; IMPORTANT: Stack must be 16-byte aligned per AAPCS64
     ;; CRITICAL FIX: Lambdas have no prologue, so funcall-ir must set x20
     ;; for the lambda's parameter stores to write to the lambda's own area
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (num-args (length args-ir))
            (stack-args (max 0 (- num-args 8)))   ; How many args go on stack
            (stack-bytes (* stack-args 8))        ; Raw bytes needed
            (stack-space (if (> stack-bytes 0)    ; Round up to 16-byte alignment
                             (* (ceiling stack-bytes 16) 16)
                             0))
            ;; Lambda parameter space (for lambda's param-stores to write to)
            (param-bytes (* num-args 8))
            (param-space (if (> param-bytes 0)
                             (* (ceiling param-bytes 16) 16)
                             16))  ; Minimum 16 bytes even for 0 params
            ;; Temp slots: 0=x24-save, 1=x20-save, 2=x30-save, 3=code-addr, 4=env, 5..5+n-1=args
            (x24-slot (temp-slot td))
            (x20-slot (temp-slot (+ td 1)))
            (x30-slot (temp-slot (+ td 2)))  ; Save LR - lambdas make BL calls!
            (code-slot (temp-slot (+ td 3)))
            (env-slot (temp-slot (+ td 4)))
            (arg-base (+ td 5))
            (nested-td (+ arg-base num-args))
            ;; Evaluate function
            (fn-code (codegen fn-ir rtaddrs fnoffs nested-td)))
       (labels ((gen-args (airs idx acc)
                  (if (null airs)
                      acc
                      (let* ((rs (if (> idx 0) (arm64:ldr 24 31 :offset x24-slot) nil))
                             (ac (codegen (car airs) rtaddrs fnoffs nested-td))
                             (st (arm64:str 0 31 :offset (temp-slot (+ arg-base idx)))))
                        (gen-args (cdr airs) (+ idx 1)
                                  (append-all (list acc rs ac st))))))
                (load-reg-args (idx total-offset acc)
                  ;; Load args 0-7 into registers x0-x7
                  ;; After alloc-stack + param-frame, sp moved down by total-offset, so adjust
                  (if (>= idx (min num-args 8))
                      acc
                      (let* ((adjusted-off (+ (temp-slot (+ arg-base idx)) total-offset))
                             (ld (arm64:ldr idx 31 :offset adjusted-off)))
                        (load-reg-args (+ idx 1) total-offset (append acc ld)))))
                (store-stack-args (idx total-offset acc)
                  ;; Store args 8+ to stack: arg i goes to [sp + (i-8)*8]
                  ;; After alloc-stack + param-frame, sp moved down by total-offset, so adjust
                  (if (>= idx num-args)
                      acc
                      (let* ((adjusted-off (+ (temp-slot (+ arg-base idx)) total-offset))
                             (ld (arm64:ldr 0 31 :offset adjusted-off))
                             (stack-off (* (- idx 8) 8))
                             (st (arm64:str 0 31 :offset stack-off)))
                        (store-stack-args (+ idx 1) total-offset (append-all (list acc ld st)))))))
         (let ((total-offset (+ stack-space param-space)))
           (append-all
            (list
             ;; Save x24 and x20
             (arm64:str 24 31 :offset x24-slot)
             (arm64:str 20 31 :offset x20-slot)
             ;; Evaluate closure into x0
             fn-code
             ;; Clear closure tag (5) to get heap address: x9 = x0 & ~0xF
             (arm64:movz 11 #xF)                     ; x11 = 0xF
             (arm64:bic 9 0 11)                  ; x9 = x0 & ~0xF
             ;; Load car = fn-offset (tagged): x10 = [x9+0]
             (arm64:ldr 10 9 :offset 0)
             ;; Untag fn-offset: x10 = x10 >> 4
             (arm64:lsr 10 10 4 :imm t)
             ;; Compute code address: x10 = x26 + x10 (code_base + offset)
             (arm64:add 10 26 10)
             (arm64:str 10 31 :offset code-slot)      ; save code address
             ;; Load cdr = env: x11 = [x9+8]
             (arm64:ldr 11 9 :offset 8)
             (arm64:str 11 31 :offset env-slot)       ; save env
             ;; Restore x24 for arg evaluation
             (arm64:ldr 24 31 :offset x24-slot)
             ;; Evaluate args
             (gen-args args-ir 0 nil)
             ;; Allocate stack space for args 8+ (if any)
             (if (> stack-args 0)
                 (arm64:sub 31 31 stack-space :imm t)
                 nil)
             ;; Allocate parameter frame for lambda
             (arm64:sub 31 31 param-space :imm t)
             ;; Set x20 for lambda's param-stores: x20 = sp + param-space - 8
             (if (> param-space 8)
                 (arm64:add 20 31 (- param-space 8) :imm t)
                 (arm64:mov 20 31))  ; If param-space <= 8, set x20 = sp
             ;; Store args 8+ to stack (they're above the param frame)
             (store-stack-args 8 total-offset nil)
             ;; Load args 0-7 into registers
             (load-reg-args 0 total-offset nil)
             ;; Set x24 to callee's env
             (arm64:ldr 24 31 :offset (+ env-slot total-offset))
             ;; Set argc
             (arm64:movz 23 num-args)
             ;; BUG #20 FIX: Save x30 - lambdas have no prologue, make BL calls!
             ;; CRITICAL: x30 saved AFTER sp modified, so must adjust offset!
             (arm64:str 30 31 :offset (+ x30-slot total-offset))
             ;; Load code address and call
             (arm64:ldr 9 31 :offset (+ code-slot total-offset))
             (arm64:blr 9)
             ;; Restore x30 immediately after lambda returns
             ;; CRITICAL: sp still modified, so must adjust offset!
             (arm64:ldr 30 31 :offset (+ x30-slot total-offset))
             ;; Deallocate parameter frame
             (arm64:add 31 31 param-space :imm t)
             ;; Deallocate stack space for args 8+ (if any)
             (if (> stack-args 0)
                 (arm64:add 31 31 stack-space :imm t)
                 nil)
             ;; Restore x24 and x20
             (arm64:ldr 24 31 :offset x24-slot)
             (arm64:ldr 20 31 :offset x20-slot)))))))
    ((has-tag ir 'dotimes-ir)
     ;; dotimes-ir = (dotimes-ir var count-ir body result-form compile-env)
     ;; Generate counted loop:
     ;; 1. Evaluate count, save to slot
     ;; 2. Initialize counter to 0
     ;; 3. Loop: compare counter to count, branch if >=
     ;; 4. Store counter as var, execute body
     ;; 5. Increment counter, branch back
     ;; 6. Evaluate result with final counter value
     (let* ((var (cadr ir))
            (count-ir (caddr ir))
            (body-ir (cadddr ir))     ; Already compiled body IR
            (result-ir (nth 4 ir))    ; Already compiled result IR
            (compile-env (nth 5 ir))
            ;; Temp slots: 0=count, 1=counter, 2=x24-save
            (count-slot (temp-slot td))
            (counter-slot (temp-slot (+ td 1)))
            (x24-slot (temp-slot (+ td 2)))
            (body-td (+ td 3))
            ;; Compile count expression
            (count-code (codegen count-ir rtaddrs fnoffs body-td))
            ;; Calculate var offset from extended env
            (new-env (env-extend (list (list var)) compile-env))
            (var-offset (* (env-lookup var new-env) 8))
            ;; Codegen the already-compiled body and result
            (body-code (codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (count-instrs body-code))
            (result-code (codegen result-ir rtaddrs fnoffs body-td)))
       (append-all
        (list
         ;; Save x24
         (arm64:str 24 31 :offset x24-slot)
         ;; Evaluate and save count
         count-code
         (arm64:str 0 31 :offset count-slot)
         ;; Initialize counter to 0
         (arm64:movz 0 0)
         (arm64:str 0 31 :offset counter-slot)
         ;; Loop start: load counter and count, compare
         ;; Loop test: 4 instrs (ldr counter, ldr count, cmp, b.ge)
         (arm64:ldr 0 31 :offset counter-slot)
         (arm64:ldr 1 31 :offset count-slot)
         (arm64:cmp 0 1)
         ;; Branch past body + incr + loop-back if counter >= count
         ;; Body instrs + store var (4) + incr (4) + branch back (1) = body-instrs + 9
         (arm64:b.ge (ash (* (+ body-instrs 9 -2)) 4))
         ;; Store counter as var at its actual offset from x20
         (arm64:ldr 0 31 :offset counter-slot)
         (arm64:sub 1 20 var-offset :imm t)
         (arm64:str 0 1 :offset 0)
         ;; Restore x24 for body
         (arm64:ldr 24 31 :offset x24-slot)
         ;; Execute body
         body-code
         ;; Increment counter
         (arm64:ldr 0 31 :offset counter-slot)
         (arm64:add 0 0 #x10 :imm t)  ; add tagged 1
         (arm64:str 0 31 :offset counter-slot)
         ;; Branch back to loop start
         ;; Distance: -(loop test (4) + store var (4) + body + incr (3))
         (arm64:b (ash (- (* (+ body-instrs 11) 4)) -2))
         ;; After loop: evaluate result with final counter
         (arm64:ldr 0 31 :offset counter-slot)
         (arm64:sub 1 20 var-offset :imm t)
         (arm64:str 0 1 :offset 0)
         (arm64:ldr 24 31 :offset x24-slot)
         result-code))))
    ((has-tag ir 'dolist-ir)
     ;; dolist-ir = (dolist-ir var list-ir body-ir result-ir compile-env)
     ;; Generate list iteration loop:
     ;; 1. Evaluate list, save to slot
     ;; 2. Loop: check if null, branch if yes
     ;; 3. Get car, store as var, execute body
     ;; 4. Get cdr, save, branch back
     ;; 5. Evaluate result
     (let* ((var (cadr ir))
            (list-ir (caddr ir))
            (body-ir (cadddr ir))     ; Already compiled body IR
            (result-ir (nth 4 ir))    ; Already compiled result IR
            (compile-env (nth 5 ir))
            ;; Temp slots: 0=list-ptr, 1=x24-save
            (list-slot (temp-slot td))
            (x24-slot (temp-slot (+ td 1)))
            (body-td (+ td 2))
            ;; Compile list expression
            (list-code (codegen list-ir rtaddrs fnoffs body-td))
            ;; Calculate var offset from extended env
            (new-env (env-extend (list (list var)) compile-env))
            (var-offset (* (env-lookup var new-env) 8))
            ;; Codegen the already-compiled body and result
            (body-code (codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (count-instrs body-code))
            (result-code (codegen result-ir rtaddrs fnoffs body-td)))
       (append-all
        (list
         ;; Save x24
         (arm64:str 24 31 :offset x24-slot)
         ;; Evaluate and save list
         list-code
         (arm64:str 0 31 :offset list-slot)
         ;; Loop start: check if list is nil (tag 0)
         (arm64:ldr 0 31 :offset list-slot)
         (arm64:movz 1 0)  ; nil = 0
         (arm64:cmp 0 1)
         ;; Branch past body if list is nil
         ;; Body: store var (4) + body + get cdr (4) + branch (1) = body-instrs + 9
         (arm64:b.eq (ash (* (+ body-instrs 9 -2)) 4))
         ;; Get car of list -> var at its actual offset
         (arm64:ldr 0 31 :offset list-slot)
         (arm64:ldr 9 19 :offset 8)  ; car function at offset 8
         (arm64:blr 9)
         (arm64:sub 1 20 var-offset :imm t)
         (arm64:str 0 1 :offset 0)
         ;; Restore x24 for body
         (arm64:ldr 24 31 :offset x24-slot)
         ;; Execute body
         body-code
         ;; Get cdr, save as new list
         (arm64:ldr 0 31 :offset list-slot)
         (arm64:ldr 9 19 :offset 16)  ; cdr function at offset 16
         (arm64:blr 9)
         (arm64:str 0 31 :offset list-slot)
         ;; Branch back to loop start
         ;; Distance: -(null check (3) + get car (5) + body + get cdr (4))
         (arm64:b (ash (- (* (+ body-instrs 12) 4)) -2))
         ;; After loop: evaluate result (var is nil at this point)
         (arm64:movz 0 0)  ; nil
         (arm64:sub 1 20 var-offset :imm t)
         (arm64:str 0 1 :offset 0)
         (arm64:ldr 24 31 :offset x24-slot)
         result-code))))
    ;; === libSystem call IR forms (for native executables) ===
    ;; These emit :extern-call markers that are resolved by deliver
    ((has-tag ir 'sys-write-ir)
     ;; sys-write-ir = (sys-write-ir fd-ir buf-ir len-ir)
     ;; Calls _write(fd, buf, len) -> returns bytes written (or -1)
     ;; Args: fd in x0, buf (string ptr) in x1, len in x2
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (_xs (temp-slot td))
            (nd (+ td 3))
            ;; Evaluate fd
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (arm64:str 0 31 :offset (temp-slot td)))
            ;; Evaluate buf
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            ;; Evaluate len
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (declare (ignore _xs))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              ;; Load args: fd->x0, buf->x1, len->x2
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:lsr 0 0 4 :imm t)                      ; untag fd
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:and* 1 1 -16 :imm t)                ; clear string tag, get ptr
              (arm64:add 1 1 8 :imm t)                      ; skip length field
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)                      ; untag len
              ;; Emit extern call marker
              (list (list :extern-call "_write"))
              ;; Tag result as fixnum
              (arm64:lsl 0 0 4 :imm t)))))
    ((has-tag ir 'sys-read-ir)
     ;; sys-read-ir = (sys-read-ir fd-ir buf-ir len-ir)
     ;; Calls _read(fd, buf, len) -> returns bytes read (or-1)
     ;; buf should be a vector
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (_xs (temp-slot td))
            (nd (+ td 3))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (arm64:str 0 31 :offset (temp-slot td)))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (declare (ignore _xs))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:lsr 0 0 4 :imm t)                      ; untag fd
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:and* 1 1 -16 :imm t)                ; clear vector tag
              (arm64:add 1 1 8 :imm t)                      ; skip length field
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)                      ; untag len
              (list (list :extern-call "_read"))
              (arm64:lsl 0 0 4 :imm t)))))
    ((has-tag ir 'sys-open-ir)
     ;; sys-open-ir = (sys-open-ir path-ir flags-ir mode-ir)
     ;; Calls _open(path, flags, mode) -> returns fd (or -1)
     (let* ((path-ir (cadr ir))
            (flags-ir (caddr ir))
            (mode-ir (cadddr ir))
            (_xs (temp-slot td))
            (nd (+ td 3))
            (path-code (codegen path-ir rtaddrs fnoffs nd))
            (save-path (arm64:str 0 31 :offset (temp-slot td)))
            (flags-code (codegen flags-ir rtaddrs fnoffs nd))
            (save-flags (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            (mode-code (codegen mode-ir rtaddrs fnoffs nd))
            (save-mode (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (declare (ignore _xs))
       (append-all
        (list path-code save-path flags-code save-flags mode-code save-mode
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:and* 0 0 -16 :imm t)                ; clear string tag
              (arm64:add 0 0 8 :imm t)                      ; skip length field
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:lsr 1 1 4 :imm t)                      ; untag flags
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)                      ; untag mode
              (list (list :extern-call "_open"))
              (arm64:lsl 0 0 4 :imm t)))))
    ((has-tag ir 'sys-close-ir)
     ;; sys-close-ir = (sys-close-ir fd-ir)
     ;; Calls _close(fd) -> returns 0 on success
     (let* ((fd-ir (cadr ir))
            (fd-code (codegen fd-ir rtaddrs fnoffs td)))
       (append-all
        (list fd-code
              (arm64:lsr 0 0 4 :imm t)                      ; untag fd
              (list (list :extern-call "_close"))
              (arm64:lsl 0 0 4 :imm t)))))
    ((has-tag ir 'sys-exit-ir)
     ;; sys-exit-ir = (sys-exit-ir code-ir)
     ;; Calls _exit(code) -> does not return
     (let* ((code-ir (cadr ir))
            (code-code (codegen code-ir rtaddrs fnoffs td)))
       (append-all
        (list code-code
              (arm64:lsr 0 0 4 :imm t)                      ; untag exit code
              (list (list :extern-call "_exit"))))))
    (t (arm64:movz 0 0))))

;;; ============================================================
;;; Part 8: Multi-Function Compiler
;;; ============================================================

(defun compile-defun (name params body env fenv)
  "Compile a function definition. Handles &key parameters by treating them
   as additional positional params (keyword rewriting happens at call site)."
  (let* ((parsed (parse-lambda-list params))
         (positional-params (car parsed))
         (keyword-specs (cdr parsed))
         (keyword-names (mapcar #'car keyword-specs))
         ;; All params in order: positional then keyword names
         (all-params (append positional-params keyword-names))
         (bs (mapcar (lambda (p) (list p)) all-params))
         (penv (env-extend bs env))
         (pb (if all-params (env-lookup (car all-params) penv) 0))
         (rfenv (cons (cons name nil) fenv))
         ;; Apply mutable capture boxing transformation before compiling
         (transformed-body (box-mutable-captures body))
         (bir (sys:compile transformed-body penv rfenv)))
    ;; Return all-params (without &key) for codegen to use
    (list name all-params bir pb)))

;; Two-pass compilation for mutual recursion support
;; Pass 1: Collect all defun names into fenv with placeholder entries
;; Pass 2: Compile function bodies with complete fenv

(defun collect-defun-names (forms acc)
  "Pass 1: Collect all defun names and param info from forms, recursing into progn.
   Returns alist of (name . parsed-lambda-list) where parsed-lambda-list is
   (positional-params . keyword-specs)."
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (let* ((name (cadr f))
                  (params (caddr f))
                  (parsed (parse-lambda-list params)))
             (collect-defun-names (cdr forms) (cons (cons name parsed) acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (collect-defun-names (cdr forms)
                                   (collect-defun-names (cdr f) acc)))
          (t (collect-defun-names (cdr forms) acc))))))

(defun compile-defuns (forms env fenv acc)
  "Pass 2: Compile all defuns using complete fenv, recursing into progn"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (let* ((nm (cadr f))
                  (ps (caddr f))
                  (body-forms (cdddr f))
                  (bd (if (null (cdr body-forms))
                          (car body-forms)
                          (cons 'progn body-forms)))
                  (cf (compile-defun nm ps bd env fenv)))
             (compile-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (compile-defuns (cdr forms) env fenv
                              (compile-defuns (cdr f) env fenv acc)))
          (t (compile-defuns (cdr forms) env fenv acc))))))

(defun find-main-form (forms)
  "Find all non-defun forms and wrap them in progn if more than one.
   Recurses into progn forms to strip nested defuns."
  (labels ((strip-defuns (fs acc)
             ;; Recursively collect non-defun forms, flattening progn
             (if (null fs)
                 acc
                 (let ((f (car fs)))
                   (cond
                     ((and (consp f) (eq (car f) 'defun))
                      ;; Skip defuns
                      (strip-defuns (cdr fs) acc))
                     ((and (consp f) (eq (car f) 'progn))
                      ;; Recurse into progn, flatten results
                      (strip-defuns (cdr fs)
                                    (strip-defuns (cdr f) acc)))
                     (t
                      ;; Keep other forms
                      (strip-defuns (cdr fs) (cons f acc))))))))
    (let ((main-forms (reverse (strip-defuns forms nil))))
      (cond ((null main-forms) nil)
            ((null (cdr main-forms)) (car main-forms))
            (t (cons 'progn main-forms))))))

(defun compile-forms (forms)
  "Two-pass compilation: first collect names, then compile with complete fenv"
  ;; Pass 1: Collect all defun names
  (let* ((fn-names (collect-defun-names forms nil))
         ;; Build fenv with all function names as placeholders
         (fenv fn-names))
    ;; Pass 2: Compile all defuns with complete fenv
    (let* ((compiled-fns (reverse (compile-defuns forms nil fenv nil)))
           ;; Find and compile the main expression
           (main-form (find-main-form forms))
           (main-ir (if main-form (sys:compile main-form nil fenv) (list 'lit 0))))
      (list compiled-fns main-ir))))

(defun gen-param-stores (params base idx acc &key leaf)
  "Store function parameters to stack frame.
   Args 0-7 come from registers x0-x7.
   Args 8+ come from caller's stack at [sp + frame_size + (i-8)*8].
   Frame size is 0x200 for leaf functions, 0x400 for non-leaf."
  (if (null params)
      acc
      (let* ((frame-size (if leaf #x1000 #x1000))  ; Must match fn-prologue - now 4KB for all functions
             (st (if (< idx 8)
                     ;; Args 0-7: copy from register xi to stack
                     (append (arm64:mov 22 idx)
                             (arm64:sub 21 20 (* (+ base idx) 8) :imm t)
                             (arm64:str 22 21 :offset 0))
                     ;; Args 8+: load from caller's stack, store to our env frame
                     ;; Caller's stack args are at [sp + frame_size + (i-8)*8]
                     (let ((stack-off (+ frame-size (* (- idx 8) 8))))
                       (append (arm64:ldr 22 31 :offset stack-off)
                               (arm64:sub 21 20 (* (+ base idx) 8) :imm t)
                               (arm64:str 22 21 :offset 0))))))
        (gen-param-stores (cdr params) base (+ idx 1) (append acc st) :leaf leaf))))

(defun fn-prologue (frame-size x20-offset &key leaf)
  "Function prologue: allocate frame, save caller's x20/lr/x24, set up new env base.
   Frame size and x20 offset are dynamically calculated based on function needs.
   x24 must be preserved across calls so defuns with internal labels don't clobber
   the caller's closure environment.
   If :leaf t, skip x24 save (leaf functions don't call other functions)."
  (if leaf
      ;; Leaf function: skip x24 save
      (append
       (arm64:sub 31 31 frame-size :imm t)   ; SUB sp, sp, #frame-size
       (arm64:stp 20 30 31 :offset 0)      ; STP x20, lr, [sp, #0] (save x20 and return addr)
       (arm64:add 20 31 x20-offset :imm t))  ; ADD x20, sp, #x20-offset (env base)
      ;; Non-leaf function: full frame with x24 save
      (append
       (arm64:sub 31 31 frame-size :imm t)   ; SUB sp, sp, #frame-size (allocate function frame)
       (arm64:stp 20 30 31 :offset 0)      ; STP x20, lr, [sp, #0] (save caller's x20 and return addr)
       (arm64:str 24 31 :offset 16)        ; STR x24, [sp, #16] (save caller's closure env)
       (arm64:add 20 31 x20-offset :imm t)))) ; ADD x20, sp, #x20-offset (env base past spill area)

(defun fn-epilogue (frame-size &key leaf)
  "Function epilogue: restore caller's x20/lr/x24, deallocate frame, return
   If :leaf t, skip x24 restore."
  (if leaf
      ;; Leaf function: skip x24 restore
      (append
       (arm64:ldp 20 30 31 :offset 0)    ; LDP x20, lr, [sp, #0] (restore x20 and lr)
       (arm64:add 31 31 frame-size :imm t))  ; ADD sp, sp, #frame-size (deallocate leaf frame)
      ;; Non-leaf function: full restore
      (append
       (arm64:ldr 24 31 :offset 16)       ; LDR x24, [sp, #16] (restore caller's closure env)
       (arm64:ldp 20 30 31 :offset 0)     ; LDP x20, lr, [sp, #0] (restore caller's x20 and lr)
       (arm64:add 31 31 frame-size :imm t)))) ; ADD sp, sp, #frame-size (deallocate function frame)

(defun gen-capture-copies (count idx acc)
  "Generate code to copy captured values from closure env (x24) to stack.
   x24 points to a cons list of captured values: (val1 . (val2 . nil)).
   We traverse the list extracting car values and storing to stack slots.
   After all copies, x24 should be nil."
  (if (>= idx count)
      acc
      (let* ((copy-code
              (append-all
               (list
                ;; x24 is current cons cell (tagged with 1)
                ;; Clear cons tag: x9 = x24 & ~0xF
                (arm64:movz 11 #xF)
                (arm64:bic 9 24 11)
                ;; Get car (the captured value): x0 = [x9+0]
                (arm64:ldr 0 9 :offset 0)
                ;; Store result to stack slot idx
                (arm64:sub 21 20 (* idx 8) :imm t)
                (arm64:str 0 21 :offset 0)
                ;; Move x24 to cdr (next cons cell): x24 = [x9+8]
                (arm64:ldr 24 9 :offset 8)))))
        (gen-capture-copies count (+ idx 1) (append acc copy-code)))))

(defun save-params-to-temps (count idx acc)
  "Save param registers x0..xN to temp slots 200+idx to preserve them during capture copy.
   Temp slots 200+ are used to avoid conflict with body temps."
  (if (>= idx count)
      acc
      (let* ((temp-slot (+ 200 idx))
             (off (* temp-slot 8))
             (save-code (append-all
                         (list
                          (arm64:sub 21 20 off :imm t)
                          (arm64:str idx 21 :offset 0)))))
        (save-params-to-temps count (+ idx 1) (append acc save-code)))))

(defun restore-params-from-temps (params base count idx acc)
  "Restore params from temp slots and store to final slots at base+idx."
  (if (null params)
      acc
      (let* ((temp-slot (+ 200 idx))
             (temp-off (* temp-slot 8))
             (final-off (* (+ base idx) 8))
             (restore-code (append-all
                            (list
                             ;; Load from temp slot
                             (arm64:sub 21 20 temp-off :imm t)
                             (arm64:ldr 22 21 :offset 0)
                             ;; Store to final slot
                             (arm64:sub 21 20 final-off :imm t)
                             (arm64:str 22 21 :offset 0)))))
        (restore-params-from-temps (cdr params) base count (+ idx 1) (append acc restore-code)))))

(defun count-max-env-offset (ir)
  "Count the maximum environment offset used in IR (for let bindings).
   This is needed to check if leaf optimization is safe."
  (cond
    ((null ir) 0)
    ((not (consp ir)) 0)
    ;; Skip alist pairs like (CODE . 0) or (FNOFFS . 1)
    ((and (consp ir) (atom (cdr ir))) 0)
    ((has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count (offs...))
     ;; The offs list contains the offsets used
     (let* ((offs (nth 3 (cdr ir)))
            (max-off (if offs (apply #'max offs) 0))
            (body-max (count-max-env-offset (caddr ir))))
       (max max-off body-max)))
    ((has-tag ir 'if-ir)
     (max (count-max-env-offset (cadr ir))
          (count-max-env-offset (caddr ir))
          (count-max-env-offset (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (apply #'max 0 (mapcar #'count-max-env-offset (cadr ir))))
    ((has-tag ir 'dolist-ir)
     ;; dolist-ir has body at (cadddr ir)
     (count-max-env-offset (cadddr ir)))
    (t
     ;; Check children for other IR nodes, filtering out non-list elements
     (apply #'max 0 (mapcar #'count-max-env-offset
                            (remove-if-not #'consp (cdr ir)))))))

(defun count-max-temp-depth (ir depth)
  "Count the maximum temp depth reached during codegen of IR.
   Temp depth increases during nested expression evaluation."
  (cond
    ((null ir) depth)
    ((not (consp ir)) depth)
    ;; Skip alist pairs like (CODE . 0) or (FNOFFS . 1)
    ((and (consp ir) (atom (cdr ir))) depth)
    ;; Literals and vars don't use temps
    ((or (has-tag ir 'lit) (has-tag ir 'var-ref)) depth)
    ;; Binary ops: depth increases by amount needed for saving x24 + operands
    ((or (has-tag ir 'add-ir) (has-tag ir 'sub-ir) (has-tag ir 'mul-ir)
         (has-tag ir 'div-ir) (has-tag ir 'mod-ir) (has-tag ir 'cons-ir)
         (has-tag ir 'cmp-eq) (has-tag ir 'cmp-lt) (has-tag ir 'cmp-gt)
         (has-tag ir 'cmp-le) (has-tag ir 'cmp-ge))
     (let* ((left-depth (count-max-temp-depth (cadr ir) (+ depth 2)))
            (right-depth (count-max-temp-depth (caddr ir) (+ depth 2))))
       (max left-depth right-depth)))
    ;; Let bindings: each binding uses temps, body uses temps
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (val-depths (mapcar (lambda (v) (count-max-temp-depth v (+ depth 2))) vals))
            (body-depth (count-max-temp-depth bir (+ depth 2))))
       (apply #'max body-depth val-depths)))
    ;; If: all branches
    ((has-tag ir 'if-ir)
     (max (count-max-temp-depth (cadr ir) (+ depth 1))
          (count-max-temp-depth (caddr ir) depth)
          (count-max-temp-depth (cadddr ir) depth)))
    ;; Progn: all forms
    ((has-tag ir 'progn-ir)
     (apply #'max depth (mapcar (lambda (f) (count-max-temp-depth f depth)) (cadr ir))))
    ;; Dolist: check body
    ((has-tag ir 'dolist-ir)
     (count-max-temp-depth (cadddr ir) depth))
    ;; Function calls: args + closure env
    ((has-tag ir 'call-fn)
     (let* ((args (caddr ir))
            (arg-depths (mapcar (lambda (a) (count-max-temp-depth a (+ depth 3))) args)))
       (apply #'max (+ depth 3) arg-depths)))
    ((has-tag ir 'funcall-ir)
     (let* ((closure (cadr ir))
            (args (caddr ir))
            (closure-depth (count-max-temp-depth closure (+ depth 2)))
            (arg-depths (mapcar (lambda (a) (count-max-temp-depth a (+ depth 4))) args)))
       (apply #'max closure-depth (+ depth 4) arg-depths)))
    ;; Default: check all children, filtering out non-list elements
    (t
     (apply #'max depth (mapcar (lambda (child) (count-max-temp-depth child depth))
                                (remove-if-not #'consp (cdr ir)))))))

(defun codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function (defun or lifted lambda).
   Defun format:  (name params body param-base)  ; param-base is a number
   Lambda format: (name params body free-vars free-offsets)  ; free-vars is a list or nil
   Uses dynamically-sized stack frames based on variable count and temp depth."
  (let* ((ps (cadr fn))
         (bir (caddr fn))
         (fourth (cadddr fn))
         ;; Calculate frame requirements
         (num-params (length ps))
         (max-let-offset (count-max-env-offset bir))
         (max-env-size (max num-params (1+ max-let-offset)))
         (max-temp-depth (count-max-temp-depth bir 0))
         ;; Calculate dynamic frame size
         ;; Layout: [saved regs+padding: 64] [temps: temp_depth*8] [env: env_size*8] [safety: 64]
         ;; Note: temp-slot uses base #x40 (64), so saved regs area is 64 bytes
         ;; IMPORTANT: spill-slot uses base #x240, so functions that call other functions
         ;; need a frame of at least #x400 to have room for spill slots
         (makes-calls (ir-may-call? bir))
         (saved-regs 64)
         (temp-area (* (+ max-temp-depth 8) 8))  ; +8 for safety margin
         (env-area (* (+ max-env-size 8) 8))     ; +8 for safety margin
         (frame-size-raw (+ saved-regs temp-area env-area 64))
         ;; Round up to 16-byte alignment, with minimum #x400 for calling functions
         (frame-size-aligned (logand (+ frame-size-raw 15) (lognot 15)))
         ;; Use #x400 minimum for calling functions
         ;; Dynamic frame-size-aligned should handle actual needs
         ;; Reduced from #x800 to allow deeper recursion (8MB stack / 1KB = 8192 calls)
         (frame-size (if makes-calls
                         (max #x400 frame-size-aligned)
                         frame-size-aligned))
         ;; x20 offset = saved regs + temp area + env space (Bug #20 FIX)
         ;; Variables accessed as [x20 - offset*8], so x20 must be high enough
         ;; that var[max-env-size-1] = x20 - (max-env-size-1)*8 is above temp/spill area
         ;; For calling functions, spill slots are at #x240, so x20 must be past that
         (spill-end (if makes-calls #x440 0))  ; #x440 = #x240 + 512 (8 call levels)
         (x20-offset-raw (+ saved-regs temp-area (* max-env-size 8)))
         (x20-offset (max x20-offset-raw (+ spill-end (* max-env-size 8))))
         ;; Leaf optimization: only for non-calling functions with no >8 params
         ;; and max-env-size < 12 to avoid temp slot collision (Bug #19/#20)
         (is-leaf (and (not makes-calls)
                       (<= num-params 8)
                       (< max-env-size 12))))
    ;; Distinguish defun from lambda by checking 4th element
    ;; Defuns have a number (param-base), lambdas have nil or a list (free-vars)
    ;; Note: Lifted lambdas (via lambdas-to-defuns) have param-base > 0 which means
    ;; they have captured variables that need to be copied from x24 to stack slots.
    (if (numberp fourth)
        ;; Defun or lifted lambda: params start at param-base
        ;; If param-base > 0, this is a lifted lambda with captures
        (let* ((pb fourth)
               (param-count (length ps))
               (has-captures (> pb 0)))
          (if has-captures
              ;; Lifted lambda: save params, copy captures, restore params
              ;; This matches the lambda path logic below
              (let* ((leaf-ok nil)  ;; captures need x24, so no leaf optimization
                     (ps-save (save-params-to-temps param-count 0 nil))
                     (cc (gen-capture-copies pb 0 nil))
                     (pc (restore-params-from-temps ps pb param-count 0 nil))
                     (bc (codegen bir rtaddrs fnoffs 0)))
                (append (fn-prologue frame-size x20-offset :leaf leaf-ok)
                        ps-save cc pc bc
                        (fn-epilogue frame-size :leaf leaf-ok)
                        (arm64:ret)))
              ;; Regular defun: just store params
              (let* ((pc (gen-param-stores ps pb 0 nil :leaf is-leaf))
                     (bc (codegen bir rtaddrs fnoffs 0)))
                (append (fn-prologue frame-size x20-offset :leaf is-leaf)
                        pc bc
                        (fn-epilogue frame-size :leaf is-leaf)
                        (arm64:ret)))))
        ;; Lambda: need to copy captures AND store params
        ;; Problem: capture copy clobbers x0-x4, but params are in x0-x4
        ;; Solution: save params to temp slots first, copy captures, then restore params
        ;; Note: Lambdas with captures cannot be leaf-optimized (capture copy uses x24)
        (let* ((free-vars fourth)
               (capture-count (if free-vars (length free-vars) 0))
               (param-count (length ps))
               ;; Save params to temp slots before they get clobbered
               (ps-save (if (> capture-count 0)
                            (save-params-to-temps param-count 0 nil)
                            nil))
               ;; Copy captured values from x24 (closure env) to stack slots 0..N-1
               (cc (gen-capture-copies capture-count 0 nil))
               ;; Restore params from temp slots to final slots N..N+M-1
               ;; Leaf optimize only if no captures (captures need x24)
               (leaf-ok (and is-leaf (= capture-count 0)))
               (pc (if (> capture-count 0)
                       (restore-params-from-temps ps capture-count param-count 0 nil)
                       (gen-param-stores ps 0 0 nil :leaf leaf-ok)))
               (bc (codegen bir rtaddrs fnoffs 0)))
          (append (fn-prologue frame-size x20-offset :leaf leaf-ok)
                  ps-save cc pc bc
                  (fn-epilogue frame-size :leaf leaf-ok)
                  (arm64:ret))))))

(defun codegen-main (mir rtaddrs)
  (append (prologue)
          (codegen mir rtaddrs nil 0)
          (epilogue)))

(defparameter *lambda-counter* 0)

(defun gensym-lambda ()
  "Generate unique lambda name"
  (incf *lambda-counter*)
  (intern (sys:string-concat "LAMBDA-" (sys:number-to-string *lambda-counter*))))

(defun lift-lambdas (ir)
  "Extract all lambda-ir nodes from IR, replacing them with lambda-ref nodes.
   Returns (cons transformed-ir lambdas) where lambdas is alist of (name . lambda-ir)"
  (labels ((lift (ir lambdas)
             (cond
               ((null ir) (cons ir lambdas))
               ((not (consp ir)) (cons ir lambdas))
               ((has-tag ir 'lambda-ir)
                ;; Found a lambda - give it a name, store it, return reference
                (let* ((name (gensym-lambda))
                       (params (cadr ir))
                       (body (caddr ir))
                       (free-vars (cadddr ir))
                       (free-offsets (nth 4 ir)))
                  ;; Recursively lift lambdas from the body
                  (let* ((mvb-result-1 (lift body lambdas)) (new-body (car mvb-result-1)) (more-lambdas (cdr mvb-result-1)))
                    (let ((lambda-entry (list name params new-body free-vars free-offsets)))
                      (cons (list 'lambda-ref name free-offsets)
                              (cons lambda-entry more-lambdas))))))
               ((has-tag ir 'let-ir)
                ;; let-ir = (let-ir vals bir count offs)
                (let ((vals (cadr ir))
                      (bir (caddr ir))
                      (count (cadddr ir))
                      (offs (nth 4 ir)))
                  (let* ((mvb-result-2 (lift-list vals lambdas)) (new-vals (car mvb-result-2)) (lambdas1 (cdr mvb-result-2)))
                    (let* ((mvb-result-26 (lift bir lambdas1)) (new-bir (car mvb-result-26)) (lambdas2 (cdr mvb-result-26)))
                    (cons (list 'let-ir new-vals new-bir count offs) lambdas2)))))
               ((has-tag ir 'if-ir)
                (let ((test (cadr ir))
                      (then (caddr ir))
                      (else (cadddr ir)))
                  (let* ((mvb-result-3 (lift test lambdas)) (new-test (car mvb-result-3)) (l1 (cdr mvb-result-3)))
                    (let* ((mvb-result-27 (lift then l1)) (new-then (car mvb-result-27)) (l2 (cdr mvb-result-27)))
                    (let* ((mvb-result-37 (lift else l2)) (new-else (car mvb-result-37)) (l3 (cdr mvb-result-37)))
                    (cons (list 'if-ir new-test new-then new-else) l3))))))
               ((has-tag ir 'progn-ir)
                (let* ((mvb-result-4 (lift-list (cadr ir) lambdas)) (new-forms (car mvb-result-4)) (new-lambdas (cdr mvb-result-4)))
                    (cons (list 'progn-ir new-forms) new-lambdas)))
               ((has-tag ir 'funcall-ir)
                (let ((fn-ir (cadr ir))
                      (args-ir (caddr ir)))
                  (let* ((mvb-result-5 (lift fn-ir lambdas)) (new-fn (car mvb-result-5)) (l1 (cdr mvb-result-5)))
                    (let* ((mvb-result-28 (lift-list args-ir l1)) (new-args (car mvb-result-28)) (l2 (cdr mvb-result-28)))
                    (cons (list 'funcall-ir new-fn new-args) l2)))))
               ((has-tag ir 'call-fn)
                (let ((name (cadr ir))
                      (args-ir (caddr ir)))
                  (let* ((mvb-result-6 (lift-list args-ir lambdas)) (new-args (car mvb-result-6)) (new-lambdas (cdr mvb-result-6)))
                    (cons (list 'call-fn name new-args) new-lambdas))))
               ((has-tag ir 'tail-call-fn)
                (let ((name (cadr ir))
                      (args-ir (caddr ir)))
                  (let* ((mvb-result-7 (lift-list args-ir lambdas)) (new-args (car mvb-result-7)) (new-lambdas (cdr mvb-result-7)))
                    (cons (list 'tail-call-fn name new-args) new-lambdas))))
               ((or (has-tag ir 'add) (has-tag ir 'sub)
                    (has-tag ir 'mul) (has-tag ir 'div)
                    (has-tag ir 'mod) (has-tag ir 'cmp-eq)
                    (has-tag ir 'cmp-lt) (has-tag ir 'cmp-gt)
                    (has-tag ir 'cmp-le) (has-tag ir 'cmp-ge)
                    (has-tag ir 'cons-ir)
                    (has-tag ir 'band) (has-tag ir 'bor)
                    (has-tag ir 'bxor) (has-tag ir 'bsh))
                (let ((left (cadr ir))
                      (right (caddr ir)))
                  (let* ((mvb-result-8 (lift left lambdas)) (new-left (car mvb-result-8)) (l1 (cdr mvb-result-8)))
                    (let* ((mvb-result-29 (lift right l1)) (new-right (car mvb-result-29)) (l2 (cdr mvb-result-29)))
                    (cons (list (car ir) new-left new-right) l2)))))
               ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir))
                (let* ((mvb-result-9 (lift (cadr ir) lambdas)) (new-arg (car mvb-result-9)) (new-lambdas (cdr mvb-result-9)))
                    (cons (list (car ir) new-arg) new-lambdas)))
               ((has-tag ir 'setq-ir)
                ;; setq-ir = (setq-ir offset value-ir)
                (let ((offset (cadr ir))
                      (val-ir (caddr ir)))
                  (let* ((mvb-result-10 (lift val-ir lambdas)) (new-val (car mvb-result-10)) (new-lambdas (cdr mvb-result-10)))
                    (cons (list 'setq-ir offset new-val) new-lambdas))))
               ((has-tag ir 'dotimes-ir)
                ;; dotimes-ir = (dotimes-ir var count-ir body-ir result-ir compile-env)
                (let ((var (cadr ir))
                      (count-ir (caddr ir))
                      (body-ir (cadddr ir))
                      (result-ir (nth 4 ir))
                      (compile-env (nth 5 ir)))
                  (let* ((mvb-result-11 (lift count-ir lambdas)) (new-count (car mvb-result-11)) (l1 (cdr mvb-result-11)))
                    (let* ((mvb-result-30 (lift body-ir l1)) (new-body (car mvb-result-30)) (l2 (cdr mvb-result-30)))
                    (let* ((mvb-result-38 (lift result-ir l2)) (new-result (car mvb-result-38)) (l3 (cdr mvb-result-38)))
                    (cons (list 'dotimes-ir var new-count new-body new-result compile-env) l3))))))
               ((has-tag ir 'dolist-ir)
                ;; dolist-ir = (dolist-ir var list-ir body-ir result-ir compile-env)
                (let ((var (cadr ir))
                      (list-ir (caddr ir))
                      (body-ir (cadddr ir))
                      (result-ir (nth 4 ir))
                      (compile-env (nth 5 ir)))
                  (let* ((mvb-result-12 (lift list-ir lambdas)) (new-list (car mvb-result-12)) (l1 (cdr mvb-result-12)))
                    (let* ((mvb-result-31 (lift body-ir l1)) (new-body (car mvb-result-31)) (l2 (cdr mvb-result-31)))
                    (let* ((mvb-result-39 (lift result-ir l2)) (new-result (car mvb-result-39)) (l3 (cdr mvb-result-39)))
                    (cons (list 'dolist-ir var new-list new-body new-result compile-env) l3))))))
               ;; 3-arg IR nodes: (tag arg1 arg2 arg3)
               ((or (has-tag ir 'vector-set-ir)
                    ;; sys-* IR nodes with 3 arguments
                    (has-tag ir 'sys-write-ir)
                    (has-tag ir 'sys-read-ir)
                    (has-tag ir 'sys-open-ir))
                (let ((arg1 (cadr ir))
                      (arg2 (caddr ir))
                      (arg3 (cadddr ir)))
                  (let* ((mvb-result-13 (lift arg1 lambdas)) (new-arg1 (car mvb-result-13)) (l1 (cdr mvb-result-13)))
                    (let* ((mvb-result-32 (lift arg2 l1)) (new-arg2 (car mvb-result-32)) (l2 (cdr mvb-result-32)))
                    (let* ((mvb-result-40 (lift arg3 l2)) (new-arg3 (car mvb-result-40)) (l3 (cdr mvb-result-40)))
                    (cons (list (car ir) new-arg1 new-arg2 new-arg3) l3))))))
               ;; 2-arg IR nodes: (tag arg1 arg2)
               ((or (has-tag ir 'vector-ref-ir)
                    (has-tag ir 'buffer-byte-ref-ir)
                    (has-tag ir 'buffer-to-string-ir)
                    (has-tag ir 'string-ref-ir)
                    (has-tag ir 'string-equal-ir))
                (let ((arg1 (cadr ir))
                      (arg2 (caddr ir)))
                  (let* ((mvb-result-14 (lift arg1 lambdas)) (new-arg1 (car mvb-result-14)) (l1 (cdr mvb-result-14)))
                    (let* ((mvb-result-33 (lift arg2 l1)) (new-arg2 (car mvb-result-33)) (l2 (cdr mvb-result-33)))
                    (cons (list (car ir) new-arg1 new-arg2) l2)))))
               ;; 1-arg IR nodes: (tag arg)
               ((or (has-tag ir 'make-vector-ir)
                    (has-tag ir 'make-string-from-vector-ir)
                    (has-tag ir 'make-symbol-from-string-ir)
                    (has-tag ir 'symbol-name-ir)
                    (has-tag ir 'string-length-ir)
                    (has-tag ir 'vector-length-ir)
                    (has-tag ir 'system-ir)
                    (has-tag ir 'null-ir) (has-tag ir 'consp-ir)
                    (has-tag ir 'symbolp-ir) (has-tag ir 'stringp-ir)
                    (has-tag ir 'vectorp-ir) (has-tag ir 'numberp-ir)
                    ;; sys-* IR nodes with 1 argument
                    (has-tag ir 'sys-exit-ir)
                    (has-tag ir 'sys-close-ir))
                (let* ((mvb-result-15 (lift (cadr ir) lambdas)) (new-arg (car mvb-result-15)) (new-lambdas (cdr mvb-result-15)))
                    (cons (list (car ir) new-arg) new-lambdas)))
               ;; Self-TCO loop constructs
               ((has-tag ir 'loop-ir)
                (let* ((mvb-result-16 (lift (cadr ir) lambdas)) (new-body (car mvb-result-16)) (new-lambdas (cdr mvb-result-16)))
                    (cons (list 'loop-ir new-body) new-lambdas)))
               ((has-tag ir 'continue-ir)
                (let* ((mvb-result-17 (lift-list (cadr ir) lambdas)) (new-args (car mvb-result-17)) (new-lambdas (cdr mvb-result-17)))
                    (cons (list 'continue-ir new-args) new-lambdas)))
               ;; while-ir = (while-ir test body)
               ((has-tag ir 'while-ir)
                (let ((test (cadr ir))
                      (body (caddr ir)))
                  (let* ((mvb-result-w1 (lift test lambdas)) (new-test (car mvb-result-w1)) (l1 (cdr mvb-result-w1)))
                    (let* ((mvb-result-w2 (lift body l1)) (new-body (car mvb-result-w2)) (l2 (cdr mvb-result-w2)))
                    (cons (list 'while-ir new-test new-body) l2)))))
               (t (cons ir lambdas))))
           (lift-list (irs lambdas)
             (if (null irs)
                 (cons nil lambdas)
                 (let* ((mvb-result-18 (lift (car irs) lambdas)) (new-first (car mvb-result-18)) (l1 (cdr mvb-result-18)))
                    (let* ((mvb-result-34 (lift-list (cdr irs) l1)) (new-rest (car mvb-result-34)) (l2 (cdr mvb-result-34)))
                    (cons (cons new-first new-rest) l2))))))
    (lift ir nil)))

(defun codegen-lambda (lambda-entry rtaddrs fnoffs)
  "Generate code for a lifted lambda.
   lambda-entry = (name params body free-vars free-offsets)"
  (let* ((params (cadr lambda-entry))
         (body (caddr lambda-entry))
         ;; Lambda params start at offset 0
         (pb 0)
         (pc (gen-param-stores params pb 0 nil))
         (bc (codegen body rtaddrs fnoffs 0)))
    (append pc bc (arm64:ret))))

(defun code-size (code)
  "Calculate byte size of code that may contain call and loop markers."
  (labels ((calc (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                     ((and (consp item) (eq (car item) :loop-start))
                      ;; Loop start marker - no bytes
                      (calc (cdr items) acc))
                     ((and (consp item)
                           (or (eq (car item) :call-fn)
                               (eq (car item) :tail-call-fn)
                               (eq (car item) :extern-call)
                               (eq (car item) :loop-continue)))
                      ;; 4-byte instructions
                      (calc (cdr items) (+ acc 4)))
                     (t
                      (calc (cdr items) (+ acc 1))))))))
    (calc code 0)))

;; build-fnoffs is now defined in codegen.lisp with iteration until stable
;; (the codegen.lisp version handles the case where code size depends on offsets)

(defun codegen-all-fns (fns rtaddrs fnoffs acc)
  "Generate code for all functions with correct fnoffs."
  (if (null fns)
      acc
      (let* ((fn (car fns))
             (code (codegen-fn fn rtaddrs fnoffs)))
        (codegen-all-fns (cdr fns) rtaddrs fnoffs (append acc code)))))

(defun lift-lambdas-from-fns (fns acc-fns acc-lambdas)
  "Lift lambdas from all function bodies.
   Returns (cons lifted-fns all-lambdas) where:
   - lifted-fns has lambda-ir replaced with lambda-ref in bodies
   - all-lambdas is list of all lifted lambda definitions"
  (if (null fns)
      (cons (reverse acc-fns) acc-lambdas)
      (let* ((fn (car fns))
             (name (car fn))
             (params (cadr fn))
             (body (caddr fn))
             (fourth (cadddr fn)))
        (let* ((mvb-result-19 (lift-lambdas body)) (new-body (car mvb-result-19)) (lambdas (cdr mvb-result-19)))
                    (let ((new-fn (list name params new-body fourth)))
            (lift-lambdas-from-fns (cdr fns)
                                      (cons new-fn acc-fns)
                                      (append acc-lambdas lambdas)))))))

(defun compile-program (forms rtaddrs &key (optimize t))
  "Compile forms to bytecode with function linking.
   Layout: prologue + main-code + epilogue + functions + lifted-lambdas
   Functions are placed after main, and call-fn generates forward BL.
   When :optimize is t, runs nanopass optimization pipeline."
  ;; Reset symbol table for fresh compilation
  (reset-symbol-table)
  (let* ((r (compile-forms forms))
         (defun-fns (car r))
         (mir-raw (cadr r))
         ;; Apply nanopass optimizations if enabled
         ;; CRITICAL: let-flattening and progn-flattening reduce IR depth from 100+ to ~10
         (mir-opt (if (and optimize (fboundp 'optimize-ir))
                      (optimize-ir mir-raw :passes '(let-flattening progn-flattening constant-folding strength-reduction dead-code-elimination))
                      mir-raw))
         ;; Function bodies get standard optimizations
         ;; Note: Self-TCO is disabled - the continue-ir overhead is > call overhead
         (defun-fns-opt (if (and optimize (fboundp 'optimize-ir))
                            (mapcar (lambda (fn)
                                      (list (first fn)
                                            (second fn)
                                            (optimize-ir (third fn) :passes '(let-flattening progn-flattening constant-folding strength-reduction dead-code-elimination))
                                            (fourth fn)
                                            (fifth fn)))
                                    defun-fns)
                            defun-fns)))
    ;; Lift lambdas from main IR (use optimized IR)
    (let* ((mvb-result-20 (lift-lambdas mir-opt)) (mir (car mvb-result-20)) (main-lambdas (cdr mvb-result-20)))
                    ;; Lift lambdas from all defun bodies (use optimized defuns)
      (let* ((mvb-result-35 (lift-lambdas-from-fns defun-fns-opt nil nil)) (lifted-defuns (car mvb-result-35)) (defun-lambdas (cdr mvb-result-35)))
                    ;; Combine: defuns + main-lambdas + defun-lambdas
        (let ((fns (append lifted-defuns main-lambdas defun-lambdas)))
          (if (null fns)
              ;; No functions defined - simple case
              ;; Still need to resolve extern calls
              (resolve-calls (codegen-main mir rtaddrs) nil)
              ;; Functions defined - need linking
              (let* (;; First, generate main code with nil fnoffs to get size
                     ;; This code contains (:call-fn name) markers
                     (main-code-temp (append (prologue)
                                             (codegen mir rtaddrs nil 0)
                                             (epilogue)))
                     ;; Use code-size to handle markers
                     (main-size (code-size main-code-temp))
                     ;; Build fnoffs starting after main code
                     (fnoffs (build-fnoffs fns main-size))
                     ;; Generate main code again - markers remain, fnoffs now known
                     (main-code (append (prologue)
                                        (codegen mir rtaddrs fnoffs 0)
                                        (epilogue)))
                     ;; Generate function code with fnoffs (functions can call each other)
                     (fn-code (codegen-all-fns fns rtaddrs fnoffs nil))
                     ;; Combine all code (still has markers)
                     (all-code (append main-code fn-code)))
                ;; Resolve all markers to actual BL instructions
                (resolve-calls all-code fnoffs))))))))

;;; ============================================================
;;; Part 9: Entry Point
;;; ============================================================

(defun eval-forms (forms)
  "Compile and evaluate multiple forms, including defun.
   Uses two-pass approach to support mutual recursion:
   1. First pass: collect all defun names into fenv with placeholders
   2. Second pass: compile bodies with complete fenv, then evaluate non-defun forms"
  ;; Pass 1: Collect all defun names
  (labels ((collect-defuns (fs acc)
             (if (null fs)
                 (reverse acc)
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (collect-defuns (cdr fs) (cons (cadr f) acc))
                       (collect-defuns (cdr fs) acc)))))
           ;; Build initial fenv with placeholders
           (build-fenv (names acc)
             (if (null names)
                 acc
                 (build-fenv (cdr names) (cons (cons (car names) nil) acc))))
           ;; Compile all defuns with complete fenv
           (compile-defuns (fs fenv acc)
             (if (null fs)
                 (cons fenv (reverse acc))
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (let* ((nm (cadr f))
                              (ps (caddr f))
                              (bd (cadddr f))
                              (cf (compile-defun nm ps bd nil fenv))
                              (entry (assoc nm fenv)))
                         ;; Update existing entry with compiled function
                         (setf (cdr entry) cf)
                         (compile-defuns (cdr fs) fenv acc))
                       ;; Non-defun form - save for later evaluation
                       (compile-defuns (cdr fs) fenv (cons f acc))))))
           ;; Evaluate non-defun forms
           (eval-forms (fs fenv)
             (if (null fs)
                 0
                 (let* ((ir (sys:compile (car fs) nil fenv))
                        (result (eval-ir-with-fns ir nil fenv)))
                   (if (null (cdr fs))
                       result
                       (eval-forms (cdr fs) fenv))))))
    ;; Execute two-pass compilation
    (let* ((defun-names (collect-defuns forms nil))
           (initial-fenv (build-fenv defun-names nil)))
      (let* ((mvb-result-21 (compile-defuns forms initial-fenv nil)) (final-fenv (car mvb-result-21)) (other-forms (cdr mvb-result-21)))
                    (eval-forms other-forms final-fenv)))))

;;; ============================================================
;;; Part 9: Public API
;;; ============================================================

;;; Delivery functions

(defun bytes-to-c-array (bytes)
  "Convert byte list to C array initializer string"
  (with-output-to-string (s)
    (let ((col 0))
      (dolist (b bytes)
        (format s "0x~2,'0X," b)
        (incf col)
        (when (= col 16)
          (format s "~%    ")
          (setf col 0))))))

(defun generate-embedded-c (bytes output-name)
  "Generate C source with embedded bytecode"
  (format nil "/* Auto-generated by Habu - ~A */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <string.h>
#include \"runtime/habu.h\"

static const unsigned char g_bytecode[] = {
    ~A
};
static const size_t g_bytecode_size = ~A;

void* g_runtime_table[64];
typedef int64_t (*compiled_fn_t)(void** runtime_table);

int main(int argc, char **argv) {
    (void)argc; (void)argv;
    void *exec_mem = mmap(NULL, g_bytecode_size,
                          PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (exec_mem == MAP_FAILED) { perror(\"mmap\"); return 1; }
    memcpy(exec_mem, g_bytecode, g_bytecode_size);
    if (mprotect(exec_mem, g_bytecode_size, PROT_READ | PROT_EXEC) != 0) {
        perror(\"mprotect\"); munmap(exec_mem, g_bytecode_size); return 1;
    }
    init(1024 * 1024);
    g_runtime_table[0] = (void*)cons;
    g_runtime_table[1] = (void*)car;
    g_runtime_table[2] = (void*)cdr;
    g_runtime_table[3] = (void*)make_closure;
    g_runtime_table[4] = (void*)closure_code;
    g_runtime_table[5] = (void*)closure_env;
    g_runtime_table[6] = exec_mem;
    g_runtime_table[7] = (void*)make_vector;
    g_runtime_table[8] = (void*)vector_set;
    g_runtime_table[9] = (void*)vector_ref;
    g_runtime_table[10] = (void*)make_string_from_vector;
    g_runtime_table[11] = (void*)make_symbol_from_string;
    g_runtime_table[12] = (void*)string_length_raw;
    g_runtime_table[13] = (void*)symbol_name;
    g_runtime_table[14] = (void*)set_car;
    g_runtime_table[15] = (void*)set_cdr;
    g_runtime_table[16] = (void*)string_ref;
    g_runtime_table[17] = (void*)values_set;
    g_runtime_table[18] = (void*)values_get;
    g_runtime_table[19] = (void*)make_hash_table;
    g_runtime_table[20] = (void*)gethash;
    g_runtime_table[21] = (void*)puthash;
    g_runtime_table[22] = (void*)remhash;
    g_runtime_table[23] = (void*)hash_table_count;
    g_runtime_table[24] = (void*)string_concat;
    g_runtime_table[25] = (void*)string_substring;
    g_runtime_table[26] = (void*)fixnum_to_string;
    g_runtime_table[27] = (void*)values_count_get;
    g_runtime_table[28] = (void*)gensym;
    g_runtime_table[29] = (void*)make_float;
    g_runtime_table[30] = (void*)float_add;
    g_runtime_table[31] = (void*)float_sub;
    g_runtime_table[32] = (void*)float_mul;
    g_runtime_table[33] = (void*)float_div;
    g_runtime_table[34] = (void*)float_lt;
    g_runtime_table[35] = (void*)float_gt;
    g_runtime_table[36] = (void*)float_le;
    g_runtime_table[37] = (void*)float_ge;
    g_runtime_table[38] = (void*)float_eq;
    g_runtime_table[39] = (void*)fixnum_to_float;
    g_runtime_table[40] = (void*)float_to_fixnum;
    g_runtime_table[41] = (void*)float_value;
    g_runtime_table[42] = (void*)open_file;
    g_runtime_table[43] = (void*)close_file;
    g_runtime_table[44] = (void*)read_line;
    g_runtime_table[45] = (void*)write_string;
    g_runtime_table[46] = (void*)read_file;
    g_runtime_table[47] = (void*)write_file;
    g_runtime_table[48] = (void*)print_value;
    g_runtime_table[49] = (void*)println_value;
    g_runtime_table[50] = (void*)get_time_ns;
    g_runtime_table[51] = (void*)system_cmd;
    g_runtime_table[52] = (void*)string_equal;
    g_runtime_table[53] = (void*)write_bytes;
    compiled_fn_t fn = (compiled_fn_t)exec_mem;
    int64_t result = fn(g_runtime_table);
    printf(\"Result: %lld\\n\", result >> 4);
    munmap(exec_mem, g_bytecode_size);
    return 0;
}
"
          output-name
          (bytes-to-c-array bytes)
          (length bytes)))

;; deliver function is now in codegen.lisp (unified version)
;; deliver-file is also in codegen.lisp

;;; ============================================================
;;; Part 10: Disassembler
;;; ============================================================

(defun disassemble-arm64-instr (word addr)
  "Disassemble a single ARM64 instruction to string."
  (cond
      ;; MOVZ (64-bit): 1101 0010 1... = D28...
      ((= (logand word #xFF800000) #xD2800000)
       (let* ((rd (logand word #x1F))
              (imm16 (logand (ash word -5) #xFFFF))
              (hw (logand (ash word -21) #x3)))
         (format nil "MOVZ x~D, #0x~X~@[, LSL #~D~]"
                 rd imm16 (if (> hw 0) (* hw 16) nil))))
      ;; MOVK (64-bit): 1111 0010 1... = F28...
      ((= (logand word #xFF800000) #xF2800000)
       (let* ((rd (logand word #x1F))
              (imm16 (logand (ash word -5) #xFFFF))
              (hw (logand (ash word -21) #x3)))
         (format nil "MOVK x~D, #0x~X~@[, LSL #~D~]"
                 rd imm16 (if (> hw 0) (* hw 16) nil))))
      ;; MOV (ORR with XZR)
      ((and (= (logand word #xFF0003E0) #xAA0003E0))
       (let ((rd (logand word #x1F))
             (rm (logand (ash word -16) #x1F)))
         (format nil "MOV x~D, x~D" rd rm)))
      ;; ADD immediate 64-bit: 1001 0001 xx... = 91...
      ((= (logand word #xFF000000) #x91000000)
       (let* ((rd (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (imm12 (logand (ash word -10) #xFFF)))
         (format nil "ADD x~D, x~D, #~D" rd rn imm12)))
      ;; SUB immediate 64-bit: 1101 0001 xx... = D1...
      ((= (logand word #xFF000000) #xD1000000)
       (let* ((rd (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (imm12 (logand (ash word -10) #xFFF)))
         (format nil "SUB x~D, x~D, #~D" rd rn imm12)))
      ;; ADD/SUB register
      ((or (= (logand word #x7F200000) #x0B000000)
           (= (logand word #x7F200000) #x4B000000))
       (let* ((rd (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (rm (logand (ash word -16) #x1F))
              (is-sub (= (logand word #x80000000) #x40000000)))
         (format nil "~A x~D, x~D, x~D"
                 (if is-sub "SUB" "ADD") rd rn rm)))
      ;; MUL
      ((= (logand word #x7FE0FC00) #x1B007C00)
       (let ((rd (logand word #x1F))
             (rn (logand (ash word -5) #x1F))
             (rm (logand (ash word -16) #x1F)))
         (format nil "MUL x~D, x~D, x~D" rd rn rm)))
      ;; SDIV
      ((= (logand word #x7FE0FC00) #x1AC00C00)
       (let ((rd (logand word #x1F))
             (rn (logand (ash word -5) #x1F))
             (rm (logand (ash word -16) #x1F)))
         (format nil "SDIV x~D, x~D, x~D" rd rn rm)))
      ;; LDR immediate (unsigned offset)
      ((= (logand word #xFFC00000) #xF9400000)
       (let* ((rt (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (imm12 (logand (ash word -10) #xFFF)))
         (format nil "LDR x~D, [x~D, #~D]" rt rn (* imm12 8))))
      ;; STR immediate (unsigned offset)
      ((= (logand word #xFFC00000) #xF9000000)
       (let* ((rt (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (imm12 (logand (ash word -10) #xFFF)))
         (format nil "STR x~D, [x~D, #~D]" rt rn (* imm12 8))))
      ;; LDP (load pair)
      ((= (logand word #xFFC00000) #xA9400000)
       (let* ((rt1 (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (rt2 (logand (ash word -10) #x1F))
              (imm7 (logand (ash word -15) #x7F))
              (offset (if (> imm7 63) (- imm7 128) imm7)))
         (format nil "LDP x~D, x~D, [x~D, #~D]" rt1 rt2 rn (* offset 8))))
      ;; STP (store pair)
      ((= (logand word #xFFC00000) #xA9000000)
       (let* ((rt1 (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (rt2 (logand (ash word -10) #x1F))
              (imm7 (logand (ash word -15) #x7F))
              (offset (if (> imm7 63) (- imm7 128) imm7)))
         (format nil "STP x~D, x~D, [x~D, #~D]" rt1 rt2 rn (* offset 8))))
      ;; BL (branch with link)
      ((= (logand word #xFC000000) #x94000000)
       (let* ((imm26 (logand word #x3FFFFFF))
              (offset (if (> imm26 (ash 1 25))
                          (- imm26 (ash 1 26))
                          imm26)))
         (format nil "BL #~D  ; -> 0x~X" (* offset 4) (+ addr (* offset 4)))))
      ;; B (unconditional branch)
      ((= (logand word #xFC000000) #x14000000)
       (let* ((imm26 (logand word #x3FFFFFF))
              (offset (if (> imm26 (ash 1 25))
                          (- imm26 (ash 1 26))
                          imm26)))
         (format nil "B #~D  ; -> 0x~X" (* offset 4) (+ addr (* offset 4)))))
      ;; B.cond (conditional branch)
      ((= (logand word #xFF000010) #x54000000)
       (let* ((imm19 (logand (ash word -5) #x7FFFF))
              (cond-code (logand word #xF))
              (offset (if (> imm19 (ash 1 18))
                          (- imm19 (ash 1 19))
                          imm19))
              (cond-name (case cond-code
                           (0 "EQ") (1 "NE") (10 "GE") (11 "LT")
                           (12 "GT") (13 "LE") (t (format nil "~D" cond-code)))))
         (format nil "B.~A #~D  ; -> 0x~X" cond-name (* offset 4) (+ addr (* offset 4)))))
      ;; RET
      ((= word #xD65F03C0)
       "RET")
      ;; BLR
      ((= (logand word #xFFFFFC1F) #xD63F0000)
       (let ((rn (logand (ash word -5) #x1F)))
         (format nil "BLR x~D" rn)))
      ;; BR
      ((= (logand word #xFFFFFC1F) #xD61F0000)
       (let ((rn (logand (ash word -5) #x1F)))
         (format nil "BR x~D" rn)))
      ;; CMP (alias for SUBS with XZR dest)
      ((and (= (logand word #x7FE0001F) #x6B00001F))
       (let ((rn (logand (ash word -5) #x1F))
             (rm (logand (ash word -16) #x1F)))
         (format nil "CMP x~D, x~D" rn rm)))
      ;; CSET
      ((= (logand word #x7FE0FC00) #x1A9F07E0)
       (let ((rd (logand word #x1F))
             (cond-code (logand (ash word -12) #xF)))
         (format nil "CSET x~D, ~A" rd
                 (case cond-code
                   (0 "NE") (1 "EQ") (10 "LT") (11 "GE")
                   (12 "LE") (13 "GT") (t (format nil "~D" cond-code))))))
      ;; AND/ORR/EOR register
      ((= (logand word #x1F000000) #x0A000000)
       (let* ((rd (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (rm (logand (ash word -16) #x1F))
              (opc (logand (ash word -29) #x3))
              (op-name (case opc (0 "AND") (1 "ORR") (2 "EOR") (t "???"))))
         (format nil "~A x~D, x~D, x~D" op-name rd rn rm)))
      ;; LSL/LSR/ASR variable
      ((= (logand word #x7FE0FC00) #x1AC02000)
       (let* ((rd (logand word #x1F))
              (rn (logand (ash word -5) #x1F))
              (rm (logand (ash word -16) #x1F))
              (op2 (logand (ash word -10) #x3))
              (op-name (case op2 (0 "LSL") (1 "LSR") (2 "ASR") (t "???"))))
         (format nil "~A x~D, x~D, x~D" op-name rd rn rm)))
      ;; ADRP
      ((= (logand word #x9F000000) #x90000000)
       (let ((rd (logand word #x1F)))
         (format nil "ADRP x~D, <page>" rd)))
      ;; Default
      (t (format nil ".word 0x~8,'0X" word))))

(defun disassemble-bytecode (bytecode &key (start-addr 0))
  "Disassemble a list of bytes to ARM64 mnemonics.
   BYTECODE is a list of bytes (little-endian ARM64 instructions).
   Returns a list of (address hex-word mnemonic) tuples."
  (let ((results nil)
        (addr start-addr))
    (loop while (>= (length bytecode) 4) do
      (let* ((b0 (pop bytecode))
             (b1 (pop bytecode))
             (b2 (pop bytecode))
             (b3 (pop bytecode))
             (word (logior b0 (ash b1 8) (ash b2 16) (ash b3 24)))
             (mnemonic (disassemble-arm64-instr word addr)))
        (push (list addr (format nil "~8,'0X" word) mnemonic) results)
        (incf addr 4)))
    (nreverse results)))

(defun disassemble-form (form &key verbose)
  "Disassemble a Lisp form, showing IR and ARM64 bytecode.
   FORM can be a simple expression or a defun.
   Returns a plist with :ir, :bytecode, and :disasm."
  (let* ((ir (cond
               ;; defun - compile function body
               ((and (consp form) (eq (car form) 'defun))
                (let* ((name (second form))
                       (params (third form))
                       (body (cdddr form))
                       (env (mapcar #'cons params
                                    (loop for i from 0 below (length params) collect i)))
                       (body-form (if (cdr body) (cons 'progn body) (car body))))
                  (list (sys:compile body-form env nil) name params)))
               ;; Simple expression
               (t (sys:compile form nil nil))))
         (bytecode (codegen ir nil nil 0))
         (disasm (disassemble-bytecode bytecode)))
    (when verbose
      (format t "~%IR: ~S~%~%" ir)
      (format t "Bytecode (~D bytes):~%" (length bytecode))
      (dolist (entry disasm)
        (format t "  ~4,'0X: ~A  ~A~%" (first entry) (second entry) (third entry)))
      (format t "~%"))
    (list :ir ir :bytecode bytecode :disasm disasm)))

;; Aliases for common operations
(defun habu-disassemble (form &key verbose)
  "Disassemble a form to IR and ARM64 bytecode."
  (disassemble-form form :verbose verbose))

(defun habu-compile (form)
  "Compile a form to IR without generating bytecode."
  (cond
    ((and (consp form) (eq (car form) 'defun))
     (let* ((params (third form))
            (body (cdddr form))
            (env (mapcar #'cons params
                         (loop for i from 0 below (length params) collect i)))
            (body-form (if (cdr body) (cons 'progn body) (car body))))
       (sys:compile body-form env nil)))
    (t (sys:compile form nil nil))))

(export '(habu-disassemble habu-compile disassemble-form disassemble-bytecode
          disassemble-arm64-instr) :habu)

;;; ============================================================
;;; Main entry point (for testing)
;;; ============================================================

(defun main ()
  ;; Full pipeline: parse -> compile to IR -> evaluate IR
  (let* ((src "(+ (* 3 4) 5)")
         (forms (read-all src)))
    (if (consp forms)
        (eval-forms forms)
        0)))

;; Only run main when loaded directly
;; (main)

;;; ============================================================
;;; Compiler entry point for self-hosting (habu-main)
;;; ============================================================
;;;
;;; This is the entry point for the self-hosted compiler.
;;; It reads a source file, compiles it to ARM64 bytecode,
;;; and writes the bytecode to an output file.
;;;
;;; Usage (when compiled to native):
;;;   habu-main reads from /tmp/input.lisp
;;;   habu-main writes to /tmp/output.bin
;;;
;;; This is a simplified version for initial bootstrap testing.
;;; Full command-line argument support requires additional runtime.

(defun habu-main-source ()
  "Source code for the self-hosting compiler entry point.
   This compiles input.lisp to output.bin (hardcoded paths for bootstrap)."
  "(defun list-to-vector (lst)
     ;; Convert a list to a vector
     (let* ((len (length lst))
            (vec (make-vector len)))
       (labels ((fill (l i)
                  (if (null l)
                      vec
                      (progn
                        (vector-set vec i (car l))
                        (fill (cdr l) (+ i 1))))))
         (fill lst 0))))

   (defun length (lst)
     ;; List length helper
     (labels ((iter (l n)
                (if (null l)
                    n
                    (iter (cdr l) (+ n 1)))))
       (iter lst 0)))

   ;; Main entry point
   (let* ((source (read-file \"/tmp/input.lisp\"))
          (forms (read-all source))
          (bytecode (compile-program forms nil))
          (byte-vec (list-to-vector bytecode)))
     (write-bytes \"/tmp/output.bin\" byte-vec)
     (println (length bytecode))
     0)")

;;; ============================================================
;;; Part 8a: Separate Compilation Units Support
;;; ============================================================

(defun compile-program-with-symtab (forms rtaddrs &key (optimize t))
  "Compile forms to bytecode and return (cons bytecode symbol-table).
   Symbol table is an alist of (name . byte-offset) for all exported functions.
   This is used for separate compilation units and FASL linking."
  (reset-symbol-table)
  (let* ((r (compile-forms forms))
         (defun-fns (car r))
         (mir-raw (cadr r))
         (mir-opt (if (and optimize (fboundp 'optimize-ir))
                      (optimize-ir mir-raw :passes '(let-flattening progn-flattening constant-folding strength-reduction dead-code-elimination))
                      mir-raw))
         (defun-fns-opt (if (and optimize (fboundp 'optimize-ir))
                            (mapcar (lambda (fn)
                                      (list (first fn) (second fn)
                                            (optimize-ir (third fn) :passes '(let-flattening progn-flattening constant-folding strength-reduction dead-code-elimination))
                                            (fourth fn) (fifth fn)))
                                    defun-fns)
                            defun-fns)))
    (let* ((mvb-result-23 (lift-lambdas mir-opt)) (mir (car mvb-result-23)) (main-lambdas (cdr mvb-result-23)))
                    (let* ((mvb-result-36 (lift-lambdas-from-fns defun-fns-opt nil nil)) (lifted-defuns (car mvb-result-36)) (defun-lambdas (cdr mvb-result-36)))
                    (let ((fns (append lifted-defuns main-lambdas defun-lambdas)))
          (if (null fns)
              (cons (resolve-calls (codegen-main mir rtaddrs) nil) nil)
              (let* ((main-code-temp (append (prologue) (codegen mir rtaddrs nil 0) (epilogue)))
                     (main-size (code-size main-code-temp))
                     (fnoffs (build-fnoffs fns main-size))
                     (main-code (append (prologue) (codegen mir rtaddrs fnoffs 0) (epilogue)))
                     (fn-code (codegen-all-fns fns rtaddrs fnoffs nil))
                     (all-code (append main-code fn-code))
                     (bytecode (resolve-calls all-code fnoffs)))
                (cons bytecode fnoffs))))))))

;;; Export for FASL compilation
(export 'compile-program-with-symtab :habu)

;;; ============================================================
;;; Part 8b: Enhanced FASL Format (v2) with Symbol Tables
;;; ============================================================

;;; FASL Format v2:
;;; Header (32 bytes):
;;;   Magic:         4 bytes "HFSL"
;;;   Version:       4 bytes (2 for symbol table support)
;;;   Flags:         4 bytes
;;;   Code-len:      4 bytes
;;;   Symtab-offset: 4 bytes (offset to symbol table from start of file)
;;;   Symtab-count:  4 bytes (number of exported symbols)
;;;   Reserved:      8 bytes
;;; Code Section: N bytes of ARM64 machine code
;;; Symbol Table: For each symbol: [name-len:4][name:N][offset:8]

(defun write-u32-le (n stream)
  "Write 32-bit unsigned integer in little-endian format to stream."
  (write-byte (logand n #xFF) stream)
  (write-byte (logand (ash n -8) #xFF) stream)
  (write-byte (logand (ash n -16) #xFF) stream)
  (write-byte (logand (ash n -24) #xFF) stream))

(defun write-u64-le (n stream)
  "Write 64-bit unsigned integer in little-endian format to stream."
  (write-u32-le (logand n #xFFFFFFFF) stream)
  (write-u32-le (logand (ash n -32) #xFFFFFFFF) stream))

(defun write-fasl-v2 (bytecode-list symbol-table output-path)
  "Write enhanced FASL v2 with symbol table.
   bytecode-list: list of bytes (ARM64 machine code)
   symbol-table: alist of (name . offset) pairs
   output-path: file to write"
  (with-open-file (out output-path :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create
                                    :element-type '(unsigned-byte 8))
    (let* ((code-len (length bytecode-list))
           (symtab-offset (+ 32 code-len))  ;; Header is 32 bytes
           (symtab-count (if symbol-table (length symbol-table) 0)))
      ;; Write header
      (write-byte #x48 out)  ;; 'H'
      (write-byte #x46 out)  ;; 'F'
      (write-byte #x53 out)  ;; 'S'
      (write-byte #x4C out)  ;; 'L'
      (write-u32-le 2 out)   ;; Version 2
      (write-u32-le 0 out)   ;; Flags
      (write-u32-le code-len out)
      (write-u32-le symtab-offset out)
      (write-u32-le symtab-count out)
      (write-u32-le 0 out)   ;; Reserved
      (write-u32-le 0 out)   ;; Reserved
      ;; Write code section
      (dolist (byte bytecode-list)
        (write-byte byte out))
      ;; Write symbol table
      (when symbol-table
        (dolist (entry symbol-table)
          (let* ((name (symbol-name (car entry)))
                 (offset (cdr entry))
                 (name-bytes (map 'list #'char-code name))
                 (name-len (length name-bytes)))
            (write-u32-le name-len out)
            (dolist (byte name-bytes)
              (write-byte byte out))
            (write-u64-le offset out)))))))

(defun read-u32-le (stream)
  "Read 32-bit unsigned integer in little-endian format from stream."
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior b0 (ash b1 8) (ash b2 16) (ash b3 24))))

(defun read-u64-le (stream)
  "Read 64-bit unsigned integer in little-endian format from stream."
  (let ((lo (read-u32-le stream))
        (hi (read-u32-le stream)))
    (logior lo (ash hi 32))))

(defun read-fasl-v2 (fasl-path)
  "Read enhanced FASL v2 and return (cons bytecode-list symbol-table).
   symbol-table is an alist of (name . offset) pairs."
  (with-open-file (in fasl-path :direction :input
                                :element-type '(unsigned-byte 8))
    ;; Read and verify magic
    (let ((magic (list (read-byte in) (read-byte in) (read-byte in) (read-byte in))))
      (unless (equal magic '(#x48 #x46 #x53 #x4C))  ;; "HFSL"
        (error "Invalid FASL magic")))
    ;; Read header
    (let* ((code-len (read-u32-le in)))
      (read-u32-le in)  ; Skip version
      (read-u32-le in)  ; Skip flags
      (read-u32-le in)  ; Skip symtab-offset
      (let ((symtab-count (read-u32-le in)))
        (read-u32-le in)  ; Skip reserved1
        (read-u32-le in)  ; Skip reserved2
        ;; Read code section
        (let ((bytecode (loop repeat code-len collect (read-byte in))))
          ;; Read symbol table
          (if (zerop symtab-count)
              (cons bytecode nil)
              (let ((symtab
                     (loop repeat symtab-count
                           collect
                           (let* ((name-len (read-u32-le in))
                                  (name-bytes (loop repeat name-len collect (read-byte in)))
                                  (name-string (map 'string #'code-char name-bytes))
                                  (offset (read-u64-le in)))
                             (cons (intern name-string :habu) offset)))))
                (cons bytecode symtab))))))))

(defun compile-file-to-fasl (source-path fasl-path)
  "Compile Lisp source file to FASL v2 with symbol table.
   Usage: (compile-file-to-fasl \"util.lisp\" \"util.fasl\")"
  (let* ((source (with-open-file (in source-path :direction :input)
                   (let ((contents (make-string (file-length in))))
                     (read-sequence contents in)
                     contents)))
         (forms (read-all source)))
    (let* ((mvb-result-24 (compile-program-with-symtab forms nil :optimize t)) (bytecode (car mvb-result-24)) (symtab (cdr mvb-result-24)))
                    (write-fasl-v2 bytecode symtab fasl-path)
      (format t "Compiled ~A -> ~A (~A bytes, ~A symbols)~%"
              source-path fasl-path (length bytecode) (if symtab (length symtab) 0))
      fasl-path)))

;;; Export FASL functions
(export '(write-fasl-v2 read-fasl-v2 compile-file-to-fasl) :habu)

;;; ============================================================
;;; Part 8c: FASL Linker - Combine Multiple Compilation Units
;;; ============================================================

(defun link-fasls (fasl-paths output-path &key verbose)
  "Link multiple FASL files into a single executable.
   Usage: (link-fasls '(\"util.fasl\" \"main.fasl\") \"myprogram\")"
  (let ((all-code nil)
        (global-symtab nil)
        (current-offset 0))
    ;; Read all FASL files and build global symbol table
    (dolist (fasl-path fasl-paths)
      (let* ((mvb-result-25 (read-fasl-v2 fasl-path)) (bytecode (car mvb-result-25)) (symtab (cdr mvb-result-25)))
                    (when verbose
          (format t "Read ~A: ~A bytes, ~A symbols~%"
                  fasl-path (length bytecode) (if symtab (length symtab) 0)))
        ;; Append code
        (setf all-code (append all-code bytecode))
        ;; Adjust symbol offsets and add to global table
        (when symtab
          (dolist (entry symtab)
            (let* ((name (car entry))
                   (offset (cdr entry))
                   (adjusted-offset (+ current-offset offset)))
              (push (cons name adjusted-offset) global-symtab))))
        ;; Update offset for next FASL
        (setf current-offset (+ current-offset (length bytecode)))))
    ;; Reverse to maintain order
    (setf global-symtab (reverse global-symtab))
    (when verbose
      (format t "Total code: ~A bytes~%" (length all-code))
      (format t "Global symbols: ~A~%" (length global-symtab))
      (when global-symtab
        (format t "Symbol table:~%")
        (dolist (entry global-symtab)
          (format t "  ~A @ ~A~%" (car entry) (cdr entry)))))
    ;; Collect external calls from bytecode
    (let ((imports (collect-imports all-code)))
      ;; Always use imports path for consistent Mach-O structure
      ;; Add _exit as dummy import if none (never called but ensures proper structure)
      (let ((imports (if (null imports) '("_exit") imports)))
        (when verbose
          (format t "Imports: ~A~%" imports))
        (write-macho-executable-with-imports-and-heap output-path all-code imports #x100000)))
    (when verbose
      (format t "Created: ~A~%" output-path))
    output-path))

;;; Export linker function
(export 'link-fasls :habu)
