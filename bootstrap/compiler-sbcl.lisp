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
;;;
;;; NOTE: Package definitions are in package.lisp

(in-package :sys)

;;; Forward declarations for functions defined in other files
;;; These are loaded via ASDF after this file
(declaim (ftype (function (t t t t &optional t) t) write-macho-executable-with-imports-and-heap))
(declaim (ftype (function (t &key (:passes t)) t) habu:optimize-ir))
(declaim (ftype (function (t t) t) wrap-bytecode-with-heap-for-imports))
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

;;; ==========================================================
;;; Trace State (defined here to avoid circular dependency)
;;; ==========================================================

(defvar *traced-functions* (make-hash-table :test 'eq)
  "Hash table of function names being traced.")

(defvar *trace-depth* 0
  "Current nesting depth for trace output.")

(defun trace-indent ()
  "Print indentation for current trace depth."
  (dotimes (i (* 2 *trace-depth*))
    (write-char #\Space *trace-output*)))

(defun trace-enter (name args)
  "Print function entry trace message."
  (trace-indent)
  (format *trace-output* "~D: (~S~{ ~S~})~%" *trace-depth* name args))

(defun trace-exit (name value)
  "Print function exit trace message."
  (trace-indent)
  (format *trace-output* "~D: ~S returned ~S~%" *trace-depth* name value))

(defun trace-function (name)
  "Enable tracing for function NAME. Returns NAME."
  (setf (gethash name *traced-functions*) t)
  name)

(defun untrace-function (name)
  "Disable tracing for function NAME. Returns NAME."
  (remhash name *traced-functions*)
  name)

(defun traced-p (name)
  "Return T if function NAME is being traced."
  (gethash name *traced-functions*))

(defun list-traced ()
  "Return list of all currently traced function names."
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k result))
             *traced-functions*)
    result))

;;; ==========================================================
;;; Undefined Function Tracking
;;; ==========================================================
;;; Tracks undefined function calls for compile-time warnings and
;;; link-time verification. Exit code 200 = undefined function at runtime.

(defvar *undefined-functions* nil "Functions called but not defined")
(defvar *all-call-targets* nil "All call-fn targets for link-time check")
(defvar *declared-imports* nil "Functions declared as imports (resolved at link time)")
(defvar *skip-link-verification* nil "When T, skip link-time verification entirely")

(defun reset-compile-warnings ()
  "Reset undefined function tracking before compilation"
  (setq *undefined-functions* nil)
  (setq *all-call-targets* nil)
  (setq *declared-imports* nil))

(defun declare-imports (imports)
  "Declare functions that will be resolved at link time"
  (setq *declared-imports* imports))

(defun record-undefined-function (name)
  "Record that an undefined function was called (unless it's a declared import)"
  (unless (or (member name *undefined-functions*)
              (member name *declared-imports*))
    (push name *undefined-functions*)))

(defun mark-function-defined (name)
  "Remove NAME from undefined functions list when its defun is found"
  (setq *undefined-functions* (remove name *undefined-functions*)))

(defun record-call-target (name)
  "Record a call-fn target for link-time verification"
  (unless (member name *all-call-targets*)
    (push name *all-call-targets*)))

(defun report-compile-warnings ()
  "Report undefined functions found during compilation. Returns T if errors found."
  (when *undefined-functions*
    (format t "~%ERROR: Undefined functions referenced:~%")
    (dolist (fn (reverse *undefined-functions*))
      (format t "  - ~A~%" fn))
    t))

(defun verify-link-references (defined-fns)
  "Verify all call-fn targets are in defined-fns list (or declared imports). Returns T if errors found.
   If *skip-link-verification* is true, always returns NIL (skips verification)."
  (when *skip-link-verification*
    (format t "~%Note: Link verification skipped~%")
    (return-from verify-link-references nil))
  (let ((undefined nil))
    (dolist (target *all-call-targets*)
      (unless (or (member target defined-fns)
                  ;; Compare imports by symbol-name (packages may differ)
                  (member (symbol-name target) *declared-imports*
                          :test #'string= :key #'symbol-name))
        (push target undefined)))
    (when undefined
      (format t "~%LINK ERROR: Functions called but not compiled:~%")
      (dolist (fn (reverse undefined))
        (format t "  - ~A~%" fn))
      t)))

;;; ==========================================================

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
;;; Macro Table - User-defined macros
;;; ============================================================

(defparameter *macro-table* (make-hash-table :test 'equal)
  "Compile-time macro storage: name-string -> (params . body)")

(defun reset-macro-table ()
  "Clear all macro definitions."
  (clrhash *macro-table*))

(defun normalize-macro-name (name)
  "Normalize macro name to string for table lookup.
   Macros come from reader (symbols) but table uses string keys."
  (cond
    ((stringp name) name)
    ((symbolp name) (symbol-name name))
    (t (error "normalize-macro-name: expected symbol or string, got ~S" name))))

(defun macro-function (name)
  "Look up macro definition by name. Returns (params . body) or nil."
  (gethash (normalize-macro-name name) *macro-table*))

(defun (setf macro-function) (value name)
  "Set macro definition for name."
  (setf (gethash (normalize-macro-name name) *macro-table*) value))

(defun parse-macro-params (params args)
  "Parse macro parameter list and build alist of (param . value).
   Handles &body and &rest by collecting remaining args into a list."
  (let ((result nil)
        (p params)
        (a args)
        (rest-mode nil))
    (loop while p do
          (let ((param (car p)))
            (cond
              ;; Skip &body and &rest markers, set rest-mode
              ((or (eq param '&body) (eq param '&rest)
                   (and (symbolp param)
                        (or (string= (symbol-name param) "&BODY")
                            (string= (symbol-name param) "&REST"))))
               (setq rest-mode t))
              ;; In rest mode, bind param to remaining args
              (rest-mode
               (push (cons param a) result)
               (setq a nil))  ; Consume all remaining
              ;; Normal param
              (t
               (push (cons param (car a)) result)
               (setq a (cdr a)))))
          (setq p (cdr p)))
    result))

(defun substitute-params (body params args)
  "Substitute parameter names with QUOTED argument forms in macro body.
   Arguments are quoted so they are not evaluated when the body is eval'd.
   Handles &body and &rest parameters."
  (let ((bindings (parse-macro-params params args)))
    (substitute-params-with-bindings body bindings)))

(defun substitute-params-with-bindings (body bindings)
  "Substitute parameters using pre-computed bindings alist."
  (cond
    ((null body) nil)
    ((symbolp body)
     (let ((binding (assoc body bindings :test #'eq)))
       (if (null binding)
           ;; Try string comparison for cross-package symbols
           (let ((binding2 (assoc body bindings
                                  :test (lambda (a b)
                                          (string= (symbol-name a)
                                                   (symbol-name b))))))
             (if binding2
                 (let ((arg (cdr binding2)))
                   (if (or (symbolp arg) (consp arg))
                       (list 'quote arg)
                       arg))
                 body))
           (let ((arg (cdr binding)))
             (if (or (symbolp arg) (consp arg))
                 (list 'quote arg)
                 arg)))))
    ((atom body) body)
    (t (cons (substitute-params-with-bindings (car body) bindings)
             (substitute-params-with-bindings (cdr body) bindings)))))

(defun macroexpand-1 (form)
  "Expand macro form once. Returns (values expanded-form expanded-p).
   Prioritizes our *macro-table* to override CL macros like DOTIMES.
   Uses SBCL's built-in macroexpand for HABU:: macros (for backquote handling)."
  (if (and (consp form) (symbolp (car form)))
      ;; First check our *macro-table* - this takes priority
      (let ((macro-def (macro-function (car form))))
        (if macro-def
            ;; Check if this was defined in HABU package (has SBCL definition too)
            (let* ((name (car form))
                   (sbcl-macro (and (symbolp name)
                                    (eq (symbol-package name) (find-package :habu))
                                    (cl:macro-function name))))
              (if sbcl-macro
                  ;; Use SBCL's macroexpand for HABU:: macros (proper backquote)
                  (cl:macroexpand-1 form)
                  ;; Use our substitution for CL-inherited macros
                  (let* ((params (car macro-def))
                         (body (cdr macro-def))
                         (args (cdr form))
                         (substituted (substitute-params body params args)))
                    (values (cl:eval substituted) t))))
            ;; No macro in our table - check SBCL
            (let ((sbcl-macro (cl:macro-function (car form))))
              (if sbcl-macro
                  (cl:macroexpand-1 form)
                  (values form nil)))))
      (values form nil)))

(defun macroexpand (form)
  "Repeatedly expand macro form until no more expansions.
   Returns (values expanded-form expanded-p)."
  (multiple-value-bind (new-form expanded-p) (macroexpand-1 form)
    (if expanded-p
        (multiple-value-bind (final-form any-p) (macroexpand new-form)
          (declare (ignore any-p))
          (values final-form t))
        (values form nil))))

;;; ============================================================
;;; Block Environment - for block/return-from
;;; ============================================================

(defparameter *block-env* nil
  "Stack of active block names during compilation.
   Each entry is (name . gensym) where gensym is the unique ID for this block.")

(defvar *block-counter* 0
  "Counter for generating unique block IDs.")

(defun reset-block-env ()
  "Clear block environment."
  (setf *block-env* nil
        *block-counter* 0))

(defun make-block-id (name)
  "Generate a unique block ID."
  (incf *block-counter*)
  (cons name *block-counter*))

(defun find-block (name)
  "Find block entry by name in *block-env*. Returns (name . id) or nil."
  (assoc name *block-env* :test #'eq))

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
                   ((and (consp item) (eq (car item) :lambda-ref-marker)) 4) ; ADR instruction
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
                     ;; Internal call - resolve to BL if found, else preserve marker
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-entry (assoc fn-name fnoffs)))
                        (if fn-entry
                            ;; Known function - resolve to BL
                            (let* ((fn-pos (cdr fn-entry))
                                   (rel-offset (- fn-pos pos))
                                   (bl-bytes (arm64:bl (ash rel-offset -2))))
                              (resolve-at (cdr items)
                                          (+ pos 4)
                                          (append (reverse bl-bytes) acc)
                                          loop-start-stack))
                            ;; Unknown function - preserve marker for link-time resolution
                            (resolve-at (cdr items)
                                        (+ pos 4)
                                        (cons (list :call-fn fn-name pos) acc)
                                        loop-start-stack))))
                     ;; Tail call - resolve to B if found, else preserve marker
                     ((and (consp item) (eq (car item) :tail-call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-entry (assoc fn-name fnoffs)))
                        (if fn-entry
                            ;; Known function - resolve to B
                            (let* ((fn-pos (cdr fn-entry))
                                   (rel-offset (- fn-pos pos))
                                   (b-bytes (arm64:b (ash rel-offset -2))))
                              (resolve-at (cdr items)
                                          (+ pos 4)
                                          (append (reverse b-bytes) acc)
                                          loop-start-stack))
                            ;; Unknown function - preserve marker for link-time resolution
                            (resolve-at (cdr items)
                                        (+ pos 4)
                                        (cons (list :tail-call-fn fn-name pos) acc)
                                        loop-start-stack))))
                     ;; External call - pass through marker as-is (counts as 4 bytes)
                     ;; extract-extern-calls-from-bytecode will replace with NOPs later
                     ((and (consp item) (eq (car item) :extern-call))
                      (resolve-at (cdr items)
                                  (+ pos 4)
                                  (cons (list :extern-call (cadr item) pos) acc)
                                  loop-start-stack))
                     ;; Lambda-ref marker - resolve to ADR if found in fnoffs
                     ((and (consp item) (eq (car item) :lambda-ref-marker))
                      (let* ((dest-reg (cadr item))
                             (lambda-name (caddr item))
                             (fn-entry (assoc lambda-name fnoffs)))
                        (if fn-entry
                            ;; Known lambda - resolve to ADR
                            (let* ((fn-pos (cdr fn-entry))
                                   (rel-offset (- fn-pos pos))
                                   (adr-bytes (arm64:adr dest-reg rel-offset)))
                              (resolve-at (cdr items)
                                          (+ pos 4)
                                          (append (reverse adr-bytes) acc)
                                          loop-start-stack))
                            ;; Unknown lambda - preserve marker for link-time resolution
                            (resolve-at (cdr items)
                                        (+ pos 4)
                                        (cons (list :lambda-ref-marker dest-reg lambda-name pos) acc)
                                        loop-start-stack))))
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

(defun sbcl-gc-trigger-code ()
  "Generate inline GC trigger check for SBCL-hosted compilation.
   Uses x8 as scratch (reserved for runtime, not allocatable).
   Emits :call-fn marker if GC needed.
   Simple GC: checks from-end at [x27+16], calls GC-COLLECT."
  (append-all
   (list (arm64:ldr :x8 :gc :offset +gc-from-end-offset+)  ; x8 = from_end
         (arm64:cmp :heap :x8)                             ; compare x28, from_end
         (arm64:b.lo 2)                                    ; skip if x28 < from_end
         (list (list :call-fn 'GC-COLLECT)))))

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
                           (arm64:movz :x0 tagged-len)
                           (load-addr :x0 tagged-len))
                       (arm64:ldr :x11 :x19 :offset 56)
                       (arm64:blr :x11)
                       (arm64:str :x0 :sp :offset vec-slot)))))
    ;; Store each character: ldr x0, [sp, vec-slot]; movz x1, tagged-idx; movz x2, tagged-ch; ldr x11, [x19, #64]; blr x11
    ;; Runtime table index 8 = vector_set at offset 64
    (let ((stores-reversed nil))
      (let ((idx 0))
        (dolist (ch chars)
          (let* ((tagged-idx (ash idx 4))    ; Tag index as fixnum
                 (tagged-ch (ash ch 4))      ; Tag character as fixnum
                 (store-code (append-all
                              (list (arm64:ldr :x0 :sp :offset vec-slot)
                                    (if (< tagged-idx #x10000)
                                        (arm64:movz :x1 tagged-idx)
                                        (load-addr 1 tagged-idx))
                                    (if (< tagged-ch #x10000)
                                        (arm64:movz :x2 tagged-ch)
                                        (load-addr 2 tagged-ch))
                                    (arm64:ldr :x11 :x19 :offset 64)
                                    (arm64:blr :x11)))))
            (setq stores-reversed (revappend store-code stores-reversed))
            (setq idx (+ idx 1)))))
      (let* ((stores (nreverse stores-reversed))
             ;; Make string from vector: ldr x0, [sp, vec-slot]; ldr x9, [x19, #80]; blr x9
             ;; Runtime table index 10 = make_string_from_vector at offset 80
             (make-str (append-all
                        (list (arm64:ldr :x0 :sp :offset vec-slot)
                              (arm64:ldr :x9 :x19 :offset 80)
                              (arm64:blr :x9)))))
        (append-all (list alloc stores make-str))))))

(defun codegen-string-inline (chars)
  "Generate code to build a string inline on the heap using x28 bump pointer.
   String layout: [length (8 bytes)][char data (n bytes)]
   Returns code that leaves tagged string pointer in x0.
   All allocations are 16-byte aligned for 4-bit tagging scheme.
   IMPORTANT: GC pre-check before allocation to prevent writing to unmapped memory."
  (let* ((len (length chars))
         ;; Round up allocation to 16-byte alignment: (8 + len + 15) & ~15
         (alloc-size (logand (+ 8 len 15) (lognot 15))))
    (let ((stores-reversed nil))
      (let ((idx 0))
        (dolist (ch chars)
          (let* ((offset (+ 8 idx))
                 (code (append-all
                        (list (arm64:movz :x1 ch)
                              (arm64:strb :x1 :heap offset)))))
            (setq stores-reversed (revappend code stores-reversed))
            (setq idx (+ idx 1)))))
      (let ((store-code (nreverse stores-reversed)))
        (append-all
         (list
          ;; GC pre-check BEFORE writing to heap
          (sbcl-gc-trigger-code)
          ;; Store length at [x28+0]
          (arm64:movz :x1 len)
          (arm64:str :x1 :heap :offset 0)
          ;; Store each char
          store-code
          ;; Return tagged pointer, bump heap
          (arm64:mov :x0 :heap)                   ; x0 = current heap ptr
          (arm64:movz :x1 alloc-size)
          (arm64:add :heap :heap :x1)                ; x28 += alloc size
          ;; Tag with string tag (0x4)
          (arm64:movz :x1 4)
          (arm64:orr :x0 :x0 :x1)))))))

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

(defun flat-env-lookup (sym env)
  "Lookup in flat environment list (sym1 sym2 ...) - returns sym if found, nil otherwise"
  (member sym env))

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

;; Append list of lists using push/nreverse - O(n) instead of O(n²)
(defun append-all (lists)
  (let ((result-reversed nil))
    (dolist (lst lists)
      (setq result-reversed (revappend lst result-reversed)))
    (nreverse result-reversed)))

;;; ============================================================
;;; Keyword Argument Support
;;; ============================================================

(defun parse-lambda-list (params)
  "Parse lambda list, splitting at &optional and &key.
   Returns (positional-params . keyword-specs) where keyword-specs is
   a list of (name default) pairs.
   &optional params are added to positional-params (names only, defaults ignored)."
  (let ((positional nil)
        (keywords nil)
        (in-opt nil)
        (in-keys nil))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setq in-opt t in-keys nil))
        ((eq p '&key) (setq in-keys t in-opt nil))
        (in-keys
         ;; Keyword param: either SYMBOL or (SYMBOL DEFAULT)
         (if (consp p)
             (push (list (car p) (cadr p)) keywords)
             (push (list p nil) keywords)))
        (in-opt
         ;; Optional param: SYMBOL or (SYMBOL DEFAULT)
         ;; Add name to positional params (default ignored for now)
         (if (consp p)
             (push (car p) positional)
             (push p positional)))
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

(defun all-digits-p (name start)
  "Check if all characters from START to end of NAME are digits"
  (let ((len (length name)))
    (if (>= start len)
        nil  ; Empty after start = not a number
        (labels ((check (i)
                   (if (>= i len)
                       t  ; Reached end, all were digits
                       (if (digit-p (char-code (char name i)))
                           (check (1+ i))
                           nil))))  ; Found non-digit
          (check start)))))

(defun read-sym (s pos)
  (let* ((r (read-sym-chars s pos nil))
         (chars (car r))
         (end (cdr r))
         (name (chars-to-string (reverse chars)))
         (uname (string-upcase name)))
    ;; Check for numeric literals before treating as symbol
    ;; A token is numeric only if ALL chars (after optional sign) are digits
    (let ((first-ch (if (> (length name) 0) (char-code (char name 0)) 0)))
      (cond
        ;; Starts with digit - only numeric if ALL chars are digits
        ((and (digit-p first-ch)
              (all-digits-p name 0))
         (read-int s pos))
        ;; Negative number: starts with -, rest must be ALL digits
        ((and (= first-ch #x2D)
              (> (length name) 1)
              (all-digits-p name 1))
         (read-int s pos))
        ;; Positive number: starts with +, rest must be ALL digits
        ((and (= first-ch #x2B)
              (> (length name) 1)
              (all-digits-p name 1))
         (read-int s pos))
        ;; NIL and T
        ((string= uname "NIL") (cons nil end))
        ((string= uname "T") (cons t end))
        ;; Keywords start with ':' - intern into KEYWORD package
        ((and (> (length uname) 1) (char= (char uname 0) #\:))
         (cons (intern (subseq uname 1) "KEYWORD") end))
        ;; Package-qualified symbols like ARM64:ADD
        ((position #\: uname)
         (let* ((colon-pos (position #\: uname))
                (pkg-name (subseq uname 0 colon-pos))
                (sym-name (subseq uname (1+ colon-pos)))
                (pkg (find-package pkg-name)))
           ;; If package exists use it, otherwise intern full name in HABU
           (cons (if pkg
                     (intern sym-name pkg)
                     (intern uname (find-package :habu)))
                 end)))
        ;; Regular symbols - intern into HABU package
        (t (cons (intern uname (find-package :habu)) end))))))

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
                           ;; Normalize feature name - reader produces symbols
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

(defparameter *temp-registers* '(:x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15))
(defparameter *num-temp-registers* 11)

(defparameter *arg-registers* '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7))

(defun arg-register (n)
  "Return keyword for argument register n (0-7)."
  (nth n *arg-registers*))

(defun temp-register (depth)
  "Return register number for temp depth, or nil if must spill to stack."
  (if (< depth *num-temp-registers*)
      (nth depth *temp-registers*)
      nil))

(defun temp-slot (depth)
  "Return stack offset for temp depth. Used when registers exhausted or across calls."
  (let ((off (+ #x40 (* depth 8))))  ; #x40 = temp base (64)
    (if (>= off #x8000)               ; #x8000 = temp guard (32768), allows ~4000 slots
        (error "Too many temp slots: ~A" depth)
        off)))

(defun save-temp (depth)
  "Generate code to save x0 to temp location (register or stack)."
  (let ((reg (temp-register depth)))
    (if reg
        (arm64:mov reg :x0)            ; MOV xN, x0
        (arm64:str :x0 :sp :offset (temp-slot depth)))))  ; STR x0, [sp, #off]

(defun load-temp (dest-reg depth)
  "Generate code to load temp location to dest-reg."
  (let ((reg (temp-register depth)))
    (if reg
        (if (eq dest-reg reg)
            nil                        ; Already in correct register
            (arm64:mov dest-reg reg)) ; MOV dest, xN
        (arm64:ldr dest-reg :sp :offset (temp-slot depth)))))

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
   (arm64:sub :sp :sp 1 :imm t :shift12 t)   ; SUB sp, sp, #1, LSL #12 = #4096
   (arm64:stp :fp :lr :sp :offset 0)  ; STP x29, x30, [sp, #0]
   (arm64:stp :x19 :env :sp :offset 16) ; STP x19, x20, [sp, #16]
   (arm64:stp :x21 :x22 :sp :offset 32) ; STP x21, x22, [sp, #32]
   (arm64:stp :x23 :closure :sp :offset 48) ; STP x23, x24, [sp, #48]
   (arm64:mov :x19 :x0)           ; MOV x19, x0 (save runtime table)
   (arm64:add :env :sp #x180 :imm t)))  ; ADD x20, sp, #384 (env-base)

(defun epilogue ()
  (append
   (arm64:ldp :x23 :closure :sp :offset 48) ; LDP x23, x24, [sp, #48]
   (arm64:ldp :x21 :x22 :sp :offset 32) ; LDP x21, x22, [sp, #32]
   (arm64:ldp :x19 :env :sp :offset 16) ; LDP x19, x20, [sp, #16]
   (arm64:ldp :fp :lr :sp :offset 0)  ; LDP x29, x30, [sp, #0]
   (arm64:add :sp :sp 1 :imm t :shift12 t)    ; ADD sp, sp, #1, LSL #12 = #4096
   (arm64:ret)))

;;; ============================================================
;;; Part 5b: Free Variable Analysis
;;; ============================================================

(defun find-free-vars (expr bound env)
  "Find variables referenced in expr that are in env but not in bound.
   Handles both env formats:
   - Alist: ((sym . offset) ...) - used by compiler-sbcl.lisp
   - Flat list: (sym1 sym2 ...) - used by compiler.lisp"
  ;; Detect env format: alist has cons cells, flat list has symbols
  (let ((env-is-flat (and env (symbolp (car env)))))
    (labels ((in-env (sym)
               ;; Check if sym is in environment (handles both formats)
               (if env-is-flat
                   (member sym env)
                   (env-lookup sym env)))
             (collect (e bnd acc)
               (cond
                 ((null e) acc)
                 ((symbolp e)
                  ;; Check if it's a variable reference (in env but not bound)
                  (if (and (in-env e)
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
                 ((or (eq (car e) 'LET) (eq (car e) 'let))
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
                 ((or (eq (car e) 'LET*) (eq (car e) 'let*))
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
      (collect expr bound nil))))

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
               ;; CASE/ECASE - walk keyform and clause bodies (skip keys)
               ((or (eq (car e) 'case) (eq (car e) 'CASE)
                    (eq (car e) 'ecase) (eq (car e) 'ECASE))
                (let* ((keyform (cadr e))
                       (clauses (cddr e))
                       (acc2 (collect keyform bnd acc)))
                  ;; Walk each clause body, skip the key
                  (labels ((do-clauses (cs acc)
                             (if (null cs) acc
                                 (let* ((clause (car cs))
                                        (body (cdr clause)))  ; skip key (car clause)
                                   (do-clauses (cdr cs) (collect-list body bnd acc))))))
                    (do-clauses clauses acc2))))
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
               ;; CASE/ECASE - walk keyform and clause bodies (skip keys)
               ((or (eq (car e) 'case) (eq (car e) 'CASE)
                    (eq (car e) 'ecase) (eq (car e) 'ECASE))
                (let* ((keyform (cadr e))
                       (clauses (cddr e))
                       (acc2 (collect keyform bnd acc)))
                  ;; Walk each clause body, skip the key
                  (labels ((do-clauses (cs acc)
                             (if (null cs) acc
                                 (let* ((clause (car cs))
                                        (body (cdr clause)))  ; skip key (car clause)
                                   (do-clauses (cdr cs) (collect-list body bnd acc))))))
                    (do-clauses clauses acc2))))
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
               ;; CASE/ECASE - walk keyform and clause bodies (skip keys)
               ((or (eq (car e) 'case) (eq (car e) 'CASE)
                    (eq (car e) 'ecase) (eq (car e) 'ECASE))
                (let* ((keyform (cadr e))
                       (clauses (cddr e))
                       (acc2 (collect keyform bnd acc)))
                  ;; Walk each clause body, skip the key
                  (labels ((do-clauses (cs acc)
                             (if (null cs) acc
                                 (let* ((clause (car cs))
                                        (body (cdr clause)))
                                   (do-clauses (cdr cs) (collect-list body bnd acc))))))
                    (do-clauses clauses acc2))))
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
               ;; CASE/ECASE - transform keyform and clause bodies (keep keys intact)
               ((or (eq (car e) 'case) (eq (car e) 'CASE)
                    (eq (car e) 'ecase) (eq (car e) 'ECASE))
                (let* ((keyform (cadr e))
                       (clauses (cddr e))
                       (transformed-keyform (transform keyform boxed))
                       (transformed-clauses
                        (mapcar (lambda (clause)
                                  (cons (car clause)  ; keep key intact
                                        (mapcar (lambda (x) (transform x boxed))
                                                (cdr clause))))
                                clauses)))
                  (cons (car e) (cons transformed-keyform transformed-clauses))))
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
         ((or (eq op 'LABELS) (eq op 'FLET)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-calls (caddr expr) fn-names)))
         ;; while - rewrite test and body (BUG FIX: while inside labels)
         ((eq op 'while)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (cons 'while (cons (rewrite-labels-calls test fn-names)
                               (mapcar (lambda (e) (rewrite-labels-calls e fn-names)) body)))))
         ;; let/let* - rewrite values and body, not binding names
         ((or (eq op 'LET) (eq op 'LET*))
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
         ((or (eq op 'LABELS) (eq op 'FLET)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-body (caddr expr) fn-names fntab-var)))
         ;; while - rewrite test and body (BUG FIX: while inside labels)
         ((eq op 'while)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (cons 'while (cons (rewrite-labels-body test fn-names fntab-var)
                               (mapcar (lambda (e) (rewrite-labels-body e fn-names fntab-var)) body)))))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*))
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
         ((or (eq op 'LABELS) (eq op 'FLET)) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (rewrite-labels-main (caddr expr) fn-names)))
         ;; while - rewrite test and body (BUG FIX: while inside labels)
         ((eq op 'while)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (cons 'while (cons (rewrite-labels-main test fn-names)
                               (mapcar (lambda (e) (rewrite-labels-main e fn-names)) body)))))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*))
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

;;; Known global variables that map to IR forms
;;; Defined here because sys:compile references them
(defparameter *known-globals*
  '((*intern-table* . (get-intern-table-ir . set-intern-table-ir))
    (*lambda-counter* . (get-lambda-counter-ir . set-lambda-counter-ir))
    (*packages* . (get-packages-ir . set-packages-ir))
    (*current-package* . (get-current-package-ir . set-current-package-ir))))

;;; Compile-time constants collected from defconstant forms
(defparameter *constants* nil
  "Alist of (name . value) for defconstant values during compilation")

;;; User-defined global variables from defvar/defparameter
;;; These are stored in the global-vars table at [x27+104]
(defparameter *defined-globals* nil
  "Alist of (name . initial-value-ir) for defvar/defparameter during compilation")

(defun quote-ir (obj)
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'nil-ir))  ;; Use nil-ir for proper nil, not (lit 0)
    ;; Keywords MUST be checked before symbolp (keywords are symbols in CL)
    ((keywordp obj) (list 'kw-lit (symbol-name obj)))
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (quote-ir (car obj)) (quote-ir (cdr obj))))
    ((stringp obj) (list 'str-lit obj))
    ;; Default: error on unknown quoted value type
    (t (error "quote-ir: unhandled type for ~S" obj))))

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
       ((eq expr 'nil) (list 'nil-ir))          ; nil is 0 (hybrid scheme)
       ;; Keywords are self-evaluating - compile as keyword literal (tag 10)
       ((keywordp expr) (list 'kw-lit (symbol-name expr)))
       (t
        ;; Use numberp since offset 0 is falsey in Habu
        (let ((off (env-lookup expr env)))
          (if (numberp off)
              (list 'var off)
              ;; Check if it's a compile-time constant
              (let ((const-entry (and (boundp '*constants*) *constants* (assoc expr *constants*))))
                (if const-entry
                    (list 'lit (cdr const-entry))
                    ;; Check if it's a known global variable
                    (let ((global-entry (assoc expr *known-globals*)))
                      (if global-entry
                          ;; Emit getter IR form for the global
                          (list (car (cdr global-entry)))
                          ;; Check if it's a user-defined global (defvar/defparameter)
                          (let ((defined-entry (and (boundp '*defined-globals*)
                                                    *defined-globals*
                                                    (assoc expr *defined-globals*))))
                            (if defined-entry
                                ;; Emit vector-ref on globals vector
                                ;; Entry is (name index init-form)
                                (list 'vector-ref-ir
                                      '(get-global-vars-ir)
                                      (list 'lit (cadr defined-entry)))
                                ;; Check if it's a known function name - return as lambda-ref
                                ;; This creates a closure pointing to the function (no captures)
                                (if (and fenv (assoc expr fenv))
                                    (list 'lambda-ref expr nil)
                                    (error "Undefined variable: ~S" expr)))))))))))))
    ((consp expr)
     ;; Check for user-defined macro FIRST, but NOT for special forms we handle
     ;; (incf/decf are macros but we compile them directly)
     ;; Note: LOOP is handled by SBCL macro expansion - complex forms aren't worth reimplementing
     (if (and (symbolp (car expr))
              (macro-function (car expr))
              (not (member (car expr) '(incf decf while))))
         (sys:compile (macroexpand-1 expr) env fenv)
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
                        ;; Variadic: (logand A B C D...) -> (logand (logand A B) C D...)
                        (sys:compile (cons 'logand (cons (list 'logand (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op 'logior)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bor (sys:compile (car args) env fenv) (sys:compile (cadr args) env fenv))
                        ;; Variadic: (logior A B C D...) -> (logior (logior A B) C D...)
                        (sys:compile (cons 'logior (cons (list 'logior (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op 'logxor)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (sys:compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bxor (sys:compile (car args) env fenv) (sys:compile (cadr args) env fenv))
                        ;; Variadic: (logxor A B C D...) -> (logxor (logxor A B) C D...)
                        (sys:compile (cons 'logxor (cons (list 'logxor (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op 'lognot)
          ;; Bitwise NOT - use mvn-ir for ARM64 MVN instruction
          (list 'mvn-ir (sys:compile (cadr expr) env fenv)))
         ((eq op 'ash)
          (list 'bsh (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op '=)
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'eq)
          ;; eq is pointer equality - same as = for our tagged values
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ((eq op 'eql)
          ;; eql is eq for symbols, = for numbers - for tagged values same as eq
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
         ;; case - multi-branch conditional with key comparison
         ;; (case keyform (key1 body1...) (key2 body2...) ... [(t default...)])
         ((eq op 'case)
          (let* ((keyform (cadr expr))
                 (clauses (cddr expr))
                 ;; Generate a gensym for the key variable
                 (key-var (gensym "CASE-KEY-")))
            ;; Expand to: (let ((key-var keyform)) (cond ...))
            (sys:compile
             `(let ((,key-var ,keyform))
                (cond ,@(mapcar (lambda (clause)
                                  (let ((keys (car clause))
                                        (body (cdr clause)))
                                    (cond
                                      ;; t or otherwise clause
                                      ((or (eq keys 't) (eq keys 'otherwise))
                                       `(t ,@body))
                                      ;; Single key
                                      ((atom keys)
                                       `((eql ,key-var ',keys) ,@body))
                                      ;; Multiple keys - (k1 k2 k3)
                                      (t
                                       `((or ,@(mapcar (lambda (k) `(eql ,key-var ',k)) keys))
                                         ,@body)))))
                                clauses)))
             env fenv)))
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
         ;; match - pattern matching (uses expand-match from expand.lisp)
         ((eq op 'match)
          (sys:compile (expand-match (cadr expr) (cddr expr)) env fenv))
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
          ;; nil = 0 in hybrid scheme
          (list 'cmp-eq (sys:compile (cadr expr) env fenv) '(nil-ir)))
        ((eq op 'numberp)
         ;; Hybrid scheme: fixnum has bit0=1, use numberp-ir to check
         (list 'numberp-ir (sys:compile (cadr expr) env fenv)))
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
         ((eq op 'keywordp)
          ;; Keyword tag is 7, so compare with (lit 7) -> becomes 7<<4=112
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 7)))
         ((eq op 'closurep)
          ;; Closure tag is 5, so compare with (lit 5) -> becomes 5<<4=80
          (list 'cmp-eq (list 'get-tag (sys:compile (cadr expr) env fenv)) (list 'lit 5)))
         ;; get-tag - extract type tag as tagged fixnum (tag << 4)
         ((eq op 'get-tag)
          (list 'get-tag (sys:compile (cadr expr) env fenv)))
         ;; set-tag - change tag bits on a pointer value
         ;; (set-tag value new-tag) -> value with low 4 bits replaced by (untag new-tag)
         ((eq op 'set-tag)
          (list 'set-tag (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)))
         ;; length - polymorphic: works on strings, vectors, and lists
         ((eq op 'length)
          (let ((val-sym (gensym "VAL"))
                (len-iter-fn (gensym "LEN-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC")))
            (sys:compile
             `(let ((,val-sym ,(cadr expr)))
                (cond
                  ((stringp ,val-sym) (string-length ,val-sym))
                  ((vectorp ,val-sym) (vector-length ,val-sym))
                  (t (labels ((,len-iter-fn (,lst-var ,acc-var)
                                (if (null ,lst-var)
                                    ,acc-var
                                    (,len-iter-fn (cdr ,lst-var) (+ ,acc-var 1)))))
                       (,len-iter-fn ,val-sym 0)))))
             env fenv)))
         ;; reverse - DISABLED: was inlining labels but had codegen bug
         ;; Now calls user-defined reverse function instead
         ;; ((eq op 'reverse) ...)

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
         ;; declare - ignore declarations (they are hints to the compiler, not code)
         ((eq op 'declare)
          '(nil-ir))
         ;; progn - evaluate forms in sequence, return last
         ((eq op 'progn)
          (let ((forms (cdr expr)))
            (if (null forms)
                (list 'lit 0)
                (if (null (cdr forms))
                    (sys:compile (car forms) env fenv)
                    (list 'progn-ir
                          (mapcar (lambda (f) (sys:compile f env fenv)) forms))))))
         ;; block - establish named exit point
         ;; (block name body...) -> (block-ir id body-ir)
         ((eq op 'block)
          (let* ((name (cadr expr))
                 (body (cddr expr))
                 (block-id (make-block-id name))
                 ;; Push block onto environment
                 (*block-env* (cons (cons name block-id) *block-env*)))
            (list 'block-ir block-id
                  (if (null body)
                      (list 'nil-ir)
                      (if (null (cdr body))
                          (sys:compile (car body) env fenv)
                          (sys:compile (cons 'progn body) env fenv))))))
         ;; return-from - exit to named block with value
         ;; (return-from name value) -> (return-from-ir id value-ir)
         ((eq op 'return-from)
          (let* ((name (cadr expr))
                 (value (if (cddr expr) (caddr expr) nil))
                 (block-entry (find-block name)))
            (if block-entry
                (list 'return-from-ir (cdr block-entry)
                      (sys:compile value env fenv))
                (error "RETURN-FROM: no block named ~S" name))))
         ;; return - shorthand for (return-from nil value)
         ((eq op 'return)
          (sys:compile (list 'return-from nil (cadr expr)) env fenv))
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
         ;; not - logical not (nil = 0 in hybrid scheme)
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
                ;; Check if it's a known global variable
                (let ((global-entry (assoc var *known-globals*)))
                  (if global-entry
                      ;; Emit setter IR form for the global
                      (list (cdr (cdr global-entry)) (sys:compile val env fenv))
                      ;; Check if it's a user-defined global (defvar/defparameter)
                      (let ((defined-entry (and (boundp '*defined-globals*)
                                                *defined-globals*
                                                (assoc var *defined-globals*))))
                        (if defined-entry
                            ;; Emit vector-set on globals vector
                            ;; Entry is (name index init-form)
                            (list 'vector-set-ir
                                  '(get-global-vars-ir)
                                  (list 'lit (cadr defined-entry))
                                  (sys:compile val env fenv))
                            ;; Unknown variable - return nil
                            (list 'lit 0))))))))
         ;; incf - increment variable: (incf place delta) -> (setq place (+ place delta))
         ((eq op 'incf)
          (let* ((place (cadr expr))
                 (delta (if (cddr expr) (caddr expr) 1)))
            (sys:compile `(setq ,place (+ ,place ,delta)) env fenv)))
         ;; decf - decrement variable: (decf place delta) -> (setq place (- place delta))
         ((eq op 'decf)
          (let* ((place (cadr expr))
                 (delta (if (cddr expr) (caddr expr) 1)))
            (sys:compile `(setq ,place (- ,place ,delta)) env fenv)))
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
         ;; string-ref - get character at index (returns char code)
         ((eq op 'string-ref)
          (list 'string-ref-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; char - CL alias for string-ref (for compatibility with CL code)
         ((eq op 'char)
          (list 'string-ref-ir (sys:compile (cadr expr) env fenv) (sys:compile (caddr expr) env fenv)))
         ;; char-code - identity since string-ref already returns char code
         ;; In CL, (char-code #\A) returns 65. In habu, characters ARE their codes.
         ((eq op 'char-code)
          (sys:compile (cadr expr) env fenv))
         ;; char-at - safe string-ref that returns 0 beyond end
         ;; Expands to: (if (>= pos (string-length str)) 0 (string-ref str pos))
         ((eq op 'char-at)
          (let ((str-sym (gensym "STR"))
                (pos-sym (gensym "POS")))
            (sys:compile `(let ((,str-sym ,(cadr expr))
                                (,pos-sym ,(caddr expr)))
                            (if (>= ,pos-sym (string-length ,str-sym))
                                0
                                (string-ref ,str-sym ,pos-sym)))
                         env fenv)))
         ;; char-code - in Habu, characters ARE fixnums, so this is identity
         ((eq op 'char-code)
          (sys:compile (cadr expr) env fenv))
         ;; code-char - in Habu, characters ARE fixnums, so this is identity
         ((eq op 'code-char)
          (sys:compile (cadr expr) env fenv))
         ;; string-concat - alias for string-append
         ((eq op 'string-concat)
          (sys:compile (list 'string-append (cadr expr) (caddr expr)) env fenv))
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
         ;; error - call fatal-error with message for stack trace and proper crash
         ((eq op 'error)
          (let ((msg-ir (if (cdr expr)
                            (sys:compile (cadr expr) env fenv)
                            (list 'str-lit "error"))))
            (list 'call-fn 'fatal-error (list msg-ir))))
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
         ;; buffer-byte-set - set raw byte at index in vector data area
         ;; Used for building strings byte-by-byte
         ((eq op 'buffer-byte-set)
          (list 'buffer-byte-set-ir
                (sys:compile (cadr expr) env fenv)    ; vector
                (sys:compile (caddr expr) env fenv)   ; index
                (sys:compile (cadddr expr) env fenv))) ; byte value
         ;; make-string-from-vector - convert vector of char codes to string
         ((eq op 'make-string-from-vector)
          (list 'make-string-from-vector-ir (sys:compile (cadr expr) env fenv)))
         ;; make-string - CL-compatible: (make-string size &key initial-element)
         ;; Simplified: (make-string n) or (make-string n init-char-code)
         ((eq op 'make-string)
          (if (cddr expr)
              ;; (make-string n init-char)
              (list 'make-string-ir
                    (sys:compile (cadr expr) env fenv)
                    (sys:compile (caddr expr) env fenv))
              ;; (make-string n) - initialize to 0
              (list 'make-string-ir
                    (sys:compile (cadr expr) env fenv)
                    (list 'lit 0))))
         ;; string-set! - set character at index (consistent with string-ref)
         ;; (string-set! string index char-code) -> returns char-code
         ((eq op 'string-set!)
          (list 'string-set!-ir
                (sys:compile (cadr expr) env fenv)
                (sys:compile (caddr expr) env fenv)
                (sys:compile (cadddr expr) env fenv)))
         ;; substring - compile as function call to existing substring function
         ((eq op 'substring)
          (list 'call-fn 'substring
                (list (sys:compile (cadr expr) env fenv)
                      (sys:compile (caddr expr) env fenv)
                      (sys:compile (cadddr expr) env fenv))))
         ;; buffer-to-string - convert raw byte buffer to string (for sys-read data)
         ((eq op 'buffer-to-string)
          (list 'buffer-to-string-ir
                (sys:compile (cadr expr) env fenv)    ; buffer
                (sys:compile (caddr expr) env fenv))) ; length
         ;; make-symbol-from-string - create NEW symbol (not interned!)
         ((eq op 'make-symbol-from-string)
          (list 'make-symbol-from-string-ir (sys:compile (cadr expr) env fenv)))
         ;; make-symbol - CL function to create uninterned symbol (same as make-symbol-from-string)
         ((eq op 'make-symbol)
          (list 'make-symbol-from-string-ir (sys:compile (cadr expr) env fenv)))
         ;; NOTE: intern is NOT an intrinsic - it's a regular function that
         ;; uses get-intern-table/set-intern-table to manage the intern table
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
         ;; sys-write-char - write a single character to fd (char as fixnum)
         ((eq op 'sys-write-char)
          (list 'sys-write-char-ir
                (sys:compile (cadr expr) env fenv)    ; fd
                (sys:compile (caddr expr) env fenv))) ; char (fixnum)
         ;; sys-read - read(fd, buf, len) -> returns bytes read
         ((eq op 'sys-read)
          (list 'sys-read-ir
                (sys:compile (cadr expr) env fenv)    ; fd
                (sys:compile (caddr expr) env fenv)   ; buf (vector)
                (sys:compile (cadddr expr) env fenv))) ; len
         ;; sys-read-byte - read a single byte from fd -> byte (0-255) or -1 on EOF/error
         ((eq op 'sys-read-byte)
          (list 'sys-read-byte-ir
                (sys:compile (cadr expr) env fenv)))  ; fd
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
         ;; === JIT Memory Primitives (ARM64 macOS) ===
         ;; mmap - mmap(addr, len, prot, flags, fd, offset) -> addr or -1
         ((eq op 'mmap)
          (list 'mmap-ir
                (sys:compile (cadr expr) env fenv)      ; addr
                (sys:compile (caddr expr) env fenv)     ; len
                (sys:compile (cadddr expr) env fenv)    ; prot
                (sys:compile (car (cddddr expr)) env fenv)   ; flags
                (sys:compile (cadr (cddddr expr)) env fenv)  ; fd
                (sys:compile (caddr (cddddr expr)) env fenv))) ; offset
         ;; munmap - munmap(addr, len) -> 0 on success
         ((eq op 'munmap)
          (list 'munmap-ir
                (sys:compile (cadr expr) env fenv)    ; addr
                (sys:compile (caddr expr) env fenv))) ; len
         ;; pthread-jit-write-protect-np - pthread_jit_write_protect_np(enabled)
         ;; enabled = 0: allow write, disallow execute
         ;; enabled = 1: disallow write, allow execute
         ((eq op 'pthread-jit-write-protect-np)
          (list 'pthread-jit-write-protect-np-ir
                (sys:compile (cadr expr) env fenv)))  ; enabled
         ;; sys-dcache-flush - sys_dcache_flush(start, size)
         ((eq op 'sys-dcache-flush)
          (list 'sys-dcache-flush-ir
                (sys:compile (cadr expr) env fenv)    ; start
                (sys:compile (caddr expr) env fenv))) ; size
         ;; sys-icache-invalidate - sys_icache_invalidate(start, size)
         ((eq op 'sys-icache-invalidate)
          (list 'sys-icache-invalidate-ir
                (sys:compile (cadr expr) env fenv)    ; start
                (sys:compile (caddr expr) env fenv))) ; size
         ;; funcall-ptr - call function pointer with no args, returns x0
         ((eq op 'funcall-ptr)
          (list 'funcall-ptr-ir
                (sys:compile (cadr expr) env fenv)))  ; code pointer
         ;; mem-set-byte - store a byte to memory at ptr+offset
         ((eq op 'mem-set-byte)
          (list 'mem-set-byte-ir
                (sys:compile (cadr expr) env fenv)    ; ptr
                (sys:compile (caddr expr) env fenv)   ; offset
                (sys:compile (cadddr expr) env fenv))) ; byte value
         ;; mem-load-64 - load 64-bit word from memory at ptr+offset
         ((eq op 'mem-load-64)
          (list 'mem-load-64-ir
                (sys:compile (cadr expr) env fenv)    ; ptr
                (sys:compile (caddr expr) env fenv))) ; offset
         ;; mem-load-byte - load single byte from memory at ptr+offset
         ((eq op 'mem-load-byte)
          (list 'mem-load-byte-ir
                (sys:compile (cadr expr) env fenv)    ; ptr
                (sys:compile (caddr expr) env fenv))) ; offset
         ;; === JIT Wrapper Primitives (simplified API) ===
         ;; jit-mmap - allocate JIT-capable memory
         ;; (jit-mmap size) - allocates size bytes with MAP_JIT flag
         ((eq op 'jit-mmap)
          (list 'mmap-jit-ir
                (sys:compile (cadr expr) env fenv)))  ; size
         ;; jit-write-protect - toggle JIT write protection
         ;; (jit-write-protect enabled) - 0=writable, 1=executable
         ((eq op 'jit-write-protect)
          (list 'pthread-jit-write-protect-np-ir
                (sys:compile (cadr expr) env fenv)))  ; enabled
         ;; jit-dcache-flush - flush data cache
         ;; (jit-dcache-flush start size)
         ((eq op 'jit-dcache-flush)
          (list 'sys-dcache-flush-ir
                (sys:compile (cadr expr) env fenv)    ; start
                (sys:compile (caddr expr) env fenv))) ; size
         ;; jit-icache-invalidate - invalidate instruction cache
         ;; (jit-icache-invalidate start size)
         ((eq op 'jit-icache-invalidate)
          (list 'sys-icache-invalidate-ir
                (sys:compile (cadr expr) env fenv)    ; start
                (sys:compile (caddr expr) env fenv))) ; size
         ;; jit-call - call JIT-compiled code pointer with optional args
         ;; (jit-call ptr &rest args)
         ((eq op 'jit-call)
          (list 'funcall-ptr-ir
                (sys:compile (cadr expr) env fenv)    ; code pointer
                (mapcar (lambda (a) (sys:compile a env fenv)) (cddr expr)))) ; args
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
         ;; get-symbol-counter - get the symbol counter from [x27+48]
         ((eq op 'get-symbol-counter)
          (list 'get-symbol-counter-ir))
         ;; set-symbol-counter - set the symbol counter at [x27+48]
         ((eq op 'set-symbol-counter)
          (list 'set-symbol-counter-ir
                (sys:compile (cadr expr) env fenv)))
         ;; get-symbol-table-sym - get the symbol table from [x27+56]
         ((eq op 'get-symbol-table-sym)
          (list 'get-symbol-table-sym-ir))
         ;; set-symbol-table-sym - set the symbol table at [x27+56]
         ((eq op 'set-symbol-table-sym)
          (list 'set-symbol-table-sym-ir
                (sys:compile (cadr expr) env fenv)))
         ;; get-frame-pointer - get the current frame pointer (x29) for stack walking
         ((eq op 'get-frame-pointer)
          (list 'get-frame-pointer-ir))
         ;; get-code-base - get the code base pointer (x26) for symbol table access
         ((eq op 'get-code-base)
          (list 'get-code-base-ir))
         ;; get-symtab-offset - get embedded symbol table offset from [x27+112]
         ((eq op 'get-symtab-offset)
          (list 'get-symtab-offset-ir))
         ;; get-symtab-count - get embedded symbol table entry count from [x27+120]
         ((eq op 'get-symtab-count)
          (list 'get-symtab-count-ir))
         ;; get-keyword-table - get the global keyword table from [x27+128]
         ((eq op 'get-keyword-table)
          (list 'get-keyword-table-ir))
         ;; set-keyword-table - set the global keyword table at [x27+128]
         ((eq op 'set-keyword-table)
          (list 'set-keyword-table-ir
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
              ;; Record for link-time verification
              (progn
                (habu::record-call-target op)
                (list 'call-fn op (mapcar (lambda (a) (sys:compile a env fenv)) final-args)))))
           ;; op is a declared import - external function resolved at link time
           ((member (symbol-name op) *declared-imports*
                    :test #'string= :key #'symbol-name)
            (habu::record-call-target op)
            (list 'call-fn op (mapcar (lambda (a) (sys:compile a env fenv)) (cdr expr))))
           ;; op is a variable (parameter) - compile as funcall
           (t
            (let ((off (env-lookup op env)))
              (if (numberp off)
                  (list 'funcall-ir (list 'var off) (mapcar (lambda (a) (sys:compile a env fenv)) (cdr expr)))
                  ;; Unknown function - CRASH with error at compile time
                  (error "sys:compile: Unknown function ~S (not in fenv, not primitive, not local)" op))))))))))
    ;; Unknown expression type - error at compile time
    (t (error "sys:compile: unhandled expression type ~S" expr))))

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
    (t (error "eval-ir: Unhandled IR tag ~S" (if (consp ir) (car ir) ir)))))

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
            (fn-def (cdr (assoc fnm fenv)))
            (traced (traced-p fnm)))
       (if fn-def
           ;; fn-def = (name params body-ir param-base)
           (let* ((body-ir (caddr fn-def)))
             ;; Evaluate arguments
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
               (let ((arg-vals (eval-args args-ir nil)))
                 ;; Trace entry
                 (when traced
                   (trace-enter fnm arg-vals)
                   (incf *trace-depth*))
                 ;; Call with new env containing args
                 (let ((result (eval-ir-with-fns body-ir arg-vals fenv)))
                   ;; Trace exit
                   (when traced
                     (decf *trace-depth*)
                     (trace-exit fnm result))
                   result))))
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
           ;; body is now pre-compiled IR (no tracing for anonymous closures)
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
           (let* ((fn-def (cdr (assoc fn-val fenv)))
                  (traced (and (symbolp fn-val) (traced-p fn-val))))
             (if fn-def
                 (let* ((body-ir (caddr fn-def)))
                   (labels ((eval-args (airs acc)
                              (if (null airs) (reverse acc)
                                  (eval-args (cdr airs)
                                             (cons (eval-ir-with-fns (car airs) env fenv) acc)))))
                     (let ((arg-vals (eval-args args-ir nil)))
                       ;; Trace entry
                       (when traced
                         (trace-enter fn-val arg-vals)
                         (incf *trace-depth*))
                       ;; Call function
                       (let ((result (eval-ir-with-fns body-ir arg-vals fenv)))
                         ;; Trace exit
                         (when traced
                           (decf *trace-depth*)
                           (trace-exit fn-val result))
                         result))))
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
    (t (error "eval-ir-with-fns: Unhandled IR tag ~S" (if (consp ir) (car ir) ir)))))


;;; ============================================================
;;; Part 7: Code Generator (REMOVED)
;;; ============================================================
;;;
;;; The old accumulator-based codegen has been REMOVED.
;;; All code generation now goes through the register-allocated path:
;;;   - Functions: codegen-fn -> codegen-fn-reg-alloc (reg-alloc.lisp)
;;;   - Main: codegen-main -> codegen-main-reg-alloc (reg-alloc.lisp)
;;;
;;; The following functions are stubs that ERROR if called:

;; NOTE: codegen function is now defined in codegen.lisp with 3 args:
;;   (defun codegen (linear-ir rtaddrs fnoffs) ...)
;; The old 4-arg accumulator codegen has been completely removed.
;; See codegen.lisp for the linear IR -> ARM64 code generator.

(defun codegen-binop (left-ir right-ir op-instrs rtaddrs fnoffs td)
  "OLD ACCUMULATOR CODEGEN HELPER - REMOVED."
  (declare (ignore left-ir right-ir op-instrs rtaddrs fnoffs td))
  (error "OLD ACCUMULATOR CODEGEN REMOVED: codegen-binop should not be called"))

(defun ir-may-call? (ir)
  "OLD ACCUMULATOR CODEGEN HELPER - REMOVED."
  (declare (ignore ir))
  (error "OLD ACCUMULATOR CODEGEN REMOVED: ir-may-call? should not be called"))

(defun ir-is-simple? (ir)
  "OLD ACCUMULATOR CODEGEN HELPER - REMOVED."
  (declare (ignore ir))
  (error "OLD ACCUMULATOR CODEGEN REMOVED: ir-is-simple? should not be called"))

(defun cmp-result-to-bool (cond-code)
  "OLD ACCUMULATOR CODEGEN HELPER - REMOVED."
  (declare (ignore cond-code))
  (error "OLD ACCUMULATOR CODEGEN REMOVED: cmp-result-to-bool should not be called"))

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

;; Multi-pass compilation for mutual recursion and constant support
;; Pass 0: Collect constants from defconstant forms
;; Pass 1: Collect all defun names into fenv with placeholder entries
;; Pass 2: Compile function bodies with complete fenv

(defun declaration-form-p (form)
  "Check if form is a declaration that should be skipped in code generation."
  (and (consp form)
       (member (car form) '(defconstant defvar defparameter in-package defpackage declaim defmacro))))

(defun collect-defmacros (forms)
  "Collect all defmacro definitions and register them in *macro-table*.
   Also evaluates non-CL macros in SBCL for proper backquote expansion.
   Recurses into progn forms. Must be called before any macro expansion."
  (dolist (f forms)
    (cond
      ((and (consp f) (eq (car f) 'defmacro))
       (let* ((name (cadr f))
              (params (caddr f))
              (body-forms (cdddr f))
              (body (if (null (cdr body-forms))
                        (car body-forms)
                        (cons 'progn body-forms)))
              (name-str (normalize-macro-name name)))
         ;; Store in *macro-table* for reference
         (setf (gethash name-str *macro-table*) (cons params body))
         ;; Only eval in SBCL if it's not a CL symbol (avoid package lock)
         ;; Check if the symbol is in the HABU package (not inherited from CL)
         (when (and (symbolp name)
                    (eq (symbol-package name) (find-package :habu)))
           (cl:eval f))))
      ((and (consp f) (eq (car f) 'progn))
       (collect-defmacros (cdr f))))))

(defun collect-constants (forms acc)
  "Collect all defconstant definitions into an alist of (name . value).
   Recurses into progn forms."
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defconstant))
           ;; (defconstant name value) - evaluate value at compile time
           (let* ((name (cadr f))
                  (value (caddr f))
                  ;; Evaluate constant value (should be a literal or simple expression)
                  (evaluated (if (numberp value) value
                                (if (and (consp value) (eq (car value) 'quote))
                                    (cadr value)
                                    (eval value)))))
             (collect-constants (cdr forms) (cons (cons name evaluated) acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn
           (collect-constants (cdr forms)
                             (collect-constants (cdr f) acc)))
          (t (collect-constants (cdr forms) acc))))))

(defun collect-globals (forms acc)
  "Collect all defvar/defparameter definitions into an alist of (name index . init-form).
   Recurses into progn forms. Init-form is nil if not provided.
   Index is the position in the globals vector (0-based)."
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (or (eq (car f) 'defvar) (eq (car f) 'defparameter)))
           ;; (defvar name) or (defvar name init-value)
           (let* ((name (cadr f))
                  (init-form (caddr f))  ; nil if not provided
                  (idx (length acc)))    ; index = current count
             (collect-globals (cdr forms) (cons (list name idx init-form) acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn
           (collect-globals (cdr forms)
                            (collect-globals (cdr f) acc)))
          (t (collect-globals (cdr forms) acc))))))

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
             ;; Mark as defined to clear any forward reference warnings
             (mark-function-defined name)
             (collect-defun-names (cdr forms) (cons (cons name parsed) acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (collect-defun-names (cdr forms)
                                   (collect-defun-names (cdr f) acc)))
          (t (collect-defun-names (cdr forms) acc))))))

#+sbcl
(defun collect-arm64-functions ()
  "Collect all exported ARM64 package functions for fenv.
   These are SBCL functions that return ARM64 instruction bytes.
   Returns alist of (symbol . (nil . nil)) for variadic params."
  (let ((acc nil))
    (do-external-symbols (sym (find-package :arm64))
      ;; Only include functions (not constants like +CC-EQ+)
      (when (and (fboundp sym)
                 (not (macro-function sym))
                 (not (special-operator-p sym)))
        (push (cons sym (cons nil nil)) acc)))
    acc))

#+sbcl
(defun collect-habu-functions ()
  "Collect all HABU package functions for fenv.
   These are SBCL-defined compiler helper functions.
   Only includes functions actually defined in HABU, not inherited CL functions.
   Returns alist of (symbol . (nil . nil)) for variadic params."
  (let ((acc nil)
        (habu-pkg (find-package :habu)))
    (do-symbols (sym habu-pkg)
      ;; Only include functions actually defined in HABU package
      ;; (not CL functions that are accessible via inheritance)
      (when (and (eq (symbol-package sym) habu-pkg)
                 (fboundp sym)
                 (not (macro-function sym))
                 (not (special-operator-p sym)))
        (push (cons sym (cons nil nil)) acc)))
    acc))

#+sbcl
(defun collect-arm64-constants ()
  "Collect all ARM64 package constants for *constants*.
   These are defconstant values like +EQ+, +NE+, etc.
   Returns alist of (symbol . value)."
  (let ((acc nil))
    (do-external-symbols (sym (find-package :arm64))
      ;; Only include bound symbols that are NOT functions (constants)
      (when (and (boundp sym)
                 (not (fboundp sym)))
        (push (cons sym (symbol-value sym)) acc)))
    acc))

#+sbcl
(defun collect-habu-constants ()
  "Collect all HABU package constants for *constants*."
  (let ((acc nil))
    (do-symbols (sym (find-package :habu))
      (when (and (boundp sym)
                 (not (fboundp sym))
                 (constantp sym))
        (push (cons sym (symbol-value sym)) acc)))
    acc))

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
             ;; DEBUG: Print all function names
             (format t "~%DEBUG: Compiled defun ~S~%" nm)
             ;; DEBUG: Print MAIN's IR in detail
             (when (string= (symbol-name nm) "MAIN")
               (format t "  ** FOUND MAIN **~%")
               (format t "  body-forms count: ~D~%" (length body-forms))
               (format t "  bd: ~S~%" (if (and (consp bd) (eq (car bd) 'progn))
                                          (list 'progn (length (cdr bd)) 'forms)
                                          bd))
               (format t "  IR tag: ~S~%" (car (third cf)))
               (when (eq (car (third cf)) 'progn-ir)
                 (format t "  IR progn forms: ~D~%" (length (cadr (third cf))))))
             (compile-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (compile-defuns (cdr forms) env fenv
                              (compile-defuns (cdr f) env fenv acc)))
          (t (compile-defuns (cdr forms) env fenv acc))))))

(defun find-main-form (forms)
  "Find all non-defun, non-declaration forms and wrap them in progn if more than one.
   Recurses into progn forms to strip nested defuns and declarations."
  (labels ((strip-defuns (fs acc)
             ;; Recursively collect non-defun, non-declaration forms, flattening progn
             (if (null fs)
                 acc
                 (let ((f (car fs)))
                   (cond
                     ((and (consp f) (eq (car f) 'defun))
                      ;; Skip defuns
                      (strip-defuns (cdr fs) acc))
                     ((declaration-form-p f)
                      ;; Skip declaration forms (defconstant, defvar, defparameter, in-package, defpackage)
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

(defun generate-globals-init-ir (globals fenv)
  "Generate IR to initialize the globals vector.
   Creates a vector of size N and initializes each slot.
   GLOBALS is an alist of (name index init-form).
   Returns IR that creates and initializes the vector, or nil if no globals."
  (if (null globals)
      nil
      (let* ((n (length globals))
             ;; Create vector: (set-global-vars (make-vector N))
             (create-ir (list 'set-global-vars-ir
                              (list 'make-vector-ir (list 'lit n))))
             ;; Initialize each slot
             (init-irs (mapcar (lambda (entry)
                                 (let* ((idx (cadr entry))
                                        (init-form (caddr entry))
                                        (init-ir (if init-form
                                                     (sys:compile init-form nil fenv)
                                                     '(nil-ir))))
                                   (list 'vector-set-ir
                                         '(get-global-vars-ir)
                                         (list 'lit idx)
                                         init-ir)))
                               globals)))
        ;; Combine into progn - progn-ir expects (progn-ir (form1 form2 ...))
        (list 'progn-ir (cons create-ir init-irs)))))

(defun compile-forms (forms)
  "Multi-pass compilation: collect macros, constants, globals, then names, then compile"
  ;; Pass 0: Collect macros first (they can be used in any subsequent form)
  (collect-defmacros forms)
  ;; Pass 1a: Collect constants from defconstant forms
  ;; Pass 1b: Collect globals from defvar/defparameter forms
  ;; Also add ARM64 package constants (SBCL only)
  (let* ((form-constants (collect-constants forms nil))
         #+sbcl
         (arm64-constants (collect-arm64-constants))
         #-sbcl
         (arm64-constants nil)
         #+sbcl
         (habu-constants (collect-habu-constants))
         #-sbcl
         (habu-constants nil)
         (*constants* (append form-constants arm64-constants habu-constants))
         (*defined-globals* (collect-globals forms nil))
         ;; Pass 2: Collect all defun names
         (fn-names (collect-defun-names forms nil))
         ;; Add declared imports to fn-names (they have unknown params, use nil)
         (import-fenv (mapcar (lambda (name) (cons name (cons nil nil))) *declared-imports*))
         ;; Add ARM64 package functions (SBCL only - they generate instruction bytes)
         #+sbcl
         (arm64-fenv (collect-arm64-functions))
         #-sbcl
         (arm64-fenv nil)
         ;; Add HABU package functions (SBCL only - compiler helpers like load-addr)
         #+sbcl
         (habu-fenv (collect-habu-functions))
         #-sbcl
         (habu-fenv nil)
         ;; Build fenv with all function names + imports + ARM64 + HABU functions
         (fenv (append fn-names import-fenv arm64-fenv habu-fenv)))
    ;; Pass 2: Compile all defuns with complete fenv
    (let* ((compiled-fns (reverse (compile-defuns forms nil fenv nil)))
           ;; Find and compile the main expression
           (main-form (find-main-form forms))
           ;; If no explicit main form but there's a defun named MAIN, call it
           ;; fn-names is an alist ((name . params) ...), and name is HABU::MAIN
           (main-ir (if main-form
                        (sys:compile main-form nil fenv)
                        (if (assoc 'habu::main fn-names)
                            (sys:compile '(habu::main) nil fenv)
                            (list 'lit 0))))
           ;; Generate globals initialization if needed
           (globals-init-ir (generate-globals-init-ir *defined-globals* fenv))
           ;; Wrap main-ir with globals init if there are globals
           (final-main-ir (if globals-init-ir
                              (list 'progn-ir (list globals-init-ir main-ir))
                              main-ir)))
      (list compiled-fns final-main-ir))))

(defun gen-param-stores (params base idx acc &key leaf)
  "Store function parameters to stack frame.
   Args 0-7 come from registers x0-x7.
   Args 8+ come from caller's stack at [sp + frame_size + (i-8)*8].
   Frame size is 0x200 for leaf functions, 0x400 for non-leaf."
  (let ((arg-regs '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7))
        (result-reversed acc))
    (let ((i idx))
      (dolist (param params)
        (let* ((frame-size (if leaf #x1000 #x1000))  ; Must match fn-prologue - now 4KB for all functions
               (st (if (< i 8)
                       ;; Args 0-7: copy from register xi to stack
                       (append (arm64:mov :x22 (nth i arg-regs))
                               (arm64:sub :x21 :env (* (+ base i) 8) :imm t)
                               (arm64:str :x22 :x21 :offset 0))
                       ;; Args 8+: load from caller's stack, store to our env frame
                       ;; Caller's stack args are at [sp + frame_size + (i-8)*8]
                       (let ((stack-off (+ frame-size (* (- i 8) 8))))
                         (append (arm64:ldr :x22 :sp :offset stack-off)
                                 (arm64:sub :x21 :env (* (+ base i) 8) :imm t)
                                 (arm64:str :x22 :x21 :offset 0))))))
          (setq result-reversed (revappend st result-reversed))
          (setq i (+ i 1)))))
    (nreverse result-reversed)))

(defun fn-prologue (frame-size x20-offset &key leaf)
  "Function prologue: allocate frame, save caller's x20/lr/x24, set up new env base.
   Frame size and x20 offset are dynamically calculated based on function needs.
   x24 must be preserved across calls so defuns with internal labels don't clobber
   the caller's closure environment.
   If :leaf t, skip x24 save (leaf functions don't call other functions)."
  (if leaf
      ;; Leaf function: skip x24 save
      (append
       (arm64:sub :sp :sp frame-size :imm t)   ; SUB sp, sp, #frame-size
       (arm64:stp :env :lr :sp :offset 0)      ; STP x20, lr, [sp, #0] (save x20 and return addr)
       (arm64:add :env :sp x20-offset :imm t))  ; ADD x20, sp, #x20-offset (env base)
      ;; Non-leaf function: full frame with x24 save
      (append
       (arm64:sub :sp :sp frame-size :imm t)   ; SUB sp, sp, #frame-size (allocate function frame)
       (arm64:stp :env :lr :sp :offset 0)      ; STP x20, lr, [sp, #0] (save caller's x20 and return addr)
       (arm64:str :closure :sp :offset 16)        ; STR x24, [sp, #16] (save caller's closure env)
       (arm64:add :env :sp x20-offset :imm t)))) ; ADD x20, sp, #x20-offset (env base past spill area)

(defun fn-epilogue (frame-size &key leaf)
  "Function epilogue: restore caller's x20/lr/x24, deallocate frame, return
   If :leaf t, skip x24 restore."
  (if leaf
      ;; Leaf function: skip x24 restore
      (append
       (arm64:ldp :env :lr :sp :offset 0)    ; LDP x20, lr, [sp, #0] (restore x20 and lr)
       (arm64:add :sp :sp frame-size :imm t))  ; ADD sp, sp, #frame-size (deallocate leaf frame)
      ;; Non-leaf function: full restore
      (append
       (arm64:ldr :closure :sp :offset 16)       ; LDR x24, [sp, #16] (restore caller's closure env)
       (arm64:ldp :env :lr :sp :offset 0)     ; LDP x20, lr, [sp, #0] (restore caller's x20 and lr)
       (arm64:add :sp :sp frame-size :imm t)))) ; ADD sp, sp, #frame-size (deallocate function frame)

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
                (arm64:movz :x11 #xF)
                (arm64:bic :x9 :closure :x11)
                ;; Get car (the captured value): x0 = [x9+0]
                (arm64:ldr :x0 :x9 :offset 0)
                ;; Store result to stack slot idx
                (arm64:sub :x21 :env (* idx 8) :imm t)
                (arm64:str :x0 :x21 :offset 0)
                ;; Move x24 to cdr (next cons cell): x24 = [x9+8]
                (arm64:ldr :closure :x9 :offset 8)))))
        (gen-capture-copies count (+ idx 1) (append acc copy-code)))))

(defun save-params-to-temps (count idx acc)
  "Save param registers x0..xN to sp-relative slots to preserve them during capture copy.
   Uses offsets 24, 32, 40, ... (after saved x20/lr at 0, x24 at 16).
   These are within the function's stack frame and won't be clobbered by nested calls."
  (if (>= idx count)
      acc
      (let* ((off (+ 24 (* idx 8)))  ; Start at sp+24, after saved regs
             (save-code (append-all
                         (list
                          (arm64:str (arm64:num-to-reg idx) :sp :offset off)))))  ; str xi, [sp, #off]
        (save-params-to-temps count (+ idx 1) (append acc save-code)))))

(defun restore-params-from-temps (params base count idx acc)
  "Restore params from sp-relative temp slots and store to final env slots at base+idx."
  (if (null params)
      acc
      (let* ((temp-off (+ 24 (* idx 8)))  ; Match save-params-to-temps offsets
             (final-off (* (+ base idx) 8))
             (restore-code (append-all
                            (list
                             ;; Load from sp-relative temp slot
                             (arm64:ldr :x22 :sp :offset temp-off)
                             ;; Store to final env slot (x20-relative)
                             (arm64:sub :x21 :env final-off :imm t)
                             (arm64:str :x22 :x21 :offset 0)))))
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
    ;; TCO: loop-ir wraps body
    ((has-tag ir 'loop-ir)
     (count-max-env-offset (cadr ir)))
    ;; TCO: continue-ir has arg expressions
    ((has-tag ir 'continue-ir)
     (apply #'max 0 (mapcar #'count-max-env-offset (cadr ir))))
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
     ;; funcall-ir uses slots: td+0 to td+4 (x24/x20/x30/code/env saves)
     ;; plus td+5 to td+5+num_args-1 for arg storage
     ;; nested evaluation happens at td+5+num_args
     (let* ((closure (cadr ir))
            (args (caddr ir))
            (num-args (length args))
            (nested-base (+ depth 5 num-args))
            (closure-depth (count-max-temp-depth closure nested-base))
            (arg-depths (mapcar (lambda (a) (count-max-temp-depth a nested-base)) args)))
       (apply #'max nested-base closure-depth arg-depths)))
    ;; TCO: loop-ir wraps body
    ((has-tag ir 'loop-ir)
     (count-max-temp-depth (cadr ir) depth))
    ;; TCO: continue-ir has arg expressions evaluated to temps, then copied to params
    ;; Each arg needs a temp slot, plus 1 for x24 save
    ((has-tag ir 'continue-ir)
     (let* ((args (cadr ir))
            (nargs (length args))
            ;; continue-ir uses: xs (x24 save), spill slots for args
            ;; The spill slot access is at (spill-slot td idx), not temp-slot
            ;; But we need temp depth to not overlap with arg evaluation
            (arg-depths (mapcar (lambda (a) (count-max-temp-depth a (+ depth 1))) args)))
       (apply #'max (+ depth nargs 1) arg-depths)))
    ;; Default: check all children, filtering out non-list elements
    (t
     (apply #'max depth (mapcar (lambda (child) (count-max-temp-depth child depth))
                                (remove-if-not #'consp (cdr ir)))))))

(defun codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function using register-allocated linear codegen.
   Defun format:  (name params body param-base)  ; param-base is a number
   Lambda format: (name params body free-vars free-offsets)  ; free-vars is a list or nil

   NOTE: This ONLY uses register-allocated codegen from reg-alloc.lisp.
   The old accumulator-based codegen had register clobbering bugs and was removed."
  (declare (ignore rtaddrs fnoffs))
  (let ((code (codegen-fn-reg-alloc fn)))
    (unless code
      (error "codegen-fn-reg-alloc failed for ~A - IR not supported" (car fn)))
    code))

(defun codegen-main (mir rtaddrs)
  "Generate main program code using register-allocated codegen.
   NOTE: This now uses codegen-main-reg-alloc exclusively.
   The old accumulator-based codegen has been removed."
  (declare (ignore rtaddrs))
  (codegen-main-reg-alloc mir))

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
               ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir)
                    (has-tag ir 'set-global-vars-ir))
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
               ;; 6-arg IR nodes: mmap
               ((has-tag ir 'mmap-ir)
                (let ((arg1 (cadr ir))
                      (arg2 (caddr ir))
                      (arg3 (cadddr ir))
                      (arg4 (nth 4 ir))
                      (arg5 (nth 5 ir))
                      (arg6 (nth 6 ir)))
                  (let* ((mvb-result-m1 (lift arg1 lambdas)) (new-arg1 (car mvb-result-m1)) (l1 (cdr mvb-result-m1)))
                    (let* ((mvb-result-m2 (lift arg2 l1)) (new-arg2 (car mvb-result-m2)) (l2 (cdr mvb-result-m2)))
                    (let* ((mvb-result-m3 (lift arg3 l2)) (new-arg3 (car mvb-result-m3)) (l3 (cdr mvb-result-m3)))
                    (let* ((mvb-result-m4 (lift arg4 l3)) (new-arg4 (car mvb-result-m4)) (l4 (cdr mvb-result-m4)))
                    (let* ((mvb-result-m5 (lift arg5 l4)) (new-arg5 (car mvb-result-m5)) (l5 (cdr mvb-result-m5)))
                    (let* ((mvb-result-m6 (lift arg6 l5)) (new-arg6 (car mvb-result-m6)) (l6 (cdr mvb-result-m6)))
                    (cons (list 'mmap-ir new-arg1 new-arg2 new-arg3 new-arg4 new-arg5 new-arg6) l6)))))))))
               ;; 3-arg IR nodes: (tag arg1 arg2 arg3)
               ((or (has-tag ir 'vector-set-ir)
                    ;; sys-* IR nodes with 3 arguments
                    (has-tag ir 'sys-write-ir)
                    (has-tag ir 'sys-read-ir)
                    (has-tag ir 'sys-open-ir)
                    (has-tag ir 'mem-set-byte-ir)
                    ;; String operations with 3 args
                    (has-tag ir 'string-set!-ir)
                    (has-tag ir 'substring-ir))
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
                    (has-tag ir 'string-equal-ir)
                    (has-tag ir 'make-string-ir)
                    ;; JIT memory primitives with 2 args
                    (has-tag ir 'munmap-ir)
                    (has-tag ir 'sys-dcache-flush-ir)
                    (has-tag ir 'sys-icache-invalidate-ir)
                    (has-tag ir 'mem-load-64-ir)
                    ;; sys-write-char takes 2 args
                    (has-tag ir 'sys-write-char-ir))
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
                    (has-tag ir 'sys-close-ir)
                    (has-tag ir 'sys-read-byte-ir)
                    ;; JIT memory primitives with 1 arg
                    (has-tag ir 'pthread-jit-write-protect-np-ir)
                    (has-tag ir 'funcall-ptr-ir))
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
  "Calculate byte size of code that may contain call, loop, and block markers.
   Handles nested lists (from ARM64 instruction encoders)."
  (labels ((calc (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                     ((and (consp item)
                           (or (eq (car item) :loop-start)
                               (eq (car item) :block-start)
                               (eq (car item) :block-end)))
                      ;; Position markers only - no bytes
                      (calc (cdr items) acc))
                     ((and (consp item)
                           (or (eq (car item) :call-fn)
                               (eq (car item) :tail-call-fn)
                               (eq (car item) :extern-call)
                               (eq (car item) :continue)
                               (eq (car item) :loop-continue)
                               (eq (car item) :return-from)
                               (eq (car item) :lambda-ref-marker)))
                      ;; 4-byte instructions
                      (calc (cdr items) (+ acc 4)))
                     ((consp item)
                      ;; Nested list - recurse into it
                      (calc (cdr items) (+ acc (calc item 0))))
                     (t
                      ;; Single byte
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
         ;; Function bodies get standard optimizations (TCO applied later after lift-lambdas)
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
        (let* ((fns-raw (append lifted-defuns main-lambdas defun-lambdas))
               ;; Apply TCO: convert self-tail-calls to loops
               ;; This eliminates stack growth for recursive functions like COLLECT-DEFUNS
               (fns (if (and optimize (fboundp 'apply-tco-to-all-functions))
                        (apply-tco-to-all-functions fns-raw)
                        fns-raw)))
          (if (null fns)
              ;; No functions defined - simple case
              ;; Still need to resolve extern calls
              (resolve-calls (codegen-main mir rtaddrs) nil)
              ;; Functions defined - need linking
              ;; NOTE: codegen-main-reg-alloc generates code with :call-fn markers
              ;; that get resolved by resolve-calls at the end. The markers are
              ;; position-independent so we only need to generate code once.
              (let* (;; Generate main code - markers will be resolved later
                     (main-code (codegen-main-reg-alloc mir))
                     ;; Use code-size to handle markers
                     (main-size (code-size main-code))
                     ;; Build fnoffs starting after main code
                     (fnoffs (build-fnoffs fns main-size))
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
   1. First pass: collect all defun names with parsed param info into fenv
   2. Second pass: compile bodies with complete fenv, store in compiled-fns
   3. Evaluate non-defun forms using fenv for compilation, compiled-fns for execution"
  ;; Pass 1: Collect all defun names with their parsed lambda lists
  ;; fenv format: ((name . (positional-params . keyword-specs)) ...)
  (labels ((collect-defuns (fs acc)
             (if (null fs)
                 (reverse acc)
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (let* ((nm (cadr f))
                              (ps (caddr f))
                              (parsed (parse-lambda-list ps)))
                         (collect-defuns (cdr fs) (cons (cons nm parsed) acc)))
                       (collect-defuns (cdr fs) acc)))))
           ;; Compile all defuns with complete fenv, build compiled-fns alist
           ;; compiled-fns format: ((name . (name params body-ir param-base)) ...)
           (compile-defuns-to-alist (fs fenv compiled-fns other-forms)
             (if (null fs)
                 (list compiled-fns (reverse other-forms))
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (let* ((nm (cadr f))
                              (ps (caddr f))
                              (bd (cadddr f))
                              (cf (compile-defun nm ps bd nil fenv)))
                         (compile-defuns-to-alist (cdr fs) fenv
                                                  (cons (cons nm cf) compiled-fns)
                                                  other-forms))
                       ;; Non-defun form - save for later evaluation
                       (compile-defuns-to-alist (cdr fs) fenv compiled-fns (cons f other-forms))))))
           ;; Evaluate non-defun forms
           ;; Use fenv for sys:compile (has param info), compiled-fns for eval-ir-with-fns
           (do-eval-forms (fs fenv compiled-fns)
             (if (null fs)
                 0
                 (let* ((ir (sys:compile (car fs) nil fenv))
                        (result (eval-ir-with-fns ir nil compiled-fns)))
                   (if (null (cdr fs))
                       result
                       (do-eval-forms (cdr fs) fenv compiled-fns))))))
    ;; Execute two-pass compilation
    (let* ((fenv (collect-defuns forms nil))
           (result (compile-defuns-to-alist forms fenv nil nil))
           (compiled-fns (car result))
           (other-forms (cadr result)))
      (do-eval-forms other-forms fenv compiled-fns))))

(defun eval (form)
  "Evaluate FORM in the null lexical environment and return its value.
   CL-spec compliant: takes a single form, returns result."
  (eval-forms (list form)))

(defun compile (name &optional definition)
  "Compile a function. CL-spec compliant.
   If NAME is nil and DEFINITION is supplied, compiles the lambda and returns it.
   If NAME is a symbol, compiles its function definition.
   Returns: function, warnings-p, failure-p"
  (cond
    ;; (compile nil '(lambda ...)) - compile anonymous function
    ((and (null name) definition)
     (let* ((forms (list definition))
            (code (compile-program forms nil)))
       (values code nil nil)))
    ;; (compile 'name) - compile named function (not yet supported)
    ((symbolp name)
     (error "Compiling named functions not yet supported"))
    (t
     (error "Invalid arguments to compile"))))

(defun disassemble (fn)
  "Print disassembly of FN to *standard-output*. CL-spec compliant.
   FN can be a function, lambda expression, or function name."
  (disasm fn))

;;; ============================================================
;;; Part 9: Public API
;;; ============================================================

;; deliver function is in codegen.lisp (mmap-based approach)
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

;;; ============================================================
;;; Part 8b: Enhanced FASL Format (v3) with Symbol Tables and Imports
;;; ============================================================

;;; FASL Format v3:
;;; Header (48 bytes):
;;;   Magic:          4 bytes "HFSL"
;;;   Version:        4 bytes (3 for import support)
;;;   Flags:          4 bytes
;;;   Code-len:       4 bytes (actual bytes, markers replaced with NOPs)
;;;   Symtab-offset:  4 bytes (offset to symbol table from start of file)
;;;   Symtab-count:   4 bytes (number of exported symbols)
;;;   Import-offset:  4 bytes (offset to import table from start of file)
;;;   Import-count:   4 bytes (number of import entries)
;;;   Reserved:       16 bytes
;;; Code Section: N bytes of ARM64 machine code (extern-calls replaced with NOPs)
;;; Symbol Table: For each symbol: [name-len:4][name:N][offset:8]
;;; Import Table: For each import: [name-len:4][name:N][code-offset:4]

(defun extract-markers-from-bytecode (bytecode)
  "Extract call markers from bytecode.
   Returns (values clean-bytecode extern-calls internal-calls) where:
   - clean-bytecode: bytecode with markers replaced by NOP instruction bytes
   - extern-calls: list of (name . offset) pairs for C library calls
   - internal-calls: list of (type name offset) triples for Lisp function calls
     where type is :call or :tail-call"
  (let ((result nil)
        (extern-calls nil)
        (internal-calls nil)
        (pos 0))
    (dolist (item bytecode)
      (cond
        ;; Extern-call marker: (:extern-call name position)
        ((and (consp item) (eq (car item) :extern-call))
         (let ((name (cadr item))
               (marker-pos (if (cddr item) (caddr item) pos)))
           (push (cons name marker-pos) extern-calls)
           ;; Emit NOP (0xD503201F) as placeholder - 4 bytes little-endian
           (push #x1F result)
           (push #x20 result)
           (push #x03 result)
           (push #xD5 result)
           (incf pos 4)))
        ;; Internal call marker: (:call-fn name position)
        ((and (consp item) (eq (car item) :call-fn))
         (let ((name (cadr item)))
           (push (list :call name pos) internal-calls)
           ;; Emit NOP placeholder
           (push #x1F result)
           (push #x20 result)
           (push #x03 result)
           (push #xD5 result)
           (incf pos 4)))
        ;; Internal tail-call marker: (:tail-call-fn name position)
        ((and (consp item) (eq (car item) :tail-call-fn))
         (let ((name (cadr item)))
           (push (list :tail-call name pos) internal-calls)
           ;; Emit NOP placeholder
           (push #x1F result)
           (push #x20 result)
           (push #x03 result)
           (push #xD5 result)
           (incf pos 4)))
        ;; Regular byte
        ((integerp item)
         (push item result)
         (incf pos))
        ;; Unknown marker type - this indicates a compiler bug
        (t (error "extract-markers-from-bytecode: Unknown marker ~S at position ~D" item pos))))
    (values (nreverse result) (nreverse extern-calls) (nreverse internal-calls))))

;; Backward compatibility alias
(defun extract-extern-calls-from-bytecode (bytecode)
  "Extract extern-call markers from bytecode.
   Wrapper for backward compatibility."
  (multiple-value-bind (clean extern internal)
      (extract-markers-from-bytecode bytecode)
    (declare (ignore internal))
    (values clean extern)))

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
  "Write FASL v4 with symbol table, imports, and internal calls.
   bytecode-list: list of bytes or call markers
   symbol-table: alist of (name . offset) pairs
   output-path: file to write

   FASL v4 Format (64-byte header):
   - Magic: 4 bytes 'HFSL'
   - Version: 4 bytes (4)
   - Flags: 4 bytes
   - Code-len: 4 bytes
   - Symtab-offset: 4 bytes
   - Symtab-count: 4 bytes
   - Import-offset: 4 bytes
   - Import-count: 4 bytes
   - Internal-offset: 4 bytes  (NEW)
   - Internal-count: 4 bytes   (NEW)
   - Reserved: 24 bytes"
  ;; Extract all markers and clean the bytecode
  (multiple-value-bind (clean-bytecode extern-calls internal-calls)
      (extract-markers-from-bytecode bytecode-list)
    (with-open-file (out output-path :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :element-type '(unsigned-byte 8))
      (let* ((code-len (length clean-bytecode))
             (symtab-count (if symbol-table (length symbol-table) 0))
             (import-count (length extern-calls))
             (internal-count (length internal-calls))
             ;; Calculate sizes for offset computation
             (symtab-size (loop for entry in symbol-table
                                sum (+ 4 (length (symbol-name (car entry))) 8)))
             ;; Helper to normalize names to strings
             (normalize-entry-name (lambda (name)
                                     (if (symbolp name) (symbol-name name) name)))
             (import-size (loop for entry in extern-calls
                               sum (+ 4 (length (funcall normalize-entry-name (car entry))) 4)))
             (header-size 64)  ;; v4 uses 64-byte header
             (symtab-offset (+ header-size code-len))
             (import-offset (+ symtab-offset symtab-size))
             (internal-offset (+ import-offset import-size)))
        ;; Write header (64 bytes for v4)
        (write-byte #x48 out)  ;; 'H'
        (write-byte #x46 out)  ;; 'F'
        (write-byte #x53 out)  ;; 'S'
        (write-byte #x4C out)  ;; 'L'
        (write-u32-le 4 out)   ;; Version 4
        (write-u32-le 0 out)   ;; Flags
        (write-u32-le code-len out)
        (write-u32-le symtab-offset out)
        (write-u32-le symtab-count out)
        (write-u32-le import-offset out)
        (write-u32-le import-count out)
        (write-u32-le internal-offset out)  ;; NEW
        (write-u32-le internal-count out)   ;; NEW
        ;; Reserved (24 bytes = 6 u32s)
        (write-u32-le 0 out)
        (write-u32-le 0 out)
        (write-u32-le 0 out)
        (write-u32-le 0 out)
        (write-u32-le 0 out)
        (write-u32-le 0 out)
        ;; Write code section (clean bytes only)
        (dolist (byte clean-bytecode)
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
              (write-u64-le offset out))))
        ;; Write import table (extern calls)
        (when extern-calls
          (dolist (entry extern-calls)
            (let* ((name (funcall normalize-entry-name (car entry)))
                   (offset (cdr entry))
                   (name-bytes (map 'list #'char-code name))
                   (name-len (length name-bytes)))
              (write-u32-le name-len out)
              (dolist (byte name-bytes)
                (write-byte byte out))
              (write-u32-le offset out))))
        ;; Write internal call table (Lisp function calls)
        ;; Format: [type:1][name-len:4][name:N][offset:4]
        ;; type: 0 = call, 1 = tail-call
        (when internal-calls
          (dolist (entry internal-calls)
            (let* ((call-type (car entry))
                   (name (symbol-name (cadr entry)))
                   (offset (caddr entry))
                   (name-bytes (map 'list #'char-code name))
                   (name-len (length name-bytes)))
              ;; Write type byte
              (write-byte (if (eq call-type :call) 0 1) out)
              (write-u32-le name-len out)
              (dolist (byte name-bytes)
                (write-byte byte out))
              (write-u32-le offset out))))))))

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
  "Read FASL v2, v3, or v4 and return (bytecode symtab imports internal-calls).
   For v2: imports and internal-calls are nil.
   For v3: internal-calls is nil.
   For v4: full support for all tables.
   Also supports old FASL format (magic 'FASL') from fasl.lisp.
   symtab is an alist of (name . offset) pairs.
   imports is list of (name . code-offset) pairs for C calls.
   internal-calls is list of (type name offset) for Lisp function calls."
  (with-open-file (in fasl-path :direction :input
                                :element-type '(unsigned-byte 8))
    ;; Read and verify magic - accept both HFSL and FASL formats
    (let ((magic (list (read-byte in) (read-byte in) (read-byte in) (read-byte in))))
      (cond
        ;; HFSL format - continue with v2/v3/v4 parsing below
        ((equal magic '(#x48 #x46 #x53 #x4C)) nil)
        ;; FASL format from fasl.lisp - delegate to read-fasl and convert
        ((equal magic '(#x4C #x53 #x41 #x46))  ;; "FASL" little-endian
         (file-position in 0)  ;; Reset to start
         (multiple-value-bind (header functions code relocations constants str-table imports)
             (read-fasl fasl-path)
           (declare (ignore constants imports))
           ;; Convert to read-fasl-v2 format: (bytecode symtab imports internal-calls)
           (let ((symtab nil)
                 (internal-calls nil))
             ;; Build symtab from function table
             (dolist (fn functions)
               (let* ((name-offset (fasl-function-name-offset fn))
                      (name (read-string-from-table str-table name-offset)))
                 (push (cons (intern name :habu) (fasl-function-code-offset fn)) symtab)))
             ;; Convert relocations to internal-calls
             ;; Use interned symbols to match symtab format
             (dolist (reloc relocations)
               (when (= (fasl-relocation-type reloc) +reloc-fn-call+)
                 (let* ((target-idx (fasl-relocation-target reloc))
                        (target-fn (when (< target-idx (length functions))
                                     (nth target-idx functions)))
                        (target-name (when target-fn
                                       (read-string-from-table str-table
                                         (fasl-function-name-offset target-fn)))))
                   (when target-name
                     (push (list :call-fn (intern target-name :habu) (fasl-relocation-offset reloc))
                           internal-calls)))))
             (return-from read-fasl-v2
               (list (coerce code 'list) (nreverse symtab) nil (nreverse internal-calls))))))
        (t (error "Invalid FASL magic"))))
    ;; Read header - version determines format
    (let* ((version (read-u32-le in))
           (flags (read-u32-le in))
           (code-len (read-u32-le in))
           (symtab-offset (read-u32-le in))
           (symtab-count (read-u32-le in)))
      (declare (ignore flags symtab-offset))
      (cond
        ;; Version 2: 32-byte header, no imports
        ((= version 2)
         (read-u32-le in)  ; Skip reserved1
         (read-u32-le in)  ; Skip reserved2
         ;; Read code section
         (let ((bytecode (loop repeat code-len collect (read-byte in))))
           ;; Read symbol table
           (let ((symtab
                  (when (> symtab-count 0)
                    (loop repeat symtab-count
                          collect
                          (let* ((name-len (read-u32-le in))
                                 (name-bytes (loop repeat name-len collect (read-byte in)))
                                 (name-string (map 'string #'code-char name-bytes))
                                 (offset (read-u64-le in)))
                            (cons (intern name-string :habu) offset))))))
             (list bytecode symtab nil nil))))  ; No imports/internal for v2

        ;; Version 3: 48-byte header, with imports
        ((= version 3)
         (let ((import-offset (read-u32-le in))
               (import-count (read-u32-le in)))
           (declare (ignore import-offset))
           ;; Skip remaining reserved (16 bytes = 4 u32s)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           ;; Read code section
           (let ((bytecode (loop repeat code-len collect (read-byte in))))
             ;; Read symbol table
             (let ((symtab
                    (when (> symtab-count 0)
                      (loop repeat symtab-count
                            collect
                            (let* ((name-len (read-u32-le in))
                                   (name-bytes (loop repeat name-len collect (read-byte in)))
                                   (name-string (map 'string #'code-char name-bytes))
                                   (offset (read-u64-le in)))
                              (cons (intern name-string :habu) offset))))))
               ;; Read import table
               (let ((imports
                      (when (> import-count 0)
                        (loop repeat import-count
                              collect
                              (let* ((name-len (read-u32-le in))
                                     (name-bytes (loop repeat name-len collect (read-byte in)))
                                     (name-string (map 'string #'code-char name-bytes))
                                     (offset (read-u32-le in)))
                                (cons name-string offset))))))
                 (list bytecode symtab imports nil))))))  ; No internal-calls for v3

        ;; Version 4: 64-byte header, with imports and internal calls
        ((= version 4)
         (let ((import-offset (read-u32-le in))
               (import-count (read-u32-le in))
               (internal-offset (read-u32-le in))
               (internal-count (read-u32-le in)))
           (declare (ignore import-offset internal-offset))
           ;; Skip remaining reserved (24 bytes = 6 u32s)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           (read-u32-le in)
           ;; Read code section
           (let ((bytecode (loop repeat code-len collect (read-byte in))))
             ;; Read symbol table
             (let ((symtab
                    (when (> symtab-count 0)
                      (loop repeat symtab-count
                            collect
                            (let* ((name-len (read-u32-le in))
                                   (name-bytes (loop repeat name-len collect (read-byte in)))
                                   (name-string (map 'string #'code-char name-bytes))
                                   (offset (read-u64-le in)))
                              (cons (intern name-string :habu) offset))))))
               ;; Read import table
               (let ((imports
                      (when (> import-count 0)
                        (loop repeat import-count
                              collect
                              (let* ((name-len (read-u32-le in))
                                     (name-bytes (loop repeat name-len collect (read-byte in)))
                                     (name-string (map 'string #'code-char name-bytes))
                                     (offset (read-u32-le in)))
                                (cons name-string offset))))))
                 ;; Read internal call table
                 (let ((internal-calls
                        (when (> internal-count 0)
                          (loop repeat internal-count
                                collect
                                (let* ((type-byte (read-byte in))
                                       (call-type (if (= type-byte 0) :call :tail-call))
                                       (name-len (read-u32-le in))
                                       (name-bytes (loop repeat name-len collect (read-byte in)))
                                       (name-string (map 'string #'code-char name-bytes))
                                       (offset (read-u32-le in)))
                                  (list call-type (intern name-string :habu) offset))))))
                   (list bytecode symtab imports internal-calls)))))))

        (t (error "Unsupported FASL version: ~A" version))))))

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

;;; ============================================================
;;; Part 8c: Link-time Call Resolution
;;; ============================================================

(defun resolve-internal-calls (code symtab)
  "Resolve remaining :call-fn and :tail-call-fn markers at link time.
   CODE is a list of bytes and markers.
   SYMTAB is the global symbol table (alist of (name . offset)).
   Returns resolved bytecode with BL/B instructions."
  (let ((result nil)
        (pos 0))
    (dolist (item code)
      (cond
        ;; Internal call marker - resolve to BL
        ((and (consp item) (eq (car item) :call-fn))
         (let* ((fn-name (cadr item))
                (marker-pos (caddr item))
                (fn-entry (assoc fn-name symtab))
                (fn-pos (if fn-entry (cdr fn-entry)
                           (error "Link error: undefined function ~A at position ~A" fn-name marker-pos)))
                (rel-offset (- fn-pos pos))
                (bl-bytes (arm64:bl (ash rel-offset -2))))
           (dolist (b bl-bytes)
             (push b result))
           (incf pos 4)))
        ;; Tail call marker - resolve to B
        ((and (consp item) (eq (car item) :tail-call-fn))
         (let* ((fn-name (cadr item))
                (marker-pos (caddr item))
                (fn-entry (assoc fn-name symtab))
                (fn-pos (if fn-entry (cdr fn-entry)
                           (error "Link error: undefined function ~A at position ~A" fn-name marker-pos)))
                (rel-offset (- fn-pos pos))
                (b-bytes (arm64:b (ash rel-offset -2))))
           (dolist (b b-bytes)
             (push b result))
           (incf pos 4)))
        ;; Lambda-ref marker - resolve to ADR (load function address)
        ;; Format: (:lambda-ref-marker dest-reg lambda-name)
        ((and (consp item) (eq (car item) :lambda-ref-marker))
         (let* ((dest-reg (cadr item))
                (lambda-name (caddr item))
                (fn-entry (assoc lambda-name symtab))
                (fn-pos (if fn-entry (cdr fn-entry)
                           (error "Link error: undefined lambda ~A" lambda-name)))
                (rel-offset (- fn-pos pos))
                ;; ADR: rd = PC + imm21
                ;; Encode ADR instruction
                (adr-bytes (arm64:adr dest-reg rel-offset)))
           (dolist (b adr-bytes)
             (push b result))
           (incf pos 4)))
        ;; Other markers pass through (extern-call, etc)
        ((consp item)
         (push item result)
         (incf pos 4))
        ;; Regular byte
        (t
         (push item result)
         (incf pos 1))))
    (reverse result)))

(defun count-actual-bytes-link (items)
  "Count actual bytes in a list, excluding markers.
   Markers are conses like (:extern-call ...), (:call-fn ...), etc."
  (let ((count 0))
    (dolist (item items count)
      (unless (consp item)
        (incf count)))))

(defun resolve-extern-calls-link (code stub-map code-base)
  "Resolve :extern-call markers to BL instructions for linking.
   CODE: list of bytes and :extern-call markers
   STUB-MAP: hash-table mapping import name to stub address
   CODE-BASE: base address of code (code-offset + wrapper-size)
   Returns bytecode with BL instructions replacing markers."
  (let ((result nil)
        (pos 0))
    (dolist (item code)
      (cond
        ;; Extern call marker - resolve to BL
        ((and (consp item) (eq (car item) :extern-call))
         (let* ((name (cadr item))
                (bl-addr (+ code-base pos))
                (stub-addr (gethash name stub-map))
                (rel-offset (- stub-addr bl-addr))
                (off-instr (ash rel-offset -2))
                (off-masked (logand off-instr #x3FFFFFF))
                (bl-instr (logior #x94000000 off-masked)))
           ;; Emit BL in little-endian
           (push (logand bl-instr #xFF) result)
           (push (logand (ash bl-instr -8) #xFF) result)
           (push (logand (ash bl-instr -16) #xFF) result)
           (push (logand (ash bl-instr -24) #xFF) result)
           (incf pos 4)))
        ;; Regular byte
        (t
         (push item result)
         (incf pos 1))))
    (reverse result)))

;;; ============================================================
;;; Part 8d: GC Runtime FASL Generation
;;; ============================================================

(defun flatten-gc-code (code pos)
  "Flatten GC code, tracking :fn-label positions and internal calls.
   Returns (flat-bytes fn-labels internal-calls) where:
   - fn-labels is alist of (name . pos)
   - internal-calls is list of (type name offset)"
  (let ((flat nil)
        (labels nil)
        (internal-calls nil)
        (current-pos pos))
    (dolist (item code)
      (cond
        ;; Function label marker - record position
        ((and (consp item) (eq (car item) :fn-label))
         (push (cons (cadr item) current-pos) labels))
        ;; Internal call marker: (:call-fn name)
        ((and (consp item) (eq (car item) :call-fn))
         (push (list :call (cadr item) current-pos) internal-calls)
         ;; Emit NOP placeholder (0xD503201F in little-endian)
         (push #x1F flat)
         (push #x20 flat)
         (push #x03 flat)
         (push #xD5 flat)
         (incf current-pos 4))
        ;; Internal tail-call marker: (:tail-call-fn name)
        ((and (consp item) (eq (car item) :tail-call-fn))
         (push (list :tail-call (cadr item) current-pos) internal-calls)
         ;; Emit NOP placeholder
         (push #x1F flat)
         (push #x20 flat)
         (push #x03 flat)
         (push #xD5 flat)
         (incf current-pos 4))
        ;; Label marker - skip (handled separately)
        ((and (consp item) (eq (car item) :label))
         nil)
        ;; Byte - add to output
        ((integerp item)
         (push item flat)
         (incf current-pos))
        ;; List of bytes
        ((and (consp item) (integerp (car item)))
         (dolist (b item)
           (push b flat)
           (incf current-pos)))
        ;; Unknown marker type - indicates compiler bug
        (t (error "flatten-gc-code: Unknown marker type ~S" item))))
    (list (reverse flat) (reverse labels) (reverse internal-calls))))

(defun generate-gc-fasl (output-path)
  "Generate FASL containing GC runtime machine code.
   The GC runtime includes GC-COPY and GC-COLLECT functions."
  (let* ((gc-code (gc-runtime-code))
         (result (flatten-gc-code gc-code 0))
         (flat-bytes (first result))
         (fn-labels (second result))
         (gc-internal-calls (third result)))
    ;; Note: GC internal calls will be resolved when linking
    ;; For standalone GC FASL, write with internal calls embedded
    (declare (ignore gc-internal-calls))
    (write-fasl-v2 flat-bytes fn-labels output-path)
    (format t "Generated GC runtime FASL: ~A (~A bytes, ~A functions)~%"
            output-path (length flat-bytes) (length fn-labels))
    output-path))

;;; ============================================================
;;; Part 8d: FASL Linker - Combine Multiple Compilation Units
;;; ============================================================
;;;
;;; Architecture:
;;;   1. Read all FASLs (user code + GC runtime)
;;;   2. Concatenate bytecode, adjusting symbol offsets
;;;   3. Resolve all :call-fn markers to BL instructions
;;;   4. Generate Mach-O executable with heap
;;;
;;; Future: DWARF debug info will be generated here (see habu-49n)
;;;

(defun link-fasls (fasl-paths output-path &key verbose (include-gc t))
  "Link multiple FASL files into a single executable.
   Usage: (link-fasls '(\"util.fasl\" \"main.fasl\") \"myprogram\")

   Linking process:
   1. Read all FASL files and concatenate bytecode
   2. If include-gc, append GC runtime code with function labels
   3. Resolve internal calls (Lisp function calls across files)
   4. Prepend branch to MAIN entry point
   5. Resolve extern calls (C library calls)
   6. Wrap with heap initialization and write Mach-O

   Future: DWARF debug info generation will be added here (see habu-49n)"
  (let ((all-code nil)
        (global-symtab nil)
        (all-imports nil)
        (all-internal-calls nil)
        (current-offset 0))
    ;; Phase 1: Read all FASL files and build global symbol table
    (dolist (fasl-path fasl-paths)
      (let* ((fasl-data (read-fasl-v2 fasl-path))
             (bytecode (first fasl-data))
             (symtab (second fasl-data))
             (imports (third fasl-data))
             (internal-calls (fourth fasl-data)))
        (when verbose
          (format t "Read ~A: ~A bytes, ~A symbols, ~A imports, ~A internal-calls~%"
                  fasl-path (length bytecode)
                  (if symtab (length symtab) 0)
                  (if imports (length imports) 0)
                  (if internal-calls (length internal-calls) 0)))
        ;; Append code
        (setf all-code (append all-code bytecode))
        ;; Adjust symbol offsets and add to global table
        (when symtab
          (dolist (entry symtab)
            (let* ((name (car entry))
                   (offset (cdr entry))
                   (adjusted-offset (+ current-offset offset)))
              (push (cons name adjusted-offset) global-symtab))))
        ;; Adjust import offsets and collect
        (when imports
          (dolist (entry imports)
            (let* ((name (car entry))
                   (offset (cdr entry))
                   (adjusted-offset (+ current-offset offset)))
              (push (cons name adjusted-offset) all-imports))))
        ;; Adjust internal call offsets and collect
        (when internal-calls
          (dolist (entry internal-calls)
            (let* ((call-type (first entry))
                   (name (second entry))
                   (offset (third entry))
                   (adjusted-offset (+ current-offset offset)))
              (push (list call-type name adjusted-offset) all-internal-calls))))
        ;; Update offset for next FASL
        (setf current-offset (+ current-offset (length bytecode)))))

    ;; Phase 2: Append GC runtime if requested
    (when include-gc
      (let* ((gc-code (gc-runtime-code))
             (result (flatten-gc-code gc-code current-offset))
             (gc-flat (first result))
             (gc-labels (second result))
             (gc-internal-calls (third result)))
        (when verbose
          (format t "GC runtime: ~A bytes, functions: ~A, internal-calls: ~A~%"
                  (length gc-flat) (mapcar #'car gc-labels) (length gc-internal-calls)))
        ;; Append GC bytecode
        (setf all-code (append all-code gc-flat))
        ;; Add GC function labels to symbol table
        (dolist (label gc-labels)
          (push label global-symtab))
        ;; Add GC internal calls to be resolved (already have correct offsets)
        (dolist (call gc-internal-calls)
          (push call all-internal-calls))
        (setf current-offset (+ current-offset (length gc-flat)))))

    ;; Reverse to maintain order
    (setf global-symtab (reverse global-symtab))
    (setf all-imports (reverse all-imports))
    (setf all-internal-calls (reverse all-internal-calls))
    (when verbose
      (format t "Total code: ~A bytes~%" (length all-code))
      (format t "Global symbols: ~A~%" (length global-symtab))
      (format t "Total imports: ~A~%" (length all-imports))
      (format t "Internal calls to resolve: ~A~%" (length all-internal-calls)))

    ;; Phase 3: Resolve internal function calls
    ;; Patch bytecode at specified offsets with BL/B instructions
    (when all-internal-calls
      (let ((code-vec (coerce all-code 'vector)))
        (dolist (call all-internal-calls)
          (let* ((call-type (first call))
                 (fn-name (second call))
                 (call-offset (third call))
                 (fn-entry (assoc fn-name global-symtab)))
            (unless fn-entry
              (error "Link error: undefined function ~A at offset ~A" fn-name call-offset))
            (let* ((fn-pos (cdr fn-entry))
                   (rel-offset (- fn-pos call-offset))
                   ;; Generate BL or B instruction
                   (instr-bytes (if (eq call-type :call)
                                    (arm64:bl (ash rel-offset -2))
                                    (arm64:b (ash rel-offset -2)))))
              ;; Patch 4 bytes at call-offset
              (setf (aref code-vec call-offset) (nth 0 instr-bytes))
              (setf (aref code-vec (+ call-offset 1)) (nth 1 instr-bytes))
              (setf (aref code-vec (+ call-offset 2)) (nth 2 instr-bytes))
              (setf (aref code-vec (+ call-offset 3)) (nth 3 instr-bytes)))))
        (setf all-code (coerce code-vec 'list))))
    (when verbose
      (format t "Resolved ~A internal calls~%" (length all-internal-calls)))

    ;; Phase 4: Find MAIN and prepend branch to it
    ;; (After call resolution since we need correct offsets)
    (let ((main-entry (assoc 'habu::main global-symtab)))
      (when (and verbose main-entry)
        (format t "Entry point MAIN at offset ~A~%" (cdr main-entry)))
      (when main-entry
        (let* ((main-offset (cdr main-entry))
               (branch-offset (ash (+ main-offset 4) -2))
               (b-bytes (arm64:b branch-offset)))
          (setf all-code (append b-bytes all-code))
          ;; Adjust all symbol offsets by 4
          (setf global-symtab (mapcar (lambda (entry)
                                        (cons (car entry) (+ 4 (cdr entry))))
                                      global-symtab))
          ;; Adjust all import offsets by 4
          (setf all-imports (mapcar (lambda (entry)
                                      (cons (car entry) (+ 4 (cdr entry))))
                                    all-imports))
          (when verbose
            (format t "Prepended branch to MAIN, code now ~A bytes~%" (length all-code))))))

    ;; Phase 5: Resolve extern calls and calculate layout
    (let* ((import-names (remove-duplicates (mapcar #'car all-imports) :test #'string=))
           (import-names (if (null import-names) '("_exit") import-names))
           (wrapper-size +heap-wrapper-size+)  ; from macho.lisp - single source of truth
           (num-imports (length import-names))
           (stub-size 12)
           (stubs-total (* num-imports stub-size))
           (code-offset #x400)
           (code-size (length all-code))
           (stubs-offset (+ code-offset wrapper-size code-size)))
      (when verbose
        (format t "Unique imports: ~A~%" import-names)
        (format t "Code size: ~A bytes~%" code-size))

      ;; Build stub offset map
      (let ((stub-map (let ((ht (make-hash-table :test 'equal)))
                        (loop for name in import-names
                              for i from 0
                              do (setf (gethash name ht) (+ stubs-offset (* i stub-size))))
                        ht)))
        ;; Patch extern calls in bytecode
        (when all-imports
          (let ((code-vec (coerce all-code 'vector))
                (code-base (+ code-offset wrapper-size)))
            (dolist (import-entry all-imports)
              (let* ((name (car import-entry))
                     (call-offset (cdr import-entry))
                     (bl-addr (+ code-base call-offset))
                     (stub-addr (gethash name stub-map)))
                (unless stub-addr
                  (error "Link error: unknown import ~A" name))
                (let* ((rel-offset (- stub-addr bl-addr))
                       (off-instr (ash rel-offset -2))
                       (off-masked (logand off-instr #x3FFFFFF))
                       (bl-instr (logior #x94000000 off-masked)))
                  ;; Patch 4 bytes at call-offset (little-endian)
                  (setf (aref code-vec call-offset) (logand bl-instr #xFF))
                  (setf (aref code-vec (+ call-offset 1)) (logand (ash bl-instr -8) #xFF))
                  (setf (aref code-vec (+ call-offset 2)) (logand (ash bl-instr -16) #xFF))
                  (setf (aref code-vec (+ call-offset 3)) (logand (ash bl-instr -24) #xFF)))))
            (setf all-code (coerce code-vec 'list))))
        (when verbose
          (format t "Resolved ~A extern calls~%" (length all-imports)))

        ;; Calculate heap page offset
        (let* ((total-code-size (+ code-size wrapper-size))
               (stubs-end (+ code-offset total-code-size stubs-total))
               (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
               (text-pages-4kb (/ text-vmsize #x1000))
               (data-const-pages-4kb (/ #x4000 #x1000))
               (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
               (wrapped-code (wrap-bytecode-with-heap-for-imports all-code heap-page-offset)))
          (when verbose
            (format t "Wrapped code: ~A bytes (wrapper: ~A + code: ~A)~%"
                    (length wrapped-code) wrapper-size code-size))

          ;; Phase 6: Write Mach-O executable
          ;; TODO (habu-49n): Generate DWARF debug info here before writing executable
          ;; DWARF sections would include:
          ;;   - .debug_info: Compilation unit and type info
          ;;   - .debug_line: Source line to address mapping
          ;;   - .debug_abbrev: Abbreviation tables
          ;;   - .debug_str: String table
          ;; Source locations need to be tracked through compilation (habu-ksj)
          (write-macho-executable-with-imports-and-heap
           output-path wrapped-code import-names #x8000000 global-symtab))))
    (when verbose
      (format t "Created: ~A~%" output-path))
    output-path))

