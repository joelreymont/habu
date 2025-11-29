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
  (:export
   ;; String primitives (for runtime)
   #:string-length #:string-ref #:make-string-from-vector
   #:string-concat #:number-to-string
   ;; Vector primitives
   #:make-vector #:vector-set))

;;; HABU: Public compiler API
;;; Use habu:deliver, habu:compile-program, etc.
(defpackage :habu
  (:use :cl :sys)
  (:export
   ;; Public compiler API (clean names)
   #:read-all           ; Parse source string to forms
   #:compile-program    ; Compile forms to ARM64 bytecode
   #:deliver            ; Compile source to standalone executable
   #:deliver-file       ; Compile file to standalone executable
   ;; libSystem delivery
   #:deliver-with-libsystem
   ;; Disassembler
   #:disassemble-form    ; Disassemble a form to IR and bytecode
   #:disassemble-bytecode ; Disassemble bytecode to ARM64 mnemonics
   ;; Optimizer
   #:optimize-ir
   ;; Legacy nc-* aliases (for test compatibility - will be removed)
   #:nc-read-all #:nc-compile #:nc-compile-program
   #:nc-eval-ir #:nc-eval-forms #:nc-codegen #:nc-codegen-main
   #:nc-eval-ir-with-fns #:nc-compile-forms
   #:nc-deliver #:nc-deliver-file
   ;; Re-export system primitives for convenience
   #:string-length #:string-ref #:make-string-from-vector
   #:make-vector #:vector-set
   #:string-concat #:number-to-string))

(in-package :sys)

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
(declaim (ftype (function (list) list) nc-append-all))
(declaim (ftype (function (integer) integer) nc-temp-slot))
(declaim (ftype (function (list) integer) nc-code-size))

(defun nc-intern-symbol (name)
  "Get or create a symbol ID for NAME. Returns tagged symbol value."
  (let ((entry (assoc name *symbol-table* :test #'equal)))
    (if entry
        (cdr entry)
        (let ((id *symbol-counter*))
          (push (cons name id) *symbol-table*)
          (incf *symbol-counter*)
          id))))

(defun nc-reset-symbol-table ()
  "Reset symbol table for new compilation."
  (setf *symbol-table* nil)
  (setf *symbol-counter* 1))

;;; ============================================================
;;; Part 1: ARM64 Instruction Encoders (nc-asm-*)
;;; ============================================================

;; All encoder functions use let* to avoid nested calls in arg position
(defun nc-encode-word (word)
  (let* ((b0 (logand word #xFF))
         (s1 (ash word -8))
         (b1 (logand s1 #xFF))
         (s2 (ash word -16))
         (b2 (logand s2 #xFF))
         (s3 (ash word -24))
         (b3 (logand s3 #xFF)))
    (list b0 b1 b2 b3)))

(defun nc-movz (rd imm)
  (let* ((masked (logand imm #xFFFF))
         (shifted (ash masked 5))
         (ored (logior #xD2800000 shifted))
         (word (logior ored rd)))
    (nc-encode-word word)))

(defun nc-add-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x8B000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sub-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #xCB000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-mul-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9B007C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sdiv-reg (rd rn rm)
  ;; SDIV Xd, Xn, Xm - Signed divide
  ;; Encoding: 1001 1010 110 Rm 00001 0 Rn Rd
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9AC00C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-lsl-imm (rd rn shift)
  (let* ((s1 (- #x40 shift))
         (immr (logand s1 #x3F))
         (imms (- #x3F shift))
         (immr-shift (ash immr 16))
         (imms-shift (ash imms 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xD3400000 immr-shift))
         (or2 (logior or1 imms-shift))
         (or3 (logior or2 rn-shift))
         (word (logior or3 rd)))
    (nc-encode-word word)))

(defun nc-lsr-imm (rd rn shift)
  (let* ((shift-s (ash shift 16))
         (rn-s (ash rn 5))
         (or1 (logior #xD340FC00 shift-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-asr-imm (rd rn shift)
  "ASR Xd, Xn, #shift - arithmetic shift right immediate"
  ;; SBFM Xd, Xn, #shift, #63 -> sf=1 opc=00 N=1 immr=shift imms=63
  ;; Base encoding: #x93400000 | (immr << 16) | (imms << 10) | (rn << 5) | rd
  ;; With imms=63(#x3F): #x9340FC00 already has imms baked in
  (let* ((immr (logand shift #x3F))
         (immr-s (ash immr 16))
         (rn-s (ash rn 5))
         (word (logior #x9340FC00 immr-s rn-s rd)))
    (nc-encode-word word)))

(defun nc-mov-reg (rd rm)
  (let* ((rm-s (ash rm 16))
         (or1 (logior #xAA0003E0 rm-s))
         (word (logior or1 rd)))
    (nc-encode-word word)))

(defun nc-and-reg (rd rn rm)
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #x8A000000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-and-imm (rd rn n immr imms)
  "AND Xd, Xn, #imm - bitmask immediate
   For clearing low 4 bits: N=1, immr=0, imms=#x3B (59 = 64-4-1)"
  ;; sf=1 opc=00 100100 N immr imms Rn Rd
  ;; Base: #x92000000
  (let* ((n-s (ash n 22))
         (immr-s (ash immr 16))
         (imms-s (ash imms 10))
         (rn-s (ash rn 5))
         (word (logior #x92000000 n-s immr-s imms-s rn-s rd)))
    (nc-encode-word word)))

(defun nc-orr-reg (rd rn rm)
  "ORR Xd, Xn, Xm - bitwise OR"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #xAA000000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-eor-reg (rd rn rm)
  "EOR Xd, Xn, Xm - bitwise XOR"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #xCA000000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-bic-reg (rd rn rm)
  "BIC Xd, Xn, Xm - bit clear (Xd = Xn AND NOT Xm)"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #x8A200000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-lslv-reg (rd rn rm)
  "LSLV Xd, Xn, Xm - logical shift left variable"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #x9AC02000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-asrv-reg (rd rn rm)
  "ASRV Xd, Xn, Xm - arithmetic shift right variable"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #x9AC02800 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-neg-reg (rd rm)
  "NEG Xd, Xm - negate (SUB Xd, XZR, Xm)"
  (let* ((rm-s (ash rm 16))
         (rn-s (ash 31 5))
         (or1 (logior #xCB000000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-ldr-offset (rt rn offset)
  (let* ((off-s (ash offset -3))
         (off-ss (ash off-s 10))
         (rn-s (ash rn 5))
         (or1 (logior #xF9400000 off-ss))
         (or2 (logior or1 rn-s))
         (word (logior or2 rt)))
    (nc-encode-word word)))

(defun nc-str-offset (rt rn offset)
  (let* ((off-s (ash offset -3))
         (off-ss (ash off-s 10))
         (rn-s (ash rn 5))
         (or1 (logior #xF9000000 off-ss))
         (or2 (logior or1 rn-s))
         (word (logior or2 rt)))
    (nc-encode-word word)))

(defun nc-ldrb-reg (rt rn rm)
  "LDRB Wt, [Xn, Xm] - load byte from address Xn+Xm, zero-extend to 64-bit"
  ;; Encoding: 00 111 0 00 01 1 Rm 011 0 10 Rn Rt
  ;; #x38606800 = base + shifted register mode
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (word (logior #x38606800 rm-s rn-s rt)))
    (nc-encode-word word)))

(defun nc-ldrb-offset (rt rn offset)
  "LDRB Wt, [Xn, #offset] - load byte from address Xn+offset, zero-extend to 64-bit"
  ;; Encoding: 00 111 0 01 01 imm12 Rn Rt
  ;; Base: #x39400000
  (let* ((imm-s (ash (logand offset #xFFF) 10))
         (rn-s (ash rn 5))
         (word (logior #x39400000 imm-s rn-s rt)))
    (nc-encode-word word)))

(defun nc-strb-imm (rt rn offset)
  "STRB Wt, [Xn, #offset] - store byte to address Xn+offset"
  ;; Encoding: 00 111 0 01 00 imm12 Rn Rt
  ;; Base: #x39000000
  (let* ((imm-s (ash (logand offset #xFFF) 10))
         (rn-s (ash rn 5))
         (word (logior #x39000000 imm-s rn-s rt)))
    (nc-encode-word word)))

(defun nc-strb-reg (rt rn rm)
  "STRB Wt, [Xn, Xm] - store byte to address Xn+Xm"
  ;; Encoding: 00 111 0 00 00 1 Rm 011 0 10 Rn Rt
  ;; #x38206800 = base + shifted register mode
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (word (logior #x38206800 rm-s rn-s rt)))
    (nc-encode-word word)))

(defun nc-add-imm (rd rn imm)
  (let* ((imm-m (logand imm #xFFF))
         (imm-s (ash imm-m 10))
         (rn-s (ash rn 5))
         (or1 (logior #x91000000 imm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sub-imm (rd rn imm)
  (let* ((imm-m (logand imm #xFFF))
         (imm-s (ash imm-m 10))
         (rn-s (ash rn 5))
         (or1 (logior #xD1000000 imm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-stp-offset (rt1 rt2 rn imm)
  (let* ((imm-s (ash imm -3))
         (imm-m (logand imm-s #x7F))
         (imm-ss (ash imm-m 15))
         (rt2-s (ash rt2 10))
         (rn-s (ash rn 5))
         (or1 (logior #xA9000000 imm-ss))
         (or2 (logior or1 rt2-s))
         (or3 (logior or2 rn-s))
         (word (logior or3 rt1)))
    (nc-encode-word word)))

(defun nc-ldp-offset (rt1 rt2 rn imm)
  (let* ((imm-s (ash imm -3))
         (imm-m (logand imm-s #x7F))
         (imm-ss (ash imm-m 15))
         (rt2-s (ash rt2 10))
         (rn-s (ash rn 5))
         (or1 (logior #xA9400000 imm-ss))
         (or2 (logior or1 rt2-s))
         (or3 (logior or2 rn-s))
         (word (logior or3 rt1)))
    (nc-encode-word word)))

(defun nc-adr (rd offset)
  "ADR Xd, label - PC-relative address (+-1MB range).
   OFFSET is in bytes, signed 21-bit range."
  ;; ADR encoding: 0 immlo[1:0] 10000 immhi[18:0] Rd[4:0]
  ;; offset = (immhi << 2) | immlo
  (let* ((immlo (logand offset #x3))
         (immhi (logand (ash offset -2) #x7FFFF))
         (word (logior #x10000000
                       (ash immlo 29)
                       (ash immhi 5)
                       rd)))
    (nc-encode-word word)))

(defun nc-cmp-reg (rn rm)
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #xEB00001F rm-s))
         (word (logior or1 rn-s)))
    (nc-encode-word word)))

(defun nc-cmp-imm (rn imm)
  "CMP Xn, #imm - compare with 12-bit immediate"
  (let* ((imm-s (ash (logand imm #xFFF) 10))
         (rn-s (ash rn 5))
         (or1 (logior #xF100001F imm-s))
         (word (logior or1 rn-s)))
    (nc-encode-word word)))

(defun nc-cset (rd cond-code)
  (let* ((cc-x (logxor cond-code 1))
         (cc-s (ash cc-x 12))
         (or1 (logior #x9A9F07E0 cc-s))
         (word (logior or1 rd)))
    (nc-encode-word word)))

(defun nc-b-offset (offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x3FFFFFF))
         (word (logior #x14000000 off-m)))
    (nc-encode-word word)))

(defun nc-bl-offset (offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x3FFFFFF))
         (word (logior #x94000000 off-m)))
    (nc-encode-word word)))

(defun nc-b-cond (cond-code offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x7FFFF))
         (off-ss (ash off-m 5))
         (or1 (logior #x54000000 off-ss))
         (word (logior or1 cond-code)))
    (nc-encode-word word)))

(defun nc-ret ()
  (nc-encode-word #xD65F03C0))

(defun nc-cbz (rt offset)
  "CBZ Xt, offset - compare and branch if zero. OFFSET is in bytes."
  ;; CBZ: 1 0 110100 imm19 Rt (for 64-bit)
  ;; imm19 = offset / 4, signed
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (word (logior #xB4000000 (ash imm19 5) rt)))
    (nc-encode-word word)))

(defun nc-cbnz (rt offset)
  "CBNZ Xt, offset - compare and branch if not zero. OFFSET is in bytes."
  ;; CBNZ: 1 0 110101 imm19 Rt (for 64-bit)
  ;; imm19 = offset / 4, signed
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (word (logior #xB5000000 (ash imm19 5) rt)))
    (nc-encode-word word)))

;;; Position tracking helpers for function linking
(defun nc-emit-with-pos (code)
  "Emit code and update position counter. Returns the code."
  (let ((len (length code)))
    (incf *codegen-pos* len)
    code))

(defun nc-record-call-fixup (fn-name)
  "Record that a BL instruction at current position needs fixup for fn-name."
  (push (cons *codegen-pos* fn-name) *call-fixups*))

(defun nc-patch-bl-at (code pos rel-offset)
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

(defun nc-apply-fixups (code fnoffs)
  "Apply all recorded call fixups to code."
  (dolist (fixup *call-fixups*)
    (let* ((bl-pos (car fixup))
           (fn-name (cdr fixup))
           (fn-entry (assoc fn-name fnoffs)))
      (when fn-entry
        (let* ((fn-pos (cdr fn-entry))
               (rel-offset (- fn-pos bl-pos)))
          (nc-patch-bl-at code bl-pos rel-offset)))))
  code)

(defun nc-resolve-calls (code fnoffs)
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
                             (b-bytes (nc-b-offset rel-offset)))
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
                             (bl-bytes (nc-bl-offset rel-offset)))
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
                             (b-bytes (nc-b-offset rel-offset)))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-start-stack)))
                     ;; External call - leave marker with position info
                     ((and (consp item) (eq (car item) :extern-call))
                      (resolve-at (cdr items)
                                  (+ pos 4)
                                  (cons (list :extern-call (cadr item) pos) acc)
                                  loop-start-stack))
                     ;; Regular byte
                     (t
                      (resolve-at (cdr items)
                                  (+ pos 1)
                                  (cons item acc)
                                  loop-start-stack)))))))
    (resolve-at code 0 nil nil)))

(defun nc-collect-extern-calls (code)
  "Collect all extern call markers from code.
   Returns list of (name . position) pairs."
  (let ((calls nil))
    (dolist (item code)
      (when (and (consp item) (eq (car item) :extern-call))
        (push (cons (cadr item) (caddr item)) calls)))
    (nreverse calls)))

(defun nc-get-unique-imports (extern-calls)
  "Get unique import names from extern calls list."
  (let ((names nil))
    (dolist (call extern-calls)
      (let ((name (car call)))
        (unless (member name names :test #'equal)
          (push name names))))
    (nreverse names)))

(defun nc-flatten-extern-calls (code &optional stub-map code-base-addr)
  "Replace extern call markers with BL instructions.
   If STUB-MAP and CODE-BASE-ADDR are provided, emits correct BL instructions.
   Otherwise emits placeholder BL instructions (for post-processing).
   Returns (values flattened-code extern-call-positions)
   where extern-call-positions is ((name . byte-pos) ...)"
  (let ((result nil)
        (positions nil))
    (dolist (item code)
      (if (and (consp item) (eq (car item) :extern-call))
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
                  (push #x94 result))))  ; BL opcode high byte
          (push item result)))
    (values (nreverse result) (nreverse positions))))

(defun nc-movk (rd imm shift)
  (let* ((shift-s (ash shift -4))
         (shift-ss (ash shift-s 21))
         (imm-m (logand imm #xFFFF))
         (imm-s (ash imm-m 5))
         (or1 (logior #xF2800000 shift-ss))
         (or2 (logior or1 imm-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-blr (rn)
  (let* ((rn-s (ash rn 5))
         (word (logior #xD63F0000 rn-s)))
    (nc-encode-word word)))

(defun nc-load-addr (rd addr)
  (let* ((lo16 (logand addr #xFFFF))
         (sh16 (ash addr -16))
         (hi16 (logand sh16 #xFFFF))
         (sh32 (ash addr -32))
         (hi32 (logand sh32 #xFFFF))
         (sh48 (ash addr -48))
         (hi48 (logand sh48 #xFFFF))
         (base (nc-movz rd lo16))
         (p1 (if (> hi16 0) (nc-movk rd hi16 16) nil))
         (r1 (append base p1))
         (p2 (if (> hi32 0) (nc-movk rd hi32 32) nil))
         (r2 (append r1 p2))
         (p3 (if (> hi48 0) (nc-movk rd hi48 48) nil)))
    (append r2 p3)))

(defun nc-load-addr-32 (rd addr)
  "Load a 32-bit address into register rd using exactly 8 bytes (MOVZ + MOVK).
   This is used for function offsets to ensure consistent code size during
   the two-pass compilation where fnoffs may be nil in the first pass."
  (let* ((lo16 (logand addr #xFFFF))
         (hi16 (logand (ash addr -16) #xFFFF)))
    (append (nc-movz rd lo16)
            (nc-movk rd hi16 16))))

(defun nc-cond-eq () 0)
(defun nc-cond-ne () 1)
(defun nc-cond-lt () 11)
(defun nc-cond-le () 13)
(defun nc-cond-gt () 12)
(defun nc-cond-ge () 10)

(defun nc-string-to-char-codes (str)
  "Convert string to list of character codes"
  (labels ((iter (i acc)
             (if (>= i (length str))
                 (reverse acc)
                 (iter (+ i 1) (cons (char-code (char str i)) acc)))))
    (iter 0 nil)))

(defun nc-codegen-string-from-chars (chars td)
  "Generate code to build a string from character codes.
   Returns code that leaves the string in x0."
  (let* ((len (length chars))
         (tagged-len (ash len 4))  ; Tag length as fixnum
         (vec-slot (nc-temp-slot td))
         ;; Allocate vector: movz x0, tagged-len; ldr x11, [x19, #56]; blr x11
         ;; Runtime table index 7 = make_vector at offset 56
         (alloc (nc-append-all
                 (list (if (< tagged-len #x10000)
                           (nc-movz 0 tagged-len)
                           (nc-load-addr 0 tagged-len))
                       (nc-ldr-offset 11 19 56)
                       (nc-blr 11)
                       (nc-str-offset 0 31 vec-slot)))))
    ;; Store each character: ldr x0, [sp, vec-slot]; movz x1, tagged-idx; movz x2, tagged-ch; ldr x11, [x19, #64]; blr x11
    ;; Runtime table index 8 = vector_set at offset 64
    (labels ((store-chars (chs idx acc)
               (if (null chs)
                   acc
                   (let* ((ch (car chs))
                          (tagged-idx (ash idx 4))    ; Tag index as fixnum
                          (tagged-ch (ash ch 4))      ; Tag character as fixnum
                          (store-code (nc-append-all
                                       (list (nc-ldr-offset 0 31 vec-slot)
                                             (if (< tagged-idx #x10000)
                                                 (nc-movz 1 tagged-idx)
                                                 (nc-load-addr 1 tagged-idx))
                                             (if (< tagged-ch #x10000)
                                                 (nc-movz 2 tagged-ch)
                                                 (nc-load-addr 2 tagged-ch))
                                             (nc-ldr-offset 11 19 64)
                                             (nc-blr 11)))))
                     (store-chars (cdr chs) (+ idx 1) (append acc store-code))))))
      (let* ((stores (store-chars chars 0 nil))
             ;; Make string from vector: ldr x0, [sp, vec-slot]; ldr x9, [x19, #80]; blr x9
             ;; Runtime table index 10 = make_string_from_vector at offset 80
             (make-str (nc-append-all
                        (list (nc-ldr-offset 0 31 vec-slot)
                              (nc-ldr-offset 9 19 80)
                              (nc-blr 9)))))
        (nc-append-all (list alloc stores make-str))))))

(defun nc-codegen-string-inline (chars)
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
                          (code (nc-append-all
                                 (list (nc-movz 1 ch)
                                       (nc-strb-imm 1 28 offset)))))
                     (store-chars (cdr chs) (+ idx 1) (append acc code))))))
      (let ((store-code (store-chars chars 0 nil)))
        (nc-append-all
         (list
          ;; Store length at [x28+0]
          (nc-movz 1 len)
          (nc-str-offset 1 28 0)
          ;; Store each char
          store-code
          ;; Return tagged pointer, bump heap
          (nc-mov-reg 0 28)                   ; x0 = current heap ptr
          (nc-movz 1 alloc-size)
          (nc-add-reg 28 28 1)                ; x28 += alloc size
          ;; Tag with string tag (0x4)
          (nc-movz 1 4)
          (nc-orr-reg 0 0 1)))))))

;;; ============================================================
;;; Part 2: Utility Functions (nc-util-*)
;;; ============================================================

(defun nc-has-tag (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun nc-env-lookup (sym env)
  (if (null env)
      nil
      (if (eq (caar env) sym)
          (cdar env)
          (nc-env-lookup sym (cdr env)))))

(defun nc-env-extend (bindings env)
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

(defun nc-count-instrs (code)
  (if (null code) 0 (ash (length code) -2)))

;; Append two lists - bind first, then append
(defun nc-append2 (a b)
  (let ((ar a))
    (append ar b)))

;; Append list of lists using fold - avoiding nested calls
(defun nc-append-all (lists)
  (labels ((iter (ls acc)
             (if (null ls) acc
                 (let* ((hd (car ls))
                        (tl (cdr ls))
                        (na (append acc hd)))
                   (iter tl na)))))
    (iter lists nil)))

;;; ============================================================
;;; Part 3: Reader (nc-read-*)
;;; ============================================================

(defun nc-whitespace-p (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
(defun nc-digit-p (ch) (and (>= ch #x30) (<= ch #x39)))
(defun nc-hex-digit-p (ch) (or (nc-digit-p ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
(defun nc-alpha-p (ch) (or (and (>= ch #x41) (<= ch #x5A)) (and (>= ch #x61) (<= ch #x7A))))
(defun nc-symbol-char-p (ch)
  (or (nc-alpha-p ch) (nc-digit-p ch) (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E) (= ch #x21) (= ch #x3F)
      (= ch #x26) (= ch #x25) (= ch #x3A)))

(defun nc-char-at (s pos)
  (if (< pos (string-length s)) (string-ref s pos) 0))

(defun nc-digit-val (ch) (- ch #x30))
(defun nc-hex-val (ch)
  (cond ((nc-digit-p ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) 10))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) 10))
        (t 0)))

(defun nc-skip-line (s pos)
  (let ((ch (nc-char-at s pos)))
    (if (or (= ch #x0A) (= ch 0)) (+ pos 1) (nc-skip-line s (+ pos 1)))))

(defun nc-skip-ws (s pos)
  (let ((ch (nc-char-at s pos)))
    (cond ((nc-whitespace-p ch) (nc-skip-ws s (+ pos 1)))
          ((= ch #x3B) (nc-skip-ws s (nc-skip-line s (+ pos 1))))
          (t pos))))

(defun nc-read-digits (s pos n)
  (let ((ch (nc-char-at s pos)))
    (if (nc-digit-p ch)
        (nc-read-digits s (+ pos 1) (+ (* n 10) (nc-digit-val ch)))
        (cons n pos))))

(defun nc-read-int (s pos)
  (let ((neg nil) (start pos))
    (let ((ch (nc-char-at s pos)))
      (cond ((= ch #x2D) (setq neg t) (setq start (+ pos 1)))
            ((= ch #x2B) (setq start (+ pos 1)))))
    (let* ((r (nc-read-digits s start 0))
           (val (car r))
           (end (cdr r)))
      (cons (if neg (- 0 val) val) end))))

(defun nc-read-hex-digits (s pos n)
  (let ((ch (nc-char-at s pos)))
    (if (nc-hex-digit-p ch)
        (nc-read-hex-digits s (+ pos 1) (+ (* n 16) (nc-hex-val ch)))
        (cons n pos))))

(defun nc-read-hex (s pos)
  (nc-read-hex-digits s pos 0))

(defun nc-chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (dotimes (i len)
      (vector-set vec i (nth i chars)))
    (make-string-from-vector vec)))

(defun nc-read-sym-chars (s pos chars)
  (let ((ch (nc-char-at s pos)))
    (if (nc-symbol-char-p ch)
        (nc-read-sym-chars s (+ pos 1) (cons ch chars))
        (cons chars pos))))

(defun nc-read-sym (s pos)
  (let* ((r (nc-read-sym-chars s pos nil))
         (chars (car r))
         (end (cdr r))
         (name (nc-chars-to-string (reverse chars)))
         (uname (string-upcase name)))
    (cons (cond ((string= uname "NIL") nil)
                ((string= uname "T") t)
                ;; Intern into HABU package for correct symbol comparison
                (t (intern uname (find-package :habu))))
          end)))

(defun nc-read-str-chars (s pos chars)
  (let ((ch (nc-char-at s pos)))
    (cond
      ((= ch #x22) (cons chars (+ pos 1)))
      ((= ch #x5C)
       (let* ((esc (nc-char-at s (+ pos 1)))
              (ec (cond ((= esc #x6E) #x0A) ((= esc #x74) #x09) ((= esc #x72) #x0D) (t esc))))
         (nc-read-str-chars s (+ pos 2) (cons ec chars))))
      ((= ch 0) (cons chars pos))
      (t (nc-read-str-chars s (+ pos 1) (cons ch chars))))))

(defun nc-read-str (s pos)
  (let* ((r (nc-read-str-chars s (+ pos 1) nil))
         (chars (car r))
         (end (cdr r)))
    (cons (nc-chars-to-string (reverse chars)) end)))

(defun nc-read (source pos)
  (labels
      ((read-list-elems (p)
         (let* ((p2 (nc-skip-ws source p))
                (ch (nc-char-at source p2)))
           (cond
             ((= ch #x29) (cons nil (+ p2 1)))
             ((= ch #x2E)
              (let* ((r (read-one (+ p2 1)))
                     (cdr-val (car r))
                     (p3 (cdr r))
                     (p4 (nc-skip-ws source p3)))
                (cons cdr-val (+ p4 1))))
             ((= ch 0) (cons nil p2))
             (t (let* ((er (read-one p2))
                       (el (car er))
                       (p3 (cdr er))
                       (rr (read-list-elems p3)))
                  (cons (cons el (car rr)) (cdr rr)))))))
       (read-list (p) (read-list-elems (+ p 1)))
       (read-sharp (p)
         (let ((ch (nc-char-at source (+ p 1))))
           (cond
             ((or (= ch #x78) (= ch #x58)) (nc-read-hex source (+ p 2)))
             ((= ch #x27)
              (let ((r (read-one (+ p 2))))
                (cons (list 'function (car r)) (cdr r))))
             ((= ch #x5C)
              (let ((ch2 (nc-char-at source (+ p 2))))
                (if (nc-alpha-p (nc-char-at source (+ p 3)))
                    (let* ((r (nc-read-sym-chars source (+ p 2) nil))
                           (nm (nc-chars-to-string (reverse (car r)))))
                      (cons (cond ((string= nm "newline") #x0A) ((string= nm "space") #x20)
                                  ((string= nm "tab") #x09) (t ch2))
                            (cdr r)))
                    (cons ch2 (+ p 3)))))
             (t (cons nil (+ p 2))))))
       (read-one (p)
         (let* ((p2 (nc-skip-ws source p))
                (ch (nc-char-at source p2)))
           (if (>= p2 (string-length source))
               (cons nil p2)
               (cond
                 ((= ch #x22) (nc-read-str source p2))
                 ((= ch #x28) (read-list p2))
                 ((= ch #x27)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quote (car r)) (cdr r))))
                 ((= ch #x60)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quasiquote (car r)) (cdr r))))
                 ((= ch #x2C)
                  (if (= (nc-char-at source (+ p2 1)) #x40)
                      (let ((r (read-one (+ p2 2))))
                        (cons (list 'unquote-splicing (car r)) (cdr r)))
                      (let ((r (read-one (+ p2 1))))
                        (cons (list 'unquote (car r)) (cdr r)))))
                 ((= ch #x23) (read-sharp p2))
                 ((or (nc-digit-p ch)
                      (and (or (= ch #x2D) (= ch #x2B))
                           (nc-digit-p (nc-char-at source (+ p2 1)))))
                  (nc-read-int source p2))
                 ((nc-symbol-char-p ch) (nc-read-sym source p2))
                 ((= ch #x29) (cons nil (+ p2 1)))
                 (t (read-one (+ p2 1))))))))
    (read-one pos)))

(defun nc-read-from-string (source)
  (car (nc-read source 0)))

(defun nc-read-all (source)
  (let ((len (string-length source)))
    (labels ((ra (pos acc)
               (let ((p2 (nc-skip-ws source pos)))
                 (if (>= p2 len)
                     (reverse acc)
                     (let ((r (nc-read source p2)))
                       (ra (cdr r) (cons (car r) acc)))))))
      (ra 0 nil))))

;;; ============================================================
;;; Part 4: Stack Frame Constants (inlined for delivery)
;;; ============================================================

;; Constants inlined directly to avoid global variable initialization issues
;; Frame size: #xFF0, Env base: #x180, Temp base: #x40
;; Temp guard: #x180, Spill base: #x200

(defun nc-frame-size () #xFF0)
(defun nc-env-base () #x180)
(defun nc-temp-base () #x40)
(defun nc-temp-guard () #x180)
(defun nc-spill-base () #x200)

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

(defun nc-temp-register (depth)
  "Return register number for temp depth, or nil if must spill to stack."
  (if (< depth *num-temp-registers*)
      (nth depth *temp-registers*)
      nil))

(defun nc-temp-slot (depth)
  "Return stack offset for temp depth. Used when registers exhausted or across calls."
  (let ((off (+ #x40 (* depth 8))))  ; #x40 = temp base (64)
    (if (>= off #xF00)                ; #xF00 = temp guard (3840), allows 480 slots
        (error "Too many temp slots: ~A" depth)
        off)))

(defun nc-save-temp (depth)
  "Generate code to save x0 to temp location (register or stack)."
  (let ((reg (nc-temp-register depth)))
    (if reg
        (nc-mov-reg reg 0)            ; MOV xN, x0
        (nc-str-offset 0 31 (nc-temp-slot depth)))))  ; STR x0, [sp, #off]

(defun nc-load-temp (dest-reg depth)
  "Generate code to load temp location to dest-reg."
  (let ((reg (nc-temp-register depth)))
    (if reg
        (if (= dest-reg reg)
            nil                        ; Already in correct register
            (nc-mov-reg dest-reg reg)) ; MOV dest, xN
        (nc-ldr-offset dest-reg 31 (nc-temp-slot depth)))))

(defun nc-spill-slot (td idx)
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

(defun nc-prologue ()
  ;; Main entry prologue - x0 has runtime table pointer from C caller
  ;; Use 4KB frame to support deep nesting in large programs
  (append
   (nc-sub-imm 31 31 #x1000)   ; SUB sp, sp, #4096 (allocate stack frame)
   (nc-stp-offset 29 30 31 0)  ; STP x29, x30, [sp, #0]
   (nc-stp-offset 19 20 31 16) ; STP x19, x20, [sp, #16]
   (nc-stp-offset 21 22 31 32) ; STP x21, x22, [sp, #32]
   (nc-stp-offset 23 24 31 48) ; STP x23, x24, [sp, #48]
   (nc-mov-reg 19 0)           ; MOV x19, x0 (save runtime table)
   (nc-add-imm 20 31 #x180)))  ; ADD x20, sp, #384 (env-base)

(defun nc-epilogue ()
  (append
   (nc-ldp-offset 23 24 31 48) ; LDP x23, x24, [sp, #48]
   (nc-ldp-offset 21 22 31 32) ; LDP x21, x22, [sp, #32]
   (nc-ldp-offset 19 20 31 16) ; LDP x19, x20, [sp, #16]
   (nc-ldp-offset 29 30 31 0)  ; LDP x29, x30, [sp, #0]
   (nc-add-imm 31 31 #x1000)    ; ADD sp, sp, #4096 (deallocate stack)
   (nc-ret)))

;;; ============================================================
;;; Part 5b: Free Variable Analysis
;;; ============================================================

(defun nc-find-free-vars (expr bound env)
  "Find variables referenced in expr that are in env but not in bound"
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((symbolp e)
                ;; Check if it's a variable reference (in env but not bound)
                (if (and (nc-env-lookup e env)
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
;;; Part 6: IR Compiler (nc-compile-*)
;;; ============================================================

(defun nc-rewrite-labels-calls (expr fn-names)
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
          (cons 'funcall (cons op (mapcar (lambda (e) (nc-rewrite-labels-calls e fn-names)) (cdr expr)))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (nc-rewrite-labels-calls (caddr expr) fn-names)))
         ;; let/let* - rewrite values and body, not binding names
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (nc-rewrite-labels-calls (cadr b) fn-names))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (nc-rewrite-labels-calls e fn-names)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (nc-rewrite-labels-calls e fn-names)) expr)))))
    (t expr)))

(defun nc-rewrite-labels-body (expr fn-names fntab-var)
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
                          (mapcar (lambda (e) (nc-rewrite-labels-body e fn-names fntab-var)) (cdr expr))))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (nc-rewrite-labels-body (caddr expr) fn-names fntab-var)))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (nc-rewrite-labels-body (cadr b) fn-names fntab-var))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (nc-rewrite-labels-body e fn-names fntab-var)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (nc-rewrite-labels-body e fn-names fntab-var)) expr)))))
    (t expr)))

(defun nc-rewrite-labels-main (expr fn-names)
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
                          (mapcar (lambda (e) (nc-rewrite-labels-main e fn-names)) (cdr expr))))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; Lambda - rewrite body but don't rewrite param list
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (nc-rewrite-labels-main (caddr expr) fn-names)))
         ;; let/let* - rewrite values and body
         ((or (eq op 'LET) (eq op 'LET*) (eq op 'let) (eq op 'let*))
          (let* ((bindings (cadr expr))
                 (body (cddr expr))
                 (new-bindings (mapcar (lambda (b)
                                         (if (consp b)
                                             (list (car b) (nc-rewrite-labels-main (cadr b) fn-names))
                                             b))
                                       bindings)))
            (cons op (cons new-bindings (mapcar (lambda (e) (nc-rewrite-labels-main e fn-names)) body)))))
         ;; Default: recursively rewrite all parts
         (t (mapcar (lambda (e) (nc-rewrite-labels-main e fn-names)) expr)))))
    (t expr)))

(defun nc-quote-ir (obj)
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'nil-ir))  ;; Use nil-ir for proper nil, not (lit 0)
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (nc-quote-ir (car obj)) (nc-quote-ir (cdr obj))))
    (t (list 'lit 0))))

(defun nc-compile (expr env fenv)
  (cond
    ((numberp expr) (list 'lit expr))
    ((stringp expr) (list 'str-lit expr))
    ((symbolp expr)
     ;; Handle special symbols first
     (cond
       ;; t compiles to non-zero literal for native executables without runtime
       ;; In boolean context, any non-zero value is truthy
       ((eq expr 't) (list 'lit 1))           ; t = 1 (truthy)
       ((eq expr 'nil) (list 'lit 0))          ; nil is 0
       (t
        ;; Use numberp since offset 0 is falsey in Habu
        (let ((off (nc-env-lookup expr env)))
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
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (+ (car args) (cadr args)))
                            ;; Also fold if compiled results are both literals
                            (let ((left-ir (nc-compile (car args) env fenv))
                                  (right-ir (nc-compile (cadr args) env fenv)))
                              (if (and (nc-has-tag left-ir 'lit) (nc-has-tag right-ir 'lit))
                                  (list 'lit (+ (cadr left-ir) (cadr right-ir)))
                                  (list 'add left-ir right-ir))))
                        (nc-compile (cons '+ (cons (list '+ (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '-)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args))
                    ;; Constant folding for unary minus
                    (if (numberp (car args))
                        (list 'lit (- (car args)))
                        (let ((arg-ir (nc-compile (car args) env fenv)))
                          (if (nc-has-tag arg-ir 'lit)
                              (list 'lit (- (cadr arg-ir)))
                              (list 'sub (list 'lit 0) arg-ir))))
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (- (car args) (cadr args)))
                            (let ((left-ir (nc-compile (car args) env fenv))
                                  (right-ir (nc-compile (cadr args) env fenv)))
                              (if (and (nc-has-tag left-ir 'lit) (nc-has-tag right-ir 'lit))
                                  (list 'lit (- (cadr left-ir) (cadr right-ir)))
                                  (list 'sub left-ir right-ir))))
                        (nc-compile (cons '- (cons (list '- (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '*)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 1)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        ;; Constant folding: if both args are numbers, compute at compile time
                        (if (and (numberp (car args)) (numberp (cadr args)))
                            (list 'lit (* (car args) (cadr args)))
                            (let ((left-ir (nc-compile (car args) env fenv))
                                  (right-ir (nc-compile (cadr args) env fenv)))
                              (if (and (nc-has-tag left-ir 'lit) (nc-has-tag right-ir 'lit))
                                  (list 'lit (* (cadr left-ir) (cadr right-ir)))
                                  (list 'mul left-ir right-ir))))
                        (nc-compile (cons '* (cons (list '* (car args) (cadr args)) (cddr args))) env fenv))))))
         ;; division with constant folding
         ((eq op '/)
          (if (and (numberp (cadr expr)) (numberp (caddr expr)) (not (zerop (caddr expr))))
              (list 'lit (truncate (cadr expr) (caddr expr)))
              (list 'div (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv))))
         ;; modulo
         ((eq op 'mod)
          (list 'mod-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'rem)
          (list 'mod-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; 1+ and 1-
         ((eq op '1+)
          (list 'add (nc-compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op '1-)
          (list 'sub (nc-compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op 'logand)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit -1)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'band (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (list 'logand (list 'logand (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'logior)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bor (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (list 'logior (list 'logior (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'logxor)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'bxor (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (list 'logxor (list 'logxor (car args) (cadr args)) (caddr args)) env fenv))))))
         ((eq op 'ash)
          (list 'bsh (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '=)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'eq)
          ;; eq is pointer equality - same as = for our tagged values
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '<)
          (list 'cmp-lt (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '>)
          (list 'cmp-gt (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '<=)
          (list 'cmp-le (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '>=)
          (list 'cmp-ge (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'if)
          (list 'if-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)
                (if (cdddr expr) (nc-compile (cadddr expr) env fenv) (list 'lit 0))))
         ;; cond - multi-branch conditional
         ((eq op 'cond)
          (let ((clauses (cdr expr)))
            (if (null clauses)
                (list 'lit 0)
                (let* ((clause (car clauses))
                       (test (car clause))
                       (body (cdr clause)))
                  (if (eq test 't)
                      ;; t clause - always execute
                      (if (null body)
                          (list 'lit 1)
                          (if (null (cdr body))
                              (nc-compile (car body) env fenv)
                              (nc-compile (cons 'progn body) env fenv)))
                      (list 'if-ir
                            (nc-compile test env fenv)
                            (if (null body)
                                (nc-compile test env fenv)
                                (if (null (cdr body))
                                    (nc-compile (car body) env fenv)
                                    (nc-compile (cons 'progn body) env fenv)))
                            (nc-compile (cons 'cond (cdr clauses)) env fenv)))))))
         ;; when - if with implicit progn (no else branch)
         ((eq op 'when)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (nc-compile test env fenv)
                  (if (null (cdr body))
                      (nc-compile (car body) env fenv)
                      (nc-compile (cons 'progn body) env fenv))
                  (list 'lit 0))))
         ;; unless - negated when
         ((eq op 'unless)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (nc-compile test env fenv)
                  (list 'lit 0)
                  (if (null (cdr body))
                      (nc-compile (car body) env fenv)
                      (nc-compile (cons 'progn body) env fenv)))))
         ;; dotimes - counted iteration
         ((eq op 'dotimes)
          ;; (dotimes (var count [result]) body...)
          (let* ((spec (cadr expr))
                 (var (car spec))
                 (count-form (cadr spec))
                 (result-form (if (cddr spec) (caddr spec) 0))
                 (body (cddr expr))
                 ;; Compile body with extended env that includes loop var
                 (new-env (nc-env-extend (list (list var)) env))
                 (body-ir (if (null (cdr body))
                              (nc-compile (car body) new-env fenv)
                              (nc-compile (cons 'progn body) new-env fenv)))
                 (result-ir (nc-compile result-form new-env fenv)))
            ;; Create a dotimes-ir node with compiled body
            (list 'dotimes-ir
                  var
                  (nc-compile count-form env fenv)
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
                 (new-env (nc-env-extend (list (list var)) env))
                 (body-ir (if (null (cdr body))
                              (nc-compile (car body) new-env fenv)
                              (nc-compile (cons 'progn body) new-env fenv)))
                 (result-ir (if result-form
                                (nc-compile result-form new-env fenv)
                                (list 'lit 0))))
            ;; Create a dolist-ir node with compiled body
            (list 'dolist-ir
                  var
                  (nc-compile list-form env fenv)
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
                                  (vi (nc-compile vl env fenv))
                                  (ne (nc-env-extend (list (list nm)) eacc)))
                             (proc (cdr bs) ne (cons vi vals) (cons nm names)))))
                     ;; Avoid mapcar - use labels recursion instead
                     (get-offs (ns e acc)
                       (if (null ns)
                           (reverse acc)
                           (get-offs (cdr ns) e (cons (nc-env-lookup (car ns) e) acc)))))
              (let* ((r (proc bindings env nil nil))
                     (nenv (car r))
                     (vals (cadr r))
                     (names (caddr r))
                     (offs (get-offs names nenv nil))
                     ;; Wrap multiple body forms in progn
                     (body (if (null (cdr body-forms))
                               (car body-forms)
                               (cons 'progn body-forms)))
                     (bir (nc-compile body nenv fenv)))
                (list 'let-ir vals bir (length bindings) offs)))))
         ((eq op 'LET*)  ; Changed to uppercase
          (let* ((bs (cadr expr))
                 (body-forms (cddr expr))
                 (body (if (null (cdr body-forms))
                           (car body-forms)
                           (cons 'progn body-forms))))
            (if (null bs)
                (nc-compile body env fenv)
                (nc-compile (list 'LET (list (car bs)) (cons 'LET* (cons (cdr bs) body-forms))) env fenv))))
         ((eq op 'quote) (nc-quote-ir (cadr expr)))
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
                 (free-vars (nc-find-free-vars body params env))
                 ;; Get the offsets for each free var in current env
                 (free-offsets (mapcar (lambda (v) (nc-env-lookup v env)) free-vars))
                 ;; Build environment for body: params + free vars
                 ;; Free vars come first (as captured in closure env), then params
                 (param-bindings (mapcar #'list params))
                 (body-env (nc-env-extend param-bindings
                              (nc-env-extend (mapcar #'list free-vars) nil)))
                 ;; Compile body to IR
                 (body-ir (nc-compile body body-env fenv)))
            (list 'lambda-ir params body-ir free-vars free-offsets)))
         ((eq op 'cons)
          (list 'cons-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'car) (list 'car-ir (nc-compile (cadr expr) env fenv)))
         ((eq op 'cdr) (list 'cdr-ir (nc-compile (cadr expr) env fenv)))
         ;; cadr, caddr, cadddr, cddr, cdddr - common accessor chains
         ((eq op 'cadr) (nc-compile `(car (cdr ,(cadr expr))) env fenv))
         ((eq op 'caddr) (nc-compile `(car (cdr (cdr ,(cadr expr)))) env fenv))
         ((eq op 'cadddr) (nc-compile `(car (cdr (cdr (cdr ,(cadr expr))))) env fenv))
         ((eq op 'cddr) (nc-compile `(cdr (cdr ,(cadr expr))) env fenv))
         ((eq op 'cdddr) (nc-compile `(cdr (cdr (cdr ,(cadr expr)))) env fenv))
         ;; first, second, third, fourth - list accessors
         ((eq op 'first) (nc-compile `(car ,(cadr expr)) env fenv))
         ((eq op 'second) (nc-compile `(cadr ,(cadr expr)) env fenv))
         ((eq op 'third) (nc-compile `(caddr ,(cadr expr)) env fenv))
         ((eq op 'fourth) (nc-compile `(cadddr ,(cadr expr)) env fenv))
         ;; rest - same as cdr
         ((eq op 'rest) (nc-compile `(cdr ,(cadr expr)) env fenv))
         ;; nth - get nth element
         ((eq op 'nth)
          (let ((n (cadr expr))
                (lst (caddr expr)))
            (if (numberp n)
                ;; Constant index - expand to car/cdr chain
                (if (= n 0)
                    (nc-compile `(car ,lst) env fenv)
                    (nc-compile `(nth ,(- n 1) (cdr ,lst)) env fenv))
                ;; Variable index - use labels recursion
                (let ((nth-iter-fn (gensym "NTH-ITER"))
                      (n-var (gensym "N"))
                      (lst-var (gensym "LST")))
                  (nc-compile
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
            (nc-compile
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
                     (if (null args) (list 'lit 0)
                         (list 'cons-ir (nc-compile (car args) env fenv) (bl (cdr args))))))
            (bl (cdr expr))))
         ((eq op 'null)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (list 'lit 0)))
         ((eq op 'numberp)
          ;; get-tag returns tagged fixnum (tag << 4), lit also tags its value
          ;; so to compare tag=0, use (lit 0) -> becomes 0
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 0)))
         ((eq op 'consp)
          ;; get-tag returns tagged fixnum (tag << 4), lit also tags its value
          ;; so to compare tag=1, use (lit 1) -> becomes 1<<4=16
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 1)))
         ((eq op 'symbolp)
          ;; Symbol tag is 2, so compare with (lit 2) -> becomes 2<<4=32
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 2)))
         ((eq op 'stringp)
          ;; String tag is 4, so compare with (lit 4) -> becomes 4<<4=64
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 4)))
         ((eq op 'vectorp)
          ;; Vector tag is 3, so compare with (lit 3) -> becomes 3<<4=48
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 3)))
         ;; length - list length via recursion
         ((eq op 'length)
          (let ((len-iter-fn (gensym "LEN-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC")))
            (nc-compile
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
            (nc-compile
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
                (nc-compile nil env fenv)
                (if (null (cdr args))
                    (nc-compile (car args) env fenv)
                    ;; Two-arg append: copy first list, point to second
                    (let ((app-iter-fn (gensym "APP-ITER"))
                          (lst-var (gensym "LST"))
                          (tail-var (gensym "TAIL")))
                      (nc-compile
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
            (nc-compile
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
            (nc-compile
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
            (nc-compile
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
                    (nc-compile (car forms) env fenv)
                    (list 'progn-ir
                          (mapcar (lambda (f) (nc-compile f env fenv)) forms))))))
         ;; and - short-circuit and
         ((eq op 'and)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 1)
                (if (null (cdr args))
                    (nc-compile (car args) env fenv)
                    (list 'if-ir
                          (nc-compile (car args) env fenv)
                          (nc-compile (cons 'and (cdr args)) env fenv)
                          (list 'lit 0))))))
         ;; or - short-circuit or (returns first truthy value)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 0)
                (if (null (cdr args))
                    (nc-compile (car args) env fenv)
                    ;; Need to evaluate first arg, check if truthy, return it or continue
                    ;; Use a let to bind the value, then check and return
                    (let ((tmp (gensym "OR")))
                      (nc-compile
                       (list 'LET (list (list tmp (car args)))
                             (list 'if tmp tmp (cons 'or (cdr args))))
                       env fenv))))))
         ;; not - logical not
         ((eq op 'not)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (list 'lit 0)))
         ;; funcall - call function by value
         ((eq op 'funcall)
          (list 'funcall-ir
                (nc-compile (cadr expr) env fenv)
                (mapcar (lambda (a) (nc-compile a env fenv)) (cddr expr))))
         ;; setq - assign to variable
         ((eq op 'setq)
          (let* ((var (cadr expr))
                 (val (caddr expr))
                 (off (nc-env-lookup var env)))
            (if (numberp off)
                (list 'setq-ir off (nc-compile val env fenv))
                (list 'lit 0))))
         ;; setcar - mutate car of cons cell
         ((eq op 'setcar)
          (list 'setcar-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; setcdr - mutate cdr of cons cell
         ((eq op 'setcdr)
          (list 'setcdr-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; read-file - read entire file contents as string
         ((eq op 'read-file)
          (list 'read-file-ir (nc-compile (cadr expr) env fenv)))
         ;; write-file - write string to file
         ((eq op 'write-file)
          (list 'write-file-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; println - print value with newline
         ((eq op 'println)
          (list 'println-ir (nc-compile (cadr expr) env fenv)))
         ;; string-length - get length of string
         ((eq op 'string-length)
          (list 'string-length-ir (nc-compile (cadr expr) env fenv)))
         ;; string-ref - get character at index
         ((eq op 'string-ref)
          (list 'string-ref-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
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
            (nc-compile
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
            (nc-compile
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
          (list 'system-ir (nc-compile (cadr expr) env fenv)))
         ;; string= - compare two strings (via runtime)
         ((eq op 'string=)
          (list 'string-equal-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)))
         ;; make-vector - allocate a vector of size n
         ((eq op 'make-vector)
          (list 'make-vector-ir (nc-compile (cadr expr) env fenv)))
         ;; vector-set - set element at index
         ((eq op 'vector-set)
          (list 'vector-set-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)
                (nc-compile (cadddr expr) env fenv)))
         ;; vector-ref - get element at index
         ((eq op 'vector-ref)
          (list 'vector-ref-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)))
         ;; aref - same as vector-ref for now
         ((eq op 'aref)
          (list 'vector-ref-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)))
         ;; vector-length - get vector size
         ((eq op 'vector-length)
          (list 'vector-length-ir (nc-compile (cadr expr) env fenv)))
         ;; buffer-byte-ref - get raw byte at index from vector data area
         ;; Used for reading file data written by sys-read
         ((eq op 'buffer-byte-ref)
          (list 'buffer-byte-ref-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)))
         ;; make-string-from-vector - convert vector of char codes to string
         ((eq op 'make-string-from-vector)
          (list 'make-string-from-vector-ir (nc-compile (cadr expr) env fenv)))
         ;; buffer-to-string - convert raw byte buffer to string (for sys-read data)
         ((eq op 'buffer-to-string)
          (list 'buffer-to-string-ir
                (nc-compile (cadr expr) env fenv)    ; buffer
                (nc-compile (caddr expr) env fenv))) ; length
         ;; make-symbol-from-string - intern a string as symbol
         ((eq op 'make-symbol-from-string)
          (list 'make-symbol-from-string-ir (nc-compile (cadr expr) env fenv)))
         ;; intern - same as make-symbol-from-string
         ((eq op 'intern)
          (list 'make-symbol-from-string-ir (nc-compile (cadr expr) env fenv)))
         ;; symbol-name - get the name string of a symbol
         ((eq op 'symbol-name)
          (list 'symbol-name-ir (nc-compile (cadr expr) env fenv)))
         ;; write-bytes - write vector of bytes to file
         ((eq op 'write-bytes)
          (list 'write-bytes-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)))
         ;; === libSystem calls (for native executables) ===
         ;; sys-write - write(fd, buf, len) -> returns bytes written
         ((eq op 'sys-write)
          (list 'sys-write-ir
                (nc-compile (cadr expr) env fenv)    ; fd
                (nc-compile (caddr expr) env fenv)   ; buf (string)
                (nc-compile (cadddr expr) env fenv))) ; len
         ;; sys-read - read(fd, buf, len) -> returns bytes read
         ((eq op 'sys-read)
          (list 'sys-read-ir
                (nc-compile (cadr expr) env fenv)    ; fd
                (nc-compile (caddr expr) env fenv)   ; buf (vector)
                (nc-compile (cadddr expr) env fenv))) ; len
         ;; sys-open - open(path, flags, mode) -> returns fd
         ((eq op 'sys-open)
          (list 'sys-open-ir
                (nc-compile (cadr expr) env fenv)    ; path (string)
                (nc-compile (caddr expr) env fenv)   ; flags
                (nc-compile (cadddr expr) env fenv))) ; mode
         ;; sys-close - close(fd) -> returns 0 on success
         ((eq op 'sys-close)
          (list 'sys-close-ir
                (nc-compile (cadr expr) env fenv)))  ; fd
         ;; sys-exit - exit(code) -> does not return
         ((eq op 'sys-exit)
          (list 'sys-exit-ir
                (nc-compile (cadr expr) env fenv)))  ; exit code
         ;; === High-level file I/O (using sys-* primitives) ===
         ;; native-read-file - read entire file to string
         ;; Expands to: (let* ((fd (sys-open path O_RDONLY 0))
         ;;                     (buf (make-vector 65536))
         ;;                     (n (sys-read fd buf 65536)))
         ;;               (sys-close fd)
         ;;               (buffer-to-string buf n))
         ((eq op 'native-read-file)
          (let ((path-var (gensym "PATH"))
                (fd-var (gensym "FD"))
                (buf-var (gensym "BUF"))
                (n-var (gensym "N")))
            (nc-compile
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
            (nc-compile
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
            (nc-compile
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
          (let ((path-var (gensym "PATH"))
                (fd-var (gensym "FD"))
                (buf-var (gensym "BUF"))
                (n-var (gensym "N"))
                (chunk-var (gensym "CHUNK"))
                (next-chunks-var (gensym "NEXT-CHUNKS"))
                (next-total-var (gensym "NEXT-TOTAL"))
                (chunks-var (gensym "CHUNKS"))
                (total-var (gensym "TOTAL"))
                (result-list-var (gensym "RESULT-LIST"))
                (read-chunks-fn (gensym "READ-CHUNKS")))
            (nc-compile
             (list 'let (list (list path-var (cadr expr))
                              (list fd-var (list 'sys-open path-var #x0 0)))
                   (list 'labels (list (list read-chunks-fn (list chunks-var total-var)
                                             ;; BUG #20 FIX: Allocate buffer INSIDE labels to avoid capture crash
                                             (list 'let* (list (list buf-var (list 'make-vector 65536))
                                                               (list n-var (list 'sys-read fd-var buf-var 65536)))
                                                   (list 'if (list '= n-var 0)
                                                         (list 'list chunks-var total-var)
                                                         (list 'let* (list (list chunk-var (list 'buffer-to-string buf-var n-var))
                                                                           (list next-chunks-var (list 'cons chunk-var chunks-var))
                                                                           (list next-total-var (list '+ total-var n-var)))
                                                               (list read-chunks-fn next-chunks-var next-total-var))))))
                         (list 'let* (list (list result-list-var (list read-chunks-fn nil 0))
                                           (list chunks-var (list 'car result-list-var))
                                           (list total-var (list 'car (list 'cdr result-list-var))))
                               (list 'sys-close fd-var)
                               (list 'concat-string-list chunks-var total-var))))
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
          (let ((chunks-var (gensym "CHUNKS"))
                (total-var (gensym "TOTAL"))
                (vec-var (gensym "VEC"))
                (offset-var (gensym "OFFSET"))
                (chunk-var (gensym "CHUNK"))
                (len-var (gensym "LEN"))
                (i-var (gensym "I"))
                (next-i-var (gensym "NEXT-I"))
                (next-chunks-var (gensym "NEXT-CHUNKS"))
                (next-offset-var (gensym "NEXT-OFFSET"))
                (copy-chunk-fn (gensym "COPY-CHUNK"))
                (copy-chars-fn (gensym "COPY-CHARS")))
            (nc-compile
             (list 'let* (list (list chunks-var (cadr expr))
                               (list total-var (caddr expr))
                               (list vec-var (list 'make-vector total-var)))
                   (list 'labels (list (list copy-chunk-fn (list chunks-var offset-var)
                                             (list 'if (list 'null chunks-var)
                                                   vec-var
                                                   (list 'let* (list (list chunk-var (list 'car chunks-var))
                                                                     (list len-var (list 'string-length chunk-var)))
                                                         (list 'labels (list (list copy-chars-fn (list i-var)
                                                                                   (list 'if (list '< i-var len-var)
                                                                                         (list 'progn
                                                                                               (list 'vector-set vec-var (list '+ offset-var i-var) (list 'string-ref chunk-var i-var))
                                                                                               ;; BUG #20 WORKAROUND: Evaluate + before recursive call
                                                                                               (list 'let (list (list next-i-var (list '+ i-var 1)))
                                                                                                     (list copy-chars-fn next-i-var))))))
                                                               (list copy-chars-fn 0)
                                                               ;; BUG #20 WORKAROUND: Evaluate in let before recursive call
                                                               (list 'let (list (list next-chunks-var (list 'cdr chunks-var))
                                                                                (list next-offset-var (list '+ offset-var len-var)))
                                                                     (list copy-chunk-fn next-chunks-var next-offset-var)))))))
                         (list 'make-string-from-vector (list copy-chunk-fn (list 'reverse chunks-var) 0))))
             env fenv)))
         ;; char-upcase - convert lowercase char code to uppercase
         ;; Transform to: (if (and (>= ch #x61) (<= ch #x7A)) (- ch #x20) ch)
         ((eq op 'char-upcase)
          (let ((ch-var (gensym "CH")))
            (nc-compile
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
            (nc-compile
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
            (nc-compile (list 'setq place (list '+ place delta)) env fenv)))
         ;; push - push item onto list variable
         ((eq op 'push)
          (let* ((item (cadr expr))
                 (place (caddr expr)))
            (nc-compile (list 'setq place (list 'cons item place)) env fenv)))
         ;; setf - generalized assignment
         ((eq op 'setf)
          (let* ((place (cadr expr))
                 (val (caddr expr)))
            (if (symbolp place)
                ;; Simple variable assignment
                (nc-compile (list 'setq place val) env fenv)
                (if (consp place)
                    (let ((place-op (car place)))
                      (cond ((eq place-op 'car)
                             (nc-compile (list 'setcar (cadr place) val) env fenv))
                            ((eq place-op 'cdr)
                             (nc-compile (list 'setcdr (cadr place) val) env fenv))
                            ((eq place-op 'aref)
                             (nc-compile (list 'vector-set (cadr place) (caddr place) val) env fenv))
                            ((eq place-op 'nth)
                             ;; (setf (nth n lst) val) -> setcar on nthcdr
                             (nc-compile (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val) env fenv))
                            (t (list 'lit 0))))
                    (list 'lit 0)))))
         ;; nthcdr - get nth cdr of list
         ((eq op 'nthcdr)
          (list 'nthcdr-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; values - return multiple values
         ((eq op 'values)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'values-ir nil)
                (list 'values-ir (mapcar (lambda (a) (nc-compile a env fenv)) args)))))
         ;; multiple-value-bind - bind multiple values from form
         ((eq op 'multiple-value-bind)
          (let* ((vars (cadr expr))
                 (form (caddr expr))
                 (body (cdddr expr))
                 (nvars (length vars)))
            (list 'mvb-ir vars (nc-compile form env fenv) nvars
                  (nc-compile (if (null (cdr body)) (car body) (cons 'progn body))
                              (nc-env-extend (mapcar (lambda (v) (cons v nil)) vars) env)
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
                                              (rewritten (nc-rewrite-labels-body fn-body-expr fn-names fntab-var))
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
                 (rewritten-main (nc-rewrite-labels-body main-body fn-names fntab-var))
                 ;; Build: (let bindings (setq ...) (let ((FNTAB123 (cons ...))) main))
                 (inner-let (list 'LET (list (list fntab-var fntab-init)) rewritten-main))
                 (full-expr (list 'LET let-bindings
                                  (cons 'progn (append setq-forms (list inner-let))))))
            (nc-compile full-expr env fenv)))
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
                 (rewritten-main (nc-rewrite-labels-calls main-body fn-names)))
            (nc-compile (list 'LET let-bindings rewritten-main) env fenv)))
         ;; User function call or call via variable
         (t
          (cond
           ;; op is a lambda expression: ((lambda (x) body) args...)
           ((and (consp op) (eq (car op) 'lambda))
            (list 'funcall-ir
                  (nc-compile op env fenv)
                  (mapcar (lambda (a) (nc-compile a env fenv)) (cdr expr))))
           ;; op is a known function name
           ((and fenv (assoc op fenv))
            (list 'call-fn op (mapcar (lambda (a) (nc-compile a env fenv)) (cdr expr))))
           ;; op is a variable (parameter) - compile as funcall
           (t
            (let ((off (nc-env-lookup op env)))
              (if (numberp off)
                  (list 'funcall-ir (list 'var off) (mapcar (lambda (a) (nc-compile a env fenv)) (cdr expr)))
                  (list 'lit 0)))))))))
    (t (list 'lit 0))))

;;; ============================================================
;;; Part 6b: IR Evaluator (nc-eval-*)
;;; ============================================================

(defun nc-eval-ir (ir env)
  "Evaluate IR and return tagged value"
  (cond
    ((nc-has-tag ir 'lit) (cadr ir))
    ((nc-has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((nc-has-tag ir 'add)
     (+ (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'sub)
     (- (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'mul)
     (* (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'band)
     (logand (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'bor)
     (logior (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'bxor)
     (logxor (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'bsh)
     (ash (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'cmp-eq)
     (if (= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-lt)
     (if (< (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-gt)
     (if (> (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-le)
     (if (<= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-ge)
     (if (>= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'if-ir)
     (if (not (= (nc-eval-ir (cadr ir) env) 0))
         (nc-eval-ir (caddr ir) env)
         (nc-eval-ir (cadddr ir) env)))
    ((nc-has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     ;; offs is at index 4, which is (nth 3 (cdr ir))
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))  ; Fixed: was (nth 4 ...)
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (nc-eval-ir (car vs) env)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (nc-eval-ir bir (bind vals offs env)))))
    (t 0)))

;; Global function environment for IR evaluation
(defvar *nc-fenv* nil)

(defun nc-eval-ir-with-fns (ir env fenv)
  "Evaluate IR with function environment"
  (cond
    ((nc-has-tag ir 'lit) (cadr ir))
    ((nc-has-tag ir 'nil-ir) nil)  ;; Evaluate to proper SBCL nil
    ((nc-has-tag ir 'sym-lit)
     ;; Return the symbol itself (interned)
     (intern (cadr ir)))
    ((nc-has-tag ir 'str-lit)
     ;; Return the string literal directly
     (cadr ir))
    ((nc-has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((nc-has-tag ir 'add)
     (+ (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'sub)
     (- (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'mul)
     (* (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'div)
     (truncate (nc-eval-ir-with-fns (cadr ir) env fenv)
               (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'mod-ir)
     (mod (nc-eval-ir-with-fns (cadr ir) env fenv)
          (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'band)
     (logand (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'bor)
     (logior (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'bxor)
     (logxor (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'bsh)
     (ash (nc-eval-ir-with-fns (cadr ir) env fenv)
          (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'cmp-eq)
     (if (= (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-lt)
     (if (< (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-gt)
     (if (> (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-le)
     (if (<= (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-ge)
     (if (>= (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cons-ir)
     (cons (nc-eval-ir-with-fns (cadr ir) env fenv)
           (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'car-ir)
     (car (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ((nc-has-tag ir 'cdr-ir)
     (cdr (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ((nc-has-tag ir 'if-ir)
     (if (not (= (nc-eval-ir-with-fns (cadr ir) env fenv) 0))
         (nc-eval-ir-with-fns (caddr ir) env fenv)
         (nc-eval-ir-with-fns (cadddr ir) env fenv)))
    ((nc-has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (nc-eval-ir-with-fns (car vs) env fenv)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (nc-eval-ir-with-fns bir (bind vals offs env) fenv))))
    ((nc-has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     (let ((forms-ir (cadr ir)))
       (labels ((eval-seq (fs)
                  (if (null fs)
                      0
                      (let ((v (nc-eval-ir-with-fns (car fs) env fenv)))
                        (if (null (cdr fs))
                            v
                            (eval-seq (cdr fs)))))))
         (eval-seq forms-ir))))
    ((nc-has-tag ir 'call-fn)
     ;; call-fn = (call-fn name args-ir-list)
     (let* ((fnm (cadr ir))
            (args-ir (caddr ir))
            (fn-def (cdr (assoc fnm fenv))))
       (if fn-def
           ;; fn-def = (name params body-ir param-base)
           (let* ((params (cadr fn-def))
                  (body-ir (caddr fn-def)))
             ;; Evaluate arguments
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
               (let ((arg-vals (eval-args args-ir nil)))
                 ;; Call with new env containing args
                 (nc-eval-ir-with-fns body-ir arg-vals fenv))))
           0)))
    ((nc-has-tag ir 'funcall-ir)
     ;; funcall-ir = (funcall-ir fn-ir args-ir-list)
     ;; fn-ir evaluates to a function name (symbol) or closure
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (fn-val (nc-eval-ir-with-fns fn-ir env fenv)))
       ;; Check if fn-val is a closure (list starting with :closure)
       (if (and (consp fn-val) (eq (car fn-val) :closure))
           ;; Closure: (:closure params body-ir free-vars captured-vals)
           ;; body is now pre-compiled IR
           (let* ((params (cadr fn-val))
                  (body-ir (caddr fn-val))
                  (free-vars (cadddr fn-val))
                  (captured-vals (nth 4 fn-val)))
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
               (let* ((arg-vals (eval-args args-ir nil))
                      ;; Build value list: free vars (captured) come first, then args
                      (all-vals (append captured-vals arg-vals)))
                 (nc-eval-ir-with-fns body-ir all-vals fenv))))
           ;; Named function: look up in fenv
           (let ((fn-def (cdr (assoc fn-val fenv))))
             (if fn-def
                 (let* ((params (cadr fn-def))
                        (body-ir (caddr fn-def)))
                   (labels ((eval-args (airs acc)
                              (if (null airs) (reverse acc)
                                  (eval-args (cdr airs)
                                             (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
                     (let ((arg-vals (eval-args args-ir nil)))
                       (nc-eval-ir-with-fns body-ir arg-vals fenv))))
                 0)))))
    ((nc-has-tag ir 'lambda-ref)
     ;; lambda-ref = (lambda-ref fn-name free-var-offsets)
     ;; Returns the function name as a symbol for lookup in funcall
     (cadr ir))
    ((nc-has-tag ir 'lambda-ir)
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
    ((nc-has-tag ir 'dotimes-ir)
     ;; dotimes-ir = (dotimes-ir var count-ir body-ir result-ir compile-env)
     (let* ((var (cadr ir))
            (count-ir (caddr ir))
            (body-ir (cadddr ir))
            (result-ir (nth 4 ir))
            (compile-env (nth 5 ir))
            (count (nc-eval-ir-with-fns count-ir env fenv)))
       ;; Iterative loop
       (labels ((iter (i)
                  (if (>= i count)
                      (nc-eval-ir-with-fns result-ir (append env (list i)) fenv)
                      (progn
                        (nc-eval-ir-with-fns body-ir (append env (list i)) fenv)
                        (iter (+ i 1))))))
         (iter 0))))
    ((nc-has-tag ir 'dolist-ir)
     ;; dolist-ir = (dolist-ir var list-ir body-ir result-ir compile-env)
     (let* ((var (cadr ir))
            (list-ir (caddr ir))
            (body-ir (cadddr ir))
            (result-ir (nth 4 ir))
            (compile-env (nth 5 ir))
            (lst (nc-eval-ir-with-fns list-ir env fenv)))
       ;; Iterative loop over list
       (labels ((iter (remaining)
                  (if (null remaining)
                      (nc-eval-ir-with-fns result-ir (append env (list nil)) fenv)
                      (let ((elem (car remaining)))
                        (nc-eval-ir-with-fns body-ir (append env (list elem)) fenv)
                        (iter (cdr remaining))))))
         (iter lst))))
    ;; setq-ir - assign to variable in env
    ((nc-has-tag ir 'setq-ir)
     ;; setq-ir = (setq-ir offset value-ir)
     ;; Note: env is immutable in evaluator, so we simulate via setf on nth
     (let* ((off (cadr ir))
            (val (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (setf (nth off env) val)
       val))
    ;; setcar-ir - mutate car of cons cell
    ((nc-has-tag ir 'setcar-ir)
     ;; setcar-ir = (setcar-ir cons-ir value-ir)
     (let* ((cell (nc-eval-ir-with-fns (cadr ir) env fenv))
            (val (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (setf (car cell) val)
       val))
    ;; setcdr-ir - mutate cdr of cons cell
    ((nc-has-tag ir 'setcdr-ir)
     ;; setcdr-ir = (setcdr-ir cons-ir value-ir)
     (let* ((cell (nc-eval-ir-with-fns (cadr ir) env fenv))
            (val (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (setf (cdr cell) val)
       val))
    ;; read-file-ir - read entire file as string
    ((nc-has-tag ir 'read-file-ir)
     (let ((path (nc-eval-ir-with-fns (cadr ir) env fenv)))
       (with-open-file (in path :direction :input)
         (let ((contents (make-string (file-length in))))
           (read-sequence contents in)
           contents))))
    ;; write-file-ir - write string to file
    ((nc-has-tag ir 'write-file-ir)
     (let ((path (nc-eval-ir-with-fns (cadr ir) env fenv))
           (contents (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (with-open-file (out path :direction :output :if-exists :supersede)
         (write-string contents out))
       contents))
    ;; println-ir - print value with newline
    ((nc-has-tag ir 'println-ir)
     (let ((val (nc-eval-ir-with-fns (cadr ir) env fenv)))
       (format t "~A~%" val)
       val))
    ;; string-length-ir - get length of string
    ((nc-has-tag ir 'string-length-ir)
     (length (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ;; string-ref-ir - get character at index
    ((nc-has-tag ir 'string-ref-ir)
     (char-code (char (nc-eval-ir-with-fns (cadr ir) env fenv)
                      (nc-eval-ir-with-fns (caddr ir) env fenv))))
    ;; system-ir - execute shell command (evaluator uses SBCL's system)
    ((nc-has-tag ir 'system-ir)
     (let ((cmd (nc-eval-ir-with-fns (cadr ir) env fenv)))
       #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output t :wait t)
       0))
    ;; string-equal-ir - compare two strings
    ((nc-has-tag ir 'string-equal-ir)
     (let ((s1 (nc-eval-ir-with-fns (cadr ir) env fenv))
           (s2 (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (if (string= s1 s2) 1 0)))
    ;; make-vector-ir - allocate vector
    ((nc-has-tag ir 'make-vector-ir)
     (make-array (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ;; vector-set-ir - set element at index
    ((nc-has-tag ir 'vector-set-ir)
     (let ((vec (nc-eval-ir-with-fns (cadr ir) env fenv))
           (idx (nc-eval-ir-with-fns (caddr ir) env fenv))
           (val (nc-eval-ir-with-fns (cadddr ir) env fenv)))
       (setf (aref vec idx) val)
       val))
    ;; vector-ref-ir - get element at index
    ((nc-has-tag ir 'vector-ref-ir)
     (let ((vec (nc-eval-ir-with-fns (cadr ir) env fenv))
           (idx (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (aref vec idx)))
    ;; buffer-byte-ref-ir - get raw byte at index (for evaluator, same as aref)
    ((nc-has-tag ir 'buffer-byte-ref-ir)
     (let ((vec (nc-eval-ir-with-fns (cadr ir) env fenv))
           (idx (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (aref vec idx)))
    ;; make-string-from-vector-ir - convert vector to string
    ((nc-has-tag ir 'make-string-from-vector-ir)
     (let ((vec (nc-eval-ir-with-fns (cadr ir) env fenv)))
       (map 'string #'code-char vec)))
    ;; buffer-to-string-ir - convert raw byte buffer to string
    ((nc-has-tag ir 'buffer-to-string-ir)
     (let ((buf (nc-eval-ir-with-fns (cadr ir) env fenv))
           (len (nc-eval-ir-with-fns (caddr ir) env fenv)))
       ;; For evaluator (SBCL), assume buf is a vector of bytes
       (map 'string #'code-char (subseq buf 0 len))))
    ;; make-symbol-from-string-ir - intern string as symbol
    ((nc-has-tag ir 'make-symbol-from-string-ir)
     (let ((str (nc-eval-ir-with-fns (cadr ir) env fenv)))
       (intern str)))
    ;; symbol-name-ir - get symbol's name string
    ((nc-has-tag ir 'symbol-name-ir)
     (let ((sym (nc-eval-ir-with-fns (cadr ir) env fenv)))
       (symbol-name sym)))
    ;; write-bytes-ir - write vector of bytes to file (for evaluator, use SBCL)
    ((nc-has-tag ir 'write-bytes-ir)
     (let ((path (nc-eval-ir-with-fns (cadr ir) env fenv))
           (vec (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
         (dotimes (i (length vec))
           (write-byte (aref vec i) out)))
       0))
    ;; nthcdr-ir - get nth cdr of list
    ((nc-has-tag ir 'nthcdr-ir)
     ;; nthcdr-ir = (nthcdr-ir n-ir list-ir)
     (let* ((n (nc-eval-ir-with-fns (cadr ir) env fenv))
            (lst (nc-eval-ir-with-fns (caddr ir) env fenv)))
       (labels ((drop (cnt l)
                  (if (or (<= cnt 0) (null l))
                      l
                      (drop (- cnt 1) (cdr l)))))
         (drop n lst))))
    ;; values-ir - return multiple values
    ((nc-has-tag ir 'values-ir)
     ;; values-ir = (values-ir (ir1 ir2 ...))
     (let ((irs (cadr ir)))
       (if (null irs)
           nil  ; no values
           (if (null (cdr irs))
               ;; single value - just return it
               (nc-eval-ir-with-fns (car irs) env fenv)
               ;; multiple values - return as list for evaluator
               (labels ((eval-all (vs acc)
                          (if (null vs)
                              (reverse acc)
                              (eval-all (cdr vs)
                                       (cons (nc-eval-ir-with-fns (car vs) env fenv) acc)))))
                 (eval-all irs nil))))))
    ;; mvb-ir - multiple-value-bind
    ((nc-has-tag ir 'mvb-ir)
     ;; mvb-ir = (mvb-ir vars form-ir nvars body-ir)
     (let* ((vars (cadr ir))
            (form-ir (caddr ir))
            (nvars (cadddr ir))
            (body-ir (nth 4 ir))
            (result (nc-eval-ir-with-fns form-ir env fenv)))
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
             (nc-eval-ir-with-fns body-ir (append env padded-vals) fenv))))))
    (t 0)))

;;; ============================================================
;;; Part 7: Code Generator (nc-codegen-*)
;;; ============================================================

(defun nc-ir-may-call? (ir)
  "Returns t if evaluating IR might make function calls that could clobber x24.
   This is used to optimize away unnecessary x24 save/restore in binary ops."
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((nc-has-tag ir 'lit) nil)
    ((nc-has-tag ir 'nil-ir) nil)
    ((nc-has-tag ir 'sym-lit) nil)
    ((nc-has-tag ir 'str-lit) nil)
    ((nc-has-tag ir 'var) nil)
    ;; Function calls definitely clobber x24
    ((nc-has-tag ir 'call-fn) t)
    ((nc-has-tag ir 'tail-call-fn) t)
    ((nc-has-tag ir 'funcall-ir) t)
    ((nc-has-tag ir 'call-closure) t)
    ;; Runtime calls also clobber x24
    ((nc-has-tag ir 'runtime-call) t)
    ((nc-has-tag ir 'format-call) t)
    ((nc-has-tag ir 'gensym-call) t)
    ((nc-has-tag ir 'open-file-call) t)
    ((nc-has-tag ir 'close-file-call) t)
    ((nc-has-tag ir 'read-line-call) t)
    ((nc-has-tag ir 'write-string-call) t)
    ((nc-has-tag ir 'read-file-call) t)
    ((nc-has-tag ir 'write-file-call) t)
    ((nc-has-tag ir 'values-call) t)
    ((nc-has-tag ir 'values-get-call) t)
    ((nc-has-tag ir 'values-count-call) t)
    ((nc-has-tag ir 'print-call) t)
    ((nc-has-tag ir 'profile-time-call) t)
    ((nc-has-tag ir 'sys-write-call) t)
    ((nc-has-tag ir 'sys-read-call) t)
    ((nc-has-tag ir 'sys-open-call) t)
    ((nc-has-tag ir 'sys-close-call) t)
    ;; Binary/unary ops: check children
    ((nc-has-tag ir 'add) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'sub) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'mul) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'div) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'mod-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'band) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'bor) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'bxor) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'bsh) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'cmp-eq) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'cmp-lt) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'cmp-gt) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'cmp-le) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'cmp-ge) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ;; Unary ops
    ((nc-has-tag ir 'bnot) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'neg) (nc-ir-may-call? (cadr ir)))
    ;; Cons/car/cdr: check children
    ((nc-has-tag ir 'cons-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'car-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'cdr-ir) (nc-ir-may-call? (cadr ir)))
    ;; Control flow: check all branches
    ((nc-has-tag ir 'if-ir)
     (or (nc-ir-may-call? (cadr ir))
         (nc-ir-may-call? (caddr ir))
         (nc-ir-may-call? (cadddr ir))))
    ;; Progn: check all forms
    ((nc-has-tag ir 'progn-ir)
     (some #'nc-ir-may-call? (cdr ir)))
    ;; Let: check bindings and body
    ((nc-has-tag ir 'let-ir)
     (let ((bindings (cadr ir))
           (body (caddr ir)))
       (or (some #'nc-ir-may-call? bindings)
           (nc-ir-may-call? body))))
    ;; Vector/string operations are simple (inline)
    ((nc-has-tag ir 'make-vector-call) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'vector-set-ir) (or (nc-ir-may-call? (cadr ir))
                                         (nc-ir-may-call? (caddr ir))
                                         (nc-ir-may-call? (cadddr ir))))
    ((nc-has-tag ir 'vector-ref-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'vector-length-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'string-length-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'string-ref-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ;; Type predicates are simple
    ((nc-has-tag ir 'consp-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'null-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'numberp-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'symbolp-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'stringp-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'vectorp-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'eq-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ;; Lambda-ref is just loading an address
    ((nc-has-tag ir 'lambda-ref) nil)
    ;; Setq: check value
    ((nc-has-tag ir 'setq-ir) (nc-ir-may-call? (caddr ir)))
    ((nc-has-tag ir 'setcar-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ((nc-has-tag ir 'setcdr-ir) (or (nc-ir-may-call? (cadr ir)) (nc-ir-may-call? (caddr ir))))
    ;; Loop constructs
    ((nc-has-tag ir 'dotimes-ir) t)
    ((nc-has-tag ir 'dolist-ir) t)
    ;; Self-TCO loop constructs: check body for calls
    ((nc-has-tag ir 'loop-ir) (nc-ir-may-call? (cadr ir)))
    ((nc-has-tag ir 'continue-ir) (some #'nc-ir-may-call? (cadr ir)))
    ;; Default: assume it might call to be safe
    (t t)))

(defun nc-ir-is-simple? (ir)
  "Returns t if IR is simple (var or lit) and doesn't use any registers."
  (or (nc-has-tag ir 'var)
      (nc-has-tag ir 'lit)
      (nc-has-tag ir 'nil-ir)))

(defun nc-codegen-binop (left-ir right-ir op-instrs rtaddrs fnoffs td)
  "Generate code for binary operation with register-based temps.
   Uses temp registers when safe, falls back to stack when needed."
  (let* ((nd (+ td 1))
         (left-simple (nc-ir-is-simple? left-ir))
         (right-simple (nc-ir-is-simple? right-ir))
         (right-may-call (nc-ir-may-call? right-ir))
         (left-may-call (nc-ir-may-call? left-ir))
         ;; Use temp register for left value when no calls involved
         (use-register (and (not left-may-call) (not right-may-call))))
    (cond
      ;; Optimal case: both operands are simple (var/lit)
      ;; Use temp register, no spill needed
      ((and left-simple right-simple)
       (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd))
             (rc (nc-codegen right-ir rtaddrs fnoffs nd)))
         (nc-append-all
          (list lc                          ; eval left -> x0
                (nc-save-temp td)           ; save left in temp reg/slot
                rc                          ; eval right -> x0
                (nc-mov-reg 1 0)            ; x1 = right
                (nc-load-temp 0 td)         ; x0 = left
                op-instrs))))
      ;; Left may call - need stack spill (caller-saved regs clobbered)
      (left-may-call
       (let* ((xs (nc-temp-slot td))
              (ls (nc-temp-slot (+ td 1)))
              (lc (nc-codegen left-ir rtaddrs fnoffs (+ td 2)))
              (rc (nc-codegen right-ir rtaddrs fnoffs (+ td 2))))
         (nc-append-all
          (list (nc-str-offset 24 31 xs)   ; save x24
                lc                          ; eval left -> x0
                (nc-str-offset 0 31 ls)    ; save left value (must use stack)
                (nc-ldr-offset 24 31 xs)   ; restore x24
                rc                          ; eval right -> x0
                (nc-mov-reg 1 0)           ; x1 = right
                (nc-ldr-offset 0 31 ls)    ; x0 = left
                op-instrs))))
      ;; Left doesn't call but right does - still need stack for left
      (right-may-call
       (let* ((ls (nc-temp-slot td))
              (lc (nc-codegen left-ir rtaddrs fnoffs (+ td 1)))
              (rc (nc-codegen right-ir rtaddrs fnoffs (+ td 2))))  ; FIX: use td+2 to avoid clobbering temp[td]
         (nc-append-all
          (list lc                          ; eval left -> x0
                (nc-str-offset 0 31 ls)    ; save left value at temp[td]
                rc                          ; eval right -> x0 (uses temp[td+2]+ only)
                (nc-mov-reg 1 0)           ; x1 = right
                (nc-ldr-offset 0 31 ls)    ; x0 = left
                op-instrs))))
      ;; Neither calls - can use temp registers
      (t
       (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd))
             (rc (nc-codegen right-ir rtaddrs fnoffs nd)))
         (nc-append-all
          (list lc                          ; eval left -> x0
                (nc-save-temp td)           ; save left in temp reg
                rc                          ; eval right -> x0
                (nc-mov-reg 1 0)            ; x1 = right
                (nc-load-temp 0 td)         ; x0 = left
                op-instrs)))))))

(defun nc-codegen (ir rtaddrs fnoffs td)
  (cond
    ((nc-has-tag ir 'lit)
     (let* ((v (cadr ir))
            (tg (ash v 4)))
       (if (and (>= tg 0) (< tg #x10000))
           (nc-movz 0 tg)
           (nc-load-addr 0 tg))))
    ((nc-has-tag ir 'nil-ir)
     ;; nil is represented as tagged 0 (fixnum 0) in native code
     (nc-movz 0 0))
    ((nc-has-tag ir 'sym-lit)
     ;; Symbol literal: use compile-time symbol table
     ;; Each unique symbol gets a unique ID, tagged with symbol tag (2)
     ;; Tagged value = (ID << 4) | 2
     (let* ((name (cadr ir))
            (id (nc-intern-symbol name))
            (tagged (logior (ash id 4) 2)))  ; tag 2 = symbol
       (if (< tagged #x10000)
           (nc-movz 0 tagged)
           (nc-load-addr 0 tagged))))
    ((nc-has-tag ir 'str-lit)
     ;; String literal: build string inline on heap using x28 bump pointer
     (let* ((s (cadr ir))
            (chars (nc-string-to-char-codes s)))
       (nc-codegen-string-inline chars)))
    ((nc-has-tag ir 'var)
     (let* ((off (cadr ir))
            (off8 (* off 8))
            (i1 (nc-sub-imm 1 20 off8))
            (i2 (nc-ldr-offset 0 1 0)))
       (nc-append-all (list i1 i2))))
    ((nc-has-tag ir 'get-tag)
     (let* ((ac (nc-codegen (cadr ir) rtaddrs fnoffs td))
            (i1 (nc-movz 1 #xF))
            (i2 (nc-and-reg 0 0 1))
            (i3 (nc-lsl-imm 0 0 4)))
       (nc-append-all (list ac i1 i2 i3))))
    ((nc-has-tag ir 'add)
     ;; Fast path: (add (var n) (lit k)) or (add (lit k) (var n)) -> single ADD imm
     (let ((left (cadr ir))
           (right (caddr ir)))
       (cond
         ;; (add var lit) where lit fits in 12-bit immediate
         ((and (nc-has-tag left 'var) (nc-has-tag right 'lit)
               (< (ash (cadr right) 4) #x1000))
          (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                (imm (ash (cadr right) 4)))
            (append var-code (nc-add-imm 0 0 imm))))
         ;; (add lit var) - swap operands
         ((and (nc-has-tag left 'lit) (nc-has-tag right 'var)
               (< (ash (cadr left) 4) #x1000))
          (let ((var-code (nc-codegen right rtaddrs fnoffs td))
                (imm (ash (cadr left) 4)))
            (append var-code (nc-add-imm 0 0 imm))))
         ;; General case
         (t (nc-codegen-binop left right (nc-add-reg 0 0 1) rtaddrs fnoffs td)))))
    ((nc-has-tag ir 'sub)
     ;; Fast path: (sub (var n) (lit k)) -> single SUB imm
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (nc-has-tag left 'var) (nc-has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (append var-code (nc-sub-imm 0 0 imm)))
           (nc-codegen-binop left right (nc-sub-reg 0 0 1) rtaddrs fnoffs td))))
    ((nc-has-tag ir 'mul)
     ;; Optimized multiplication: untag only ONE operand
     ;; (a<<4) * (b>>4) = (a*b)<<4 -- correctly tagged result!
     ;; Saves 2 instructions vs untagging both and retagging
     (nc-codegen-binop (cadr ir) (caddr ir)
                       (nc-append-all (list (nc-lsr-imm 1 1 4)    ; untag right only
                                            (nc-mul-reg 0 0 1)))  ; (left<<4) * right = result<<4
                       rtaddrs fnoffs td))
    ((nc-has-tag ir 'band)
     (nc-codegen-binop (cadr ir) (caddr ir) (nc-and-reg 0 0 1) rtaddrs fnoffs td))
    ((nc-has-tag ir 'bor)
     (nc-codegen-binop (cadr ir) (caddr ir) (nc-orr-reg 0 0 1) rtaddrs fnoffs td))
    ((nc-has-tag ir 'bxor)
     (nc-codegen-binop (cadr ir) (caddr ir) (nc-eor-reg 0 0 1) rtaddrs fnoffs td))
    ((nc-has-tag ir 'bsh)
     ;; Shift: optimized x24 save/restore
     (let* ((val-ir (cadr ir))
            (amt-ir (caddr ir))
            (xs (nc-temp-slot td))
            (vs (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (nc-codegen val-ir rtaddrs fnoffs nd))
            (ac (nc-codegen amt-ir rtaddrs fnoffs nd))
            (may-call (nc-ir-may-call? val-ir))
            (shift-code (nc-append-all
                         (list (nc-asr-imm 1 0 4)
                               (nc-ldr-offset 0 31 vs)
                               (nc-cmp-imm 1 0)
                               (nc-b-cond (nc-cond-ge) 16)
                               (nc-neg-reg 2 1)
                               (nc-asrv-reg 0 0 2)
                               (nc-b-offset 8)
                               (nc-lslv-reg 0 0 1)
                               (nc-lsl-imm 0 0 4)))))
       (if may-call
           (nc-append-all (list (nc-str-offset 24 31 xs) vc (nc-lsr-imm 0 0 4)
                                (nc-str-offset 0 31 vs) (nc-ldr-offset 24 31 xs)
                                ac shift-code))
           (nc-append-all (list vc (nc-lsr-imm 0 0 4) (nc-str-offset 0 31 vs)
                                ac shift-code)))))
    ((nc-has-tag ir 'cmp-eq)
     (nc-codegen-binop (cadr ir) (caddr ir)
                       (nc-append-all (list (nc-cmp-reg 0 1)
                                            (nc-cset 0 (nc-cond-eq))
                                            (nc-lsl-imm 0 0 4)))
                       rtaddrs fnoffs td))
    ((nc-has-tag ir 'cmp-lt)
     ;; Fast path: (cmp-lt (var n) (lit k)) -> CMP x0, #imm; CSET
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (nc-has-tag left 'var) (nc-has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (nc-append-all (list var-code
                                  (nc-cmp-imm 0 imm)
                                  (nc-cset 0 (nc-cond-lt))
                                  (nc-lsl-imm 0 0 4))))
           (nc-codegen-binop left right
                             (nc-append-all (list (nc-cmp-reg 0 1)
                                                  (nc-cset 0 (nc-cond-lt))
                                                  (nc-lsl-imm 0 0 4)))
                             rtaddrs fnoffs td))))
    ((nc-has-tag ir 'cmp-gt)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (nc-has-tag left 'var) (nc-has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (nc-append-all (list var-code
                                  (nc-cmp-imm 0 imm)
                                  (nc-cset 0 (nc-cond-gt))
                                  (nc-lsl-imm 0 0 4))))
           (nc-codegen-binop left right
                             (nc-append-all (list (nc-cmp-reg 0 1)
                                                  (nc-cset 0 (nc-cond-gt))
                                                  (nc-lsl-imm 0 0 4)))
                             rtaddrs fnoffs td))))
    ((nc-has-tag ir 'cmp-le)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (nc-has-tag left 'var) (nc-has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (nc-append-all (list var-code
                                  (nc-cmp-imm 0 imm)
                                  (nc-cset 0 (nc-cond-le))
                                  (nc-lsl-imm 0 0 4))))
           (nc-codegen-binop left right
                             (nc-append-all (list (nc-cmp-reg 0 1)
                                                  (nc-cset 0 (nc-cond-le))
                                                  (nc-lsl-imm 0 0 4)))
                             rtaddrs fnoffs td))))
    ((nc-has-tag ir 'cmp-ge)
     (let ((left (cadr ir))
           (right (caddr ir)))
       (if (and (nc-has-tag left 'var) (nc-has-tag right 'lit)
                (< (ash (cadr right) 4) #x1000))
           (let ((var-code (nc-codegen left rtaddrs fnoffs td))
                 (imm (ash (cadr right) 4)))
             (nc-append-all (list var-code
                                  (nc-cmp-imm 0 imm)
                                  (nc-cset 0 (nc-cond-ge))
                                  (nc-lsl-imm 0 0 4))))
           (nc-codegen-binop left right
                             (nc-append-all (list (nc-cmp-reg 0 1)
                                                  (nc-cset 0 (nc-cond-ge))
                                                  (nc-lsl-imm 0 0 4)))
                             rtaddrs fnoffs td))))
    ((nc-has-tag ir 'cons-ir)
     ;; Inline cons: allocate 16 bytes from heap (x28), store car/cdr, return tagged ptr
     ;; x28 is the heap bump pointer, initialized at startup
     ;; Cons cell: [car at offset 0, cdr at offset 8], tagged with 1
     (let* ((car-ir (cadr ir))
            (cdr-ir (caddr ir))
            (xs (nc-temp-slot td))
            (cs (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (cc (nc-codegen car-ir rtaddrs fnoffs nd))
            (dc (nc-codegen cdr-ir rtaddrs fnoffs nd))
            (may-call (nc-ir-may-call? car-ir))
            (alloc-code (nc-append-all
                         (list (nc-mov-reg 1 0)             ; x1 = cdr value
                               (nc-ldr-offset 0 31 cs)      ; x0 = car value
                               (nc-str-offset 0 28 0)       ; [x28+0] = car
                               (nc-str-offset 1 28 8)       ; [x28+8] = cdr
                               (nc-mov-reg 0 28)            ; x0 = untagged ptr
                               (nc-add-imm 28 28 16)        ; bump heap by 16
                               (nc-movz 1 1)                ; x1 = 1
                               (nc-orr-reg 0 0 1)))))       ; x0 = ptr | 1
       (if may-call
           (nc-append-all (list (nc-str-offset 24 31 xs) cc (nc-str-offset 0 31 cs)
                                (nc-ldr-offset 24 31 xs) dc alloc-code))
           (nc-append-all (list cc (nc-str-offset 0 31 cs) dc alloc-code)))))
    ((nc-has-tag ir 'car-ir)
     ;; Inline car: clear tag bits, load from offset 0
     ;; (car nil) returns nil - check for nil first
     (let ((arg-ir (cadr ir)))
       (let ((ac (nc-codegen arg-ir rtaddrs fnoffs td)))
         (nc-append-all
          (list ac
                ;; Check for nil: if x0 == 0, skip load (return 0)
                (nc-cbz 0 28)                     ; if x0 == 0, skip 7 instrs (28 bytes)
                ;; Clear low 4 bits to get pointer
                (nc-movz 1 #xFFF0)                ; x1 = mask (keep upper bits)
                (nc-movk 1 #xFFFF 16)             ; complete mask
                (nc-movk 1 #xFFFF 32)
                (nc-movk 1 #xFFFF 48)
                (nc-and-reg 0 0 1)                ; x0 = ptr with tag cleared
                (nc-ldr-offset 0 0 0))))))        ; x0 = [ptr+0] = car
    ((nc-has-tag ir 'cdr-ir)
     ;; Inline cdr: clear tag bits, load from offset 8
     ;; (cdr nil) returns nil - check for nil first
     (let ((arg-ir (cadr ir)))
       (let ((ac (nc-codegen arg-ir rtaddrs fnoffs td)))
         (nc-append-all
          (list ac
                ;; Check for nil: if x0 == 0, skip load (return 0)
                (nc-cbz 0 28)                     ; if x0 == 0, skip 7 instrs (28 bytes)
                ;; Clear low 4 bits to get pointer
                (nc-movz 1 #xFFF0)                ; x1 = mask (keep upper bits)
                (nc-movk 1 #xFFFF 16)             ; complete mask
                (nc-movk 1 #xFFFF 32)
                (nc-movk 1 #xFFFF 48)
                (nc-and-reg 0 0 1)                ; x0 = ptr with tag cleared
                (nc-ldr-offset 0 0 8))))))
    ;; setq-ir - assign to variable
    ((nc-has-tag ir 'setq-ir)
     ;; setq-ir = (setq-ir offset value-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (vc (nc-codegen val-ir rtaddrs fnoffs td))
            (off8 (* off 8))
            (s1 (nc-sub-imm 1 20 off8))
            (s2 (nc-str-offset 0 1 0)))
       (nc-append-all (list vc s1 s2))))
    ;; setcar-ir - mutate car of cons cell
    ((nc-has-tag ir 'setcar-ir)
     ;; setcar-ir = (setcar-ir cons-ir value-ir)
     ;; Runtime index 14 = habu_set_car at offset 112
     ;; habu_set_car returns void, so we return the value
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (xs (nc-temp-slot td))
            (vs (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (cc (nc-codegen cons-ir rtaddrs fnoffs nd))
            (sc (nc-str-offset 0 31 xs))
            (vc (nc-codegen val-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 vs))
            (mv (nc-mov-reg 1 0))
            (lc (nc-ldr-offset 0 31 xs))
            (lf (nc-ldr-offset 9 19 112))
            (bl (nc-blr 9))
            (lr (nc-ldr-offset 0 31 vs)))
       (nc-append-all (list cc sc vc sv mv lc lf bl lr))))
    ;; setcdr-ir - mutate cdr of cons cell
    ((nc-has-tag ir 'setcdr-ir)
     ;; setcdr-ir = (setcdr-ir cons-ir value-ir)
     ;; Runtime index 15 = habu_set_cdr at offset 120
     ;; habu_set_cdr returns void, so we return the value
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (xs (nc-temp-slot td))
            (vs (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (cc (nc-codegen cons-ir rtaddrs fnoffs nd))
            (sc (nc-str-offset 0 31 xs))
            (vc (nc-codegen val-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 vs))
            (mv (nc-mov-reg 1 0))
            (lc (nc-ldr-offset 0 31 xs))
            (lf (nc-ldr-offset 9 19 120))
            (bl (nc-blr 9))
            (lr (nc-ldr-offset 0 31 vs)))
       (nc-append-all (list cc sc vc sv mv lc lf bl lr))))
    ;; read-file-ir - read entire file as string
    ((nc-has-tag ir 'read-file-ir)
     ;; read-file-ir = (read-file-ir path-ir)
     ;; Runtime index 46 = habu_read_file at offset 368
     (let* ((path-ir (cadr ir))
            (pc (nc-codegen path-ir rtaddrs fnoffs td))
            (lf (nc-ldr-offset 9 19 368))
            (bl (nc-blr 9)))
       (nc-append-all (list pc lf bl))))
    ;; write-file-ir - write string to file
    ((nc-has-tag ir 'write-file-ir)
     ;; write-file-ir = (write-file-ir path-ir contents-ir)
     ;; Runtime index 47 = habu_write_file at offset 376
     (let* ((path-ir (cadr ir))
            (contents-ir (caddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (pc (nc-codegen path-ir rtaddrs fnoffs nd))
            (sp (nc-str-offset 0 31 xs))
            (cc (nc-codegen contents-ir rtaddrs fnoffs nd))
            (m1 (nc-mov-reg 1 0))
            (lp (nc-ldr-offset 0 31 xs))
            (lf (nc-ldr-offset 9 19 376))
            (bl (nc-blr 9)))
       (nc-append-all (list pc sp cc m1 lp lf bl))))
    ;; println-ir - print value with newline
    ((nc-has-tag ir 'println-ir)
     ;; println-ir = (println-ir value-ir)
     ;; Runtime index 49 = habu_println_value at offset 392
     (let* ((val-ir (cadr ir))
            (vc (nc-codegen val-ir rtaddrs fnoffs td))
            (lf (nc-ldr-offset 9 19 392))
            (bl (nc-blr 9)))
       (nc-append-all (list vc lf bl))))
    ;; string-length-ir - get length of string (inline)
    ((nc-has-tag ir 'string-length-ir)
     ;; string-length-ir = (string-length-ir str-ir)
     ;; String layout: [length (8 bytes)] [char data]
     ;; Clear tag, load length, tag as fixnum
     (let* ((str-ir (cadr ir))
            (sc (nc-codegen str-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list sc
              ;; Clear low 4 bits to get pointer (same approach as car-ir)
              (nc-movz 1 #xFFF0)              ; x1 = mask (keep upper bits)
              (nc-movk 1 #xFFFF 16)           ; complete mask
              (nc-movk 1 #xFFFF 32)
              (nc-movk 1 #xFFFF 48)
              (nc-and-reg 0 0 1)              ; x0 = str_ptr (untagged)
              ;; Load length from [x0+0]
              (nc-ldr-offset 0 0 0)           ; x0 = raw length
              ;; Tag as fixnum: x0 = x0 << 4
              (nc-lsl-imm 0 0 4)))))
    ;; string-ref-ir - get character at index (inline)
    ((nc-has-tag ir 'string-ref-ir)
     ;; string-ref-ir = (string-ref-ir str-ir idx-ir)
     ;; String layout: [length (8 bytes)] [char data]
     ;; Address = (str & ~0xF) + 8 + (idx >> 4)
     (let* ((str-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (nc-temp-slot td))
            (is (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (sc (nc-codegen str-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 xs))
            (ic (nc-codegen idx-ir rtaddrs fnoffs nd))
            (si (nc-str-offset 0 31 is)))
       ;; After codegen: idx saved at [sp+is], str at [sp+xs]
       (nc-append-all
        (list sc sv ic si
              ;; Load str -> x1
              (nc-ldr-offset 1 31 xs)         ; x1 = str (tagged)
              ;; Clear tag: x1 = x1 & ~0xF (same approach as car-ir)
              (nc-movz 2 #xFFF0)              ; x2 = mask (keep upper bits)
              (nc-movk 2 #xFFFF 16)           ; complete mask
              (nc-movk 2 #xFFFF 32)
              (nc-movk 2 #xFFFF 48)
              (nc-and-reg 1 1 2)              ; x1 = str_ptr (untagged)
              ;; Load idx -> x0
              (nc-ldr-offset 0 31 is)         ; x0 = idx (tagged)
              ;; Calculate offset: x0 = (idx >> 4) + 8
              (nc-lsr-imm 0 0 4)              ; x0 = untagged idx
              (nc-add-imm 0 0 8)              ; x0 = offset = 8 + idx
              ;; Load byte from str_ptr + offset
              (nc-ldrb-reg 0 1 0)             ; x0 = byte value (zero-extended)
              ;; Tag as fixnum: x0 = x0 << 4
              (nc-lsl-imm 0 0 4)))))
    ;; system-ir - execute shell command
    ((nc-has-tag ir 'system-ir)
     ;; system-ir = (system-ir cmd-ir)
     ;; Runtime index 51 = habu_system at offset 408
     (let* ((cmd-ir (cadr ir))
            (cc (nc-codegen cmd-ir rtaddrs fnoffs td))
            (lf (nc-ldr-offset 9 19 408))
            (bl (nc-blr 9)))
       (nc-append-all (list cc lf bl))))
    ;; string-equal-ir - compare two strings (inline)
    ((nc-has-tag ir 'string-equal-ir)
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
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (s1 (nc-codegen str1-ir rtaddrs fnoffs nd))
            (sp (nc-str-offset 0 31 xs))
            (s2 (nc-codegen str2-ir rtaddrs fnoffs nd)))
       (nc-append-all
        (list s1 sp s2
              ;; x2 = str2 base (untagged)
              (nc-and-imm 2 0 1 #x3C #x3B)    ; x2 = str2 & ~0xF
              ;; x1 = str1 base (untagged)
              (nc-ldr-offset 0 31 xs)         ; x0 = str1 (tagged)
              (nc-and-imm 1 0 1 #x3C #x3B)    ; x1 = str1 & ~0xF
              ;; Load lengths
              (nc-ldr-offset 3 1 0)           ; x3 = len1
              (nc-ldr-offset 4 2 0)           ; x4 = len2
              ;; Compare lengths
              (nc-cmp-reg 3 4)                ; cmp len1, len2
              (nc-b-cond (nc-cond-ne) 56)     ; if len1 != len2, jump to return_false (+14 instructions = 56 bytes)
              ;; Lengths equal, setup for loop
              ;; x1 = str1 data = x1 + 8
              (nc-add-imm 1 1 8)              ; x1 = str1 data start
              ;; x2 = str2 data = x2 + 8
              (nc-add-imm 2 2 8)              ; x2 = str2 data start
              ;; x4 = loop counter = 0
              (nc-movz 4 0)                   ; x4 = 0
              ;; loop_start: (offset here, instruction 5)
              (nc-cmp-reg 4 3)                ; cmp counter, len
              (nc-b-cond (nc-cond-ge) 28)     ; if counter >= len, jump to return_true (+7 instructions = 28 bytes)
              ;; Load bytes from both strings
              (nc-ldrb-reg 5 1 4)             ; x5 = str1[counter]
              (nc-ldrb-reg 6 2 4)             ; x6 = str2[counter]
              ;; Compare bytes
              (nc-cmp-reg 5 6)                ; cmp char1, char2
              (nc-b-cond (nc-cond-ne) 20)     ; if char1 != char2, jump to return_false (+5 instructions = 20 bytes)
              ;; Increment counter
              (nc-add-imm 4 4 1)              ; x4++
              ;; Loop back to cmp at instruction 5
              (nc-b-offset -24)               ; back 6 instructions = -24 bytes
              ;; return_true: (instruction 13)
              (nc-movz 0 16)                  ; x0 = 16 (tagged 1)
              (nc-b-offset 8)                 ; skip return_false (+2 instructions = 8 bytes)
              ;; return_false: (instruction 15)
              (nc-movz 0 0)))))
    ;; make-vector-ir - allocate vector (inline)
    ((nc-has-tag ir 'make-vector-ir)
     ;; make-vector-ir = (make-vector-ir size-ir)
     ;; Inline allocation: size in x0 is tagged fixnum
     ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
     ;; Total size = 8 + (untagged_size * 8), rounded to 16 for tagging
     (let* ((size-ir (cadr ir))
            (sc (nc-codegen size-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list sc
              ;; Store untagged length at [x28+0]
              (nc-lsr-imm 1 0 4)           ; x1 = untagged length
              (nc-str-offset 1 28 0)       ; [x28+0] = length
              ;; Calculate allocation size: 8 + (x0 >> 1)
              (nc-lsr-imm 1 0 1)           ; x1 = x0 >> 1 = untagged_size * 8
              (nc-add-imm 1 1 8)           ; x1 = 8 + data_size = total size
              ;; Round to 16-byte alignment: (x1 + 15) & ~15
              (nc-add-imm 1 1 15)          ; x1 = total + 15
              (nc-and-imm 1 1 1 #x3C #x3B) ; x1 = x1 & ~15 (clear low 4 bits)
              ;; Return tagged pointer, bump heap
              (nc-mov-reg 0 28)            ; x0 = current heap ptr
              (nc-add-reg 28 28 1)         ; x28 += total size (now 16-aligned)
              ;; Tag with vector tag (0x3)
              (nc-movz 1 3)
              (nc-orr-reg 0 0 1)))))
    ;; vector-set-ir - set element at index (inline)
    ((nc-has-tag ir 'vector-set-ir)
     ;; vector-set-ir = (vector-set-ir vec-ir idx-ir val-ir)
     ;; Inline store: compute address and store directly
     ;; Vector layout: [length (8 bytes)] [data[0] ... data[n-1]]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4) * 8 = (vec & ~0xF) + 8 + (idx >> 1)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (val-ir (cadddr ir))
            (xs (nc-temp-slot td))
            (xs2 (nc-temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (nc-codegen vec-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 xs))
            (ic (nc-codegen idx-ir rtaddrs fnoffs nd))
            (si (nc-str-offset 0 31 xs2))
            (vlc (nc-codegen val-ir rtaddrs fnoffs nd)))
       ;; After codegen: val in x0, vec at [sp+xs], idx at [sp+xs2]
       (nc-append-all
        (list vc sv ic si vlc
              ;; x0 = val, load vec -> x1, idx -> x2
              (nc-ldr-offset 1 31 xs)         ; x1 = vec (tagged)
              (nc-ldr-offset 2 31 xs2)        ; x2 = idx (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (nc-and-imm 1 1 1 #x3C #x3B)    ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate offset: x2 = (idx >> 1) + 8
              (nc-lsr-imm 2 2 1)              ; x2 = idx >> 1 = idx_untagged * 8
              (nc-add-imm 2 2 8)              ; x2 = offset = 8 + idx_untagged * 8
              ;; Store val at vec_ptr + offset
              (nc-add-reg 1 1 2)              ; x1 = address
              (nc-str-offset 0 1 0)           ; [x1] = val
              ))))
    ;; vector-ref-ir - get element at index (inline)
    ((nc-has-tag ir 'vector-ref-ir)
     ;; vector-ref-ir = (vector-ref-ir vec-ir idx-ir)
     ;; Inline load: compute address and load directly
     ;; Vector layout: [length (8 bytes)] [data[0] ... data[n-1]]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4) * 8 = (vec & ~0xF) + 8 + (idx >> 1)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (vc (nc-codegen vec-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 xs))
            (ic (nc-codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+xs]
       (nc-append-all
        (list vc sv ic
              ;; x0 = idx, load vec -> x1
              (nc-ldr-offset 1 31 xs)         ; x1 = vec (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (nc-and-imm 1 1 1 #x3C #x3B)    ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate offset: x0 = (idx >> 1) + 8
              (nc-lsr-imm 0 0 1)              ; x0 = idx >> 1 = idx_untagged * 8
              (nc-add-imm 0 0 8)              ; x0 = offset = 8 + idx_untagged * 8
              ;; Load element from vec_ptr + offset
              (nc-add-reg 1 1 0)              ; x1 = address
              (nc-ldr-offset 0 1 0)           ; x0 = [x1] = element (already tagged)
              ))))
    ;; vector-length-ir - get vector size (inline)
    ((nc-has-tag ir 'vector-length-ir)
     ;; vector-length-ir = (vector-length-ir vec-ir)
     ;; Vector layout: [length (8 bytes)][data...]
     ;; Just load the length field and tag it
     (let* ((vec-ir (cadr ir))
            (vc (nc-codegen vec-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list vc
              ;; x0 = vec (tagged)
              ;; Clear tag: x0 = x0 & ~0xF
              (nc-and-imm 0 0 1 #x3C #x3B)    ; x0 = vec_ptr (untagged)
              ;; Load length: x0 = [x0+0]
              (nc-ldr-offset 0 0 0)           ; x0 = raw length (untagged integer)
              ;; Tag as fixnum: x0 = x0 << 4
              (nc-lsl-imm 0 0 4)))))          ; x0 = tagged fixnum length
    ;; buffer-byte-ref-ir - get raw byte at index (inline)
    ((nc-has-tag ir 'buffer-byte-ref-ir)
     ;; buffer-byte-ref-ir = (buffer-byte-ref-ir vec-ir idx-ir)
     ;; Reads a single byte from vector data area (used after sys-read fills buffer)
     ;; Vector layout: [length (8 bytes)][raw bytes...]
     ;; Address = (vec & ~0xF) + 8 + (idx >> 4)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (vc (nc-codegen vec-ir rtaddrs fnoffs nd))
            (sv (nc-str-offset 0 31 xs))
            (ic (nc-codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+xs]
       (nc-append-all
        (list vc sv ic
              ;; x0 = idx (tagged), load vec -> x1
              (nc-ldr-offset 1 31 xs)         ; x1 = vec (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (nc-and-imm 1 1 1 #x3C #x3B)    ; x1 = vec_ptr (untagged, clear low 4 bits)
              ;; Calculate byte offset: x0 = idx >> 4 (untag) + 8 (skip length)
              (nc-lsr-imm 0 0 4)              ; x0 = idx_untagged (byte offset)
              (nc-add-imm 0 0 8)              ; x0 = offset = 8 + byte_index
              ;; Load byte from vec_ptr + offset
              (nc-add-reg 1 1 0)              ; x1 = address
              (nc-ldrb-offset 0 1 0)          ; x0 = byte (zero-extended to 64-bit)
              ;; Tag as fixnum
              (nc-lsl-imm 0 0 4)              ; x0 = tagged fixnum
              ))))
    ;; make-string-from-vector-ir - convert vector to string (inline)
    ((nc-has-tag ir 'make-string-from-vector-ir)
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
            (vc (nc-codegen vec-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list vc
              ;; x1 = untagged vec base
              (nc-and-imm 1 0 1 #x3C #x3B)    ; x1 = vec & ~0xF
              ;; x5 = vec length (raw)
              (nc-ldr-offset 5 1 0)           ; x5 = [x1+0] = length
              ;; Allocate string: store length at [x28], compute alloc size
              (nc-str-offset 5 28 0)          ; [x28+0] = length
              ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
              (nc-add-imm 4 5 23)             ; x4 = len + 23 (= len + 8 + 15)
              (nc-and-imm 4 4 1 #x3C #x3B)    ; x4 = (len + 23) & ~15 (clear low 4 bits)
              ;; Save string ptr (will be result), bump heap
              (nc-mov-reg 0 28)               ; x0 = string base (untagged)
              (nc-add-reg 28 28 4)            ; x28 += alloc_size
              ;; x2 = string data base = x0 + 8
              (nc-add-imm 2 0 8)              ; x2 = string data start
              ;; x3 = loop counter = 0
              (nc-movz 3 0)                   ; x3 = 0
              ;; Loop: while x3 < x5
              ;; loop_start: (offset 0 from here)
              (nc-cmp-reg 3 5)                ; cmp x3, x5
              (nc-b-cond (nc-cond-ge) 36)     ; if x3 >= x5, jump to loop_end (+9 instructions = 36 bytes)
              ;; Load vec[x3]: address = x1 + 8 + x3*8
              (nc-lsl-imm 4 3 3)              ; x4 = x3 * 8
              (nc-add-imm 4 4 8)              ; x4 = 8 + x3*8 (offset in vec)
              (nc-add-reg 4 1 4)              ; x4 = vec_base + offset
              (nc-ldr-offset 4 4 0)           ; x4 = [x4] = tagged fixnum
              ;; Untag: x4 = x4 >> 4
              (nc-lsr-imm 4 4 4)              ; x4 = char value (untagged)
              ;; Store byte: str_data[x3] = x4
              (nc-strb-reg 4 2 3)             ; [x2 + x3] = x4 (byte)
              ;; x3++
              (nc-add-imm 3 3 1)              ; x3++
              ;; Jump back to loop_start (cmp instruction)
              (nc-b-offset -36)               ; back 9 instructions = -36 bytes
              ;; loop_end:
              ;; Tag result with string tag (0x4)
              (nc-movz 4 4)                   ; x4 = 4
              (nc-orr-reg 0 0 4)))))
    ;; buffer-to-string-ir - convert raw byte buffer to string (inline)
    ((nc-has-tag ir 'buffer-to-string-ir)
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
            (buf-slot (nc-temp-slot td))
            (nd (+ td 1))
            (buf-code (nc-codegen buf-ir rtaddrs fnoffs nd))
            (len-code (nc-codegen len-ir rtaddrs fnoffs nd)))
       (nc-append-all
        (list
         ;; Evaluate buf, save to slot
         buf-code
         (nc-str-offset 0 31 buf-slot)
         ;; Evaluate len
         len-code
         ;; x5 = length (untagged)
         (nc-lsr-imm 5 0 4)                 ; x5 = len >> 4 (untag)
         ;; x1 = buf data start (untagged buf base + 8)
         (nc-ldr-offset 1 31 buf-slot)      ; x1 = buf (tagged)
         (nc-and-imm 1 1 1 #x3C #x3B)       ; x1 = buf & ~0xF (clear tag)
         (nc-add-imm 1 1 8)                 ; x1 = buf + 8 (skip length header)
         ;; Allocate string: store length at [x28]
         (nc-str-offset 5 28 0)             ; [x28+0] = length
         ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
         (nc-add-imm 4 5 23)                ; x4 = len + 23 (= len + 8 + 15)
         (nc-and-imm 4 4 1 #x3C #x3B)       ; x4 = (len + 23) & ~15
         ;; Save string ptr (will be result), bump heap
         (nc-mov-reg 0 28)                  ; x0 = string base (untagged)
         (nc-add-reg 28 28 4)               ; x28 += alloc_size
         ;; x2 = string data base = x0 + 8
         (nc-add-imm 2 0 8)                 ; x2 = string data start
         ;; x3 = loop counter = 0
         (nc-movz 3 0)                      ; x3 = 0
         ;; Loop: while x3 < x5
         ;; loop_start: (offset 0 from here)
         (nc-cmp-reg 3 5)                   ; cmp x3, x5
         (nc-b-cond (nc-cond-ge) 24)        ; if x3 >= x5, jump to loop_end (+6 instructions = 24 bytes)
         ;; Load buf[x3] - raw byte
         (nc-add-reg 4 1 3)                 ; x4 = buf_data + x3
         (nc-ldrb-offset 4 4 0)             ; x4 = byte at [x4]
         ;; Store byte: str_data[x3] = x4
         (nc-strb-reg 4 2 3)                ; [x2 + x3] = x4 (byte)
         ;; x3++
         (nc-add-imm 3 3 1)                 ; x3++
         ;; Jump back to loop_start (cmp instruction)
         (nc-b-offset -24)                  ; back 6 instructions = -24 bytes
         ;; loop_end:
         ;; Tag result with string tag (0x4)
         (nc-movz 4 4)                      ; x4 = 4
         (nc-orr-reg 0 0 4)))))
    ;; make-symbol-from-string-ir - intern string as symbol
    ((nc-has-tag ir 'make-symbol-from-string-ir)
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
            (str-code (nc-codegen str-ir rtaddrs fnoffs (+ td 5)))
            (str-slot (nc-temp-slot td)))
       (nc-append-all
        (list
         ;; Evaluate and save input string
         str-code
         (nc-str-offset 0 31 str-slot)

         ;; Get next-id from x27[0]
         (nc-ldr-offset 3 27 0)  ; x3 = next-id (untagged)

         ;; Create (id . table) cons
         ;; id = x3 << 4 (tag as fixnum)
         (nc-lsl-imm 4 3 4)      ; x4 = id as fixnum
         ;; table = [x27+8]
         (nc-ldr-offset 5 27 8)  ; x5 = current table
         ;; Allocate cons: [x28+0] = id, [x28+8] = table
         (nc-str-offset 4 28 0)
         (nc-str-offset 5 28 8)
         ;; Tag as cons
         (nc-mov-reg 6 28)
         (nc-movz 9 1)
         (nc-orr-reg 6 6 9)      ; x6 = id-next cons
         (nc-add-imm 28 28 16)   ; bump heap

         ;; Create outer cons: (name . id-next)
         ;; name = input string
         (nc-ldr-offset 0 31 str-slot)
         (nc-str-offset 0 28 0)  ; [x28+0] = name
         (nc-str-offset 6 28 8)  ; [x28+8] = id-next cons
         ;; Tag as cons
         (nc-mov-reg 7 28)
         (nc-orr-reg 7 7 9)      ; x7 = new entry cons
         (nc-add-imm 28 28 16)   ; bump heap

         ;; Update table: x27[8] = new entry
         (nc-str-offset 7 27 8)
         ;; Increment next-id: x27[0] = x3 + 1
         (nc-add-imm 3 3 1)
         (nc-str-offset 3 27 0)

         ;; Return id as symbol: (id << 4) | 2
         ;; x4 already has id << 4 (as fixnum)
         (nc-movz 11 #xF)
         (nc-bic-reg 0 4 11)     ; clear fixnum tag
         (nc-movz 9 2)
         (nc-orr-reg 0 0 9)))))  ; tag as symbol
    ;; symbol-name-ir - get symbol's name by looking up in symbol table
    ((nc-has-tag ir 'symbol-name-ir)
     ;; symbol-name-ir = (symbol-name-ir sym-ir)
     ;; Symbol table at x27[8] is list of (name . (id . rest)) entries
     ;; Symbol value is (id << 4) | 2
     ;; Algorithm:
     ;; 1. Get symbol ID: sym >> 4 (clear tag)
     ;; 2. Walk table until find entry where (car (cdr entry)) >> 4 == id
     ;; 3. Return (car entry) (the name string)
     (let* ((sym-ir (cadr ir))
            (sym-code (nc-codegen sym-ir rtaddrs fnoffs (+ td 5))))
       (nc-append-all
        (list
         ;; Evaluate symbol
         sym-code
         ;; Get ID: x1 = sym >> 4 (already properly shifted since tag is 2)
         (nc-lsr-imm 1 0 4)           ; x1 = symbol ID (untagged)
         ;; Get table: x2 = x27[8]
         (nc-ldr-offset 2 27 8)       ; x2 = table (list of entries)
         ;; Load mask for clearing tag bits
         (nc-movz 11 #xF)             ; x11 = 0xF (tag mask)
         ;; loop:
         ;; Check if nil (x2 == 0)
         (nc-cmp-imm 2 0)
         (nc-b-cond (nc-cond-eq) 48)  ; if nil, jump to end (+12 instructions = 48 bytes)
         ;; Get entry: x2 is cons (entry . rest), untag to get pointer
         (nc-bic-reg 3 2 11)          ; x3 = entry pointer (untagged)
         ;; Get id-next: (cdr entry) = [x3+8]
         (nc-ldr-offset 4 3 8)        ; x4 = (id . rest) cons
         ;; Untag and get id: (car x4) = [x4-1] after untagging
         (nc-bic-reg 4 4 11)          ; x4 = pointer to (id . rest)
         (nc-ldr-offset 5 4 0)        ; x5 = id (as fixnum, so id << 4)
         (nc-lsr-imm 5 5 4)           ; x5 = id (untagged)
         ;; Compare: x5 == x1?
         (nc-cmp-reg 5 1)
         (nc-b-cond (nc-cond-eq) 12)  ; if match, jump to found (+3 instructions = 12 bytes)
         ;; Not match, advance: x2 = (cdr entry) = [x3+8], then cdr of that = [x4+8]
         (nc-ldr-offset 2 4 8)        ; x2 = rest of table
         (nc-b-offset -44)            ; back to loop start (11 instructions = -44 bytes)
         ;; found: return (car entry) = [x3+0] (the name string)
         (nc-ldr-offset 0 3 0)        ; x0 = name string
         ;; skip to end (past the nil case)
         (nc-b-offset 8)              ; skip past nil case (branch + movz = 8 bytes)
         ;; end (nil case): return nil
         (nc-movz 0 0)))))            ; x0 = nil
    ;; write-bytes-ir - write vector of bytes to file
    ((nc-has-tag ir 'write-bytes-ir)
     ;; write-bytes-ir = (write-bytes-ir path-ir vec-ir)
     ;; Runtime index 53 = habu_write_bytes at offset 424
     ;; Takes path string in x0, byte vector in x1
     (let* ((path-ir (cadr ir))
            (vec-ir (caddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (pc (nc-codegen path-ir rtaddrs fnoffs nd))
            (sp (nc-str-offset 0 31 xs))
            (vc (nc-codegen vec-ir rtaddrs fnoffs nd))
            (mv (nc-mov-reg 1 0))
            (lp (nc-ldr-offset 0 31 xs))
            (lf (nc-ldr-offset 9 19 424))
            (bl (nc-blr 9)))
       (nc-append-all (list pc sp vc mv lp lf bl))))
    ;; nthcdr-ir - get nth cdr of list
    ((nc-has-tag ir 'nthcdr-ir)
     ;; nthcdr-ir = (nthcdr-ir n-ir list-ir)
     ;; Loop: while n > 0 do x0 = cdr(x0), n = n - 1
     (let* ((n-ir (cadr ir))
            (list-ir (caddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (nc (nc-codegen n-ir rtaddrs fnoffs nd))
            (sn (nc-str-offset 0 31 xs))
            (lc (nc-codegen list-ir rtaddrs fnoffs nd))
            (ml (nc-mov-reg 1 0))
            (ln (nc-ldr-offset 2 31 xs))
            (asr (nc-asr-imm 2 2 4))
            (cm (nc-cmp-imm 2 0))
            (be (nc-b-cond (nc-cond-le) 28))
            (m0 (nc-mov-reg 0 1))
            (lf (nc-ldr-offset 9 19 16))
            (bl (nc-blr 9))
            (m1 (nc-mov-reg 1 0))
            (si (nc-sub-imm 2 2 1))
            (bk (nc-b-offset -20))
            (mr (nc-mov-reg 0 1)))
       (nc-append-all (list nc sn lc ml ln asr cm be m0 lf bl m1 si bk mr))))
    ;; values-ir - return multiple values
    ((nc-has-tag ir 'values-ir)
     ;; values-ir = (values-ir (ir1 ir2 ...))
     ;; Runtime index 17 = habu_values_set at offset 136
     ;; habu_values_set(count, v0, v1, v2, v3)
     (let* ((irs (cadr ir)))
       (if (null irs)
           ;; No values - call values_set(0, 0, 0, 0, 0)
           (nc-append-all
            (list (nc-movz 0 0)
                  (nc-movz 1 0)
                  (nc-movz 2 0)
                  (nc-movz 3 0)
                  (nc-movz 4 0)
                  (nc-ldr-offset 9 19 136)
                  (nc-blr 9)))
           (if (null (cdr irs))
               ;; Single value - just return it
               (nc-codegen (car irs) rtaddrs fnoffs td)
               ;; Multiple values - evaluate all and call values_set
               (let* ((cnt (length irs))
                      (xs (nc-temp-slot td))
                      (nd (+ td 4)))
                 (labels ((eval-vals (vs idx acc)
                            (if (null vs)
                                acc
                                (let* ((vc (nc-codegen (car vs) rtaddrs fnoffs nd))
                                       (slot (nc-temp-slot (+ td idx)))
                                       (sv (nc-str-offset 0 31 slot)))
                                  (eval-vals (cdr vs) (+ idx 1)
                                             (nc-append-all (list acc vc sv)))))))
                   (let* ((evc (eval-vals irs 0 nil))
                          (l0 (if (> cnt 0) (nc-ldr-offset 1 31 (nc-temp-slot td)) (nc-movz 1 0)))
                          (l1 (if (> cnt 1) (nc-ldr-offset 2 31 (nc-temp-slot (+ td 1))) (nc-movz 2 0)))
                          (l2 (if (> cnt 2) (nc-ldr-offset 3 31 (nc-temp-slot (+ td 2))) (nc-movz 3 0)))
                          (l3 (if (> cnt 3) (nc-ldr-offset 4 31 (nc-temp-slot (+ td 3))) (nc-movz 4 0)))
                          (ct (ash cnt 4))
                          (mc (nc-movz 0 ct))
                          (lf (nc-ldr-offset 9 19 136))
                          (bl (nc-blr 9))
                          (lv (nc-ldr-offset 0 31 (nc-temp-slot td))))
                     (nc-append-all (list evc l0 l1 l2 l3 mc lf bl lv)))))))))
    ;; mvb-ir - multiple-value-bind
    ((nc-has-tag ir 'mvb-ir)
     ;; mvb-ir = (mvb-ir vars form-ir nvars body-ir)
     ;; Runtime index 18 = habu_values_get at offset 144
     ;; habu_values_get(index, primary) returns value at index
     ;; Values must be stored in env frame (x20-based) so body VAR refs work
     (let* ((vars (cadr ir))
            (form-ir (caddr ir))
            (nvars (cadddr ir))
            (body-ir (nth 4 ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (fc (nc-codegen form-ir rtaddrs fnoffs nd))
            (sp (nc-str-offset 0 31 xs)))
       ;; Evaluate form, save primary, then get each value and store in env frame
       (labels ((bind-vars (idx acc)
                  (if (>= idx nvars)
                      acc
                      ;; habu_values_get expects untagged index (0, 1, 2, ...)
                      (let* ((mi (nc-movz 0 idx))
                             (lp (nc-ldr-offset 1 31 xs))
                             (lf (nc-ldr-offset 9 19 144))
                             (bl (nc-blr 9))
                             ;; Store in env frame: sub x1, x20, offset; str x0, [x1]
                             (env-off (* idx 8))
                             (s1 (nc-sub-imm 1 20 env-off))
                             (sv (nc-str-offset 0 1 0)))
                        (bind-vars (+ idx 1)
                                   (nc-append-all (list acc mi lp lf bl s1 sv)))))))
         (let* ((bc (bind-vars 0 nil))
                (body-code (nc-codegen body-ir rtaddrs fnoffs nd)))
           (nc-append-all (list fc sp bc body-code))))))
    ((nc-has-tag ir 'if-ir)
     (let ((test-ir (cadr ir)))
       (let ((then-ir (caddr ir)))
         (let ((else-ir (cadddr ir)))
           (let ((tc (nc-codegen test-ir rtaddrs fnoffs td)))
             (let ((thc (nc-codegen then-ir rtaddrs fnoffs td)))
               (let ((elc (nc-codegen else-ir rtaddrs fnoffs td)))
                 ;; Use nc-code-size to correctly account for :call-fn markers
                 ;; Layout: B.EQ | then-code | B | else-code
                 ;; B.EQ skips to else-code: then-code + B (4) + self (4) = then_bytes + 8
                 ;; B skips past else-code: else-code + self (4) = else_bytes + 4
                 (let ((then-bytes (nc-code-size thc)))
                   (let ((else-bytes (nc-code-size elc)))
                     (nc-append-all
                      (list tc
                            (nc-movz 1 0)
                            (nc-cmp-reg 0 1)
                            (nc-b-cond (nc-cond-eq) (+ then-bytes 8))  ; Skip then + B + self
                            thc
                            (nc-b-offset (+ else-bytes 4))  ; Skip else + landing
                            elc)))))))))))
    ((nc-has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir)))  ;; offs is at index 3
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (acc (nc-str-offset 24 31 xs)))
       (labels ((gb (vs os a)
                  (if (null vs) a
                      (let* ((vc (nc-codegen (car vs) rtaddrs fnoffs nd))
                             (s1 (nc-sub-imm 1 20 (* (car os) 8)))
                             (s2 (nc-str-offset 0 1 0))
                             (st (append s1 s2))
                             (ld (nc-ldr-offset 24 31 xs))
                             (t1 (append a ld))
                             (t2 (append t1 vc))
                             (t3 (append t2 st)))
                        (gb (cdr vs) (cdr os) t3)))))
         (let* ((body-code (gb vals offs nil))
                (final-ld (nc-ldr-offset 24 31 xs))
                (bc (nc-codegen bir rtaddrs fnoffs nd))
                (r1 (append acc body-code))
                (r2 (append r1 final-ld)))
           (append r2 bc)))))
    ((nc-has-tag ir 'call-fn)
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
            (xs (nc-temp-slot td))
            (nd (+ td 1)))
       (labels ((ga (as i a)
                  ;; Evaluate all args to spill slots
                  (if (null as) a
                      (let* ((rs (if (> i 0) (nc-ldr-offset 24 31 xs) nil))
                             (ac (nc-codegen (car as) rtaddrs fnoffs nd))
                             (st (nc-str-offset 0 31 (nc-spill-slot td i)))
                             (t1 (append a rs))
                             (t2 (append t1 ac))
                             (t3 (append t2 st)))
                        (ga (cdr as) (+ i 1) t3))))
                (gl-reg (i a)
                  ;; Load args 0-7 into registers x0-x7
                  ;; After alloc-stack, sp moved down by stack-space, so adjust offset
                  (if (>= i (min na 8)) a
                      (let* ((adjusted-off (+ (nc-spill-slot td i) stack-space))
                             (ld (nc-ldr-offset i 31 adjusted-off))
                             (t1 (append a ld)))
                        (gl-reg (+ i 1) t1))))
                (store-stack-args (i a)
                  ;; Store args 8+ to stack: arg i goes to [sp + (i-8)*8]
                  ;; After alloc-stack, sp moved down by stack-space, so adjust offset
                  (if (>= i na) a
                      (let* ((adjusted-off (+ (nc-spill-slot td i) stack-space))
                             (ld (nc-ldr-offset 0 31 adjusted-off))
                             (stack-off (* (- i 8) 8))
                             (st (nc-str-offset 0 31 stack-off))
                             (t1 (append a ld))
                             (t2 (append t1 st)))
                        (store-stack-args (+ i 1) t2)))))
         (let* ((save-x24 (nc-str-offset 24 31 xs))
                (args-code (ga airs 0 nil))
                (restore-x24 (nc-ldr-offset 24 31 xs))
                ;; Allocate stack space for args 8+ (if any)
                (alloc-stack (if (> stack-args 0)
                                 (nc-sub-imm 31 31 stack-space)
                                 nil))
                ;; Store args 8+ to stack
                (stack-code (store-stack-args 8 nil))
                ;; Load args 0-7 into registers
                (load-args (gl-reg 0 nil))
                (set-argc (nc-movz 23 na))
                ;; Emit special marker instead of BL: (:call-fn name)
                ;; This will be resolved to actual BL in nc-resolve-calls
                (call-marker (list (list :call-fn fnm)))
                ;; Deallocate stack space after call returns
                (dealloc-stack (if (> stack-args 0)
                                   (nc-add-imm 31 31 stack-space)
                                   nil)))
           (nc-append-all (list save-x24 args-code restore-x24
                                alloc-stack stack-code load-args
                                set-argc call-marker dealloc-stack))))))
    ((nc-has-tag ir 'tail-call-fn)
     ;; Tail call optimization: evaluate args, run epilogue, then jump (B) instead of call (BL)
     ;; The callee will set up its own frame, so we tear down ours first
     ;; NOTE: Tail calls currently limited to 8 args (x0-x7) because epilogue deallocates
     ;; our frame before we can set up stack args. >8 args requires saving to callee-saved
     ;; registers or converting to regular call.
     (let* ((fnm (cadr ir))
            (airs (caddr ir))
            (na (length airs))
            (xs (nc-temp-slot td))
            (nd (+ td 1)))
       (labels ((ga (as i a)
                  (if (null as) a
                      (let* ((rs (if (> i 0) (nc-ldr-offset 24 31 xs) nil))
                             (ac (nc-codegen (car as) rtaddrs fnoffs nd))
                             (st (nc-str-offset 0 31 (nc-spill-slot td i)))
                             (t1 (append a rs))
                             (t2 (append t1 ac))
                             (t3 (append t2 st)))
                        (ga (cdr as) (+ i 1) t3))))
                (gl-reg (i a)
                  ;; Only load args 0-7 into registers for tail calls
                  (if (>= i (min na 8)) a
                      (let* ((ld (nc-ldr-offset i 31 (nc-spill-slot td i)))
                             (t1 (append a ld)))
                        (gl-reg (+ i 1) t1)))))
         (let* ((save-x24 (nc-str-offset 24 31 xs))
                (args-code (ga airs 0 nil))
                (restore-x24 (nc-ldr-offset 24 31 xs))
                (load-args (gl-reg 0 nil))
                (set-argc (nc-movz 23 na))
                ;; Run epilogue to restore caller's registers and pop our frame
                ;; Use conservative max frame size for tail calls (actual size set by caller)
                (epilogue (nc-fn-epilogue #x2000))
                ;; Emit tail call marker (resolved to B instead of BL)
                (call-marker (list (list :tail-call-fn fnm))))
           (append save-x24 args-code restore-x24 load-args set-argc epilogue call-marker)))))
    ((nc-has-tag ir 'loop-ir)
     ;; loop-ir = (loop-ir body-ir)
     ;; Generate loop marker followed by body code
     ;; The marker records position for continue-ir to jump back to
     (let ((body-ir (cadr ir)))
       (append (list (list :loop-start))
               (nc-codegen body-ir rtaddrs fnoffs td))))
    ((nc-has-tag ir 'continue-ir)
     ;; continue-ir = (continue-ir (new-arg-ir ...))
     ;; Evaluate new args to temp slots, copy to param slots, jump back to loop start
     ;; Note: We must evaluate ALL args before storing ANY to handle (f (- n 1) (+ acc n))
     (let* ((new-args-ir (cadr ir))
            (nargs (length new-args-ir))
            (xs (nc-temp-slot td))
            (nd (+ td 1)))
       ;; Generate code to evaluate all new args and store to temp slots
       (labels ((eval-args (args idx acc)
                  (if (null args)
                      acc
                      (let* ((arg-code (nc-codegen (car args) rtaddrs fnoffs nd))
                             (store (nc-str-offset 0 31 (nc-spill-slot td idx))))
                        (eval-args (cdr args) (+ idx 1) (append acc arg-code store)))))
                (copy-to-params (idx acc)
                  ;; Copy from temp slots to param slots (offsets 0, 8, 16, ...)
                  (if (>= idx nargs)
                      acc
                      (let* ((load (nc-ldr-offset 0 31 (nc-spill-slot td idx)))
                             (param-addr (nc-sub-imm 1 20 (* idx 8)))
                             (store (nc-str-offset 0 1 0)))
                        (copy-to-params (+ idx 1) (append acc load param-addr store))))))
         (let* ((save-x24 (nc-str-offset 24 31 xs))
                (eval-code (eval-args new-args-ir 0 nil))
                (restore-x24 (nc-ldr-offset 24 31 xs))
                (copy-code (copy-to-params 0 nil))
                (jump-marker (list (list :loop-continue))))
           (append save-x24 eval-code restore-x24 copy-code jump-marker)))))
    ((nc-has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     ;; Generate code for each form, keep result of last
     (let ((forms-ir (cadr ir)))
       (labels ((gen-seq (fs acc)
                  (if (null fs)
                      acc
                      (let ((fc (nc-codegen (car fs) rtaddrs fnoffs td)))
                        (gen-seq (cdr fs) (append acc fc))))))
         (gen-seq forms-ir nil))))
    ((nc-has-tag ir 'div)
     ;; Division: both operands untagged, divide, re-tag
     (nc-codegen-binop (cadr ir) (caddr ir)
                       (nc-append-all (list (nc-lsr-imm 0 0 4)
                                            (nc-lsr-imm 1 1 4)
                                            (nc-sdiv-reg 0 0 1)
                                            (nc-lsl-imm 0 0 4)))
                       rtaddrs fnoffs td))
    ((or (nc-has-tag ir 'mod) (nc-has-tag ir 'mod-ir))
     ;; Modulo: a mod b = a - (a / b) * b
     (nc-codegen-binop (cadr ir) (caddr ir)
                       (nc-append-all (list (nc-lsr-imm 0 0 4)
                                            (nc-lsr-imm 1 1 4)
                                            (nc-sdiv-reg 2 0 1)
                                            (nc-mul-reg 2 2 1)
                                            (nc-sub-reg 0 0 2)
                                            (nc-lsl-imm 0 0 4)))
                       rtaddrs fnoffs td))
    ((nc-has-tag ir 'lambda-ir)
     ;; lambda-ir should be lifted to lambda-ref before codegen
     ;; If we encounter it directly, it's an error - return 0
     (nc-movz 0 0))
    ((nc-has-tag ir 'lambda-ref)
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
            (offset-bytes fn-offset)
            (code-slot (nc-temp-slot td))
            (env-slot (nc-temp-slot (+ td 1))))
       (if (= capture-count 0)
           ;; No captures - inline closure cons: (fn-offset . nil)
           ;; Store fn-offset (tagged fixnum) in car, nil in cdr
           ;; Allocate 16 bytes on heap (x28 = bump pointer)
           (let ((tagged-offset (ash offset-bytes 4)))  ; tag as fixnum
             (nc-append-all
              (list
               ;; Store fn-offset (tagged) in [x28]
               ;; Use nc-load-addr-32 to ensure consistent size during two-pass compilation
               (nc-load-addr-32 9 tagged-offset)   ; x9 = tagged offset
               (nc-str-offset 9 28 0)           ; [x28+0] = car = fn-offset
               ;; Store nil in [x28+8]
               (nc-movz 10 0)                   ; x10 = nil
               (nc-str-offset 10 28 8)          ; [x28+8] = cdr = nil
               ;; Result = x28 | 5 (closure tag)
               (nc-mov-reg 0 28)                ; x0 = x28
               (nc-movz 9 5)                    ; x9 = closure tag
               (nc-orr-reg 0 0 9)                   ; x0 = x28 | 5
               ;; Bump heap pointer by 16
               (nc-add-imm 28 28 16))))
           ;; Has captures - build env as cons list, then make closure cons
           ;; First build env cons list (capture-count cells)
           ;; Then allocate closure cons
           (let ((capture-code
                  (labels ((build-captures (offs acc env-acc)
                             (if (null offs)
                                 (list acc env-acc)  ; return (code . result-slot)
                                 (let* ((off (car offs))
                                        (val-slot (nc-temp-slot (+ td 2 (* 2 (length offs)))))
                                        (pair-slot (nc-temp-slot (+ td 3 (* 2 (length offs)))))
                                        ;; Load captured value
                                        (load-cap
                                         (nc-append-all
                                          (list
                                           (nc-sub-imm 1 20 (* off 8)) ; x1 = &captured
                                           (nc-ldr-offset 0 1 0)       ; x0 = captured value
                                           (nc-str-offset 0 31 val-slot)))) ; save value
                                        ;; Allocate cons: (value . prev-env)
                                        (alloc-cons
                                         (nc-append-all
                                          (list
                                           (nc-ldr-offset 9 31 val-slot)  ; car = captured value
                                           (nc-str-offset 9 28 0)         ; [x28+0] = car
                                           ;; cdr = previous env acc
                                           (if (null env-acc)
                                               (nc-movz 9 0)              ; first: cdr = nil
                                               (nc-ldr-offset 9 31 env-acc)) ; else: load prev env
                                           (nc-str-offset 9 28 8)         ; [x28+8] = cdr
                                           ;; Result = x28 | 1 (cons tag)
                                           (nc-mov-reg 0 28)
                                           (nc-movz 9 1)
                                           (nc-orr-reg 0 0 9)                 ; x0 = cons ptr
                                           ;; Save and bump
                                           (nc-str-offset 0 31 pair-slot)
                                           (nc-add-imm 28 28 16)))))
                                   (build-captures (cdr offs)
                                                   (nc-append-all (list acc load-cap alloc-cons))
                                                   pair-slot)))))
                    ;; Reverse free-offsets so first captured var ends up at car of env list
                   ;; This matches nc-gen-capture-copies which stores car at slot 0, etc.
                   (build-captures (reverse free-offsets) nil nil))))
             (let* ((env-code (car capture-code))
                    (env-result-slot (cadr capture-code))
                    (tagged-offset (ash offset-bytes 4)))
               (nc-append-all
                (list
                 ;; Build env cons list
                 env-code
                 ;; Now allocate closure cons: (fn-offset . env)
                 ;; Use nc-load-addr-32 to ensure consistent size during two-pass compilation
                 (nc-load-addr-32 9 tagged-offset)     ; car = fn-offset (tagged)
                 (nc-str-offset 9 28 0)             ; [x28+0] = car
                 (nc-ldr-offset 9 31 env-result-slot) ; cdr = env cons list
                 (nc-str-offset 9 28 8)             ; [x28+8] = cdr
                 ;; Result = x28 | 5 (closure tag)
                 (nc-mov-reg 0 28)
                 (nc-movz 9 5)
                 (nc-orr-reg 0 0 9)
                 ;; Bump heap
                 (nc-add-imm 28 28 16))))))))
    ((nc-has-tag ir 'funcall-ir)
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
            ;; Temp slots: 0=x24-save, 1=x20-save, 2=x30-save, 3=closure-addr, 4=code-addr, 5=env, 6..6+n-1=args
            (x24-slot (nc-temp-slot td))
            (x20-slot (nc-temp-slot (+ td 1)))
            (x30-slot (nc-temp-slot (+ td 2)))  ; Save LR - lambdas make BL calls!
            (closure-slot (nc-temp-slot (+ td 3)))
            (code-slot (nc-temp-slot (+ td 4)))
            (env-slot (nc-temp-slot (+ td 5)))
            (arg-base (+ td 6))
            (nested-td (+ arg-base num-args))
            ;; Evaluate function
            (fn-code (nc-codegen fn-ir rtaddrs fnoffs nested-td)))
       (labels ((gen-args (airs idx acc)
                  (if (null airs)
                      acc
                      (let* ((rs (if (> idx 0) (nc-ldr-offset 24 31 x24-slot) nil))
                             (ac (nc-codegen (car airs) rtaddrs fnoffs nested-td))
                             (st (nc-str-offset 0 31 (nc-temp-slot (+ arg-base idx)))))
                        (gen-args (cdr airs) (+ idx 1)
                                  (nc-append-all (list acc rs ac st))))))
                (load-reg-args (idx total-offset acc)
                  ;; Load args 0-7 into registers x0-x7
                  ;; After alloc-stack + param-frame, sp moved down by total-offset, so adjust
                  (if (>= idx (min num-args 8))
                      acc
                      (let* ((adjusted-off (+ (nc-temp-slot (+ arg-base idx)) total-offset))
                             (ld (nc-ldr-offset idx 31 adjusted-off)))
                        (load-reg-args (+ idx 1) total-offset (append acc ld)))))
                (store-stack-args (idx total-offset acc)
                  ;; Store args 8+ to stack: arg i goes to [sp + (i-8)*8]
                  ;; After alloc-stack + param-frame, sp moved down by total-offset, so adjust
                  (if (>= idx num-args)
                      acc
                      (let* ((adjusted-off (+ (nc-temp-slot (+ arg-base idx)) total-offset))
                             (ld (nc-ldr-offset 0 31 adjusted-off))
                             (stack-off (* (- idx 8) 8))
                             (st (nc-str-offset 0 31 stack-off)))
                        (store-stack-args (+ idx 1) total-offset (nc-append-all (list acc ld st)))))))
         (let ((total-offset (+ stack-space param-space)))
           (nc-append-all
            (list
             ;; Save x24 and x20
             (nc-str-offset 24 31 x24-slot)
             (nc-str-offset 20 31 x20-slot)
             ;; Evaluate closure into x0
             fn-code
             ;; Clear closure tag (5) to get heap address: x9 = x0 & ~0xF
             (nc-movz 11 #xF)                     ; x11 = 0xF
             (nc-bic-reg 9 0 11)                  ; x9 = x0 & ~0xF
             ;; Load car = fn-offset (tagged): x10 = [x9+0]
             (nc-ldr-offset 10 9 0)
             ;; Untag fn-offset: x10 = x10 >> 4
             (nc-lsr-imm 10 10 4)
             ;; Compute code address: x10 = x26 + x10 (code_base + offset)
             (nc-add-reg 10 26 10)
             (nc-str-offset 10 31 code-slot)      ; save code address
             ;; Load cdr = env: x11 = [x9+8]
             (nc-ldr-offset 11 9 8)
             (nc-str-offset 11 31 env-slot)       ; save env
             ;; Restore x24 for arg evaluation
             (nc-ldr-offset 24 31 x24-slot)
             ;; Evaluate args
             (gen-args args-ir 0 nil)
             ;; Allocate stack space for args 8+ (if any)
             (if (> stack-args 0)
                 (nc-sub-imm 31 31 stack-space)
                 nil)
             ;; Allocate parameter frame for lambda
             (nc-sub-imm 31 31 param-space)
             ;; Set x20 for lambda's param-stores: x20 = sp + param-space - 8
             (if (> param-space 8)
                 (nc-add-imm 20 31 (- param-space 8))
                 (nc-mov-reg 20 31))  ; If param-space <= 8, set x20 = sp
             ;; Store args 8+ to stack (they're above the param frame)
             (store-stack-args 8 total-offset nil)
             ;; Load args 0-7 into registers
             (load-reg-args 0 total-offset nil)
             ;; Set x24 to callee's env
             (nc-ldr-offset 24 31 (+ env-slot total-offset))
             ;; Set argc
             (nc-movz 23 num-args)
             ;; BUG #20 FIX: Save x30 - lambdas have no prologue, make BL calls!
             ;; CRITICAL: x30 saved AFTER sp modified, so must adjust offset!
             (nc-str-offset 30 31 (+ x30-slot total-offset))
             ;; Load code address and call
             (nc-ldr-offset 9 31 (+ code-slot total-offset))
             (nc-blr 9)
             ;; Restore x30 immediately after lambda returns
             ;; CRITICAL: sp still modified, so must adjust offset!
             (nc-ldr-offset 30 31 (+ x30-slot total-offset))
             ;; Deallocate parameter frame
             (nc-add-imm 31 31 param-space)
             ;; Deallocate stack space for args 8+ (if any)
             (if (> stack-args 0)
                 (nc-add-imm 31 31 stack-space)
                 nil)
             ;; Restore x24 and x20
             (nc-ldr-offset 24 31 x24-slot)
             (nc-ldr-offset 20 31 x20-slot)))))))
    ((nc-has-tag ir 'dotimes-ir)
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
            (count-slot (nc-temp-slot td))
            (counter-slot (nc-temp-slot (+ td 1)))
            (x24-slot (nc-temp-slot (+ td 2)))
            (body-td (+ td 3))
            ;; Compile count expression
            (count-code (nc-codegen count-ir rtaddrs fnoffs body-td))
            ;; Calculate var offset from extended env
            (new-env (nc-env-extend (list (list var)) compile-env))
            (var-offset (* (nc-env-lookup var new-env) 8))
            ;; Codegen the already-compiled body and result
            (body-code (nc-codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (nc-count-instrs body-code))
            (result-code (nc-codegen result-ir rtaddrs fnoffs body-td)))
       (nc-append-all
        (list
         ;; Save x24
         (nc-str-offset 24 31 x24-slot)
         ;; Evaluate and save count
         count-code
         (nc-str-offset 0 31 count-slot)
         ;; Initialize counter to 0
         (nc-movz 0 0)
         (nc-str-offset 0 31 counter-slot)
         ;; Loop start: load counter and count, compare
         ;; Loop test: 4 instrs (ldr counter, ldr count, cmp, b.ge)
         (nc-ldr-offset 0 31 counter-slot)
         (nc-ldr-offset 1 31 count-slot)
         (nc-cmp-reg 0 1)
         ;; Branch past body + incr + loop-back if counter >= count
         ;; Body instrs + store var (4) + incr (4) + branch back (1) = body-instrs + 9
         (nc-b-cond (nc-cond-ge) (* (+ body-instrs 9) 4))
         ;; Store counter as var at its actual offset from x20
         (nc-ldr-offset 0 31 counter-slot)
         (nc-sub-imm 1 20 var-offset)
         (nc-str-offset 0 1 0)
         ;; Restore x24 for body
         (nc-ldr-offset 24 31 x24-slot)
         ;; Execute body
         body-code
         ;; Increment counter
         (nc-ldr-offset 0 31 counter-slot)
         (nc-add-imm 0 0 #x10)  ; add tagged 1
         (nc-str-offset 0 31 counter-slot)
         ;; Branch back to loop start
         ;; Distance: -(loop test (4) + store var (4) + body + incr (3))
         (nc-b-offset (- (* (+ body-instrs 11) 4)))
         ;; After loop: evaluate result with final counter
         (nc-ldr-offset 0 31 counter-slot)
         (nc-sub-imm 1 20 var-offset)
         (nc-str-offset 0 1 0)
         (nc-ldr-offset 24 31 x24-slot)
         result-code))))
    ((nc-has-tag ir 'dolist-ir)
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
            (list-slot (nc-temp-slot td))
            (x24-slot (nc-temp-slot (+ td 1)))
            (body-td (+ td 2))
            ;; Compile list expression
            (list-code (nc-codegen list-ir rtaddrs fnoffs body-td))
            ;; Calculate var offset from extended env
            (new-env (nc-env-extend (list (list var)) compile-env))
            (var-offset (* (nc-env-lookup var new-env) 8))
            ;; Codegen the already-compiled body and result
            (body-code (nc-codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (nc-count-instrs body-code))
            (result-code (nc-codegen result-ir rtaddrs fnoffs body-td)))
       (nc-append-all
        (list
         ;; Save x24
         (nc-str-offset 24 31 x24-slot)
         ;; Evaluate and save list
         list-code
         (nc-str-offset 0 31 list-slot)
         ;; Loop start: check if list is nil (tag 0)
         (nc-ldr-offset 0 31 list-slot)
         (nc-movz 1 0)  ; nil = 0
         (nc-cmp-reg 0 1)
         ;; Branch past body if list is nil
         ;; Body: store var (4) + body + get cdr (4) + branch (1) = body-instrs + 9
         (nc-b-cond (nc-cond-eq) (* (+ body-instrs 9) 4))
         ;; Get car of list -> var at its actual offset
         (nc-ldr-offset 0 31 list-slot)
         (nc-ldr-offset 9 19 8)  ; car function at offset 8
         (nc-blr 9)
         (nc-sub-imm 1 20 var-offset)
         (nc-str-offset 0 1 0)
         ;; Restore x24 for body
         (nc-ldr-offset 24 31 x24-slot)
         ;; Execute body
         body-code
         ;; Get cdr, save as new list
         (nc-ldr-offset 0 31 list-slot)
         (nc-ldr-offset 9 19 16)  ; cdr function at offset 16
         (nc-blr 9)
         (nc-str-offset 0 31 list-slot)
         ;; Branch back to loop start
         ;; Distance: -(null check (3) + get car (5) + body + get cdr (4))
         (nc-b-offset (- (* (+ body-instrs 12) 4)))
         ;; After loop: evaluate result (var is nil at this point)
         (nc-movz 0 0)  ; nil
         (nc-sub-imm 1 20 var-offset)
         (nc-str-offset 0 1 0)
         (nc-ldr-offset 24 31 x24-slot)
         result-code))))
    ;; === libSystem call IR forms (for native executables) ===
    ;; These emit :extern-call markers that are resolved by deliver-with-libsystem
    ((nc-has-tag ir 'sys-write-ir)
     ;; sys-write-ir = (sys-write-ir fd-ir buf-ir len-ir)
     ;; Calls _write(fd, buf, len) -> returns bytes written (or -1)
     ;; Args: fd in x0, buf (string ptr) in x1, len in x2
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 3))
            ;; Evaluate fd
            (fd-code (nc-codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (nc-str-offset 0 31 (nc-temp-slot td)))
            ;; Evaluate buf
            (buf-code (nc-codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (nc-str-offset 0 31 (nc-temp-slot (+ td 1))))
            ;; Evaluate len
            (len-code (nc-codegen len-ir rtaddrs fnoffs nd))
            (save-len (nc-str-offset 0 31 (nc-temp-slot (+ td 2)))))
       (nc-append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              ;; Load args: fd->x0, buf->x1, len->x2
              (nc-ldr-offset 0 31 (nc-temp-slot td))
              (nc-lsr-imm 0 0 4)                      ; untag fd
              (nc-ldr-offset 1 31 (nc-temp-slot (+ td 1)))
              (nc-and-imm 1 1 1 #x3C #x3B)            ; clear string tag, get ptr
              (nc-add-imm 1 1 8)                      ; skip length field
              (nc-ldr-offset 2 31 (nc-temp-slot (+ td 2)))
              (nc-lsr-imm 2 2 4)                      ; untag len
              ;; Emit extern call marker
              (list (list :extern-call "_write"))
              ;; Tag result as fixnum
              (nc-lsl-imm 0 0 4)))))
    ((nc-has-tag ir 'sys-read-ir)
     ;; sys-read-ir = (sys-read-ir fd-ir buf-ir len-ir)
     ;; Calls _read(fd, buf, len) -> returns bytes read (or-1)
     ;; buf should be a vector
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 3))
            (fd-code (nc-codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (nc-str-offset 0 31 (nc-temp-slot td)))
            (buf-code (nc-codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (nc-str-offset 0 31 (nc-temp-slot (+ td 1))))
            (len-code (nc-codegen len-ir rtaddrs fnoffs nd))
            (save-len (nc-str-offset 0 31 (nc-temp-slot (+ td 2)))))
       (nc-append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (nc-ldr-offset 0 31 (nc-temp-slot td))
              (nc-lsr-imm 0 0 4)                      ; untag fd
              (nc-ldr-offset 1 31 (nc-temp-slot (+ td 1)))
              (nc-and-imm 1 1 1 #x3C #x3B)            ; clear vector tag
              (nc-add-imm 1 1 8)                      ; skip length field
              (nc-ldr-offset 2 31 (nc-temp-slot (+ td 2)))
              (nc-lsr-imm 2 2 4)                      ; untag len
              (list (list :extern-call "_read"))
              (nc-lsl-imm 0 0 4)))))
    ((nc-has-tag ir 'sys-open-ir)
     ;; sys-open-ir = (sys-open-ir path-ir flags-ir mode-ir)
     ;; Calls _open(path, flags, mode) -> returns fd (or -1)
     (let* ((path-ir (cadr ir))
            (flags-ir (caddr ir))
            (mode-ir (cadddr ir))
            (xs (nc-temp-slot td))
            (nd (+ td 3))
            (path-code (nc-codegen path-ir rtaddrs fnoffs nd))
            (save-path (nc-str-offset 0 31 (nc-temp-slot td)))
            (flags-code (nc-codegen flags-ir rtaddrs fnoffs nd))
            (save-flags (nc-str-offset 0 31 (nc-temp-slot (+ td 1))))
            (mode-code (nc-codegen mode-ir rtaddrs fnoffs nd))
            (save-mode (nc-str-offset 0 31 (nc-temp-slot (+ td 2)))))
       (nc-append-all
        (list path-code save-path flags-code save-flags mode-code save-mode
              (nc-ldr-offset 0 31 (nc-temp-slot td))
              (nc-and-imm 0 0 1 #x3C #x3B)            ; clear string tag
              (nc-add-imm 0 0 8)                      ; skip length field
              (nc-ldr-offset 1 31 (nc-temp-slot (+ td 1)))
              (nc-lsr-imm 1 1 4)                      ; untag flags
              (nc-ldr-offset 2 31 (nc-temp-slot (+ td 2)))
              (nc-lsr-imm 2 2 4)                      ; untag mode
              (list (list :extern-call "_open"))
              (nc-lsl-imm 0 0 4)))))
    ((nc-has-tag ir 'sys-close-ir)
     ;; sys-close-ir = (sys-close-ir fd-ir)
     ;; Calls _close(fd) -> returns 0 on success
     (let* ((fd-ir (cadr ir))
            (fd-code (nc-codegen fd-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list fd-code
              (nc-lsr-imm 0 0 4)                      ; untag fd
              (list (list :extern-call "_close"))
              (nc-lsl-imm 0 0 4)))))
    ((nc-has-tag ir 'sys-exit-ir)
     ;; sys-exit-ir = (sys-exit-ir code-ir)
     ;; Calls _exit(code) -> does not return
     (let* ((code-ir (cadr ir))
            (code-code (nc-codegen code-ir rtaddrs fnoffs td)))
       (nc-append-all
        (list code-code
              (nc-lsr-imm 0 0 4)                      ; untag exit code
              (list (list :extern-call "_exit"))))))
    (t (nc-movz 0 0))))

;;; ============================================================
;;; Part 8: Multi-Function Compiler
;;; ============================================================

(defun nc-compile-defun (name params body env fenv)
  (let* ((bs (mapcar (lambda (p) (list p)) params))
         (penv (nc-env-extend bs env))
         (pb (if params (nc-env-lookup (car params) penv) 0))
         (rfenv (cons (cons name nil) fenv))
         (bir (nc-compile body penv rfenv)))
    (list name params bir pb)))

;; Two-pass compilation for mutual recursion support
;; Pass 1: Collect all defun names into fenv with placeholder entries
;; Pass 2: Compile function bodies with complete fenv

(defun nc-collect-defun-names (forms acc)
  "Pass 1: Collect all defun names from forms, recursing into progn"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (nc-collect-defun-names (cdr forms) (cons (list (cadr f)) acc)))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (nc-collect-defun-names (cdr forms)
                                   (nc-collect-defun-names (cdr f) acc)))
          (t (nc-collect-defun-names (cdr forms) acc))))))

(defun nc-compile-defuns (forms env fenv acc)
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
                  (cf (nc-compile-defun nm ps bd env fenv)))
             (nc-compile-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           ;; Recurse into progn body, then continue with rest
           (nc-compile-defuns (cdr forms) env fenv
                              (nc-compile-defuns (cdr f) env fenv acc)))
          (t (nc-compile-defuns (cdr forms) env fenv acc))))))

(defun nc-find-main-form (forms)
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

(defun nc-compile-forms (forms)
  "Two-pass compilation: first collect names, then compile with complete fenv"
  ;; Pass 1: Collect all defun names
  (let* ((fn-names (nc-collect-defun-names forms nil))
         ;; Build fenv with all function names as placeholders
         (fenv fn-names))
    ;; Pass 2: Compile all defuns with complete fenv
    (let* ((compiled-fns (reverse (nc-compile-defuns forms nil fenv nil)))
           ;; Find and compile the main expression
           (main-form (nc-find-main-form forms))
           (main-ir (if main-form (nc-compile main-form nil fenv) (list 'lit 0))))
      (list compiled-fns main-ir))))

(defun nc-gen-param-stores (params base idx acc &key leaf)
  "Store function parameters to stack frame.
   Args 0-7 come from registers x0-x7.
   Args 8+ come from caller's stack at [sp + frame_size + (i-8)*8].
   Frame size is 0x200 for leaf functions, 0x400 for non-leaf."
  (if (null params)
      acc
      (let* ((frame-size (if leaf #x1000 #x1000))  ; Must match nc-fn-prologue - now 4KB for all functions
             (st (if (< idx 8)
                     ;; Args 0-7: copy from register xi to stack
                     (append (nc-mov-reg 22 idx)
                             (nc-sub-imm 21 20 (* (+ base idx) 8))
                             (nc-str-offset 22 21 0))
                     ;; Args 8+: load from caller's stack, store to our env frame
                     ;; Caller's stack args are at [sp + frame_size + (i-8)*8]
                     (let ((stack-off (+ frame-size (* (- idx 8) 8))))
                       (append (nc-ldr-offset 22 31 stack-off)
                               (nc-sub-imm 21 20 (* (+ base idx) 8))
                               (nc-str-offset 22 21 0))))))
        (nc-gen-param-stores (cdr params) base (+ idx 1) (append acc st) :leaf leaf))))

(defun nc-fn-prologue (frame-size x20-offset &key leaf)
  "Function prologue: allocate frame, save caller's x20/lr/x24, set up new env base.
   Frame size and x20 offset are dynamically calculated based on function needs.
   x24 must be preserved across calls so defuns with internal labels don't clobber
   the caller's closure environment.
   If :leaf t, skip x24 save (leaf functions don't call other functions)."
  (if leaf
      ;; Leaf function: skip x24 save
      (append
       (nc-sub-imm 31 31 frame-size)   ; SUB sp, sp, #frame-size
       (nc-stp-offset 20 30 31 0)      ; STP x20, lr, [sp, #0] (save x20 and return addr)
       (nc-add-imm 20 31 x20-offset))  ; ADD x20, sp, #x20-offset (env base)
      ;; Non-leaf function: full frame with x24 save
      (append
       (nc-sub-imm 31 31 frame-size)   ; SUB sp, sp, #frame-size (allocate function frame)
       (nc-stp-offset 20 30 31 0)      ; STP x20, lr, [sp, #0] (save caller's x20 and return addr)
       (nc-str-offset 24 31 16)        ; STR x24, [sp, #16] (save caller's closure env)
       (nc-add-imm 20 31 x20-offset)))) ; ADD x20, sp, #x20-offset (env base past spill area)

(defun nc-fn-epilogue (frame-size &key leaf)
  "Function epilogue: restore caller's x20/lr/x24, deallocate frame, return
   If :leaf t, skip x24 restore."
  (if leaf
      ;; Leaf function: skip x24 restore
      (append
       (nc-ldp-offset 20 30 31 0)    ; LDP x20, lr, [sp, #0] (restore x20 and lr)
       (nc-add-imm 31 31 frame-size))  ; ADD sp, sp, #frame-size (deallocate leaf frame)
      ;; Non-leaf function: full restore
      (append
       (nc-ldr-offset 24 31 16)       ; LDR x24, [sp, #16] (restore caller's closure env)
       (nc-ldp-offset 20 30 31 0)     ; LDP x20, lr, [sp, #0] (restore caller's x20 and lr)
       (nc-add-imm 31 31 frame-size)))) ; ADD sp, sp, #frame-size (deallocate function frame)

(defun nc-gen-capture-copies (count idx acc)
  "Generate code to copy captured values from closure env (x24) to stack.
   x24 points to a cons list of captured values: (val1 . (val2 . nil)).
   We traverse the list extracting car values and storing to stack slots.
   After all copies, x24 should be nil."
  (if (>= idx count)
      acc
      (let* ((copy-code
              (nc-append-all
               (list
                ;; x24 is current cons cell (tagged with 1)
                ;; Clear cons tag: x9 = x24 & ~0xF
                (nc-movz 11 #xF)
                (nc-bic-reg 9 24 11)
                ;; Get car (the captured value): x0 = [x9+0]
                (nc-ldr-offset 0 9 0)
                ;; Store result to stack slot idx
                (nc-sub-imm 21 20 (* idx 8))
                (nc-str-offset 0 21 0)
                ;; Move x24 to cdr (next cons cell): x24 = [x9+8]
                (nc-ldr-offset 24 9 8)))))
        (nc-gen-capture-copies count (+ idx 1) (append acc copy-code)))))

(defun nc-save-params-to-temps (count idx acc)
  "Save param registers x0..xN to temp slots 200+idx to preserve them during capture copy.
   Temp slots 200+ are used to avoid conflict with body temps."
  (if (>= idx count)
      acc
      (let* ((temp-slot (+ 200 idx))
             (off (* temp-slot 8))
             (save-code (nc-append-all
                         (list
                          (nc-sub-imm 21 20 off)
                          (nc-str-offset idx 21 0)))))
        (nc-save-params-to-temps count (+ idx 1) (append acc save-code)))))

(defun nc-restore-params-from-temps (params base count idx acc)
  "Restore params from temp slots and store to final slots at base+idx."
  (if (null params)
      acc
      (let* ((temp-slot (+ 200 idx))
             (temp-off (* temp-slot 8))
             (final-off (* (+ base idx) 8))
             (restore-code (nc-append-all
                            (list
                             ;; Load from temp slot
                             (nc-sub-imm 21 20 temp-off)
                             (nc-ldr-offset 22 21 0)
                             ;; Store to final slot
                             (nc-sub-imm 21 20 final-off)
                             (nc-str-offset 22 21 0)))))
        (nc-restore-params-from-temps (cdr params) base count (+ idx 1) (append acc restore-code)))))

(defun nc-count-max-env-offset (ir)
  "Count the maximum environment offset used in IR (for let bindings).
   This is needed to check if leaf optimization is safe."
  (cond
    ((null ir) 0)
    ((not (consp ir)) 0)
    ;; Skip alist pairs like (CODE . 0) or (FNOFFS . 1)
    ((and (consp ir) (atom (cdr ir))) 0)
    ((nc-has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count (offs...))
     ;; The offs list contains the offsets used
     (let* ((offs (nth 3 (cdr ir)))
            (max-off (if offs (apply #'max offs) 0))
            (body-max (nc-count-max-env-offset (caddr ir))))
       (max max-off body-max)))
    ((nc-has-tag ir 'if-ir)
     (max (nc-count-max-env-offset (cadr ir))
          (nc-count-max-env-offset (caddr ir))
          (nc-count-max-env-offset (cadddr ir))))
    ((nc-has-tag ir 'progn-ir)
     (apply #'max 0 (mapcar #'nc-count-max-env-offset (cadr ir))))
    ((nc-has-tag ir 'dolist-ir)
     ;; dolist-ir has body at (cadddr ir)
     (nc-count-max-env-offset (cadddr ir)))
    (t
     ;; Check children for other IR nodes, filtering out non-list elements
     (apply #'max 0 (mapcar #'nc-count-max-env-offset
                            (remove-if-not #'consp (cdr ir)))))))

(defun nc-count-max-temp-depth (ir depth)
  "Count the maximum temp depth reached during codegen of IR.
   Temp depth increases during nested expression evaluation."
  (cond
    ((null ir) depth)
    ((not (consp ir)) depth)
    ;; Skip alist pairs like (CODE . 0) or (FNOFFS . 1)
    ((and (consp ir) (atom (cdr ir))) depth)
    ;; Literals and vars don't use temps
    ((or (nc-has-tag ir 'lit) (nc-has-tag ir 'var-ref)) depth)
    ;; Binary ops: depth increases by amount needed for saving x24 + operands
    ((or (nc-has-tag ir 'add-ir) (nc-has-tag ir 'sub-ir) (nc-has-tag ir 'mul-ir)
         (nc-has-tag ir 'div-ir) (nc-has-tag ir 'mod-ir) (nc-has-tag ir 'cons-ir)
         (nc-has-tag ir 'cmp-eq) (nc-has-tag ir 'cmp-lt) (nc-has-tag ir 'cmp-gt)
         (nc-has-tag ir 'cmp-le) (nc-has-tag ir 'cmp-ge))
     (let* ((left-depth (nc-count-max-temp-depth (cadr ir) (+ depth 2)))
            (right-depth (nc-count-max-temp-depth (caddr ir) (+ depth 2))))
       (max left-depth right-depth)))
    ;; Let bindings: each binding uses temps, body uses temps
    ((nc-has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (val-depths (mapcar (lambda (v) (nc-count-max-temp-depth v (+ depth 2))) vals))
            (body-depth (nc-count-max-temp-depth bir (+ depth 2))))
       (apply #'max body-depth val-depths)))
    ;; If: all branches
    ((nc-has-tag ir 'if-ir)
     (max (nc-count-max-temp-depth (cadr ir) (+ depth 1))
          (nc-count-max-temp-depth (caddr ir) depth)
          (nc-count-max-temp-depth (cadddr ir) depth)))
    ;; Progn: all forms
    ((nc-has-tag ir 'progn-ir)
     (apply #'max depth (mapcar (lambda (f) (nc-count-max-temp-depth f depth)) (cadr ir))))
    ;; Dolist: check body
    ((nc-has-tag ir 'dolist-ir)
     (nc-count-max-temp-depth (cadddr ir) depth))
    ;; Function calls: args + closure env
    ((nc-has-tag ir 'call-fn)
     (let* ((args (caddr ir))
            (arg-depths (mapcar (lambda (a) (nc-count-max-temp-depth a (+ depth 3))) args)))
       (apply #'max (+ depth 3) arg-depths)))
    ((nc-has-tag ir 'funcall-ir)
     (let* ((closure (cadr ir))
            (args (caddr ir))
            (closure-depth (nc-count-max-temp-depth closure (+ depth 2)))
            (arg-depths (mapcar (lambda (a) (nc-count-max-temp-depth a (+ depth 4))) args)))
       (apply #'max closure-depth (+ depth 4) arg-depths)))
    ;; Default: check all children, filtering out non-list elements
    (t
     (apply #'max depth (mapcar (lambda (child) (nc-count-max-temp-depth child depth))
                                (remove-if-not #'consp (cdr ir)))))))

(defun nc-codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function (defun or lifted lambda).
   Defun format:  (name params body param-base)  ; param-base is a number
   Lambda format: (name params body free-vars free-offsets)  ; free-vars is a list or nil
   Uses dynamically-sized stack frames based on variable count and temp depth."
  (let* ((ps (cadr fn))
         (bir (caddr fn))
         (fourth (cadddr fn))
         ;; Calculate frame requirements
         (num-params (length ps))
         (max-let-offset (nc-count-max-env-offset bir))
         (max-env-size (max num-params (1+ max-let-offset)))
         (max-temp-depth (nc-count-max-temp-depth bir 0))
         ;; Calculate dynamic frame size
         ;; Layout: [saved regs+padding: 64] [temps: temp_depth*8] [env: env_size*8] [safety: 64]
         ;; Note: nc-temp-slot uses base #x40 (64), so saved regs area is 64 bytes
         (saved-regs 64)
         (temp-area (* (+ max-temp-depth 8) 8))  ; +8 for safety margin
         (env-area (* (+ max-env-size 8) 8))     ; +8 for safety margin
         (frame-size-raw (+ saved-regs temp-area env-area 64))
         ;; Round up to 16-byte alignment
         (frame-size (logand (+ frame-size-raw 15) (lognot 15)))
         ;; x20 offset = saved regs + temp area
         (x20-offset (+ saved-regs temp-area))
         ;; Leaf optimization: only for non-calling functions with no >8 params
         (is-leaf (and (not (nc-ir-may-call? bir))
                       (<= num-params 8))))
    ;; Distinguish defun from lambda by checking 4th element
    ;; Defuns have a number (param-base), lambdas have nil or a list (free-vars)
    (if (numberp fourth)
        ;; Defun: params start at param-base
        (let* ((pb fourth)
               (pc (nc-gen-param-stores ps pb 0 nil :leaf is-leaf))
               (bc (nc-codegen bir rtaddrs fnoffs 0)))
          (append (nc-fn-prologue frame-size x20-offset :leaf is-leaf)
                  pc bc
                  (nc-fn-epilogue frame-size :leaf is-leaf)
                  (nc-ret)))
        ;; Lambda: need to copy captures AND store params
        ;; Problem: capture copy clobbers x0-x4, but params are in x0-x4
        ;; Solution: save params to temp slots first, copy captures, then restore params
        ;; Note: Lambdas with captures cannot be leaf-optimized (capture copy uses x24)
        (let* ((free-vars fourth)
               (capture-count (if free-vars (length free-vars) 0))
               (param-count (length ps))
               ;; Save params to temp slots before they get clobbered
               (ps-save (if (> capture-count 0)
                            (nc-save-params-to-temps param-count 0 nil)
                            nil))
               ;; Copy captured values from x24 (closure env) to stack slots 0..N-1
               (cc (nc-gen-capture-copies capture-count 0 nil))
               ;; Restore params from temp slots to final slots N..N+M-1
               ;; Leaf optimize only if no captures (captures need x24)
               (leaf-ok (and is-leaf (= capture-count 0)))
               (pc (if (> capture-count 0)
                       (nc-restore-params-from-temps ps capture-count param-count 0 nil)
                       (nc-gen-param-stores ps 0 0 nil :leaf leaf-ok)))
               (bc (nc-codegen bir rtaddrs fnoffs 0)))
          (append (nc-fn-prologue frame-size x20-offset :leaf leaf-ok)
                  ps-save cc pc bc
                  (nc-fn-epilogue frame-size :leaf leaf-ok)
                  (nc-ret))))))

(defun nc-codegen-main (mir rtaddrs)
  (append (nc-prologue)
          (nc-codegen mir rtaddrs nil 0)
          (nc-epilogue)))

(defparameter *lambda-counter* 0)

(defun nc-gensym-lambda ()
  "Generate unique lambda name"
  (incf *lambda-counter*)
  (intern (sys:string-concat "LAMBDA-" (sys:number-to-string *lambda-counter*))))

(defun nc-lift-lambdas (ir)
  "Extract all lambda-ir nodes from IR, replacing them with lambda-ref nodes.
   Returns (values transformed-ir lambdas) where lambdas is alist of (name . lambda-ir)"
  (labels ((lift (ir lambdas)
             (cond
               ((null ir) (values ir lambdas))
               ((not (consp ir)) (values ir lambdas))
               ((nc-has-tag ir 'lambda-ir)
                ;; Found a lambda - give it a name, store it, return reference
                (let* ((name (nc-gensym-lambda))
                       (params (cadr ir))
                       (body (caddr ir))
                       (free-vars (cadddr ir))
                       (free-offsets (nth 4 ir)))
                  ;; Recursively lift lambdas from the body
                  (multiple-value-bind (new-body more-lambdas)
                      (lift body lambdas)
                    (let ((lambda-entry (list name params new-body free-vars free-offsets)))
                      (values (list 'lambda-ref name free-offsets)
                              (cons lambda-entry more-lambdas))))))
               ((nc-has-tag ir 'let-ir)
                ;; let-ir = (let-ir vals bir count offs)
                (let ((vals (cadr ir))
                      (bir (caddr ir))
                      (count (cadddr ir))
                      (offs (nth 4 ir)))
                  (multiple-value-bind (new-vals lambdas1)
                      (lift-list vals lambdas)
                    (multiple-value-bind (new-bir lambdas2)
                        (lift bir lambdas1)
                      (values (list 'let-ir new-vals new-bir count offs) lambdas2)))))
               ((nc-has-tag ir 'if-ir)
                (let ((test (cadr ir))
                      (then (caddr ir))
                      (else (cadddr ir)))
                  (multiple-value-bind (new-test l1) (lift test lambdas)
                    (multiple-value-bind (new-then l2) (lift then l1)
                      (multiple-value-bind (new-else l3) (lift else l2)
                        (values (list 'if-ir new-test new-then new-else) l3))))))
               ((nc-has-tag ir 'progn-ir)
                (multiple-value-bind (new-forms new-lambdas)
                    (lift-list (cadr ir) lambdas)
                  (values (list 'progn-ir new-forms) new-lambdas)))
               ((nc-has-tag ir 'funcall-ir)
                (let ((fn-ir (cadr ir))
                      (args-ir (caddr ir)))
                  (multiple-value-bind (new-fn l1) (lift fn-ir lambdas)
                    (multiple-value-bind (new-args l2) (lift-list args-ir l1)
                      (values (list 'funcall-ir new-fn new-args) l2)))))
               ((nc-has-tag ir 'call-fn)
                (let ((name (cadr ir))
                      (args-ir (caddr ir)))
                  (multiple-value-bind (new-args new-lambdas)
                      (lift-list args-ir lambdas)
                    (values (list 'call-fn name new-args) new-lambdas))))
               ((nc-has-tag ir 'tail-call-fn)
                (let ((name (cadr ir))
                      (args-ir (caddr ir)))
                  (multiple-value-bind (new-args new-lambdas)
                      (lift-list args-ir lambdas)
                    (values (list 'tail-call-fn name new-args) new-lambdas))))
               ((or (nc-has-tag ir 'add) (nc-has-tag ir 'sub)
                    (nc-has-tag ir 'mul) (nc-has-tag ir 'div)
                    (nc-has-tag ir 'mod) (nc-has-tag ir 'cmp-eq)
                    (nc-has-tag ir 'cmp-lt) (nc-has-tag ir 'cmp-gt)
                    (nc-has-tag ir 'cmp-le) (nc-has-tag ir 'cmp-ge)
                    (nc-has-tag ir 'cons-ir)
                    (nc-has-tag ir 'band) (nc-has-tag ir 'bor)
                    (nc-has-tag ir 'bxor) (nc-has-tag ir 'bsh))
                (let ((left (cadr ir))
                      (right (caddr ir)))
                  (multiple-value-bind (new-left l1) (lift left lambdas)
                    (multiple-value-bind (new-right l2) (lift right l1)
                      (values (list (car ir) new-left new-right) l2)))))
               ((or (nc-has-tag ir 'car-ir) (nc-has-tag ir 'cdr-ir))
                (multiple-value-bind (new-arg new-lambdas)
                    (lift (cadr ir) lambdas)
                  (values (list (car ir) new-arg) new-lambdas)))
               ((nc-has-tag ir 'setq-ir)
                ;; setq-ir = (setq-ir offset value-ir)
                (let ((offset (cadr ir))
                      (val-ir (caddr ir)))
                  (multiple-value-bind (new-val new-lambdas)
                      (lift val-ir lambdas)
                    (values (list 'setq-ir offset new-val) new-lambdas))))
               ((nc-has-tag ir 'dotimes-ir)
                ;; dotimes-ir = (dotimes-ir var count-ir body-ir result-ir compile-env)
                (let ((var (cadr ir))
                      (count-ir (caddr ir))
                      (body-ir (cadddr ir))
                      (result-ir (nth 4 ir))
                      (compile-env (nth 5 ir)))
                  (multiple-value-bind (new-count l1) (lift count-ir lambdas)
                    (multiple-value-bind (new-body l2) (lift body-ir l1)
                      (multiple-value-bind (new-result l3) (lift result-ir l2)
                        (values (list 'dotimes-ir var new-count new-body new-result compile-env) l3))))))
               ((nc-has-tag ir 'dolist-ir)
                ;; dolist-ir = (dolist-ir var list-ir body-ir result-ir compile-env)
                (let ((var (cadr ir))
                      (list-ir (caddr ir))
                      (body-ir (cadddr ir))
                      (result-ir (nth 4 ir))
                      (compile-env (nth 5 ir)))
                  (multiple-value-bind (new-list l1) (lift list-ir lambdas)
                    (multiple-value-bind (new-body l2) (lift body-ir l1)
                      (multiple-value-bind (new-result l3) (lift result-ir l2)
                        (values (list 'dolist-ir var new-list new-body new-result compile-env) l3))))))
               ;; 3-arg IR nodes: (tag arg1 arg2 arg3)
               ((nc-has-tag ir 'vector-set-ir)
                (let ((vec-ir (cadr ir))
                      (idx-ir (caddr ir))
                      (val-ir (cadddr ir)))
                  (multiple-value-bind (new-vec l1) (lift vec-ir lambdas)
                    (multiple-value-bind (new-idx l2) (lift idx-ir l1)
                      (multiple-value-bind (new-val l3) (lift val-ir l2)
                        (values (list 'vector-set-ir new-vec new-idx new-val) l3))))))
               ;; 2-arg IR nodes: (tag arg1 arg2)
               ((or (nc-has-tag ir 'vector-ref-ir)
                    (nc-has-tag ir 'buffer-byte-ref-ir)
                    (nc-has-tag ir 'buffer-to-string-ir)
                    (nc-has-tag ir 'string-ref-ir)
                    (nc-has-tag ir 'string-equal-ir))
                (let ((arg1 (cadr ir))
                      (arg2 (caddr ir)))
                  (multiple-value-bind (new-arg1 l1) (lift arg1 lambdas)
                    (multiple-value-bind (new-arg2 l2) (lift arg2 l1)
                      (values (list (car ir) new-arg1 new-arg2) l2)))))
               ;; 1-arg IR nodes: (tag arg)
               ((or (nc-has-tag ir 'make-vector-ir)
                    (nc-has-tag ir 'make-string-from-vector-ir)
                    (nc-has-tag ir 'make-symbol-from-string-ir)
                    (nc-has-tag ir 'symbol-name-ir)
                    (nc-has-tag ir 'string-length-ir)
                    (nc-has-tag ir 'vector-length-ir)
                    (nc-has-tag ir 'system-ir)
                    (nc-has-tag ir 'null-ir) (nc-has-tag ir 'consp-ir)
                    (nc-has-tag ir 'symbolp-ir) (nc-has-tag ir 'stringp-ir)
                    (nc-has-tag ir 'vectorp-ir) (nc-has-tag ir 'numberp-ir))
                (multiple-value-bind (new-arg new-lambdas)
                    (lift (cadr ir) lambdas)
                  (values (list (car ir) new-arg) new-lambdas)))
               ;; Self-TCO loop constructs
               ((nc-has-tag ir 'loop-ir)
                (multiple-value-bind (new-body new-lambdas)
                    (lift (cadr ir) lambdas)
                  (values (list 'loop-ir new-body) new-lambdas)))
               ((nc-has-tag ir 'continue-ir)
                (multiple-value-bind (new-args new-lambdas)
                    (lift-list (cadr ir) lambdas)
                  (values (list 'continue-ir new-args) new-lambdas)))
               (t (values ir lambdas))))
           (lift-list (irs lambdas)
             (if (null irs)
                 (values nil lambdas)
                 (multiple-value-bind (new-first l1) (lift (car irs) lambdas)
                   (multiple-value-bind (new-rest l2) (lift-list (cdr irs) l1)
                     (values (cons new-first new-rest) l2))))))
    (lift ir nil)))

(defun nc-codegen-lambda (lambda-entry rtaddrs fnoffs)
  "Generate code for a lifted lambda.
   lambda-entry = (name params body free-vars free-offsets)"
  (let* ((params (cadr lambda-entry))
         (body (caddr lambda-entry))
         ;; Lambda params start at offset 0
         (pb 0)
         (pc (nc-gen-param-stores params pb 0 nil))
         (bc (nc-codegen body rtaddrs fnoffs 0)))
    (append pc bc (nc-ret))))

(defun nc-code-size (code)
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

(defun nc-build-fnoffs (fns offset acc)
  "Build function offset table from list of compiled functions.
   Returns alist of (name . byte-offset).
   Each function's code is generated with temporary nil fnoffs to get size."
  (if (null fns)
      (reverse acc)
      (let* ((fn (car fns))
             (name (car fn))
             ;; Generate code to calculate size (will regenerate with correct fnoffs later)
             (code (nc-codegen-fn fn nil nil))
             ;; Use nc-code-size to handle markers
             (size (nc-code-size code))
             (entry (cons name offset)))
        (nc-build-fnoffs (cdr fns) (+ offset size) (cons entry acc)))))

(defun nc-codegen-all-fns (fns rtaddrs fnoffs acc)
  "Generate code for all functions with correct fnoffs."
  (if (null fns)
      acc
      (let* ((fn (car fns))
             (code (nc-codegen-fn fn rtaddrs fnoffs)))
        (nc-codegen-all-fns (cdr fns) rtaddrs fnoffs (append acc code)))))

(defun nc-lift-lambdas-from-fns (fns acc-fns acc-lambdas)
  "Lift lambdas from all function bodies.
   Returns (values lifted-fns all-lambdas) where:
   - lifted-fns has lambda-ir replaced with lambda-ref in bodies
   - all-lambdas is list of all lifted lambda definitions"
  (if (null fns)
      (values (reverse acc-fns) acc-lambdas)
      (let* ((fn (car fns))
             (name (car fn))
             (params (cadr fn))
             (body (caddr fn))
             (fourth (cadddr fn)))
        (multiple-value-bind (new-body lambdas)
            (nc-lift-lambdas body)
          (let ((new-fn (list name params new-body fourth)))
            (nc-lift-lambdas-from-fns (cdr fns)
                                      (cons new-fn acc-fns)
                                      (append acc-lambdas lambdas)))))))

(defun nc-compile-program (forms rtaddrs &key (optimize t))
  "Compile forms to bytecode with function linking.
   Layout: prologue + main-code + epilogue + functions + lifted-lambdas
   Functions are placed after main, and call-fn generates forward BL.
   When :optimize is t, runs nanopass optimization pipeline."
  ;; Reset symbol table for fresh compilation
  (nc-reset-symbol-table)
  (let* ((r (nc-compile-forms forms))
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
    (multiple-value-bind (mir main-lambdas)
        (nc-lift-lambdas mir-opt)
      ;; Lift lambdas from all defun bodies (use optimized defuns)
      (multiple-value-bind (lifted-defuns defun-lambdas)
          (nc-lift-lambdas-from-fns defun-fns-opt nil nil)
        ;; Combine: defuns + main-lambdas + defun-lambdas
        (let ((fns (append lifted-defuns main-lambdas defun-lambdas)))
          (if (null fns)
              ;; No functions defined - simple case
              ;; Still need to resolve extern calls
              (nc-resolve-calls (nc-codegen-main mir rtaddrs) nil)
              ;; Functions defined - need linking
              (let* (;; First, generate main code with nil fnoffs to get size
                     ;; This code contains (:call-fn name) markers
                     (main-code-temp (append (nc-prologue)
                                             (nc-codegen mir rtaddrs nil 0)
                                             (nc-epilogue)))
                     ;; Use nc-code-size to handle markers
                     (main-size (nc-code-size main-code-temp))
                     ;; Build fnoffs starting after main code
                     (fnoffs (nc-build-fnoffs fns main-size nil))
                     ;; Generate main code again - markers remain, fnoffs now known
                     (main-code (append (nc-prologue)
                                        (nc-codegen mir rtaddrs fnoffs 0)
                                        (nc-epilogue)))
                     ;; Generate function code with fnoffs (functions can call each other)
                     (fn-code (nc-codegen-all-fns fns rtaddrs fnoffs nil))
                     ;; Combine all code (still has markers)
                     (all-code (append main-code fn-code)))
                ;; Resolve all markers to actual BL instructions
                (nc-resolve-calls all-code fnoffs))))))))

;;; ============================================================
;;; Part 9: Entry Point
;;; ============================================================

(defun nc-eval-forms (forms)
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
                 (values fenv (reverse acc))
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (let* ((nm (cadr f))
                              (ps (caddr f))
                              (bd (cadddr f))
                              (cf (nc-compile-defun nm ps bd nil fenv))
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
                 (let* ((ir (nc-compile (car fs) nil fenv))
                        (result (nc-eval-ir-with-fns ir nil fenv)))
                   (if (null (cdr fs))
                       result
                       (eval-forms (cdr fs) fenv))))))
    ;; Execute two-pass compilation
    (let* ((defun-names (collect-defuns forms nil))
           (initial-fenv (build-fenv defun-names nil)))
      (multiple-value-bind (final-fenv other-forms)
          (compile-defuns forms initial-fenv nil)
        (eval-forms other-forms final-fenv)))))

;;; ============================================================
;;; Part 9: Public API
;;; ============================================================

;;; Public API wrappers (exported from HABU package)
;;; These provide clean names for external use: habu:deliver, etc.

(defun read-all (source-string)
  "Parse SOURCE-STRING and return list of forms.
   Usage: (habu:read-all \"(+ 1 2)\")"
  (nc-read-all source-string))

(defun compile-program (forms &optional fenv)
  "Compile FORMS to ARM64 bytecode.
   Usage: (habu:compile-program (habu:read-all source))"
  (nc-compile-program forms fenv))

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

(defun deliver (source-string output-path)
  "Compile SOURCE-STRING to standalone executable at OUTPUT-PATH.
   Usage: (habu:deliver \"(+ 1 2)\" \"/tmp/test\")"
  (let* ((c-source-path (format nil "~A.c" output-path))
         (forms (nc-read-all source-string))
         (bytes (nc-compile-program forms nil)))
    (format t "Compiling ~A bytes of ARM64 code...~%" (length bytes))
    (let ((c-source (generate-embedded-c bytes (pathname-name (pathname output-path)))))
      (with-open-file (out c-source-path :direction :output :if-exists :supersede)
        (write-string c-source out)))
    (let* ((cmd (format nil "clang -O2 -o ~A ~A runtime/gc.c runtime/io.c runtime/runtime.c runtime/region.c -I. 2>&1"
                        output-path c-source-path))
           (result (with-output-to-string (s)
                     (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output s :error :output))))
      (when (> (length result) 0)
        (format t "~A~%" result))
      (if (probe-file output-path)
          (progn
            (delete-file c-source-path)
            (format t "Created: ~A~%" output-path)
            output-path)
          (error "Compilation failed")))))

(defun deliver-file (source-path output-path)
  "Compile Lisp file at SOURCE-PATH to executable at OUTPUT-PATH.
   Usage: (habu:deliver-file \"program.lisp\" \"program\")"
  (let ((source (with-open-file (in source-path :direction :input)
                  (let ((contents (make-string (file-length in))))
                    (read-sequence contents in)
                    contents))))
    (deliver source output-path)))

;;; Backward compatibility aliases (nc-* versions)
(setf (symbol-function 'nc-deliver) #'deliver)
(setf (symbol-function 'nc-deliver-file) #'deliver-file)

;;; ============================================================
;;; Native Delivery with libSystem (no runtime dependency)
;;; ============================================================

(defun deliver-with-libsystem (source-string output-path &key verbose)
  "Compile SOURCE-STRING to native executable using libSystem for I/O.
   This creates a standalone executable that dynamically links to libSystem.B.dylib
   for functions like write, read, open, close, exit.

   Usage: (habu:deliver-with-libsystem \"(sys-write 1 \\\"Hi\\\" 2)\" \"/tmp/test\")

   The source can use sys-write, sys-read, sys-open, sys-close, sys-exit."
  ;; Load the pure-Habu macho linker if not already loaded
  (unless (fboundp 'wrap-bytecode-with-heap-for-imports)
    (load (merge-pathnames "bootstrap/macho.lisp"
                           (or *load-pathname* *default-pathname-defaults*))))

  (let* ((forms (nc-read-all source-string))
         ;; Compile to bytecode with extern markers
         (bytes-with-markers (nc-compile-program forms nil))
         ;; Collect extern calls and get unique imports
         (extern-calls (nc-collect-extern-calls bytes-with-markers))
         (imports (nc-get-unique-imports extern-calls))
         ;; Wrapper stub size in bytes (17 instructions * 4 bytes)
         ;; Note: wrap-bytecode-with-heap-for-imports has 17 instructions
         (wrapper-size 68))

    (when verbose
      (princ "Compiled ") (princ (length bytes-with-markers)) (princ " bytes (with markers)") (terpri)
      (princ "External calls: ") (princ extern-calls) (terpri)
      (princ "Imports: ") (princ imports) (terpri))

    ;; Always use the imports path for consistent Mach-O structure
    ;; The no-imports path has issues with large programs (SIGKILL)
    ;; If no imports, add _exit as a dummy import (never called but ensures proper structure)
    (let ((imports (if (null imports) '("_exit") imports)))
      (when (and verbose (null (nc-get-unique-imports extern-calls)))
        (princ "No imports detected - adding _exit for consistent Mach-O structure") (terpri))

      ;; Calculate stub offsets BEFORE flattening so we can emit correct BL instructions
      ;; This eliminates the need for post-processing file patching
      (let* ((num-imports (length imports))
             (stubs-total (if (> num-imports 0) (* num-imports 12) 0))
             (code-offset #x400)  ; header + commands + padding
             ;; Calculate exact flattened code size:
             ;; Each :extern-call marker becomes exactly 4 bytes (BL instruction)
             ;; All other items are already bytes
             (num-markers (count-if (lambda (x) (and (consp x) (eq (car x) :extern-call))) bytes-with-markers))
             (non-marker-bytes (remove-if (lambda (x) (and (consp x) (eq (car x) :extern-call))) bytes-with-markers))
             (exact-flat-size (+ (length non-marker-bytes) (* num-markers 4)))
             (exact-code-size (+ exact-flat-size wrapper-size))
             (stubs-offset (+ code-offset exact-code-size))
             (stub-size 12))

        ;; Build stub offset map: import-name -> stub-file-offset
        (let ((stub-map (make-hash-table :test 'equal)))
          (labels ((build-stub-map (remaining-imports i)
                     (when remaining-imports
                       (setf (gethash (car remaining-imports) stub-map) (+ stubs-offset (* i stub-size)))
                       (build-stub-map (cdr remaining-imports) (+ i 1)))))
            (build-stub-map imports 0))

          ;; Flatten with correct BL instructions (no post-processing needed!)
          ;; code-base-addr is where the wrapped code starts (after wrapper stub)
          (multiple-value-bind (flat-code extern-positions)
              (nc-flatten-extern-calls bytes-with-markers stub-map (+ code-offset wrapper-size))
            (when verbose
              (princ "Flattened code: ") (princ (length flat-code)) (princ " bytes") (terpri)
              (princ "Extern positions: ") (princ extern-positions) (terpri))

            ;; Wrap code with heap initialization
            ;; Calculate heap page offset dynamically based on code size
            ;; Layout: code -> stubs -> __DATA_CONST (16KB) -> __DATA (heap)
            ;; Note: macOS ARM64 uses 16KB pages (#x4000), but ADRP uses 4KB page units (#x1000)
            ;; heap-page-offset = (text-vmsize / 4KB) + (data-const-pages-4kb / 4KB)
            (let* ((total-size (+ (length flat-code) wrapper-size))
                   (stubs-end (+ code-offset total-size stubs-total))
                   ;; Linker aligns to 16KB pages (macOS ARM64 page size)
                   (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
                   ;; Convert to 4KB units for ADRP
                   (text-pages-4kb (/ text-vmsize #x1000))
                   ;; DATA_CONST is one 16KB page = 4 ADRP pages
                   (data-const-pages-4kb (/ #x4000 #x1000))
                   (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
                   (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

              ;; Create executable with imports and heap using pure-Habu linker
              ;; BL instructions are already correct - no post-processing needed!
              (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x100000)

              ;; Codesign the executable (macOS requirement)
              ;; Only on macOS and when sb-ext is available
              #+sbcl
              (when (probe-file "/usr/bin/codesign")
                (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" output-path)))

              (when verbose
                (terpri) (princ "Created: ") (princ output-path) (terpri))
              output-path)))))))

(defun deliver-file-with-libsystem (source-path output-path &key verbose)
  "Compile Lisp file to native executable using libSystem.
   Usage: (habu:deliver-file-with-libsystem \"program.lisp\" \"program\")"
  (let ((source
         #+sbcl
         (with-open-file (in source-path :direction :input)
           (let ((contents (make-string (file-length in))))
             (read-sequence contents in)
             contents))
         #-sbcl
         ;; When running as native code, can't use SBCL's with-open-file
         ;; This path would only be taken if the compiled compiler is running natively
         ;; For now, this is a placeholder - native compilation will inline native-read-file-large
         (error "deliver-file-with-libsystem requires SBCL for file I/O. Use compiled version with native-read-file-large.")))
    (deliver-with-libsystem source output-path :verbose verbose)))

;;; Export new functions
(export '(deliver-with-libsystem deliver-file-with-libsystem) :habu)

;;; ============================================================
;;; Part 10: Disassembler
;;; ============================================================

(defun disassemble-arm64-instr (word addr)
  "Disassemble a single ARM64 instruction to string."
  (let* ((op (ash word -24))
         (op2 (logand (ash word -21) #x7)))
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
              (is-sub (= (logand word #x40000000) #x40000000)))
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
      (t (format nil ".word 0x~8,'0X" word)))))

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
                  (values (nc-compile body-form env nil) name params)))
               ;; Simple expression
               (t (nc-compile form nil nil))))
         (bytecode (nc-codegen ir nil nil 0))
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
       (nc-compile body-form env nil)))
    (t (nc-compile form nil nil))))

(export '(habu-disassemble habu-compile disassemble-form disassemble-bytecode
          disassemble-arm64-instr) :habu)

;;; ============================================================
;;; Main entry point (for testing)
;;; ============================================================

(defun main ()
  ;; Full pipeline: parse -> compile to IR -> evaluate IR
  (let* ((src "(+ (* 3 4) 5)")
         (forms (nc-read-all src)))
    (if (consp forms)
        (nc-eval-forms forms)
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
