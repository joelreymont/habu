;;;; SBCL-only loader stubs for Habu codegen (keeps main file standalone)
;;;; Do NOT use in production; only for bring-up/testing in SBCL host

(defpackage :habu-sbcl-codegen
  (:use :cl :habu-shim)
  (:export codegen-expr compile-expr compile-to-arm64-with-runtime compile-to-arm64
           make-runtime-addrs runtime-lookup *runtime-addrs*
           compile-program-with-functions-with-runtime compile-program-with-functions
           env-lookup env-extend compile-forms))

(in-package :habu-sbcl-codegen)

(defparameter *runtime-addrs* nil)
(defparameter *collected-lambdas* nil)
(defparameter *stack-frame-size* #xFF0)
(defparameter *env-base-offset* #x180)
(defparameter *temp-slot-base* #x40)
(defparameter *temp-slot-guard* #x180)
(defparameter *arg-spill-base* #x200)
(defparameter *arg-spill-stride* #x8)
(defparameter *max-arg-spill-count*
  (/ (- *stack-frame-size* *arg-spill-base*) *arg-spill-stride*))

(defun string->char-codes (s)
  "Return list of integer char codes from string S."
  (loop for ch across s collect (char-code ch)))

(defun quote->ir (obj)
  "Lower a quoted object to IR using cons/runtime construction.
Supports fixnums, nil, lists, symbols, strings, and vectors of those."
  (cond
    ((fixnum? obj) (list 'lit obj))
    ((null obj) (list 'lit #x0))
    ((stringp obj) (cons 'string-lit (string->char-codes obj)))
    ((symbolp obj) (list 'symbol-lit (symbol-name obj)))
    ((vectorp obj) (cons 'vector-lit (map 'list #'quote->ir obj)))
    ((consp obj) (list 'cons-call (quote->ir (car obj)) (quote->ir (cdr obj))))
    (t (list 'lit #x0))))

(defun codegen-string-from-chars (chars temp-depth)
  "Build string literal from CHAR integer list; returns code yielding string in x0."
  (let* ((len (length chars))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (arm64-movz 0 len)
                        (arm64-ldr 11 19 56)  ; make-vector
                        (arm64-blr 11)
                        (arm64-str 0 31 vec-slot)))
         (body alloc))
    (loop for ch in chars
          for idx from 0 do
            (let* ((tagged (ash ch 4))
                   (store (append
                            (arm64-ldr 0 31 vec-slot) ; x0 = vector
                            (arm64-movz 1 idx)        ; x1 = index
                            (if (< tagged #x10000)
                                (arm64-movz 2 tagged)
                                (arm64-load-addr 2 tagged))
                            (arm64-ldr 11 19 64)      ; vector-set
                            (arm64-blr 11))))
              (setf body (append body store))))
    (append body
            (arm64-ldr 0 31 vec-slot)   ; x0 = vector
            (arm64-ldr 9 19 80)         ; make-string-from-vector
            (arm64-blr 9))))

(defun codegen-vector-literal (elements runtime-addrs fn-offsets current-offset temp-depth)
  "Emit code for vector literal ELEMENTS (already IR), return vector in x0."
  (let* ((len (length elements))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (arm64-movz 0 len)
                        (arm64-ldr 11 19 56)
                        (arm64-blr 11)
                        (arm64-str 0 31 vec-slot)))
         (cursor (if current-offset (+ current-offset (count-instrs alloc)) nil))
         (body alloc))
    (loop for el in elements
          for idx from 0 do
            (let* ((el-code (codegen-expr el runtime-addrs fn-offsets cursor (+ temp-depth 1)))
                   (store (append
                            (arm64-mov 2 0)            ; value -> x2
                            (arm64-ldr 0 31 vec-slot)  ; x0 = vector
                            (arm64-movz 1 idx)         ; x1 = index
                            (arm64-ldr 11 19 64)       ; vector-set
                            (arm64-blr 11)))
                   (step (+ (count-instrs el-code) (count-instrs store))))
              (setf body (append body el-code store))
              (when cursor (setf cursor (+ cursor step)))))
    (append body (arm64-ldr 0 31 vec-slot))))

(defun collect-var-offsets (ir)
  "Collect all variable offsets referenced in IR."
  (cond
    ((null ir) nil)
    ((has-tag? ir 'var) (list (cadr ir)))
    ((has-tag? ir 'capture) nil)
    ((consp ir) (remove-duplicates
                 (apply #'append (mapcar #'collect-var-offsets ir))))
    (t nil)))

(defun rewrite-captures (ir capture-map)
  "Rewrite IR var nodes whose offset is in capture-map to capture nodes."
  (cond
    ((null ir) nil)
    ((has-tag? ir 'var)
     (let* ((off (cadr ir))
            (entry (assoc off capture-map)))
       (if entry
           (list 'capture (cdr entry))
           ir)))
    ((consp ir) (cons (rewrite-captures (car ir) capture-map)
                      (rewrite-captures (cdr ir) capture-map)))
    (t ir)))

(defun encode-word-le (word)
  "Encode 32-bit word into little-endian byte list for smoke output."
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

(defun pick-runtime-imm (runtime-addrs fallback)
  "Choose a low 16-bit immediate from runtime-addrs (alist), else fallback."
  (let ((entry (car runtime-addrs)))
    (if entry
        (logand (cdr entry) #xFFFF)
        (logand fallback #xFFFF))))

(defun has-tag? (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun env-lookup (sym env)
  "Look up a symbol in the environment, returns stack offset or nil"
  (cond
    ((null env) nil)
    ((eq (caar env) sym) (cdar env))
    (t (env-lookup sym (cdr env)))))

(defun env-extend (bindings env)
  "Extend environment with new bindings, allocating stack offsets"
  ;; Find the maximum offset in current environment
  (let ((max-offset (if env
                        (apply #'max (mapcar #'cdr env))
                        -1)))
    (append
      ;; New bindings get offsets starting after the max
      (let ((offset (+ max-offset 1)))
        (mapcar (lambda (binding)
                  (cons (car binding)      ; Variable name
                        (prog1 offset      ; Current offset
                          (incf offset)))) ; Next offset
                bindings))
      env)))

(defun runtime-lookup (name runtime-addrs)
  "SBCL shim: lookup name in alist runtime-addrs (symbol . addr)."
  (if (nil? runtime-addrs)
      #x0
      (let* ((entry (car runtime-addrs))
             (entry-name (car entry))
             (entry-addr (cdr entry)))
        (if (eq name entry-name)
            entry-addr
            (runtime-lookup name (cdr runtime-addrs))))))

(defun make-runtime-addrs (cons-addr car-addr cdr-addr
                              &key make-closure closure-code closure-env
                                code-base make-vector vector-set vector-ref
                                make-string-from-vector make-symbol-from-string
                                string-length-raw symbol-name)
  "Create runtime address table for codegen (alist of symbol . addr).
Cons/car/cdr are required; others should be provided in production."
  (flet ((fail (what) (error "Missing runtime address: ~A" what)))
    (unless cons-addr (fail 'habu_cons))
    (unless car-addr (fail 'habu_car))
    (unless cdr-addr (fail 'habu_cdr))
    (remove-if (lambda (entry) (null (cdr entry)))
               (list (cons 'habu_cons cons-addr)
                     (cons 'habu_car car-addr)
                     (cons 'habu_cdr cdr-addr)
                     (cons 'habu_make_closure make-closure)
                     (cons 'habu_closure_code closure-code)
                     (cons 'habu_closure_env closure-env)
                     (cons 'habu_code_base code-base)
                     (cons 'habu_make_vector make-vector)
                     (cons 'habu_vector_set vector-set)
                     (cons 'habu_vector_ref vector-ref)
                     (cons 'habu_make_string_from_vector make-string-from-vector)
                     (cons 'habu_make_symbol_from_string make-symbol-from-string)
                     (cons 'habu_string_length_raw string-length-raw)
                     (cons 'habu_symbol_name symbol-name)))))

(defun op= (sym name)
  "Package-agnostic symbol name comparison."
  (and (symbolp sym) (string= (symbol-name sym) name)))

(defun sbcl-comma-p (obj)
  #+sbcl
  (let ((sym (find-symbol "COMMA" "SB-IMPL")))
    (and sym (eq (type-of obj) sym)))
  #-sbcl nil)

(defun sbcl-comma-kind (obj)
  #+sbcl (funcall (symbol-function (find-symbol "COMMA-KIND" "SB-IMPL")) obj)
  #-sbcl 0)

(defun sbcl-comma-expr (obj)
  #+sbcl (funcall (symbol-function (find-symbol "COMMA-EXPR" "SB-IMPL")) obj)
  #-sbcl nil)

(defun expand-quasiquote-ir (obj env fenv)
  "Expand quasiquoted OBJ into IR with unquotes evaluated via compile-expr."
  (cond
    ((sbcl-comma-p obj)
     (let ((kind (sbcl-comma-kind obj))
           (expr (sbcl-comma-expr obj)))
       (if (= kind 0)
           (compile-expr expr env fenv)
           (error "unquote-splicing not supported in quasiquote IR expansion: ~S" obj))))
    ;; ,expr -> compile expr
    ((and (consp obj) (op= (car obj) "UNQUOTE"))
     (compile-expr (cadr obj) env fenv))
    ;; ,@expr unsupported for now
    ((and (consp obj) (op= (car obj) "UNQUOTE-SPLICING"))
     (error "unquote-splicing not supported in quasiquote IR expansion: ~S" obj))
    ;; Vector literal
    ((vectorp obj)
     (list 'vector-lit (map 'list (lambda (el) (expand-quasiquote-ir el env fenv)) obj)))
    ;; Cons -> cons-call of car/cdr
    ((consp obj)
     (list 'cons-call
           (expand-quasiquote-ir (car obj) env fenv)
           (expand-quasiquote-ir (cdr obj) env fenv)))
    ;; Atom -> literal lowering
    (t (quote->ir obj))))

;; ARM64 Instruction Encoders (Functional)
(defun arm64-movz (rd imm)
  "MOVZ Xd, #imm16 - Move zero-extended 16-bit immediate
   Encoding: bits [20:5] = imm16, bits [4:0] = rd"
  (let* ((imm16 (logand imm #xFFFF))
         (base #xD2800000)
         (imm-bits (ash imm16 5))
         (encoded (logior base imm-bits rd)))
    (encode-word-le encoded)))

(defun arm64-add (rd rn rm)
  "ADD Xd, Xn, Xm - Add registers"
  (let* ((base #x8B000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sub (rd rn rm)
  "SUB Xd, Xn, Xm - Subtract registers"
  (let* ((base #xCB000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-mul (rd rn rm)
  "MUL Xd, Xn, Xm - Multiply registers"
  (let* ((base #x9B007C00)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm - Signed divide"
  (let* ((base #x9AC00C00)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-lsl (rd rn shift)
  "LSL Xd, Xn, #shift - Logical shift left immediate (alias for UBFM)"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (immr (logand (- 64 shift-bits) #x3F))  ; -shift mod 64
         (imms (logand (- 63 shift-bits) #x3F))  ; 63 - shift
         (encoded (logior base
                          (ash immr 16)
                          (ash imms 10)
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-lsr (rd rn shift)
  "LSR Xd, Xn, #shift - Logical shift right immediate"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (encoded (logior base
                          (ash shift-bits 16)  ; immr
                          (ash 63 10)          ; imms = 63
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-mov (rd rm)
  "MOV Xd, Xm - Move register (via ORR)"
  (let* ((base #xAA0003E0)
         (encoded (logior base (ash rm 16) rd)))
    (encode-word-le encoded)))

(defun arm64-and (rd rn rm)
  "AND Xd, Xn, Xm - Bitwise AND registers"
  (let* ((base #x8A000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-orr (rd rn rm)
  "ORR Xd, Xn, Xm - Bitwise OR registers"
  (let* ((base #xAA000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-ldr (rt rn offset)
  "LDR Xt, [Xn, #offset] - Load register from memory
   offset is in bytes, must be 8-byte aligned, encoded as offset/8"
  (let* ((base #xF9400000)
         (imm12 (logand (/ offset 8) #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-str (rt rn offset)
  "STR Xt, [Xn, #offset] - Store register to memory
   offset is in bytes, must be 8-byte aligned, encoded as offset/8"
  (let* ((base #xF9000000)
         (imm12 (logand (/ offset 8) #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-add-imm (rd rn imm)
  "ADD Xd, Xn, #imm12 - Add immediate (use sp register properly)"
  (let* ((base #x91000000)
         (imm12 (logand imm #xFFF))
         ;; ARM64 uses reg 31 to mean SP in some contexts
         (rn-bits (if (= rn 31) 31 rn))
         (rd-bits (if (= rd 31) 31 rd))
         (encoded (logior base (ash imm12 10) (ash rn-bits 5) rd-bits)))
    (encode-word-le encoded)))

(defun arm64-sub-imm (rd rn imm)
  "SUB Xd, Xn, #imm12 - Subtract immediate (use sp register properly)"
  (let* ((base #xD1000000)
         (imm12 (logand imm #xFFF))
         ;; ARM64 uses reg 31 to mean SP in some contexts
         (rn-bits (if (= rn 31) 31 rn))
         (rd-bits (if (= rd 31) 31 rd))
         (encoded (logior base (ash imm12 10) (ash rn-bits 5) rd-bits)))
    (encode-word-le encoded)))

(defun arm64-stp (rt1 rt2 rn imm)
  "STP Xt1, Xt2, [Xn, #imm] - Store pair of registers
   imm is in bytes, must be multiple of 8, encoded as imm/8"
  (let* ((base #xA9000000)
         (pre-index (if (< imm 0) #x00800000 0))  ; Pre-index if negative
         (post-index 0)  ; Not used for now
         (imm7 (logand (/ (abs imm) 8) #x7F))
         (encoded (logior base pre-index post-index (ash imm7 15) (ash rt2 10) (ash rn 5) rt1)))
    (encode-word-le encoded)))

(defun arm64-ldp (rt1 rt2 rn imm)
  "LDP Xt1, Xt2, [Xn, #imm] - Load pair of registers
   Uses offset mode for imm >= 0, post-index for negative (though we don't use negative)
   imm is in bytes, must be multiple of 8, encoded as imm/8"
  (let* ((base #xA9400000)  ; Offset mode
         (imm7 (logand (/ imm 8) #x7F))
         (encoded (logior base (ash imm7 15) (ash rt2 10) (ash rn 5) rt1)))
    (encode-word-le encoded)))

(defun arm64-cmp (rn rm)
  "CMP Xn, Xm - Compare registers (sets flags)"
  (let* ((base #xEB00001F)  ; SUBS XZR, Xn, Xm
         (encoded (logior base (ash rm 16) (ash rn 5))))
    (encode-word-le encoded)))

(defun arm64-cset (rd cond)
  "CSET Xd, cond - Conditional set (1 if condition, else 0)"
  (let* ((base #x9A9F07E0)  ; CSINC Xd, XZR, XZR, invert(cond)
         (inv-cond (logxor cond 1))  ; Invert condition
         (encoded (logior base (ash inv-cond 12) rd)))
    (encode-word-le encoded)))

(defun arm64-b (offset)
  "B offset - Unconditional branch (offset in instructions, signed 26-bit)"
  (let* ((base #x14000000)
         (offset-bits (if (< offset 0)
                          (logand (+ offset #x4000000) #x3FFFFFF)
                          (logand offset #x3FFFFFF)))
         (encoded (logior base offset-bits)))
    (encode-word-le encoded)))

(defun arm64-bl (offset)
  "BL offset - Branch with link (offset in instructions, signed 26-bit)"
  (let* ((base #x94000000)
         ;; Handle negative offsets properly with 26-bit two's complement
         (offset-bits (if (< offset 0)
                         (logand (+ offset #x4000000) #x3FFFFFF)  ; Add 2^26 for two's complement
                         (logand offset #x3FFFFFF)))
         (encoded (logior base offset-bits)))
    (encode-word-le encoded)))

(defun arm64-b-cond (cond offset)
  "B.cond offset - Conditional branch (offset in instructions, signed 19-bit)"
  (let* ((base #x54000000)
         (offset-19bit (logand offset #x7FFFF))  ; Mask to 19 bits
         (offset-bits (ash offset-19bit 5))       ; Shift to bits [23:5]
         (encoded (logior base offset-bits cond)))
    (encode-word-le encoded)))

(defun arm64-ret ()
  "RET - Return from subroutine"
  (encode-word-le #xD65F03C0))

(defun arm64-movk (rd imm shift)
  "MOVK Xd, #imm16, LSL #shift - Move with keep (loads 16 bits without clearing others)
   shift must be 0, 16, 32, or 48"
  (let* ((imm16 (logand imm #xFFFF))
         (hw (/ shift 16))  ; Which 16-bit chunk (0, 1, 2, or 3)
         (base #xF2800000)
         (imm-bits (ash imm16 5))
         (hw-bits (ash hw 21))
         (encoded (logior base hw-bits imm-bits rd)))
    (encode-word-le encoded)))

(defun arm64-blr (rn)
  "BLR Xn - Branch with link to register"
  (let* ((base #xD63F0000)
         (encoded (logior base (ash rn 5))))
    (encode-word-le encoded)))

(defun arm64-str-pre (rt rn offset)
  "STR Xt, [Xn, #offset]! - Store register with pre-decrement
   Used for push: STR x0, [sp, #-16]!"
  ;; Encoding: 1111 1000 0 imm9 11 Rn Rt
  ;; Base for STR pre-index: F8001C00
  (let* ((base #xF8001C00)  ; Pre-index variant with writeback
         (imm9 (logand offset #x1FF))  ; 9-bit immediate (already in two's complement)
         (encoded (logior base (ash imm9 12) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-ldr-post (rt rn offset)
  "LDR Xt, [Xn], #offset - Load register with post-increment
   Used for pop: LDR x0, [sp], #16"
  ;; Encoding: 1111 1000 0100 0000 01 imm9 Rn Rt
  ;; Base for LDR post-index: F8400400
  (let* ((base #xF8400400)  ; Post-index variant
         (imm9 (logand offset #x1FF))  ; 9-bit immediate
         (encoded (logior base (ash imm9 12) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-push (rt)
  "Push register onto stack using SUB + STR"
  ;; Decrement stack pointer then store
  (append (arm64-sub-imm 31 31 16)    ; sp = sp - 16
          (arm64-str rt 31 0)))       ; [sp] = rt

(defun arm64-pop (rt)
  "Pop register from stack using LDR + ADD"
  ;; Load then increment stack pointer
  (append (arm64-ldr rt 31 0)         ; rt = [sp]
          (arm64-add-imm 31 31 16)))  ; sp = sp + 16

(defun arm64-load-addr (rd addr)
  "Load 64-bit address into register using MOVZ + MOVK sequence"
  (let ((bits-0-15 (logand addr #xFFFF))
        (bits-16-31 (logand (ash addr -16) #xFFFF))
        (bits-32-47 (logand (ash addr -32) #xFFFF))
        (bits-48-63 (logand (ash addr -48) #xFFFF)))
    (append (arm64-movz rd bits-0-15)
            (arm64-movk rd bits-16-31 16)
            (arm64-movk rd bits-32-47 32)
            (arm64-movk rd bits-48-63 48))))

(defun temp-slot-offset (temp-depth)
  "Stack offset (bytes) for temporary storage at a given nesting depth."
  (let ((offset (+ *temp-slot-base* (* temp-depth #x8))))
    ;; Keep temps within the stack frame and below env base
    (when (>= offset *temp-slot-guard*)
      (error "temp-depth ~A exceeds frame temp area (offset #x~X)" temp-depth offset))
    offset))

(defun arg-spill-offset (index)
  "Stack offset for staged arguments before calls (8-byte stride)."
  (let ((offset (+ *arg-spill-base* (* index *arg-spill-stride*))))
    (when (>= offset *stack-frame-size*)
      (error "argument index ~A exceeds spill area (offset #x~X)" index offset))
    offset))

(defun codegen-expr (ir runtime-addrs &optional fn-offsets current-offset (temp-depth 0))
  "Enhanced codegen: literals, arithmetic, runtime calls with depth-tracked temps"
  (cond
    ;; Literal: load tagged fixnum (value << 4)
    ((has-tag? ir 'lit)
     (let* ((value (cadr ir))
            (tagged (ash value 4)))  ; Tag fixnum: value << 4
       (if (and (>= tagged 0) (< tagged #x10000))
           (arm64-movz 0 tagged)
           (arm64-load-addr 0 tagged))))

    ;; Variable: load from stack (negative offset from x20 = environment base)
    ((has-tag? ir 'var)
     (let ((offset (cadr ir)))
       ;; Variables are stored at negative offsets from x20 (stack grows down)
       ;; Use x1 as temp to compute address
       (append
         (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
         (arm64-ldr 0 1 0))))                ; Load from [x1 + 0]

    ;; Set variable: store value to stack, return the value
    ((has-tag? ir 'set-var)
     (let* ((offset (cadr ir))
            (val-ir (caddr ir))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset temp-depth)))
       ;; Compute value, store to variable slot, value stays in x0
       (append val-code
               (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
               (arm64-str 0 1 0))))               ; Store x0 to [x1 + 0]

    ;; String literal: build vector of chars then make-string-from-vector
    ((has-tag? ir 'string-lit)
     (codegen-string-from-chars (cdr ir) temp-depth))

    ;; Symbol literal: build string then symbol-from-string
    ((has-tag? ir 'symbol-lit)
     (let* ((str-code (codegen-string-from-chars (string->char-codes (cadr ir)) temp-depth))
            (cursor (if current-offset (+ current-offset (count-instrs str-code)) nil)))
       (append str-code
               (arm64-ldr 9 19 88) ; make-symbol-from-string
               (arm64-blr 9))))

    ;; Vector literal
    ((has-tag? ir 'vector-lit)
     (codegen-vector-literal (cdr ir) runtime-addrs fn-offsets current-offset temp-depth))

    ;; Tag inspection: (get-tag x) => fixnum tag bits
    ((has-tag? ir 'get-tag)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-movz 1 #xF)   ; mask
               (arm64-and 0 0 1)    ; tag in x0
               (arm64-lsl 0 0 4)))) ; tag as fixnum

    ;; Captured variable: load from closure env vector in x24
    ((has-tag? ir 'capture)
     (let ((idx (cadr ir)))
       (append
         (arm64-mov 0 24)                   ; x0 = closure env vector
         (arm64-movz 1 idx)                 ; x1 = index (raw)
         (arm64-ldr 9 19 72)                ; x9 = habu_vector_ref (slot 9)
         (arm64-blr 9))))                   ; x0 = env[idx]

    ;; Addition: (add left right)
    ;; Use depth-indexed temp slot to park left operand while evaluating right
    ((has-tag? ir 'add)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code                     ; Compute left → x0
               (arm64-str 0 31 left-slot)    ; Save left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code                    ; Compute right → x0
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp to x0
               (arm64-add 0 0 1))))        ; x0 = x0 + x1

    ;; Subtraction: (sub left right)
    ;; Use depth-indexed temp slot to park left operand while evaluating right
    ((has-tag? ir 'sub)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code
               (arm64-str 0 31 left-slot)    ; Save left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp to x0
               (arm64-sub 0 0 1))))        ; x0 = x0 - x1

    ;; Multiplication: (mul left right) - must untag/retag
    ;; Temp slot holds untagged left operand while computing right
    ((has-tag? ir 'mul)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 3)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code
               (arm64-lsr 0 0 4)             ; Untag left
               (arm64-str 0 31 left-slot)    ; Save untagged left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code
               (arm64-lsr 1 0 4)             ; Untag right into x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp into x0
               (arm64-mul 0 0 1)             ; Multiply x0 = x0 * x1
               (arm64-lsl 0 0 4))))        ; Retag result

    ;; Comparison: (cmp-eq left right) - returns tagged 1 or 0
    ((has-tag? ir 'cmp-eq)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 0)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than: (cmp-lt left right)
    ((has-tag? ir 'cmp-lt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 11)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than: (cmp-gt left right)
    ((has-tag? ir 'cmp-gt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 12)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than or equal: (cmp-le left right)
    ((has-tag? ir 'cmp-le)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 13)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than or equal: (cmp-ge left right)
    ((has-tag? ir 'cmp-ge)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 10)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Division: (div left right)
    ((has-tag? ir 'div)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 0 0 1)            ; x0 = left/right
               (arm64-lsl 0 0 4))))          ; retag

    ;; Modulo: (mod left right)
    ((has-tag? ir 'mod)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 2 0 1)            ; x2 = quotient
               (arm64-mul 2 2 1)             ; x2 = quotient * right
               (arm64-sub 0 0 2)             ; x0 = left - product
               (arm64-lsl 0 0 4))))          ; retag

    ;; Remainder: (rem left right)
    ((has-tag? ir 'rem)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 2 0 1)            ; x2 = quotient
               (arm64-mul 2 2 1)             ; x2 = quotient * right
               (arm64-sub 0 0 2)             ; x0 = remainder
               (arm64-lsl 0 0 4))))          ; retag

    ;; Not equal: (cmp-ne left right)
    ((has-tag? ir 'cmp-ne)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 1)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Conditional: (if-expr test then else)
    ((has-tag? ir 'if-expr)
     (let* ((test-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (test-code (codegen-expr test-ir runtime-addrs fn-offsets current-offset temp-depth))
            (test-len (count-instrs test-code))
            (then-code (codegen-expr then-ir runtime-addrs fn-offsets
                                     (if current-offset
                                         (+ current-offset test-len 2)
                                         nil)
                                     temp-depth))
            (then-len (/ (length then-code) 4))
            (else-code (codegen-expr else-ir runtime-addrs fn-offsets
                                     (if current-offset
                                         (+ current-offset test-len 2 then-len 1)
                                         nil)
                                     temp-depth))
            (else-len (/ (length else-code) 4)))
       ;; Layout: CMP, B.EQ → else-code, then-code, B skip-else, else-code
       ;; True branch (non-zero) falls through to then-code; false jumps to else-code
       ;; Offsets from the B instructions:
       ;;   B.EQ: offset = then-len + 2 (skip then-code + following B)
       ;;   B (skip else): offset = else-len + 1
       (append test-code
               (arm64-cmp 0 31)            ; Compare result with 0 (XZR)
               (arm64-b-cond 0 (+ 2 then-len)) ; Jump to else if zero
               then-code
               (arm64-b (+ 1 else-len))    ; Skip else after then
               else-code)))

    ;; Cons: (cons-call left right) - call runtime cons via table
    ;;   Runtime table pointer is in x19 (saved by prologue)
    ;;   Save/restore x24 around arg evaluation in case args contain funcalls
    ((has-tag? ir 'cons-call)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            ;; cursor: save x24 + left-code + store left + restore x24
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (left-cursor (if current-offset
                            (+ current-offset 1 (count-instrs left-code) 2)
                            nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets left-cursor nested-depth)))
       ;; Call cons(left, right) using runtime table[0]
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before any arg evaluation
               left-code                     ; Compute left → x0
               (arm64-str 0 31 left-slot)    ; Save left to temp slot
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right arg
               right-code                    ; Compute right → x0
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp slot
               (arm64-ldr 9 19 0)            ; Load cons from table: LDR x9, [x19, #0]
               (arm64-blr 9))))             ; Call cons(x0, x1) → result in x0

    ;; Vector ref: (vector-ref vec idx)
    ((has-tag? ir 'vector-ref)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (vec-slot (temp-slot-offset temp-depth))
            (vec-code (codegen-expr vec-ir runtime-addrs fn-offsets current-offset temp-depth))
            (cursor (if current-offset (+ current-offset (count-instrs vec-code)) nil))
            (idx-code (codegen-expr idx-ir runtime-addrs fn-offsets cursor (+ temp-depth 1))))
       (append vec-code
               (arm64-str 0 31 vec-slot)
               idx-code
               (arm64-lsr 1 0 4)           ; untag index
               (arm64-ldr 0 31 vec-slot)
               (arm64-ldr 9 19 72) ; vector-ref
               (arm64-blr 9))))

    ;; Car: (car-call arg) - call runtime car via table
    ((has-tag? ir 'car-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 8)           ; Load car from table: LDR x9, [x19, #8]
               (arm64-blr 9))))             ; Call car(x0) → result in x0

    ;; Cdr: (cdr-call arg) - call runtime cdr via table
    ((has-tag? ir 'cdr-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 16)          ; Load cdr from table: LDR x9, [x19, #16]
               (arm64-blr 9))))             ; Call cdr(x0) → result in x0

    ;; Set-car: (setcar-call cons-ir val-ir) - call runtime set_car, return value
    ((has-tag? ir 'setcar-call)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot-offset temp-depth))
            (val-slot (temp-slot-offset (1+ temp-depth)))
            (cons-code (codegen-expr cons-ir runtime-addrs fn-offsets current-offset temp-depth))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append cons-code
               (arm64-str 0 31 cons-slot)   ; Save cons to temp slot
               val-code
               (arm64-str 0 31 val-slot)    ; Save value to temp slot
               (arm64-ldr 0 31 cons-slot)   ; x0 = cons (arg 1)
               (arm64-ldr 1 31 val-slot)    ; x1 = value (arg 2)
               (arm64-ldr 9 19 112)         ; Load set_car from table (slot 14)
               (arm64-blr 9)                ; Call set_car(cons, value) -> void
               (arm64-ldr 0 31 val-slot)))) ; Return the value

    ;; Set-cdr: (setcdr-call cons-ir val-ir) - call runtime set_cdr, return value
    ((has-tag? ir 'setcdr-call)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot-offset temp-depth))
            (val-slot (temp-slot-offset (1+ temp-depth)))
            (cons-code (codegen-expr cons-ir runtime-addrs fn-offsets current-offset temp-depth))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append cons-code
               (arm64-str 0 31 cons-slot)   ; Save cons to temp slot
               val-code
               (arm64-str 0 31 val-slot)    ; Save value to temp slot
               (arm64-ldr 0 31 cons-slot)   ; x0 = cons (arg 1)
               (arm64-ldr 1 31 val-slot)    ; x1 = value (arg 2)
               (arm64-ldr 9 19 120)         ; Load set_cdr from table (slot 15)
               (arm64-blr 9)                ; Call set_cdr(cons, value) -> void
               (arm64-ldr 0 31 val-slot)))) ; Return the value

    ;; Symbol-name
    ((has-tag? ir 'symbol-name)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-ldr 9 19 104) ; symbol-name
               (arm64-blr 9))))

    ;; String length (returns fixnum)
    ((has-tag? ir 'string-len)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-ldr 9 19 96) ; string-length-raw
               (arm64-blr 9)
               (arm64-lsl 0 0 4)))) ; tag length as fixnum

    ;; Let expression: (let-expr bind-values body-ir num-bindings env-offsets)
    ;; Must track current-offset properly for function calls in bindings/body
    ((has-tag? ir 'let-expr)
     (let* ((bind-values (cadr ir))
            (body-ir (caddr ir))
            (num-bindings (cadddr ir))
            (env-offsets (nth 4 ir))  ; Get environment offsets for this let's bindings
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            ;; Track offset as we generate code: start after save x24 (1 instruction)
            (cursor (if current-offset (+ current-offset 1) nil))
            (accum (arm64-str 24 31 x24-slot)))  ; save x24 at start
       ;; Generate binding values with proper offset tracking
       (dolist (pair (mapcar #'list bind-values env-offsets))
         (let* ((val-ir (car pair))
                (offset (cadr pair))
                ;; Each binding: LDR x24 (1) + binding-code + SUB (1) + STR (1)
                (restore (arm64-ldr 24 31 x24-slot))
                (bind-cursor (if cursor (+ cursor 1) nil))  ; +1 for LDR x24
                (bind-code (codegen-expr val-ir runtime-addrs fn-offsets bind-cursor nested-depth))
                (store-code (append
                              (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
                              (arm64-str 0 1 0)))                ; Store at [x1 + 0]
                (block-instrs (+ 1 (count-instrs bind-code) 2)))  ; LDR + bind + SUB + STR
           (setf accum (append accum restore bind-code store-code))
           (when cursor
             (setf cursor (+ cursor block-instrs)))))
       ;; Restore x24 before body and generate body
       (let* ((restore (arm64-ldr 24 31 x24-slot))
              (body-cursor (if cursor (+ cursor 1) nil))  ; +1 for LDR x24
              (body-code (codegen-expr body-ir runtime-addrs fn-offsets body-cursor nested-depth)))
         (append accum restore body-code))))

    ;; Progn: evaluate each subexpression in order, return last result
    ;; Must preserve x24 across forms since funcalls may clobber it
    ((has-tag? ir 'progn)
     (let* ((exprs (cdr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            (cursor (if current-offset (+ current-offset 1) nil))  ; +1 for save
            (accum (arm64-str 24 31 x24-slot)))  ; save x24 at start
       (loop for sub in exprs
             for idx from 0 do
               (let* ((restore (if (> idx 0)
                                   (arm64-ldr 24 31 x24-slot)  ; restore before each subsequent form
                                   nil))
                      ;; Account for restore instruction before chunk when idx > 0
                      (chunk-cursor (if (and cursor (> idx 0)) (+ cursor 1) cursor))
                      (chunk (codegen-expr sub runtime-addrs fn-offsets chunk-cursor nested-depth))
                      (instrs (+ (count-instrs restore) (count-instrs chunk))))
                 (setf accum (append accum restore chunk))
                 (when cursor
                   (setf cursor (+ cursor instrs)))))
       accum))

    ;; Lambda reference: build closure for compiled lambda
    ((has-tag? ir 'lambda-ref)
     (let* ((lambda-name (cadr ir))
            (fn-entry (assoc lambda-name fn-offsets))
            (fn-offset (if fn-entry (cadr fn-entry) 0))
            (captures (if fn-entry (caddr fn-entry) nil))
            (capture-count (length captures))
            (offset-bytes (* fn-offset 4))
            (code-slot (temp-slot-offset temp-depth))
            (env-slot (+ code-slot 8)))
       ;; Runtime table layout:
       ;;   [0] cons, [8] car, [16] cdr, [24] make-closure, [32] closure-code, [40] closure-env, [48] code base
       ;;   [56] make-vector, [64] vector-set, [72] vector-ref
       ;;   [80] make-string-from-vector, [88] make-symbol-from-string, [96] string-length-raw, [104] symbol-name
       ;; Package forms are folded at compile time; the runtime table stops at symbol-name.
       (append
         (arm64-ldr 9 19 48)              ; x9 = code base
         (arm64-load-addr 10 offset-bytes); x10 = offset bytes
         (arm64-add 0 9 10)               ; x0 = code base + offset
         (arm64-str 0 31 code-slot)       ; save code pointer
         ;; Allocate env vector if needed
         (if (= capture-count 0)
             (append
               (arm64-movz 1 0)           ; x1 = NIL
               (arm64-ldr 11 19 24)       ; make-closure
               (arm64-blr 11))
             (append
               (arm64-movz 0 capture-count) ; x0 = length
               (arm64-ldr 11 19 56)         ; x11 = make-vector
               (arm64-blr 11)               ; x0 = vector
               (arm64-str 0 31 env-slot)    ; save vector
               ;; Store captures
               (apply #'append
                      (mapcar (lambda (off idx)
                                (append
                                  (arm64-ldr 0 31 env-slot) ; x0 = vector
                                  (arm64-movz 1 idx)        ; x1 = index
                                  (arm64-sub-imm 2 20 (* off 8)) ; x2 = x20 - off*8
                                  (arm64-ldr 2 2 0)         ; x2 = captured value
                                  (arm64-ldr 11 19 64)      ; x11 = vector-set
                                  (arm64-blr 11)))
                              captures
                              (loop for i from 0 below capture-count collect i)))
               ;; Make closure
               (arm64-ldr 0 31 code-slot)  ; x0 = code pointer
               (arm64-ldr 1 31 env-slot)   ; x1 = env vector
               (arm64-ldr 11 19 24)        ; make-closure
               (arm64-blr 11))))))         ; x0 = closure

    ;; Call closure: evaluate fn-expr to closure, load code pointer, call with args
    ;; Args are stored to temp slots (not arg-spill area) to avoid clobbering by nested funcalls.
    ;; IMPORTANT: Args need CALLER's x24 for var-refs, not callee's env.
    ((has-tag? ir 'call-closure)
     (let* ((fn-ir (cadr ir))
            (arg-irs (caddr ir))
            (num-args (length arg-irs))
            ;; Stack slot layout (all relative to temp-depth):
            ;;   0: caller-x24  1: closure  2: code  3: callee-env
            ;;   4..(4+num-args-1): args
            ;;   (4+num-args)...: available for nested expressions
            (caller-x24-slot (temp-slot-offset temp-depth))
            (closure-slot (temp-slot-offset (+ temp-depth 1)))
            (code-slot (temp-slot-offset (+ temp-depth 2)))
            (callee-env-slot (temp-slot-offset (+ temp-depth 3)))
            (arg-base (+ temp-depth 4))  ; args start at slot 4
            (nested-depth (+ arg-base num-args))  ; nested exprs use slots after args
            (extra-count (max 0 (- num-args 5)))
            ;; fn-code uses nested-depth since slots 0-3 are reserved for our use
            (fn-code (codegen-expr fn-ir runtime-addrs fn-offsets
                                   (if current-offset (+ current-offset 1) nil)
                                   nested-depth))
            (setup (append
                     (arm64-str 24 31 caller-x24-slot)  ; save caller's x24 FIRST
                     fn-code                           ; closure in x0
                     (arm64-str 0 31 closure-slot)     ; save closure value
                     ;; Get code pointer via runtime helper
                     (arm64-ldr 9 19 32)               ; x9 = closure_code
                     (arm64-blr 9)                     ; x0 = code pointer
                     (arm64-str 0 31 code-slot)        ; save code pointer
                     ;; Get closure env and save to stack (DON'T set x24 yet!)
                     (arm64-ldr 0 31 closure-slot)     ; x0 = closure value
                     (arm64-ldr 9 19 40)               ; x9 = closure_env
                     (arm64-blr 9)                     ; x0 = env pointer
                     (arm64-str 0 31 callee-env-slot))) ; save callee's env for later
            (stage-code setup)
            (cursor (if current-offset
                        (+ current-offset (count-instrs stage-code))
                        nil)))
       ;; Stage args to temp slots (not arg-spill area!)
       ;; Each arg is stored to temp-slot-offset(arg-base + idx)
       ;; Before each arg, restore caller's x24 so var-refs work correctly
       (loop for arg-ir in arg-irs
             for idx from 0 do
               (let* ((arg-slot (temp-slot-offset (+ arg-base idx)))
                      (restore-x24 (arm64-ldr 24 31 caller-x24-slot))  ; restore caller's x24 (1 instr)
                      ;; Account for restore-x24 instruction before arg-code
                      (arg-cursor (if cursor (+ cursor 1) nil))
                      (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets arg-cursor nested-depth))
                      (store (arm64-str 0 31 arg-slot))  ; store to temp slot, not arg-spill
                      (block (append restore-x24 arg-code store))
                      (block-len (count-instrs block)))
                 (setf stage-code (append stage-code block))
                 (when cursor (incf cursor block-len))))
       (let* (;; Load args from temp slots into registers x0-x4
              (load-code
                (cond
                  ((= num-args 0) nil)
                  ((= num-args 1)
                   (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0))))
                  ((= num-args 2)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))))
                  ((= num-args 3)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))))
                  ((= num-args 4)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))
                     (arm64-ldr 3 31 (temp-slot-offset (+ arg-base 3)))))
                  (t
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))
                     (arm64-ldr 3 31 (temp-slot-offset (+ arg-base 3)))
                     (arm64-ldr 4 31 (temp-slot-offset (+ arg-base 4)))))))
              ;; For >5 args, point x25 to the extra args in temp slots
              (set-extra-ptr (if (> extra-count 0)
                                 (arm64-sub-imm 25 31 (- (temp-slot-offset (+ arg-base 5))))
                                 (arm64-movz 25 0)))
              (arg-count-code (arm64-movz 23 num-args))
              ;; Set x24 to callee's env for the actual call
              (restore-env (arm64-ldr 24 31 callee-env-slot))
              (pre-call (append stage-code load-code set-extra-ptr arg-count-code restore-env (arm64-ldr 9 31 code-slot))))
         (append
           pre-call
           (arm64-blr 9)))))                  ; call

    ;; Function call: (call-fn name arg-irs)
    ((has-tag? ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (arg-irs (caddr ir))
            (num-args (length arg-irs))
            (extra-count (max 0 (- num-args 5)))
            (max-capacity (1- *max-arg-spill-count*))
            (fn-entry (assoc fn-name fn-offsets))
            (fn-offset (if fn-entry (cadr fn-entry) 0))
            ;; Save x24 to temp slot so we can restore it before each arg evaluation
            ;; This handles the case where an arg contains a funcall that clobbers x24
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1)))
       (when (> num-args max-capacity)
         (error "call-fn ~A has ~A args; exceeds spill capacity ~A" fn-name num-args max-capacity))
       (let* ((cursor (if current-offset (+ current-offset 3) nil))  ; +3 for save x24 + x27 setup + first restore
              (stage-code (append
                            (arm64-str 24 31 x24-slot)   ; save caller's x24
                            (arm64-add-imm 27 31 0))))   ; x27 = sp for stable base
         ;; Stage all arguments in order into the spill area using x27 as a stable base
         ;; Before each arg, restore x24 in case previous arg clobbered it
         (loop for arg-ir in arg-irs
               for idx from 0 do
                 (let* ((restore-x24 (arm64-ldr 24 31 x24-slot))  ; restore x24 before each arg
                        (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets cursor nested-depth))
                        (store (arm64-str 0 27 (arg-spill-offset idx)))
                        (block (append restore-x24 arg-code store))
                        (block-len (count-instrs block)))
                   (setf stage-code (append stage-code block))
                   (when cursor (incf cursor block-len))))
         (let* ((load-code
                  (cond
                    ((= num-args 0) nil)
                    ((= num-args 1)
                     (arm64-ldr 0 27 (arg-spill-offset 0)))
                    ((= num-args 2)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))))
                    ((= num-args 3)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))))
                    ((= num-args 4)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))
                       (arm64-ldr 3 27 (arg-spill-offset 3))))
                    (t
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))
                       (arm64-ldr 3 27 (arg-spill-offset 3))
                       (arm64-ldr 4 27 (arg-spill-offset 4))))))
                (set-extra-ptr (if (> extra-count 0)
                                   (arm64-add-imm 25 27 (arg-spill-offset 5))
                                   (arm64-movz 25 0)))
                (arg-count-code (arm64-movz 23 num-args))
                (pre-call (append stage-code load-code set-extra-ptr arg-count-code))
                 (current-pc (if current-offset
                                 (+ current-offset (count-instrs pre-call))
                                 0))
                 (branch-offset (- fn-offset current-pc)))
            (append
              pre-call
              (arm64-bl branch-offset)))))) 

    ;; Division: (div left right) - fixnum helper
    ((has-tag? ir 'div)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 8)  ; habu_div at slot 1
               (arm64-blr 9))))

    ;; Modulo: (mod left right)
    ((has-tag? ir 'mod)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 16) ; habu_mod at slot 2
               (arm64-blr 9))))

    ;; Remainder: (rem left right)
    ((has-tag? ir 'rem)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 24) ; habu_rem at slot 3
               (arm64-blr 9))))

    ;; Default: zero
    (t (arm64-movz 0 0))))

(defun compile-expr (expr env fenv)
  "Enhanced IR generation: literals, arithmetic operations"
  (cond
    ;; Fixnum literal
    ((fixnum? expr)
     (list 'lit expr))

    ;; Symbol (variable)
    ((symbol? expr)
     (let ((off (env-lookup expr env)))
       (if off (list 'var off) (list 'lit 0))))

    ;; List (function call or special form)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Addition (variadic: fold (+ a b c) -> (+ (+ a b) c))
         ((eq op '+)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))           ; (+) => 0
              ((null (cdr args))                     ; (+ a) => a
               (compile-expr (car args) env fenv))
              ((null (cddr args))                    ; (+ a b)
               (list 'add
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (+ a b c ...) => (+ (+ a b) c ...)
               (compile-expr (cons '+ (cons (list '+ (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Subtraction (variadic: fold (- a b c) -> (- (- a b) c))
         ((eq op '-)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))           ; (-) => 0
              ((null (cdr args))                     ; (- a) => negate
               (list 'sub (list 'lit 0) (compile-expr (car args) env fenv)))
              ((null (cddr args))                    ; (- a b)
               (list 'sub
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (- a b c ...) => (- (- a b) c ...)
               (compile-expr (cons '- (cons (list '- (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Multiplication (variadic: fold (* a b c) -> (* (* a b) c))
         ((eq op '*)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 1))           ; (*) => 1
              ((null (cdr args))                     ; (* a) => a
               (compile-expr (car args) env fenv))
              ((null (cddr args))                    ; (* a b)
               (list 'mul
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (* a b c ...) => (* (* a b) c ...)
               (compile-expr (cons '* (cons (list '* (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Division
         ((eq op '/)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'div
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Modulo
         ((eq op 'mod)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'mod
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Remainder
         ((eq op 'rem)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'rem
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Equality comparison
         ((eq op '=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Less than
         ((eq op '<)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-lt
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Greater than
         ((eq op '>)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-gt
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Less than or equal
         ((eq op '<=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-le
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Greater than or equal
         ((eq op '>=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-ge
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Not equal (standard Lisp /=)
         ((eq op '/=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-ne
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Let binding
         ((eq op 'let)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((bindings (cadr expr))
                     (body (caddr expr))
                     ;; Extract binding pairs
                     (bind-pairs (mapcar (lambda (b)
                                          (if (consp b)
                                              (list (car b)
                                                    (compile-expr (cadr b) env fenv))
                                              (list b (list 'lit 0))))
                                        bindings))
                     ;; Create new environment with binding names
                     (bind-names (mapcar #'car bind-pairs))
                     (bind-values (mapcar #'cadr bind-pairs))
                     (new-env (env-extend (mapcar #'list bind-names) env))
                     ;; Get the offsets for each binding
                     (env-offsets (mapcar (lambda (name)
                                           (env-lookup name new-env))
                                         bind-names))
                     ;; Compile body in new environment
                     (body-ir (compile-expr body new-env fenv)))
                (list 'let-expr bind-values body-ir (length bindings) env-offsets))
              (list 'lit 0)))

         ;; Let* binding (sequential)
         ((eq op 'let*)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((bindings (cadr expr))
                    (body (caddr expr)))
                (if (null bindings)
                    (compile-expr body env fenv)
                    ;; Transform to nested lets
                    (compile-expr
                     `(let (,(car bindings))
                        (let* ,(cdr bindings) ,body))
                     env fenv)))
              (list 'lit 0)))

         ;; Setq (variable mutation)
         ((eq op 'setq)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((var (cadr expr))
                     (val-expr (caddr expr))
                     (offset (env-lookup var env)))
                (if offset
                    (list 'set-var offset (compile-expr val-expr env fenv))
                    (list 'lit 0))) ; unknown var
              (list 'lit 0)))

         ;; Setf (generalized assignment)
         ((eq op 'setf)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((place (cadr expr))
                    (val-expr (caddr expr)))
                (cond
                  ;; Simple variable: same as setq
                  ((symbolp place)
                   (let ((offset (env-lookup place env)))
                     (if offset
                         (list 'set-var offset (compile-expr val-expr env fenv))
                         (list 'lit 0))))
                  ;; (setf (car x) val) -> (set-car x val)
                  ((and (consp place) (eq (car place) 'car) (consp (cdr place)))
                   (list 'setcar-call
                         (compile-expr (cadr place) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; (setf (cdr x) val) -> (set-cdr x val)
                  ((and (consp place) (eq (car place) 'cdr) (consp (cdr place)))
                   (list 'setcdr-call
                         (compile-expr (cadr place) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; Other places not yet supported
                  (t (list 'lit 0))))
              (list 'lit 0)))

         ;; Labels (local recursive functions)
         ;; Transform using cons cells as mutable boxes for proper closure semantics:
         ;; (labels ((f (x) body)) expr) ->
         ;;   (let* ((f-box (cons nil nil)))
         ;;     (progn (setf (car f-box) (lambda (x) body'))
         ;;            expr'))
         ;; where body' and expr' transform (f args...) to (funcall (car f-box) args...)
         ((eq op 'labels)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-defs (cadr expr))
                     (body (caddr expr))
                     (fn-names (mapcar #'car fn-defs))
                     ;; Create box names for each function
                     (box-names (mapcar (lambda (name)
                                          (intern (concatenate 'string (symbol-name name) "-BOX")))
                                        fn-names))
                     (name-to-box (mapcar #'cons fn-names box-names))
                     ;; Transform calls to local functions to use (car box)
                     (transform-calls (lambda (e)
                                        (labels ((xform (x)
                                                   (cond
                                                     ((not (consp x)) x)
                                                     ;; (f args...) -> (funcall (car f-box) args...)
                                                     ((and (symbolp (car x))
                                                           (assoc (car x) name-to-box))
                                                      (let ((box (cdr (assoc (car x) name-to-box))))
                                                        `(funcall (car ,box) ,@(mapcar #'xform (cdr x)))))
                                                     (t (mapcar #'xform x)))))
                                          (xform e))))
                     ;; Build let* bindings: (f-box (cons nil nil))
                     (let-bindings (mapcar (lambda (box) (list box '(cons #x0 #x0))) box-names))
                     ;; Build setf assignments: (setf (car f-box) (lambda ...))
                     (setf-forms (mapcar (lambda (def)
                                           (let* ((name (car def))
                                                  (params (cadr def))
                                                  (fn-body (caddr def))
                                                  (box (cdr (assoc name name-to-box)))
                                                  (xformed-body (funcall transform-calls fn-body)))
                                             `(setf (car ,box) (lambda ,params ,xformed-body))))
                                         fn-defs))
                     ;; Transform body calls
                     (xformed-body (funcall transform-calls body))
                     ;; Build: (let* ((f-box (cons nil nil)) ...) (progn (setf (car f-box) ...) ... body))
                     (transformed `(let* ,let-bindings
                                     (progn ,@setf-forms ,xformed-body))))
                (compile-expr transformed env fenv))
              (list 'lit 0)))

         ;; Flet (local non-recursive functions)
         ;; Transform: (flet ((f (x) body)) expr) ->
         ;;            (let ((f (lambda (x) body))) expr')
         ;; where expr' has calls (f ...) transformed to (funcall f ...)
         ((eq op 'flet)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-defs (cadr expr))
                     (body (caddr expr))
                     (fn-names (mapcar #'car fn-defs))
                     ;; Transform calls to local functions into funcall
                     (transform-calls (lambda (e)
                                        (labels ((xform (x)
                                                   (cond
                                                     ((not (consp x)) x)
                                                     ((and (symbolp (car x))
                                                           (member (car x) fn-names))
                                                      (cons 'funcall (cons (car x) (mapcar #'xform (cdr x)))))
                                                     (t (mapcar #'xform x)))))
                                          (xform e))))
                     ;; Build let bindings with lambdas (no need for nil + setq since non-recursive)
                     (let-bindings (mapcar (lambda (def)
                                             (let* ((name (car def))
                                                    (params (cadr def))
                                                    (fn-body (caddr def)))
                                               `(,name (lambda ,params ,fn-body))))
                                           fn-defs))
                     ;; Transform body calls
                     (xformed-body (funcall transform-calls body))
                     ;; Build: (let ((f (lambda ...)) ...) body')
                     (transformed `(let ,let-bindings ,xformed-body)))
                (compile-expr transformed env fenv))
              (list 'lit 0)))

         ;; Incf (increment in place)
         ((eq op 'incf)
          (if (consp (cdr expr))
              (let* ((place (cadr expr))
                     (delta (if (consp (cddr expr)) (caddr expr) 1)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (+ ,place ,delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Decf (decrement in place)
         ((eq op 'decf)
          (if (consp (cdr expr))
              (let* ((place (cadr expr))
                     (delta (if (consp (cddr expr)) (caddr expr) 1)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (- ,place ,delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Push (add to front of list variable)
         ((eq op 'push)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (place (caddr expr)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (cons ,item ,place)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Pop (remove and return first element)
         ((eq op 'pop)
          (if (consp (cdr expr))
              (let ((place (cadr expr)))
                (if (symbolp place)
                    ;; Returns car, then sets to cdr
                    ;; Need progn: (prog1 (car place) (setq place (cdr place)))
                    ;; For now, just return car (mutation happens but value is cdr)
                    (compile-expr `(car ,place) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Quote
         ((eq op 'quote)
          (if (consp (cdr expr))
              (quote->ir (cadr expr))
              (list 'lit #x0)))

         ;; Progn
         ((eq op 'progn)
          (let ((body (cdr expr)))
            (if body
                (cons 'progn (mapcar (lambda (form) (compile-expr form env fenv)) body))
                (list 'lit #x0))))

         ;; Conditional
         ((eq op 'if)
          (if (and (consp (cdr expr))
                   (consp (cddr expr))
                   (consp (cdddr expr)))
              (list 'if-expr
                    (compile-expr (cadr expr) env fenv)   ; test
                    (compile-expr (caddr expr) env fenv)  ; then
                    (compile-expr (cadddr expr) env fenv)) ; else
              ;; Two-arg if: (if test then) -> (if test then nil)
              (if (and (consp (cdr expr)) (consp (cddr expr)))
                  (list 'if-expr
                        (compile-expr (cadr expr) env fenv)
                        (compile-expr (caddr expr) env fenv)
                        (list 'lit #x0))
                  (list 'lit 0))))

         ;; Not (logical negation)
         ((eq op 'not)
          (if (consp (cdr expr))
              (list 'if-expr
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 0)   ; if true -> nil
                    (list 'lit 1))  ; if false -> t (1, will be tagged to #x10)
              (list 'lit 1))) ; (not) with no args -> t

         ;; Cond (multi-way conditional) - transforms to nested if
         ((eq op 'cond)
          (labels ((expand-cond (clauses)
                     (if (null clauses)
                         (list 'lit #x0)  ; no clause matched -> nil
                         (let* ((clause (car clauses))
                                (test (car clause))
                                (body (cdr clause)))
                           (cond
                             ;; (t body...) or (otherwise body...) - default clause
                             ((or (eq test t) (eq test 'otherwise))
                              (if body
                                  (if (cdr body)
                                      (compile-expr (cons 'progn body) env fenv)
                                      (compile-expr (car body) env fenv))
                                  (list 'lit 1))) ; bare t -> t (1, tags to #x10)
                             ;; Empty body: return test value if true
                             ((null body)
                              (let ((test-ir (compile-expr test env fenv)))
                                (list 'if-expr test-ir test-ir (expand-cond (cdr clauses)))))
                             ;; Normal clause with body
                             (t
                              (list 'if-expr
                                    (compile-expr test env fenv)
                                    (if (cdr body)
                                        (compile-expr (cons 'progn body) env fenv)
                                        (compile-expr (car body) env fenv))
                                    (expand-cond (cdr clauses)))))))))
            (expand-cond (cdr expr))))

         ;; When (guard form)
         ((eq op 'when)
          (if (consp (cdr expr))
              (let ((test (cadr expr))
                    (body (cddr expr)))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit #x0))
                      (list 'lit #x0)))
              (list 'lit #x0)))

         ;; Unless (negated guard form)
         ((eq op 'unless)
          (if (consp (cdr expr))
              (let ((test (cadr expr))
                    (body (cddr expr)))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (list 'lit #x0)  ; if true -> nil
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit #x0))))
              (list 'lit #x0)))

         ;; And (short-circuit conjunction)
         ((eq op 'and)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 1))  ; (and) -> t (1, tags to #x10)
              ((null (cdr args)) (compile-expr (car args) env fenv)) ; (and x) -> x
              (t ; (and a b ...) -> (if a (and b ...) nil)
               (list 'if-expr
                     (compile-expr (car args) env fenv)
                     (compile-expr (cons 'and (cdr args)) env fenv)
                     (list 'lit 0))))))

         ;; Or (short-circuit disjunction)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit #x0))  ; (or) -> nil
              ((null (cdr args)) (compile-expr (car args) env fenv)) ; (or x) -> x
              (t ; (or a b ...) -> need temp to preserve value
               ;; Simple version: (if a a (or b ...)) - evaluates a twice
               ;; For now use this; optimize later with let if needed
               (let ((first-ir (compile-expr (car args) env fenv)))
                 (list 'if-expr
                       first-ir
                       first-ir  ; returns first if true (evaluates twice)
                       (compile-expr (cons 'or (cdr args)) env fenv)))))))

         ;; Null predicate (also nil?)
         ((or (eq op 'null) (op= op "NIL?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Consp predicate (tag #x1) (also cons?)
         ((or (eq op 'consp) (op= op "CONS?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 1)) ; tag 1 (will be tagged to #x10, matching get-tag result)
              (list 'lit 0)))

         ;; Atom predicate (not consp)
         ((eq op 'atom)
          (if (consp (cdr expr))
              (list 'if-expr
                    (list 'cmp-eq
                          (list 'get-tag (compile-expr (cadr expr) env fenv))
                          (list 'lit 1)) ; cons tag
                    (list 'lit 0)   ; cons -> nil (not atom)
                    (list 'lit 1))  ; not cons -> t (is atom)
              (list 'lit 1)))

         ;; Numberp predicate (tag #x0 = fixnum) (also fixnum?)
         ((or (eq op 'numberp) (op= op "FIXNUM?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit #x0)) ; tag 0
              (list 'lit #x0)))

         ;; Symbolp predicate (tag #x2) (also symbol?)
         ((or (eq op 'symbolp) (op= op "SYMBOL?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 2)) ; tag 2
              (list 'lit 0)))

         ;; Stringp predicate (tag #x4)
         ((eq op 'stringp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 4)) ; tag 4
              (list 'lit 0)))

         ;; Vectorp predicate (tag #x3)
         ((eq op 'vectorp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 3)) ; tag 3
              (list 'lit 0)))

         ;; Functionp predicate (tag #x5 = closure)
         ((eq op 'functionp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 5)) ; tag 5
              (list 'lit 0)))

         ;; Listp predicate (null or cons)
         ((eq op 'listp)
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                ;; listp = (or (null x) (consp x))
                (list 'if-expr
                      (list 'cmp-eq arg-ir (list 'lit 0)) ; null check
                      (list 'lit 1) ; null -> t
                      (list 'cmp-eq
                            (list 'get-tag arg-ir)
                            (list 'lit 1)))) ; cons check (tag 1)
              (list 'lit 0)))

         ;; Zerop predicate
         ((eq op 'zerop)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Plusp predicate (> 0)
         ((eq op 'plusp)
          (if (consp (cdr expr))
              (list 'cmp-gt
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Minusp predicate (< 0)
         ((eq op 'minusp)
          (if (consp (cdr expr))
              (list 'cmp-lt
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Eq (pointer equality)
         ((eq op 'eq)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Eql (same as eq for fixnums, symbols, chars)
         ((eq op 'eql)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; 1+ (increment by 1)
         ((eq op '1+)
          (if (consp (cdr expr))
              (list 'add
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; 1- (decrement by 1)
         ((eq op '1-)
          (if (consp (cdr expr))
              (list 'sub
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; Abs (absolute value) - (if (< x 0) (- 0 x) x)
         ((eq op 'abs)
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt arg-ir (list 'lit 0))
                      (list 'sub (list 'lit 0) arg-ir)
                      arg-ir))
              (list 'lit 0)))

         ;; Max (maximum of two values)
         ((eq op 'max)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((a-ir (compile-expr (cadr expr) env fenv))
                    (b-ir (compile-expr (caddr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-gt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (consp (cdr expr))
                  (compile-expr (cadr expr) env fenv)
                  (list 'lit 0))))

         ;; Min (minimum of two values)
         ((eq op 'min)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((a-ir (compile-expr (cadr expr) env fenv))
                    (b-ir (compile-expr (caddr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (consp (cdr expr))
                  (compile-expr (cadr expr) env fenv)
                  (list 'lit 0))))

         ;; Cadr (car of cdr)
         ((eq op 'cadr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Caddr (car of cdr of cdr)
         ((eq op 'caddr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Cadddr (car of cdr of cdr of cdr)
         ((eq op 'cadddr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Cddr (cdr of cdr)
         ((eq op 'cddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Cdddr (cdr of cdr of cdr)
         ((eq op 'cdddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Cddddr (cdr of cdr of cdr of cdr)
         ((eq op 'cddddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Caar (car of car)
         ((eq op 'caar)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'car-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Cdar (cdr of car)
         ((eq op 'cdar)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'car-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; First (same as car)
         ((eq op 'first)
          (if (consp (cdr expr))
              (list 'car-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Second (same as cadr)
         ((eq op 'second)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Third (same as caddr)
         ((eq op 'third)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Fourth
         ((eq op 'fourth)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Fifth
         ((eq op 'fifth)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call
                                      (list 'cdr-call (compile-expr (cadr expr) env fenv))))))
              (list 'lit 0)))

         ;; Rest (same as cdr)
         ((eq op 'rest)
          (if (consp (cdr expr))
              (list 'cdr-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; List (create list from args)
         ((eq op 'list)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 0)  ; (list) -> nil
                (labels ((build-list (items)
                           (if (null items)
                               (list 'lit 0)
                               (list 'cons-call
                                     (compile-expr (car items) env fenv)
                                     (build-list (cdr items))))))
                  (build-list args)))))

         ;; List* (last arg is tail, rest consed)
         ((eq op 'list*)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))
              ((null (cdr args)) (compile-expr (car args) env fenv))
              (t (labels ((build-list* (items)
                            (if (null (cdr items))
                                (compile-expr (car items) env fenv)
                                (list 'cons-call
                                      (compile-expr (car items) env fenv)
                                      (build-list* (cdr items))))))
                   (build-list* args))))))

         ;; Acons (add to front of alist: (cons (cons key val) alist))
         ((eq op 'acons)
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (list 'cons-call
                    (list 'cons-call
                          (compile-expr (cadr expr) env fenv)   ; key
                          (compile-expr (caddr expr) env fenv)) ; value
                    (compile-expr (cadddr expr) env fenv))      ; alist
              (list 'lit 0)))

         ;; Nth (get nth element) - compile-time unrolled for small n
         ((eq op 'nth)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((n-expr (cadr expr))
                    (list-ir (compile-expr (caddr expr) env fenv)))
                (if (and (integerp n-expr) (<= n-expr 10))
                    ;; Small constant index: unroll to car/cdr chain
                    (let ((result list-ir))
                      (dotimes (i n-expr)
                        (setf result (list 'cdr-call result)))
                      (list 'car-call result))
                    ;; Variable index: would need runtime helper
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Nthcdr (get nth tail) - compile-time unrolled for small n
         ((eq op 'nthcdr)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((n-expr (cadr expr))
                    (list-ir (compile-expr (caddr expr) env fenv)))
                (if (and (integerp n-expr) (<= n-expr 10))
                    ;; Small constant index: unroll to cdr chain
                    (let ((result list-ir))
                      (dotimes (i n-expr)
                        (setf result (list 'cdr-call result)))
                      result)
                    ;; Variable index: would need runtime helper
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Elt (generic element access - same as nth for lists)
         ((eq op 'elt)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((seq-ir (compile-expr (cadr expr) env fenv))
                    (idx-expr (caddr expr)))
                (if (and (integerp idx-expr) (<= idx-expr 10))
                    (let ((result seq-ir))
                      (dotimes (i idx-expr)
                        (setf result (list 'cdr-call result)))
                      (list 'car-call result))
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Identity function
         ((eq op 'identity)
          (if (consp (cdr expr))
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Constantly - returns first arg (simplified for constant arg)
         ((eq op 'constantly)
          (if (consp (cdr expr))
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Length - recursive list length
         ((eq op 'length)
          (if (consp (cdr expr))
              (let ((lst-arg (cadr expr)))
                ;; Transform to: (labels ((len (lst) (if (null lst) 0 (+ 1 (len (cdr lst)))))) (len arg))
                (compile-expr
                 `(labels ((len-iter (lst acc)
                             (if (null lst)
                                 acc
                                 (len-iter (cdr lst) (+ acc #x1)))))
                    (len-iter ,lst-arg #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Append - recursive list append (two-argument version)
         ((eq op 'append)
          (cond
            ;; (append) -> nil
            ((null (cdr expr)) (list 'lit 0))
            ;; (append x) -> x
            ((null (cddr expr)) (compile-expr (cadr expr) env fenv))
            ;; (append x y) -> recursive implementation
            ((null (cdddr expr))
             (let ((lst1 (cadr expr))
                   (lst2 (caddr expr)))
               (compile-expr
                `(labels ((app (xs ys)
                            (if (null xs)
                                ys
                                (cons (car xs) (app (cdr xs) ys)))))
                   (app ,lst1 ,lst2))
                env fenv)))
            ;; (append x y z ...) -> (append x (append y z ...))
            (t (compile-expr
                `(append ,(cadr expr) (append ,@(cddr expr)))
                env fenv))))

         ;; Assoc - find key in alist
         ((eq op 'assoc)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((key (cadr expr))
                    (alist (caddr expr)))
                (compile-expr
                 `(labels ((assoc-iter (k al)
                             (if (null al)
                                 #x0
                                 (if (eq k (car (car al)))
                                     (car al)
                                     (assoc-iter k (cdr al))))))
                    (assoc-iter ,key ,alist))
                 env fenv))
              (list 'lit 0)))

         ;; Member - find item in list
         ((eq op 'member)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((member-iter (x xs)
                             (if (null xs)
                                 #x0
                                 (if (eq x (car xs))
                                     xs
                                     (member-iter x (cdr xs))))))
                    (member-iter ,item ,lst))
                 env fenv))
              (list 'lit 0)))

         ;; Reverse - reverse a list
         ((eq op 'reverse)
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((rev (xs acc)
                             (if (null xs)
                                 acc
                                 (rev (cdr xs) (cons (car xs) acc)))))
                    (rev ,lst #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Dotimes - (dotimes (i n [result]) body...)
         ;; Iterates i from 0 to n-1, returns result (default nil)
         ((eq op 'dotimes)
          (if (and (consp (cdr expr)) (consp (cadr expr)))
              (let* ((var-form (cadr expr))
                     (var (car var-form))
                     (count-form (cadr var-form))
                     (result-form (if (consp (cddr var-form)) (caddr var-form) #x0))
                     (body (cddr expr))
                     (body-expr (if (null (cdr body)) (car body) `(progn ,@body))))
                (compile-expr
                 `(labels ((dotimes-iter (,var limit)
                             (if (>= ,var limit)
                                 ,result-form
                                 (progn
                                   ,body-expr
                                   (dotimes-iter (+ ,var #x1) limit)))))
                    (dotimes-iter #x0 ,count-form))
                 env fenv))
              (list 'lit 0)))

         ;; Dolist - (dolist (x list [result]) body...)
         ;; Iterates over each element of list, returns result (default nil)
         ((eq op 'dolist)
          (if (and (consp (cdr expr)) (consp (cadr expr)))
              (let* ((var-form (cadr expr))
                     (var (car var-form))
                     (list-form (cadr var-form))
                     (result-form (if (consp (cddr var-form)) (caddr var-form) #x0))
                     (body (cddr expr))
                     (body-expr (if (null (cdr body)) (car body) `(progn ,@body))))
                (compile-expr
                 `(labels ((dolist-iter (remaining)
                             (if (null remaining)
                                 ,result-form
                                 (let ((,var (car remaining)))
                                   (progn
                                     ,body-expr
                                     (dolist-iter (cdr remaining)))))))
                    (dolist-iter ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapcar - (mapcar fn list) - apply fn to each element, return list of results
         ((eq op 'mapcar)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapcar-iter (fn lst)
                             (if (null lst)
                                 #x0
                                 (cons (funcall fn (car lst))
                                       (mapcar-iter fn (cdr lst))))))
                    (mapcar-iter ,fn-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapc - (mapc fn list) - like mapcar but returns the original list
         ((eq op 'mapc)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapc-iter (fn lst orig)
                             (if (null lst)
                                 orig
                                 (progn
                                   (funcall fn (car lst))
                                   (mapc-iter fn (cdr lst) orig)))))
                    (let ((the-list ,list-form))
                      (mapc-iter ,fn-form the-list the-list)))
                 env fenv))
              (list 'lit 0)))

         ;; Reduce - (reduce fn list &optional init) - fold function over list
         ((eq op 'reduce)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-form (cadr expr))
                     (list-form (caddr expr))
                     (init-form (if (consp (cdddr expr)) (cadddr expr) nil)))
                (if init-form
                    ;; With initial value
                    (compile-expr
                     `(labels ((reduce-iter (fn lst acc)
                                 (if (null lst)
                                     acc
                                     (reduce-iter fn (cdr lst) (funcall fn acc (car lst))))))
                        (reduce-iter ,fn-form ,list-form ,init-form))
                     env fenv)
                    ;; Without initial value - use first element
                    (compile-expr
                     `(labels ((reduce-iter (fn lst acc)
                                 (if (null lst)
                                     acc
                                     (reduce-iter fn (cdr lst) (funcall fn acc (car lst))))))
                        (let ((the-list ,list-form))
                          (if (null the-list)
                              #x0
                              (reduce-iter ,fn-form (cdr the-list) (car the-list)))))
                     env fenv)))
              (list 'lit 0)))

         ;; Apply - (apply fn args-list) - call fn with args spread from list
         ;; For the patterns used in the compiler, we optimize known functions
         ((eq op 'apply)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (args-form (caddr expr)))
                ;; Check for (apply #'append ...) or (apply #'max ...)
                (cond
                  ;; (apply #'append list-of-lists) -> fold append over list
                  ((and (consp fn-form)
                        (or (eq (car fn-form) 'function)
                            (eq (car fn-form) 'quote))
                        (eq (cadr fn-form) 'append))
                   (compile-expr
                    `(labels ((apply-append (lists acc)
                                (if (null lists)
                                    acc
                                    (apply-append (cdr lists) (append acc (car lists))))))
                       (apply-append ,args-form nil))
                    env fenv))
                  ;; (apply #'max list-of-numbers) -> reduce max over list
                  ((and (consp fn-form)
                        (or (eq (car fn-form) 'function)
                            (eq (car fn-form) 'quote))
                        (eq (cadr fn-form) 'max))
                   (compile-expr
                    `(labels ((apply-max (lst best)
                                (if (null lst)
                                    best
                                    (let ((el (car lst)))
                                      (apply-max (cdr lst) (if (> el best) el best))))))
                       (let ((the-list ,args-form))
                         (if (null the-list)
                             #x0
                             (apply-max (cdr the-list) (car the-list)))))
                    env fenv))
                  ;; General apply - dispatch based on arg count (up to 5 args)
                  (t
                   (compile-expr
                    `(let ((fn ,fn-form)
                           (args ,args-form))
                       (let ((len (length args)))
                         (cond
                           ((= len #x0) (funcall fn))
                           ((= len #x1) (funcall fn (car args)))
                           ((= len #x2) (funcall fn (car args) (cadr args)))
                           ((= len #x3) (funcall fn (car args) (cadr args) (caddr args)))
                           ((= len #x4) (funcall fn (car args) (cadr args) (caddr args) (cadddr args)))
                           (t (funcall fn (car args) (cadr args) (caddr args) (cadddr args)
                                       (car (cddddr args)))))))
                    env fenv))))
              (list 'lit 0)))

         ;; Loop - compile-time transformation to labels + recursion
         ;; Simple implementation for common patterns used in compiler
         ((eq op 'loop)
          (let* ((clauses (cdr expr))
                 (result-form nil))
            ;;(format t "[LOOP DEBUG] clauses=~S len=~A~%" clauses (length clauses))
            ;;(format t "[LOOP DEBUG] (car clauses)=~S (eq? ~A)~%" (car clauses) (eq (car clauses) 'for))
            ;;(format t "[LOOP DEBUG] (caddr clauses)=~S~%" (caddr clauses))
            ;; Parse clauses
            (cond
              ;; (loop for var in list collect expr)
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "COLLECT"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (collect-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var acc)
                                   (if (null ,list-var)
                                       (reverse acc)
                                       (let ((,var (car ,list-var)))
                                         (iter (cdr ,list-var) (cons ,collect-expr acc))))))
                          (iter ,list-expr nil)))))
              ;; (loop for var from start below end collect expr)
              ((and (>= (length clauses) 7)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "FROM")
                    (op= (car (cddddr clauses)) "BELOW")
                    (op= (caddr (cddddr clauses)) "COLLECT"))
               (let ((var (cadr clauses))
                     (start (cadddr clauses))
                     (end (cadr (cddddr clauses)))
                     (collect-expr (cadddr (cddddr clauses))))
                 (setq result-form
                       `(labels ((iter (,var acc)
                                   (if (>= ,var ,end)
                                       (reverse acc)
                                       (iter (+ ,var #x1) (cons ,collect-expr acc)))))
                          (iter ,start nil)))))
              ;; (loop for var across vec collect expr)
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "ACROSS")
                    (op= (car (cddddr clauses)) "COLLECT"))
               (let ((var (cadr clauses))
                     (vec-expr (cadddr clauses))
                     (collect-expr (cadr (cddddr clauses))))
                 (setq result-form
                       `(let ((vec ,vec-expr))
                          (labels ((iter (idx acc)
                                     (if (>= idx (length vec))
                                         (reverse acc)
                                         (let ((,var (elt vec idx)))
                                           (iter (+ idx #x1) (cons ,collect-expr acc))))))
                            (iter #x0 nil))))))
              ;; (loop for var in list for idx from 0 do body)
              ((and (>= (length clauses) 8)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "FOR")
                    (op= (caddr (cddddr clauses)) "FROM")
                    (op= (car (cddddr (cddddr clauses))) "DO"))
               (let* ((var1 (cadr clauses))
                      (list-expr (cadddr clauses))
                      (var2 (cadr (cddddr clauses)))
                      (start (cadddr (cddddr clauses)))
                      (do-expr (cadr (cddddr (cddddr clauses))))
                      (list-var (intern (concatenate 'string (symbol-name var1) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var ,var2)
                                   (if (null ,list-var)
                                       nil
                                       (let ((,var1 (car ,list-var)))
                                         (progn
                                           ,do-expr
                                           (iter (cdr ,list-var) (+ ,var2 #x1)))))))
                          (iter ,list-expr ,start)))))
              ;; (loop until condition do body)
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "UNTIL")
                    (op= (caddr clauses) "DO"))
               (let ((condition (cadr clauses))
                     (do-expr (cadddr clauses)))
                 (setq result-form
                       `(labels ((iter ()
                                   (if ,condition
                                       nil
                                       (progn
                                         ,do-expr
                                         (iter)))))
                          (iter)))))
              (t
               (setq result-form '(lit 0))))
            (compile-expr result-form env fenv)))

         ;; Error - stub implementation (print message and exit)
         ;; For now, just ignore the message and return 0
         ;; A full implementation would print to stderr and exit with error code
         ((op= op "ERROR")
          (list 'lit 0))  ; Stub: just return 0 for now

         ;; Remove-if - filter out elements that match predicate
         ((op= op "REMOVE-IF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((filter-iter (remaining acc)
                             (if (null remaining)
                                 (reverse acc)
                                 (if (funcall ,pred (car remaining))
                                     (filter-iter (cdr remaining) acc)
                                     (filter-iter (cdr remaining) (cons (car remaining) acc))))))
                    (filter-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-if-not - filter out elements that don't match predicate
         ((op= op "REMOVE-IF-NOT")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((filter-iter (remaining acc)
                             (if (null remaining)
                                 (reverse acc)
                                 (if (funcall ,pred (car remaining))
                                     (filter-iter (cdr remaining) (cons (car remaining) acc))
                                     (filter-iter (cdr remaining) acc)))))
                    (filter-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-duplicates - remove duplicate elements
         ((op= op "REMOVE-DUPLICATES")
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((dedup-iter (remaining seen)
                             (if (null remaining)
                                 (reverse seen)
                                 (let ((el (car remaining)))
                                   (if (member el seen)
                                       (dedup-iter (cdr remaining) seen)
                                       (dedup-iter (cdr remaining) (cons el seen)))))))
                    (dedup-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

        ;; Cons
         ((eq op 'cons)
         (if (and (consp (cdr expr)) (consp (cddr expr)))
             (list 'cons-call
                   (compile-expr (cadr expr) env fenv)
                   (compile-expr (caddr expr) env fenv))
             (list 'lit 0)))

         ;; Vector ref
         ((op= op "VECTOR-REF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'vector-ref
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Symbol-name
         ((op= op "SYMBOL-NAME")
          (if (consp (cdr expr))
              (list 'symbol-name (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; String length
         ((op= op "STRING-LENGTH")
          (if (consp (cdr expr))
              (list 'string-len (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Get tag
         ((op= op "GET-TAG")
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                (cond
                  ((has-tag? arg-ir 'string-lit) (list 'lit #x4))
                  ((has-tag? arg-ir 'symbol-lit) (list 'lit #x2))
                  ((has-tag? arg-ir 'vector-lit) (list 'lit #x3))
                  (t (list 'get-tag arg-ir))))
              (list 'lit 0)))

         ;; Packages: no-op except find-symbol folded to symbol literal
         ((op= op "DEFPACKAGE") (list 'lit 0))
         ((op= op "IN-PACKAGE") (list 'lit 0))
         ((op= op "USE-PACKAGE") (list 'lit 0))
         ((op= op "EXPORT") (list 'lit 0))
         ((op= op "IMPORT") (list 'lit 0))
         ((op= op "FIND-SYMBOL")
          (let* ((name-expr (cadr expr))
                 (name (cond
                         ((and (consp name-expr) (op= (car name-expr) "QUOTE") (symbolp (cadr name-expr)))
                          (symbol-name (cadr name-expr)))
                         ((symbolp name-expr) (symbol-name name-expr))
                         ((stringp name-expr) name-expr)
                         (t nil))))
            (if name
                (list 'symbol-lit (string-upcase name))
                (list 'lit 0))))

         ;; Quasiquote
         ((op= op "QUASIQUOTE")
          (if (consp (cdr expr))
              (expand-quasiquote-ir (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Lambda/closure
         ((op= op "LAMBDA")
         (let* ((raw-params (cadr expr))
                (body (caddr expr))
                (lambda-name (gensym "lambda-"))
                (outer-max (if env (apply #'max (mapcar #'cdr env)) -1)))
           (multiple-value-bind (fixed optional rest) (parse-params raw-params)
             (let* ((optional-names (mapcar #'car optional))
                    (optional-supplied (mapcar (lambda (entry)
                                                 (or (caddr entry) (gensym "supplied-")))
                                               optional))
                    (bindings (append fixed optional-names optional-supplied (if rest (list rest) nil)))
                    (param-env-detect (env-extend (mapcar #'list bindings) env))
                    (opt-inits (mapcar (lambda (entry)
                                         (let ((init (cadr entry)))
                                           (if init
                                               (compile-expr init param-env-detect fenv)
                                               '(lit 0))))
                                       optional))
                    (body-ir-base (compile-expr body param-env-detect fenv))
                    (captured-offsets (remove-if-not (lambda (off) (<= off outer-max))
                                                     (collect-var-offsets body-ir-base)))
                    (capture-map (let ((idx 0))
                                   (mapcar (lambda (off)
                                             (prog1 (cons off idx)
                                               (incf idx)))
                                           captured-offsets)))
                    (body-ir (rewrite-captures body-ir-base capture-map))
                    (compiled (list lambda-name fixed optional-names opt-inits optional-supplied body-ir captured-offsets (1+ outer-max) rest)))
               (push compiled *collected-lambdas*)
               (list 'lambda-ref lambda-name)))))

         ;; Car
         ((eq op 'car)
          (if (consp (cdr expr))
              (list 'car-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Cdr
         ((eq op 'cdr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Funcall: call closure value
         ((eq op 'funcall)
          (let ((fn-expr (cadr expr))
                (args (cddr expr)))
            (list 'call-closure
                  (compile-expr fn-expr env fenv)
                  (mapcar (lambda (arg) (compile-expr arg env fenv)) args))))

         ;; Inline lambda application: ((lambda (...) ...) args...)
         ((consp op)
          (let ((fn (compile-expr op env fenv))
                (args (mapcar (lambda (arg) (compile-expr arg env fenv)) (cdr expr))))
            (list 'call-closure fn args)))

         ;; Function call - check if it's a user-defined function
         (t
          ;; Try to look up as a user function
          (if (and fenv (assoc op fenv))
              ;; It's a user-defined function
              (let ((args (cdr expr)))
                (list 'call-fn op
                      (mapcar (lambda (arg) (compile-expr arg env fenv)) args)))
              ;; Unknown operation
              (list 'lit 0))))))

    ;; Unknown
    (t (list 'lit 0))))

(defun codegen-main-with-runtime (ir runtime-addrs)
  "Generate main function with runtime table support
   Calling convention: x0 = runtime table pointer
   Prologue saves x19-x24 for runtime table, environment base, and callee-saved temps"
  (let ((body (codegen-expr ir runtime-addrs nil nil 0)))
    ;; Allocate 1024 bytes: ample space for locals/env below caller frame
    (append (arm64-sub-imm 31 31 1024)     ; SUB sp, sp, #1024 (stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            ;; Set x20 to point to environment area well inside frame
            (arm64-add-imm 20 31 384)     ; ADD x20, sp, #x180
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 1024)     ; ADD sp, sp, #1024 (restore stack)
            (arm64-ret))))

(defun compile-to-arm64-with-runtime (expr runtime-addrs)
  (codegen-main-with-runtime (compile-expr expr nil nil) runtime-addrs))

(defun compile-to-arm64 (expr)
  (compile-to-arm64-with-runtime expr nil))

;;; ============================================
;;; Multi-Function Compilation Stubs
;;; ============================================

(defun count-instrs (code)
  "Count number of 4-byte instructions in code list"
  (if (null code)
      0
      (+ 1 (count-instrs (nthcdr 4 code)))))

(defun parse-params (params)
  "Split params into fixed list, optional descriptors, and rest symbol.
Optional descriptors are (name init-form supplied-name)."
  (let ((fixed '())
        (optional '())
        (rest nil)
        (state :fixed))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setf state :optional))
        ((eq p '&rest) (setf state :rest))
        ((eq state :fixed) (push (if (symbolp p) p (car p)) fixed))
        ((eq state :optional)
         (cond
           ((symbolp p) (push (list p nil nil) optional))
           ((consp p)
            (let ((name (car p))
                  (init (cadr p))
                  (supplied (caddr p)))
              (push (list name init supplied) optional)))))
        ((eq state :rest) (setf rest p))))
    (values (nreverse fixed) (nreverse optional) rest)))

(defun compile-defun (name params body env fenv)
  "Compile defun into (name fixed optional body-ir captures param-base rest-param)"
  ;; Create environment with parameters as the initial bindings
  (multiple-value-bind (fixed optional rest) (parse-params params)
    (let* ((optional-names (mapcar #'car optional))
           (optional-supplied (mapcar (lambda (entry)
                                        (or (caddr entry) (gensym "supplied-")))
                                      optional))
           (bindings (append fixed optional-names optional-supplied (if rest (list rest) nil)))
           (param-env (env-extend (mapcar #'list bindings) env))
           (param-base (if bindings
                           (env-lookup (car bindings) param-env)
                           0))
           (opt-inits (mapcar (lambda (entry)
                                (let ((init (cadr entry)))
                                  (if init
                                      (compile-expr init param-env fenv)
                                      '(lit 0))))
                              optional))
          ;; Add this function to fenv to allow recursive calls
          ;; Use a placeholder compiled-fn since we're still compiling it
          (recursive-fenv (cons (cons name nil) fenv))
          ;; Compile body in the parameter environment with recursive fenv
          (body-ir (compile-expr body param-env recursive-fenv)))
      (list name fixed optional-names opt-inits optional-supplied body-ir nil param-base rest))))

(defun compile-forms-helper (forms env fenv)
  "Compile list of forms, separating defuns from main expression
   Returns: (list-of-compiled-functions main-expression-ir)"
  (if (consp forms)
      (let ((form (car forms)))
        (if (and (consp form) (eq (car form) 'defun))
            ;; It's a defun
            (let* ((name (cadr form))
                   (params (caddr form))
                   (body (cadddr form))
                   (compiled-fn (compile-defun name params body env fenv))
                   ;; Add to function environment
                   (new-fenv (cons (cons name compiled-fn) fenv))
                   ;; Compile rest of forms
                   (rest-result (compile-forms-helper (cdr forms) env new-fenv))
                   (rest-fns (car rest-result))
                   (main-ir (cadr rest-result)))
              ;; Return accumulated functions and main expression
              (list (cons compiled-fn rest-fns) main-ir))
            ;; Not a defun - this is the main expression
            (list nil (compile-expr form env fenv))))
      ;; No more forms
      (list nil '(lit 0))))

(defun compile-forms (forms)
  "Stub: compile list of top-level forms"
  (let ((*collected-lambdas* nil))
    (let* ((result (compile-forms-helper forms nil nil))
           (fns (car result))
           (main-ir (cadr result)))
      (list (append fns (nreverse *collected-lambdas*)) main-ir))))

(defun codegen-function-with-params (params optional-names optional-inits optional-supplied body-ir runtime-addrs &optional fn-offsets current-offset param-base rest-param)
  "Generate code for function with parameters
   Parameters are passed in x0-x7, stored to stack for access as variables"
  (let* ((required-count (length params))
         (optional-count (length optional-names))
         (supplied-count (length optional-supplied))
         (total-non-rest (+ required-count optional-count))
         (has-rest (not (null rest-param)))
         (rest-offset (if has-rest (+ param-base total-non-rest supplied-count) nil))
         (prologue-size 6)
         ;; Cache temp slots for incoming args to preserve register values while building &rest or filling optionals
         (arg0-slot (temp-slot-offset 0))
         (arg1-slot (temp-slot-offset 1))
         (arg2-slot (temp-slot-offset 2))
         (arg3-slot (temp-slot-offset 3))
         (arg4-slot (temp-slot-offset 4))
         (need-arg-save (or has-rest (> optional-count 0)))
         (arg-save-code (when need-arg-save
                          (append
                           (arm64-str 0 31 arg0-slot)
                           (arm64-str 1 31 arg1-slot)
                           (arm64-str 2 31 arg2-slot)
                           (arm64-str 3 31 arg3-slot)
                           (arm64-str 4 31 arg4-slot))))
         ;; Store fixed parameters. For &rest, load from saved slots to avoid clobbering incoming registers.
         (param-store-code
           (let ((code (if need-arg-save arg-save-code nil)))
             (dotimes (i required-count)
               (let* ((param-offset (* (+ param-base i) 8))
                      (load-arg
                        (if (< i 5)
                            (if need-arg-save
                                (arm64-ldr 22 31 (cond
                                                   ((= i 0) arg0-slot)
                                                   ((= i 1) arg1-slot)
                                                   ((= i 2) arg2-slot)
                                                   ((= i 3) arg3-slot)
                                                   (t arg4-slot)))
                                (arm64-mov 22 i))
                            (arm64-ldr 22 25 (* (- i 5) #x8))))
                      (store (append
                               load-arg
                               (arm64-sub-imm 21 20 param-offset)
                               (arm64-str 22 21 0))))
                 (setf code (append code store))))
             code))
         (param-store-size (count-instrs param-store-code))
         (optional-code
           (let ((code '())
                 (cursor (+ prologue-size param-store-size)))
             (dotimes (i optional-count)
               (let* ((opt-offset (* (+ param-base required-count i) 8))
                      (addr-reg 21)
                      (idx-reg 12)
                      (threshold (+ required-count i))
                      (default-expr-ir (nth i optional-inits))
                      (default-eval (codegen-expr default-expr-ir runtime-addrs fn-offsets
                                                  (if current-offset
                                                      (+ current-offset cursor)
                                                      nil)
                                                  0))
                      (store-default (append
                                      (arm64-sub-imm addr-reg 20 opt-offset)
                                      (arm64-str 0 addr-reg 0)))
                      (default-block (append default-eval store-default))
                      (supplied-value
                        (cond
                          ((= threshold 0) (arm64-ldr 22 31 arg0-slot))
                          ((= threshold 1) (arm64-ldr 22 31 arg1-slot))
                          ((= threshold 2) (arm64-ldr 22 31 arg2-slot))
                          ((= threshold 3) (arm64-ldr 22 31 arg3-slot))
                          ((= threshold 4) (arm64-ldr 22 31 arg4-slot))
                          (t (arm64-ldr 22 25 (* (- threshold 5) #x8)))))
                      (supplied-offset (* (+ param-base required-count optional-count i) 8))
                      (store-supplied-flag (append
                                             (arm64-movz 0 #x10)
                                             (arm64-sub-imm addr-reg 20 supplied-offset)
                                             (arm64-str 0 addr-reg 0)))
                      (store-supplied (append
                                       supplied-value
                                       (arm64-sub-imm addr-reg 20 opt-offset)
                                       (arm64-str 22 addr-reg 0)
                                       store-supplied-flag))
                      (store-default-flag (append
                                            (arm64-movz 0 0)
                                            (arm64-sub-imm addr-reg 20 supplied-offset)
                                            (arm64-str 0 addr-reg 0)))
                      (default-block (append default-eval store-default store-default-flag))
                      (skip-default (+ (count-instrs default-block) 1))
                      (skip-to-default (+ (count-instrs store-supplied) 2))
                      (block (append
                              (arm64-movz idx-reg threshold)
                              (arm64-cmp 23 idx-reg)
                              (arm64-b-cond #xD skip-to-default) ; if arg_count <= threshold -> default
                              store-supplied
                              (arm64-b skip-default)
                              default-block))
                      (block-len (count-instrs block)))
                 (setf code (append code block))
                 (incf cursor block-len)))
             code))
         (optional-size (count-instrs optional-code))
         ;; Store remaining extra list to rest param if needed
         (rest-code
          (when has-rest
            (let* ((rest-list-reg 13)
                   (idx-reg 12)
                   (arg-reg 14)
                   (five-reg 15)
                   (addr-reg 16)
                   (limit-reg 17)
                   (offset-reg 10)
                   (init-code (append
                                (arm64-movz rest-list-reg #x0)
                                (arm64-mov idx-reg 23)
                                (arm64-sub-imm idx-reg idx-reg #x1)
                                (arm64-movz five-reg #x5)
                                (arm64-movz limit-reg total-non-rest)))
                   (extras-load (append
                                  (arm64-sub-imm offset-reg idx-reg #x5)
                                  (arm64-lsl offset-reg offset-reg 3)
                                  (arm64-add offset-reg 25 offset-reg)
                                  (arm64-ldr arg-reg offset-reg 0)))
                   (reg-load (append
                               (arm64-add-imm addr-reg 31 arg0-slot)
                               (arm64-lsl offset-reg idx-reg 3)
                               (arm64-add addr-reg addr-reg offset-reg)
                               (arm64-ldr arg-reg addr-reg 0)))
                   (cons-body (append
                                (arm64-mov 0 arg-reg)
                                (arm64-mov 1 rest-list-reg)
                                (arm64-ldr 9 19 0)
                                (arm64-blr 9)
                                (arm64-mov rest-list-reg 0)
                                (arm64-sub-imm idx-reg idx-reg #x1)))
                   (extras-len (count-instrs extras-load))
                   (reg-len (count-instrs reg-load))
                   (cons-core-len (count-instrs cons-body))
                   (offset-to-done (+ extras-len reg-len cons-core-len 5))
                   (offset-to-reg (+ extras-len 2))
                   (offset-to-cons (+ reg-len 1))
                   (loop-back-offset (- (+ extras-len reg-len cons-core-len 5))))
              (append
               init-code
               (arm64-cmp idx-reg limit-reg)
               (arm64-b-cond #xB offset-to-done)
               (arm64-cmp idx-reg five-reg)
               (arm64-b-cond #xB offset-to-reg)
               extras-load
               (arm64-b offset-to-cons)
               reg-load
               cons-body
               (arm64-b loop-back-offset)
               (arm64-sub-imm 1 20 (* rest-offset 8))
               (arm64-str rest-list-reg 1 0)))))
         (rest-size (count-instrs rest-code))
         (body-offset (if current-offset
                          (+ current-offset prologue-size param-store-size optional-size rest-size)
                          nil))
         ;; Pass fn-offsets and body-offset to body generation
         (body (codegen-expr body-ir runtime-addrs fn-offsets body-offset 0)))
    (append
      ;; Function prologue
      (arm64-sub-imm 31 31 *stack-frame-size*)      ; Allocate stack frame
      (arm64-stp 29 30 31 0)         ; Save FP/LR
      (arm64-stp 19 20 31 16)        ; Save x19/x20
      (arm64-stp 21 22 31 32)        ; Save x21/x22
      (arm64-stp 23 24 31 48)        ; Save x23/x24
      ;; x25 may carry extra-arg pointer from caller; leave intact
      ;; x25 may carry extra-arg pointer from caller; leave intact
      ;; x19 already has runtime table from caller - don't overwrite!
      (arm64-add-imm 20 31 *env-base-offset*)      ; Set environment base

      ;; Store parameters to stack
      param-store-code

      ;; Handle optional parameters
      optional-code

      ;; Build &rest if present
      rest-code

      ;; Function body
      body

      ;; Function epilogue
      (arm64-ldp 23 24 31 48)        ; Restore x23/x24
      (arm64-ldp 21 22 31 32)        ; Restore x21/x22
      (arm64-ldp 19 20 31 16)        ; Restore x19/x20
      (arm64-ldp 29 30 31 0)         ; Restore FP/LR
      (arm64-add-imm 31 31 *stack-frame-size*)      ; Deallocate stack
      (arm64-ret))))

(defun calculate-function-offsets (compiled-fns start-offset runtime-addrs)
  "First pass: calculate function offsets by generating code without fn-offsets"
  (if (consp compiled-fns)
(destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
          (car compiled-fns)
        (let* (;; Generate without fn-offsets to get size
               (fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs nil nil param-base rest-param))
               (fn-size (count-instrs fn-code))
               ;; Recursively calculate rest
               (rest-offsets (calculate-function-offsets (cdr compiled-fns)
                                                         (+ start-offset fn-size)
                                                         runtime-addrs)))
          (cons (list name start-offset captures param-base rest-param) rest-offsets)))
      nil))

(defun codegen-functions-with-offsets (compiled-fns fn-offsets current-offset runtime-addrs)
  "Second pass: generate functions with correct fn-offsets"
  (if (consp compiled-fns)
(destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
          (car compiled-fns)
        ;; Generate with fn-offsets for proper function calls
        (let* ((fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs
                                                      fn-offsets current-offset param-base rest-param))
               (fn-size (count-instrs fn-code))
               ;; Generate rest
               (rest-code (codegen-functions-with-offsets (cdr compiled-fns) fn-offsets
                                                          (+ current-offset fn-size)
                                                          runtime-addrs)))
          (append fn-code rest-code)))
      nil))

(defun codegen-functions-helper (compiled-fns current-offset runtime-addrs)
  "Generate code for all compiled functions using iterative offset stabilization.
   Returns: (total-code function-offsets)"
  (let ((fn-offsets (calculate-function-offsets compiled-fns current-offset runtime-addrs))
        (stable nil)
        (codes nil))
    (loop until stable do
      (let ((current current-offset)
            (new-offsets '())
            (new-codes '()))
        (dolist (fn compiled-fns)
          (destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
              fn
            (let* ((fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs
                                                          fn-offsets current param-base rest-param))
                   (fn-size (count-instrs fn-code)))
              (push fn-code new-codes)
              (push (list name current captures param-base rest-param) new-offsets)
              (incf current fn-size))))
        (setf new-offsets (nreverse new-offsets))
        (setf new-codes (nreverse new-codes))
        (if (equal (mapcar #'cadr new-offsets) (mapcar #'cadr fn-offsets))
            (setf stable t)
            (setf fn-offsets new-offsets))
        (when stable
          (setf codes new-codes))))
    (list (apply #'append codes) fn-offsets)))

(defun codegen-expr-with-fns (ir runtime-addrs fn-offsets current-offset)
  "Codegen with function offset tracking"
  (codegen-expr ir runtime-addrs fn-offsets current-offset 0))

(defun codegen-main-with-runtime-and-fns (ir runtime-addrs fn-offsets current-offset)
  "Generate main code with function offsets for calls"
  ;; Pass function offsets through to codegen
  ;; The body comes after the 7-instruction prologue
  (let ((body (codegen-expr-with-fns ir runtime-addrs fn-offsets (+ current-offset 7))))
    ;; Same prologue/epilogue as before
    (append (arm64-sub-imm 31 31 1024)     ; SUB sp, sp, #1024 (stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            (arm64-add-imm 20 31 384)     ; ADD x20, sp, #x180
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 1024)     ; ADD sp, sp, #1024 (restore stack)
            (arm64-ret))))

(defun compile-program-with-functions-with-runtime (forms runtime-addrs)
  "Compile entire program with function definitions
   Returns: complete machine code with main at offset 0 (entry point)"
  (let* ((compile-result (compile-forms forms))
         (compiled-fns (car compile-result))
         (main-ir (cadr compile-result))
         ;; Initial main to estimate size
         (main-code-temp (codegen-main-with-runtime-and-fns main-ir runtime-addrs nil 0))
         (main-size-temp (count-instrs main-code-temp))
         ;; First pass functions
         (fns-pass1 (codegen-functions-helper compiled-fns main-size-temp runtime-addrs))
         (fn-offsets-pass1 (cadr fns-pass1))
         ;; Main with first-pass offsets
         (main-code-pass1 (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets-pass1 0))
         (main-size-final (count-instrs main-code-pass1))
         ;; Recompute function offsets with final main size if changed
         (fns-result (if (= main-size-final main-size-temp)
                         fns-pass1
                         (codegen-functions-helper compiled-fns main-size-final runtime-addrs)))
         (fn-offsets (cadr fns-result))
         (fns-code (car fns-result))
         ;; Final main with final offsets
         (main-code (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets 0)))
    (append main-code fns-code)))

(defun compile-program-with-functions (forms)
  "Stub: compile program using default runtime addresses"
  (compile-program-with-functions-with-runtime forms nil))
