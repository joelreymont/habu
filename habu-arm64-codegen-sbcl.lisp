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

(defun make-runtime-addrs (cons-addr car-addr cdr-addr)
  "Create runtime address table for cons/car/cdr."
  (list (cons 'habu_cons cons-addr)
        (cons 'habu_car car-addr)
        (cons 'habu_cdr cdr-addr)))

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
         (offset-bits (logand offset #x3FFFFFF))
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

(defun codegen-expr (ir runtime-addrs &optional fn-offsets current-offset)
  "Enhanced codegen: literals, arithmetic, runtime calls"
  (cond
    ;; Literal: load tagged fixnum (value << 4)
    ((has-tag? ir 'lit)
     (let* ((value (cadr ir))
            (tagged (ash value 4)))  ; Tag fixnum: value << 4
       (arm64-movz 0 (logand tagged #xFFFF))))

    ;; Variable: load from stack (negative offset from x20 = environment base)
    ((has-tag? ir 'var)
     (let ((offset (cadr ir)))
       ;; Variables are stored at negative offsets from x20 (stack grows down)
       ;; Use x1 as temp to compute address
       (append
         (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
         (arm64-ldr 0 1 0))))                ; Load from [x1 + 0]

    ;; Addition: (add left right)
    ;; Save x21 to stack, use it for left operand, restore after
    ((has-tag? ir 'add)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code                   ; Compute left → x0
               (arm64-str 0 31 72)         ; Save left to [sp+72]
               right-code                  ; Compute right → x0
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 72)         ; Load left from [sp+72] to x0
               (arm64-add 0 0 1))))        ; x0 = x0 + x1

    ;; Subtraction: (sub left right)
    ;; Save x21 to stack, use it for left operand, restore after
    ((has-tag? ir 'sub)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 80)         ; Save left to [sp+80]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 80)         ; Load left from [sp+80] to x0
               (arm64-sub 0 0 1))))        ; x0 = x0 - x1

    ;; Multiplication: (mul left right) - must untag/retag
    ;; Save x23 to stack, use it for left operand, restore after
    ((has-tag? ir 'mul)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 2)
                                        nil))))
       (append left-code
               (arm64-lsr 0 0 4)           ; Untag left
               (arm64-str 0 31 64)         ; Save untagged left to [sp+64]
               right-code
               (arm64-lsr 1 0 4)           ; Untag right into x1
               (arm64-ldr 0 31 64)         ; Load left from [sp+64] into x0
               (arm64-mul 0 0 1)           ; Multiply x0 = x0 * x1
               (arm64-lsl 0 0 4))))        ; Retag result

    ;; Comparison: (cmp-eq left right) - returns tagged 1 or 0
    ((has-tag? ir 'cmp-eq)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 0)            ; x0 = 1 if equal, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than: (cmp-lt left right)
    ((has-tag? ir 'cmp-lt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 11)           ; x0 = 1 if less than, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than: (cmp-gt left right)
    ((has-tag? ir 'cmp-gt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 12)           ; x0 = 1 if greater than, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than or equal: (cmp-le left right)
    ((has-tag? ir 'cmp-le)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 13)           ; x0 = 1 if less or equal, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than or equal: (cmp-ge left right)
    ((has-tag? ir 'cmp-ge)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 10)           ; x0 = 1 if greater or equal, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Not equal: (cmp-ne left right)
    ((has-tag? ir 'cmp-ne)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       (append left-code
               (arm64-str 0 31 88)         ; Save left to [sp+88]
               right-code
               (arm64-mov 1 0)             ; Move right to x1
               (arm64-ldr 0 31 88)         ; Load left from [sp+88] to x0
               (arm64-cmp 0 1)             ; Compare
               (arm64-cset 0 1)            ; x0 = 1 if not equal, else 0
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Conditional: (if-expr test then else)
    ((has-tag? ir 'if-expr)
     (let* ((test-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (test-code (codegen-expr test-ir runtime-addrs fn-offsets current-offset))
            (then-code (codegen-expr then-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs test-code) 2)
                                        nil)))
            (else-code (codegen-expr else-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs test-code)
                                           2 (count-instrs then-code) 1)
                                        nil)))
            (then-len (/ (length then-code) 4))
            (else-len (/ (length else-code) 4)))
       ;; Layout after test:
       ;;   CMP x0, xzr         (position N)
       ;;   B.EQ offset         (position N+1) <-- branch from here
       ;;   then-code           (position N+2, then-len instructions)
       ;;   B else-skip         (position N+2+then-len)
       ;;   else-code           (position N+3+then-len) <-- target
       ;; Offset from N+1 to N+3+then-len = 2+then-len
       ;; Layout: CMP, B.NE, else-code, B, then-code
       ;; If truthy (non-zero): B.NE skips else-code and B, lands on then-code
       ;; If falsy (zero): execute else-code, B skips then-code
       (append test-code
               (arm64-cmp 0 31)            ; Compare result with 0 (XZR)
               ;; Branch if NOT equal (non-zero/true) to then-code
               ;; Skip: else-code (else-len) + B instruction (1) = else-len + 1
               ;; From current position: +1 for B.NE itself, +else-len for else, +1 for B = +2+else-len
               (arm64-b-cond 1 (+ 2 else-len))  ; B.NE: jump to then if true
               else-code
               ;; Unconditional branch to skip then-code
               ;; From B instruction to end of then-code: then-len instructions to skip
               ;; Plus implicit +1 because branch is PC-relative from current instruction
               (arm64-b (+ 1 then-len))
               then-code)))

    ;; Cons: (cons-call left right) - call runtime cons via table
    ;;   Runtime table pointer is in x19 (saved by prologue)
    ((has-tag? ir 'cons-call)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                    (if current-offset
                                        (+ current-offset (count-instrs left-code) 1)
                                        nil))))
       ;; Call cons(left, right) using runtime table[0]
       (append left-code                    ; Compute left → x0
               (arm64-push 0)               ; Push left onto stack
               right-code                   ; Compute right → x0
               (arm64-mov 1 0)              ; Move right to x1
               (arm64-pop 0)               ; Pop left from stack
               (arm64-ldr 9 19 0)           ; Load cons from table: LDR x9, [x19, #0]
               (arm64-blr 9))))             ; Call cons(x0, x1) → result in x0

    ;; Car: (car-call arg) - call runtime car via table
    ((has-tag? ir 'car-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 8)           ; Load car from table: LDR x9, [x19, #8]
               (arm64-blr 9))))             ; Call car(x0) → result in x0

    ;; Cdr: (cdr-call arg) - call runtime cdr via table
    ((has-tag? ir 'cdr-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 16)          ; Load cdr from table: LDR x9, [x19, #16]
               (arm64-blr 9))))             ; Call cdr(x0) → result in x0

    ;; Let expression: (let-expr bind-values body-ir num-bindings env-offsets)
    ((has-tag? ir 'let-expr)
     (let* ((bind-values (cadr ir))
            (body-ir (caddr ir))
            (num-bindings (cadddr ir))
            (env-offsets (nth 4 ir))  ; Get environment offsets for this let's bindings
            ;; Generate code for each binding value
            (bind-codes (mapcar (lambda (val-ir)
                                  (codegen-expr val-ir runtime-addrs fn-offsets current-offset))
                                bind-values)))
       ;; Store values at their designated offsets without moving x20
       (append
         ;; Store each binding value at its environment offset
         (apply #'append
                (mapcar (lambda (bind-code offset)
                          (append bind-code
                                  ;; Store at negative offset from x20 (stack grows down)
                                  ;; Use x1 as temp to compute address
                                  (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
                                  (arm64-str 0 1 0)))                ; Store at [x1 + 0]
                        bind-codes
                        env-offsets))

         ;; Execute body with bindings available
         (codegen-expr body-ir runtime-addrs fn-offsets current-offset))))

    ;; Function call: (call-fn name arg-irs)
    ((has-tag? ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (arg-irs (caddr ir))
            (arg-codes (mapcar (lambda (arg-ir)
                                (codegen-expr arg-ir runtime-addrs fn-offsets current-offset))
                              arg-irs))
            (num-args (length arg-irs))
            ;; Look up function offset
            (fn-entry (assoc fn-name fn-offsets))
            (fn-offset (if fn-entry (cadr fn-entry) 0)))
       ;; Generate code to evaluate each argument and move to x0-x2
       (append
         ;; Evaluate and move arguments to x0-x2
         (cond
           ((= num-args 0)
            nil)  ; No arguments
           ((= num-args 1)
            (car arg-codes))  ; Single arg already in x0
           ((= num-args 2)
            (append
              (car arg-codes)             ; First arg → x0
              (arm64-mov 2 0)             ; Save in x2
              (cadr arg-codes)            ; Second arg → x0
              (arm64-mov 1 0)             ; Move to x1
              (arm64-mov 0 2)))           ; Restore first to x0
           ((= num-args 3)
            (append
              (car arg-codes)             ; First arg → x0
              (arm64-mov 3 0)             ; Save in x3
              (cadr arg-codes)            ; Second arg → x0
              (arm64-mov 4 0)             ; Save in x4
              (caddr arg-codes)           ; Third arg → x0
              (arm64-mov 2 0)             ; Move to x2
              (arm64-mov 1 4)             ; Second to x1
              (arm64-mov 0 3)))           ; First to x0
           (t nil))  ; TODO: Handle more arguments

         ;; Generate BL to function
         ;; Calculate branch offset (in instructions, not bytes)
         ;; Branch offset = (target_offset - current_offset_after_args) / 4
         (let* ((code-so-far (cond
                                     ((= num-args 0) nil)
                                     ((= num-args 1) (car arg-codes))
                                     ((= num-args 2) (append
                                                           (car arg-codes)
                                                           (arm64-mov 2 0)
                                                           (cadr arg-codes)
                                                           (arm64-mov 1 0)
                                                           (arm64-mov 0 2)))
                                     ((= num-args 3) (append
                                                           (car arg-codes)
                                                           (arm64-mov 3 0)
                                                           (cadr arg-codes)
                                                           (arm64-mov 4 0)
                                                           (caddr arg-codes)
                                                           (arm64-mov 2 0)
                                                           (arm64-mov 1 4)
                                                           (arm64-mov 0 3)))
                                     (t nil)))
               (current-pc (if current-offset
                             ;; PC is at the BL instruction itself
                             (+ current-offset (count-instrs code-so-far))
                             0))  ; No offset if not tracking
               ;; Branch offset is difference in instructions
               (branch-offset (- fn-offset current-pc)))
           (arm64-bl branch-offset)))))

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
         ;; Addition
         ((eq op '+)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'add
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Subtraction
         ((eq op '-)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'sub
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Multiplication
         ((eq op '*)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'mul
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

         ;; Conditional
         ((eq op 'if)
          (if (and (consp (cdr expr))
                   (consp (cddr expr))
                   (consp (cdddr expr)))
              (list 'if-expr
                    (compile-expr (cadr expr) env fenv)   ; test
                    (compile-expr (caddr expr) env fenv)  ; then
                    (compile-expr (cadddr expr) env fenv)) ; else
              (list 'lit 0)))

         ;; Cons
         ((eq op 'cons)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cons-call
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

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
   Prologue saves x19 (runtime table) and x20-x21 (environment registers)"
  (let ((body (codegen-expr ir runtime-addrs nil nil)))
    ;; Allocate 256 bytes: 64 for saved registers + 192 for local variables/let bindings
    (append (arm64-sub-imm 31 31 256)     ; SUB sp, sp, #256 (large stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            ;; Set x20 to point to end of stack frame (grows downward for let bindings)
            (arm64-add-imm 20 31 248)     ; ADD x20, sp, #248 (near top of frame)
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 256)     ; ADD sp, sp, #256 (restore stack)
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

(defun compile-defun (name params body env fenv)
  "Compile defun into (name params body-ir)"
  ;; Create environment with parameters as the initial bindings
  (let* ((param-env (env-extend (mapcar #'list params) nil))
         ;; Add this function to fenv to allow recursive calls
         ;; Use a placeholder compiled-fn since we're still compiling it
         (recursive-fenv (cons (cons name nil) fenv))
         ;; Compile body in the parameter environment with recursive fenv
         (body-ir (compile-expr body param-env recursive-fenv)))
    (list name params body-ir)))

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
  (compile-forms-helper forms nil nil))

(defun codegen-function-with-params (params body-ir runtime-addrs &optional fn-offsets current-offset)
  "Generate code for function with parameters
   Parameters are passed in x0-x7, stored to stack for access as variables"
  (let* ((param-count (length params))
         ;; Calculate where body starts (after prologue and parameter storage)
         ;; Prologue: 6 instructions (SUB, 4xSTP, ADD)
         ;; Parameter storage: varies by param-count
         (prologue-size 6)
         (param-store-size (cond ((= param-count 0) 0)
                                 ((= param-count 1) 2)  ; 2 instructions
                                 ((= param-count 2) 4)  ; 4 instructions
                                 ((= param-count 3) 6)  ; 6 instructions
                                 (t 0)))
         (body-offset (if current-offset
                         (+ current-offset prologue-size param-store-size)
                         nil))
         ;; Pass fn-offsets and body-offset to body generation
         (body (codegen-expr body-ir runtime-addrs fn-offsets body-offset)))
    (append
      ;; Function prologue
      (arm64-sub-imm 31 31 256)      ; Allocate stack frame
      (arm64-stp 29 30 31 0)         ; Save FP/LR
      (arm64-stp 19 20 31 16)        ; Save x19/x20
      (arm64-stp 21 22 31 32)        ; Save x21/x22
      (arm64-stp 23 24 31 48)        ; Save x23/x24
      ;; x19 already has runtime table from caller - don't overwrite!
      (arm64-add-imm 20 31 248)      ; Set environment base

      ;; Store parameters to stack
      ;; Parameters are in x0-x2, store them at offsets 0, 1, 2...
      (cond
        ((= param-count 0) nil)
        ((= param-count 1)
         (append
           (arm64-sub-imm 1 20 0)     ; x1 = x20 - 0
           (arm64-str 0 1 0)))         ; Store x0 at [x1]
        ((= param-count 2)
         (append
           (arm64-sub-imm 2 20 0)     ; x2 = x20 - 0
           (arm64-str 0 2 0)           ; Store x0 at offset 0
           (arm64-sub-imm 2 20 8)     ; x2 = x20 - 8
           (arm64-str 1 2 0)))         ; Store x1 at offset 1
        ((= param-count 3)
         (append
           (arm64-sub-imm 3 20 0)     ; x3 = x20 - 0
           (arm64-str 0 3 0)           ; Store x0 at offset 0
           (arm64-sub-imm 3 20 8)     ; x3 = x20 - 8
           (arm64-str 1 3 0)           ; Store x1 at offset 1
           (arm64-sub-imm 3 20 16)    ; x3 = x20 - 16
           (arm64-str 2 3 0)))         ; Store x2 at offset 2
        (t nil))  ; TODO: Handle more parameters

      ;; Function body
      body

      ;; Function epilogue
      (arm64-ldp 23 24 31 48)        ; Restore x23/x24
      (arm64-ldp 21 22 31 32)        ; Restore x21/x22
      (arm64-ldp 19 20 31 16)        ; Restore x19/x20
      (arm64-ldp 29 30 31 0)         ; Restore FP/LR
      (arm64-add-imm 31 31 256)      ; Deallocate stack
      (arm64-ret))))

(defun calculate-function-offsets (compiled-fns start-offset runtime-addrs)
  "First pass: calculate function offsets by generating code without fn-offsets"
  (if (consp compiled-fns)
      (let* ((fn (car compiled-fns))
             (name (car fn))
             (params (cadr fn))
             (body-ir (caddr fn))
             ;; Generate without fn-offsets to get size
             (fn-code (codegen-function-with-params params body-ir runtime-addrs nil nil))
             (fn-size (count-instrs fn-code))
             ;; Recursively calculate rest
             (rest-offsets (calculate-function-offsets (cdr compiled-fns)
                                                       (+ start-offset fn-size)
                                                       runtime-addrs)))
        (cons (list name start-offset) rest-offsets))
      nil))

(defun codegen-functions-with-offsets (compiled-fns fn-offsets current-offset runtime-addrs)
  "Second pass: generate functions with correct fn-offsets"
  (if (consp compiled-fns)
      (let* ((fn (car compiled-fns))
             (params (cadr fn))
             (body-ir (caddr fn))
             ;; Generate with fn-offsets for proper function calls
             (fn-code (codegen-function-with-params params body-ir runtime-addrs
                                                   fn-offsets current-offset))
             (fn-size (count-instrs fn-code))
             ;; Generate rest
             (rest-code (codegen-functions-with-offsets (cdr compiled-fns) fn-offsets
                                                        (+ current-offset fn-size)
                                                        runtime-addrs)))
        (append fn-code rest-code))
      nil))

(defun codegen-functions-helper (compiled-fns current-offset runtime-addrs)
  "Generate code for all compiled functions using two-pass approach
   Returns: (total-code function-offsets)"
  ;; First pass: calculate all function offsets
  (let ((fn-offsets (calculate-function-offsets compiled-fns current-offset runtime-addrs)))
    ;; Second pass: generate code with correct offsets
    (let ((code (codegen-functions-with-offsets compiled-fns fn-offsets current-offset runtime-addrs)))
      (list code fn-offsets))))

(defun codegen-expr-with-fns (ir runtime-addrs fn-offsets current-offset)
  "Codegen with function offset tracking"
  (codegen-expr ir runtime-addrs fn-offsets current-offset))

(defun codegen-main-with-runtime-and-fns (ir runtime-addrs fn-offsets current-offset)
  "Generate main code with function offsets for calls"
  ;; Pass function offsets through to codegen
  ;; The body comes after the 7-instruction prologue
  (let ((body (codegen-expr-with-fns ir runtime-addrs fn-offsets (+ current-offset 7))))
    ;; Same prologue/epilogue as before
    (append (arm64-sub-imm 31 31 256)     ; SUB sp, sp, #256 (large stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            (arm64-add-imm 20 31 248)     ; ADD x20, sp, #248
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 256)     ; ADD sp, sp, #256 (restore stack)
            (arm64-ret))))

(defun compile-program-with-functions-with-runtime (forms runtime-addrs)
  "Compile entire program with function definitions
   Returns: complete machine code with main at offset 0 (entry point)"
  (let* ((compile-result (compile-forms forms))
         (compiled-fns (car compile-result))
         (main-ir (cadr compile-result))
         ;; Generate main code first to get its size
         (main-code-temp (codegen-main-with-runtime-and-fns main-ir runtime-addrs nil 0))
         (main-size (count-instrs main-code-temp))
         ;; Now generate functions after main
         (fns-result (codegen-functions-helper compiled-fns main-size runtime-addrs))
         (fns-code (car fns-result))
         (fn-offsets (cadr fns-result))
         ;; Regenerate main with correct function offsets
         (main-code (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets 0)))
    ;; Put main first (at offset 0) so it's the entry point
    (append main-code fns-code)))

(defun compile-program-with-functions (forms)
  "Stub: compile program using default runtime addresses"
  (compile-program-with-functions-with-runtime forms nil))
