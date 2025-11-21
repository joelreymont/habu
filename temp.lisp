;;;; ARM64 Machine Code Generator - Pure Habu Lisp
;;;; Generates raw ARM64 bytes following SBCL model

;;; ============================================
;;; Byte Utilities
;;; ============================================

(defun my-mod (n d)
  (- n (* d (/ n d))))

(defun get-byte (n offset)
  "Extract byte at offset from integer"
  (my-mod (/ n (* offset 256)) 256))

(defun make-word (b0 b1 b2 b3)
  "Make 32-bit word from 4 bytes (little-endian)"
  (+ b0 (* b1 256) (* b2 65536) (* b3 16777216)))

(defun word-to-bytes (w)
  "Convert 32-bit word to 4-byte list"
  (list (my-mod w 256)
        (my-mod (/ w 256) 256)
        (my-mod (/ w 65536) 256)
        (my-mod (/ w 16777216) 256)))

;;; ============================================
;;; ARM64 Instruction Encoders (Parametric)
;;; ============================================

(defun encode-word (w)
  "Convert 32-bit word to 4-byte list (little-endian)"
  (word-to-bytes w))

(defun arm64-movz (rd imm)
  "MOVZ Xd, #imm - Move 16-bit immediate to register (zero extended)
   Encoding: 1101 0010 1... .... ...i iiii iiid dddd
   Base: #xD2800000 | (imm << 5) | rd"
  (let ((base #xD2800000))
    (let ((shifted-imm (* imm 32)))  ; imm << 5
      (let ((encoded (+ base (+ shifted-imm rd))))
        (encode-word encoded)))))

(defun arm64-add (rd rn rm)
  "ADD Xd, Xn, Xm - Add registers
   Encoding: 1000 1011 000m mmmm 0000 00nn nnn d dddd
   Base: #x8B000000 | (rm << 16) | (rn << 5) | rd"
  (let ((base #x8B000000))
    (let ((shifted-rm (* rm 65536)))  ; rm << 16
      (let ((shifted-rn (* rn 32)))   ; rn << 5
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-sub (rd rn rm)
  "SUB Xd, Xn, Xm - Subtract registers
   Encoding: 1100 1011 000m mmmm 0000 00nn nnnd dddd
   Base: #xCB000000 | (rm << 16) | (rn << 5) | rd"
  (let ((base #xCB000000))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-rn (* rn 32)))
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-mul (rd rn rm)
  "MUL Xd, Xn, Xm - Multiply registers
   Encoding: 1001 1011 000m mmmm 0111 11nn nnnd dddd
   Base: #x9B007C00 | (rm << 16) | (rn << 5) | rd"
  (let ((base #x9B007C00))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-rn (* rn 32)))
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-udiv (rd rn rm)
  "UDIV Xd, Xn, Xm - Unsigned divide
   Encoding: 1001 1010 110m mmmm 0000 10nn nnnd dddd
   Base: #x9AC00800 | (rm << 16) | (rn << 5) | rd"
  (let ((base #x9AC00800))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-rn (* rn 32)))
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-msub (rd rn rm ra)
  "MSUB Xd, Xn, Xm, Xa - Multiply-subtract: Xd = Xa - Xn*Xm
   Encoding: 1001 1011 000m mmmm 1aaa aann nnnd dddd
   Base: #x9B008000 | (rm << 16) | (ra << 10) | (rn << 5) | rd"
  (let ((base #x9B008000))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-ra (* ra 1024)))
        (let ((shifted-rn (* rn 32)))
          (let ((encoded (+ base (+ shifted-rm (+ shifted-ra (+ shifted-rn rd))))))
            (encode-word encoded)))))))

(defun arm64-and (rd rn rm)
  "AND Xd, Xn, Xm - Bitwise AND registers
   Encoding: 1000 1010 000m mmmm 0000 00nn nnnd dddd
   Base: #x8A000000 | (rm << 16) | (rn << 5) | rd"
  (let ((base #x8A000000))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-rn (* rn 32)))
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-orr (rd rn rm)
  "ORR Xd, Xn, Xm - Bitwise OR registers
   Encoding: 1010 1010 000m mmmm 0000 00nn nnnd dddd
   Base: #xAA000000 | (rm << 16) | (rn << 5) | rd"
  (let ((base #xAA000000))
    (let ((shifted-rm (* rm 65536)))
      (let ((shifted-rn (* rn 32)))
        (let ((encoded (+ base (+ shifted-rm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-lsr (rd rn shift)
  "LSR Xd, Xn, #shift - Logical shift right
   Encoding: 1101 0011 01.. .... ssss ssnn nnnd dddd
   Base: #xD340FC00 | (shift << 16) | (rn << 5) | rd
   For shifts 0-63"
  (let ((base #xD3400000))
    (let ((imms 63))  ; all 1s for LSR (bits 15:10)
      (let ((shifted-shift (* shift 65536)))  ; shift amount in immr
        (let ((shifted-rn (* rn 32)))
          (let ((encoded (+ base (+ (* imms 1024) (+ shifted-shift (+ shifted-rn rd))))))
            (encode-word encoded)))))))

(defun arm64-lsl (rd rn shift)
  "LSL Xd, Xn, #shift - Logical shift left
   Encoding: Uses UBFM with immr = -shift mod 64, imms = 63-shift
   Base: #xD3400000"
  (let ((base #xD3400000))
    (let ((immr (my-mod (- 0 shift) 64)))  ; -shift mod 64
      (let ((imms (- 63 shift)))  ; 63 - shift
        (let ((shifted-immr (* immr 65536)))
          (let ((shifted-imms (* imms 1024)))
            (let ((shifted-rn (* rn 32)))
              (let ((encoded (+ base (+ shifted-immr (+ shifted-imms (+ shifted-rn rd))))))
                (encode-word encoded)))))))))

(defun arm64-str (rt rn imm)
  "STR Xt, [Xn, #imm]! - Store register with pre-increment
   For sp (x31) as base and negative offset"
  ;;; Hardcoded for str x0, [sp, #-16]! for now
  ;;; TODO: Make fully parametric
  (if (= rt 0)
    (if (= rn 31)  ; sp
      (if (= imm -16)
        (quote (240 15 31 248))  ; Verified encoding
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-ldr-post (rt rn imm)
  "LDR Xt, [Xn], #imm - Load register with post-increment"
  ;;; Hardcoded for ldr x0, [sp], #16 for now
  ;;; TODO: Make fully parametric
  (if (= rt 0)
    (if (= rn 31)  ; sp
      (if (= imm 16)
        (quote (224 7 65 248))  ; Verified encoding
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-ldr (rt rn offset)
  "LDR Xt, [Xn, #offset]"
  (let ((base #xF9400000))
    (let ((imm12 (/ offset 8)))
      (let ((shifted-imm (* imm12 1024)))
        (let ((shifted-rn (* rn 32)))
          (let ((encoded (+ base (+ shifted-imm (+ shifted-rn rt)))))
            (encode-word encoded)))))))

(defun arm64-mov (rd rn)
  "MOV Xd, Xn - Move register (implemented as ORR Xd, XZR, Xn)
   Encoding: 1010 1010 000m mmmm 0000 00nn nnnd dddd
   Base: #xAA0003E0 | (rn << 16) | rd"
  (let ((base #xAA0003E0))
    (let ((shifted-rn (* rn 65536)))  ; rn << 16
      (let ((encoded (+ base (+ shifted-rn rd))))
        (encode-word encoded)))))

(defun arm64-stp (rt1 rt2 rn imm)
  "STP Xt1, Xt2, [Xn, #imm]! - Store pair with pre-increment"
  ;;; Hardcoded for stp x29, x30, [sp, #-16]!
  ;;; TODO: Make fully parametric
  (if (= rt1 29)
    (if (= rt2 30)
      (if (= rn 31)  ; sp
        (if (= imm -16)
          (quote (253 123 191 169))  ; Verified encoding
          (quote (0 0 0 0)))
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-ldp (rt1 rt2 rn imm)
  "LDP Xt1, Xt2, [Xn], #imm - Load pair with post-increment"
  ;;; Hardcoded for ldp x29, x30, [sp], #16
  ;;; TODO: Make fully parametric
  (if (= rt1 29)
    (if (= rt2 30)
      (if (= rn 31)  ; sp
        (if (= imm 16)
          (quote (253 123 193 168))  ; Verified encoding
          (quote (0 0 0 0)))
        (quote (0 0 0 0)))
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))

(defun arm64-add-imm (rd rn imm)
  "ADD Xd, Xn, #imm - Add immediate (for sp operations)
   Encoding: 1001 0001 00.. iiii iiii iinn nnnd dddd
   Base: #x91000000 | (imm << 10) | (rn << 5) | rd"
  (let ((base #x91000000))
    (let ((shifted-imm (* imm 1024)))  ; imm << 10
      (let ((shifted-rn (* rn 32)))  ; rn << 5
        (let ((encoded (+ base (+ shifted-imm (+ shifted-rn rd)))))
          (encode-word encoded))))))

(defun arm64-ret ()
  "RET - Return from subroutine
   Encoding: 1101 0110 0101 1111 0000 0011 1100 0000"
  (quote (192 3 95 214)))

(defun arm64-cmp (rn rm)
  "CMP Xn, Xm - Compare registers (sets flags)
   Implemented as SUBS XZR, Xn, Xm
   Encoding: 1110 1011 000m mmmm 0000 00nn nnn1 1111
   Base: #xEB00001F | (rm << 16) | (rn << 5)"
  (let ((base #xEB00001F))
    (let ((shifted-rm (* rm 65536)))  ; rm << 16
      (let ((shifted-rn (* rn 32)))   ; rn << 5
        (let ((encoded (+ base (+ shifted-rm shifted-rn))))
          (encode-word encoded))))))

(defun arm64-cset (rd cond)
  "CSET Xd, cond - Conditional set (1 if condition true, else 0)
   Implemented as CSINC Xd, XZR, XZR, invert(cond)
   Encoding: 1001 1010 1001 1111 cccc 0111 111d dddd
   Base: #x9A9F07E0 | (inverted_cond << 12) | rd
   Condition codes: EQ=0, NE=1, LT=11, LE=13, GT=12, GE=10
   Inversion: cond XOR 1"
  (let ((base #x9A9F07E0))
    (let ((inverted-cond (if (= (my-mod cond 2) 0) (+ cond 1) (- cond 1))))  ; XOR 1
      (let ((shifted-cond (* inverted-cond 4096)))  ; inverted_cond << 12
        (let ((encoded (+ base (+ shifted-cond rd))))
          (encode-word encoded))))))

(defun arm64-b-cond (cond offset)
  "B.cond <label> - Conditional branch
   Encoding: 0101 0100 iiii iiii iiii iiii iii0 cccc
   Base: #x54000000 | (offset << 5) | cond
   offset is signed 19-bit, in units of 4 bytes (instructions)
   Condition codes: EQ=0, NE=1, LT=11, GT=12, etc."
  (let ((base #x54000000))
    (let ((shifted-offset (* offset 32)))  ; offset << 5 (in instruction units)
      (let ((encoded (+ base (+ shifted-offset cond))))
        (encode-word encoded)))))

(defun arm64-b (offset)
  "B <label> - Unconditional branch
   Encoding: 0001 01ii iiii iiii iiii iiii iiii iiii
   Base: #x14000000 | offset
   offset is signed 26-bit, in units of 4 bytes (instructions)"
  (let ((base #x14000000))
    (let ((encoded (+ base offset)))
      (encode-word encoded))))

(defun arm64-bl (offset)
  "BL <label> - Branch with link (function call)
   Encoding: 1001 01ii iiii iiii iiii iiii iiii iiii
   Base: #x94000000 | offset
   offset is signed 26-bit, in units of 4 bytes (instructions)
   Saves return address in x30 (LR)"
  (let ((base #x94000000))
    (let ((encoded (+ base offset)))
      (encode-word encoded))))

(defun arm64-movk (rd imm shift)
  "MOVK Xd, #imm, LSL #shift - Move 16-bit immediate, keeping other bits
   Encoding: 1111 0010 1ss. .... iiii iiii iiid dddd
   Base: #xF2800000 | (shift_sel << 21) | (imm << 5) | rd
   shift must be 0, 16, 32, or 48"
  (let ((base #xF2800000))
    (let ((shift-sel (/ shift 16)))
      (let ((shifted-sel (* shift-sel 2097152)))  ; shift-sel << 21
        (let ((shifted-imm (* imm 32)))           ; imm << 5
          (let ((encoded (+ base (+ shifted-sel (+ shifted-imm rd)))))
            (encode-word encoded)))))))

(defun arm64-blr (rn)
  "BLR Xn - Branch to address in register Xn
   Encoding: 1101 0110 0011 1111 0000 00nn nnn0 0000
   Base: #xD63F0000 | (rn << 5)"
  (let ((base #xD63F0000))
    (let ((shifted-rn (* rn 32)))  ; rn << 5
      (let ((encoded (+ base shifted-rn)))
        (encode-word encoded)))))

(defun load-address-to-reg (rd addr)
  "Load 64-bit address into register rd using movz + movk sequence
   Breaks address into four 16-bit chunks and loads them"
  (let ((bits0-15 (my-mod addr 65536)))
    (let ((bits16-31 (my-mod (/ addr 65536) 65536)))
      (let ((bits32-47 (my-mod (/ addr 4294967296) 65536)))
        (let ((bits48-63 (/ addr 281474976710656)))
          (append-code (arm64-movz rd bits0-15)
            (append-code (arm64-movk rd bits16-31 16)
              (append-code (arm64-movk rd bits32-47 32)
                (arm64-movk rd bits48-63 48)))))))))

(defun append-code (c1 c2)
  (if (nil? c1) c2 (cons (car c1) (append-code (cdr c1) c2))))

(defun count-instrs (code)
  "Count number of 4-byte instructions in code list"
  (if (nil? code)
    0
    (+ 1 (count-instrs (cdr (cdr (cdr (cdr code))))))))

(defun cmp-zero ()
  "CMP x0, #0 - Compare x0 with zero"
  (arm64-cmp 0 31))  ; Compare x0 with XZR (zero register)

;;; ============================================
;;; High-Level Code Generation
;;; ============================================

(defun has-tag? (ir tag)
  (if (cons? ir)
    (if (symbol? (car ir))
      (symbol=? (car ir) tag)
      (quote nil))
    (quote nil)))

(defun runtime-lookup (name runtime-addrs)
  "Lookup runtime function address by name from association list"
  (if (nil? runtime-addrs)
    #x0
    (let ((entry (car runtime-addrs)))
      (let ((entry-name (car entry)))
        (let ((entry-addr (cdr entry)))
          (if (symbol=? name entry-name)
            entry-addr
            (runtime-lookup name (cdr runtime-addrs))))))))

(defun make-runtime-addrs (cons-addr car-addr cdr-addr)
  "Create runtime address table for cons/car/cdr"
  (cons (cons (quote habu_cons) cons-addr)
    (cons (cons (quote habu_car) car-addr)
      (cons (cons (quote habu_cdr) cdr-addr)
        (quote nil)))))

(defun fn-offset-lookup (fname fn-offsets)
  "Lookup function offset by name from fn-offsets list
   fn-offsets is list of (name offset-in-instructions)
   Returns offset or nil if not found"
  (if (nil? fn-offsets)
    (quote nil)
    (let ((entry (car fn-offsets)))
      (let ((entry-name (car entry)))
        (let ((entry-offset (car (cdr entry))))
          (if (symbol=? fname entry-name)
            entry-offset
            (fn-offset-lookup fname (cdr fn-offsets))))))))

#-sbcl
(defun codegen-progn-list (exprs runtime-addrs)
  "Generate code for list of expressions, returning last result"
  (if (cons? exprs)
    (let ((first-code (codegen-expr (car exprs) runtime-addrs)))
      (let ((rest (cdr exprs)))
        (if (cons? rest)
          ;;; More expressions: append this code and continue
          (append-code first-code (codegen-progn-list rest runtime-addrs))
          ;;; Last expression: just return its code
          first-code)))
    ;;; Empty list: return 0
    (arm64-movz 0 #x0)))

#-sbcl
(defun codegen-save-bindings (bindings runtime-addrs)
  "Generate code to evaluate and save all bindings to stack
   Each binding is (var value-ir offset)"
  (if (cons? bindings)
    (let ((binding (car bindings)))
      (let ((value-ir (car (cdr binding))))
        ;;; Generate code for this value
        (let ((value-code (codegen-expr value-ir runtime-addrs (quote nil) 0 nil)))
          ;;; Save to stack
          (let ((save-code (arm64-str 0 31 -16)))
            ;;; Continue with rest of bindings
            (let ((rest-code (codegen-save-bindings (cdr bindings) runtime-addrs)))
              (append-code value-code
                (append-code save-code rest-code)))))))
    nil))

#-sbcl
(defun codegen-eval-args-push (args-ir runtime-addrs)
  "Generate code to evaluate each argument and push to stack"
  (if (cons? args-ir)
    (let ((arg (car args-ir)))
      (let ((code (codegen-expr arg runtime-addrs (quote nil) 0 nil)))
        (let ((push (arm64-str 0 31 -16)))
          (let ((rest-code (codegen-eval-args-push (cdr args-ir) runtime-addrs)))
            (append-code code
              (append-code push rest-code))))))
    nil))

#-sbcl
(defun codegen-pop-args-to-regs (num-args)
  "Generate code to pop arguments from stack to registers x0-x2
   Pops in reverse order: last arg first"
  (if (= num-args 0)
    nil
    (if (= num-args 1)
      ;;; Pop single arg to x0
      (arm64-ldr-post 0 31 16)
      (if (= num-args 2)
        ;;; Pop two args: x1 then x0
        (append-code (arm64-ldr-post 1 31 16)
                     (arm64-ldr-post 0 31 16))
        (if (= num-args 3)
          ;;; Pop three args: x2, x1, x0
        (append-code (arm64-ldr-post 2 31 16)
          (append-code (arm64-ldr-post 1 31 16)
                       (arm64-ldr-post 0 31 16)))
          ;;; More than 3 args not supported yet
          nil)))))

#-sbcl
(defun codegen-eval-args-to-regs (args-ir runtime-addrs)
  "Generate code to evaluate arguments and place in registers x0-x2
   Strategy: eval and push all, then pop to registers in correct order"
  (let ((num-args (count-args args-ir)))
    (let ((push-code (codegen-eval-args-push args-ir runtime-addrs)))
      (let ((pop-code (codegen-pop-args-to-regs num-args)))
        (append-code push-code pop-code)))))

#-sbcl
(defun count-args (args)
  "Count number of arguments in list"
  (if (cons? args)
    (+ 1 (count-args (cdr args)))
    0))

#-sbcl
(defun count-bindings (bindings)
  "Count number of bindings in list"
  (if (cons? bindings)
    (+ 1 (count-bindings (cdr bindings)))
    0))

#-sbcl
(defun codegen-cond-clauses (clauses runtime-addrs)
  "Generate code for cond clauses - returns code that leaves result in x0"
  (if (cons? clauses)
    (let* ((clause (car clauses))
           (test (car clause))
           (result (car (cdr clause)))
           (rest-clauses (cdr clauses))
           (test-code (codegen-expr test runtime-addrs (quote nil) 0 nil))
           (result-code (codegen-expr result runtime-addrs (quote nil) 0 nil))
           (rest-code (if (cons? rest-clauses)
                        (codegen-cond-clauses rest-clauses runtime-addrs)
                        (arm64-movz 0 #x0)))  ; default: return nil
           (result-size (count-instrs result-code))
           (rest-size (count-instrs rest-code))
           (beq-offset (+ result-size 2))   ; result + beq + b
           (b-offset (+ rest-size 1))       ; rest + b
           (cmp-code (cmp-zero))
           (beq-instr (arm64-b-cond 0 beq-offset))  ; EQ=0
           (b-instr (arm64-b b-offset)))
      (append-code test-code
        (append-code cmp-code
          (append-code beq-instr
            (append-code result-code
              (append-code b-instr rest-code))))))
    ;;; No clauses - return nil (this is the default case)
    (arm64-movz 0 #x0)))

#-sbcl
#-sbcl
(defun codegen-expr (ir runtime-addrs fn-offsets current-offset tail?)
  "Generate ARM64 code for expression (result in x0)
   fn-offsets: list of (fname offset-in-instructions)
   current-offset: current position in instructions for BL offset calculation
   tail?: true if this expression is in tail position (can use tail-call optimization)"
  (if (has-tag? ir (quote lit))
    ;;; Literal: movz x0, #(value << 4)
    (let ((value (car (cdr ir))))
      (let ((tagged (* value 16)))
        (arm64-movz 0 tagged)))

    (if (has-tag? ir (quote var))
      ;;; Variable reference: load from stack
      (let ((offset (car (cdr ir))))
        ;;; Load from [sp, #(offset * 16)]
        ;;; offset 0 = [sp, #0], offset 1 = [sp, #16], etc.
        (arm64-ldr 0 31 (* offset 16)))

      (if (has-tag? ir (quote let))
        ;;; Let binding: evaluate value, save on stack, evaluate body
        (let ((var (car (cdr ir))))  ; variable name (not used in codegen)
          (let ((value-ir (car (cdr (cdr ir)))))
            (let ((body-ir (car (cdr (cdr (cdr ir))))))
              ;;; Generate code for value
              (let ((value-code (codegen-expr value-ir runtime-addrs fn-offsets current-offset nil)))
                ;;; Save x0 on stack: str x0, [sp, #-16]!
                (let ((save-code (arm64-str 0 31 -16)))
                  ;;; Generate code for body (offset updated by value-code + save-code)
                  ;;; Body is in tail position if the let is in tail position
                  (let ((body-offset (+ current-offset (+ (count-instrs value-code) (count-instrs save-code)))))
                    (let ((body-code (codegen-expr body-ir runtime-addrs fn-offsets body-offset tail?)))
                      ;;; Restore stack: add sp, sp, #16
                      (let ((restore-code (arm64-add-imm 31 31 16)))
                        (append-code value-code
                        (append-code save-code
                          (append-code body-code restore-code))))))))))

        (if (has-tag? ir (quote let-multi))
          ;;; Multiple let bindings: evaluate and save all values, then evaluate body
          (let ((bindings (car (cdr ir))))
            (let ((body-ir (car (cdr (cdr ir)))))
              (let ((save-code (codegen-save-bindings bindings runtime-addrs)))
                ;;; Body is in tail position if the let-multi is in tail position
                (let ((body-code (codegen-expr body-ir runtime-addrs fn-offsets current-offset tail?)))
                  (let ((binding-count (count-bindings bindings)))
                    (let ((restore-code (arm64-add-imm 31 31 (* binding-count 16))))
                      (append-code save-code
                        (append-code body-code restore-code))))))))

          (if (has-tag? ir (quote fncall))
            ;;; Function call with BL instruction
            (let ((fname (car (cdr ir))))
              (let ((args-ir (car (cdr (cdr ir)))))
                ;;; Evaluate arguments and place in x0-x2
                (let ((args-code (codegen-eval-args-to-regs args-ir runtime-addrs)))
                  ;;; Look up target function offset
                  (let ((target-offset (fn-offset-lookup fname fn-offsets)))
                    (if (nil? target-offset)
                      ;;; Function not found - use dummy BL 0 (for backwards compat)
                      (let ((bl-instr (arm64-bl 0)))
                        (append-code args-code bl-instr))
                      ;;; Check for tail-call optimization
                      (if tail?
                        ;;; TAIL CALL: jump directly (reuse current frame, no stack changes)
                        ;;; This turns recursion into iteration!
                        (let ((jump-position (+ current-offset (count-instrs args-code))))
                          (let ((jump-offset (- target-offset jump-position)))
                            (let ((jump-instr (arm64-b jump-offset)))
                              (append-code args-code jump-instr))))
                        ;;; NORMAL CALL: use BL instruction
                        (let ((bl-position (+ current-offset (count-instrs args-code))))
                          (let ((bl-offset (- target-offset bl-position)))
                            (let ((bl-instr (arm64-bl bl-offset)))
                              (append-code args-code bl-instr))))))))))

          (if (has-tag? ir (quote call))
            ;;; Operation (unary or binary)
            (let ((op (car (cdr ir))))
        (let ((arg1 (car (cdr (cdr ir)))))
          (let ((arg2 (car (cdr (cdr (cdr ir))))))
            (if (nil? arg2)
              ;;; Unary operation (e.g., not, fixnum?)
              (let ((code1 (codegen-expr arg1 runtime-addrs fn-offsets current-offset nil)))
                (if (symbol=? op (quote not))
                  ;;; NOT: invert boolean
                  (append-code code1
                    (append-code (arm64-cmp 0 31)  ; cmp x0, xzr
                      (append-code (arm64-cset 0 0)  ; cset x0, EQ (inverted!)
                        (arm64-lsl 0 0 4))))  ; lsl x0, x0, #4
                  (if (symbol=? op (quote fixnum?))
                    ;;; FIXNUM?: check if lower 4 bits are 0
                    (append-code code1
                      (append-code (arm64-movz 1 15)  ; movz x1, #15
                        (append-code (arm64-and 1 0 1)  ; and x1, x0, x1
                          (append-code (arm64-cmp 1 31)  ; cmp x1, xzr
                            (append-code (arm64-cset 0 0)  ; cset x0, EQ
                              (arm64-lsl 0 0 4))))))
                    (if (symbol=? op (quote nil?))
                      ;;; NIL?: check if value is 0 (nil = 0)
                      (append-code code1
                        (append-code (arm64-cmp 0 31)  ; cmp x0, xzr
                          (append-code (arm64-cset 0 0)  ; cset x0, EQ
                            (arm64-lsl 0 0 4))))
                      (if (symbol=? op (quote zero?))
                        ;;; ZERO?: check if untagged value is 0
                        (append-code code1
                          (append-code (arm64-cmp 0 31)  ; cmp x0, xzr
                            (append-code (arm64-cset 0 0)  ; cset x0, EQ
                              (arm64-lsl 0 0 4))))
                        (if (symbol=? op (quote car))
                          ;;; CAR: call habu_car with value in x0
                          (let ((habu-car-addr (runtime-lookup (quote habu_car) runtime-addrs)))
                            (let ((load-addr (load-address-to-reg 2 habu-car-addr)))
                              (let ((call-code (arm64-blr 2)))
                                (append-code code1
                                  (append-code load-addr call-code)))))
                          (if (symbol=? op (quote cdr))
                            ;;; CDR: call habu_cdr with value in x0
                            (let ((habu-cdr-addr (runtime-lookup (quote habu_cdr) runtime-addrs)))
                              (let ((load-addr (load-address-to-reg 2 habu-cdr-addr)))
                                (let ((call-code (arm64-blr 2)))
                                  (append-code code1
                                    (append-code load-addr call-code)))))
                            ;;; Unknown unary op
                            code1))))))
              ;;; Binary operation with nested args
              ;;; First check for cons/car/cdr (runtime calls)
              (if (symbol=? op (quote cons))
                ;;; CONS: (cons a b) - call habu_cons
                (let ((habu-cons-addr (runtime-lookup (quote habu_cons) runtime-addrs)))
                  (let ((code1 (codegen-expr arg1 runtime-addrs fn-offsets current-offset nil)))      ; arg1 → x0
                    (let ((save-code (arm64-str 0 31 -16)))  ; push x0
                      (let ((code2 (codegen-expr arg2 runtime-addrs fn-offsets current-offset nil)))     ; arg2 → x0
                        (let ((move-code (arm64-mov 1 0)))   ; x0 → x1
                          (let ((load-code (arm64-ldr-post 0 31 16)))  ; pop to x0
                            (let ((load-addr (load-address-to-reg 2 habu-cons-addr)))
                              (let ((call-code (arm64-blr 2)))
                                (append-code code1
                                  (append-code save-code
                                    (append-code code2
                                      (append-code move-code
                                        (append-code load-code
                                          (append-code load-addr call-code))))))))))))))
              (let ((code1 (codegen-expr arg1 runtime-addrs fn-offsets current-offset nil)))
              ;;; Save arg1: str x0, [sp, #-16]!
              (let ((save-code (arm64-str 0 31 -16)))
                ;;; Generate code for arg2
                (let ((code2 (codegen-expr arg2 runtime-addrs fn-offsets current-offset nil)))
                  ;;; Move arg2 to x1: mov x1, x0
                  (let ((move-code (arm64-mov 1 0)))
                    ;;; Load arg1 to x0: ldr x0, [sp], #16
                    (let ((load-code (arm64-ldr-post 0 31 16)))
                      ;;; Perform operation
                      (let ((op-code
                              (if (symbol=? op (quote +))
                                (arm64-add 0 0 1)
                                (if (symbol=? op (quote -))
                                  (arm64-sub 0 0 1)
                                  (if (symbol=? op (quote *))
                                    ;;; For multiply, untag one operand
                                    (append-code
                                      (arm64-lsr 0 0 4)
                                      (arm64-mul 0 0 1))
                                    (if (symbol=? op (quote /))
                                      ;;; For divide, untag both operands, divide, then retag
                                      (append-code
                                        (arm64-lsr 0 0 4)  ; untag dividend
                                        (append-code
                                          (arm64-lsr 1 1 4)  ; untag divisor
                                          (append-code
                                            (arm64-udiv 0 0 1)  ; divide
                                            (arm64-lsl 0 0 4)))))  ; retag result
                                      (if (symbol=? op (quote mod))
                                        ;;; Modulo: a mod b = a - (a/b)*b
                                        (append-code
                                          (arm64-lsr 0 0 4)  ; untag a
                                          (append-code
                                            (arm64-lsr 1 1 4)  ; untag b
                                            (append-code
                                              (arm64-mov 2 0)  ; save a in x2
                                              (append-code
                                                (arm64-udiv 0 0 1)  ; x0 = a/b
                                                (append-code
                                                  (arm64-msub 0 0 1 2)  ; x0 = a - (a/b)*b
                                                  (arm64-lsl 0 0 4)))))))  ; retag
                                        (if (symbol=? op (quote =))
                                      ;;; Comparison: cmp then cset
                                      (append-code
                                        (arm64-cmp 0 1)
                                        (append-code
                                          (arm64-cset 0 0)  ; EQ = 0
                                          (arm64-lsl 0 0 4)))
                                      (if (symbol=? op (quote <))
                                        (append-code
                                          (arm64-cmp 0 1)
                                          (append-code
                                            (arm64-cset 0 11)  ; LT = 11
                                            (arm64-lsl 0 0 4)))
                                        (if (symbol=? op (quote >))
                                          (append-code
                                            (arm64-cmp 0 1)
                                            (append-code
                                              (arm64-cset 0 12)  ; GT = 12
                                              (arm64-lsl 0 0 4)))
                                          (if (symbol=? op (quote !=))
                                            (append-code
                                              (arm64-cmp 0 1)
                                              (append-code
                                                (arm64-cset 0 1)  ; NE = 1
                                                (arm64-lsl 0 0 4)))
                                            (if (symbol=? op (quote <=))
                                              (append-code
                                                (arm64-cmp 0 1)
                                                (append-code
                                                  (arm64-cset 0 13)  ; LE = 13
                                                  (arm64-lsl 0 0 4)))
                                              (if (symbol=? op (quote >=))
                                                (append-code
                                                  (arm64-cmp 0 1)
                                                  (append-code
                                                    (arm64-cset 0 10)  ; GE = 10
                                                    (arm64-lsl 0 0 4)))
                                                (if (symbol=? op (quote and))
                                                  ;;; Logical AND: convert both to booleans then AND
                                                  (let ((cmp0 (arm64-cmp 0 31)))  ; cmp x0, xzr
                                                    (let ((cset0 (arm64-cset 2 1)))  ; cset x2, NE
                                                      (let ((cmp1 (arm64-cmp 1 31)))  ; cmp x1, xzr
                                                        (let ((cset1 (arm64-cset 3 1)))  ; cset x3, NE
                                                          (let ((and-op (arm64-and 0 2 3)))  ; and x0, x2, x3
                                                            (append-code cmp0
                                                              (append-code cset0
                                                                (append-code cmp1
                                                                  (append-code cset1
                                                                    (append-code and-op
                                                                      (arm64-lsl 0 0 4)))))))))))
                                                  (if (symbol=? op (quote or))
                                                    ;;; Logical OR: convert both to booleans then OR
                                                    (let ((cmp0 (arm64-cmp 0 31)))
                                                      (let ((cset0 (arm64-cset 2 1)))
                                                        (let ((cmp1 (arm64-cmp 1 31)))
                                                          (let ((cset1 (arm64-cset 3 1)))
                                                            (let ((or-op (arm64-orr 0 2 3)))  ; orr x0, x2, x3
                                                              (append-code cmp0
                                                                (append-code cset0
                                                                  (append-code cmp1
                                                                    (append-code cset1
                                                                      (append-code or-op
                                                                        (arm64-lsl 0 0 4)))))))))))
                                                    (arm64-add 0 0 0)))))))))))))
                        ;;; Combine all code
                        (append-code code1
                          (append-code save-code
                            (append-code code2
                              (append-code move-code
                                (append-code load-code op-code)))))))))))))

      (if (has-tag? ir (quote if))
        ;;; If expression: (if test then else)
        (let ((test-expr (car (cdr ir))))
          (let ((then-expr (car (cdr (cdr ir)))))
            (let ((else-expr (car (cdr (cdr (cdr ir))))))
              ;;; Compile test
              (let ((test-code (codegen-expr test-expr runtime-addrs fn-offsets current-offset nil)))
                ;;; Compare result with zero (nil/false)
                (let ((cmp-code (cmp-zero)))
                  ;;; Compile then and else branches - both in tail position if the if is
                  (let ((then-code (codegen-expr then-expr runtime-addrs fn-offsets current-offset tail?)))
                    (let ((else-code (codegen-expr else-expr runtime-addrs fn-offsets current-offset tail?)))
                      ;;; Calculate offsets
                      (let ((then-size (count-instrs then-code)))
                        (let ((else-size (count-instrs else-code)))
                          ;;; b.eq else: from b.eq to else = then + b-end + b.eq itself
                          (let ((beq-offset (+ (+ then-size 1) 1)))
                            ;;; b end: from b to end = else + b itself
                            (let ((b-offset (+ else-size 1)))
                              (let ((beq-instr (arm64-b-cond 0 beq-offset)))  ; EQ=0
                                (let ((b-instr (arm64-b b-offset)))
                                  ;;; Assemble: test, cmp, beq, then, b, else
                                  (append-code test-code
                                    (append-code cmp-code
                                      (append-code beq-instr
                                        (append-code then-code
                                          (append-code b-instr else-code)))))))))))))))

        (if (has-tag? ir (quote progn))
          ;;; Progn: sequential execution
          (let ((exprs (car (cdr ir))))
            (codegen-progn-list exprs runtime-addrs))

          (if (has-tag? ir (quote cond))
            ;;; Cond: multi-way conditional
            (let ((clauses (car (cdr ir))))
              (codegen-cond-clauses clauses runtime-addrs))

              ;;; Unknown
              (arm64-movz 0 #x0))))))))))))))))))) ; close codegen-expr

#-sbcl
(defun codegen-main-with-runtime (ir runtime-addrs fn-offsets starting-offset)
  "Generate complete main function
   fn-offsets: list of (fname offset) for function calls
   starting-offset: offset where main starts (after all functions)"
  (let ((prologue (make-safe-prologue)))
    (let ((prologue-size (count-instrs prologue)))
      (let ((body-offset (+ starting-offset prologue-size)))
        ;;; Main body not in tail position (returns to OS)
        (let ((body (codegen-expr ir runtime-addrs fn-offsets body-offset nil)))
          (let ((untag (arm64-lsr 0 0 4)))
            (let ((epilogue (make-safe-epilogue)))
              (append-code prologue
                (append-code body
                  (append-code untag epilogue))))))))))

#-sbcl
(defun codegen-main (ir)
  "Generate main function without runtime addresses (defaults to zero)"
  (codegen-main-with-runtime ir (quote nil) (quote nil) 0))

;;; ============================================
;;; Compiler Integration
;;; ============================================

#-sbcl
(defun compile-progn-list (exprs env fenv)
  "Compile list of expressions for progn with environment and function environment"
  (if (cons? exprs)
    (let ((first (compile-expr (car exprs) env fenv)))
      (let ((rest (cdr exprs)))
        (if (cons? rest)
          (cons first (compile-progn-list rest env fenv))
          (list first))))  ; Last expression
    nil))

(defun compile-lambda-args (args env fenv)
  "Compile list of lambda arguments"
  (if (cons? args)
    (cons (compile-expr (car args) env fenv)
          (compile-lambda-args (cdr args) env fenv))
    nil))

(defun pair-params-args-helper (params compiled-args offset)
  "Pair parameters with their compiled argument values at sequential offsets"
  (if (cons? params)
    (if (cons? compiled-args)
      (let ((param (car params)))
        (let ((arg-ir (car compiled-args)))
          (cons (list param arg-ir offset)
                (pair-params-args-helper (cdr params) (cdr compiled-args) (+ offset 1)))))
      nil)  ; not enough args
    nil))

(defun pair-params-args (params compiled-args offset)
  "Pair parameters with their compiled argument values starting at offset"
  (pair-params-args-helper params compiled-args offset))

(defun extend-env-with-lambda-params (params offset env)
  "Extend environment with lambda parameters at sequential offsets"
  (if (cons? params)
    (let ((param (car params)))
      (extend-env-with-lambda-params (cdr params)
                                     (+ offset 1)
                                     (env-extend param offset env)))
    env))

;;; ============================================
;;; Environment Handling
;;; ============================================

(defun env-lookup (var env)
  "Look up variable in environment, return offset or nil"
  (if (cons? env)
    (let ((binding (car env)))
      (let ((name (car binding)))
        (if (symbol=? var name)
          (car (cdr binding))  ; return offset
          (env-lookup var (cdr env)))))
    nil))

(defun env-extend (var offset env)
  "Extend environment with new binding"
  (cons (list var offset) env))

;;; ============================================
;;; Function Environment
;;; ============================================

(defun fenv-lookup (fname fenv)
  "Look up function in function environment, return function info or nil"
  (if (cons? fenv)
    (let ((binding (car fenv)))
      (let ((name (car binding)))
        (if (symbol=? fname name)
          (cdr binding)  ; return function info (params, body)
          (fenv-lookup fname (cdr fenv)))))
    nil))

(defun fenv-extend (fname params body fenv)
  "Extend function environment with new function definition"
  (cons (list fname params body) fenv))

(defun compile-param-bindings-helper (params offset)
  "Helper to create environment bindings for function parameters
   Returns list of (param offset) bindings with sequential offsets"
  (if (cons? params)
    (let ((param (car params)))
      (let ((rest (cdr params)))
        (cons (list param offset)
              (compile-param-bindings-helper rest (+ offset 1)))))
    nil))

(defun compile-param-bindings (params)
  "Create environment bindings for function parameters starting at offset 0"
  (compile-param-bindings-helper params 0))

(defun count-params (params)
  "Count number of parameters in list"
  (if (cons? params)
    (+ 1 (count-params (cdr params)))
    0))

(defun codegen-save-params-helper (n)
  "Generate code to save first n parameters from x0-xn to stack
   Saves x0, x1, x2, ... sequentially"
  (if (= n 0)
    nil
    (if (= n 1)
      ;;; Save just x0
      (arm64-str 0 31 -16)
      (if (= n 2)
        ;;; Save x0 then x1
        (append-code (arm64-str 0 31 -16)
                     (arm64-str 1 31 -16))
        (if (= n 3)
          ;;; Save x0, x1, x2
          (append-code (arm64-str 0 31 -16)
            (append-code (arm64-str 1 31 -16)
                         (arm64-str 2 31 -16)))
          ;;; For now, support up to 3 params
          (append-code (arm64-str 0 31 -16)
            (append-code (arm64-str 1 31 -16)
                         (arm64-str 2 31 -16))))))))

#-sbcl
(defun make-safe-prologue ()
  "Generate safe function prologue: sub sp, stp, mov x29
   Avoids page boundary crashes by allocating stack first"
  (let ((sub-sp (arm64-sub-imm 31 31 32)))
    (let ((save-fp-lr (arm64-stp 29 30 31 0)))
      (let ((set-fp (arm64-add-imm 29 31 0)))
        (append-code sub-sp (append-code save-fp-lr set-fp))))))

#-sbcl
(defun make-safe-epilogue ()
  "Generate safe function epilogue: ldp, add sp, ret
   Matches safe prologue pattern"
  (let ((restore-fp-lr (arm64-ldp 29 30 31 0)))
    (let ((restore-sp (arm64-add-imm 31 31 32)))
      (append-code restore-fp-lr (append-code restore-sp (arm64-ret))))))

(defun codegen-function-with-runtime (params body-ir runtime-addrs)
  "Generate code for a complete function with parameters
   Returns machine code with prologue, parameter saves, body, epilogue"
  (let ((param-count (count-params params)))
    (let ((prologue (make-safe-prologue)))
      (let ((save-params (codegen-save-params-helper param-count)))
        ;;; Function body is in tail position (can use tail-call optimization)
        (let ((body-code (codegen-expr body-ir runtime-addrs (quote nil) 0 (quote true))))
          (let ((restore-stack (arm64-add-imm 31 31 (* param-count 16))))
            (let ((untag (arm64-lsr 0 0 4)))
              (let ((epilogue (make-safe-epilogue)))
                (append-code prologue
                  (append-code save-params
                    (append-code body-code
                      (append-code restore-stack
                        (append-code untag epilogue)))))))))))))

#-sbcl
(defun compile-let-bindings (bindings offset env fenv)
  "Compile list of let bindings, each with incrementing offset"
  (if (cons? bindings)
    (let ((binding (car bindings)))
      (if (cons? binding)
        (let ((var (car binding)))
          (let ((rest (cdr binding)))
            (if (cons? rest)
              (let ((value (car rest)))
                ;;; Compile this binding's value in current env
                (cons (list var (compile-expr value env fenv) offset)
                      (compile-let-bindings (cdr bindings) (+ offset 1) env fenv)))
              nil)))  ; malformed binding
        nil))  ; malformed binding
    nil))

#-sbcl
(defun extend-env-with-bindings (bindings offset env)
  "Extend environment with all bindings at sequential offsets"
  (if (cons? bindings)
    (let ((binding (car bindings)))
      (if (cons? binding)
        (let ((var (car binding)))
          (extend-env-with-bindings (cdr bindings)
                                    (+ offset 1)
                                    (env-extend var offset env)))
        env))  ; skip malformed
    env))

#-sbcl
(defun compile-cond-clauses (clauses env fenv)
  "Compile list of (test result) pairs for cond with environment and function environment"
  (if (cons? clauses)
    (let ((clause (car clauses)))
      (if (cons? clause)
        (let ((test (car clause)))
          (let ((rest (cdr clause)))
            (if (cons? rest)
              (let ((result (car rest)))
                (let ((rest-clauses (cdr clauses)))
                  (cons (list (compile-expr test env fenv) (compile-expr result env fenv))
                        (if (cons? rest-clauses)
                          (compile-cond-clauses rest-clauses env fenv)
                          nil))))
              ;;; Malformed clause
              (compile-cond-clauses (cdr clauses) env fenv))))
        ;;; Malformed clause
        (compile-cond-clauses (cdr clauses) env fenv))))
    nil)

#-sbcl
(defun compile-expr (expr env fenv)
  "Compile expression to IR with environment for variable bindings and function environment"
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      ;;; Variable reference - look up in environment
      (let ((offset (env-lookup expr env)))
        (if offset
          (list (quote var) offset)  ; found in environment
          (list (quote lit) 0)))  ; unbound variable returns 0
      (if (cons? expr)
        (let ((op (car expr)))
        (let ((args (cdr expr)))
          ;;; Special form: if
          (if (symbol=? op (quote if))
            (if (cons? args)
              (let ((test (car args)))
                (let ((rest (cdr args)))
                  (if (cons? rest)
                    (let ((then-expr (car rest)))
                      (let ((else-rest (cdr rest)))
                        (if (cons? else-rest)
                          (list (quote if)
                                (compile-expr test env fenv)
                                (compile-expr then-expr env fenv)
                                (compile-expr (car else-rest) env fenv))
                          (list (quote if)
                                (compile-expr test env fenv)
                                (compile-expr then-expr env fenv)
                                (list (quote lit) 0)))))  ; default else: 0
                    (list (quote lit) 0))))  ; malformed
              (list (quote lit) 0))  ; malformed
            ;;; Special form: progn
            (if (symbol=? op (quote progn))
              (if (cons? args)
                ;;; Compile all expressions in sequence
                (list (quote progn) (compile-progn-list args env fenv))
                (list (quote lit) 0))  ; empty progn returns 0
              ;;; Special form: quote
              (if (symbol=? op (quote quote))
                (if (cons? args)
                  ;;; For now, just handle quoted fixnums
                  (let ((quoted (car args)))
                    (if (fixnum? quoted)
                      (list (quote lit) quoted)
                      (list (quote lit) 0)))  ; non-fixnum quotes return 0 for now
                  (list (quote lit) 0))  ; malformed
                ;;; Special form: let (supports multiple bindings)
                (if (symbol=? op (quote let))
                  (if (cons? args)
                    (let ((bindings (car args)))
                      (if (cons? bindings)
                        (let ((body-exprs (cdr args)))
                          (if (cons? body-exprs)
                            ;;; Compile all bindings with sequential offsets
                            (list (quote let-multi)
                                  (compile-let-bindings bindings 0 env fenv)
                                  (compile-expr (car body-exprs)
                                    (extend-env-with-bindings bindings 0 env) fenv))
                            (list (quote lit) 0)))  ; malformed - no body
                        (list (quote lit) 0)))  ; malformed - empty bindings
                    (list (quote lit) 0))  ; malformed
                  ;;; Special form: cond
                  (if (symbol=? op (quote cond))
                  (if (cons? args)
                    ;;; Compile all cond clauses
                    (list (quote cond) (compile-cond-clauses args env fenv))
                    (list (quote lit) 0))  ; empty cond returns 0
                  ;;; Special form: lambda
                  (if (symbol=? op (quote lambda))
                    (if (cons? args)
                      (let ((params (car args)))
                        (let ((body-exprs (cdr args)))
                          (if (cons? body-exprs)
                            ;;; Return lambda IR with params and body
                            (list (quote lambda) params (car body-exprs))
                            (list (quote lit) 0))))  ; malformed - no body
                      (list (quote lit) 0))  ; malformed
                    ;;; Special form: when
                    (if (symbol=? op (quote when))
                      (if (cons? args)
                        (let ((test (car args)))
                          (let ((rest (cdr args)))
                            (if (cons? rest)
                              ;;; (when test body) expands to (if test body nil)
                              (list (quote if)
                                    (compile-expr test env fenv)
                                    (compile-expr (car rest) env fenv)
                                    (list (quote lit) 0))
                              (list (quote lit) 0))))  ; malformed
                        (list (quote lit) 0))  ; malformed
                    ;;; Special form: unless
                    (if (symbol=? op (quote unless))
                      (if (cons? args)
                        (let ((test (car args)))
                          (let ((rest (cdr args)))
                            (if (cons? rest)
                              ;;; (unless test body) expands to (if test nil body)
                              (list (quote if)
                                    (compile-expr test env fenv)
                                    (list (quote lit) 0)
                                    (compile-expr (car rest) env fenv))
                              (list (quote lit) 0))))  ; malformed
                        (list (quote lit) 0))  ; malformed
                      ;;; Check if this is a lambda application ((lambda ...) args)
                      (if (cons? op)
                        (if (symbol=? (car op) (quote lambda))
                          ;;; Lambda application: ((lambda (x) body) arg)
                          (let ((lambda-args (cdr op)))
                            (if (cons? lambda-args)
                              (let ((params (car lambda-args)))
                                (let ((lambda-body-list (cdr lambda-args)))
                                  (if (cons? lambda-body-list)
                                    (let ((body (car lambda-body-list)))
                                      (if (cons? args)
                                        ;;; Compile as let bindings
                                        (let ((arg-values (compile-lambda-args args env fenv)))
                                          (list (quote let-multi)
                                                (pair-params-args params arg-values 0)
                                                (compile-expr body
                                                  (extend-env-with-lambda-params params 0 env) fenv)))
                                        (list (quote lit) 0)))  ; no args
                                    (list (quote lit) 0))))  ; malformed
                              (list (quote lit) 0)))  ; malformed
                          ;;; Regular call
                          (if (cons? args)
                            (let ((arg1 (car args)))
                              (let ((rest (cdr args)))
                                (if (cons? rest)
                                  (list (quote call) op
                                        (compile-expr arg1 env fenv)
                                        (compile-expr (car rest) env))
                                  (list (quote call) op (compile-expr arg1 env fenv)))))
                            (list (quote call) op)))
                        ;;; Regular call with symbol operator
                        ;;; Check if it's a known function
                        (let ((fn-info (fenv-lookup op fenv)))
                          (if fn-info
                            ;;; It's a defined function - compile as function call
                            (list (quote fncall) op (compile-lambda-args args env))
                            ;;; Regular built-in operation
                            (if (cons? args)
                              (let ((arg1 (car args)))
                                (let ((rest (cdr args)))
                                  (if (cons? rest)
                                    (list (quote call) op
                                          (compile-expr arg1 env fenv)
                                          (compile-expr (car rest) env))
                                    (list (quote call) op (compile-expr arg1 env fenv)))))
                              (list (quote call) op))))))))))))))))))) ; close compile-expr

(defun compile-to-arm64-with-runtime (expr runtime-addrs)
  "Full pipeline with explicit runtime addresses: Habu expr → IR → ARM64 bytes"
  (codegen-main-with-runtime (compile-expr expr nil nil) runtime-addrs (quote nil) 0))

#-sbcl
(defun compile-to-arm64 (expr)
  "Full pipeline: Habu expr → IR → ARM64 bytes"
  (compile-to-arm64-with-runtime expr (quote nil)))

;;; ============================================
;;; Function Definition and Multi-Form Compilation
;;; ============================================

(defun compile-defun (name params body env fenv)
  "Compile a function definition
   Returns: (name params-count compiled-body-ir)"
  (let ((param-count (count-params params)))
    (let ((param-env (compile-param-bindings params)))
      (let ((body-ir (compile-expr body param-env fenv)))
        (list name param-count body-ir)))))

(defun compile-forms-helper (forms env fenv)
  "Compile list of top-level forms, separating defuns from main expression"
  (if (cons? forms)
    (let ((form (car forms)))
      (if (cons? form)
        (let ((op (car form)))
          (if (symbol=? op (quote defun))
            ;;; It's a defun - compile it and add to function environment
            (if (cons? (cdr form))
              (let ((name (car (cdr form))))
                (let ((rest (cdr (cdr form))))
                  (if (cons? rest)
                    (let ((params (car rest)))
                      (let ((body-rest (cdr rest)))
                        (if (cons? body-rest)
                          (let ((body (car body-rest)))
                            ;;; Compile this defun
                            (let ((compiled-fn (compile-defun name params body env fenv)))
                              ;;; Add to fenv and continue
                              (let ((new-fenv (cons compiled-fn fenv)))
                                (let ((rest-result (compile-forms-helper (cdr forms) env new-fenv)))
                                  (let ((rest-fns (car rest-result)))
                                    (let ((main-ir (car (cdr rest-result))))
                                      (list (cons compiled-fn rest-fns) main-ir)))))))
                          ;;; Malformed defun - skip
                          (compile-forms-helper (cdr forms) env fenv))))
                    ;;; Malformed defun - skip
                    (compile-forms-helper (cdr forms) env fenv))))
              ;;; Malformed defun - skip
              (compile-forms-helper (cdr forms) env fenv))
            ;;; Not a defun - treat as main expression
            (list fenv (compile-expr form env fenv))))
        ;;; Not a list - treat as main expression
        (list fenv (compile-expr form env fenv))))
    ;;; No more forms - return empty
    (list fenv (list (quote lit) 0))))

(defun compile-forms (forms)
  "Compile list of top-level forms
   Returns: (list-of-compiled-functions main-expression-ir)"
  (compile-forms-helper forms nil nil))

(defun codegen-functions-helper (compiled-fns current-offset runtime-addrs)
  "Generate machine code for all compiled functions
   Returns: (total-code function-offsets)
   function-offsets is list of (name offset-in-instructions)"
  (if (cons? compiled-fns)
    (let ((fn (car compiled-fns)))
      (let ((name (car fn)))
        (let ((param-count (car (cdr fn))))
          (let ((body-ir (car (cdr (cdr fn)))))
            ;;; Generate code for this function
            (let ((fn-code (codegen-function-with-params param-count body-ir runtime-addrs)))
              (let ((fn-size (count-instrs fn-code)))
                ;;; Recurse for rest of functions
                (let ((rest-result (codegen-functions-helper (cdr compiled-fns)
                                                             (+ current-offset fn-size)
                                                             runtime-addrs)))
                  (let ((rest-code (car rest-result)))
                    (let ((rest-offsets (car (cdr rest-result))))
                      (list (append-code fn-code rest-code)
                            (cons (list name current-offset) rest-offsets)))))))))))
    ;;; No more functions
    (list nil nil)))

(defun codegen-function-with-params (param-count body-ir runtime-addrs)
  "Generate code for function with parameter count
   Parameters come in x0, x1, etc. and are saved to stack"
  (let ((prologue (make-safe-prologue)))
    (let ((save-params (codegen-save-params-helper param-count)))
      ;;; Function body is in tail position
      (let ((body-code (codegen-expr body-ir runtime-addrs (quote nil) 0 (quote true))))
        (let ((restore-stack (if (= param-count 0)
                               nil
                               (arm64-add-imm 31 31 (* param-count 16)))))
          (let ((untag (arm64-lsr 0 0 4)))
            (let ((epilogue (make-safe-epilogue)))
              (append-code prologue
                (append-code save-params
                  (append-code body-code
                    (append-code restore-stack
                      (append-code untag epilogue))))))))))))

(defun codegen-function (params body-ir)
  "Generate function code using default runtime addresses"
  (codegen-function-with-runtime params body-ir (quote nil)))

(defun compile-program-with-functions-with-runtime (forms runtime-addrs)
  "Compile entire program with function definitions
   Returns: complete machine code with all functions + main"
  (let ((compile-result (compile-forms forms)))
    (let ((compiled-fns (car compile-result)))
      (let ((main-ir (car (cdr compile-result))))
        ;;; Generate code for all functions
        (let ((fns-result (codegen-functions-helper compiled-fns 0 runtime-addrs)))
          (let ((fns-code (car fns-result)))
            (let ((fn-offsets (car (cdr fns-result))))
              (let ((fns-size (count-instrs fns-code)))
                ;;; Generate main code (starts after all functions)
                (let ((main-code (codegen-main-with-runtime main-ir runtime-addrs fn-offsets fns-size)))
                  ;;; Combine: functions first, then main
                  (append-code fns-code main-code))))))))))

(defun compile-program-with-functions (forms)
  "Compile program using default runtime addresses (nil placeholders)"
  (compile-program-with-functions-with-runtime forms (quote nil)))

;;; ============================================
;;; Tests (commented out - run manually if needed)
;;; ============================================

;;; (compile-expr 42 nil)
;;; (compile-expr (quote (+ 3 4)) nil)
;;; (compile-to-arm64 42)
;;; (compile-to-arm64 (quote (+ 5 7)))

) ; end #-sbcl progn
