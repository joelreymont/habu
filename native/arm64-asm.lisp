;;;; ARM64 Instruction Encoders - Pure Habu Version
;;;;
;;;; Standalone ARM64 assembler for self-hosted compilation.
;;;; No package system, no SBCL dependencies.
;;;; All functions use only operations supported by native Habu:
;;;; - Arithmetic: +, -, *, /
;;;; - Bitwise: logand, logior, logxor, ash
;;;; - Lists: cons, car, cdr, list

;;; Condition codes
(defun arm64-eq () #x0)   ; equal
(defun arm64-ne () #x1)   ; not equal
(defun arm64-lt () #xB)   ; signed less than
(defun arm64-le () #xD)   ; signed less or equal
(defun arm64-gt () #xC)   ; signed greater than
(defun arm64-ge () #xA)   ; signed greater or equal

;;; macOS ARM64 syscall numbers
(defun sys-exit ()  1)
(defun sys-read ()  3)
(defun sys-write () 4)
(defun sys-open ()  5)
(defun sys-close () 6)

;;; Special registers
(defun reg-sp ()  31)    ; stack pointer
(defun reg-lr ()  30)    ; link register (x30)
(defun reg-xzr () 31)    ; zero register

;;; Core encoding - return 4-element list of bytes (little-endian)
(defun arm64-encode (word)
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

;;; Data Movement

(defun arm64-movz (rd imm lsl)
  "MOVZ Xd, #imm{, LSL #shift}. LSL must be 0, 16, 32, or 48."
  (arm64-encode (logior #xD2800000
                        (ash (ash lsl -4) 21)
                        (ash (logand imm #xFFFF) 5)
                        rd)))

(defun arm64-movk (rd imm lsl)
  "MOVK Xd, #imm{, LSL #shift}. LSL must be 0, 16, 32, or 48."
  (arm64-encode (logior #xF2800000
                        (ash (ash lsl -4) 21)
                        (ash (logand imm #xFFFF) 5)
                        rd)))

(defun arm64-mov (rd rm)
  "MOV Xd, Xm (alias for ORR Xd, XZR, Xm)."
  (arm64-encode (logior #xAA0003E0 (ash rm 16) rd)))

(defun arm64-adrp (rd page-offset)
  "ADRP Xd, label. PAGE-OFFSET is signed page offset."
  (let* ((immlo (logand page-offset #x3))
         (immhi (logand (ash page-offset -2) #x7FFFF)))
    (arm64-encode (logior #x90000000
                          (ash immlo 29)
                          (ash immhi 5)
                          rd))))

(defun arm64-adr (rd byte-offset)
  "ADR Xd, label. Computes Xd = PC + byte-offset."
  (let* ((immlo (logand byte-offset #x3))
         (immhi (logand (ash byte-offset -2) #x7FFFF)))
    (arm64-encode (logior #x10000000
                          (ash immlo 29)
                          (ash immhi 5)
                          rd))))

(defun arm64-add-lo12 (rd rn lo12)
  "ADD Xd, Xn, #:lo12:label. Add low 12 bits of address."
  (arm64-encode (logior #x91000000
                        (ash (logand lo12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

;;; Arithmetic

(defun arm64-add-reg (rd rn rm)
  "ADD Xd, Xn, Xm"
  (arm64-encode (logior #x8B000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-add-imm (rd rn imm)
  "ADD Xd, Xn, #imm (12-bit immediate)"
  (arm64-encode (logior #x91000000
                        (ash (logand imm #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun arm64-sub-reg (rd rn rm)
  "SUB Xd, Xn, Xm"
  (arm64-encode (logior #xCB000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-sub-imm (rd rn imm)
  "SUB Xd, Xn, #imm (12-bit immediate)"
  (arm64-encode (logior #xD1000000
                        (ash (logand imm #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun arm64-mul (rd rn rm)
  "MUL Xd, Xn, Xm (alias for MADD Xd, Xn, Xm, XZR)"
  (arm64-encode (logior #x9B007C00
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm"
  (arm64-encode (logior #x9AC00C00
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-neg (rd rm)
  "NEG Xd, Xm (alias for SUB Xd, XZR, Xm)"
  (arm64-encode (logior #xCB0003E0 (ash rm 16) rd)))

;;; Bitwise Operations

(defun arm64-and (rd rn rm)
  "AND Xd, Xn, Xm"
  (arm64-encode (logior #x8A000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-orr (rd rn rm)
  "ORR Xd, Xn, Xm"
  (arm64-encode (logior #xAA000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-eor (rd rn rm)
  "EOR Xd, Xn, Xm"
  (arm64-encode (logior #xCA000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-bic (rd rn rm)
  "BIC Xd, Xn, Xm (AND with NOT Xm)"
  (arm64-encode (logior #x8A200000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-and-imm (rd rn imm-n imm-r imm-s)
  "AND Xd, Xn, #imm (bitmask immediate)"
  (arm64-encode (logior #x92000000
                        (ash imm-n 22)
                        (ash imm-r 16)
                        (ash imm-s 10)
                        (ash rn 5)
                        rd)))

(defun arm64-lsl-imm (rd rn shift)
  "LSL Xd, Xn, #shift"
  (arm64-encode (logior #xD3400000
                        (ash (logand (- #x40 shift) #x3F) 16)
                        (ash (- #x3F shift) 10)
                        (ash rn 5)
                        rd)))

(defun arm64-lsl-reg (rd rn rm)
  "LSL Xd, Xn, Xm (LSLV)"
  (arm64-encode (logior #x9AC02000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-lsr-imm (rd rn shift)
  "LSR Xd, Xn, #shift"
  (arm64-encode (logior #xD340FC00
                        (ash shift 16)
                        (ash rn 5)
                        rd)))

(defun arm64-lsr-reg (rd rn rm)
  "LSR Xd, Xn, Xm (LSRV)"
  (arm64-encode (logior #x9AC02400
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-asr-imm (rd rn shift)
  "ASR Xd, Xn, #shift"
  (arm64-encode (logior #x9340FC00
                        (ash shift 16)
                        (ash rn 5)
                        rd)))

(defun arm64-asr-reg (rd rn rm)
  "ASR Xd, Xn, Xm (ASRV)"
  (arm64-encode (logior #x9AC02800
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

;;; Memory Operations

(defun arm64-ldr (rt rn offset)
  "LDR Xt, [Xn{, #offset}]. Offset scaled by 8."
  (arm64-encode (logior #xF9400000
                        (ash (ash offset -3) 10)
                        (ash rn 5)
                        rt)))

(defun arm64-str (rt rn offset)
  "STR Xt, [Xn{, #offset}]. Offset scaled by 8."
  (arm64-encode (logior #xF9000000
                        (ash (ash offset -3) 10)
                        (ash rn 5)
                        rt)))

(defun arm64-ldp (rt1 rt2 rn offset)
  "LDP Xt1, Xt2, [Xn{, #offset}]. Offset scaled by 8, signed 7-bit."
  (arm64-encode (logior #xA9400000
                        (ash (logand (ash offset -3) #x7F) 15)
                        (ash rt2 10)
                        (ash rn 5)
                        rt1)))

(defun arm64-stp (rt1 rt2 rn offset)
  "STP Xt1, Xt2, [Xn{, #offset}]. Offset scaled by 8, signed 7-bit."
  (arm64-encode (logior #xA9000000
                        (ash (logand (ash offset -3) #x7F) 15)
                        (ash rt2 10)
                        (ash rn 5)
                        rt1)))

(defun arm64-ldrb-reg (rt rn rm)
  "LDRB Wt, [Xn, Xm]. Load byte, zero-extend."
  (arm64-encode (logior #x38606800
                        (ash rm 16)
                        (ash rn 5)
                        rt)))

(defun arm64-strb-reg (rt rn rm)
  "STRB Wt, [Xn, Xm]. Store byte."
  (arm64-encode (logior #x38206800
                        (ash rm 16)
                        (ash rn 5)
                        rt)))

(defun arm64-ldrb-imm (rt rn offset)
  "LDRB Wt, [Xn, #offset]. Load byte with immediate offset."
  (arm64-encode (logior #x39400000
                        (ash (logand offset #xFFF) 10)
                        (ash rn 5)
                        rt)))

(defun arm64-strb-imm (rt rn offset)
  "STRB Wt, [Xn, #offset]. Store byte with immediate offset."
  (arm64-encode (logior #x39000000
                        (ash (logand offset #xFFF) 10)
                        (ash rn 5)
                        rt)))

;;; Compare

(defun arm64-cmp-reg (rn rm)
  "CMP Xn, Xm (alias for SUBS XZR, Xn, Xm)"
  (arm64-encode (logior #xEB00001F
                        (ash rm 16)
                        (ash rn 5))))

(defun arm64-cmp-imm (rn imm)
  "CMP Xn, #imm (alias for SUBS XZR, Xn, #imm)"
  (arm64-encode (logior #xF100001F
                        (ash (logand imm #xFFF) 10)
                        (ash rn 5))))

(defun arm64-cset (rd cond)
  "CSET Xd, cond. cond is one of arm64-eq, arm64-ne, etc."
  (arm64-encode (logior #x9A9F07E0
                        (ash (logxor cond 1) 12)
                        rd)))

;;; Branch Instructions

(defun arm64-b (offset)
  "B label. Offset in instructions (not bytes)."
  (arm64-encode (logior #x14000000
                        (logand offset #x03FFFFFF))))

(defun arm64-bl (offset)
  "BL label. Offset in instructions (not bytes)."
  (arm64-encode (logior #x94000000
                        (logand offset #x03FFFFFF))))

(defun arm64-blr (rn)
  "BLR Xn. Branch with link to register."
  (arm64-encode (logior #xD63F0000 (ash rn 5))))

(defun arm64-br (rn)
  "BR Xn. Branch to register (no link)."
  (arm64-encode (logior #xD61F0000 (ash rn 5))))

(defun arm64-b-cond (offset cond)
  "B.cond label. Conditional branch."
  (arm64-encode (logior #x54000000
                        (ash (logand offset #x7FFFF) 5)
                        cond)))

(defun arm64-b-eq (offset)
  "B.EQ label."
  (arm64-b-cond offset (arm64-eq)))

(defun arm64-b-ne (offset)
  "B.NE label."
  (arm64-b-cond offset (arm64-ne)))

(defun arm64-b-lt (offset)
  "B.LT label."
  (arm64-b-cond offset (arm64-lt)))

(defun arm64-b-le (offset)
  "B.LE label."
  (arm64-b-cond offset (arm64-le)))

(defun arm64-b-gt (offset)
  "B.GT label."
  (arm64-b-cond offset (arm64-gt)))

(defun arm64-b-ge (offset)
  "B.GE label."
  (arm64-b-cond offset (arm64-ge)))

(defun arm64-ret ()
  "RET. Return from subroutine."
  (arm64-encode #xD65F03C0))

(defun arm64-svc (imm16)
  "SVC #imm16. Supervisor call (syscall)."
  (arm64-encode (logior #xD4000001 (ash (logand imm16 #xFFFF) 5))))

;;; Convenience functions

(defun arm64-load-imm16 (rd imm)
  "Load 16-bit immediate into register."
  (arm64-movz rd imm 0))

(defun arm64-load-imm32 (rd imm)
  "Load 32-bit immediate into register (2 instructions)."
  (append (arm64-movz rd (logand imm #xFFFF) 0)
          (arm64-movk rd (logand (ash imm -16) #xFFFF) 16)))

(defun arm64-load-imm48 (rd imm)
  "Load 48-bit immediate into register (3 instructions)."
  (append (arm64-movz rd (logand imm #xFFFF) 0)
          (arm64-movk rd (logand (ash imm -16) #xFFFF) 16)
          (arm64-movk rd (logand (ash imm -32) #xFFFF) 32)))

(defun arm64-load-imm64 (rd imm)
  "Load 64-bit immediate into register (4 instructions)."
  (append (arm64-movz rd (logand imm #xFFFF) 0)
          (arm64-movk rd (logand (ash imm -16) #xFFFF) 16)
          (arm64-movk rd (logand (ash imm -32) #xFFFF) 32)
          (arm64-movk rd (logand (ash imm -48) #xFFFF) 48)))

;;; Prologue/epilogue helpers

(defun arm64-push-pair (rt1 rt2)
  "Push pair to stack with pre-decrement."
  ;; STP rt1, rt2, [sp, #-16]!
  (arm64-encode (logior #xA9BF0000
                        (ash (logand (ash -16 -3) #x7F) 15)
                        (ash rt2 10)
                        (ash 31 5)   ; SP
                        rt1)))

(defun arm64-pop-pair (rt1 rt2)
  "Pop pair from stack with post-increment."
  ;; LDP rt1, rt2, [sp], #16
  (arm64-encode (logior #xA8C10000
                        (ash (logand (ash 16 -3) #x7F) 15)
                        (ash rt2 10)
                        (ash 31 5)   ; SP
                        rt1)))
