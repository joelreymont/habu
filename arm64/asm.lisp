;;;; ARM64 Instruction Encoders
;;;;
;;;; Standalone ARM64 assembler with clean API.
;;;; Uses keyword arguments and hex constants throughout.
;;;; No dependencies on other Habu packages.

(defpackage :arm64
  (:use :cl)
  (:export
   ;; Core encoding
   #:encode
   ;; Data movement
   #:movz #:movk #:mov #:adrp #:adr #:add-lo12
   ;; Arithmetic
   #:add #:sub #:mul #:sdiv #:neg
   ;; Bitwise
   #:and* #:orr #:eor #:bic #:lsl #:lsr #:asr
   ;; Memory
   #:ldr #:str #:ldp #:stp #:ldrb #:strb
   ;; Compare
   #:cmp #:cset
   ;; Branch
   #:b #:bl #:br #:blr #:cbz #:cbnz #:b.eq #:b.ne #:b.lt #:b.le #:b.gt #:b.ge #:ret
   ;; System
   #:svc
   ;; Condition codes
   #:+eq+ #:+ne+ #:+lt+ #:+le+ #:+gt+ #:+ge+
   ;; Registers (by convention)
   #:+sp+ #:+lr+ #:+xzr+
   ;; macOS syscall numbers
   #:+sys-exit+ #:+sys-read+ #:+sys-write+ #:+sys-open+ #:+sys-close+))

(in-package :arm64)

;;; ============================================================
;;; Constants
;;; ============================================================

;; Condition codes
(defconstant +eq+ #x0)   ; equal
(defconstant +ne+ #x1)   ; not equal
(defconstant +lt+ #xB)   ; signed less than
(defconstant +le+ #xD)   ; signed less or equal
(defconstant +gt+ #xC)   ; signed greater than
(defconstant +ge+ #xA)   ; signed greater or equal

;; macOS ARM64 syscall numbers (BSD layer, use with SVC #x80)
(defconstant +sys-exit+  1)
(defconstant +sys-read+  3)
(defconstant +sys-write+ 4)
(defconstant +sys-open+  5)
(defconstant +sys-close+ 6)

;; Special registers (by convention, all are encoded as 31)
(defconstant +sp+  31)   ; stack pointer
(defconstant +lr+  30)   ; link register (x30)
(defconstant +xzr+ 31)   ; zero register

;;; ============================================================
;;; Core Encoding
;;; ============================================================

(defun encode (word)
  "Encode 32-bit instruction as little-endian byte list."
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

;;; ============================================================
;;; Data Movement
;;; ============================================================

(defun movz (rd imm &key (lsl 0))
  "MOVZ Xd, #imm{, LSL #shift}
   Move wide with zero. LSL must be 0, 16, 32, or 48."
  (encode (logior #xD2800000
                  (ash (ash lsl -4) 21)
                  (ash (logand imm #xFFFF) 5)
                  rd)))

(defun movk (rd imm &key (lsl 0))
  "MOVK Xd, #imm{, LSL #shift}
   Move wide with keep. LSL must be 0, 16, 32, or 48."
  (encode (logior #xF2800000
                  (ash (ash lsl -4) 21)
                  (ash (logand imm #xFFFF) 5)
                  rd)))

(defun mov (rd rm)
  "MOV Xd, Xm
   Move register (alias for ORR Xd, XZR, Xm)."
  (encode (logior #xAA0003E0 (ash rm 16) rd)))

(defun adrp (rd page-offset)
  "ADRP Xd, label
   Load PC-relative page address. PAGE-OFFSET is the signed page offset.
   Note: This encodes a placeholder - actual address requires relocation."
  ;; ADRP: immlo in bits 30:29, immhi in bits 23:5
  (let* ((immlo (logand page-offset #x3))
         (immhi (logand (ash page-offset -2) #x7FFFF)))
    (encode (logior #x90000000
                    (ash immlo 29)
                    (ash immhi 5)
                    rd))))

(defun adr (rd byte-offset)
  "ADR Xd, label
   Load PC-relative address. BYTE-OFFSET is signed, +/-1MB range.
   Computes Xd = PC + byte-offset."
  ;; ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd[4:0]
  (let* ((immlo (logand byte-offset #x3))
         (immhi (logand (ash byte-offset -2) #x7FFFF)))
    (encode (logior #x10000000
                    (ash immlo 29)
                    (ash immhi 5)
                    rd))))

(defun add-lo12 (rd rn lo12)
  "ADD Xd, Xn, #:lo12:label
   Add low 12 bits of address. For use with ADRP."
  (encode (logior #x91000000
                  (ash (logand lo12 #xFFF) 10)
                  (ash rn 5)
                  rd)))

;;; ============================================================
;;; Arithmetic
;;; ============================================================

(defun add (rd rn rm-or-imm &key imm shift12)
  "ADD Xd, Xn, Xm  or  ADD Xd, Xn, #imm [, LSL #12]
   Add register or 12-bit immediate. SHIFT12 shifts immediate left by 12."
  (if imm
      (encode (logior #x91000000
                      (if shift12 #x400000 0)  ; bit 22 = sh
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash rn 5)
                      rd))
      (encode (logior #x8B000000
                      (ash rm-or-imm 16)
                      (ash rn 5)
                      rd))))

(defun sub (rd rn rm-or-imm &key imm shift12)
  "SUB Xd, Xn, Xm  or  SUB Xd, Xn, #imm [, LSL #12]
   Subtract register or 12-bit immediate. SHIFT12 shifts immediate left by 12."
  (if imm
      (encode (logior #xD1000000
                      (if shift12 #x400000 0)  ; bit 22 = sh
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash rn 5)
                      rd))
      (encode (logior #xCB000000
                      (ash rm-or-imm 16)
                      (ash rn 5)
                      rd))))

(defun mul (rd rn rm)
  "MUL Xd, Xn, Xm
   Multiply (alias for MADD Xd, Xn, Xm, XZR)."
  (encode (logior #x9B007C00
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

(defun sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm
   Signed divide."
  (encode (logior #x9AC00C00
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

(defun neg (rd rm)
  "NEG Xd, Xm
   Negate (alias for SUB Xd, XZR, Xm)."
  (encode (logior #xCB0003E0 (ash rm 16) rd)))

;;; ============================================================
;;; Bitwise Operations
;;; ============================================================

(defun and* (rd rn rm-or-imm &key imm)
  "AND Xd, Xn, Xm  or  AND Xd, Xn, #mask
   Bitwise AND. Named and* to avoid CL conflict.
   Use :imm t for immediate mode with common masks:
   #x7 (low 3 bits), #xF (low 4 bits), #xFF (low 8 bits),
   -4 (~3), -8 (~7), -16 (~15), -32 (~31) for alignment."
  (if imm
      ;; Immediate mode - encode common masks
      ;; ARM64 logical immediate encoding: 0x92400000 | (immr << 16) | (imms << 10) | Rn | Rd
      (let ((base #x92400000)
            (rn-shift (ash rn 5)))
        (cond
          ;; Keep low bits masks (N=1, immr=0, imms=bits-1)
          ((= rm-or-imm #x7)   ; low 3 bits: imms=2
           (encode (logior base (ash 2 10) rn-shift rd)))
          ((= rm-or-imm #xF)   ; low 4 bits: imms=3
           (encode (logior base (ash 3 10) rn-shift rd)))
          ((= rm-or-imm #xFF)  ; low 8 bits: imms=7
           (encode (logior base (ash 7 10) rn-shift rd)))
          ;; Alignment masks (clear low bits) - immr=64-N, imms=63-N
          ((or (= rm-or-imm #xFFFFFFFFFFFFFFFC) (= rm-or-imm -4))   ; ~3
           (encode (logior base (ash 62 16) (ash 61 10) rn-shift rd)))
          ((or (= rm-or-imm #xFFFFFFFFFFFFFFF8) (= rm-or-imm -8))   ; ~7
           (encode (logior base (ash 61 16) (ash 60 10) rn-shift rd)))
          ((or (= rm-or-imm #xFFFFFFFFFFFFFFF0) (= rm-or-imm -16))  ; ~15
           (encode (logior base (ash 60 16) (ash 59 10) rn-shift rd)))
          ((or (= rm-or-imm #xFFFFFFFFFFFFFFE0) (= rm-or-imm -32))  ; ~31
           (encode (logior base (ash 59 16) (ash 58 10) rn-shift rd)))
          (t
           (error "and* :imm - unsupported mask #x~X. Use common masks or encode manually." rm-or-imm))))
      ;; Register mode
      (encode (logior #x8A000000
                      (ash rm-or-imm 16)
                      (ash rn 5)
                      rd))))

(defun orr (rd rn rm)
  "ORR Xd, Xn, Xm
   Bitwise OR."
  (encode (logior #xAA000000
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

(defun eor (rd rn rm)
  "EOR Xd, Xn, Xm
   Bitwise XOR."
  (encode (logior #xCA000000
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

(defun bic (rd rn rm)
  "BIC Xd, Xn, Xm
   Bit clear (AND with NOT Xm)."
  (encode (logior #x8A200000
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

(defun lsl (rd rn shift &key imm)
  "LSL Xd, Xn, Xm  or  LSL Xd, Xn, #shift
   Logical shift left."
  (if imm
      ;; LSL immediate: UBFM Xd, Xn, #(-shift MOD 64), #(63-shift)
      (encode (logior #xD3400000
                      (ash (logand (- #x40 shift) #x3F) 16)
                      (ash (- #x3F shift) 10)
                      (ash rn 5)
                      rd))
      ;; LSL register: LSLV
      (encode (logior #x9AC02000
                      (ash shift 16)
                      (ash rn 5)
                      rd))))

(defun lsr (rd rn shift &key imm)
  "LSR Xd, Xn, Xm  or  LSR Xd, Xn, #shift
   Logical shift right."
  (if imm
      ;; LSR immediate: UBFM Xd, Xn, #shift, #63
      (encode (logior #xD340FC00
                      (ash shift 16)
                      (ash rn 5)
                      rd))
      ;; LSR register: LSRV
      (encode (logior #x9AC02400
                      (ash shift 16)
                      (ash rn 5)
                      rd))))

(defun asr (rd rn shift &key imm)
  "ASR Xd, Xn, Xm  or  ASR Xd, Xn, #shift
   Arithmetic shift right."
  (if imm
      ;; ASR immediate: SBFM Xd, Xn, #shift, #63
      (encode (logior #x9340FC00
                      (ash shift 16)
                      (ash rn 5)
                      rd))
      ;; ASR register: ASRV
      (encode (logior #x9AC02800
                      (ash shift 16)
                      (ash rn 5)
                      rd))))

;;; ============================================================
;;; Memory Operations
;;; ============================================================

(defun ldr (rt rn &key (offset 0))
  "LDR Xt, [Xn{, #offset}]
   Load 64-bit register. Offset scaled by 8, must be multiple of 8."
  (encode (logior #xF9400000
                  (ash (ash offset -3) 10)
                  (ash rn 5)
                  rt)))

(defun str (rt rn &key (offset 0))
  "STR Xt, [Xn{, #offset}]
   Store 64-bit register. Offset scaled by 8, must be multiple of 8."
  (encode (logior #xF9000000
                  (ash (ash offset -3) 10)
                  (ash rn 5)
                  rt)))

(defun ldp (rt1 rt2 rn &key (offset 0))
  "LDP Xt1, Xt2, [Xn{, #offset}]
   Load pair. Offset scaled by 8, signed 7-bit."
  (encode (logior #xA9400000
                  (ash (logand (ash offset -3) #x7F) 15)
                  (ash rt2 10)
                  (ash rn 5)
                  rt1)))

(defun stp (rt1 rt2 rn &key (offset 0))
  "STP Xt1, Xt2, [Xn{, #offset}]
   Store pair. Offset scaled by 8, signed 7-bit."
  (encode (logior #xA9000000
                  (ash (logand (ash offset -3) #x7F) 15)
                  (ash rt2 10)
                  (ash rn 5)
                  rt1)))

(defun ldrb (rt rn rm-or-offset &key reg)
  "LDRB Wt, [Xn, Xm]  or  LDRB Wt, [Xn, #offset]
   Load byte, zero-extend to 64-bit.
   Use :reg t for register offset, otherwise immediate offset."
  (if reg
      ;; Register offset: LDRB Wt, [Xn, Xm]
      (encode (logior #x38606800
                      (ash rm-or-offset 16)
                      (ash rn 5)
                      rt))
      ;; Immediate offset: LDRB Wt, [Xn, #offset]
      (encode (logior #x39400000
                      (ash (logand rm-or-offset #xFFF) 10)
                      (ash rn 5)
                      rt))))

(defun strb (rt rn rm-or-offset &key reg)
  "STRB Wt, [Xn, Xm]  or  STRB Wt, [Xn, #offset]
   Store byte.
   Use :reg t for register offset, otherwise immediate offset."
  (if reg
      ;; Register offset: STRB Wt, [Xn, Xm]
      (encode (logior #x38206800
                      (ash rm-or-offset 16)
                      (ash rn 5)
                      rt))
      ;; Immediate offset: STRB Wt, [Xn, #offset]
      (encode (logior #x39000000
                      (ash (logand rm-or-offset #xFFF) 10)
                      (ash rn 5)
                      rt))))

;;; ============================================================
;;; Compare
;;; ============================================================

(defun cmp (rn rm-or-imm &key imm)
  "CMP Xn, Xm  or  CMP Xn, #imm
   Compare (alias for SUBS XZR, Xn, ...)."
  (if imm
      (encode (logior #xF100001F
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash rn 5)))
      (encode (logior #xEB00001F
                      (ash rm-or-imm 16)
                      (ash rn 5)))))

(defun cset (rd cond)
  "CSET Xd, cond
   Conditional set. cond is one of +eq+, +ne+, etc."
  (encode (logior #x9A9F07E0
                  (ash (logxor cond 1) 12)
                  rd)))

;;; ============================================================
;;; Branch Instructions
;;; ============================================================

(defun b (offset)
  "B label
   Unconditional branch. Offset in instructions (not bytes)."
  (encode (logior #x14000000
                  (logand offset #x03FFFFFF))))

(defun bl (offset)
  "BL label
   Branch with link. Offset in instructions (not bytes)."
  (encode (logior #x94000000
                  (logand offset #x03FFFFFF))))

(defun blr (rn)
  "BLR Xn
   Branch with link to register."
  (encode (logior #xD63F0000 (ash rn 5))))

(defun br (rn)
  "BR Xn
   Branch to register (no link)."
  (encode (logior #xD61F0000 (ash rn 5))))

(defun cbz (rt offset)
  "CBZ Xt, label
   Compare and branch if zero. Offset in instructions (not bytes)."
  (encode (logior #xB4000000
                  (ash (logand offset #x7FFFF) 5)
                  rt)))

(defun cbnz (rt offset)
  "CBNZ Xt, label
   Compare and branch if not zero. Offset in instructions (not bytes)."
  (encode (logior #xB5000000
                  (ash (logand offset #x7FFFF) 5)
                  rt)))

(defun b.eq (offset)
  "B.EQ label
   Branch if equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +eq+)))

(defun b.ne (offset)
  "B.NE label
   Branch if not equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +ne+)))

(defun b.lt (offset)
  "B.LT label
   Branch if less than. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +lt+)))

(defun b.le (offset)
  "B.LE label
   Branch if less or equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +le+)))

(defun b.gt (offset)
  "B.GT label
   Branch if greater than. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +gt+)))

(defun b.ge (offset)
  "B.GE label
   Branch if greater or equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  +ge+)))

(defun ret ()
  "RET
   Return from subroutine (branch to LR)."
  (encode #xD65F03C0))

(defun svc (imm16)
  "SVC #imm16
   Supervisor call (syscall). On macOS, use #x80 for syscalls."
  ;; SVC encoding: 1101 0100 000 imm16[15:0] 00001
  ;; = 0xD4000001 | (imm16 << 5)
  (encode (logior #xD4000001 (ash (logand imm16 #xFFFF) 5))))
