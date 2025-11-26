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
   #:movz #:movk #:mov
   ;; Arithmetic
   #:add #:sub #:mul #:sdiv #:neg
   ;; Bitwise
   #:and* #:orr #:eor #:lsl #:lsr #:asr
   ;; Memory
   #:ldr #:str #:ldp #:stp
   ;; Compare
   #:cmp #:cset
   ;; Branch
   #:b #:bl #:blr #:b.eq #:b.ne #:b.lt #:b.le #:b.gt #:b.ge #:ret
   ;; Condition codes
   #:+eq+ #:+ne+ #:+lt+ #:+le+ #:+gt+ #:+ge+
   ;; Registers (by convention)
   #:+sp+ #:+lr+ #:+xzr+))

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

;;; ============================================================
;;; Arithmetic
;;; ============================================================

(defun add (rd rn rm-or-imm &key imm)
  "ADD Xd, Xn, Xm  or  ADD Xd, Xn, #imm
   Add register or 12-bit immediate."
  (if imm
      (encode (logior #x91000000
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash rn 5)
                      rd))
      (encode (logior #x8B000000
                      (ash rm-or-imm 16)
                      (ash rn 5)
                      rd))))

(defun sub (rd rn rm-or-imm &key imm)
  "SUB Xd, Xn, Xm  or  SUB Xd, Xn, #imm
   Subtract register or 12-bit immediate."
  (if imm
      (encode (logior #xD1000000
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

(defun and* (rd rn rm)
  "AND Xd, Xn, Xm
   Bitwise AND. Named and* to avoid CL conflict."
  (encode (logior #x8A000000
                  (ash rm 16)
                  (ash rn 5)
                  rd)))

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
