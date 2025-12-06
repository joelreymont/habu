;;;; ARM64 Instruction Encoders
;;;;
;;;; Standalone ARM64 assembler with clean API.
;;;; Uses keyword arguments and hex constants throughout.
;;;; Works in both SBCL and native Habu modes.

(defpackage :arm64
  (:use :cl)
  (:export
   ;; Core encoding
   #:encode
   ;; Register keyword resolver
   #:reg
   #:num-to-reg
   ;; Data movement
   #:movz #:movk #:movn #:mov #:adrp #:adr #:add-lo12
   ;; Arithmetic
   #:add #:sub #:subs #:mul #:sdiv #:neg
   ;; Bitwise
   #:and* #:orr #:eor #:bic #:mvn #:lsl #:lsr #:asr
   ;; Memory
   #:ldr #:ldr-reg #:str #:ldp #:stp #:ldrb #:ldrb-post #:strb #:strb-post
   #:ldur #:stur
   ;; Bitwise with immediate
   #:orr-imm
   ;; Compare
   #:cmp #:cset
   ;; Branch
   #:b #:bl #:br #:blr #:cbz #:cbnz #:b.eq #:b.ne #:b.lt #:b.le #:b.gt #:b.ge
   #:b.lo #:b.hs #:b.hi #:b.ls #:ret
   ;; System
   #:svc #:brk #:nop
   ;; Condition codes (for cset instruction)
   #:+cc-eq+ #:+cc-ne+ #:+cc-lt+ #:+cc-le+ #:+cc-gt+ #:+cc-ge+
   #:+cc-lo+ #:+cc-hs+ #:+cc-hi+ #:+cc-ls+
   ;; Legacy condition code names (deprecated, use +cc-* instead)
   #:+eq+ #:+ne+ #:+lt+ #:+le+ #:+gt+ #:+ge+
   #:+lo+ #:+hs+ #:+hi+ #:+ls+
   ;; Special registers
   #:+sp+ #:+lr+ #:+xzr+ #:+fp+
   ;; Habu register conventions
   #:+reg-env+ #:+reg-closure+ #:+reg-code-base+ #:+reg-gc+ #:+reg-heap+
   ;; macOS syscall numbers
   #:+sys-exit+ #:+sys-read+ #:+sys-write+ #:+sys-open+ #:+sys-close+))

(in-package :arm64)

;;; ============================================================
;;; Constants
;;; ============================================================
;;; In SBCL: use defconstant
;;; In native Habu: define as functions (no defconstant support)

;;; ------------------------------------------------------------
;;; Condition Codes (for CSET, CSEL, CINC, etc.)
;;; ------------------------------------------------------------
;;; These encode the condition to test after a CMP instruction.
;;; Usage: (cset rd +cc-gt+) sets rd=1 if greater-than, else 0

#+sbcl (defconstant +cc-eq+ #x0)   ; equal (Z=1)
#+sbcl (defconstant +cc-ne+ #x1)   ; not equal (Z=0)
#+sbcl (defconstant +cc-lt+ #xB)   ; signed less than (N!=V)
#+sbcl (defconstant +cc-le+ #xD)   ; signed less or equal (Z=1 or N!=V)
#+sbcl (defconstant +cc-gt+ #xC)   ; signed greater than (Z=0 and N=V)
#+sbcl (defconstant +cc-ge+ #xA)   ; signed greater or equal (N=V)
#+sbcl (defconstant +cc-lo+ #x3)   ; unsigned lower / carry clear (C=0)
#+sbcl (defconstant +cc-hs+ #x2)   ; unsigned higher or same / carry set (C=1)
#+sbcl (defconstant +cc-hi+ #x8)   ; unsigned higher (C=1 and Z=0)
#+sbcl (defconstant +cc-ls+ #x9)   ; unsigned lower or same (C=0 or Z=1)

;; Legacy names (for backward compatibility)
#+sbcl (defconstant +eq+ #x0)
#+sbcl (defconstant +ne+ #x1)
#+sbcl (defconstant +lt+ #xB)
#+sbcl (defconstant +le+ #xD)
#+sbcl (defconstant +gt+ #xC)
#+sbcl (defconstant +ge+ #xA)
#+sbcl (defconstant +lo+ #x3)
#+sbcl (defconstant +hs+ #x2)
#+sbcl (defconstant +hi+ #x8)
#+sbcl (defconstant +ls+ #x9)

;;; ------------------------------------------------------------
;;; Special Registers
;;; ------------------------------------------------------------
;;; ARM64 has 31 general-purpose registers (x0-x30).
;;; Register 31 is context-dependent: SP or XZR.

#+sbcl (defconstant +sp+  31)   ; stack pointer (when used as base in load/store)
#+sbcl (defconstant +xzr+ 31)   ; zero register (reads as 0, writes discarded)
#+sbcl (defconstant +lr+  30)   ; link register (return address, x30)
#+sbcl (defconstant +fp+  29)   ; frame pointer (x29, by convention)

;;; ------------------------------------------------------------
;;; Habu Register Conventions
;;; ------------------------------------------------------------
;;; These registers have special meaning in Habu-generated code.
;;; See CONTEXT.md for full register usage documentation.

#+sbcl (defconstant +reg-env+       20)  ; x20: environment frame base
#+sbcl (defconstant +reg-closure+   24)  ; x24: closure environment pointer
#+sbcl (defconstant +reg-code-base+ 26)  ; x26: code base register
#+sbcl (defconstant +reg-gc+        27)  ; x27: GC globals base
#+sbcl (defconstant +reg-heap+      28)  ; x28: heap bump pointer

;;; ------------------------------------------------------------
;;; macOS ARM64 Syscall Numbers
;;; ------------------------------------------------------------
;;; BSD layer syscalls, invoked via SVC #x80
;;; x16 = syscall number, x0-x7 = arguments, result in x0

#+sbcl (defconstant +sys-exit+  1)
#+sbcl (defconstant +sys-read+  3)
#+sbcl (defconstant +sys-write+ 4)
#+sbcl (defconstant +sys-open+  5)
#+sbcl (defconstant +sys-close+ 6)

;;; ------------------------------------------------------------
;;; Native Habu Definitions (functions instead of constants)
;;; ------------------------------------------------------------

#-sbcl (defun +cc-eq+ () #x0)
#-sbcl (defun +cc-ne+ () #x1)
#-sbcl (defun +cc-lt+ () #xB)
#-sbcl (defun +cc-le+ () #xD)
#-sbcl (defun +cc-gt+ () #xC)
#-sbcl (defun +cc-ge+ () #xA)
#-sbcl (defun +cc-lo+ () #x3)
#-sbcl (defun +cc-hs+ () #x2)
#-sbcl (defun +cc-hi+ () #x8)
#-sbcl (defun +cc-ls+ () #x9)
#-sbcl (defun +eq+ () #x0)
#-sbcl (defun +ne+ () #x1)
#-sbcl (defun +lt+ () #xB)
#-sbcl (defun +le+ () #xD)
#-sbcl (defun +gt+ () #xC)
#-sbcl (defun +ge+ () #xA)
#-sbcl (defun +lo+ () #x3)
#-sbcl (defun +hs+ () #x2)
#-sbcl (defun +hi+ () #x8)
#-sbcl (defun +ls+ () #x9)
#-sbcl (defun +sp+ () 31)
#-sbcl (defun +xzr+ () 31)
#-sbcl (defun +lr+ () 30)
#-sbcl (defun +fp+ () 29)
#-sbcl (defun +reg-env+ () 20)
#-sbcl (defun +reg-closure+ () 24)
#-sbcl (defun +reg-code-base+ () 26)
#-sbcl (defun +reg-gc+ () 27)
#-sbcl (defun +reg-heap+ () 28)
#-sbcl (defun +sys-exit+ () 1)
#-sbcl (defun +sys-read+ () 3)
#-sbcl (defun +sys-write+ () 4)
#-sbcl (defun +sys-open+ () 5)
#-sbcl (defun +sys-close+ () 6)

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
;;; Register Keywords
;;; ============================================================
;;; Registers MUST be specified as keywords (:x0, :x1, ..., :x30).
;;; Raw numbers are rejected with an error.
;;; Special aliases: :sp, :xzr, :lr, :fp, :env, :closure, :gc, :heap

(defun reg (r)
  "Convert register specifier to number.
   ONLY accepts keyword symbols - raw numbers are an error.

   General purpose: :x0 through :x30
   Special: :sp (31), :xzr (31), :lr (30), :fp (29)
   Habu conventions: :env (20), :closure (24), :code-base (26), :gc (27), :heap (28)"
  (when (numberp r)
    (error "Raw register numbers not allowed. Use keywords like :x0, :x1, :sp, :env instead of ~D" r))
  (case r
    ;; General purpose registers
    (:x0 0) (:x1 1) (:x2 2) (:x3 3) (:x4 4) (:x5 5) (:x6 6) (:x7 7)
    (:x8 8) (:x9 9) (:x10 10) (:x11 11) (:x12 12) (:x13 13) (:x14 14) (:x15 15)
    (:x16 16) (:x17 17) (:x18 18) (:x19 19) (:x20 20) (:x21 21) (:x22 22) (:x23 23)
    (:x24 24) (:x25 25) (:x26 26) (:x27 27) (:x28 28) (:x29 29) (:x30 30)
    ;; Special registers
    (:sp 31) (:xzr 31) (:lr 30) (:fp 29)
    ;; Habu-specific register aliases
    (:env 20)        ; environment frame base
    (:closure 24)    ; closure environment pointer
    (:code-base 26)  ; code base register
    (:gc 27)         ; GC globals base
    (:heap 28)       ; heap bump pointer
    (t (error "Unknown register: ~S" r))))

(declaim (inline num-to-reg))
(defun num-to-reg (n)
  "Convert register number 0-31 to keyword.
   Used when codegen needs to pass a computed register number."
  (case n
    (0 :x0) (1 :x1) (2 :x2) (3 :x3) (4 :x4) (5 :x5) (6 :x6) (7 :x7)
    (8 :x8) (9 :x9) (10 :x10) (11 :x11) (12 :x12) (13 :x13) (14 :x14) (15 :x15)
    (16 :x16) (17 :x17) (18 :x18) (19 :x19) (20 :x20) (21 :x21) (22 :x22) (23 :x23)
    (24 :x24) (25 :x25) (26 :x26) (27 :x27) (28 :x28) (29 :x29) (30 :x30) (31 :sp)
    (t (error "Invalid register number: ~D" n))))

;;; ============================================================
;;; Data Movement
;;; ============================================================

(defun movz (rd imm &key (lsl 0))
  "MOVZ Xd, #imm{, LSL #shift}
   Move wide with zero. LSL must be 0, 16, 32, or 48."
  (encode (logior #xD2800000
                  (ash (ash lsl -4) 21)
                  (ash (logand imm #xFFFF) 5)
                  (reg rd))))

(defun movk (rd imm &key (lsl 0))
  "MOVK Xd, #imm{, LSL #shift}
   Move wide with keep. LSL must be 0, 16, 32, or 48."
  (encode (logior #xF2800000
                  (ash (ash lsl -4) 21)
                  (ash (logand imm #xFFFF) 5)
                  (reg rd))))

(defun movn (rd imm &key (lsl 0))
  "MOVN Xd, #imm{, LSL #shift}
   Move wide with NOT. Result is ~(imm << shift).
   Example: (movn :x0 15) produces -16 in x0."
  (encode (logior #x92800000
                  (ash (ash lsl -4) 21)
                  (ash (logand imm #xFFFF) 5)
                  (reg rd))))

(defun mov (rd rm)
  "MOV Xd, Xm
   Move register (alias for ORR Xd, XZR, Xm)."
  (encode (logior #xAA0003E0 (ash (reg rm) 16) (reg rd))))

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
                    (reg rd)))))

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
                    (reg rd)))))

(defun add-lo12 (rd rn lo12)
  "ADD Xd, Xn, #:lo12:label
   Add low 12 bits of address. For use with ADRP."
  (encode (logior #x91000000
                  (ash (logand lo12 #xFFF) 10)
                  (ash (reg rn) 5)
                  (reg rd))))

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
                      (ash (reg rn) 5)
                      (reg rd)))
      (encode (logior #x8B000000
                      (ash (reg rm-or-imm) 16)
                      (ash (reg rn) 5)
                      (reg rd)))))

(defun sub (rd rn rm-or-imm &key imm shift12)
  "SUB Xd, Xn, Xm  or  SUB Xd, Xn, #imm [, LSL #12]
   Subtract register or 12-bit immediate. SHIFT12 shifts immediate left by 12."
  (if imm
      (encode (logior #xD1000000
                      (if shift12 #x400000 0)  ; bit 22 = sh
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash (reg rn) 5)
                      (reg rd)))
      (encode (logior #xCB000000
                      (ash (reg rm-or-imm) 16)
                      (ash (reg rn) 5)
                      (reg rd)))))

(defun mul (rd rn rm)
  "MUL Xd, Xn, Xm
   Multiply (alias for MADD Xd, Xn, Xm, XZR)."
  (encode (logior #x9B007C00
                  (ash (reg rm) 16)
                  (ash (reg rn) 5)
                  (reg rd))))

(defun sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm
   Signed divide."
  (encode (logior #x9AC00C00
                  (ash (reg rm) 16)
                  (ash (reg rn) 5)
                  (reg rd))))

(defun neg (rd rm)
  "NEG Xd, Xm
   Negate (alias for SUB Xd, XZR, Xm)."
  (encode (logior #xCB0003E0 (ash (reg rm) 16) (reg rd))))

(defun subs (rd rn rm-or-imm &key imm)
  "SUBS Xd, Xn, Xm  or  SUBS Xd, Xn, #imm
   Subtract and set flags."
  (if imm
      (encode (logior #xF1000000
                      (ash (logand rm-or-imm #xFFF) 10)
                      (ash (reg rn) 5)
                      (reg rd)))
      (encode (logior #xEB000000
                      (ash (reg rm-or-imm) 16)
                      (ash (reg rn) 5)
                      (reg rd)))))

(defun ldr-reg (rt rn rm &key (shift 0))
  "LDR Xt, [Xn, Xm, LSL #shift]
   Load 64-bit with register offset. Shift must be 0 or 3."
  ;; LDR (register): 11111000011 Rm opt S 10 Rn Rt
  ;; S=1 means LSL #3, S=0 means LSL #0
  (let ((s-bit (if (= shift 3) 1 0)))
    (encode (logior #xF8606800
                    (ash (reg rm) 16)
                    (ash s-bit 12)
                    (ash (reg rn) 5)
                    (reg rt)))))

(defun ldrb-post (rt rn imm9)
  "LDRB Wt, [Xn], #imm
   Load byte with post-increment. Imm is signed 9-bit."
  (encode (logior #x38400400
                  (ash (logand imm9 #x1FF) 12)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun strb-post (rt rn imm9)
  "STRB Wt, [Xn], #imm
   Store byte with post-increment. Imm is signed 9-bit."
  (encode (logior #x38000400
                  (ash (logand imm9 #x1FF) 12)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun orr-imm (rd rn imm)
  "ORR Xd, Xn, #imm
   Bitwise OR with immediate. Supports common small constants."
  ;; ARM64 logical immediate encoding is complex.
  ;; For tagging: we only need small constants like 1, 2, 3, 4, 5, 6, 7
  ;; These are: N=1, immr=0, imms varies
  (let ((base #xB2400000)
        (rn-reg (reg rn))
        (rd-reg (reg rd)))
    (cond
      ((= imm 1) (encode (logior base (ash 0 10) (ash rn-reg 5) rd-reg)))   ; imms=0
      ((= imm 3) (encode (logior base (ash 1 10) (ash rn-reg 5) rd-reg)))   ; imms=1
      ((= imm 7) (encode (logior base (ash 2 10) (ash rn-reg 5) rd-reg)))   ; imms=2
      ((= imm 15) (encode (logior base (ash 3 10) (ash rn-reg 5) rd-reg)))  ; imms=3
      ;; Unsupported immediate - return nil (compile-time error detection)
      #+sbcl (t (error "orr-imm: unsupported immediate ~D" imm))
      #-sbcl (t nil))))

;;; ============================================================
;;; Bitwise Operations
;;; ============================================================

(defun and* (rd rn rm-or-imm &key imm)
  "AND Xd, Xn, Xm  or  AND Xd, Xn, #mask
   Bitwise AND. Named and* to avoid CL conflict.
   Use :imm t for immediate mode with common masks:
   #x7 (low 3 bits), #xF (low 4 bits), #xFF (low 8 bits),
   -4 (~3), -8 (~7), -16 (~15), -32 (~31) for alignment."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        ;; Immediate mode - encode common masks
        ;; ARM64 logical immediate encoding: 0x92400000 | (immr << 16) | (imms << 10) | Rn | Rd
        (let ((base #x92400000)
              (rn-shift (ash rn-reg 5)))
          (cond
            ;; Keep low bits masks (N=1, immr=0, imms=bits-1)
            ((= rm-or-imm #x7)   ; low 3 bits: imms=2
             (encode (logior base (ash 2 10) rn-shift rd-reg)))
            ((= rm-or-imm #xF)   ; low 4 bits: imms=3
             (encode (logior base (ash 3 10) rn-shift rd-reg)))
            ((= rm-or-imm #xFF)  ; low 8 bits: imms=7
             (encode (logior base (ash 7 10) rn-shift rd-reg)))
            ;; Alignment masks (clear low bits) - immr=64-N, imms=63-N
            ((or (= rm-or-imm #xFFFFFFFFFFFFFFFC) (= rm-or-imm -4))   ; ~3
             (encode (logior base (ash 62 16) (ash 61 10) rn-shift rd-reg)))
            ((or (= rm-or-imm #xFFFFFFFFFFFFFFF8) (= rm-or-imm -8))   ; ~7
             (encode (logior base (ash 61 16) (ash 60 10) rn-shift rd-reg)))
            ((or (= rm-or-imm #xFFFFFFFFFFFFFFF0) (= rm-or-imm -16))  ; ~15
             (encode (logior base (ash 60 16) (ash 59 10) rn-shift rd-reg)))
            ((or (= rm-or-imm #xFFFFFFFFFFFFFFE0) (= rm-or-imm -32))  ; ~31
             (encode (logior base (ash 59 16) (ash 58 10) rn-shift rd-reg)))
            ;; Unsupported mask - return nil (compile-time error detection)
            #+sbcl (t (error "and* :imm - unsupported mask #x~X. Use common masks or encode manually." rm-or-imm))
            #-sbcl (t nil)))
        ;; Register mode
        (encode (logior #x8A000000
                        (ash (reg rm-or-imm) 16)
                        (ash rn-reg 5)
                        rd-reg)))))

(defun orr (rd rn rm-or-imm &key imm)
  "ORR Xd, Xn, Xm  or  ORR Xd, Xn, #imm
   Bitwise OR.
   Use :imm t for immediate mode with small constants (1, 3, 7, 15)."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        ;; Immediate mode - encode small constants for tagging
        (let ((base #xB2400000))
          (cond
            ((= rm-or-imm 1) (encode (logior base (ash 0 10) (ash rn-reg 5) rd-reg)))   ; imms=0
            ((= rm-or-imm 3) (encode (logior base (ash 1 10) (ash rn-reg 5) rd-reg)))   ; imms=1
            ((= rm-or-imm 7) (encode (logior base (ash 2 10) (ash rn-reg 5) rd-reg)))   ; imms=2
            ((= rm-or-imm 15) (encode (logior base (ash 3 10) (ash rn-reg 5) rd-reg)))  ; imms=3
            ;; Unsupported immediate
            #+sbcl (t (error "orr :imm - unsupported immediate ~D. Use 1, 3, 7, or 15." rm-or-imm))
            #-sbcl (t nil)))
        ;; Register mode
        (encode (logior #xAA000000
                        (ash (reg rm-or-imm) 16)
                        (ash rn-reg 5)
                        rd-reg)))))

(defun eor (rd rn rm)
  "EOR Xd, Xn, Xm
   Bitwise XOR."
  (encode (logior #xCA000000
                  (ash (reg rm) 16)
                  (ash (reg rn) 5)
                  (reg rd))))

(defun bic (rd rn rm)
  "BIC Xd, Xn, Xm
   Bit clear (AND with NOT Xm)."
  (encode (logior #x8A200000
                  (ash (reg rm) 16)
                  (ash (reg rn) 5)
                  (reg rd))))

(defun mvn (rd rm)
  "MVN Xd, Xm
   Move NOT - bitwise complement.
   Alias for ORN Xd, XZR, Xm."
  (encode (logior #xAA2003E0        ; ORN with Rn=XZR (0x1F << 5)
                  (ash (reg rm) 16)
                  (reg rd))))

(defun lsl (rd rn shift &key imm)
  "LSL Xd, Xn, Xm  or  LSL Xd, Xn, #shift
   Logical shift left."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        ;; LSL immediate: UBFM Xd, Xn, #(-shift MOD 64), #(63-shift)
        (encode (logior #xD3400000
                        (ash (logand (- #x40 shift) #x3F) 16)
                        (ash (- #x3F shift) 10)
                        (ash rn-reg 5)
                        rd-reg))
        ;; LSL register: LSLV
        (encode (logior #x9AC02000
                        (ash (reg shift) 16)
                        (ash rn-reg 5)
                        rd-reg)))))

(defun lsr (rd rn shift &key imm)
  "LSR Xd, Xn, Xm  or  LSR Xd, Xn, #shift
   Logical shift right."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        ;; LSR immediate: UBFM Xd, Xn, #shift, #63
        (encode (logior #xD340FC00
                        (ash shift 16)
                        (ash rn-reg 5)
                        rd-reg))
        ;; LSR register: LSRV
        (encode (logior #x9AC02400
                        (ash (reg shift) 16)
                        (ash rn-reg 5)
                        rd-reg)))))

(defun asr (rd rn shift &key imm)
  "ASR Xd, Xn, Xm  or  ASR Xd, Xn, #shift
   Arithmetic shift right."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        ;; ASR immediate: SBFM Xd, Xn, #shift, #63
        (encode (logior #x9340FC00
                        (ash shift 16)
                        (ash rn-reg 5)
                        rd-reg))
        ;; ASR register: ASRV
        (encode (logior #x9AC02800
                        (ash (reg shift) 16)
                        (ash rn-reg 5)
                        rd-reg)))))

;;; ============================================================
;;; Memory Operations
;;; ============================================================

(defun ldr (rt rn &key (offset 0))
  "LDR Xt, [Xn{, #offset}]
   Load 64-bit register. Offset scaled by 8, must be multiple of 8."
  (encode (logior #xF9400000
                  (ash (ash offset -3) 10)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun str (rt rn &key (offset 0))
  "STR Xt, [Xn{, #offset}]
   Store 64-bit register. Offset scaled by 8, must be multiple of 8.
   For negative offsets, use STUR instead."
  (encode (logior #xF9000000
                  (ash (ash offset -3) 10)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun stur (rt rn &key (offset 0))
  "STUR Xt, [Xn{, #offset}]
   Store 64-bit register with unscaled offset.
   Offset is signed 9-bit (-256 to 255), not scaled."
  (encode (logior #xF8000000
                  (ash (logand offset #x1FF) 12)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun ldur (rt rn &key (offset 0))
  "LDUR Xt, [Xn{, #offset}]
   Load 64-bit register with unscaled offset.
   Offset is signed 9-bit (-256 to 255), not scaled."
  (encode (logior #xF8400000
                  (ash (logand offset #x1FF) 12)
                  (ash (reg rn) 5)
                  (reg rt))))

(defun ldp (rt1 rt2 rn &key (offset 0))
  "LDP Xt1, Xt2, [Xn{, #offset}]
   Load pair. Offset scaled by 8, signed 7-bit."
  (encode (logior #xA9400000
                  (ash (logand (ash offset -3) #x7F) 15)
                  (ash (reg rt2) 10)
                  (ash (reg rn) 5)
                  (reg rt1))))

(defun stp (rt1 rt2 rn &key (offset 0))
  "STP Xt1, Xt2, [Xn{, #offset}]
   Store pair. Offset scaled by 8, signed 7-bit."
  (encode (logior #xA9000000
                  (ash (logand (ash offset -3) #x7F) 15)
                  (ash (reg rt2) 10)
                  (ash (reg rn) 5)
                  (reg rt1))))

(defun ldrb (rt rn rm-or-offset &key reg)
  "LDRB Wt, [Xn, Xm]  or  LDRB Wt, [Xn, #offset]
   Load byte, zero-extend to 64-bit.
   Use :reg t for register offset, otherwise immediate offset."
  (let ((rt-reg (reg rt))
        (rn-reg (reg rn)))
    (if reg
        ;; Register offset: LDRB Wt, [Xn, Xm]
        (encode (logior #x38606800
                        (ash (reg rm-or-offset) 16)
                        (ash rn-reg 5)
                        rt-reg))
        ;; Immediate offset: LDRB Wt, [Xn, #offset]
        (encode (logior #x39400000
                        (ash (logand rm-or-offset #xFFF) 10)
                        (ash rn-reg 5)
                        rt-reg)))))

(defun strb (rt rn rm-or-offset &key reg)
  "STRB Wt, [Xn, Xm]  or  STRB Wt, [Xn, #offset]
   Store byte.
   Use :reg t for register offset, otherwise immediate offset."
  (let ((rt-reg (reg rt))
        (rn-reg (reg rn)))
    (if reg
        ;; Register offset: STRB Wt, [Xn, Xm]
        (encode (logior #x38206800
                        (ash (reg rm-or-offset) 16)
                        (ash rn-reg 5)
                        rt-reg))
        ;; Immediate offset: STRB Wt, [Xn, #offset]
        (encode (logior #x39000000
                        (ash (logand rm-or-offset #xFFF) 10)
                        (ash rn-reg 5)
                        rt-reg)))))

;;; ============================================================
;;; Compare
;;; ============================================================

(defun cmp (rn rm-or-imm &key imm)
  "CMP Xn, Xm  or  CMP Xn, #imm
   Compare (alias for SUBS XZR, Xn, ...)."
  (let ((rn-reg (reg rn)))
    (if imm
        (encode (logior #xF100001F
                        (ash (logand rm-or-imm #xFFF) 10)
                        (ash rn-reg 5)))
        (encode (logior #xEB00001F
                        (ash (reg rm-or-imm) 16)
                        (ash rn-reg 5))))))

(defun cset (rd cond)
  "CSET Xd, cond
   Conditional set. cond is one of +cc-eq+, +cc-ne+, etc."
  (encode (logior #x9A9F07E0
                  (ash (logxor cond 1) 12)
                  (reg rd))))

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
  (encode (logior #xD63F0000 (ash (reg rn) 5))))

(defun br (rn)
  "BR Xn
   Branch to register (no link)."
  (encode (logior #xD61F0000 (ash (reg rn) 5))))

(defun cbz (rt offset)
  "CBZ Xt, label
   Compare and branch if zero. Offset in instructions (not bytes)."
  (encode (logior #xB4000000
                  (ash (logand offset #x7FFFF) 5)
                  (reg rt))))

(defun cbnz (rt offset)
  "CBNZ Xt, label
   Compare and branch if not zero. Offset in instructions (not bytes)."
  (encode (logior #xB5000000
                  (ash (logand offset #x7FFFF) 5)
                  (reg rt))))

(defun b.eq (offset)
  "B.EQ label
   Branch if equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x0)))  ; +eq+ = 0

(defun b.ne (offset)
  "B.NE label
   Branch if not equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x1)))  ; +ne+ = 1

(defun b.lt (offset)
  "B.LT label
   Branch if less than. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #xB)))  ; +lt+ = 11

(defun b.le (offset)
  "B.LE label
   Branch if less or equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #xD)))  ; +le+ = 13

(defun b.gt (offset)
  "B.GT label
   Branch if greater than. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #xC)))  ; +gt+ = 12

(defun b.ge (offset)
  "B.GE label
   Branch if greater or equal. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #xA)))  ; +ge+ = 10

(defun b.lo (offset)
  "B.LO label
   Branch if unsigned lower (carry clear). Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x3)))  ; +lo+ = 3

(defun b.hs (offset)
  "B.HS label
   Branch if unsigned higher or same (carry set). Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x2)))  ; +hs+ = 2

(defun b.hi (offset)
  "B.HI label
   Branch if unsigned higher. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x8)))  ; +hi+ = 8

(defun b.ls (offset)
  "B.LS label
   Branch if unsigned lower or same. Offset in instructions."
  (encode (logior #x54000000
                  (ash (logand offset #x7FFFF) 5)
                  #x9)))  ; +ls+ = 9

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

(defun brk (imm16)
  "BRK #imm16
   Breakpoint instruction. Causes SIGTRAP signal.
   Use for undefined function traps, assertions, etc."
  ;; BRK encoding: 1101 0100 001 imm16[15:0] 00000
  ;; = 0xD4200000 | (imm16 << 5)
  (encode (logior #xD4200000 (ash (logand imm16 #xFFFF) 5))))

(defun nop ()
  "NOP - No operation"
  (encode #xD503201F))
