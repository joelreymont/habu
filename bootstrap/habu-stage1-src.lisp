;;; Habu Stage 1 Compiler Source
;;; Auto-generated from #-sbcl sections of bootstrap files
;;; This is pure Habu code - no reader conditionals
;;; Compile with SBCL bootstrap to create Stage 1 binary

(defpackage :arm64
  (:use :cl)
  (:export #:encode
           #:reg
           #:num-to-reg
           #:movz
           #:movk
           #:movn
           #:mov
           #:adrp
           #:adr
           #:add-lo12
           #:add
           #:sub
           #:subs
           #:mul
           #:sdiv
           #:neg
           #:and*
           #:orr
           #:eor
           #:bic
           #:lsl
           #:lsr
           #:asr
           #:ldr
           #:ldr-reg
           #:str
           #:ldp
           #:stp
           #:ldrb
           #:ldrb-post
           #:strb
           #:strb-post
           #:ldur
           #:stur
           #:orr-imm
           #:cmp
           #:cset
           #:b
           #:bl
           #:br
           #:blr
           #:cbz
           #:cbnz
           #:b.eq
           #:b.ne
           #:b.lt
           #:b.le
           #:b.gt
           #:b.ge
           #:b.lo
           #:b.hs
           #:b.hi
           #:b.ls
           #:ret
           #:svc
           #:nop
           #:+cc-eq+
           #:+cc-ne+
           #:+cc-lt+
           #:+cc-le+
           #:+cc-gt+
           #:+cc-ge+
           #:+cc-lo+
           #:+cc-hs+
           #:+cc-hi+
           #:+cc-ls+
           #:+eq+
           #:+ne+
           #:+lt+
           #:+le+
           #:+gt+
           #:+ge+
           #:+lo+
           #:+hs+
           #:+hi+
           #:+ls+
           #:+sp+
           #:+lr+
           #:+xzr+
           #:+fp+
           #:+reg-env+
           #:+reg-closure+
           #:+reg-code-base+
           #:+reg-gc+
           #:+reg-heap+
           #:+sys-exit+
           #:+sys-read+
           #:+sys-write+
           #:+sys-open+
           #:+sys-close+))

(in-package :arm64)

(defun +cc-eq+ () 0)

(defun +cc-ne+ () 1)

(defun +cc-lt+ () 11)

(defun +cc-le+ () 13)

(defun +cc-gt+ () 12)

(defun +cc-ge+ () 10)

(defun +cc-lo+ () 3)

(defun +cc-hs+ () 2)

(defun +cc-hi+ () 8)

(defun +cc-ls+ () 9)

(defun +eq+ () 0)

(defun +ne+ () 1)

(defun +lt+ () 11)

(defun +le+ () 13)

(defun +gt+ () 12)

(defun +ge+ () 10)

(defun +lo+ () 3)

(defun +hs+ () 2)

(defun +hi+ () 8)

(defun +ls+ () 9)

(defun +sp+ () 31)

(defun +xzr+ () 31)

(defun +lr+ () 30)

(defun +fp+ () 29)

(defun +reg-env+ () 20)

(defun +reg-closure+ () 24)

(defun +reg-code-base+ () 26)

(defun +reg-gc+ () 27)

(defun +reg-heap+ () 28)

(defun +sys-exit+ () 1)

(defun +sys-read+ () 3)

(defun +sys-write+ () 4)

(defun +sys-open+ () 5)

(defun +sys-close+ () 6)

(defun encode (sb-ext:word)
  "Encode 32-bit instruction as little-endian byte list."
  (list (logand sb-ext:word 255) (logand (ash sb-ext:word -8) 255)
        (logand (ash sb-ext:word -16) 255) (logand (ash sb-ext:word -24) 255)))

(defun reg (r)
  "Convert register specifier to number.
   ONLY accepts keyword symbols - raw numbers are an error.

   General purpose: :x0 through :x30
   Special: :sp (31), :xzr (31), :lr (30), :fp (29)
   Habu conventions: :env (20), :closure (24), :code-base (26), :gc (27), :heap (28)"
  (when (numberp r)
    (error "Raw register numbers not allowed. Use keywords like :x0, :x1, :sp, :env"))
  ;; Use string-equal on symbol-name for native Habu compatibility
  ;; (case uses eql which fails when keywords aren't properly interned)
  (let ((name (symbol-name r)))
    (cond
      ((string-equal name "X0") 0)
      ((string-equal name "X1") 1)
      ((string-equal name "X2") 2)
      ((string-equal name "X3") 3)
      ((string-equal name "X4") 4)
      ((string-equal name "X5") 5)
      ((string-equal name "X6") 6)
      ((string-equal name "X7") 7)
      ((string-equal name "X8") 8)
      ((string-equal name "X9") 9)
      ((string-equal name "X10") 10)
      ((string-equal name "X11") 11)
      ((string-equal name "X12") 12)
      ((string-equal name "X13") 13)
      ((string-equal name "X14") 14)
      ((string-equal name "X15") 15)
      ((string-equal name "X16") 16)
      ((string-equal name "X17") 17)
      ((string-equal name "X18") 18)
      ((string-equal name "X19") 19)
      ((string-equal name "X20") 20)
      ((string-equal name "X21") 21)
      ((string-equal name "X22") 22)
      ((string-equal name "X23") 23)
      ((string-equal name "X24") 24)
      ((string-equal name "X25") 25)
      ((string-equal name "X26") 26)
      ((string-equal name "X27") 27)
      ((string-equal name "X28") 28)
      ((string-equal name "X29") 29)
      ((string-equal name "X30") 30)
      ((string-equal name "SP") 31)
      ((string-equal name "XZR") 31)
      ((string-equal name "LR") 30)
      ((string-equal name "FP") 29)
      ((string-equal name "ENV") 20)
      ((string-equal name "CLOSURE") 24)
      ((string-equal name "CODE-BASE") 26)
      ((string-equal name "GC") 27)
      ((string-equal name "HEAP") 28)
      (t (error "Unknown register")))))

(declaim (inline num-to-reg))

(defun num-to-reg (n)
  "Convert register number 0-31 to keyword.
   Used when codegen needs to pass a computed register number."
  (case n
    (0 :x0)
    (1 :x1)
    (2 :x2)
    (3 :x3)
    (4 :x4)
    (5 :x5)
    (6 :x6)
    (7 :x7)
    (8 :x8)
    (9 :x9)
    (10 :x10)
    (11 :x11)
    (12 :x12)
    (13 :x13)
    (14 :x14)
    (15 :x15)
    (16 :x16)
    (17 :x17)
    (18 :x18)
    (19 :x19)
    (20 :x20)
    (21 :x21)
    (22 :x22)
    (23 :x23)
    (24 :x24)
    (25 :x25)
    (26 :x26)
    (27 :x27)
    (28 :x28)
    (29 :x29)
    (30 :x30)
    (31 :sp)
    (t (error "Invalid register number: ~D" n))))

(defun movz
       (rd imm &key (lsl 0))
  "MOVZ Xd, #imm{, LSL #shift}
   Move wide with zero. LSL must be 0, 16, 32, or 48."
  (encode
   (logior 3531603968 (ash (ash lsl -4) 21)
           (ash (logand imm 65535) 5)
           (reg rd))))

(defun movk
       (rd imm &key (lsl 0))
  "MOVK Xd, #imm{, LSL #shift}
   Move wide with keep. LSL must be 0, 16, 32, or 48."
  (encode
   (logior 4068474880 (ash (ash lsl -4) 21)
           (ash (logand imm 65535) 5)
           (reg rd))))

(defun movn
       (rd imm &key (lsl 0))
  "MOVN Xd, #imm{, LSL #shift}
   Move wide with NOT. Result is ~(imm << shift).
   Example: (movn :x0 15) produces -16 in x0."
  (encode
   (logior 2457862144 (ash (ash lsl -4) 21)
           (ash (logand imm 65535) 5)
           (reg rd))))

(defun mov (rd rm)
  "MOV Xd, Xm
   Move register (alias for ORR Xd, XZR, Xm)."
  (encode
   (logior 2852127712 (ash (reg rm) 16)
           (reg rd))))

(defun adrp (rd page-offset)
  "ADRP Xd, label
   Load PC-relative page address. PAGE-OFFSET is the signed page offset.
   Note: This encodes a placeholder - actual address requires relocation."
  (let* ((immlo (logand page-offset 3))
         (immhi (logand (ash page-offset -2) 524287)))
    (encode
     (logior 2415919104 (ash immlo 29) (ash immhi 5)
             (reg rd)))))

(defun adr (rd byte-offset)
  "ADR Xd, label
   Load PC-relative address. BYTE-OFFSET is signed, +/-1MB range.
   Computes Xd = PC + byte-offset."
  (let* ((immlo (logand byte-offset 3))
         (immhi (logand (ash byte-offset -2) 524287)))
    (encode
     (logior 268435456 (ash immlo 29) (ash immhi 5)
             (reg rd)))))

(defun add-lo12
       (rd rn lo12)
  "ADD Xd, Xn, #:lo12:label
   Add low 12 bits of address. For use with ADRP."
  (encode
   (logior 2432696320 (ash (logand lo12 4095) 10)
           (ash (reg rn) 5)
           (reg rd))))

(defun add
       (rd rn rm-or-imm
        &key imm shift12)
  "ADD Xd, Xn, Xm  or  ADD Xd, Xn, #imm [, LSL #12]
   Add register or 12-bit immediate. SHIFT12 shifts immediate left by 12."
  (if imm
      (encode
       (logior 2432696320
               (if shift12
                   4194304
                   0)
               (ash (logand rm-or-imm 4095) 10)
               (ash (reg rn) 5)
               (reg rd)))
      (encode
       (logior 2332033024 (ash (reg rm-or-imm) 16)
               (ash (reg rn) 5)
               (reg rd)))))

(defun sub
       (rd rn rm-or-imm
        &key imm shift12)
  "SUB Xd, Xn, Xm  or  SUB Xd, Xn, #imm [, LSL #12]
   Subtract register or 12-bit immediate. SHIFT12 shifts immediate left by 12."
  (if imm
      (encode
       (logior 3506438144
               (if shift12
                   4194304
                   0)
               (ash (logand rm-or-imm 4095) 10)
               (ash (reg rn) 5)
               (reg rd)))
      (encode
       (logior 3405774848 (ash (reg rm-or-imm) 16)
               (ash (reg rn) 5)
               (reg rd)))))

(defun mul (rd rn rm)
  "MUL Xd, Xn, Xm
   Multiply (alias for MADD Xd, Xn, Xm, XZR)."
  (encode
   (logior 2600500224 (ash (reg rm) 16)
           (ash (reg rn) 5)
           (reg rd))))

(defun sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm
   Signed divide."
  (encode
   (logior 2596277248 (ash (reg rm) 16)
           (ash (reg rn) 5)
           (reg rd))))

(defun neg (rd rm)
  "NEG Xd, Xm
   Negate (alias for SUB Xd, XZR, Xm)."
  (encode
   (logior 3405775840 (ash (reg rm) 16)
           (reg rd))))

(defun subs
       (rd rn rm-or-imm
        &key imm)
  "SUBS Xd, Xn, Xm  or  SUBS Xd, Xn, #imm
   Subtract and set flags."
  (if imm
      (encode
       (logior 4043309056 (ash (logand rm-or-imm 4095) 10)
               (ash (reg rn) 5)
               (reg rd)))
      (encode
       (logior 3942645760 (ash (reg rm-or-imm) 16)
               (ash (reg rn) 5)
               (reg rd)))))

(defun ldr-reg
       (rt rn rm
        &key (shift 0))
  "LDR Xt, [Xn, Xm, LSL #shift]
   Load 64-bit with register offset. Shift must be 0 or 3."
  (let ((s-bit
         (if (= shift 3)
             1
             0)))
    (encode
     (logior 4167067648 (ash (reg rm) 16)
             (ash s-bit 12) (ash (reg rn) 5)
             (reg rt)))))

(defun ldrb-post
       (rt rn imm9)
  "LDRB Wt, [Xn], #imm
   Load byte with post-increment. Imm is signed 9-bit."
  (encode
   (logior 943719424 (ash (logand imm9 511) 12)
           (ash (reg rn) 5)
           (reg rt))))

(defun strb-post
       (rt rn imm9)
  "STRB Wt, [Xn], #imm
   Store byte with post-increment. Imm is signed 9-bit."
  (encode
   (logior 939525120 (ash (logand imm9 511) 12)
           (ash (reg rn) 5)
           (reg rt))))

(defun orr-imm (rd rn imm)
  "ORR Xd, Xn, #imm
   Bitwise OR with immediate. Supports common small constants."
  (let ((base 2990538752)
        (rn-reg (reg rn))
        (rd-reg (reg rd)))
    (cond
     ((= imm 1)
      (encode
       (logior base (ash 0 10) (ash rn-reg 5)
               rd-reg)))
     ((= imm 3)
      (encode
       (logior base (ash 1 10) (ash rn-reg 5)
               rd-reg)))
     ((= imm 7)
      (encode
       (logior base (ash 2 10) (ash rn-reg 5)
               rd-reg)))
     ((= imm 15)
      (encode
       (logior base (ash 3 10) (ash rn-reg 5)
               rd-reg)))
     (t nil))))

(defun and*
       (rd rn rm-or-imm
        &key imm)
  "AND Xd, Xn, Xm  or  AND Xd, Xn, #mask
   Bitwise AND. Named and* to avoid CL conflict.
   Use :imm t for immediate mode with common masks:
   #x7 (low 3 bits), #xF (low 4 bits), #xFF (low 8 bits),
   -4 (~3), -8 (~7), -16 (~15), -32 (~31) for alignment."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        (let ((base 2453667840)
              (rn-shift (ash rn-reg 5)))
          (cond
           ((= rm-or-imm 7)
            (encode
             (logior base (ash 2 10) rn-shift
                     rd-reg)))
           ((= rm-or-imm 15)
            (encode
             (logior base (ash 3 10) rn-shift
                     rd-reg)))
           ((= rm-or-imm 255)
            (encode
             (logior base (ash 7 10) rn-shift
                     rd-reg)))
           ((or (= rm-or-imm 18446744073709551612)
                (= rm-or-imm -4))
            (encode
             (logior base (ash 62 16) (ash 61 10) rn-shift
                     rd-reg)))
           ((or (= rm-or-imm 18446744073709551608)
                (= rm-or-imm -8))
            (encode
             (logior base (ash 61 16) (ash 60 10) rn-shift
                     rd-reg)))
           ((or (= rm-or-imm 18446744073709551600)
                (= rm-or-imm -16))
            (encode
             (logior base (ash 60 16) (ash 59 10) rn-shift
                     rd-reg)))
           ((or (= rm-or-imm 18446744073709551584)
                (= rm-or-imm -32))
            (encode
             (logior base (ash 59 16) (ash 58 10) rn-shift
                     rd-reg)))
           (t nil)))
        (encode
         (logior 2315255808 (ash (reg rm-or-imm) 16)
                 (ash rn-reg 5) rd-reg)))))

(defun orr
       (rd rn rm-or-imm
        &key imm)
  "ORR Xd, Xn, Xm  or  ORR Xd, Xn, #imm
   Bitwise OR.
   Use :imm t for immediate mode with small constants (1, 3, 7, 15)."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        (let ((base 2990538752))
          (cond
           ((= rm-or-imm 1)
            (encode
             (logior base (ash 0 10) (ash rn-reg 5)
                     rd-reg)))
           ((= rm-or-imm 3)
            (encode
             (logior base (ash 1 10) (ash rn-reg 5)
                     rd-reg)))
           ((= rm-or-imm 7)
            (encode
             (logior base (ash 2 10) (ash rn-reg 5)
                     rd-reg)))
           ((= rm-or-imm 15)
            (encode
             (logior base (ash 3 10) (ash rn-reg 5)
                     rd-reg)))
           (t nil)))
        (encode
         (logior 2852126720 (ash (reg rm-or-imm) 16)
                 (ash rn-reg 5) rd-reg)))))

(defun eor (rd rn rm)
  "EOR Xd, Xn, Xm
   Bitwise XOR."
  (encode
   (logior 3388997632 (ash (reg rm) 16)
           (ash (reg rn) 5)
           (reg rd))))

(defun bic (rd rn rm)
  "BIC Xd, Xn, Xm
   Bit clear (AND with NOT Xm)."
  (encode
   (logior 2317352960 (ash (reg rm) 16)
           (ash (reg rn) 5)
           (reg rd))))

(defun lsl
       (rd rn shift
        &key imm)
  "LSL Xd, Xn, Xm  or  LSL Xd, Xn, #shift
   Logical shift left."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        (encode
         (logior 3544186880 (ash (logand (- 64 shift) 63) 16)
                 (ash (- 63 shift) 10) (ash rn-reg 5)
                 rd-reg))
        (encode
         (logior 2596282368 (ash (reg shift) 16)
                 (ash rn-reg 5) rd-reg)))))

(defun lsr
       (rd rn shift
        &key imm)
  "LSR Xd, Xn, Xm  or  LSR Xd, Xn, #shift
   Logical shift right."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        (encode
         (logior 3544251392 (ash shift 16) (ash rn-reg 5)
                 rd-reg))
        (encode
         (logior 2596283392 (ash (reg shift) 16)
                 (ash rn-reg 5) rd-reg)))))

(defun asr
       (rd rn shift
        &key imm)
  "ASR Xd, Xn, Xm  or  ASR Xd, Xn, #shift
   Arithmetic shift right."
  (let ((rd-reg (reg rd))
        (rn-reg (reg rn)))
    (if imm
        (encode
         (logior 2470509568 (ash shift 16) (ash rn-reg 5)
                 rd-reg))
        (encode
         (logior 2596284416 (ash (reg shift) 16)
                 (ash rn-reg 5) rd-reg)))))

(defun ldr
       (rt rn &key (offset 0))
  "LDR Xt, [Xn{, #offset}]
   Load 64-bit register. Offset scaled by 8, must be multiple of 8."
  (encode
   (logior 4181721088 (ash (ash offset -3) 10)
           (ash (reg rn) 5)
           (reg rt))))

(defun str
       (rt rn &key (offset 0))
  "STR Xt, [Xn{, #offset}]
   Store 64-bit register. Offset scaled by 8, must be multiple of 8.
   For negative offsets, use STUR instead."
  (encode
   (logior 4177526784 (ash (ash offset -3) 10)
           (ash (reg rn) 5)
           (reg rt))))

(defun stur
       (rt rn &key (offset 0))
  "STUR Xt, [Xn{, #offset}]
   Store 64-bit register with unscaled offset.
   Offset is signed 9-bit (-256 to 255), not scaled."
  (encode
   (logior 4160749568 (ash (logand offset 511) 12)
           (ash (reg rn) 5)
           (reg rt))))

(defun ldur
       (rt rn &key (offset 0))
  "LDUR Xt, [Xn{, #offset}]
   Load 64-bit register with unscaled offset.
   Offset is signed 9-bit (-256 to 255), not scaled."
  (encode
   (logior 4164943872 (ash (logand offset 511) 12)
           (ash (reg rn) 5)
           (reg rt))))

(defun ldp
       (rt1 rt2 rn
        &key (offset 0))
  "LDP Xt1, Xt2, [Xn{, #offset}]
   Load pair. Offset scaled by 8, signed 7-bit."
  (encode
   (logior 2839543808 (ash (logand (ash offset -3) 127) 15)
           (ash (reg rt2) 10)
           (ash (reg rn) 5)
           (reg rt1))))

(defun stp
       (rt1 rt2 rn
        &key (offset 0))
  "STP Xt1, Xt2, [Xn{, #offset}]
   Store pair. Offset scaled by 8, signed 7-bit."
  (encode
   (logior 2835349504 (ash (logand (ash offset -3) 127) 15)
           (ash (reg rt2) 10)
           (ash (reg rn) 5)
           (reg rt1))))

(defun ldrb
       (rt rn rm-or-offset
        &key reg)
  "LDRB Wt, [Xn, Xm]  or  LDRB Wt, [Xn, #offset]
   Load byte, zero-extend to 64-bit.
   Use :reg t for register offset, otherwise immediate offset."
  (let ((rt-reg (reg rt))
        (rn-reg (reg rn)))
    (if reg
        (encode
         (logior 945842176 (ash (reg rm-or-offset) 16)
                 (ash rn-reg 5) rt-reg))
        (encode
         (logior 960495616 (ash (logand rm-or-offset 4095) 10)
                 (ash rn-reg 5) rt-reg)))))

(defun strb
       (rt rn rm-or-offset
        &key reg)
  "STRB Wt, [Xn, Xm]  or  STRB Wt, [Xn, #offset]
   Store byte.
   Use :reg t for register offset, otherwise immediate offset."
  (let ((rt-reg (reg rt))
        (rn-reg (reg rn)))
    (if reg
        (encode
         (logior 941647872 (ash (reg rm-or-offset) 16)
                 (ash rn-reg 5) rt-reg))
        (encode
         (logior 956301312 (ash (logand rm-or-offset 4095) 10)
                 (ash rn-reg 5) rt-reg)))))

(defun cmp
       (rn rm-or-imm &key imm)
  "CMP Xn, Xm  or  CMP Xn, #imm
   Compare (alias for SUBS XZR, Xn, ...)."
  (let ((rn-reg (reg rn)))
    (if imm
        (encode
         (logior 4043309087 (ash (logand rm-or-imm 4095) 10)
                 (ash rn-reg 5)))
        (encode
         (logior 3942645791 (ash (reg rm-or-imm) 16)
                 (ash rn-reg 5))))))

(defun cset (rd cond)
  "CSET Xd, cond
   Conditional set. cond is one of +cc-eq+, +cc-ne+, etc."
  (encode
   (logior 2594113504 (ash (logxor cond 1) 12) (reg rd))))

(defun b (offset)
  "B label
   Unconditional branch. Offset in instructions (not bytes)."
  (encode (logior 335544320 (logand offset 67108863))))

(defun bl (offset)
  "BL label
   Branch with link. Offset in instructions (not bytes)."
  (encode (logior 2483027968 (logand offset 67108863))))

(defun blr (rn)
  "BLR Xn
   Branch with link to register."
  (encode
   (logior 3594452992 (ash (reg rn) 5))))

(defun br (rn)
  "BR Xn
   Branch to register (no link)."
  (encode
   (logior 3592355840 (ash (reg rn) 5))))

(defun cbz (rt offset)
  "CBZ Xt, label
   Compare and branch if zero. Offset in instructions (not bytes)."
  (encode
   (logior 3019898880 (ash (logand offset 524287) 5)
           (reg rt))))

(defun cbnz (rt offset)
  "CBNZ Xt, label
   Compare and branch if not zero. Offset in instructions (not bytes)."
  (encode
   (logior 3036676096 (ash (logand offset 524287) 5)
           (reg rt))))

(defun b.eq (offset)
  "B.EQ label
   Branch if equal. Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 0)))

(defun b.ne (offset)
  "B.NE label
   Branch if not equal. Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 1)))

(defun b.lt (offset)
  "B.LT label
   Branch if less than. Offset in instructions."
  (encode
   (logior 1409286144 (ash (logand offset 524287) 5) 11)))

(defun b.le (offset)
  "B.LE label
   Branch if less or equal. Offset in instructions."
  (encode
   (logior 1409286144 (ash (logand offset 524287) 5) 13)))

(defun b.gt (offset)
  "B.GT label
   Branch if greater than. Offset in instructions."
  (encode
   (logior 1409286144 (ash (logand offset 524287) 5) 12)))

(defun b.ge (offset)
  "B.GE label
   Branch if greater or equal. Offset in instructions."
  (encode
   (logior 1409286144 (ash (logand offset 524287) 5) 10)))

(defun b.lo (offset)
  "B.LO label
   Branch if unsigned lower (carry clear). Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 3)))

(defun b.hs (offset)
  "B.HS label
   Branch if unsigned higher or same (carry set). Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 2)))

(defun b.hi (offset)
  "B.HI label
   Branch if unsigned higher. Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 8)))

(defun b.ls (offset)
  "B.LS label
   Branch if unsigned lower or same. Offset in instructions."
  (encode (logior 1409286144 (ash (logand offset 524287) 5) 9)))

(defun ret ()
  "RET
   Return from subroutine (branch to LR)."
  (encode 3596551104))

(defun svc (imm16)
  "SVC #imm16
   Supervisor call (syscall). On macOS, use #x80 for syscalls."
  (encode (logior 3556769793 (ash (logand imm16 65535) 5))))

(defun nop () "NOP - No operation" (encode 3573751839))

(in-package :habu)

(defconstant +gc-intern-table-offset+ 0)

(defconstant +gc-lambda-counter-offset+ 8)

(defconstant +gc-from-end-offset+ 16)

(defconstant +gc-half-heap-offset+ 24)

(defconstant +gc-space-flag-offset+ 32)

(defconstant +gc-state-offset+ 40)

(defconstant +gc-symbol-counter-offset+ 48)

(defconstant +gc-symbol-table-offset+ 56)

(defconstant +gc-argc-offset+ 64)

(defconstant +gc-argv-offset+ 72)

(defconstant +gc-packages-offset+ 80)

(defconstant +gc-current-package-offset+ 88)

(defconstant +gc-stack-base-offset+ 96)

(defconstant +gc-heap-data-offset+ 112)

(defconstant +gc-tag-mask+ 15)

(defconstant +gc-tag-forward+ 7)

(defun gc-trigger-check ()
  "Generate inline GC trigger check. Call after bumping x28.
   Uses x9 as scratch. Calls gc_collect if x28 >= from_end."
  (append (arm64:ldr :x9 :gc :offset +gc-from-end-offset+) (arm64:cmp :heap :x9)
          (arm64:b.lo 2) (list '(:call-fn gc-collect))))

(defun gc-object-size-asm ()
  "Generate code to calculate object size from tagged pointer in x0.
   Result in x1 (size in bytes). Uses x2 as scratch.
   Assumes tag is NOT 0 (fixnum), 6 (nil), or 7 (forward)."
  (append (arm64:and* :x2 :x0 +gc-tag-mask+ :imm t) (arm64:cmp :x2 1 :imm t)
          (arm64:b.ne 3) (arm64:movz :x1 16) (arm64:b 24) (arm64:cmp :x2 2 :imm t) (arm64:b.ne 3)
          (arm64:movz :x1 8) (arm64:b 20) (arm64:cmp :x2 5 :imm t) (arm64:b.ne 3)
          (arm64:movz :x1 16) (arm64:b 16) (arm64:cmp :x2 3 :imm t) (arm64:b.ne 7)
          (arm64:and* :x2 :x0 -16 :imm t) (arm64:ldr :x1 :x2 :offset 0)
          (arm64:lsl :x1 :x1 3 :imm t) (arm64:add :x1 :x1 8 :imm t) (arm64:b 7)
          (arm64:and* :x2 :x0 -16 :imm t) (arm64:ldr :x1 :x2 :offset 0)
          (arm64:add :x1 :x1 23 :imm t) (arm64:and* :x1 :x1 -16 :imm t)))

(defun gc-copy-asm ()
  "Generate gc_copy function.
   Input: x0 = tagged pointer
   Output: x0 = new tagged pointer (or unchanged if not from-space)
   Uses: x1-x5 as scratch
   Assumes: x17 = to_free, x18 = from_start, x19 = from_end"
  (append (list '(:fn-label gc-copy))
          (arm64:and* :x1 :x0 +gc-tag-mask+ :imm t) (arm64:cbz :x1 14)
          (arm64:cmp :x1 6 :imm t) (arm64:b.eq 12) (arm64:and* :x2 :x0 -16 :imm t)
          (arm64:cmp :x2 :x22) (arm64:b.lo 9) (arm64:cmp :x2 :x19) (arm64:b.hs 7)
          (arm64:ldr :x3 :x2 :offset 0) (arm64:and* :x4 :x3 +gc-tag-mask+ :imm t)
          (arm64:cmp :x4 +gc-tag-forward+ :imm t) (arm64:b.ne 4)
          (arm64:and* :x0 :x3 -16 :imm t) (arm64:orr :x0 :x0 :x1) (arm64:ret) (arm64:mov :x5 :x1)
          (arm64:mov :x4 :x2) (arm64:cmp :x5 1 :imm t) (arm64:b.ne 3) (arm64:movz :x1 16)
          (arm64:b 20) (arm64:cmp :x5 2 :imm t) (arm64:b.ne 3) (arm64:movz :x1 8) (arm64:b 16)
          (arm64:cmp :x5 5 :imm t) (arm64:b.ne 3) (arm64:movz :x1 16) (arm64:b 12)
          (arm64:cmp :x5 3 :imm t) (arm64:b.ne 6) (arm64:ldr :x1 :x4 :offset 0)
          (arm64:lsl :x1 :x1 3 :imm t) (arm64:add :x1 :x1 8 :imm t) (arm64:b 5)
          (arm64:ldr :x1 :x4 :offset 0) (arm64:add :x1 :x1 23 :imm t)
          (arm64:and* :x1 :x1 -16 :imm t) (arm64:mov :x2 :x17) (arm64:mov :x3 :x1)
          (arm64:cbz :x3 7) (arm64:ldr :x0 :x4 :offset 0) (arm64:str :x0 :x17 :offset 0)
          (arm64:add :x4 :x4 8 :imm t) (arm64:add :x17 :x17 8 :imm t) (arm64:sub :x3 :x3 8 :imm t)
          (arm64:b -6) (arm64:sub :x4 :x4 :x1) (arm64:movz :x6 +gc-tag-forward+)
          (arm64:orr :x0 :x2 :x6) (arm64:str :x0 :x4 :offset 0) (arm64:orr :x0 :x2 :x5)
          (arm64:ret)))

(defun gc-collect-asm ()
  "Generate gc_collect function.
   Called when x28 >= from_end.
   Saves roots, copies live objects, flips spaces, updates x28."
  (append (list '(:fn-label gc-collect)) (arm64:sub :sp :sp 208 :imm t)
          (arm64:stp :lr :fp :sp :offset 0) (arm64:stp :x0 :x1 :sp :offset 16)
          (arm64:stp :x2 :x3 :sp :offset 32) (arm64:stp :x4 :x5 :sp :offset 48)
          (arm64:stp :x6 :x7 :sp :offset 64) (arm64:stp :x8 :x9 :sp :offset 80)
          (arm64:stp :x10 :x11 :sp :offset 96) (arm64:stp :x12 :x13 :sp :offset 112)
          (arm64:stp :x14 :x15 :sp :offset 128) (arm64:stp :env :x21 :sp :offset 144)
          (arm64:stp :x22 :x23 :sp :offset 160) (arm64:stp :closure :x25 :sp :offset 176)
          (arm64:str :code-base :sp :offset 192)
          (arm64:ldr :x22 :gc :offset +gc-space-flag-offset+)
          (arm64:add :x22 :x22 :gc)
          (arm64:add :x22 :x22 +gc-heap-data-offset+ :imm t)
          (arm64:ldr :x9 :gc :offset +gc-half-heap-offset+)
          (arm64:add :x19 :x22 :x9)
          (arm64:ldr :x10 :gc :offset +gc-space-flag-offset+) (arm64:cbnz :x10 4)
          (arm64:add :x17 :gc +gc-heap-data-offset+ :imm t)
          (arm64:add :x17 :x17 :x9) (arm64:b 2)
          (arm64:add :x17 :gc +gc-heap-data-offset+ :imm t) (arm64:mov :x16 :x17)
          (arm64:ldr :x0 :gc :offset +gc-intern-table-offset+)
          (list '(:call-fn gc-copy))
          (arm64:str :x0 :gc :offset +gc-intern-table-offset+)
          (arm64:ldr :x0 :sp :offset 16) (list '(:call-fn gc-copy))
          (arm64:str :x0 :sp :offset 16) (arm64:ldr :x0 :sp :offset 24)
          (list '(:call-fn gc-copy)) (arm64:str :x0 :sp :offset 24)
          (arm64:ldr :x0 :sp :offset 32) (list '(:call-fn gc-copy))
          (arm64:str :x0 :sp :offset 32) (arm64:ldr :x0 :sp :offset 40)
          (list '(:call-fn gc-copy)) (arm64:str :x0 :sp :offset 40)
          (arm64:ldr :x0 :sp :offset 48) (list '(:call-fn gc-copy))
          (arm64:str :x0 :sp :offset 48) (arm64:ldr :x0 :sp :offset 56)
          (list '(:call-fn gc-copy)) (arm64:str :x0 :sp :offset 56)
          (arm64:ldr :x0 :sp :offset 64) (list '(:call-fn gc-copy))
          (arm64:str :x0 :sp :offset 64) (arm64:ldr :x0 :sp :offset 72)
          (list '(:call-fn gc-copy)) (arm64:str :x0 :sp :offset 72)
          (arm64:ldr :x0 :sp :offset 160) (list '(:call-fn gc-copy))
          (arm64:str :x0 :sp :offset 160) (arm64:add :env :sp 208 :imm t)
          (arm64:ldr :x21 :gc :offset +gc-stack-base-offset+)
          (list '(:label gc-stack-scan-loop)) (arm64:cmp :env :x21)
          (arm64:b.hs 25) (arm64:ldr :x0 :env :offset 0) (arm64:cmp :x0 :x22) (arm64:b.lo 19)
          (arm64:cmp :x0 :x19) (arm64:b.hs 17)
          (arm64:and* :x1 :x0 +gc-tag-mask+ :imm t) (arm64:cbz :x1 14)
          (arm64:cmp :x1 6 :imm t) (arm64:b.hs 12) (list '(:call-fn gc-copy))
          (arm64:str :x0 :env :offset 0) (arm64:add :env :env 8 :imm t) (arm64:b -20)
          (list '(:label gc-stack-scan-done))
          (list '(:label gc-scan-loop)) (arm64:cmp :x16 :x17) (arm64:b.hs 20)
          (arm64:ldr :x0 :x16 :offset 0)
          (arm64:and* :x1 :x0 +gc-tag-mask+ :imm t) (arm64:cbz :x1 6)
          (arm64:cmp :x1 6 :imm t) (arm64:b.eq 4) (arm64:cmp :x1 7 :imm t) (arm64:b.eq 2)
          (list '(:call-fn gc-copy)) (arm64:str :x0 :x16 :offset 0)
          (arm64:add :x16 :x16 8 :imm t) (arm64:b -14)
          (list '(:label gc-scan-done))
          (arm64:ldr :x9 :gc :offset +gc-half-heap-offset+)
          (arm64:ldr :x10 :gc :offset +gc-space-flag-offset+)
          (arm64:sub :x10 :x9 :x10)
          (arm64:str :x10 :gc :offset +gc-space-flag-offset+)
          (arm64:add :x11 :gc +gc-heap-data-offset+ :imm t)
          (arm64:add :x11 :x11 :x10) (arm64:add :x11 :x11 :x9)
          (arm64:str :x11 :gc :offset +gc-from-end-offset+)
          (arm64:mov :heap :x17) (arm64:ldp :lr :fp :sp :offset 0)
          (arm64:ldp :x0 :x1 :sp :offset 16) (arm64:ldp :x2 :x3 :sp :offset 32)
          (arm64:ldp :x4 :x5 :sp :offset 48) (arm64:ldp :x6 :x7 :sp :offset 64)
          (arm64:ldp :x8 :x9 :sp :offset 80) (arm64:ldp :x10 :x11 :sp :offset 96)
          (arm64:ldp :x12 :x13 :sp :offset 112) (arm64:ldp :x14 :x15 :sp :offset 128)
          (arm64:ldp :env :x21 :sp :offset 144) (arm64:ldp :x22 :x23 :sp :offset 160)
          (arm64:ldp :closure :x25 :sp :offset 176) (arm64:ldr :code-base :sp :offset 192)
          (arm64:add :sp :sp 208 :imm t) (arm64:ret)))

(defun gc-runtime-code ()
  "Generate complete GC runtime (gc_copy + gc_collect).
   Returns list of ARM64 instruction bytes with function markers."
  (append (gc-copy-asm) (gc-collect-asm)))

(defun gc-heap-init-code
       (heap-page-offset half-heap-size)
  "Generate heap initialization code for GC-enabled runtime.
   HEAP-PAGE-OFFSET: pages from ADRP to __DATA segment
   HALF-HEAP-SIZE: size of each semispace in bytes

   Initializes:
     [x27+0]:  intern_table = nil (0x06)
     [x27+8]:  lambda_counter = 0
     [x27+16]: from_end = x27 + 64 + half_heap_size
     [x27+24]: half_heap_size
     [x27+32]: space_flag = 0
     [x27+40]: gc_state = 0
     [x27+48]: symbol_counter (codegen)
     [x27+56]: symbol_table (codegen)
     [x27+64]: heap data starts, x28 points here"
  (let* ((half-high (ash half-heap-size -16))
         (half-low (logand half-heap-size 65535)))
    (append (arm64:adrp :gc heap-page-offset) (arm64:movz :x9 6)
            (arm64:str :x9 :gc :offset +gc-intern-table-offset+)
            (arm64:movz :x9 0)
            (arm64:str :x9 :gc :offset +gc-lambda-counter-offset+)
            (arm64:movz :x10 half-low)
            (if (> half-high 0)
                (arm64:movk :x10 half-high :lsl 16)
                (list (arm64:nop)))
            (arm64:str :x10 :gc :offset +gc-half-heap-offset+)
            (arm64:str :x9 :gc :offset +gc-space-flag-offset+)
            (arm64:str :x9 :gc :offset +gc-state-offset+)
            (arm64:add :x11 :gc +gc-heap-data-offset+ :imm t)
            (arm64:add :x11 :x11 :x10)
            (arm64:str :x11 :gc :offset +gc-from-end-offset+)
            (arm64:add :heap :gc +gc-heap-data-offset+ :imm t))))

(defun mmap-heap-init-code (heap-size)
  "Generate heap initialization code using mmap syscall.
   HEAP-SIZE: total heap size in bytes (must include globals + both semispaces)

   Uses mmap to allocate heap memory at runtime instead of requiring
   a pre-allocated __DATA segment in the binary.

   Initializes:
     x27 = mmap'd heap base
     [x27+0]:   intern_table = nil (0x06)
     [x27+8]:   lambda_counter = 0
     [x27+16]:  from_end = x27 + 112 + half_heap_size
     [x27+24]:  half_heap_size
     [x27+32]:  space_flag = 0
     [x27+40]:  gc_state = 0
     [x27+48]:  symbol_counter = 0
     [x27+56]:  symbol_table = nil
     [x27+64]:  argc = 0
     [x27+72]:  argv = nil
     [x27+80]:  packages = nil
     [x27+88]:  current-package = nil
     [x27+96]:  stack_base = sp
     [x27+112]: heap data starts
     x28 = x27 + 112 (allocation pointer)"
  (let* ((half-heap-size
          (ash (- heap-size +gc-heap-data-offset+) -1))
         (half-high (ash half-heap-size -16))
         (half-low (logand half-heap-size 65535))
         (size-high (ash heap-size -16))
         (size-low (logand heap-size 65535)))
    (append (arm64:movz :x0 0) (arm64:movz :x1 size-low)
            (if (> size-high 0)
                (arm64:movk :x1 size-high :lsl 16)
                (arm64:nop))
            (arm64:movz :x2 3) (arm64:movz :x3 4098) (arm64:movz :x4 65535)
            (arm64:movk :x4 65535 :lsl 16) (arm64:movk :x4 65535 :lsl 32)
            (arm64:movk :x4 65535 :lsl 48) (arm64:movz :x5 0) (arm64:movz :x16 197) (arm64:svc 128)
            (arm64:cmp :x0 0 :imm t) (arm64:b.lt 2) (arm64:b 4) (arm64:movz :x0 1)
            (arm64:movz :x16 1) (arm64:svc 128) (arm64:mov :gc :x0) (arm64:movz :x9 6)
            (arm64:str :x9 :gc :offset +gc-intern-table-offset+)
            (arm64:movz :x10 0)
            (arm64:str :x10 :gc :offset +gc-lambda-counter-offset+)
            (arm64:movz :x11 half-low)
            (if (> half-high 0)
                (arm64:movk :x11 half-high :lsl 16)
                (arm64:nop))
            (arm64:str :x11 :gc :offset +gc-half-heap-offset+)
            (arm64:str :x10 :gc :offset +gc-space-flag-offset+)
            (arm64:str :x10 :gc :offset +gc-state-offset+)
            (arm64:str :x10 :gc :offset +gc-symbol-counter-offset+)
            (arm64:str :x9 :gc :offset +gc-symbol-table-offset+)
            (arm64:str :x10 :gc :offset +gc-argc-offset+)
            (arm64:str :x9 :gc :offset +gc-argv-offset+)
            (arm64:str :x9 :gc :offset +gc-packages-offset+)
            (arm64:str :x9 :gc :offset +gc-current-package-offset+)
            (arm64:mov :x12 :sp)
            (arm64:str :x12 :gc :offset +gc-stack-base-offset+)
            (arm64:add :x12 :gc +gc-heap-data-offset+ :imm t)
            (arm64:add :x12 :x12 :x11)
            (arm64:str :x12 :gc :offset +gc-from-end-offset+)
            (arm64:add :heap :gc +gc-heap-data-offset+ :imm t))))

(defun jit-alloc-code (size-reg result-reg)
  "Generate code to allocate JIT memory via mmap with MAP_JIT.
   SIZE-REG: register containing size in bytes
   RESULT-REG: register to receive the allocated address
   Returns list of ARM64 instruction bytes.
   On error, exits with code 1."
  (append (arm64:movz :x0 0) (arm64:mov :x1 size-reg) (arm64:movz :x2 7)
          (arm64:movz :x3 6146) (arm64:movz :x4 65535) (arm64:movk :x4 65535 :lsl 16)
          (arm64:movk :x4 65535 :lsl 32) (arm64:movk :x4 65535 :lsl 48) (arm64:movz :x5 0)
          (arm64:movz :x16 197) (arm64:svc 128) (arm64:cmp :x0 0 :imm t) (arm64:b.lt 2) (arm64:b 4)
          (arm64:movz :x0 1) (arm64:movz :x16 1) (arm64:svc 128)
          (if (sym-eq result-reg :x0)
              (arm64:nop)
              (arm64:mov result-reg :x0))))

(defun jit-cache-flush-code
       (addr-reg size-reg)
  "Generate code to flush data cache and invalidate instruction cache.
   ADDR-REG: register containing start address
   SIZE-REG: register containing size in bytes
   Uses x9-x11 as scratch registers."
  (append (arm64:mov :x9 addr-reg)
          (arm64:add :x10 addr-reg size-reg)
          (list 41 123 11 213) (list 41 117 11 213) (arm64:add :x9 :x9 64 :imm t)
          (arm64:cmp :x9 :x10) (arm64:b.lt -4) (list 159 59 3 213) (list 223 63 3 213)))

(defun stage1-entry ()
  "Entry point for Stage 1 compiler. Calls the real main after all code is loaded."
  0)

(defvar *intern-table* nil)

(defvar *lambda-counter* 0)

(defvar *packages* nil)

(defvar *current-package* nil)

(defun make-special-forms ()
  "Build list of special form entries"
  (cons (cons "DEFUN" 'defun)
        (cons (cons "PROGN" 'progn)
              (cons (cons "IF" 'if)
                    (cons (cons "LET" 'let)
                          (cons (cons "LET*" 'let*)
                                (cons (cons "QUOTE" 'quote)
                                      (cons (cons "LAMBDA" 'lambda)
                                            (cons (cons "FUNCALL" 'funcall)
                                                  (cons (cons "LABELS" 'labels)
                                                        (cons (cons "FUNCTION" 'function)
                                                              (cons (cons "COND" 'cond)
                                                                    (cons (cons "WHEN" 'when)
                                                                          (cons
                                                                           (cons "UNLESS" 'unless)
                                                                           (cons (cons "AND" 'and)
                                                                                 (cons
                                                                                  (cons "OR" 'or)
                                                                                  (cons
                                                                                   (cons "NOT"
                                                                                         'not)
                                                                                   (cons
                                                                                    (cons "SETQ"
                                                                                          'setq)
                                                                                    nil))))))))))))))))))

(defun make-arithmetic ()
  "Build list of arithmetic entries"
  (cons (cons "+" '+)
        (cons (cons "-" '-)
              (cons (cons "*" '*)
                    (cons (cons "/" '/)
                          (cons (cons "MOD" 'mod)
                                (cons (cons "=" '=)
                                      (cons (cons "<" '<)
                                            (cons (cons ">" '>)
                                                  (cons (cons "<=" '<=)
                                                        (cons (cons ">=" '>=)
                                                              (cons (cons "/=" '/=)
                                                                    (cons (cons "EQ" 'eq)
                                                                          (cons
                                                                           (cons "LOGAND" 'logand)
                                                                           (cons
                                                                            (cons "LOGIOR" 'logior)
                                                                            (cons
                                                                             (cons "LOGXOR"
                                                                                   'logxor)
                                                                             (cons
                                                                              (cons "ASH" 'ash)
                                                                              nil)))))))))))))))))

(defun make-list-ops ()
  "Build list of list operation entries"
  (cons (cons "CONS" 'cons)
        (cons (cons "CAR" 'car)
              (cons (cons "CDR" 'cdr)
                    (cons (cons "CADR" 'cadr)
                          (cons (cons "CADDR" 'caddr)
                                (cons (cons "CDDR" 'cddr)
                                      (cons (cons "CDDDR" 'cdddr)
                                            (cons (cons "CADDDR" 'cadddr)
                                                  (cons (cons "NTH" 'nth)
                                                        (cons (cons "LIST" 'list)
                                                              (cons (cons "LENGTH" 'length)
                                                                    (cons (cons "REVERSE" 'reverse)
                                                                          (cons
                                                                           (cons "SETCAR"
                                                                                 'setcar)
                                                                           (cons
                                                                            (cons "SETCDR"
                                                                                  'setcdr)
                                                                            nil)))))))))))))))

(defun make-predicates ()
  "Build list of predicate entries"
  (cons (cons "NULL" 'null)
        (cons (cons "CONSP" 'consp)
              (cons (cons "NUMBERP" 'numberp)
                    (cons (cons "SYMBOLP" 'symbolp)
                          (cons (cons "STRINGP" 'stringp) (cons (cons "VECTORP" 'vectorp) nil)))))))

(defun make-string-ops ()
  "Build list of string operation entries"
  (cons (cons "STRING-LENGTH" 'string-length)
        (cons (cons "STRING-REF" 'string-ref)
              (cons (cons "STRING-CONCAT" 'string-concat)
                    (cons (cons "STRING-EQUAL" 'string-equal) nil)))))

(defun make-vector-ops ()
  "Build list of vector operation entries"
  (cons (cons "MAKE-VECTOR" 'make-vector)
        (cons (cons "VECTOR-REF" 'vector-ref)
              (cons (cons "VECTOR-SET" 'vector-set)
                    (cons (cons "VECTOR-LENGTH" 'vector-length)
                          (cons
                           (cons "MAKE-STRING-FROM-VECTOR"
                                 'make-string-from-vector)
                           (cons (cons "BUFFER-TO-STRING" 'buffer-to-string)
                                 nil)))))))

(defun make-symbol-ops ()
  "Build list of symbol operation entries"
  (cons (cons "SYMBOL-NAME" 'symbol-name)
        (cons (cons "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string) nil)))

(defun make-system-ops ()
  "Build list of system operation entries"
  (cons (cons "SYS-EXIT" 'sys-exit)
        (cons (cons "SYS-OPEN" 'sys-open)
              (cons (cons "SYS-READ" 'sys-read)
                    (cons (cons "SYS-WRITE" 'sys-write)
                          (cons (cons "SYS-WRITE-CHAR" 'sys-write-char)
                                (cons (cons "SYS-READ-BYTE" 'sys-read-byte)
                                      (cons (cons "SYS-CLOSE" 'sys-close)
                                            (cons
                                             (cons "NATIVE-READ-FILE"
                                                   'native-read-file)
                                             (cons
                                              (cons "GET-INTERN-TABLE"
                                                    'get-intern-table)
                                              (cons
                                               (cons "SET-INTERN-TABLE"
                                                     'set-intern-table)
                                               (cons
                                                (cons "GET-LAMBDA-COUNTER"
                                                      'get-lambda-counter)
                                                (cons
                                                 (cons "SET-LAMBDA-COUNTER"
                                                       'set-lambda-counter)
                                                 (cons (cons "IN-PACKAGE" 'in-package)
                                                       (cons (cons "DEFPACKAGE" 'defpackage)
                                                             (cons (cons "NIL" 'nil)
                                                                   (cons (cons "T" 't)
                                                                         nil)))))))))))))))))

(defun make-ir-basic ()
  "Build list of basic IR tag entries"
  (cons (cons "LIT" 'lit)
        (cons (cons "VAR-REF" 'var-ref)
              (cons (cons "SYM-LIT" 'sym-lit)
                    (cons (cons "STR-LIT" 'str-lit)
                          (cons (cons "NIL-IR" 'nil-ir)
                                (cons (cons "ADD" 'add)
                                      (cons (cons "SUB" 'sub)
                                            (cons (cons "MUL" 'mul)
                                                  (cons (cons "DIV" 'div)
                                                        (cons
                                                         (cons "CMP-EQ" 'cmp-eq)
                                                         (cons
                                                          (cons "CMP-LT" 'cmp-lt)
                                                          (cons
                                                           (cons "CMP-GT"
                                                                 'cmp-gt)
                                                           (cons
                                                            (cons "CMP-LE"
                                                                  'cmp-le)
                                                            (cons
                                                             (cons "CMP-GE"
                                                                   'cmp-ge)
                                                             nil)))))))))))))))

(defun make-ir-cons ()
  "Build list of cons IR tag entries"
  (cons (cons "CONS-IR" 'cons-ir)
        (cons (cons "CAR-IR" 'car-ir)
              (cons (cons "CDR-IR" 'cdr-ir)
                    (cons (cons "SETCAR-IR" 'setcar-ir)
                          (cons (cons "SETCDR-IR" 'setcdr-ir) nil))))))

(defun make-ir-control ()
  "Build list of control flow IR tag entries"
  (cons (cons "IF-IR" 'if-ir)
        (cons (cons "LET-IR" 'let-ir)
              (cons (cons "LET*-IR" 'let*-ir)
                    (cons (cons "PROGN-IR" 'progn-ir)
                          (cons (cons "OR-IR" 'or-ir)
                                (cons (cons "AND-IR" 'and-ir) nil)))))))

(defun make-ir-functions ()
  "Build list of function IR tag entries"
  (cons (cons "CALL-FN" 'call-fn)
        (cons (cons "FUNCALL-IR" 'funcall-ir)
              (cons (cons "LAMBDA-IR" 'lambda-ir)
                    (cons (cons "FN-REF-IR" 'fn-ref-ir)
                          (cons (cons "LABELS-IR" 'labels-ir) nil))))))

(defun make-ir-syscalls ()
  "Build list of syscall IR tag entries"
  (cons (cons "SYS-EXIT-IR" 'sys-exit-ir)
        (cons (cons "SYS-OPEN-IR" 'sys-open-ir)
              (cons (cons "SYS-READ-IR" 'sys-read-ir)
                    (cons (cons "SYS-WRITE-IR" 'sys-write-ir)
                          (cons (cons "SYS-WRITE-CHAR-IR" 'sys-write-char-ir)
                                (cons (cons "SYS-READ-BYTE-IR" 'sys-read-byte-ir)
                                      (cons (cons "SYS-CLOSE-IR" 'sys-close-ir)
                                            (cons (cons "SETQ-IR" 'setq-ir)
                                                  nil)))))))))

(defun make-ir-predicates ()
  "Build list of predicate IR tag entries"
  (cons (cons "GET-TAG" 'get-tag)
        (cons (cons "QUOTE-IR" 'quote-ir)
              (cons (cons "NULL-IR" 'null-ir)
                    (cons (cons "LIST-IR" 'list-ir) nil)))))

(defun make-ir-strings ()
  "Build list of string IR tag entries"
  (cons (cons "STRING-LENGTH-IR" 'string-length-ir)
        (cons (cons "STRING-REF-IR" 'string-ref-ir)
              (cons (cons "STRING-CONCAT-IR" 'string-concat-ir)
                    (cons (cons "SYMBOL-NAME-IR" 'symbol-name-ir)
                          (cons (cons "MAKE-SYMBOL-IR" 'make-symbol-ir) nil))))))

(defun make-ir-vectors ()
  "Build list of vector IR tag entries"
  (cons (cons "MAKE-VECTOR-IR" 'make-vector-ir)
        (cons (cons "VECTOR-REF-IR" 'vector-ref-ir)
              (cons (cons "VECTOR-SET-IR" 'vector-set-ir)
                    (cons (cons "VECTOR-LENGTH-IR" 'vector-length-ir)
                          (cons
                           (cons "MAKE-STRING-FROM-VECTOR-IR"
                                 'make-string-from-vector-ir)
                           (cons
                            (cons "BUFFER-TO-STRING-IR" 'buffer-to-string-ir)
                            nil)))))))

(defun make-ir-intern ()
  "Build list of intern table IR tag entries"
  (cons (cons "GET-INTERN-TABLE-IR" 'get-intern-table-ir)
        (cons (cons "SET-INTERN-TABLE-IR" 'set-intern-table-ir)
              (cons (cons "GET-LAMBDA-COUNTER-IR" 'get-lambda-counter-ir)
                    (cons (cons "SET-LAMBDA-COUNTER-IR" 'set-lambda-counter-ir)
                          (cons (cons "NATIVE-READ-FILE-IR" 'native-read-file-ir)
                                nil))))))

(defun make-ir-lambda ()
  "Build list of lambda IR tag entries"
  (cons (cons "LIFTED-LAMBDA-IR" 'lifted-lambda-ir)
        (cons (cons "LAMBDA-REF" 'lambda-ref)
              (cons (cons ":CALL" ':call) (cons (cons ":EXTERN-CALL" ':extern-call) nil)))))

(defun append-lists (a b)
  "Append list b to end of list a"
  (if (null a)
      b
      (cons (car a)
            (append-lists (cdr a) b))))

(defun ensure-symbols-registered ()
  "Register compiler symbols in the intern table if not already done.
   Uses SYS-EXIT-IR (an IR tag) to detect if already initialized."
  (if (find-interned "SYS-EXIT-IR" (get-intern-table))
      nil
      (set-intern-table
       (append-lists (make-special-forms)
        (append-lists (make-arithmetic)
         (append-lists (make-list-ops)
          (append-lists (make-predicates)
           (append-lists (make-string-ops)
            (append-lists (make-vector-ops)
             (append-lists (make-symbol-ops)
              (append-lists (make-system-ops)
               (append-lists (make-ir-basic)
                (append-lists (make-ir-cons)
                 (append-lists (make-ir-control)
                  (append-lists (make-ir-functions)
                   (append-lists (make-ir-syscalls)
                    (append-lists (make-ir-predicates)
                     (append-lists (make-ir-strings)
                      (append-lists (make-ir-vectors)
                       (append-lists (make-ir-intern)
                        (make-ir-lambda)))))))))))))))))))))

(defun map-list (fn lst)
  "Map function over list - iterative"
  (let ((current lst) (result nil))
    (while (not (null current))
     (setq result
             (cons (funcall fn (car current))
                   result))
     (setq current (cdr current)))
    (reverse result)))

(defun assoc-get (key alist)
  "Get value for key in alist - iterative"
  (let ((current alist)
        (found nil)
        (result nil))
    (while
     (and (not found) (not (null current)))
     (if (sym-eq key (car (car current)))
         (progn
          (setq found t)
          (setq result (cdr (car current))))
         (setq current (cdr current))))
    result))

(defun string= (s1 s2)
  "Compare two strings for equality - iterative"
  (if (or (null s1) (null s2))
      (and (null s1) (null s2))
      (let ((len1 (string-length s1))
            (len2 (string-length s2)))
        (if (= len1 len2)
            (let ((i 0) (equal t))
              (while (and equal (< i len1))
               (if (= (string-ref s1 i)
                      (string-ref s2 i))
                   (setq i (+ i 1))
                   (setq equal nil)))
              equal)
            nil))))

(defun find-interned (name table)
  "Find symbol with NAME in intern TABLE (alist of (name . symbol)) - iterative"
  (let ((current table)
        (found nil)
        (result nil))
    (while
     (and (not found) (not (null current)))
     (if (string= name (car (car current)))
         (progn
          (setq found t)
          (setq result (cdr (car current))))
         (setq current (cdr current))))
    result))

(defun intern (name)
  "Intern a string as a symbol. Returns existing symbol if found, else creates new.
   Preserves package prefix if present (ARM64:MOVZ stays ARM64:MOVZ).
   Adds current package prefix for unqualified names."
  (let ((qname
         (if (and (contains-colon name)
                  (> (string-length name) 0)
                  (not (= (string-ref name 0) 58)))
             (string-upcase name)
             (qualify-symbol-name name))))
    (let ((existing
           (find-interned qname
            (get-intern-table))))
      (if existing
          existing
          (let ((sym
                 (make-symbol-from-string qname)))
            (set-intern-table
             (cons (cons qname sym)
                   (get-intern-table)))
            sym)))))

(defun get-intern-table () *intern-table*)

(defun set-intern-table (table)
  (setq *intern-table* table))

(defun get-lambda-counter () *lambda-counter*)

(defun set-lambda-counter (n)
  (setq *lambda-counter* n))

(defun get-current-package () *current-package*)

(defun set-current-package (pkg)
  (setq *current-package* pkg))

(defun get-packages () *packages*)

(defun add-package (name)
  "Register a new package name"
  (if (not (member-string name *packages*))
      (setq *packages*
              (cons name *packages*))))

(defun member-string (s lst)
  "Check if string s is in list lst"
  (let ((current lst) (found nil))
    (while
     (and (not found) (not (null current)))
     (if (string= s (car current))
         (setq found t)
         (setq current (cdr current))))
    found))

(defun contains-colon (name)
  "Check if string contains a colon (for package-qualified symbols)"
  (let ((len (string-length name))
        (i 0)
        (found nil))
    (while
     (and (< i len) (not found))
     (if (= (string-ref name i) 58)
         (setq found t)
         (setq i (+ i 1))))
    found))

(defun strip-package-prefix (name)
  "Strip package prefix from symbol name. ARM64:ENCODE -> ENCODE.
   Keywords (:FOO) are returned unchanged.
   Names without colon are returned unchanged."
  (let ((len (string-length name)))
    (if (= len 0)
        name
        (if (= (string-ref name 0) 58)
            name
            (let ((i (- len 1))
                  (colon-pos -1))
              (while (>= i 0)
               (if (= (string-ref name i) 58)
                   (progn
                    (setq colon-pos i)
                    (setq i -1))
                   (setq i (- i 1))))
              (if (< colon-pos 0)
                  name
                  (substring name
                   (+ colon-pos 1) len)))))))

(defun qualify-symbol-name (name)
  "Add current package prefix if name doesn't have one and package is set.
   Names starting with : are keywords, leave unchanged.
   Names already containing : are package-qualified, leave unchanged."
  (if (null *current-package*)
      name
      (if (= (string-length name) 0)
          name
          (if (= (string-ref name 0) 58)
              name
              (if (contains-colon name)
                  name
                  (string-concat
                   (string-concat *current-package* ":")
                   name))))))

(defun whitespace? (ch)
  (or (= ch 32) (= ch 9) (= ch 10)
      (= ch 13)))

(defun digit? (ch)
  (and (>= ch 48) (<= ch 57)))

(defun digit-val (ch) (- ch 48))

(defun alpha? (ch)
  (or (and (>= ch 65) (<= ch 90))
      (and (>= ch 97) (<= ch 122))))

(defun symbol-char? (ch)
  (or (alpha? ch)
      (digit? ch) (= ch 45)
      (= ch 95) (= ch 43) (= ch 42)
      (= ch 47) (= ch 61) (= ch 60)
      (= ch 62) (= ch 33) (= ch 63)
      (= ch 38) (= ch 58) (= ch 37)))

(defun char-at (str pos)
  (if (>= pos (string-length str))
      0
      (string-ref str pos)))

(defun skip-line (source pos)
  "Skip to end of line - iterative"
  (let ((current-pos pos))
    (while
     (let ((ch
            (char-at source current-pos)))
       (and (not (= ch 10)) (not (= ch 0))))
     (setq current-pos (+ current-pos 1)))
    (+ current-pos 1)))

(defun skip-ws (source pos)
  "Skip whitespace and comments - iterative with inlined whitespace check"
  (let ((current-pos pos) (done nil))
    (while (not done)
     (let ((ch
            (char-at source current-pos)))
       (cond
        ((or (= ch 32) (= ch 9) (= ch 10)
             (= ch 13))
         (setq current-pos (+ current-pos 1)))
        ((= ch 59)
         (setq current-pos
                 (skip-line source
                  (+ current-pos 1))))
        (t (setq done t)))))
    current-pos))

(defun read-digits
       (source pos n)
  "Read decimal digits iteratively with inlined predicates"
  (let ((current-pos pos)
        (current-n n))
    (while
     (let ((ch
            (char-at source current-pos)))
       (and (>= ch 48) (<= ch 57)))
     (let ((ch
            (char-at source current-pos)))
       (setq current-n
               (+ (* current-n 10) (- ch 48))))
     (setq current-pos (+ current-pos 1)))
    (cons current-n current-pos)))

(defun read-int (source pos)
  (let ((ch
         (char-at source pos)))
    (cond
     ((= ch 45)
      (let ((result
             (read-digits source (+ pos 1)
              0)))
        (cons (- 0 (car result)) (cdr result))))
     ((= ch 43)
      (read-digits source (+ pos 1) 0))
     (t (read-digits source pos 0)))))

(defun hex-digit-val (ch)
  (cond
   ((and (>= ch 48) (<= ch 57)) (- ch 48))
   ((and (>= ch 65) (<= ch 70))
    (+ (- ch 65) 10))
   ((and (>= ch 97) (<= ch 102))
    (+ (- ch 97) 10))
   (t 0)))

(defun hex-digit? (ch)
  (or (and (>= ch 48) (<= ch 57))
      (and (>= ch 65) (<= ch 70))
      (and (>= ch 97) (<= ch 102))))

(defun read-hex-digits
       (source pos n)
  "Read hexadecimal digits iteratively with inlined predicates"
  (let ((current-pos pos)
        (current-n n))
    (while
     (let ((ch
            (char-at source current-pos)))
       (or (and (>= ch 48) (<= ch 57))
           (and (>= ch 65) (<= ch 70))
           (and (>= ch 97) (<= ch 102))))
     (let ((ch
            (char-at source current-pos)))
       (setq current-n
               (+ (* current-n 16)
                  (cond
                   ((and (>= ch 48) (<= ch 57))
                    (- ch 48))
                   ((and (>= ch 65) (<= ch 70))
                    (+ (- ch 65) 10))
                   (t (+ (- ch 97) 10))))))
     (setq current-pos (+ current-pos 1)))
    (cons current-n current-pos)))

(defun list-to-vector-rev (lst len)
  "Convert reversed list to vector of given length"
  (let ((vec (make-vector len))
        (i (- len 1)))
    (while (>= i 0)
     (vector-set vec i
      (car lst))
     (setq lst (cdr lst))
     (setq i (- i 1)))
    vec))

(defun read-str-chars
       (source pos acc)
  "Read string characters using list accumulator (O(n) allocation)"
  (let ((current-pos pos)
        (char-list nil)
        (char-count 0)
        (done nil))
    (while (not done)
     (let ((ch
            (char-at source current-pos)))
       (cond
        ((= ch 34) (setq done t)
         (setq current-pos (+ current-pos 1)))
        ((= ch 92)
         (let ((next-ch
                (char-at source
                 (+ current-pos 1))))
           (cond
            ((= next-ch 110)
             (setq char-list (cons 10 char-list))
             (setq char-count (+ char-count 1)))
            ((= next-ch 116)
             (setq char-list (cons 9 char-list))
             (setq char-count (+ char-count 1)))
            ((= next-ch 34)
             (setq char-list (cons 34 char-list))
             (setq char-count (+ char-count 1)))
            ((= next-ch 92)
             (setq char-list (cons 92 char-list))
             (setq char-count (+ char-count 1)))
            (t
             (setq char-list
                     (cons next-ch char-list))
             (setq char-count (+ char-count 1))))
           (setq current-pos (+ current-pos 2))))
        ((= ch 0) (setq done t))
        (t
         (setq char-list (cons ch char-list))
         (setq char-count (+ char-count 1))
         (setq current-pos (+ current-pos 1))))))
    (if (= char-count 0)
        (cons acc current-pos)
        (let ((vec
               (list-to-vector-rev char-list
                char-count)))
          (let ((new-str
                 (make-string-from-vector vec)))
            (cons
             (if (= (string-length acc) 0)
                 new-str
                 (string-concat acc new-str))
             current-pos))))))

(defun read-str (source pos)
  (read-str-chars source (+ pos 1) ""))

(defun read-sym-chars
       (source pos acc)
  "Read symbol characters using vector accumulator (O(n) allocation).
   Inlines symbol-char? check to avoid function call overhead."
  (let ((start-pos pos)
        (current-pos pos)
        (source-len (string-length source)))
    (while
     (let ((ch
            (if (>= current-pos source-len)
                0
                (string-ref source
                 current-pos))))
       (or (and (>= ch 65) (<= ch 90))
           (and (>= ch 97) (<= ch 122))
           (and (>= ch 48) (<= ch 57))
           (= ch 45) (= ch 95) (= ch 43)
           (= ch 42) (= ch 47) (= ch 61)
           (= ch 60) (= ch 62) (= ch 33)
           (= ch 63) (= ch 38) (= ch 58)
           (= ch 37)))
     (setq current-pos (+ current-pos 1)))
    (let ((len (- current-pos start-pos)))
      (if (= len 0)
          (cons acc current-pos)
          (let ((vec (make-vector len))
                (i 0))
            (while (< i len)
             (vector-set vec i
              (char-at source
               (+ start-pos i)))
             (setq i (+ i 1)))
            (let ((new-str
                   (make-string-from-vector vec)))
              (cons
               (if (= (string-length acc) 0)
                   new-str
                   (string-concat acc
                    new-str))
               current-pos)))))))

(defun upcase-char (ch)
  (if (and (>= ch 97) (<= ch 122))
      (- ch 32)
      ch))

(defun upcase-string (s)
  "Upcase string using vector accumulator (O(n) allocation)"
  (let ((len (string-length s)))
    (if (= len 0)
        s
        (let ((vec (make-vector len))
              (i 0))
          (while (< i len)
           (vector-set vec i
            (upcase-char
             (string-ref s i)))
           (setq i (+ i 1)))
          (make-string-from-vector vec)))))

(defun read-sym (source pos)
  (let ((result
         (read-sym-chars source pos "")))
    (let ((name (car result))
          (end-pos (cdr result)))
      (if (= (string-length name) 0)
          (cons nil (+ pos 1))
          (let ((uname (upcase-string name)))
            (let ((first-ch
                   (string-ref name 0)))
              (cond
               ((digit? first-ch)
                (read-int source pos))
               ((and (= first-ch 45)
                     (> (string-length name) 1)
                     (digit?
                      (string-ref name 1)))
                (read-int source pos))
               ((and (= first-ch 43)
                     (> (string-length name) 1)
                     (digit?
                      (string-ref name 1)))
                (read-int source pos))
               ((string= uname "NIL") (cons nil end-pos))
               ((string= uname "T") (cons t end-pos))
               (t (cons (intern uname) end-pos)))))))))

(defun habu-read (source pos)
  (labels ((read-list-elems (start-pos)
             (let ((current-pos start-pos)
                   (acc nil)
                   (done nil)
                   (final-cdr nil))
               (while (not done)
                (let ((pos2
                       (skip-ws source
                        current-pos)))
                  (setq current-pos pos2)
                  (let ((ch
                         (char-at source
                          current-pos)))
                    (cond
                     ((= ch 41) (setq done t)
                      (setq current-pos (+ current-pos 1)))
                     ((= ch 46)
                      (let ((result
                             (read-one (+ current-pos 1))))
                        (setq final-cdr (car result))
                        (setq current-pos (cdr result)))
                      (setq current-pos
                              (+
                               (skip-ws source
                                current-pos)
                               1))
                      (setq done t))
                     ((= ch 0) (setq done t))
                     (t
                      (let ((elem-result
                             (read-one current-pos)))
                        (setq acc
                                (cons (car elem-result) acc))
                        (setq current-pos
                                (cdr elem-result))))))))
               (let ((result final-cdr))
                 (while acc
                  (setq result
                          (cons (car acc) result))
                  (setq acc (cdr acc)))
                 (cons result current-pos))))
           (read-list (pos)
             (read-list-elems (+ pos 1)))
           (feature-present? (sym)
             (if (symbolp sym)
                 (let ((name (symbol-name sym)))
                   (or (string= name "HABU")
                       (string= name "habu")))
                 nil))
           (read-sharp (pos)
             (let ((ch
                    (char-at source
                     (+ pos 1))))
               (cond
                ((= ch 120)
                 (read-hex-digits source
                  (+ pos 2) 0))
                ((= ch 92)
                 (let ((ch2
                        (char-at source
                         (+ pos 2))))
                   (if (alpha? ch2)
                       (let ((result
                              (read-sym-chars source
                               (+ pos 2) "")))
                         (let ((name (car result)))
                           (cons
                            (cond ((string= name "newline") 10)
                                  ((string= name "space") 32)
                                  ((string= name "tab") 9)
                                  (t ch2))
                            (cdr result))))
                       (cons ch2 (+ pos 3)))))
                ((= ch 43)
                 (let ((feat-result
                        (read-one (+ pos 2))))
                   (let ((feature (car feat-result))
                         (pos3 (cdr feat-result)))
                     (let ((form-result
                            (read-one pos3)))
                       (let ((form (car form-result))
                             (pos4 (cdr form-result)))
                         (if (feature-present? feature)
                             (cons form pos4)
                             (read-one pos4)))))))
                ((= ch 45)
                 (let ((feat-result
                        (read-one (+ pos 2))))
                   (let ((feature (car feat-result))
                         (pos3 (cdr feat-result)))
                     (let ((form-result
                            (read-one pos3)))
                       (let ((form (car form-result))
                             (pos4 (cdr form-result)))
                         (if (not (feature-present? feature))
                             (cons form pos4)
                             (read-one pos4)))))))
                (t (cons nil (+ pos 2))))))
           (read-one (pos)
             (let ((pos2
                    (skip-ws source pos)))
               (if (>= pos2
                       (string-length source))
                   (cons nil pos2)
                   (let ((ch
                          (char-at source
                           pos2)))
                     (cond
                      ((= ch 34)
                       (read-str source
                        pos2))
                      ((= ch 40)
                       (read-list pos2))
                      ((= ch 39)
                       (let ((result
                              (read-one (+ pos2 1))))
                         (cons (list 'quote (car result))
                               (cdr result))))
                      ((= ch 96)
                       (let ((result
                              (read-one (+ pos2 1))))
                         (cons (list 'quote (car result))
                               (cdr result))))
                      ((= ch 44)
                       (let ((pos3 (+ pos2 1)))
                         (if (=
                              (char-at source
                               pos3)
                              64)
                             (let ((result
                                    (read-one (+ pos3 1))))
                               (cons
                                (list 'unquote-splicing
                                      (car result))
                                (cdr result)))
                             (let ((result
                                    (read-one pos3)))
                               (cons
                                (list 'unquote (car result))
                                (cdr result))))))
                      ((= ch 35)
                       (read-sharp pos2))
                      ((= ch 124)
                       (let ((result
                              (read-pipe-symbol source
                               (+ pos2 1) "")))
                         (let ((name (car result))
                               (end-pos (cdr result)))
                           (cons (intern name) end-pos))))
                      (t
                       (read-sym source
                        pos2))))))))
    (read-one pos)))

(defun read-pipe-symbol
       (source pos acc)
  "Read pipe-quoted symbol using vector accumulator (O(n) allocation)"
  (let ((start-pos pos)
        (current-pos pos))
    (while
     (and
      (not
       (= (char-at source current-pos) 124))
      (not
       (= (char-at source current-pos) 0)))
     (setq current-pos (+ current-pos 1)))
    (let ((len (- current-pos start-pos)))
      (if (= len 0)
          (cons acc
                (if (=
                     (char-at source
                      current-pos)
                     124)
                    (+ current-pos 1)
                    current-pos))
          (let ((vec (make-vector len))
                (i 0))
            (while (< i len)
             (vector-set vec i
              (char-at source
               (+ start-pos i)))
             (setq i (+ i 1)))
            (let ((new-str
                   (make-string-from-vector vec)))
              (cons
               (if (= (string-length acc) 0)
                   new-str
                   (string-concat acc
                    new-str))
               (if (=
                    (char-at source
                     current-pos)
                    124)
                   (+ current-pos 1)
                   current-pos))))))))

(defun keyword-to-string (kw)
  "Convert a keyword symbol to its package name string.
   :FOO -> FOO, :foo -> FOO"
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) 58))
        (upcase-string
         (substring name 1
          (string-length name)))
        (upcase-string name))))

(defun substring
       (s start end)
  "Extract substring from start to end"
  (let ((len (- end start)))
    (if (<= len 0)
        ""
        (let ((vec (make-vector len))
              (i 0))
          (while (< i len)
           (vector-set vec i
            (string-ref s
             (+ start i)))
           (setq i (+ i 1)))
          (make-string-from-vector vec)))))

(defun process-package-form (form)
  "Process defpackage or in-package form, updating reader state.
   Returns t if form was processed, nil otherwise."
  (if (and (consp form) (symbolp (car form)))
      (let ((head-name (symbol-name (car form))))
        (cond
         ((string= head-name "IN-PACKAGE")
          (if (and (cdr form) (symbolp (cadr form)))
              (let ((pkg-name
                     (keyword-to-string (cadr form))))
                (set-current-package pkg-name)
                t)
              nil))
         ((string= head-name "DEFPACKAGE")
          (if (and (cdr form) (symbolp (cadr form)))
              (let ((pkg-name
                     (keyword-to-string (cadr form))))
                (add-package pkg-name)
                t)
              nil))
         (t nil)))
      nil))

(defun read-all (source)
  "Read all forms from source string - iterative.
   Processes defpackage and in-package forms to update reader state."
  (let ((pos 0)
        (acc nil)
        (source-len (string-length source)))
    (while (< pos source-len)
     (setq pos
             (skip-ws source pos))
     (if (< pos source-len)
         (let ((result
                (habu-read source pos)))
           (let ((form (car result)))
             (process-package-form form)
             (setq acc (cons form acc)))
           (setq pos (cdr result)))))
    (reverse acc)))

(defun reverse (lst)
  "Reverse list - iterative"
  (let ((current lst) (acc nil))
    (while (not (null current))
     (setq acc (cons (car current) acc))
     (setq current (cdr current)))
    acc))

(defun register-compiler-symbols ()
  "Register all symbols used in compiler dispatch to the intern table.
   This must be called before reading any source code."
  (set-intern-table
   (list (cons "DEFUN" 'defun) (cons "PROGN" 'progn) (cons "IF" 'if) (cons "LET" 'let)
         (cons "LET*" 'let*) (cons "QUOTE" 'quote) (cons "LAMBDA" 'lambda)
         (cons "FUNCALL" 'funcall) (cons "LABELS" 'labels) (cons "FUNCTION" 'function)
         (cons "COND" 'cond) (cons "WHEN" 'when) (cons "UNLESS" 'unless) (cons "AND" 'and)
         (cons "OR" 'or) (cons "NOT" 'not) (cons "SETQ" 'setq) (cons "+" '+) (cons "-" '-)
         (cons "*" '*) (cons "/" '/) (cons "MOD" 'mod) (cons "=" '=) (cons "<" '<) (cons ">" '>)
         (cons "<=" '<=) (cons ">=" '>=) (cons "/=" '/=) (cons "EQ" 'eq) (cons "LOGAND" 'logand)
         (cons "LOGIOR" 'logior) (cons "LOGXOR" 'logxor) (cons "ASH" 'ash) (cons "CONS" 'cons)
         (cons "CAR" 'car) (cons "CDR" 'cdr) (cons "CADR" 'cadr) (cons "CADDR" 'caddr)
         (cons "CDDR" 'cddr) (cons "CDDDR" 'cdddr) (cons "CADDDR" 'cadddr) (cons "NTH" 'nth)
         (cons "LIST" 'list) (cons "LENGTH" 'length) (cons "REVERSE" 'reverse)
         (cons "SETCAR" 'setcar) (cons "SETCDR" 'setcdr)
         (cons "NULL" 'null) (cons "CONSP" 'consp) (cons "NUMBERP" 'numberp)
         (cons "SYMBOLP" 'symbolp) (cons "STRINGP" 'stringp) (cons "VECTORP" 'vectorp)
         (cons "STRING-LENGTH" 'string-length)
         (cons "STRING-REF" 'string-ref)
         (cons "STRING-CONCAT" 'string-concat)
         (cons "STRING-EQUAL" 'string-equal) (cons "MAKE-VECTOR" 'make-vector)
         (cons "VECTOR-REF" 'vector-ref)
         (cons "VECTOR-SET" 'vector-set)
         (cons "VECTOR-LENGTH" 'vector-length)
         (cons "MAKE-STRING-FROM-VECTOR" 'make-string-from-vector)
         (cons "BUFFER-TO-STRING" 'buffer-to-string)
         (cons "BUFFER-BYTE-REF" 'buffer-byte-ref)
         (cons "BUFFER-BYTE-SET" 'buffer-byte-set)
         (cons "SYMBOL-NAME" 'symbol-name)
         (cons "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string)
         (cons "SYS-EXIT" 'sys-exit)
         (cons "SYS-OPEN" 'sys-open)
         (cons "SYS-READ" 'sys-read)
         (cons "SYS-WRITE" 'sys-write)
         (cons "SYS-WRITE-CHAR" 'sys-write-char)
         (cons "SYS-READ-BYTE" 'sys-read-byte)
         (cons "SYS-CLOSE" 'sys-close)
         (cons "NATIVE-READ-FILE" 'native-read-file)
         (cons "GET-INTERN-TABLE" 'get-intern-table)
         (cons "SET-INTERN-TABLE" 'set-intern-table)
         (cons "GET-LAMBDA-COUNTER" 'get-lambda-counter)
         (cons "SET-LAMBDA-COUNTER" 'set-lambda-counter)
         (cons "JIT-MMAP" 'jit-mmap)
         (cons "JIT-WRITE-PROTECT" 'jit-write-protect)
         (cons "JIT-DCACHE-FLUSH" 'jit-dcache-flush)
         (cons "JIT-ICACHE-INVALIDATE" 'jit-icache-invalidate)
         (cons "JIT-CALL" 'jit-call)
         (cons "MEM-SET-BYTE" 'mem-set-byte)
         (cons "MEM-LOAD-64" 'mem-load-64) (cons "NIL" 'nil) (cons "T" 't))))

(defun append (lst1 lst2)
  "Append two lists without using CL append"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l)
                  (cons (car l) acc)))))
    (append-iter (reverse-helper lst1 nil)
     lst2)))

(defun reverse-helper (lst acc)
  "Tail-recursive reverse helper - defined early for use by append"
  (if (null lst)
      acc
      (reverse-helper (cdr lst)
       (cons (car lst) acc))))

(defun reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l)
                  (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun length (seq)
  "Length of list or string"
  (if (stringp seq)
      (string-length seq)
      (labels ((len-iter (l n)
                 (if (null l)
                     n
                     (len-iter (cdr l)
                      (+ n 1)))))
        (len-iter seq 0))))

(defun nth (n lst)
  "Get nth element"
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

(defun count-if (pred lst)
  "Count elements satisfying predicate"
  (labels ((count-iter (l n)
             (if (null l)
                 n
                 (count-iter (cdr l)
                  (if (funcall pred (car l))
                      (+ n 1)
                      n)))))
    (count-iter lst 0)))

(defun remove-if (pred lst)
  "Remove elements satisfying predicate"
  (labels ((remove-iter (l acc)
             (if (null l)
                 (reverse acc)
                 (remove-iter (cdr l)
                  (if (funcall pred (car l))
                      acc
                      (cons (car l) acc))))))
    (remove-iter lst nil)))

(defun string-equal-iter
       (s1 s2 i len)
  "Internal: compare strings starting at index i"
  (if (>= i len)
      t
      (if (= (string-ref s1 i)
             (string-ref s2 i))
          (string-equal-iter s1 s2
           (+ i 1) len)
          nil)))

(defun string-equal (s1 s2)
  "Compare two strings character by character - pure Habu implementation"
  (if (or (null s1) (null s2))
      (and (null s1) (null s2))
      (let ((len1 (string-length s1))
            (len2 (string-length s2)))
        (if (= len1 len2)
            (string-equal-iter s1 s2 0
             len1)
            nil))))

(defun assoc (key alist)
  "Find (key . value) pair in alist using string comparison"
  (if (null alist)
      nil
      (if (string-equal key (car (car alist)))
          (car alist)
          (assoc key (cdr alist)))))

(defun mapcar (fn lst)
  "Map function over list"
  (labels ((map-iter (l acc)
             (if (null l)
                 (reverse acc)
                 (map-iter (cdr l)
                  (cons (funcall fn (car l))
                        acc)))))
    (map-iter lst nil)))

(defun fold-binop
       (ir-tag args env
        fenv)
  "Fold variadic operation into nested binary operations.
   (+ a b c) => (add (add a b) c)"
  (if (null (cdr args))
      (compile-expr-full (car args) env
       fenv)
      (labels ((fold (remaining acc)
                 (if (null remaining)
                     acc
                     (fold (cdr remaining)
                      (list ir-tag acc
                            (compile-expr-full (car remaining)
                             env fenv))))))
        (fold (cddr args)
         (list ir-tag
               (compile-expr-full (car args)
                env fenv)
               (compile-expr-full (cadr args)
                env fenv))))))

(defun compile-lit (val)
  "Compile literal to IR"
  (list 'lit val))

(defun compile-var (sym env)
  "Compile variable reference using flat env list"
  (let ((offset
         (flat-env-lookup sym env)))
    (if offset
        (list 'var-ref offset)
        (list 'lit 0))))

(defun flat-env-lookup (sym env)
  "Look up symbol in environment, return offset or nil - ITERATIVE VERSION"
  (let ((e env)
        (offset 0)
        (result nil)
        (done nil))
    (while (and (not done) (not (null e)))
     (if (sym-eq (car e) sym)
         (progn
          (setq result offset)
          (setq done t))
         (progn
          (setq e (cdr e))
          (setq offset (+ offset 1)))))
    result))

(defun compile-if (expr env)
  "Compile (if test then else) to IR"
  (let ((test
         (compile-expr (nth 1 expr) env))
        (then
         (compile-expr (nth 2 expr) env))
        (else
         (compile-expr (nth 3 expr) env)))
    (list 'if-ir test then
          else)))

(defun compile-expr (expr env)
  "Compile expression to IR - pure Habu version"
  (cond ((numberp expr) (compile-lit expr))
        ((null expr) (list 'nil-ir))
        ((symbolp expr)
         (compile-var expr env))
        ((not (consp expr)) (compile-lit 0))
        ((sym-eq (car expr) 'if)
         (compile-if expr env))
        ((sym-eq (car expr) '+)
         (list 'add-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '*)
         (list 'mul-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '-)
         (list 'sub-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '=)
         (list 'cmp-eq
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        (t (error "compile-expr: unhandled form ~S" expr))))

(defun compile-let (expr env)
  "Compile (let ((var val) ...) body) to IR"
  (let ((bindings (nth 1 expr))
        (body (nth 2 expr)))
    (labels ((extend-env (binds e)
               (if (null binds)
                   e
                   (extend-env (cdr binds)
                    (cons (car (car binds)) e)))))
      (let ((new-env
             (extend-env bindings env)))
        (labels ((compile-bindings (binds acc)
                   (if (null binds)
                       (reverse acc)
                       (let ((val (nth 1 (car binds))))
                         (compile-bindings (cdr binds)
                          (cons
                           (compile-expr val
                            env)
                           acc))))))
          (let ((val-irs
                 (compile-bindings bindings nil))
                (body-ir
                 (compile-expr body new-env)))
            (list 'let-ir val-irs
                  body-ir)))))))

(defun quote-ir (obj)
  "Build IR for quoted value - recursively builds cons-ir for lists"
  (cond ((numberp obj) (list 'lit obj))
        ((null obj) (list 'nil-ir))
        ((symbolp obj)
         (list 'sym-lit (symbol-name obj)))
        ((consp obj)
         (list 'cons-ir (quote-ir (car obj))
               (quote-ir (cdr obj))))
        ((stringp obj) (list 'str-lit obj))
        (t (error "quote-ir: unhandled type for ~S" obj))))

(defun compile-quote (expr)
  "Compile (quote x) to IR"
  (quote-ir (nth 1 expr)))

(defun compile-cons (expr env)
  "Compile (cons a b) to IR"
  (list 'cons-ir
        (compile-expr (nth 1 expr) env)
        (compile-expr (nth 2 expr) env)))

(defun compile-car (expr env)
  "Compile (car x) to IR"
  (list 'car-ir
        (compile-expr (nth 1 expr) env)))

(defun compile-cdr (expr env)
  "Compile (cdr x) to IR"
  (list 'cdr-ir
        (compile-expr (nth 1 expr) env)))

(defun compile-list (expr env)
  "Compile (list a b c) to IR"
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'nil-ir)
                 (list 'cons-ir
                       (compile-expr (car elems)
                        env)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))

(defun compile-progn (expr env)
  "Compile (progn e1 e2 e3) to IR"
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (reverse acc)
                 (compile-exprs (cdr exprs)
                  (cons
                   (compile-expr (car exprs)
                    env)
                   acc)))))
    (list 'progn-ir
          (compile-exprs (cdr expr) nil))))

(defun compile-expr-v2 (expr env)
  "Enhanced expression compiler - handles more forms"
  (cond ((numberp expr) (compile-lit expr))
        ((symbolp expr)
         (compile-var expr env))
        ((not (consp expr)) (compile-lit 0))
        ((sym-eq (car expr) 'if)
         (compile-if expr env))
        ((sym-eq (car expr) 'quote)
         (compile-quote expr))
        ((sym-eq (car expr) 'let)
         (compile-let expr env))
        ((sym-eq (car expr) 'progn)
         (compile-progn expr env))
        ((sym-eq (car expr) '+)
         (list 'add-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '-)
         (list 'sub-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '*)
         (list 'mul-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '/)
         (list 'div-ir
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '=)
         (list 'cmp-eq
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) '<)
         (list 'cmp-lt
               (compile-expr (nth 1 expr)
                env)
               (compile-expr (nth 2 expr)
                env)))
        ((sym-eq (car expr) 'cons)
         (compile-cons expr env))
        ((sym-eq (car expr) 'car)
         (compile-car expr env))
        ((sym-eq (car expr) 'cdr)
         (compile-cdr expr env))
        ((sym-eq (car expr) 'list)
         (compile-list expr env))
        (t (error "compile-expr-v2: unhandled form ~S" expr))))

(defun keyword-name-p (sym)
  "Check if symbol is a keyword (name starts with :)"
  (if (symbolp sym)
      (let ((name (symbol-name sym)))
        (if (> (string-length name) 0)
            (= (string-ref name 0) 58)
            nil))
      nil))

(defun keyword-to-param-name (kw)
  "Extract parameter name from keyword.
   In native Habu: :FOO has name ':FOO', need to skip first char"
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) 58))
        (labels ((copy-chars (i acc)
                   (if (>= i
                           (string-length name))
                       (make-string-from-vector acc)
                       (progn
                        (vector-set acc
                         (- i 1)
                         (string-ref name i))
                        (copy-chars (+ i 1)
                         acc)))))
          (let ((result-vec
                 (make-vector
                  (- (string-length name) 1))))
            (copy-chars 1 result-vec)))
        name)))

(defun parse-lambda-list (params)
  "Parse lambda list, splitting at &optional and &key.
   Returns (positional-params . keyword-specs) where keyword-specs is
   a list of (name default) pairs.
   &optional params are added to positional-params (names only, defaults ignored)."
  (labels ((collect
               (ps pos-acc kw-acc
                in-opt in-keys)
             (if (null ps)
                 (cons (reverse pos-acc) (reverse kw-acc))
                 (let ((p (car ps)))
                   (cond
                    ((sym-eq p '&optional)
                     (collect (cdr ps)
                      pos-acc kw-acc t nil))
                    ((sym-eq p '&key)
                     (collect (cdr ps)
                      pos-acc kw-acc nil t))
                    (in-keys
                     (if (consp p)
                         (collect (cdr ps)
                          pos-acc
                          (cons (list (car p) (cadr p))
                                kw-acc)
                          nil t)
                         (collect (cdr ps)
                          pos-acc
                          (cons (list p nil) kw-acc) nil t)))
                    (in-opt
                     (if (consp p)
                         (collect (cdr ps)
                          (cons (car p) pos-acc)
                          kw-acc t nil)
                         (collect (cdr ps)
                          (cons p pos-acc)
                          kw-acc t nil)))
                    (t
                     (collect (cdr ps)
                      (cons p pos-acc) kw-acc
                      nil nil)))))))
    (collect params nil nil nil nil)))

(defun vec-ref (v i)
  (vector-ref v i))

(defun vec-set (v i val)
  (vector-set v i val))

(defun kw-to-param-sym (kw)
  "Convert keyword :FOO to parameter symbol FOO.
   Used for symbol-based comparison in keyword argument matching."
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) 58))
        (make-symbol-from-string
         (keyword-to-param-name kw))
        kw)))

(defun find-kw-position
       (param-sym keyword-specs)
  "Find position of param-sym in keyword-specs using symbol equality.
   param-sym is a symbol (e.g., IMM), keyword-specs is ((NAME DEFAULT) ...)."
  (labels ((search-specs (specs pos)
             (if (null specs)
                 nil
                 (if (sym-eq param-sym (car (car specs)))
                     pos
                     (search-specs (cdr specs)
                      (+ pos 1))))))
    (search-specs keyword-specs 0)))

(defun rewrite-kw-call
       (args n-positional keyword-specs)
  "Rewrite call args with keywords to fully positional args.
   Returns list of args in positional order, with defaults for unspecified keywords."
  (let* ((n-keywords (length keyword-specs))
         (kw-values (make-vector n-keywords)))
    (labels ((init-defaults (specs idx)
               (if (null specs)
                   nil
                   (progn
                    (vec-set kw-values idx
                     (cadr (car specs)))
                    (init-defaults (cdr specs)
                     (+ idx 1))))))
      (init-defaults keyword-specs 0))
    (labels ((take-n
                 (lst n acc)
               (if (or (null lst) (= n 0))
                   (cons (reverse acc) lst)
                   (take-n (cdr lst) (- n 1)
                    (cons (car lst) acc)))))
      (let* ((split
              (take-n args n-positional nil))
             (pos-args (car split))
             (rest-args (cdr split)))
        (labels ((parse-kws (rest)
                   (if (null rest)
                       nil
                       (if (null (cdr rest))
                           nil
                           (let ((kw (car rest))
                                 (val (cadr rest)))
                             (if (keyword-name-p kw)
                                 (let* ((param-sym
                                         (kw-to-param-sym kw))
                                        (pos
                                         (find-kw-position
                                          param-sym
                                          keyword-specs)))
                                   (if pos
                                       (vec-set kw-values
                                        pos val))
                                   (parse-kws (cddr rest)))
                                 (parse-kws (cdr rest))))))))
          (parse-kws rest-args))
        (labels ((collect-kw-values (idx acc)
                   (if (>= idx n-keywords)
                       (reverse acc)
                       (collect-kw-values (+ idx 1)
                        (cons
                         (vec-ref kw-values
                          idx)
                         acc)))))
          (append pos-args (collect-kw-values 0 nil)))))))

(defun call-has-kw-p (args)
  "Check if call arguments contain keywords"
  (if (null args)
      nil
      (if (keyword-name-p (car args))
          t
          (call-has-kw-p (cdr args)))))

(defun flatten-parsed-params (parsed)
  "Convert parsed params (positional . kw-specs) to flat param list.
   Keyword specs ((NAME DEFAULT) ...) become just (NAME ...) in result."
  (let ((pos-params (car parsed))
        (kw-specs (cdr parsed)))
    (labels ((extract-names (specs acc)
               (if (null specs)
                   (reverse acc)
                   (extract-names (cdr specs)
                    (cons (car (car specs)) acc)))))
      (append pos-params
              (extract-names kw-specs nil)))))

(defun collect-defuns (forms acc)
  "Pass 1: Collect all defun info (name params body) from forms"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
         ((and (consp f) (sym-eq (car f) 'defun))
          (let* ((nm (cadr f))
                 (ps (caddr f))
                 (body-forms (cdddr f))
                 (bd
                  (if (null (cdr body-forms))
                      (car body-forms)
                      (cons 'progn body-forms))))
            (collect-defuns (cdr forms)
             (cons (list nm ps bd)
                   acc))))
         ((and (consp f) (sym-eq (car f) 'progn))
          (collect-defuns (cdr forms)
           (collect-defuns (cdr f) acc)))
         (t
          (collect-defuns (cdr forms)
           acc))))))

(defun expr-size (expr)
  "Estimate expression size for inlining decisions"
  (cond ((null expr) 1) ((not (consp expr)) 1)
        ((sym-eq (car expr) 'quote) 1)
        ((sym-eq (car expr) 'progn)
         (let ((sum 0))
           (let ((es (cdr expr)))
             (while (not (null es))
              (setq sum
                      (+ sum
                         (expr-size (car es))))
              (setq es (cdr es))))
           sum))
        ((sym-eq (car expr) 'if)
         (+ 1 (expr-size (cadr expr))
            (expr-size (caddr expr))
            (if (cadddr expr)
                (expr-size (cadddr expr))
                0)))
        ((sym-eq (car expr) 'let)
         (+ 2 (expr-size (caddr expr))))
        ((sym-eq (car expr) 'let*)
         (+ 2 (expr-size (caddr expr))))
        ((or (sym-eq (car expr) 'or) (sym-eq (car expr) 'and))
         (let ((sum 1))
           (let ((es (cdr expr)))
             (while (not (null es))
              (setq sum
                      (+ sum
                         (expr-size (car es))))
              (setq es (cdr es))))
           sum))
        (t (+ 1 (length (cdr expr))))))

(defun calls-self? (expr fn-name)
  "Check if expression calls fn-name (direct recursion)"
  (cond ((null expr) nil) ((not (consp expr)) nil)
        ((sym-eq (car expr) 'quote) nil)
        ((and (symbolp (car expr))
              (sym-eq (car expr) fn-name))
         t)
        (t
         (let ((found nil) (es (cdr expr)))
           (while
            (and (not found) (not (null es)))
            (setq found
                    (calls-self? (car es)
                     fn-name))
            (setq es (cdr es)))
           found))))

(defun inlinable? (fn-info)
  "Check if function is eligible for inlining.
   FN-INFO is (name params body).
   Inline if: small body, no recursion, simple predicates"
  (let ((name (car fn-info))
        (params (cadr fn-info))
        (body (caddr fn-info)))
    (and (< (expr-size body) 20)
         (not (calls-self? body name))
         (<= (length params) 4))))

(defun substitute-params
       (expr params args)
  "Replace parameters with QUOTED arguments in expression.
   PARAMS is list of parameter names, ARGS is list of argument exprs.
   Arguments are quoted so they aren't evaluated when macro body is eval'd."
  (cond ((null expr) nil)
        ((symbolp expr)
         (let ((pos
                (find-param-pos expr params
                 0)))
           (if pos
               (let ((sb-debug:arg (nth pos args)))
                 (if (or (symbolp sb-debug:arg) (consp sb-debug:arg))
                     (list 'quote sb-debug:arg)
                     sb-debug:arg))
               expr)))
        ((not (consp expr)) expr)
        ((sym-eq (car expr) 'quote) expr)
        (t
         (cons
          (substitute-params (car expr)
           params args)
          (substitute-params (cdr expr)
           params args)))))

(defun find-param-pos
       (name params idx)
  "Find position of name in params list"
  (cond ((null params) nil)
        ((sym-eq name (car params)) idx)
        (t
         (find-param-pos name (cdr params)
          (+ idx 1)))))

(defun get-fn-info (name fenv)
  "Get function info (name params body) from fenv"
  (cond ((null fenv) nil)
        ((sym-eq name (car (car fenv)))
         (car fenv))
        (t (get-fn-info name (cdr fenv)))))

(defun compile-defun
       (name params body
        env fenv)
  "Compile a single defun to (name params body-ir param-base).
   Handles &key by parsing lambda list and flattening keyword params."
  (let* ((parsed (parse-lambda-list params))
         (flat-params
          (flatten-parsed-params parsed))
         (new-env
          (extend-env flat-params env))
         (pb
          (if flat-params
              (flat-env-lookup (car flat-params)
               new-env)
              0))
         (body-ir
          (compile-expr-full body new-env
           fenv)))
    (list name flat-params body-ir
          pb)))

(defun extend-env (params env)
  "Extend environment with parameter bindings - append to preserve offset consistency"
  (append env params))

(defun skip-docstring (body-forms)
  "Skip docstring if present (string as first body element with more forms)"
  (if (and (stringp (car body-forms)) (cdr body-forms))
      (cdr body-forms)
      body-forms))

(defun compile-all-defuns
       (forms env fenv acc)
  "Pass 2: Compile all defuns with complete fenv"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
         ((and (consp f) (sym-eq (car f) 'defun))
          (let* ((nm (cadr f))
                 (ps (caddr f))
                 (body-forms
                  (skip-docstring (cdddr f)))
                 (bd
                  (if (null (cdr body-forms))
                      (car body-forms)
                      (cons 'progn body-forms)))
                 (cf
                  (compile-defun nm ps
                   bd env fenv)))
            (compile-all-defuns (cdr forms)
             env fenv
             (cons cf acc))))
         ((and (consp f) (sym-eq (car f) 'progn))
          (compile-all-defuns (cdr forms) env
           fenv
           (compile-all-defuns (cdr f) env
            fenv acc)))
         (t
          (compile-all-defuns (cdr forms) env
           fenv acc))))))

(defun package-form-p (f)
  "Check if form is defpackage or in-package (handled at read time)"
  (and (consp f)
       (or (sym-eq (car f) 'defpackage) (sym-eq (car f) 'in-package))))

(defun find-main-form (forms acc)
  "Find all non-defun forms and wrap in progn if multiple.
   Skips defpackage and in-package forms (handled at read time)."
  (if (null forms)
      (if (null acc)
          (list 'lit 0)
          (if (null (cdr acc))
              (car acc)
              (cons 'progn (reverse acc))))
      (let ((f (car forms)))
        (cond
         ((and (consp f) (sym-eq (car f) 'defun))
          (find-main-form (cdr forms) acc))
         ((package-form-p f)
          (find-main-form (cdr forms) acc))
         ((and (consp f) (sym-eq (car f) 'progn))
          (find-main-form (cdr forms)
           (find-main-form (cdr f) acc)))
         (t
          (find-main-form (cdr forms)
           (cons f acc)))))))

(defun compile-call
       (expr env fenv)
  "Compile function call (fn arg1 arg2 ...).
   Handles keyword arguments by rewriting to positional form.
   Inlines small functions to avoid call overhead."
  (let ((fn-name (car expr))
        (args (cdr expr)))
    (let ((fn-info
           (get-fn-info fn-name fenv)))
      (if fn-info
          (let* ((params (cadr fn-info))
                 (body (caddr fn-info))
                 (parsed
                  (parse-lambda-list params))
                 (pos-params (car parsed))
                 (kw-specs (cdr parsed))
                 (final-args
                  (if (and kw-specs
                           (call-has-kw-p args))
                      (rewrite-kw-call args
                       (length pos-params) kw-specs)
                      args)))
            (if (inlinable? fn-info)
                (let ((inlined-body
                       (substitute-params body
                        params final-args)))
                  (compile-expr-full inlined-body
                   env fenv))
                (progn
                 (list 'call-fn fn-name
                       (compile-args final-args
                        env fenv)))))
          (progn (list 'sys-exit-ir (list 'lit 200)))))))

(defun fenv-lookup (name fenv)
  "Look up function in function environment"
  (if (null fenv)
      nil
      (if (sym-eq (car (car fenv)) name)
          t
          (fenv-lookup name (cdr fenv)))))

(defun compile-args
       (args env fenv)
  "Compile list of arguments"
  (if (null args)
      nil
      (cons
       (compile-expr-full (car args) env
        fenv)
       (compile-args (cdr args) env
        fenv))))

(defun compile-lambda
       (expr env fenv)
  "Compile (lambda (params) body) to lambda-ir.
   CRITICAL: Must include free-offsets for closure capture to work!"
  (let* ((params (cadr expr))
         (body-forms (cddr expr))
         (body
          (if (null (cdr body-forms))
              (car body-forms)
              (cons 'progn body-forms)))
         (free-vars
          (find-free-vars body params
           env))
         (free-offsets
          (mapcar
           (lambda (v)
             (flat-env-lookup v env))
           free-vars))
         (body-env
          (extend-env params
           (extend-env free-vars nil)))
         (body-ir
          (compile-expr-full body body-env
           fenv)))
    (list 'lambda-ir params body-ir
          free-vars free-offsets)))

(defun find-free-vars
       (expr params env)
  "Find variables referenced in expr that are in env but not in params or local bindings"
  (labels ((in-list (x lst)
             (if (null lst)
                 nil
                 (if (sym-eq x (car lst))
                     t
                     (in-list x (cdr lst)))))
           (get-let-vars (bindings acc)
             (if (null bindings)
                 acc
                 (get-let-vars (cdr bindings)
                  (if (consp (car bindings))
                      (cons (car (car bindings)) acc)
                      acc))))
           (find-in-expr
               (e bound acc)
             (cond
              ((symbolp e)
               (if (and
                    (flat-env-lookup e env)
                    (not (in-list e params))
                    (not (in-list e bound))
                    (not (in-list e acc)))
                   (cons e acc)
                   acc))
              ((not (consp e)) acc)
              ((sym-eq (car e) 'quote) acc)
              ((sym-eq (car e) 'lambda)
               (let* ((lambda-params (cadr e))
                      (lambda-body (cddr e))
                      (new-bound
                       (append lambda-params bound)))
                 (find-in-list lambda-body
                  new-bound acc)))
              ((or (sym-eq (car e) 'let) (sym-eq (car e) 'let)
                   (sym-eq (car e) 'let*) (sym-eq (car e) 'let*))
               (let* ((bindings (cadr e))
                      (body (cddr e))
                      (let-vars
                       (get-let-vars bindings nil))
                      (new-bound
                       (append let-vars bound))
                      (acc2
                       (find-in-binding-vals bindings
                        bound acc))
                      (acc3
                       (find-in-list body
                        new-bound acc2)))
                 acc3))
              (t
               (find-in-list (cdr e) bound
                (find-in-expr (car e) bound
                 acc)))))
           (find-in-binding-vals
               (bindings bound acc)
             (if (null bindings)
                 acc
                 (let ((b (car bindings)))
                   (if (and (consp b) (cadr b))
                       (find-in-binding-vals (cdr bindings)
                        bound
                        (find-in-expr (cadr b)
                         bound acc))
                       (find-in-binding-vals (cdr bindings)
                        bound acc)))))
           (find-in-list
               (lst bound acc)
             (if (null lst)
                 acc
                 (find-in-list (cdr lst)
                  bound
                  (find-in-expr (car lst)
                   bound acc)))))
    (reverse (find-in-expr expr nil nil))))

(defun compile-funcall
       (expr env fenv)
  "Compile (funcall fn arg1 arg2 ...)"
  (let ((fn-expr (cadr expr))
        (args (cddr expr)))
    (list 'funcall-ir
          (compile-expr-full fn-expr env
           fenv)
          (compile-args args env
           fenv))))

(defun make-gensym-state ()
  "Create initial gensym state - a cons cell holding (counter . nil)"
  (cons 0 nil))

(defun digit-char (n)
  "Convert digit 0-9 to ASCII character code"
  (+ n 48))

(defun number-to-string (n)
  "Convert positive integer to string - pure Habu"
  (if (= n 0)
      "0"
      (labels ((digits (num acc)
                 (if (= num 0)
                     acc
                     (digits (/ num 10)
                      (cons (digit-char (mod num 10)) acc))))
               (chars-to-vec (chars)
                 (let* ((len (length chars))
                        (vec
                         (make-vector len)))
                   (labels ((fill-vec (cs i)
                              (if (null cs)
                                  vec
                                  (progn
                                   (vector-set vec
                                    i (car cs))
                                   (fill-vec (cdr cs)
                                    (+ i 1))))))
                     (fill-vec chars 0)))))
        (make-string-from-vector
         (chars-to-vec (digits n nil))))))

(defun gensym-next (state)
  "Get and increment gensym counter from state cell"
  (let ((val (+ (car state) 1)))
    (setcar state val)
    val))

(defun gensym (prefix)
  "Generate unique symbol - uses pure string operations"
  (make-symbol-from-string (string-concat prefix "G")))

(defun compile-labels
       (expr env fenv)
  "Compile labels by transforming to let/setq/lambda/funcall with FNTAB"
  (let* ((bindings (cadr expr))
         (body-forms (cddr expr))
         (body
          (if (null (cdr body-forms))
              (car body-forms)
              (cons 'progn body-forms)))
         (fn-names
          (extract-label-names bindings nil))
         (fntab-var (gensym "FNTAB"))
         (transformed
          (transform-labels fn-names bindings
           body fntab-var)))
    (compile-expr-full transformed env
     fenv)))

(defun extract-label-names (bindings acc)
  "Extract function names from labels bindings"
  (if (null bindings)
      (reverse acc)
      (extract-label-names (cdr bindings)
       (cons (car (car bindings)) acc))))

(defun transform-labels
       (fn-names bindings body
        fntab-var)
  "Transform labels to let/setq/funcall with FNTAB"
  (let* ((let-bindings
          (map-nil-bindings fn-names nil))
         (fntab-unpack
          (build-fntab-unpack fn-names
           fntab-var 0 nil))
         (setq-forms
          (build-setq-forms bindings fn-names
           fntab-var fntab-unpack nil))
         (fntab-init
          (build-fntab-init fn-names))
         (rewritten-body
          (rewrite-labels-body body fn-names
           fntab-var))
         (inner-let
          (list 'let (list (list fntab-var fntab-init))
                rewritten-body))
         (full-progn
          (append setq-forms (list inner-let))))
    (list 'let let-bindings (cons 'progn full-progn))))

(defun map-nil-bindings (names acc)
  "Build ((name nil) ...) list"
  (if (null names)
      (reverse acc)
      (map-nil-bindings (cdr names)
       (cons (list (car names) 'nil) acc))))

(defun build-fntab-unpack
       (names fntab-var depth
        acc)
  "Build ((f (car FNTAB)) (g (car (cdr FNTAB))) ...) bindings"
  (if (null names)
      (reverse acc)
      (let ((accessor
             (wrap-cdr-car fntab-var depth)))
        (build-fntab-unpack (cdr names)
         fntab-var (+ depth 1)
         (cons (list (car names) accessor)
               acc)))))

(defun wrap-cdr-car (sb-debug:var depth)
  "Build (car (cdr (cdr ... var))) expression"
  (if (= depth 0)
      (list 'car sb-debug:var)
      (list 'car (wrap-cdr sb-debug:var depth))))

(defun wrap-cdr (sb-debug:var n)
  "Wrap var in n cdrs"
  (if (= n 0)
      sb-debug:var
      (list 'cdr (wrap-cdr sb-debug:var (- n 1)))))

(defun build-setq-forms
       (bindings fn-names fntab-var
        fntab-unpack acc)
  "Build setq forms for each function"
  (if (null bindings)
      (reverse acc)
      (let* ((fn-name (car (car bindings)))
             (params (cadr (car bindings)))
             (forms (cddr (car bindings)))
             (fn-body
              (if (null (cdr forms))
                  (car forms)
                  (cons 'progn forms)))
             (rewritten
              (rewrite-labels-body fn-body
               fn-names fntab-var))
             (setq-form
              (list 'setq fn-name
                    (list 'lambda (cons fntab-var params)
                          (list 'let fntab-unpack rewritten)))))
        (build-setq-forms (cdr bindings)
         fn-names fntab-var fntab-unpack
         (cons setq-form acc)))))

(defun build-fntab-init (names)
  "Build (cons f (cons g nil)) expression"
  (if (null names)
      'nil
      (list 'cons (car names)
            (build-fntab-init (cdr names)))))

(defun rewrite-labels-body
       (expr fn-names fntab-var)
  "Rewrite calls to labels functions to pass FNTAB"
  (cond ((null expr) nil)
        ((numberp expr) expr)
        ((symbolp expr) expr)
        ((not (consp expr)) expr)
        ((and (symbolp (car expr))
              (member (car expr) fn-names))
         (cons 'funcall
               (cons (car expr)
                     (cons fntab-var
                           (rewrite-args (cdr expr)
                            fn-names fntab-var)))))
        ((sym-eq (car expr) 'quote) expr)
        ((sym-eq (car expr) 'lambda)
         (list 'lambda (cadr expr)
               (rewrite-labels-body (caddr expr)
                fn-names fntab-var)))
        ((or (sym-eq (car expr) 'let) (sym-eq (car expr) 'let)
             (sym-eq (car expr) 'let*) (sym-eq (car expr) 'let*))
         (let* ((bindings (cadr expr))
                (body-forms (cddr expr))
                (new-bindings
                 (rewrite-let-bindings bindings
                  fn-names fntab-var)))
           (cons (car expr)
                 (cons new-bindings
                       (rewrite-args body-forms
                        fn-names fntab-var)))))
        (t
         (rewrite-args expr fn-names
          fntab-var))))

(defun rewrite-args
       (args fn-names fntab-var)
  "Rewrite list of arguments"
  (if (null args)
      nil
      (cons
       (rewrite-labels-body (car args)
        fn-names fntab-var)
       (rewrite-args (cdr args) fn-names
        fntab-var))))

(defun rewrite-let-bindings
       (bindings fn-names fntab-var)
  "Rewrite let binding values"
  (if (null bindings)
      nil
      (let ((b (car bindings)))
        (if (consp b)
            (cons
             (list (car b)
                   (rewrite-labels-body (cadr b)
                    fn-names fntab-var))
             (rewrite-let-bindings (cdr bindings)
              fn-names fntab-var))
            (cons b
                  (rewrite-let-bindings (cdr bindings)
                   fn-names fntab-var))))))

(defun member (x lst)
  "Check if x is in lst"
  (if (null lst)
      nil
      (if (sym-eq x (car lst))
          t
          (member x (cdr lst)))))

(defun extend-fenv (names fenv)
  "Extend function environment with names"
  (if (null names)
      fenv
      (extend-fenv (cdr names)
       (cons (list (car names)) fenv))))

(defun compile-expr-full
       (expr env fenv)
  "Full expression compiler with function support"
  (cond ((numberp expr) (compile-lit expr))
        ((stringp expr) (list 'str-lit expr))
        ((symbolp expr)
         (if (sym-eq expr 'nil)
             (list 'nil-ir)
             (if (sym-eq expr 't)
                 (list 'sym-lit "T")
                 (compile-var expr env))))
        ((not (consp expr)) (compile-lit 0))
        (t
         (cond
          ((sym-eq (car expr) 'if)
           (compile-if-full expr env
            fenv))
          ((sym-eq (car expr) 'cond)
           (compile-cond expr env
            fenv))
          ((sym-eq (car expr) 'when)
           (compile-when expr env
            fenv))
          ((sym-eq (car expr) 'unless)
           (compile-unless expr env
            fenv))
          ((sym-eq (car expr) 'while)
           (compile-while expr env
            fenv))
          ((sym-eq (car expr) 'and)
           (compile-and expr env
            fenv))
          ((sym-eq (car expr) 'or)
           (compile-or expr env
            fenv))
          ((sym-eq (car expr) 'not)
           (list 'cmp-eq
                 (compile-expr-full (cadr expr)
                  env fenv)
                 (list 'nil-ir)))
          ((sym-eq (car expr) 'let)
           (compile-let-full expr env
            fenv))
          ((sym-eq (car expr) 'let*)
           (compile-let*-full expr env
            fenv))
          ((sym-eq (car expr) 'progn)
           (compile-progn-full expr env
            fenv))
          ((sym-eq (car expr) 'quote)
           (compile-quote expr))
          ((sym-eq (car expr) 'lambda)
           (compile-lambda expr env
            fenv))
          ((sym-eq (car expr) 'funcall)
           (compile-funcall expr env
            fenv))
          ((sym-eq (car expr) 'labels)
           (compile-labels expr env
            fenv))
          ((sym-eq (car expr) 'function)
           (let ((name (cadr expr)))
             (if (fenv-lookup name fenv)
                 (list 'fn-ref-ir name)
                 (compile-var name env))))
          ((sym-eq (car expr) '+)
           (fold-binop 'add (cdr expr)
            env fenv))
          ((sym-eq (car expr) '-)
           (fold-binop 'sub (cdr expr)
            env fenv))
          ((sym-eq (car expr) '*)
           (fold-binop 'mul (cdr expr)
            env fenv))
          ((sym-eq (car expr) '/)
           (fold-binop 'div (cdr expr)
            env fenv))
          ((sym-eq (car expr) 'mod)
           (list 'mod
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '=)
           (list 'cmp-eq
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '<)
           (list 'cmp-lt
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '>)
           (list 'cmp-gt
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '<=)
           (list 'cmp-le
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '>=)
           (list 'cmp-ge
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) '/=)
           (compile-expr-full
            (list 'not (list '= (nth 1 expr) (nth 2 expr)))
            env fenv))
          ((sym-eq (car expr) 'logand)
           (list 'band
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'logior)
           (list 'bor
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'logxor)
           (list 'bxor
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'ash)
           (list 'bsh
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'cons)
           (list 'cons-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'car)
           (list 'car-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'cdr)
           (list 'cdr-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'cadr)
           (list 'car-ir
                 (list 'cdr-ir
                       (compile-expr-full (nth 1 expr)
                        env fenv))))
          ((sym-eq (car expr) 'caddr)
           (list 'car-ir
                 (list 'cdr-ir
                       (list 'cdr-ir
                             (compile-expr-full (nth 1 expr)
                              env fenv)))))
          ((sym-eq (car expr) 'cddr)
           (list 'cdr-ir
                 (list 'cdr-ir
                       (compile-expr-full (nth 1 expr)
                        env fenv))))
          ((sym-eq (car expr) 'cdddr)
           (list 'cdr-ir
                 (list 'cdr-ir
                       (list 'cdr-ir
                             (compile-expr-full (nth 1 expr)
                              env fenv)))))
          ((sym-eq (car expr) 'cadddr)
           (list 'car-ir
                 (list 'cdr-ir
                       (list 'cdr-ir
                             (list 'cdr-ir
                                   (compile-expr-full
                                    (nth 1 expr) env
                                    fenv))))))
          ((sym-eq (car expr) 'nth)
           (compile-nth expr env
            fenv))
          ((sym-eq (car expr) 'list)
           (compile-list-full expr env
            fenv))
          ((sym-eq (car expr) 'null)
           (list 'cmp-eq
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (list 'nil-ir)))
          ((sym-eq (car expr) 'consp)
           (list 'cmp-eq
                 (list 'get-tag
                       (compile-expr-full (nth 1 expr)
                        env fenv))
                 (list 'lit 1)))
          ((sym-eq (car expr) 'numberp)
           (list 'cmp-eq
                 (list 'get-tag
                       (compile-expr-full (nth 1 expr)
                        env fenv))
                 (list 'lit 0)))
          ((sym-eq (car expr) 'symbolp)
           (list 'cmp-eq
                 (list 'get-tag
                       (compile-expr-full (nth 1 expr)
                        env fenv))
                 (list 'lit 2)))
          ((sym-eq (car expr) 'stringp)
           (list 'cmp-eq
                 (list 'get-tag
                       (compile-expr-full (nth 1 expr)
                        env fenv))
                 (list 'lit 4)))
          ((sym-eq (car expr) 'vectorp)
           (list 'cmp-eq
                 (list 'get-tag
                       (compile-expr-full (nth 1 expr)
                        env fenv))
                 (list 'lit 3)))
          ((sym-eq (car expr) 'eq)
           (list 'cmp-eq
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'length)
           (let ((len-iter-fn (gensym "LEN-ITER"))
                 (lst-var (gensym "LST"))
                 (acc-var (gensym "ACC")))
             (compile-expr-full
              (list 'labels
                    (list
                     (list len-iter-fn
                           (list lst-var acc-var)
                           (list 'if (list 'null lst-var)
                                 acc-var
                                 (list len-iter-fn
                                       (list 'cdr lst-var)
                                       (list '+ acc-var 1)))))
                    (list len-iter-fn (nth 1 expr) 0))
              env fenv)))
          ((sym-eq (car expr) 'reverse)
           (let ((rev-iter-fn (gensym "REV-ITER"))
                 (lst-var (gensym "LST"))
                 (acc-var (gensym "ACC"))
                 (next-acc-var (gensym "NEXT-ACC")))
             (compile-expr-full
              (list 'labels
                    (list
                     (list rev-iter-fn
                           (list lst-var acc-var)
                           (list 'if (list 'null lst-var)
                                 acc-var
                                 (list 'let
                                       (list
                                        (list next-acc-var
                                              (list 'cons (list 'car lst-var)
                                                    acc-var)))
                                       (list rev-iter-fn
                                             (list 'cdr lst-var)
                                             next-acc-var)))))
                    (list rev-iter-fn (nth 1 expr) nil))
              env fenv)))
          ((sym-eq (car expr) 'string-length)
           (list 'string-length-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'string-ref)
           (list 'string-ref-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'char-at)
           (let ((str-sym (gensym "STR"))
                 (pos-sym (gensym "POS")))
             (compile-expr-full
              (list 'let
                    (list (list str-sym (nth 1 expr))
                          (list pos-sym (nth 2 expr)))
                    (list 'if
                          (list '>= pos-sym
                                (list 'string-length str-sym))
                          0
                          (list 'string-ref str-sym
                                pos-sym)))
              env fenv)))
          ((or (sym-eq (car expr) 'string-concat)
               (sym-eq (car expr) 'string-concat))
           (list 'string-concat-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'char-code)
           (compile-expr-full (nth 1 expr)
            env fenv))
          ((sym-eq (car expr) 'make-vector)
           (list 'make-vector-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-ref)
           (list 'vector-ref-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-set)
           (list 'vector-set-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-length)
           (list 'vector-length-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'make-string-from-vector)
           (list 'make-string-from-vector-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'setq)
           (compile-setq expr env
            fenv))
          ((sym-eq (car expr) 'setcar)
           (list 'setcar-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'setcdr)
           (list 'setcdr-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'symbol-name)
           (list 'symbol-name-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'make-symbol-from-string)
           (list 'make-symbol-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-exit)
           (list 'sys-exit-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'get-cmdline-args)
           (list 'get-cmdline-args-ir))
          ((sym-eq (car expr) 'get-intern-table)
           (list 'get-intern-table-ir))
          ((sym-eq (car expr) 'set-intern-table)
           (list 'set-intern-table-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'get-lambda-counter)
           (list 'get-lambda-counter-ir))
          ((sym-eq (car expr) 'set-lambda-counter)
           (list 'set-lambda-counter-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-open)
           (list 'sys-open-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-read)
           (list 'sys-read-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-write)
           (list 'sys-write-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-write-char)
           (list 'sys-write-char-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-read-byte)
           (list 'sys-read-byte-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'sys-close)
           (list 'sys-close-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'make-vector)
           (list 'make-vector-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-ref)
           (list 'vector-ref-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-set)
           (list 'vector-set-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'vector-length)
           (list 'vector-length-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'buffer-to-string)
           (list 'buffer-to-string-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'buffer-byte-ref)
           (list 'buffer-byte-ref-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'buffer-byte-set)
           (list 'buffer-byte-set-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'jit-mmap)
           (list 'mmap-jit-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'jit-write-protect)
           (list 'pthread-jit-write-protect-np-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)))
          ((sym-eq (car expr) 'jit-dcache-flush)
           (list 'sys-dcache-flush-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'jit-icache-invalidate)
           (list 'sys-icache-invalidate-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'jit-call)
           (list 'funcall-ptr-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-args (cddr expr)
                  env fenv)))
          ((sym-eq (car expr) 'mem-set-byte)
           (list 'mem-set-byte-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)
                 (compile-expr-full (nth 3 expr)
                  env fenv)))
          ((sym-eq (car expr) 'mem-load-64)
           (list 'mem-load-64-ir
                 (compile-expr-full (nth 1 expr)
                  env fenv)
                 (compile-expr-full (nth 2 expr)
                  env fenv)))
          ((sym-eq (car expr) 'native-read-file)
           (let ((path-sym (gensym "PATH"))
                 (fd-sym (gensym "FD"))
                 (buf-sym (gensym "BUF"))
                 (n-sym (gensym "N")))
             (compile-expr-full
              (list 'let*
                    (list (list path-sym (nth 1 expr))
                          (list fd-sym
                                (list 'sys-open path-sym 0 0))
                          (list buf-sym
                                (list 'make-vector 524288))
                          (list n-sym
                                (list 'sys-read fd-sym
                                      buf-sym 524288)))
                    (list 'progn (list 'sys-close fd-sym)
                          (list 'buffer-to-string buf-sym
                                n-sym)))
              env fenv)))
          ((sym-eq (car expr) 'native-read-file-large)
           (let ((path-sym (gensym "PATH"))
                 (fd-sym (gensym "FD"))
                 (buf-sym (gensym "BUF"))
                 (chunks-sym (gensym "CHUNKS"))
                 (total-sym (gensym "TOTAL"))
                 (n-sym (gensym "N")))
             (compile-expr-full
              (list 'let*
                    (list (list path-sym (nth 1 expr))
                          (list fd-sym
                                (list 'sys-open path-sym 0 0))
                          (list buf-sym
                                (list 'make-vector 4096))
                          (list chunks-sym nil)
                          (list total-sym 0) (list n-sym 0))
                    (list 'progn
                          (list 'while
                                (list 'progn
                                      (list 'setq n-sym
                                            (list 'sys-read
                                                  fd-sym
                                                  buf-sym 4096))
                                      (list '> n-sym 0))
                                (list 'setq chunks-sym
                                      (list 'cons
                                            (list 'buffer-to-string
                                                  buf-sym
                                                  n-sym)
                                            chunks-sym))
                                (list 'setq total-sym
                                      (list '+ total-sym
                                            n-sym)))
                          (list 'sys-close fd-sym)
                          (list 'concat-string-list-iter
                                chunks-sym total-sym)))
              env fenv)))
          ((sym-eq (car expr) 'concat-string-list-iter)
           (let* ((chunks-var (gensym "CHUNKS"))
                  (total-var (gensym "TOTAL"))
                  (vec-var (gensym "VEC"))
                  (rev-chunks-var (gensym "REV-CHUNKS"))
                  (offset-var (gensym "OFFSET"))
                  (chunk-var (gensym "CHUNK"))
                  (len-var (gensym "LEN"))
                  (i-var (gensym "I")))
             (compile-expr-full
              (list 'let*
                    (list (list chunks-var (nth 1 expr))
                          (list total-var (nth 2 expr))
                          (list vec-var
                                (list 'make-vector total-var))
                          (list rev-chunks-var
                                (list 'reverse chunks-var))
                          (list offset-var 0))
                    (list 'progn
                          (list 'while rev-chunks-var
                                (list 'let*
                                      (list
                                       (list chunk-var
                                             (list 'car rev-chunks-var))
                                       (list len-var
                                             (list 'string-length
                                                   chunk-var))
                                       (list i-var 0))
                                      (list 'while
                                            (list '< i-var
                                                  len-var)
                                            (list 'vector-set
                                                  vec-var
                                                  (list '+ offset-var
                                                        i-var)
                                                  (list 'string-ref
                                                        chunk-var
                                                        i-var))
                                            (list 'setq i-var
                                                  (list '+ i-var 1)))
                                      (list 'setq offset-var
                                            (list '+ offset-var
                                                  len-var))
                                      (list 'setq rev-chunks-var
                                            (list 'cdr rev-chunks-var))))
                          (list 'make-string-from-vector
                                vec-var)))
              env fenv)))
          ((sym-eq (car expr) 'match)
           (compile-match expr env
            fenv))
          (t
           (cond
            ((symbolp (car expr))
             (compile-call expr env
              fenv))
            ((and (consp (car expr))
                  (sym-eq (car (car expr)) 'lambda))
             (list 'funcall-ir
                   (compile-lambda (car expr)
                    env fenv)
                   (compile-args (cdr expr)
                    env fenv)))
            (t (compile-lit 0))))))))

(defun compile-match
       (expr env fenv)
  "Compile (match scrutinee (pattern body...)...) to IR.
   Uses expand-match from expand.lisp for source-to-source transformation."
  (compile-expr-full
   (expand-match (cadr expr) (cddr expr))
   env fenv))

(defun compile-if-full
       (expr env fenv)
  (let ((test
         (compile-expr-full (nth 1 expr) env
          fenv))
        (then
         (compile-expr-full (nth 2 expr) env
          fenv))
        (else
         (if (nth 3 expr)
             (compile-expr-full (nth 3 expr)
              env fenv)
             (list 'nil-ir))))
    (list 'if-ir test then
          else)))

(defun compile-cond
       (expr env fenv)
  "Compile (cond ...) using expand-cond."
  (compile-expr-full (expand-cond (cdr expr))
   env fenv))

(defun compile-when
       (expr env fenv)
  "Compile (when test body...) to (if test (progn body...) nil)."
  (let ((test (cadr expr))
        (body (cddr expr)))
    (compile-expr-full
     (list 'if test (cons 'progn body) nil)
     env fenv)))

(defun compile-unless
       (expr env fenv)
  "Compile (unless test body...) to (if test nil (progn body...))."
  (let ((test (cadr expr))
        (body (cddr expr)))
    (compile-expr-full
     (list 'if test nil (cons 'progn body))
     env fenv)))

(defun compile-while
       (expr env fenv)
  "Compile (while test body...) - true iteration with no stack growth"
  (let ((test
         (compile-expr-full (nth 1 expr) env
          fenv))
        (body
         (compile-progn-full (cons 'progn (cddr expr))
          env fenv)))
    (list 'while-ir test body)))

(defun compile-nth
       (expr env fenv)
  "Compile (nth n list) - optimize for constant indices"
  (let ((index-expr (nth 1 expr))
        (list-expr (nth 2 expr)))
    (if (numberp index-expr)
        (let ((list-ir
               (compile-expr-full list-expr
                env fenv)))
          (nth-expand index-expr list-ir))
        (compile-expr-full
         (list 'labels
               (list
                (list 'nth-loop
                      (list 'n 'lst)
                      (list 'if (list '= 'n 0) (list 'car 'lst)
                            (list 'nth-loop (list '- 'n 1)
                                  (list 'cdr 'lst)))))
               (list 'nth-loop index-expr
                     list-expr))
         env fenv))))

(defun nth-expand (n list-ir)
  "Expand (nth n list-ir) to nested car/cdr for constant n"
  (if (= n 0)
      (list 'car-ir list-ir)
      (nth-expand (- n 1)
       (list 'cdr-ir list-ir))))

(defun compile-and
       (expr env fenv)
  "Compile (and ...) using expand-and."
  (compile-expr-full (expand-and (cdr expr))
   env fenv))

(defun compile-or
       (expr env fenv)
  "Compile (or ...) using expand-or. Properly avoids double evaluation."
  (compile-expr-full (expand-or (cdr expr))
   env fenv))

(defun compile-let-full
       (expr env fenv)
  "Compile (let ((var val) ...) body ...) to (let-ir vals body count offs)"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (labels ((extract-vars (binds acc)
               (if (null binds)
                   (reverse acc)
                   (extract-vars (cdr binds)
                    (cons (car (car binds)) acc))))
             (compile-vals (binds acc)
               (if (null binds)
                   (reverse acc)
                   (compile-vals (cdr binds)
                    (cons
                     (compile-expr-full (nth 1 (car binds))
                      env fenv)
                     acc))))
             (make-offs
                 (n base acc)
               (if (= n 0)
                   (reverse acc)
                   (make-offs (- n 1)
                    (+ base 1)
                    (cons base acc)))))
      (let* ((vars
              (extract-vars bindings nil))
             (val-irs
              (compile-vals bindings nil))
             (base-offset (length env))
             (offs
              (make-offs (length bindings)
               base-offset nil))
             (new-env
              (extend-env vars env))
             (body
              (if (null (cdr body-forms))
                  (car body-forms)
                  (cons 'progn body-forms)))
             (body-ir
              (compile-expr-full body new-env
               fenv)))
        (list 'let-ir val-irs body-ir
              (length bindings) offs)))))

(defun compile-let*-full
       (expr env fenv)
  "Compile (let* ...) using expand-let* to nested let forms."
  (compile-expr-full
   (expand-let* (nth 1 expr) (cddr expr))
   env fenv))

(defun compile-progn-full
       (expr env fenv)
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (reverse acc)
                 (compile-exprs (cdr exprs)
                  (cons
                   (compile-expr-full (car exprs)
                    env fenv)
                   acc)))))
    (list 'progn-ir
          (compile-exprs (cdr expr) nil))))

(defun compile-list-full
       (expr env fenv)
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'nil-ir)
                 (list 'cons-ir
                       (compile-expr-full (car elems)
                        env fenv)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))

(defun compile-setq
       (expr env fenv)
  "Compile (setq var val)"
  (let ((sb-debug:var (nth 1 expr))
        (val (nth 2 expr)))
    (let ((offset
           (flat-env-lookup sb-debug:var env)))
      (if offset
          (list 'setq-ir offset
                (compile-expr-full val env
                 fenv))
          (list 'nil-ir)))))

(defun src-inline-expr (expr fenv)
  "Inline small functions in source expression EXPR.
   FENV is alist of (name params body) for all defuns."
  (cond ((null expr) nil)
        ((not (consp expr)) expr)
        ((sym-eq (car expr) 'quote) expr)
        ((and (symbolp (car expr))
              (not (src-special-form? (car expr))))
         (let ((fn-info
                (src-fn-lookup (car expr)
                 fenv)))
           (if (and fn-info
                    (src-inlinable? fn-info))
               (let* ((params (cadr fn-info))
                      (body (caddr fn-info))
                      (args (cdr expr)))
                 (if (= (length params) (length args))
                     (src-inline-expr
                      (src-subst body params
                       args)
                      fenv)
                     (cons (car expr)
                           (src-inline-args (cdr expr)
                            fenv))))
               (cons (car expr)
                     (src-inline-args (cdr expr)
                      fenv)))))
        ((sym-eq (car expr) 'if)
         (list 'if
               (src-inline-expr (cadr expr)
                fenv)
               (src-inline-expr (caddr expr)
                fenv)
               (if (cadddr expr)
                   (src-inline-expr (cadddr expr)
                    fenv)
                   nil)))
        ((sym-eq (car expr) 'progn)
         (cons 'progn
               (src-inline-args (cdr expr)
                fenv)))
        ((or (sym-eq (car expr) 'let) (sym-eq (car expr) 'let*))
         (list (car expr)
               (src-inline-bindings (cadr expr)
                fenv)
               (src-inline-expr (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'lambda)
         (list 'lambda (cadr expr)
               (src-inline-expr (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'labels) expr)
        ((sym-eq (car expr) 'cond)
         (cons 'cond
               (mapcar
                (lambda (clause)
                  (src-inline-args clause
                   fenv))
                (cdr expr))))
        ((or (sym-eq (car expr) 'when) (sym-eq (car expr) 'unless))
         (cons (car expr)
               (src-inline-args (cdr expr)
                fenv)))
        ((or (sym-eq (car expr) 'and) (sym-eq (car expr) 'or))
         (cons (car expr)
               (src-inline-args (cdr expr)
                fenv)))
        ((sym-eq (car expr) 'setq)
         (list 'setq (cadr expr)
               (src-inline-expr (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'while)
         (cons 'while
               (src-inline-args (cdr expr)
                fenv)))
        ((sym-eq (car expr) 'function) expr)
        ((sym-eq (car expr) 'funcall)
         (cons 'funcall
               (src-inline-args (cdr expr)
                fenv)))
        (t
         (cons
          (src-inline-expr (car expr) fenv)
          (src-inline-expr (cdr expr)
           fenv)))))

(defun src-inline-args (args fenv)
  "Inline into a list of arguments"
  (if (null args)
      nil
      (cons (src-inline-expr (car args) fenv)
            (src-inline-args (cdr args)
             fenv))))

(defun src-inline-bindings (bindings fenv)
  "Inline into let/let* bindings"
  (if (null bindings)
      nil
      (let ((b (car bindings)))
        (cons
         (list (car b)
               (src-inline-expr (cadr b)
                fenv))
         (src-inline-bindings (cdr bindings)
          fenv)))))

(defun src-special-form? (sym)
  "Check if symbol is a special form"
  (let ((specials
         '(quote if progn let let* lambda labels cond when unless and or setq defun
           while function funcall)))
    (if (null specials)
        nil
        (src-member? sym specials))))

(defun src-member? (x lst)
  "Check if x is in lst"
  (cond ((null lst) nil) ((sym-eq x (car lst)) t)
        (t (src-member? x (cdr lst)))))

(defun src-fn-lookup (name fenv)
  "Look up function in fenv"
  (cond ((null fenv) nil)
        ((sym-eq name (car (car fenv)))
         (car fenv))
        (t (src-fn-lookup name (cdr fenv)))))

(defun src-inlinable? (fn-info)
  "Check if function should be inlined.
   Inline if: small body, not recursive, few params."
  (let ((name (car fn-info))
        (params (cadr fn-info))
        (body (caddr fn-info)))
    (and (<= (src-size body) 20)
         (not (src-calls? body name))
         (<= (length params) 4))))

(defun src-size (expr)
  "Estimate size of source expression"
  (cond ((null expr) 1) ((not (consp expr)) 1)
        ((sym-eq (car expr) 'quote) 1)
        ((or (sym-eq (car expr) 'progn) (sym-eq (car expr) 'and)
             (sym-eq (car expr) 'or))
         (let ((sum 1) (es (cdr expr)))
           (while (not (null es))
            (setq sum
                    (+ sum
                       (src-size (car es))))
            (setq es (cdr es)))
           sum))
        ((sym-eq (car expr) 'if)
         (+ 1 (src-size (cadr expr))
            (src-size (caddr expr))
            (if (cadddr expr)
                (src-size (cadddr expr))
                0)))
        ((or (sym-eq (car expr) 'let) (sym-eq (car expr) 'let*))
         (+ 2 (src-size (caddr expr))))
        (t (+ 1 (length (cdr expr))))))

(defun src-calls? (expr fn-name)
  "Check if expression contains a call to fn-name"
  (cond ((null expr) nil) ((not (consp expr)) nil)
        ((sym-eq (car expr) 'quote) nil)
        ((and (symbolp (car expr))
              (sym-eq (car expr) fn-name))
         t)
        (t
         (or (src-calls? (car expr) fn-name)
             (src-calls? (cdr expr)
              fn-name)))))

(defun src-subst
       (expr params args)
  "Substitute params with args in expression"
  (cond ((null expr) nil)
        ((symbolp expr)
         (let ((pos
                (src-param-pos expr params
                 0)))
           (if pos
               (nth pos args)
               expr)))
        ((not (consp expr)) expr)
        ((sym-eq (car expr) 'quote) expr)
        (t
         (cons
          (src-subst (car expr) params
           args)
          (src-subst (cdr expr) params
           args)))))

(defun src-param-pos
       (name params idx)
  "Find position of name in params"
  (cond ((null params) nil)
        ((sym-eq name (car params)) idx)
        (t
         (src-param-pos name (cdr params)
          (+ idx 1)))))

(defun src-inline-defuns (forms fenv)
  "Apply source inlining to all defun bodies"
  (if (null forms)
      nil
      (let ((f (car forms)))
        (cons
         (if (and (consp f) (sym-eq (car f) 'defun))
             (let* ((name (cadr f))
                    (params (caddr f))
                    (body-forms (cdddr f)))
               (list* 'defun name params
                      (src-inline-args body-forms
                       fenv)))
             (src-inline-expr f fenv))
         (src-inline-defuns (cdr forms)
          fenv)))))

(defun src-inline-all (forms)
  "Apply source-level inlining to all forms.
   First collects all defuns, then inlines into all bodies."
  (let ((fenv (collect-defuns forms nil)))
    (src-inline-defuns forms fenv)))

(defun compile-forms (forms)
  "Compile forms to (defun-list main-ir) - proper list like main compiler.
   Applies TCO (tail-call optimization) to all defuns as a nanopass.
   Note: Source-level inlining disabled - causes stack overflow in compiled reader."
  (let* ((fenv (collect-defuns forms nil))
         (defuns-raw
          (compile-all-defuns forms nil fenv
           nil))
         (defuns
          (apply-tco-to-all-functions defuns-raw))
         (main-form
          (find-main-form forms nil))
         (main-ir
          (compile-expr-full main-form nil
           fenv)))
    (list defuns main-ir)))

(defun concat5
       (a b c d
        e)
  "Concatenate 5 strings using iterative method (avoids broken string-concat)"
  (let ((total
         (+ (string-length a)
            (+ (string-length b)
               (+ (string-length c)
                  (+ (string-length d)
                     (string-length e)))))))
    (concat-string-list-iter
     (list e d c b
           a)
     total)))

(defun concat8
       (a b c d
        e f g h)
  "Concatenate 8 strings using iterative method"
  (let ((total
         (+ (string-length a)
            (+ (string-length b)
               (+ (string-length c)
                  (+ (string-length d)
                     (+ (string-length e)
                        (+ (string-length f)
                           (+ (string-length g)
                              (string-length h))))))))))
    (concat-string-list-iter
     (list h g f e
           d c b a)
     total)))

(defun self-compile (source-path output-path)
  "Pure Habu self-hosting compiler entry point (native version).
   Reads all source files, concatenates them, compiles to native executable.
   source-path is ignored - we read the bootstrap paths relative to cwd.
   Uses native-read-file-large to handle files >65KB (each file can be up to 100KB).
   Now includes arm64/asm.lisp and gc.lisp for full self-hosting."
  (let* ((a
          (native-read-file-large "arm64/asm.lisp"))
         (gc-src
          (native-read-file-large "bootstrap/gc.lisp"))
         (r
          (native-read-file-large "bootstrap/reader.lisp"))
         (c
          (native-read-file-large "bootstrap/compiler.lisp"))
         (o
          (native-read-file-large "bootstrap/optimize.lisp"))
         (g
          (native-read-file-large "bootstrap/codegen.lisp"))
         (m
          (native-read-file-large "bootstrap/macho-utils.lisp")))
    (if (and a gc-src r c
             o g m)
        (let ((source
               (concat8 a gc-src r
                c o g m
                "(sys-exit 42)")))
          (deliver source output-path)
          (sys-exit 0))
        (sys-exit 1))))

(defun compile-program (forms)
  "Compile forms to complete ARM64 bytecode with function linking.
   This is the full pipeline: parse -> IR -> lift-lambdas -> codegen -> link.
   Returns flat bytecode ready for Mach-O wrapping.
   Native version - does not call reset-symbol-table.
   Uses codegen.lisp API: lift-lambdas takes (ir lambdas), returns (ir . lambdas)"
  (let* ((r (compile-forms forms))
         (defun-fns (car r))
         (mir-raw (cadr r)))
    (let* ((mvb-result
            (lift-lambdas mir-raw nil))
           (mir (car mvb-result))
           (main-lambdas (cdr mvb-result)))
      (let* ((mvb-result2
              (lift-lambdas-from-defuns defun-fns nil
               main-lambdas))
             (lifted-defuns (car mvb-result2))
             (defun-lambdas (cdr mvb-result2))
             (lambda-defuns
              (lambdas-to-defuns defun-lambdas nil))
             (fns
              (append lifted-defuns lambda-defuns)))
        (if (null fns)
            (resolve-calls
             (codegen-main mir nil) nil)
            (let* ((main-code-temp
                    (append (prologue)
                            (codegen mir nil nil 0)
                            (epilogue)))
                   (main-size
                    (code-size main-code-temp))
                   (fnoffs
                    (build-fnoffs fns
                     main-size))
                   (main-code
                    (append (prologue)
                            (codegen mir nil
                             fnoffs 0)
                            (epilogue)))
                   (fn-code
                    (codegen-all-fns fns nil
                     fnoffs nil))
                   (all-code
                    (append main-code fn-code)))
              (resolve-calls all-code
               fnoffs)))))))

(defun collect-extern-calls (code)
  "Collect extern call markers from code. Returns ((name . pos) ...)"
  (labels ((collect (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item)
                            (sym-eq (car item) :extern-call))
                       (collect (cdr items)
                        (cons (cons (cadr item) (caddr item))
                              acc))
                       (collect (cdr items)
                        acc))))))
    (collect code nil)))

(defun get-unique-imports (extern-calls)
  "Get unique import names from extern calls list"
  (labels ((unique
               (calls seen acc)
             (if (null calls)
                 (reverse acc)
                 (let ((name (car (car calls))))
                   (if (member name seen)
                       (unique (cdr calls)
                        seen acc)
                       (unique (cdr calls)
                        (cons name seen)
                        (cons name acc)))))))
    (unique extern-calls nil nil)))

(defun string= (s1 s2)
  "Compare two strings for equality - use pure implementation"
  (string-equal s1 s2))

(defun assoc-string (key alist)
  "Find entry in alist with string key"
  (if (null alist)
      nil
      (if (string= key (car (car alist)))
          (car alist)
          (assoc-string key (cdr alist)))))

(defun flatten-extern-calls
       (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using assoc list.
   Returns (flat-code . extern-positions)
   Note: resolve-calls emits markers followed by 3 zeros - must skip them.
   Native Habu version - SBCL uses hash-table version in compiler-sbcl.lisp."
  (labels ((flatten
               (items result positions
                skip-count)
             (cond
              ((null items)
               (cons (reverse result) (reverse positions)))
              ((> skip-count 0)
               (flatten (cdr items) result
                positions (- skip-count 1)))
              ((and (consp (car items))
                    (sym-eq (car (car items)) :extern-call))
               (let* ((item (car items))
                      (name (cadr item))
                      (pos (caddr item))
                      (bl-addr
                       (+ code-base-addr pos))
                      (entry
                       (assoc-string name
                        stub-alist))
                      (stub-addr
                       (if entry
                           (cdr entry)
                           0))
                      (rel-offset
                       (- stub-addr bl-addr))
                      (off-s (ash rel-offset -2))
                      (off-m (logand off-s 67108863))
                      (bl-instr (logior 2483027968 off-m))
                      (b0 (logand bl-instr 255))
                      (b1 (logand (ash bl-instr -8) 255))
                      (b2 (logand (ash bl-instr -16) 255))
                      (b3 (logand (ash bl-instr -24) 255)))
                 (flatten (cdr items)
                  (cons b3
                        (cons b2
                              (cons b1
                                    (cons b0 result))))
                  (cons (cons name pos)
                        positions)
                  3)))
              (t
               (flatten (cdr items)
                (cons (car items) result)
                positions 0)))))
    (flatten code nil nil 0)))

(defun build-stub-alist
       (imports stubs-offset stub-size)
  "Build ((name . offset) ...) alist for stub map"
  (labels ((build
               (remaining i acc)
             (if (null remaining)
                 (reverse acc)
                 (build (cdr remaining)
                  (+ i 1)
                  (cons
                   (cons (car remaining)
                         (+ stubs-offset
                            (* i stub-size)))
                   acc)))))
    (build imports 0 nil)))

(defun is-extern-marker (x)
  "Check if x is an extern-call marker"
  (and (consp x) (sym-eq (car x) :extern-call)))

(defun deliver (source output-path)
  "Compile source string to native executable using pure compiler.
   This uses the full extern-call flattening pipeline.
   Uses only pure functions - no hash tables or CL runtime.
   Works in both SBCL and native Habu environments."
  (let* ((forms (read-all source))
         (bytes-with-markers
          (compile-program forms))
         (extern-calls
          (collect-extern-calls bytes-with-markers))
         (imports
          (get-unique-imports extern-calls))
         (wrapper-size 120))
    (let ((imports
           (if (null imports)
               '("_exit")
               imports)))
      (let* ((num-imports (length imports))
             (stubs-total
              (if (> num-imports 0)
                  (* num-imports 12)
                  0))
             (code-offset 1024)
             (exact-flat-size (length bytes-with-markers))
             (exact-code-size
              (+ exact-flat-size wrapper-size))
             (stubs-offset
              (+ code-offset exact-code-size))
             (stub-size 12))
        (let* ((stub-alist
                (build-stub-alist imports
                 stubs-offset stub-size))
               (flatten-result
                (flatten-extern-calls bytes-with-markers
                 stub-alist
                 (+ code-offset wrapper-size)))
               (flat-code (car flatten-result)))
          (let* ((total-size
                  (+ (length flat-code) wrapper-size))
                 (stubs-end
                  (+ code-offset total-size
                     stubs-total))
                 (text-vmsize
                  (* (ceiling stubs-end 16384) 16384))
                 (text-pages-4kb (/ text-vmsize 4096))
                 (data-const-pages-4kb (/ 16384 4096))
                 (heap-page-offset
                  (+ text-pages-4kb data-const-pages-4kb))
                 (wrapped-code
                  (wrap-bytecode-with-heap-for-imports
                   flat-code heap-page-offset)))
            (write-macho-executable-with-imports-and-heap
             output-path wrapped-code imports
             8388608)))))))

(defun main ()
  "Entry point for Stage 1 compiler - test compile-forms."
  ;; Step 7: test just accessing a quoted list
  (sys-exit (car '(5 6 7))))

(in-package :habu)

(defvar *optimization-passes*
  nil
  "List of (name . function) pairs for optimization passes")

(defun register-optimization (name function)
  "Register an optimization pass"
  (push (cons name function) *optimization-passes*))

(defun run-optimization (name ir)
  "Run a single optimization pass (native - no stats)"
  (let ((pass
         (assoc name *optimization-passes*)))
    (if pass
        (funcall (cdr pass) ir)
        ir)))

(defun run-all-optimizations (ir)
  "Run all registered optimization passes in order (native)"
  (let ((result ir)
        (passes (reverse *optimization-passes*)))
    (while passes
     (setq result
             (run-optimization (car (car passes))
              result))
     (setq passes (cdr passes)))
    result))

(defun fold-constants (ir)
  "Fold constant expressions at compile time.
   Transforms (add (lit a) (lit b)) -> (lit (+ a b))"
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((and (has-tag ir 'add)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (+ (cadr (cadr ir)) (cadr (caddr ir)))))
        ((and (has-tag ir 'sub)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (- (cadr (cadr ir)) (cadr (caddr ir)))))
        ((and (has-tag ir 'mul)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (* (cadr (cadr ir)) (cadr (caddr ir)))))
        ((and (has-tag ir 'div)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit)
              (not (zerop (cadr (caddr ir)))))
         (list 'lit
               (truncate (cadr (cadr ir)) (cadr (caddr ir)))))
        ((and (has-tag ir 'add)
              (has-tag (cadr ir) 'lit)
              (zerop (cadr (cadr ir))))
         (fold-constants (caddr ir)))
        ((and (has-tag ir 'add)
              (has-tag (caddr ir) 'lit)
              (zerop (cadr (caddr ir))))
         (fold-constants (cadr ir)))
        ((and (has-tag ir 'sub)
              (has-tag (caddr ir) 'lit)
              (zerop (cadr (caddr ir))))
         (fold-constants (cadr ir)))
        ((and (has-tag ir 'mul)
              (has-tag (cadr ir) 'lit)
              (= 1 (cadr (cadr ir))))
         (fold-constants (caddr ir)))
        ((and (has-tag ir 'mul)
              (has-tag (caddr ir) 'lit)
              (= 1 (cadr (caddr ir))))
         (fold-constants (cadr ir)))
        ((and (has-tag ir 'mul)
              (has-tag (cadr ir) 'lit)
              (zerop (cadr (cadr ir))))
         '(lit 0))
        ((and (has-tag ir 'mul)
              (has-tag (caddr ir) 'lit)
              (zerop (cadr (caddr ir))))
         '(lit 0))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div))
         (let* ((left
                 (fold-constants (cadr ir)))
                (right
                 (fold-constants (caddr ir)))
                (new-ir
                 (list (car ir) left right)))
           (if (and (has-tag left 'lit)
                    (has-tag right 'lit))
               (fold-constants new-ir)
               new-ir)))
        ((and (has-tag ir 'cmp-eq)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (if (= (cadr (cadr ir)) (cadr (caddr ir)))
                   1
                   0)))
        ((and (has-tag ir 'cmp-lt)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (if (< (cadr (cadr ir)) (cadr (caddr ir)))
                   1
                   0)))
        ((and (has-tag ir 'cmp-gt)
              (has-tag (cadr ir) 'lit)
              (has-tag (caddr ir) 'lit))
         (list 'lit
               (if (> (cadr (cadr ir)) (cadr (caddr ir)))
                   1
                   0)))
        ((and (has-tag ir 'if-ir)
              (has-tag (cadr ir) 'lit))
         (if (not (zerop (cadr (cadr ir))))
             (fold-constants (caddr ir))
             (fold-constants (cadddr ir))))
        ((has-tag ir 'progn-ir)
         (list 'progn-ir
               (mapcar #'fold-constants (cadr ir))))
        ((has-tag ir 'let-ir)
         (let ((bindings
                (mapcar #'fold-constants (cadr ir)))
               (body
                (fold-constants (caddr ir))))
           (list 'let-ir bindings body
                 (cadddr ir) (nth 4 ir))))
        ((or (has-tag ir 'car-ir)
             (has-tag ir 'cdr-ir)
             (has-tag ir 'null-ir)
             (has-tag ir 'consp-ir))
         (list (car ir)
               (fold-constants (cadr ir))))
        ((has-tag ir 'cons-ir)
         (list 'cons-ir
               (fold-constants (cadr ir))
               (fold-constants (caddr ir))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (fold-constants (cadr ir))
               (fold-constants (caddr ir))
               (fold-constants (cadddr ir))))
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'fold-constants (caddr ir))))
        (t ir)))

(register-optimization 'constant-folding
 #'fold-constants)

(defun reduce-strength (ir)
  "Replace expensive operations with cheaper equivalents.
   - (* x 2) -> (<< x 1)
   - (* x 4) -> (<< x 2)
   - (/ x 2) -> (>> x 1) for positive x"
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((and (has-tag ir 'mul)
              (has-tag (caddr ir) 'lit)
              (power-of-two-p (cadr (caddr ir))))
         (let ((shift
                (log2-int (cadr (caddr ir)))))
           (list 'bsh
                 (reduce-strength (cadr ir))
                 (list 'lit shift))))
        ((and (has-tag ir 'mul)
              (has-tag (cadr ir) 'lit)
              (power-of-two-p (cadr (cadr ir))))
         (let ((shift
                (log2-int (cadr (cadr ir)))))
           (list 'bsh
                 (reduce-strength (caddr ir))
                 (list 'lit shift))))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div)
             (has-tag ir 'bsh)
             (has-tag ir 'band)
             (has-tag ir 'bor)
             (has-tag ir 'bxor))
         (list (car ir)
               (reduce-strength (cadr ir))
               (reduce-strength (caddr ir))))
        ((or (has-tag ir 'cmp-eq)
             (has-tag ir 'cmp-lt)
             (has-tag ir 'cmp-gt)
             (has-tag ir 'cmp-le)
             (has-tag ir 'cmp-ge))
         (list (car ir)
               (reduce-strength (cadr ir))
               (reduce-strength (caddr ir))))
        ((or (has-tag ir 'car-ir)
             (has-tag ir 'cdr-ir)
             (has-tag ir 'null-ir)
             (has-tag ir 'consp-ir))
         (list (car ir)
               (reduce-strength (cadr ir))))
        ((has-tag ir 'cons-ir)
         (list 'cons-ir
               (reduce-strength (cadr ir))
               (reduce-strength (caddr ir))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (reduce-strength (cadr ir))
               (reduce-strength (caddr ir))
               (reduce-strength (cadddr ir))))
        ((has-tag ir 'progn-ir)
         (list 'progn-ir
               (mapcar #'reduce-strength (cadr ir))))
        ((has-tag ir 'let-ir)
         (list 'let-ir
               (mapcar #'reduce-strength (cadr ir))
               (reduce-strength (caddr ir))
               (cadddr ir) (nth 4 ir)))
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'reduce-strength (caddr ir))))
        (t ir)))

(defun power-of-two-p (n)
  "Check if n is a power of 2"
  (and (integerp n) (> n 0)
       (zerop (logand n (1- n)))))

(defun log2-int (n)
  "Integer log base 2"
  (if (<= n 1)
      0
      (1+ (log2-int (ash n -1)))))

(register-optimization 'strength-reduction
 #'reduce-strength)

(defun eliminate-dead-code (ir)
  "Remove unreachable code.
   - (progn-ir (x)) -> x (single form)
   - (progn-ir ()) -> (lit 0) (empty progn)
   - (if (lit 1) then else) -> then
   - (if (lit 0) then else) -> else
   Note: progn-ir structure is (progn-ir (form1 form2 ...)) where cadr is a list of forms"
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((has-tag ir 'progn-ir)
         (let* ((forms-list (cadr ir))
                (filtered
                 (if (null forms-list)
                     nil
                     (let ((last-form (car (last forms-list))))
                       (remove-if
                        (lambda (f)
                          (and
                           (has-tag f 'lit)
                           (not (eq f last-form))))
                        forms-list)))))
           (cond ((null filtered) '(lit 0))
                 ((= 1 (length filtered))
                  (eliminate-dead-code (car filtered)))
                 (t
                  (list 'progn-ir
                        (mapcar #'eliminate-dead-code
                                filtered))))))
        ((and (has-tag ir 'if-ir)
              (has-tag (cadr ir) 'lit))
         (if (not (zerop (cadr (cadr ir))))
             (eliminate-dead-code (caddr ir))
             (eliminate-dead-code (cadddr ir))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (eliminate-dead-code (cadr ir))
               (eliminate-dead-code (caddr ir))
               (eliminate-dead-code (cadddr ir))))
        ((has-tag ir 'let-ir)
         (list 'let-ir
               (mapcar #'eliminate-dead-code (cadr ir))
               (eliminate-dead-code (caddr ir))
               (cadddr ir) (nth 4 ir)))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div))
         (list (car ir)
               (eliminate-dead-code (cadr ir))
               (eliminate-dead-code (caddr ir))))
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'eliminate-dead-code (caddr ir))))
        (t ir)))

(register-optimization 'dead-code-elimination
 #'eliminate-dead-code)

(defun convert-self-tail-calls
       (ir fn-name param-count)
  "Convert tail calls in tail position:
   - Self-calls become continue-ir (loop back)
   - Other calls become tail-call-fn (jump without return)
   fn-name is the name of the function we're in.
   param-count is the number of parameters (for generating correct setqs)."
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ;; Self-call in tail position -> convert to continue (loop)
        ((and (has-tag ir 'call-fn)
              (sym-eq (cadr ir) fn-name))
         (list 'continue-ir
               (mapcar #'convert-non-tail (caddr ir))))
        ;; Other function call in tail position -> convert to tail-call-fn (jump)
        ((has-tag ir 'call-fn)
         (list 'tail-call-fn (cadr ir) (mapcar #'convert-non-tail (caddr ir))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (convert-non-tail (cadr ir))
               (convert-self-tail-calls (caddr ir)
                fn-name param-count)
               (convert-self-tail-calls (cadddr ir)
                fn-name param-count)))
        ((has-tag ir 'progn-ir)
         (let ((forms (cadr ir)))
           (if (null forms)
               ir
               (list 'progn-ir
                     (append
                      (mapcar #'convert-non-tail
                              (butlast forms))
                      (list
                       (convert-self-tail-calls
                        (car (last forms)) fn-name
                        param-count)))))))
        ((has-tag ir 'let-ir)
         (list 'let-ir
               (mapcar #'convert-non-tail (cadr ir))
               (convert-self-tail-calls (caddr ir)
                fn-name param-count)
               (cadddr ir) (nth 4 ir)))
        (t (convert-non-tail ir))))

(defun convert-non-tail (ir)
  "Process IR that is NOT in tail position - don't convert any calls."
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'convert-non-tail (caddr ir))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (convert-non-tail (cadr ir))
               (convert-non-tail (caddr ir))
               (convert-non-tail (cadddr ir))))
        ((has-tag ir 'progn-ir)
         (list 'progn-ir
               (mapcar #'convert-non-tail (cadr ir))))
        ((has-tag ir 'let-ir)
         (list 'let-ir
               (mapcar #'convert-non-tail (cadr ir))
               (convert-non-tail (caddr ir))
               (cadddr ir) (nth 4 ir)))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div)
             (has-tag ir 'cmp-eq)
             (has-tag ir 'cmp-lt)
             (has-tag ir 'cmp-gt)
             (has-tag ir 'cmp-le)
             (has-tag ir 'cmp-ge))
         (list (car ir)
               (convert-non-tail (cadr ir))
               (convert-non-tail (caddr ir))))
        ((or (has-tag ir 'car-ir)
             (has-tag ir 'cdr-ir)
             (has-tag ir 'null-ir)
             (has-tag ir 'consp-ir))
         (list (car ir)
               (convert-non-tail (cadr ir))))
        ((has-tag ir 'cons-ir)
         (list 'cons-ir
               (convert-non-tail (cadr ir))
               (convert-non-tail (caddr ir))))
        (t ir)))

(defun has-self-tail-call-p (ir fn-name)
  "Check if IR contains a self-tail-call to fn-name in tail position."
  (cond ((null ir) nil) ((not (consp ir)) nil)
        ((and (has-tag ir 'call-fn)
              (sym-eq (cadr ir) fn-name))
         t)
        ((has-tag ir 'if-ir)
         (or
          (has-self-tail-call-p (caddr ir)
           fn-name)
          (has-self-tail-call-p (cadddr ir)
           fn-name)))
        ((has-tag ir 'progn-ir)
         (let ((forms (cadr ir)))
           (and forms
                (has-self-tail-call-p (car (last forms))
                 fn-name))))
        ((has-tag ir 'let-ir)
         (has-self-tail-call-p (caddr ir)
          fn-name))
        (t nil)))

(defun wrap-with-loop (ir)
  "Wrap IR in a loop-ir if it contains continue-ir nodes."
  (if (contains-continue-p ir)
      (list 'loop-ir ir)
      ir))

(defun contains-continue-p (ir)
  "Check if IR contains any continue-ir nodes."
  (cond ((null ir) nil) ((not (consp ir)) nil)
        ((has-tag ir 'continue-ir) t)
        ((has-tag ir 'if-ir)
         (or (contains-continue-p (caddr ir))
             (contains-continue-p (cadddr ir))))
        ((has-tag ir 'progn-ir)
         (some #'contains-continue-p (cadr ir)))
        ((has-tag ir 'let-ir)
         (or (some #'contains-continue-p (cadr ir))
             (contains-continue-p (caddr ir))))
        (t nil)))

(defun apply-tco-to-function (compiled-fn)
  "Apply TCO optimization to a compiled function.
   compiled-fn has structure: (name params body-ir param-base)
   Returns the same structure with body-ir transformed if it has self-tail-calls."
  (let* ((name (car compiled-fn))
         (params (cadr compiled-fn))
         (body-ir (caddr compiled-fn))
         (param-base (cadddr compiled-fn))
         (nparams (length params)))
    (if (has-self-tail-call-p body-ir name)
        (let* ((converted-ir
                (convert-self-tail-calls body-ir
                 name nparams))
               (wrapped-ir
                (wrap-with-loop converted-ir)))
          (list name params wrapped-ir
                param-base))
        compiled-fn)))

(defun apply-tco-to-all-functions (compiled-fns)
  "Apply TCO optimization to all compiled functions."
  (if (null compiled-fns)
      nil
      (cons (apply-tco-to-function (car compiled-fns))
            (apply-tco-to-all-functions (cdr compiled-fns)))))

(register-optimization 'tail-call-optimization
 #'apply-tco-to-function)

(defun flatten-let (ir)
  "Flatten consecutive nested let-ir nodes into a single let-ir.
   This reduces IR nesting depth from 100+ levels to just a few.
   Example: (let-ir ((x 1)) (let-ir ((y 2)) body)) -> (let-ir ((x 1) (y 2)) body)"
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((has-tag ir 'let-ir)
         (let* ((vals (cadr ir))
                (body-ir (caddr ir))
                (count (cadddr ir))
                (offsets (nth 4 ir)))
           (if (and (consp body-ir)
                    (has-tag body-ir
                     'let-ir))
               (let* ((inner-vals (cadr body-ir))
                      (inner-body (caddr body-ir))
                      (inner-count (cadddr body-ir))
                      (inner-offsets (nth 4 body-ir))
                      (merged
                       (list 'let-ir
                             (append
                              (mapcar #'flatten-let vals)
                              (mapcar #'flatten-let
                                      inner-vals))
                             (flatten-let inner-body)
                             (+ count inner-count)
                             (append offsets inner-offsets))))
                 (flatten-let merged))
               (list 'let-ir
                     (mapcar #'flatten-let vals)
                     (flatten-let body-ir) count
                     offsets))))
        ((has-tag ir 'if-ir)
         (list 'if-ir (flatten-let (cadr ir))
               (flatten-let (caddr ir))
               (flatten-let (cadddr ir))))
        ((has-tag ir 'progn-ir)
         (list 'progn-ir
               (mapcar #'flatten-let (cadr ir))))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div))
         (list (car ir)
               (flatten-let (cadr ir))
               (flatten-let (caddr ir))))
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'flatten-let (caddr ir))))
        (t ir)))

(register-optimization 'let-flattening
 #'flatten-let)

(defun flatten-progn (ir)
  "Flatten nested progn-ir nodes into a single progn-ir.
   (progn (progn a b) c) => (progn a b c)"
  (cond ((null ir) nil) ((not (consp ir)) ir)
        ((has-tag ir 'progn-ir)
         (let* ((forms (cadr ir))
                (flattened-forms
                 (apply #'append
                        (mapcar
                         (lambda (form)
                           (let ((flat-form
                                  (flatten-progn form)))
                             (if (and (consp flat-form)
                                      (has-tag flat-form
                                       'progn-ir))
                                 (cadr flat-form)
                                 (list flat-form))))
                         forms))))
           (if (= (length flattened-forms) 1)
               (car flattened-forms)
               (list 'progn-ir flattened-forms))))
        ((has-tag ir 'if-ir)
         (list 'if-ir
               (flatten-progn (cadr ir))
               (flatten-progn (caddr ir))
               (flatten-progn (cadddr ir))))
        ((has-tag ir 'let-ir)
         (list 'let-ir
               (mapcar #'flatten-progn (cadr ir))
               (flatten-progn (caddr ir))
               (cadddr ir) (nth 4 ir)))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div))
         (list (car ir)
               (flatten-progn (cadr ir))
               (flatten-progn (caddr ir))))
        ((has-tag ir 'call-fn)
         (list 'call-fn (cadr ir)
               (mapcar #'flatten-progn (caddr ir))))
        (t ir)))

(register-optimization 'progn-flattening
 #'flatten-progn)

(defun inline-source (expr fenv)
  "Inline small functions in source expression EXPR.
   FENV is alist of (name params body) for inlinable functions.
   This transforms source-level function calls."
  (cond ((null expr) nil)
        ((not (consp expr)) expr)
        ((sym-eq (car expr) 'quote) expr)
        ((and (symbolp (car expr))
              (not (special-form-p (car expr))))
         (let ((fn-info
                (source-lookup (car expr)
                 fenv)))
           (if (and fn-info
                    (source-inlinable? fn-info))
               (let* ((params (cadr fn-info))
                      (body (caddr fn-info))
                      (args (cdr expr)))
                 (if (= (length params) (length args))
                     (inline-source
                      (source-substitute body
                       params args)
                      fenv)
                     (cons (car expr)
                           (mapcar
                            (lambda (a)
                              (inline-source a
                               fenv))
                            (cdr expr)))))
               (cons (car expr)
                     (mapcar
                      (lambda (a)
                        (inline-source a
                         fenv))
                      (cdr expr))))))
        ((sym-eq (car expr) 'if)
         (list 'if
               (inline-source (cadr expr)
                fenv)
               (inline-source (caddr expr)
                fenv)
               (if (cadddr expr)
                   (inline-source (cadddr expr)
                    fenv)
                   nil)))
        ((sym-eq (car expr) 'progn)
         (cons 'progn
               (mapcar
                (lambda (e)
                  (inline-source e fenv))
                (cdr expr))))
        ((or (sym-eq (car expr) 'let) (sym-eq (car expr) 'let*))
         (list (car expr)
               (mapcar
                (lambda (b)
                  (list (car b)
                        (inline-source (cadr b)
                         fenv)))
                (cadr expr))
               (inline-source (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'lambda)
         (list 'lambda (cadr expr)
               (inline-source (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'labels) expr)
        ((sym-eq (car expr) 'cond)
         (cons 'cond
               (mapcar
                (lambda (clause)
                  (mapcar
                   (lambda (e)
                     (inline-source e fenv))
                   clause))
                (cdr expr))))
        ((or (sym-eq (car expr) 'when) (sym-eq (car expr) 'unless))
         (cons (car expr)
               (mapcar
                (lambda (e)
                  (inline-source e fenv))
                (cdr expr))))
        ((or (sym-eq (car expr) 'and) (sym-eq (car expr) 'or))
         (cons (car expr)
               (mapcar
                (lambda (e)
                  (inline-source e fenv))
                (cdr expr))))
        ((sym-eq (car expr) 'setq)
         (list 'setq (cadr expr)
               (inline-source (caddr expr)
                fenv)))
        ((sym-eq (car expr) 'while)
         (cons 'while
               (mapcar
                (lambda (e)
                  (inline-source e fenv))
                (cdr expr))))
        (t
         (cons
          (inline-source (car expr) fenv)
          (inline-source (cdr expr) fenv)))))

(defun special-form-p (sym)
  "Check if symbol is a special form that shouldn't be inlined"
  (member sym
          '(quote if progn let let* lambda labels cond when unless and or setq defun
            while function funcall)))

(defun source-lookup (name fenv)
  "Look up function info in fenv"
  (cond ((null fenv) nil)
        ((sym-eq name (car (car fenv)))
         (car fenv))
        (t (source-lookup name (cdr fenv)))))

(defun source-inlinable? (fn-info)
  "Check if function is small enough to inline.
   FN-INFO is (name params body)
   NOTE: Inlining is currently DISABLED due to variable capture bug.
   When inlined functions have local variables with the same name as
   variables in the calling context, the wrong offset is used."
  (declare (ignore fn-info))
  nil)

(defun source-expr-size (expr)
  "Estimate size of source expression"
  (cond ((null expr) 1) ((not (consp expr)) 1)
        ((sym-eq (car expr) 'quote) 1)
        ((or (sym-eq (car expr) 'progn) (sym-eq (car expr) 'and)
             (sym-eq (car expr) 'or))
         (let ((sum 1) (elems (cdr expr)))
           (while elems
            (setq sum
                    (+ sum
                       (source-expr-size (car elems))))
            (setq elems (cdr elems)))
           sum))
        ((sym-eq (car expr) 'if)
         (+ 1 (source-expr-size (cadr expr))
            (source-expr-size (caddr expr))
            (if (cadddr expr)
                (source-expr-size (cadddr expr))
                0)))
        ((or (sym-eq (car expr) 'let) (sym-eq (car expr) 'let*))
         (+ 2 (source-expr-size (caddr expr))))
        (t (1+ (length (cdr expr))))))

(defun source-calls-self? (expr fn-name)
  "Check if expression calls fn-name"
  (cond ((null expr) nil) ((not (consp expr)) nil)
        ((sym-eq (car expr) 'quote) nil)
        ((and (symbolp (car expr))
              (sym-eq (car expr) fn-name))
         t)
        (t
         (or
          (source-calls-self? (car expr)
           fn-name)
          (source-calls-self? (cdr expr)
           fn-name)))))

(defun source-substitute
       (expr params args)
  "Replace parameters with arguments in expression"
  (cond ((null expr) nil)
        ((symbolp expr)
         (let ((pos
                (source-find-param expr
                 params 0)))
           (if pos
               (nth pos args)
               expr)))
        ((not (consp expr)) expr)
        ((sym-eq (car expr) 'quote) expr)
        (t
         (cons
          (source-substitute (car expr)
           params args)
          (source-substitute (cdr expr)
           params args)))))

(defun source-find-param
       (name params idx)
  "Find position of name in params list"
  (cond ((null params) nil)
        ((sym-eq name (car params)) idx)
        (t
         (source-find-param name (cdr params)
          (1+ idx)))))

(defun inline-all-defuns (forms)
  "Apply source inlining to all defun bodies in forms.
   First collects inlinable functions, then inlines into all bodies."
  (let ((fenv
         (collect-inlinable-fns forms nil)))
    (mapcar
     (lambda (form)
       (if (and (consp form) (sym-eq (car form) 'defun))
           (let* ((name (cadr form))
                  (params (caddr form))
                  (body-forms (cdddr form)))
             (list* 'defun name params
                    (mapcar
                     (lambda (b)
                       (inline-source b
                        fenv))
                     body-forms)))
           (inline-source form fenv)))
     forms)))

(defun collect-inlinable-fns (forms acc)
  "Collect (name params body) for all inlinable functions"
  (if (null forms)
      acc
      (let ((form (car forms)))
        (if (and (consp form) (sym-eq (car form) 'defun))
            (let* ((name (cadr form))
                   (params (caddr form))
                   (body-forms (cdddr form))
                   (body
                    (if (null (cdr body-forms))
                        (car body-forms)
                        (cons 'progn body-forms)))
                   (fn-info
                    (list name params body)))
              (collect-inlinable-fns (cdr forms)
               (if (source-inlinable? fn-info)
                   (cons fn-info acc)
                   acc)))
            (collect-inlinable-fns (cdr forms)
             acc)))))

(defun optimize-ir (ir)
  "Run optimization passes on IR (native version - fixed pass list)"
  (let ((result ir)
        (passes
         '(let-flattening progn-flattening
           constant-folding strength-reduction
           dead-code-elimination)))
    (while passes
     (setq result
             (run-optimization (car passes)
              result))
     (setq passes (cdr passes)))
    result))

(defun optimize-function-ir (fn-ir)
  "Optimize a function's IR, preserving function metadata"
  (if (has-tag fn-ir 'fn-ir)
      (list 'fn-ir (cadr fn-ir) (caddr fn-ir)
            (optimize-ir (cadddr fn-ir))
            (nth 4 fn-ir))
      fn-ir))

(defun has-tag (ir tag)
  "Check if IR has the given tag.
   Uses string-equal for native Habu compatibility (symbols not eq across reads)."
  (and (consp ir)
       (symbolp (car ir))
       (string-equal (symbol-name (car ir)) (symbol-name tag))))

(defun reset-symbol-table ()
  "Reset symbol table state.
   In SBCL: resets *symbol-state* cons cell.
   In native: resets [x27+48] = 16 (tagged 1), [x27+56] = nil."
  (progn (set-symbol-counter 1) (set-symbol-table-sym nil)))

(defun intern-symbol (name)
  "Get or create a symbol ID for NAME.
   Native mode: uses [x27+48] for counter, [x27+56] for table."
  (let* ((counter (get-symbol-counter))
         (table (get-symbol-table-sym)))
    (labels ((find-in-table (lst)
               (if (null lst)
                   nil
                   (if (string-equal name (car (car lst)))
                       (cdr (car lst))
                       (find-in-table (cdr lst))))))
      (let ((existing (find-in-table table)))
        (if existing
            existing
            (let ((id counter))
              (set-symbol-counter (+ counter 1))
              (set-symbol-table-sym
               (cons (cons name id) table))
              id))))))

(defun reset-lambda-counter ()
  "Reset lambda counter.
   In SBCL: uses *lambda-state* cons cell.
   In native: uses get-lambda-counter/set-lambda-counter primitives (stores at [x27+8])."
  (set-lambda-counter 0))

(defun gensym-lambda ()
  "Generate unique lambda name as an interned symbol like LAMBDA-1, LAMBDA-2, etc."
  (let* ((counter (get-lambda-counter))
         (new-count (+ counter 1)))
    (set-lambda-counter new-count)
    (labels ((digits (n acc)
               (if (= n 0)
                   (if (null acc)
                       (cons 48 nil)
                       acc)
                   (digits (/ n 10)
                    (cons (+ 48 (mod n 10)) acc))))
             (chars-to-vec (cs)
               (let ((len (length cs)))
                 (labels ((build
                              (i cs vec)
                            (if (null cs)
                                vec
                                (progn
                                 (vector-set vec
                                  i (car cs))
                                 (build (+ i 1)
                                  (cdr cs) vec)))))
                   (build 0 cs
                    (make-vector len))))))
      (let* ((num-chars
              (digits new-count nil))
             (prefix (list 76 65 77 66 68 65 45))
             (all-chars
              (append prefix num-chars)))
        (make-string-from-vector
         (chars-to-vec all-chars))))))

(defun lift-lambdas (ir lambdas)
  "Extract lambda-ir nodes from IR, replacing with lambda-ref.
   Returns (cons transformed-ir lambdas) where lambdas is alist of (name params body free-vars free-offsets)"
  (cond ((null ir) (cons ir lambdas))
        ((not (consp ir)) (cons ir lambdas))
        ((has-tag ir 'lambda-ir)
         (let* ((name (gensym-lambda))
                (params (cadr ir))
                (body (caddr ir))
                (free-vars (cadddr ir))
                (free-offsets (nth 4 ir)))
           (let* ((body-result
                   (lift-lambdas body
                    lambdas))
                  (new-body (car body-result))
                  (more-lambdas (cdr body-result))
                  (lambda-entry
                   (list name params new-body
                         free-vars free-offsets)))
             (cons
              (list 'lambda-ref name
                    free-offsets)
              (cons lambda-entry more-lambdas)))))
        ((has-tag ir 'let-ir)
         (let* ((vals (cadr ir))
                (body (caddr ir))
                (count (cadddr ir))
                (offs (nth 4 ir))
                (vals-result
                 (lift-list vals lambdas))
                (new-vals (car vals-result))
                (l1 (cdr vals-result))
                (body-result
                 (lift-lambdas body l1))
                (new-body (car body-result))
                (l2 (cdr body-result)))
           (cons
            (list 'let-ir new-vals new-body
                  count offs)
            l2)))
        ((has-tag ir 'if-ir)
         (let* ((test (cadr ir))
                (then (caddr ir))
                (else (cadddr ir))
                (test-result
                 (lift-lambdas test lambdas))
                (new-test (car test-result))
                (l1 (cdr test-result))
                (then-result
                 (lift-lambdas then l1))
                (new-then (car then-result))
                (l2 (cdr then-result))
                (else-result
                 (lift-lambdas else l2))
                (new-else (car else-result))
                (l3 (cdr else-result)))
           (cons
            (list 'if-ir new-test new-then
                  new-else)
            l3)))
        ((has-tag ir 'while-ir)
         (let* ((test (cadr ir))
                (body (caddr ir))
                (test-result
                 (lift-lambdas test lambdas))
                (new-test (car test-result))
                (l1 (cdr test-result))
                (body-result
                 (lift-lambdas body l1))
                (new-body (car body-result))
                (l2 (cdr body-result)))
           (cons
            (list 'while-ir new-test
                  new-body)
            l2)))
        ((has-tag ir 'progn-ir)
         (let* ((forms (cadr ir))
                (forms-result
                 (lift-list forms lambdas))
                (new-forms (car forms-result))
                (new-lambdas (cdr forms-result)))
           (cons (list 'progn-ir new-forms)
                 new-lambdas)))
        ((has-tag ir 'funcall-ir)
         (let* ((fn-ir (cadr ir))
                (args (caddr ir))
                (fn-result
                 (lift-lambdas fn-ir
                  lambdas))
                (new-fn (car fn-result))
                (l1 (cdr fn-result))
                (args-result
                 (lift-list args l1))
                (new-args (car args-result))
                (l2 (cdr args-result)))
           (cons
            (list 'funcall-ir new-fn
                  new-args)
            l2)))
        ((has-tag ir 'call-fn)
         (let* ((name (cadr ir))
                (args (caddr ir))
                (args-result
                 (lift-list args lambdas))
                (new-args (car args-result))
                (new-lambdas (cdr args-result)))
           (cons
            (list 'call-fn name new-args)
            new-lambdas)))
        ;; Tail call to other function - same as call-fn but preserves tail-call-fn tag
        ((has-tag ir 'tail-call-fn)
         (let* ((name (cadr ir))
                (args (caddr ir))
                (args-result
                 (lift-list args lambdas))
                (new-args (car args-result))
                (new-lambdas (cdr args-result)))
           (cons
            (list 'tail-call-fn name new-args)
            new-lambdas)))
        ((or (has-tag ir 'add)
             (has-tag ir 'sub)
             (has-tag ir 'mul)
             (has-tag ir 'div)
             (has-tag ir 'mod)
             (has-tag ir 'cmp-eq)
             (has-tag ir 'cmp-lt)
             (has-tag ir 'cmp-gt)
             (has-tag ir 'cons-ir)
             (has-tag ir 'setcar-ir)
             (has-tag ir 'setcdr-ir)
             (has-tag ir 'string-ref-ir)
             (has-tag ir 'string-concat-ir)
             (has-tag ir 'string-equal-ir)
             (has-tag ir 'vector-ref-ir)
             (has-tag ir
              'buffer-byte-ref-ir))
         (let* ((left (cadr ir))
                (right (caddr ir))
                (left-result
                 (lift-lambdas left lambdas))
                (new-left (car left-result))
                (l1 (cdr left-result))
                (right-result
                 (lift-lambdas right l1))
                (new-right (car right-result))
                (l2 (cdr right-result)))
           (cons
            (list (car ir) new-left
                  new-right)
            l2)))
        ((or (has-tag ir 'vector-set-ir)
             (has-tag ir
              'buffer-byte-set-ir))
         (let* ((arg1 (cadr ir))
                (arg2 (caddr ir))
                (arg3 (cadddr ir))
                (r1
                 (lift-lambdas arg1 lambdas))
                (r2
                 (lift-lambdas arg2
                  (cdr r1)))
                (r3
                 (lift-lambdas arg3
                  (cdr r2))))
           (cons
            (list (car ir) (car r1) (car r2)
                  (car r3))
            (cdr r3))))
        ((or (has-tag ir 'car-ir)
             (has-tag ir 'cdr-ir)
             (has-tag ir 'get-tag)
             (has-tag ir 'symbol-name-ir)
             (has-tag ir 'make-symbol-ir)
             (has-tag ir 'string-length-ir)
             (has-tag ir 'make-vector-ir)
             (has-tag ir 'vector-length-ir)
             (has-tag ir
              'make-string-from-vector-ir)
             (has-tag ir
              'set-global-vars-ir))
         (let* ((sb-debug:arg (cadr ir))
                (arg-result
                 (lift-lambdas sb-debug:arg lambdas))
                (new-arg (car arg-result))
                (new-lambdas (cdr arg-result)))
           (cons (list (car ir) new-arg)
                 new-lambdas)))
        ((has-tag ir 'sys-exit-ir)
         (let* ((sb-debug:arg (cadr ir))
                (arg-result
                 (lift-lambdas sb-debug:arg lambdas))
                (new-arg (car arg-result))
                (new-lambdas (cdr arg-result)))
           (cons (list 'sys-exit-ir new-arg)
                 new-lambdas)))
        ((has-tag ir 'setq-ir)
         (let* ((off (cadr ir))
                (val-ir (caddr ir))
                (val-result
                 (lift-lambdas val-ir
                  lambdas))
                (new-val (car val-result))
                (new-lambdas (cdr val-result)))
           (cons (list 'setq-ir off new-val)
                 new-lambdas)))
        (t (cons ir lambdas))))

(defun lift-list (lst lambdas)
  "Lift lambdas from a list of IR nodes"
  (if (null lst)
      (cons nil lambdas)
      (let* ((first-result
              (lift-lambdas (car lst)
               lambdas))
             (new-first (car first-result))
             (l1 (cdr first-result))
             (rest-result
              (lift-list (cdr lst) l1))
             (new-rest (car rest-result))
             (l2 (cdr rest-result)))
        (cons (cons new-first new-rest) l2))))

(defun lift-lambdas-from-defuns
       (defuns acc-defuns acc-lambdas)
  "Lift lambdas from all defun bodies.
   Defun format: (name params body param-base)
   Must preserve param-base after lifting."
  (if (null defuns)
      (cons (reverse acc-defuns) acc-lambdas)
      (let* ((defun (car defuns))
             (name (car defun))
             (params (cadr defun))
             (body (caddr defun))
             (param-base (cadddr defun))
             (body-result
              (lift-lambdas body
               acc-lambdas))
             (new-body (car body-result))
             (more-lambdas (cdr body-result))
             (new-defun
              (list name params new-body
                    param-base)))
        (lift-lambdas-from-defuns (cdr defuns)
         (cons new-defun acc-defuns)
         more-lambdas))))

(defun lambdas-to-defuns (lambdas acc)
  "Convert lifted lambda entries to defun format.
   Lambda entry: (name params body free-vars free-offsets)
   Defun format: (name params body param-base)
   The param-base for lambdas is the number of captured variables,
   since params are stored after captured vars in the environment."
  (if (null lambdas)
      (reverse acc)
      (let* ((lambda-entry (car lambdas))
             (name (car lambda-entry))
             (params (cadr lambda-entry))
             (body (caddr lambda-entry))
             (free-vars (cadddr lambda-entry))
             (param-base (length free-vars))
             (defun-entry
              (list name params body
                    param-base)))
        (lambdas-to-defuns (cdr lambdas)
         (cons defun-entry acc)))))

(defun movz (rd imm)
  (arm64:movz rd imm))

(defun movk
       (rd imm shift16)
  "MOVK Rd, #imm, LSL #shift16 - shift16 is 0, 1, 2, or 3 (for 0, 16, 32, 48)"
  (arm64:movk rd imm :lsl (* shift16 16)))

(defconstant +gc-from-end-offset+ 16)

(defconstant +gen-nursery-end-offset+ 88)

(defconstant +gen-card-table-offset+ 96)

(defconstant +gen-card-shift+ 9)

(defun gc-trigger-code ()
  "Generate inline GC trigger check. Insert after allocations.
   Uses x9 as scratch. Emits :call-fn marker if GC needed.
   In generational mode: checks nursery-end, calls GEN-MINOR-GC.
   In simple mode: checks from-end, calls GC-COLLECT."
  (append-all
   (list (arm64:ldr :x9 :gc :offset +gc-from-end-offset+) (arm64:cmp :heap :x9)
         (arm64:b.lo 2) (list (list :call-fn 'gc-collect)))))

(defun gen-write-barrier-code (target-reg)
  "Generate write barrier for stores to heap objects.
   TARGET-REG is the register containing the target object address.
   Call after every heap store that may create an old->young pointer.

   The barrier:
   1. Checks if target is in old space (address >= nursery_end)
   2. If so, computes card index and marks card dirty

   Uses x9, x10 as scratch. Only generated in generational GC mode."
  nil)

(defun reverse-helper (lst acc)
  "Iterative reverse helper using while loop"
  (let ((remaining lst)
        (result acc))
    (while (not (null remaining))
     (setq result
             (cons (car remaining) result))
     (setq remaining (cdr remaining)))
    result))

(defun reverse (lst)
  "Reverse a list using iterative while loop"
  (let ((remaining lst) (result nil))
    (while (not (null remaining))
     (setq result
             (cons (car remaining) result))
     (setq remaining (cdr remaining)))
    result))

(defun append (lst1 lst2)
  "Append two lists using iterative while loop to avoid stack overflow"
  (let ((reversed nil)
        (remaining lst1)
        (result lst2))
    (while (not (null remaining))
     (setq reversed
             (cons (car remaining) reversed))
     (setq remaining (cdr remaining)))
    (setq remaining reversed)
    (while (not (null remaining))
     (setq result
             (cons (car remaining) result))
     (setq remaining (cdr remaining)))
    result))

(defun length (lst)
  "List length using iterative while loop"
  (let ((remaining lst) (n 0))
    (while (not (null remaining))
     (setq n (+ n 1))
     (setq remaining (cdr remaining)))
    n))

(defun append-all (lists)
  "Append all lists using iterative while loop"
  (if (null lists)
      nil
      (let ((remaining lists) (result nil))
        (let ((reversed nil))
          (while (not (null remaining))
           (setq reversed
                   (cons (car remaining) reversed))
           (setq remaining (cdr remaining)))
          (setq remaining reversed)
          (while (not (null remaining))
           (setq result
                   (append (car remaining) result))
           (setq remaining (cdr remaining))))
        result)))

(defun temp-slot (td)
  "Calculate temp slot offset for depth TD.
   Temp slots occupy 0x40-0x100 (24 slots, 192 bytes)."
  (if (>= td 24)
      (progn 256)
      (+ 64 (* td 8))))

(defun load-addr (rd sb-alien:addr)
  "Load large address into register (up to 64 bits)"
  (if (< sb-alien:addr 65536)
      (movz rd sb-alien:addr)
      (if (< sb-alien:addr 4294967296)
          (append (movz rd (logand sb-alien:addr 65535))
                  (movk rd (ash sb-alien:addr -16) 1))
          (if (< sb-alien:addr 281474976710656)
              (append-all
               (list (movz rd (logand sb-alien:addr 65535))
                     (movk rd
                      (logand (ash sb-alien:addr -16) 65535) 1)
                     (movk rd
                      (logand (ash sb-alien:addr -32) 65535) 2)))
              (append-all
               (list (movz rd (logand sb-alien:addr 65535))
                     (movk rd
                      (logand (ash sb-alien:addr -16) 65535) 1)
                     (movk rd
                      (logand (ash sb-alien:addr -32) 65535) 2)
                     (movk rd
                      (logand (ash sb-alien:addr -48) 65535) 3)))))))

(defun load-addr-8 (rd sb-alien:addr)
  "Load address into register, always producing 8 bytes (2 instructions).
   Used for lambda/function references where consistent code size is needed."
  (append (movz rd (logand sb-alien:addr 65535))
          (movk rd (ash sb-alien:addr -16) 1)))

(defun gen-string-lit
       (str len total-size)
  "Generate code to allocate string literal on heap.
   String layout: [length:8][data:N]
   Returns tagged string pointer in x0, bumps x28.
   IMPORTANT: GC trigger checked BEFORE allocation to prevent writing to unmapped memory."
  (labels ((gen-store-bytes
               (offset bytes acc)
             (if (null bytes)
                 acc
                 (let* ((chunk
                         (take-bytes bytes 8))
                        (val
                         (bytes-to-u64 chunk))
                        (rest (drop-bytes bytes 8)))
                   (gen-store-bytes (+ offset 8) rest
                    (append-all
                     (list acc
                           (load-addr 9 val)
                           (arm64:str :x9 :heap :offset offset)))))))
           (str-to-bytes
               (s i acc)
             (if (>= i (string-length s))
                 (reverse acc)
                 (str-to-bytes s (+ i 1)
                  (cons (string-ref s i)
                        acc)))))
    (let* ((bytes (str-to-bytes str 0 nil))
           (bytes-with-nul (append bytes (list 0)))
           (pre-check (gc-trigger-code))
           (len-code
            (append-all
             (list (load-addr 9 len)
                   (arm64:str :x9 :heap :offset 0))))
           (data-code
            (gen-store-bytes 8 bytes-with-nul nil))
           (result-code
            (append-all
             (list (arm64:mov :x0 :heap) (arm64:add :x0 :x0 4 :imm t)
                   (arm64:add :heap :heap total-size :imm t)
                   (gc-trigger-code)))))
      (append-all
       (list pre-check len-code data-code
             result-code)))))

(defun gen-symbol-lit
       (str len total-size)
  "Generate code to allocate symbol literal on heap.
   Symbol layout: same as string [length:8][name:N]
   Returns tagged symbol pointer (tag 2) in x0, bumps x28."
  (labels ((gen-store-bytes
               (offset bytes acc)
             (if (null bytes)
                 acc
                 (let* ((chunk
                         (take-bytes bytes 8))
                        (val
                         (bytes-to-u64 chunk))
                        (rest (drop-bytes bytes 8)))
                   (gen-store-bytes (+ offset 8) rest
                    (append-all
                     (list acc
                           (load-addr 9 val)
                           (arm64:str :x9 :heap :offset offset)))))))
           (str-to-bytes
               (s i acc)
             (if (>= i (string-length s))
                 (reverse acc)
                 (str-to-bytes s (+ i 1)
                  (cons (string-ref s i)
                        acc)))))
    (let* ((bytes (str-to-bytes str 0 nil))
           (bytes-with-nul (append bytes (list 0)))
           (pre-check (gc-trigger-code))
           (len-code
            (append-all
             (list (load-addr 9 len)
                   (arm64:str :x9 :heap :offset 0))))
           (data-code
            (gen-store-bytes 8 bytes-with-nul nil))
           (result-code
            (append-all
             (list (arm64:mov :x0 :heap) (arm64:add :x0 :x0 2 :imm t)
                   (arm64:add :heap :heap total-size :imm t)
                   (gc-trigger-code)))))
      (append-all
       (list pre-check len-code data-code
             result-code)))))

(defun take-bytes (bytes n)
  "Take up to N bytes from list"
  (if (or (null bytes) (<= n 0))
      nil
      (cons (car bytes)
            (take-bytes (cdr bytes)
             (- n 1)))))

(defun drop-bytes (bytes n)
  "Drop N bytes from list"
  (if (or (null bytes) (<= n 0))
      bytes
      (drop-bytes (cdr bytes) (- n 1))))

(defun bytes-to-u64 (bytes)
  "Convert list of up to 8 bytes to u64 (little-endian)"
  (labels ((to-u64
               (bs shift acc)
             (if (null bs)
                 acc
                 (to-u64 (cdr bs) (+ shift 8)
                  (logior acc
                          (ash (car bs) shift))))))
    (to-u64 bytes 0 0)))

(defun save-temp (td)
  (arm64:str :x0 :sp :offset (temp-slot td)))

(defun load-temp (rd td)
  (arm64:ldr rd :sp :offset (temp-slot td)))

(defun strb (rt rn offset)
  "Store byte from rt to [rn + offset]"
  (arm64:strb rt rn offset))

(defun strb-reg (rt rn rm)
  "STRB Wt, [Xn, Xm] - store byte to address Xn+Xm"
  (arm64:strb rt rn rm :reg t))

(defun gen-memcpy-inline (count-reg)
  "Generate inline memcpy loop.
   x1 = src, x3 = dst, count-reg = count (modified).
   x4 = temp for byte. Increments x1, x3."
  (let* ((skip-if-zero (arm64:cbz count-reg 7))
         (load-byte (arm64:ldrb :x4 :x1 0))
         (store-byte (strb 4 3 0))
         (inc-src (arm64:add :x1 :x1 1 :imm t))
         (inc-dst (arm64:add :x3 :x3 1 :imm t))
         (dec-count
          (arm64:sub count-reg count-reg 1 :imm t))
         (loop-back (arm64:b -6)))
    (append-all
     (list skip-if-zero load-byte store-byte
           inc-src inc-dst dec-count
           loop-back))))

(defun has-tag (ir tag)
  "Check if IR has the given tag.
   Uses string-equal for native Habu compatibility (symbols not eq across reads)."
  (and (consp ir)
       (symbolp (car ir))
       (string-equal (symbol-name (car ir)) (symbol-name tag))))

(defun ir-may-call (ir)
  "Check if IR may involve a function call"
  (cond ((null ir) nil) ((not (consp ir)) nil)
        ((has-tag ir 'lit) nil)
        ((has-tag ir 'var-ref) nil)
        ((has-tag ir 'sym-lit) nil)
        ((has-tag ir 'call-fn) t)
        ((has-tag ir 'tail-call-fn) t)
        ((has-tag ir 'funcall-ir) t)
        ((has-tag ir 'sys-exit-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'get-cmdline-args-ir)
         nil)
        ((has-tag ir 'add)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'sub)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'mul)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'mod)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'cons-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'car-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'cdr-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'get-tag)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'setq-ir)
         (ir-may-call (caddr ir)))
        ((has-tag ir 'setcar-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'setcdr-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'symbol-name-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'make-symbol-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'string-length-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'string-ref-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'string-concat-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'string-equal-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'make-vector-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'vector-ref-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'vector-set-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))
             (ir-may-call (cadddr ir))))
        ((has-tag ir 'vector-length-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'buffer-byte-ref-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        ((has-tag ir 'buffer-byte-set-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))
             (ir-may-call (cadddr ir))))
        ((has-tag ir
          'make-string-from-vector-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'get-global-vars-ir)
         nil)
        ((has-tag ir 'set-global-vars-ir)
         (ir-may-call (cadr ir)))
        ((has-tag ir 'str-lit) nil)
        ((has-tag ir 'if-ir) t)
        ((has-tag ir 'while-ir) t)
        ((has-tag ir 'let-ir) t)
        ((has-tag ir 'let*-ir) t)
        ((has-tag ir 'progn-ir) t)
        ((has-tag ir 'sys-open-ir) t)
        ((has-tag ir 'sys-write-ir) t)
        ((has-tag ir 'sys-write-char-ir) t)
        ((has-tag ir 'sys-read-byte-ir) t)
        ((has-tag ir 'sys-read-ir) t)
        ((has-tag ir 'sys-close-ir) t)
        ((has-tag ir 'mmap-jit-ir) t)
        ((has-tag ir
          'pthread-jit-write-protect-np-ir)
         t)
        ((has-tag ir 'sys-dcache-flush-ir) t)
        ((has-tag ir
          'sys-icache-invalidate-ir)
         t)
        ((has-tag ir 'funcall-ptr-ir) t)
        ((has-tag ir 'mem-set-byte-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))
             (ir-may-call (cadddr ir))))
        ((has-tag ir 'mem-load-64-ir)
         (or (ir-may-call (cadr ir))
             (ir-may-call (caddr ir))))
        (t nil)))

(defun lookup-string (name fnoffs)
  "Look up a string name in fnoffs alist.
   fnoffs entries can have either symbol or string keys.
   Returns (name . offset) or nil if not found."
  (labels ((str-match (s1 s2)
             (cond
              ((and (stringp s1) (stringp s2))
               (string-equal s1 s2))
              ((and (stringp s1) (symbolp s2))
               (string-equal s1 (symbol-name s2)))
              ((and (symbolp s1) (stringp s2))
               (string-equal (symbol-name s1) s2))
              (t (eq s1 s2))))
           (search-list (lst)
             (if (null lst)
                 nil
                 (let ((entry (car lst)))
                   (if (str-match name
                        (car entry))
                       entry
                       (search-list (cdr lst)))))))
    (search-list fnoffs)))

(defun build-captures (free-offsets)
  "Generate code to build a cons list of captured values.
   free-offsets = list of stack offsets where captured values live.
   Result in x0 is a tagged cons list."
  (if (null free-offsets)
      (movz 0 0)
      (labels ((build-list (offs acc)
                 (if (null offs)
                     acc
                     (let* ((off (car offs))
                            (off8 (* off 8))
                            (load-code
                             (append (arm64:sub :x1 :env off8 :imm t)
                                     (arm64:ldr :x0 :x1 :offset 0)))
                            (store-code
                             (if (null (cdr offs))
                                 nil
                                 (append-all
                                  (list load-code (arm64:str :x0 :heap :offset 0)
                                        (arm64:ldr :x0 :heap :offset 8) nil)))))
                       (build-list (cdr offs)
                        (append acc load-code))))))
        (labels ((gen-cons-chain (offs)
                   (if (null offs)
                       (movz 0 0)
                       (let* ((off (car offs))
                              (off8 (* off 8))
                              (rest-code
                               (gen-cons-chain (cdr offs))))
                         (append-all
                          (list rest-code (arm64:str :x0 :heap :offset 8)
                                (arm64:sub :x1 :env off8 :imm t)
                                (arm64:ldr :x0 :x1 :offset 0) (arm64:str :x0 :heap :offset 0)
                                (arm64:mov :x0 :heap) (arm64:add :x0 :x0 1 :imm t)
                                (arm64:add :heap :heap 16 :imm t)))))))
          (gen-cons-chain free-offsets)))))

(defun codegen-binop
       (left-ir right-ir op-instrs
        rtaddrs fnoffs td)
  "Generate code for binary operation"
  (let* ((left-may-call
          (ir-may-call left-ir))
         (right-may-call
          (ir-may-call right-ir)))
    (cond
     (left-may-call
      (let* ((xs (temp-slot td))
             (nd (+ td 1))
             (lc
              (codegen left-ir rtaddrs
               fnoffs nd))
             (rc
              (codegen right-ir rtaddrs
               fnoffs (+ nd 1))))
        (append-all
         (list (arm64:str :closure :sp :offset xs) lc
               (save-temp nd)
               (arm64:ldr :closure :sp :offset xs) rc
               (arm64:mov :x1 :x0) (load-temp 0 nd)
               op-instrs))))
     (right-may-call
      (let* ((xs (temp-slot td))
             (nd (+ td 1))
             (lc
              (codegen left-ir rtaddrs
               fnoffs nd))
             (rc
              (codegen right-ir rtaddrs
               fnoffs (+ nd 1))))
        (append-all
         (list lc (save-temp nd)
               (arm64:str :closure :sp :offset xs) rc
               (arm64:mov :x1 :x0) (load-temp 0 nd)
               (arm64:ldr :closure :sp :offset xs)
               op-instrs))))
     (t
      (let* ((nd (+ td 1))
             (lc
              (codegen left-ir rtaddrs
               fnoffs td))
             (rc
              (codegen right-ir rtaddrs
               fnoffs nd)))
        (append-all
         (list lc (save-temp td)
               rc (arm64:mov :x1 :x0)
               (load-temp 0 td)
               op-instrs)))))))

(defun codegen
       (ir rtaddrs fnoffs
        td)
  "Generate ARM64 code from IR"
  (cond
   ((has-tag ir 'lit)
    (let* ((v (cadr ir))
           (tg (ash v 4)))
      (if (and (>= tg 0) (< tg 65536))
          (movz 0 tg)
          (load-addr 0 tg))))
   ((has-tag ir 'nil-ir)
    (movz 0 6))
   ((has-tag ir 'sym-lit)
    (let* ((name (cadr ir))
           (len (string-length name))
           (total-size (logand (+ len 8 15) (lognot 15))))
      (gen-symbol-lit name len
       total-size)))
   ((has-tag ir 'str-lit)
    (let* ((str (cadr ir))
           (len (string-length str))
           (total-size (logand (+ len 8 15) (lognot 15))))
      (gen-string-lit str len
       total-size)))
   ((has-tag ir 'var-ref)
    (let* ((off (cadr ir))
           (off8 (* off 8)))
      (append (arm64:sub :x1 :env off8 :imm t) (arm64:ldr :x0 :x1 :offset 0))))
   ((has-tag ir 'setq-ir)
    (let* ((off (cadr ir))
           (val-ir (caddr ir))
           (off8 (* off 8))
           (val-code
            (codegen val-ir rtaddrs
             fnoffs td)))
      (append-all
       (list val-code (arm64:sub :x1 :env off8 :imm t)
             (arm64:str :x0 :x1 :offset 0)))))
   ((has-tag ir 'add)
    (codegen-binop (cadr ir) (caddr ir)
     (arm64:add :x0 :x0 :x1) rtaddrs fnoffs
     td))
   ((has-tag ir 'sub)
    (codegen-binop (cadr ir) (caddr ir)
     (arm64:sub :x0 :x0 :x1) rtaddrs fnoffs
     td))
   ((has-tag ir 'mul)
    (codegen-binop (cadr ir) (caddr ir)
     (append (arm64:lsr :x1 :x1 4 :imm t) (arm64:mul :x0 :x0 :x1)) rtaddrs
     fnoffs td))
   ((has-tag ir 'div)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:lsr :x0 :x0 4 :imm t) (arm64:lsr :x1 :x1 4 :imm t) (arm64:sdiv :x0 :x0 :x1)
            (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'mod)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:lsr :x9 :x0 4 :imm t) (arm64:lsr :x10 :x1 4 :imm t) (arm64:sdiv :x11 :x9 :x10)
            (arm64:mul :x11 :x11 :x10) (arm64:sub :x0 :x9 :x11) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cmp-eq)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:cmp :x0 :x1) (arm64:cset :x0 arm64:+eq+) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cmp-lt)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:cmp :x0 :x1) (arm64:cset :x0 arm64:+lt+) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cmp-gt)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:cmp :x0 :x1) (arm64:cset :x0 arm64:+gt+) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cmp-le)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:cmp :x0 :x1) (arm64:cset :x0 arm64:+le+) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cmp-ge)
    (codegen-binop (cadr ir) (caddr ir)
     (append-all
      (list (arm64:cmp :x0 :x1) (arm64:cset :x0 arm64:+ge+) (arm64:lsl :x0 :x0 4 :imm t)))
     rtaddrs fnoffs td))
   ((has-tag ir 'cons-ir)
    (let* ((car-ir (cadr ir))
           (cdr-ir (caddr ir))
           (xs (temp-slot td))
           (cs (temp-slot (+ td 1)))
           (nd (+ td 2))
           (car-code
            (codegen car-ir rtaddrs
             fnoffs nd))
           (cdr-code
            (codegen cdr-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list (arm64:str :closure :sp :offset xs) car-code
             (arm64:str :x0 :sp :offset cs)
             (arm64:ldr :closure :sp :offset xs) cdr-code
             (gc-trigger-code) (arm64:str :x0 :heap :offset 8)
             (arm64:ldr :x0 :sp :offset cs) (arm64:str :x0 :heap :offset 0)
             (arm64:mov :x0 :heap) (arm64:add :x0 :x0 1 :imm t) (arm64:add :heap :heap 16 :imm t)
             (gc-trigger-code)
             (arm64:ldr :closure :sp :offset xs)))))
   ((has-tag ir 'car-ir)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append inner-code
              (append (arm64:sub :x0 :x0 1 :imm t) (arm64:ldr :x0 :x0 :offset 0)))))
   ((has-tag ir 'cdr-ir)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append inner-code
              (append (arm64:sub :x0 :x0 1 :imm t) (arm64:ldr :x0 :x0 :offset 8)))))
   ((has-tag ir 'string-length-ir)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append-all
       (list inner-code (arm64:sub :x0 :x0 4 :imm t)
             (arm64:ldr :x0 :x0 :offset 0) (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'string-ref-ir)
    (let* ((str-ir (cadr ir))
           (idx-ir (caddr ir))
           (spill-off (spill-base td))
           (str-code
            (codegen str-ir rtaddrs
             fnoffs td))
           (idx-code
            (codegen idx-ir rtaddrs
             fnoffs td)))
      (append-all
       (list str-code (arm64:str :x0 :sp :offset spill-off)
             idx-code (arm64:ldr :x1 :sp :offset spill-off)
             (arm64:lsr :x0 :x0 4 :imm t) (arm64:sub :x1 :x1 4 :imm t) (arm64:add :x1 :x1 8 :imm t)
             (arm64:add :x1 :x1 :x0) (arm64:ldrb :x0 :x1 0) (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'string-concat-ir)
    (let* ((str1-ir (cadr ir))
           (str2-ir (caddr ir))
           (spill1 (spill-base td))
           (spill2 (+ spill1 8))
           (spill3 (+ spill1 16))
           (str1-code
            (codegen str1-ir rtaddrs
             fnoffs td))
           (str2-code
            (codegen str2-ir rtaddrs
             fnoffs (+ td 1))))
      (append-all
       (list str1-code (arm64:str :x0 :sp :offset spill1)
             str2-code (arm64:str :x0 :sp :offset spill2)
             (arm64:ldr :x1 :sp :offset spill1) (arm64:sub :x1 :x1 4 :imm t)
             (arm64:ldr :x9 :x1 :offset 0) (arm64:ldr :x2 :sp :offset spill2)
             (arm64:sub :x2 :x2 4 :imm t) (arm64:ldr :x10 :x2 :offset 0) (arm64:add :x11 :x9 :x10)
             (arm64:str :x11 :sp :offset spill3)
             (gc-trigger-code) (arm64:str :x11 :heap :offset 0)
             (arm64:mov :x0 :heap) (arm64:add :x12 :x11 23 :imm t)
             (arm64:and* :x12 :x12 -16 :imm t) (arm64:add :heap :heap :x12)
             (arm64:ldr :x1 :sp :offset spill1) (arm64:sub :x1 :x1 4 :imm t)
             (arm64:add :x1 :x1 8 :imm t) (arm64:add :x3 :x0 8 :imm t)
             (gen-memcpy-inline 9)
             (arm64:ldr :x1 :sp :offset spill2) (arm64:sub :x1 :x1 4 :imm t)
             (arm64:add :x1 :x1 8 :imm t) (arm64:mov :x9 :x10)
             (gen-memcpy-inline 9) (arm64:add :x0 :x0 4 :imm t)))))
   ((has-tag ir 'string-equal-ir)
    (let* ((str1-ir (cadr ir))
           (str2-ir (caddr ir))
           (spill-off (spill-base td))
           (str1-code
            (codegen str1-ir rtaddrs
             fnoffs td))
           (str2-code
            (codegen str2-ir rtaddrs
             fnoffs td)))
      (append-all
       (list str1-code (arm64:str :x0 :sp :offset spill-off)
             str2-code (arm64:mov :x9 :x0)
             (arm64:ldr :x0 :sp :offset spill-off) (arm64:cmp :x0 6 :imm t)
             (arm64:b.eq 4) (arm64:cmp :x9 6 :imm t) (arm64:b.eq 25) (arm64:b 5)
             (arm64:cmp :x9 6 :imm t) (arm64:b.ne 22) (movz 0 16) (arm64:b 21)
             (arm64:and* :x2 :x9 -16 :imm t) (arm64:and* :x1 :x0 -16 :imm t)
             (arm64:ldr :x3 :x1 :offset 0) (arm64:ldr :x4 :x2 :offset 0) (arm64:cmp :x3 :x4)
             (arm64:b.ne 14) (arm64:add :x1 :x1 8 :imm t) (arm64:add :x2 :x2 8 :imm t)
             (movz 4 0) (arm64:cmp :x4 :x3) (arm64:b.ge 7)
             (arm64:ldrb :x5 :x1 :x4 :reg t) (arm64:ldrb :x6 :x2 :x4 :reg t) (arm64:cmp :x5 :x6)
             (arm64:b.ne 5) (arm64:add :x4 :x4 1 :imm t) (arm64:b -7) (movz 0 16)
             (arm64:b 2) (movz 0 6)))))
   ((has-tag ir 'make-vector-ir)
    (let* ((size-ir (cadr ir))
           (sc
            (codegen size-ir rtaddrs
             fnoffs td)))
      (append-all
       (list sc (gc-trigger-code) (arm64:lsr :x1 :x0 4 :imm t)
             (arm64:str :x1 :heap :offset 0) (arm64:lsr :x1 :x0 1 :imm t)
             (arm64:add :x1 :x1 8 :imm t) (arm64:add :x1 :x1 15 :imm t)
             (arm64:and* :x1 :x1 -16 :imm t) (arm64:mov :x0 :heap) (arm64:add :heap :heap :x1)
             (movz 1 3) (arm64:orr :x0 :x0 :x1)
             (gc-trigger-code)))))
   ((has-tag ir 'vector-set-ir)
    (let* ((vec-ir (cadr ir))
           (idx-ir (caddr ir))
           (val-ir (cadddr ir))
           (xs (temp-slot td))
           (xs2 (temp-slot (+ td 1)))
           (nd (+ td 2))
           (vc
            (codegen vec-ir rtaddrs
             fnoffs nd))
           (sv (arm64:str :x0 :sp :offset xs))
           (ic
            (codegen idx-ir rtaddrs
             fnoffs nd))
           (si (arm64:str :x0 :sp :offset xs2))
           (vlc
            (codegen val-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list vc sv ic si
             vlc (arm64:ldr :x1 :sp :offset xs)
             (arm64:ldr :x2 :sp :offset xs2) (arm64:sub :x1 :x1 3 :imm t)
             (arm64:mov :x3 :x1) (arm64:lsr :x2 :x2 1 :imm t) (arm64:add :x2 :x2 8 :imm t)
             (arm64:add :x1 :x1 :x2) (arm64:str :x0 :x1 :offset 0)
             (gen-write-barrier-code 3)))))
   ((has-tag ir 'vector-ref-ir)
    (let* ((vec-ir (cadr ir))
           (idx-ir (caddr ir))
           (xs (temp-slot td))
           (nd (+ td 1))
           (vc
            (codegen vec-ir rtaddrs
             fnoffs nd))
           (sv (arm64:str :x0 :sp :offset xs))
           (ic
            (codegen idx-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list vc sv ic
             (arm64:ldr :x1 :sp :offset xs) (arm64:sub :x1 :x1 3 :imm t)
             (arm64:lsr :x0 :x0 1 :imm t) (arm64:add :x0 :x0 8 :imm t) (arm64:add :x1 :x1 :x0)
             (arm64:ldr :x0 :x1 :offset 0)))))
   ((has-tag ir 'vector-length-ir)
    (let* ((vec-ir (cadr ir))
           (vc
            (codegen vec-ir rtaddrs
             fnoffs td)))
      (append-all
       (list vc (arm64:sub :x0 :x0 3 :imm t) (arm64:ldr :x0 :x0 :offset 0)
             (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'buffer-byte-ref-ir)
    (let* ((vec-ir (cadr ir))
           (idx-ir (caddr ir))
           (xs (temp-slot td))
           (nd (+ td 1))
           (vc
            (codegen vec-ir rtaddrs
             fnoffs nd))
           (sv (arm64:str :x0 :sp :offset xs))
           (ic
            (codegen idx-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list vc sv ic
             (arm64:ldr :x1 :sp :offset xs) (arm64:and* :x1 :x1 -16 :imm t)
             (arm64:lsr :x0 :x0 4 :imm t) (arm64:add :x0 :x0 8 :imm t) (arm64:add :x1 :x1 :x0)
             (arm64:ldrb :x0 :x1 0) (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'buffer-byte-set-ir)
    (let* ((vec-ir (cadr ir))
           (idx-ir (caddr ir))
           (val-ir (cadddr ir))
           (xs (temp-slot td))
           (xs2 (temp-slot (+ td 1)))
           (nd (+ td 2))
           (vc
            (codegen vec-ir rtaddrs
             fnoffs nd))
           (sv (arm64:str :x0 :sp :offset xs))
           (ic
            (codegen idx-ir rtaddrs
             fnoffs nd))
           (si (arm64:str :x0 :sp :offset xs2))
           (vlc
            (codegen val-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list vc sv ic si
             vlc (arm64:ldr :x1 :sp :offset xs)
             (arm64:ldr :x2 :sp :offset xs2) (arm64:and* :x1 :x1 -16 :imm t)
             (arm64:lsr :x2 :x2 4 :imm t) (arm64:lsr :x0 :x0 4 :imm t) (arm64:add :x2 :x2 8 :imm t)
             (arm64:add :x1 :x1 :x2) (arm64:strb :x0 :x1 0) (movz 0 6)))))
   ((has-tag ir 'make-string-from-vector-ir)
    (let* ((seq-ir (cadr ir))
           (sc
            (codegen seq-ir rtaddrs
             fnoffs td)))
      (append-all
       (list sc (arm64:and* :x6 :x0 15 :imm t) (arm64:cmp :x6 1 :imm t)
             (arm64:b.eq 28) (arm64:sub :x1 :x0 3 :imm t) (arm64:ldr :x5 :x1 :offset 0)
             (gc-trigger-code) (arm64:str :x5 :heap :offset 0)
             (arm64:add :x4 :x5 23 :imm t) (arm64:lsr :x4 :x4 4 :imm t)
             (arm64:lsl :x4 :x4 4 :imm t) (arm64:mov :x0 :heap) (arm64:add :heap :heap :x4)
             (arm64:add :x2 :x0 8 :imm t) (movz 3 0) (arm64:cmp :x3 :x5)
             (arm64:b.ge 9) (arm64:lsl :x4 :x3 3 :imm t) (arm64:add :x4 :x4 8 :imm t)
             (arm64:add :x4 :x1 :x4) (arm64:ldr :x4 :x4 :offset 0) (arm64:lsr :x4 :x4 4 :imm t)
             (strb-reg 4 2 3) (arm64:add :x3 :x3 1 :imm t) (arm64:b -9)
             (movz 4 4) (arm64:orr :x0 :x0 :x4) (arm64:b 33) (arm64:mov :x1 :x0)
             (movz 5 0) (arm64:cmp :x1 6 :imm t) (arm64:b.eq 5)
             (arm64:add :x5 :x5 1 :imm t) (arm64:sub :x4 :x1 1 :imm t)
             (arm64:ldr :x1 :x4 :offset 8) (arm64:b -5) (gc-trigger-code)
             (arm64:str :x5 :heap :offset 0) (arm64:add :x4 :x5 23 :imm t)
             (arm64:lsr :x4 :x4 4 :imm t) (arm64:lsl :x4 :x4 4 :imm t) (arm64:mov :x6 :heap)
             (arm64:add :heap :heap :x4) (arm64:add :x2 :x6 8 :imm t) (arm64:mov :x1 :x0)
             (movz 3 0) (arm64:cmp :x1 6 :imm t) (arm64:b.eq 8)
             (arm64:sub :x4 :x1 1 :imm t) (arm64:ldr :x7 :x4 :offset 0)
             (arm64:lsr :x7 :x7 4 :imm t) (strb-reg 7 2 3)
             (arm64:add :x3 :x3 1 :imm t) (arm64:ldr :x1 :x4 :offset 8) (arm64:b -8)
             (arm64:mov :x0 :x6) (movz 4 4) (arm64:orr :x0 :x0 :x4)))))
   ((has-tag ir 'setcar-ir)
    (let* ((cons-ir (cadr ir))
           (val-ir (caddr ir))
           (spill-off (spill-base td))
           (cons-code
            (codegen cons-ir rtaddrs
             fnoffs td))
           (val-code
            (codegen val-ir rtaddrs
             fnoffs td)))
      (append-all
       (list cons-code (arm64:str :x0 :sp :offset spill-off)
             val-code (arm64:ldr :x1 :sp :offset spill-off)
             (arm64:sub :x1 :x1 1 :imm t) (arm64:str :x0 :x1 :offset 0)
             (gen-write-barrier-code 1)))))
   ((has-tag ir 'setcdr-ir)
    (let* ((cons-ir (cadr ir))
           (val-ir (caddr ir))
           (spill-off (spill-base td))
           (cons-code
            (codegen cons-ir rtaddrs
             fnoffs td))
           (val-code
            (codegen val-ir rtaddrs
             fnoffs td)))
      (append-all
       (list cons-code (arm64:str :x0 :sp :offset spill-off)
             val-code (arm64:ldr :x1 :sp :offset spill-off)
             (arm64:sub :x1 :x1 1 :imm t) (arm64:str :x0 :x1 :offset 8)
             (gen-write-barrier-code 1)))))
   ((has-tag ir 'symbol-name-ir)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append inner-code
              (append (arm64:sub :x0 :x0 2 :imm t) (arm64:add :x0 :x0 4 :imm t)))))
   ((has-tag ir 'make-symbol-ir)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append inner-code
              (append (arm64:sub :x0 :x0 4 :imm t) (arm64:add :x0 :x0 2 :imm t)))))
   ((has-tag ir 'get-tag)
    (let ((inner-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append inner-code
              (append (arm64:and* :x0 :x0 15 :imm t) (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'if-ir)
    (let* ((cond-ir (cadr ir))
           (then-ir (caddr ir))
           (else-ir (cadddr ir))
           (cond-code
            (codegen cond-ir rtaddrs
             fnoffs td))
           (then-code
            (codegen then-ir rtaddrs
             fnoffs td))
           (else-code
            (codegen else-ir rtaddrs
             fnoffs td))
           (else-size (code-size else-code))
           (then-size (code-size then-code)))
      (append-all
       (list cond-code (arm64:cmp :x0 0 :imm t)
             (arm64:b.eq (ash (+ then-size 8) -2)) then-code
             (arm64:b (ash (+ else-size 4) -2)) else-code))))
   ((has-tag ir 'while-ir)
    (let* ((test-ir (cadr ir))
           (body-ir (caddr ir))
           (test-code
            (codegen test-ir rtaddrs
             fnoffs td))
           (body-code
            (codegen body-ir rtaddrs
             fnoffs td))
           (test-size (code-size test-code))
           (body-size (code-size body-code)))
      (append-all
       (list test-code (arm64:cmp :x0 0 :imm t)
             (arm64:b.eq (ash (+ body-size 8) -2)) body-code
             (arm64:b
              (ash (- 0 (+ test-size 8 body-size)) -2))))))
   ((has-tag ir 'loop-ir)
    (let* ((body-ir (cadr ir))
           (body-code
            (codegen body-ir rtaddrs
             fnoffs td)))
      (cons (list :loop-start) body-code)))
   ((has-tag ir 'continue-ir)
    (let* ((arg-irs (cadr ir))
           (nargs (length arg-irs)))
      (labels ((gen-args
                   (irs idx acc)
                 (if (null irs)
                     acc
                     (let* ((arg-code
                             (codegen (car irs)
                              rtaddrs fnoffs
                              td))
                            (slot-off (+ 64 (* idx 8)))
                            (store
                             (arm64:str :x0 :sp :offset slot-off)))
                       (gen-args (cdr irs)
                        (+ idx 1)
                        (append-all
                         (list acc arg-code
                               store))))))
               (gen-copies (idx)
                 (if (>= idx nargs)
                     nil
                     (let* ((temp-off (+ 64 (* idx 8)))
                            (common-lisp:load
                             (arm64:ldr :x9 :sp :offset temp-off))
                            (param-off (* idx 8))
                            (store-param
                             (append (arm64:sub :x10 :env param-off :imm t)
                                     (arm64:str :x9 :x10 :offset 0))))
                       (append-all
                        (list common-lisp:load store-param
                              (gen-copies (+ idx 1))))))))
        (let ((args-code
               (gen-args arg-irs 0 nil))
              (copy-code (gen-copies 0)))
          (append-all
           (list args-code copy-code
                 (list (list :loop-continue))))))))
   ((has-tag ir 'block-ir)
    (let* ((block-id (cadr ir))
           (body-ir (caddr ir))
           (body-code
            (codegen body-ir rtaddrs
             fnoffs td)))
      (append-all
       (list (list (list :block-start block-id)) body-code
             (list (list :block-end block-id))))))
   ((has-tag ir 'return-from-ir)
    (let* ((block-id (cadr ir))
           (value-ir (caddr ir))
           (value-code
            (codegen value-ir rtaddrs
             fnoffs td)))
      (append-all
       (list value-code (list (list :return-from block-id))))))
   ((has-tag ir 'let-ir)
    (let* ((vals (cadr ir))
           (body-ir (caddr ir))
           (offs (nth 3 (cdr ir)))
           (xs (temp-slot td))
           (nd (+ td 1))
           (save-x24 (arm64:str :closure :sp :offset xs)))
      (labels ((gen-binds
                   (vs os acc)
                 (if (null vs)
                     acc
                     (let* ((restore-x24
                             (if acc
                                 (arm64:ldr :closure :sp :offset xs)
                                 nil))
                            (val-code
                             (codegen (car vs)
                              rtaddrs fnoffs
                              nd))
                            (store-code
                             (append (arm64:sub :x1 :env (* (car os) 8) :imm t)
                                     (arm64:str :x0 :x1 :offset 0))))
                       (gen-binds (cdr vs)
                        (cdr os)
                        (append-all
                         (list acc restore-x24
                               val-code store-code)))))))
        (let* ((bindings-code
                (gen-binds vals offs nil))
               (restore-final
                (arm64:ldr :closure :sp :offset xs))
               (body-code
                (codegen body-ir rtaddrs
                 fnoffs nd)))
          (append-all
           (list save-x24 bindings-code
                 restore-final body-code))))))
   ((has-tag ir 'progn-ir)
    (let ((forms (cadr ir)))
      (codegen-progn-forms forms rtaddrs
       fnoffs td)))
   ((has-tag ir 'sys-exit-ir)
    (let ((arg-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append arg-code
              (append (arm64:lsr :x0 :x0 4 :imm t) (list (list :extern-call "_exit"))))))
   ((has-tag ir 'get-cmdline-args-ir)
    (let ((slot0 (temp-slot td))
          (slot1 (temp-slot (+ td 1)))
          (slot2 (temp-slot (+ td 2)))
          (slot3 (temp-slot (+ td 3)))
          (slot4 (temp-slot (+ td 4))))
      (append-all
       (list (arm64:str :env :sp :offset slot0) (arm64:ldr :x9 :gc :offset 64)
             (arm64:ldr :x10 :gc :offset 72) (movz 0 6)
             (arm64:subs :x11 :x9 1 :imm t) (arm64:b.lt 44) (arm64:ldr-reg :x12 :x10 :x11 :shift 3)
             (arm64:mov :x13 :x12) (movz 14 0) (arm64:ldrb :x15 :x13 0)
             (arm64:cbz :x15 4) (arm64:add :x13 :x13 1 :imm t) (arm64:add :x14 :x14 1 :imm t)
             (arm64:b -4) (arm64:str :x0 :sp :offset slot1)
             (arm64:str :x11 :sp :offset slot2)
             (arm64:str :x10 :sp :offset slot3)
             (arm64:str :x14 :sp :offset slot4) (arm64:mov :env :heap)
             (arm64:lsl :x15 :x14 4 :imm t) (arm64:str :x15 :heap :offset 0)
             (arm64:add :x16 :heap 8 :imm t) (arm64:mov :x17 :x12) (arm64:mov :x18 :x14)
             (arm64:cbz :x18 5) (arm64:ldrb-post :x19 :x17 1) (arm64:strb-post :x19 :x16 1)
             (arm64:sub :x18 :x18 1 :imm t) (arm64:b -4)
             (arm64:ldr :x14 :sp :offset slot4) (arm64:add :x15 :x14 :x15 :imm t)
             (arm64:and* :x15 :x15 -8 :imm t) (arm64:add :heap :heap :x15)
             (arm64:orr-imm :x21 :env 4) (arm64:ldr :x0 :sp :offset slot1)
             (arm64:str :x21 :heap :offset 0) (arm64:str :x0 :heap :offset 8)
             (arm64:orr-imm :x0 :heap 1) (arm64:add :heap :heap 16 :imm t)
             (arm64:ldr :x11 :sp :offset slot2)
             (arm64:ldr :x10 :sp :offset slot3) (arm64:subs :x11 :x11 1 :imm t)
             (arm64:b.ge -39) (arm64:ldr :env :sp :offset slot0)))))
   ((has-tag ir 'sys-open-ir)
    (let* ((path-ir (cadr ir))
           (flags-ir (caddr ir))
           (mode-ir (cadddr ir))
           (nd (+ td 3))
           (path-code
            (codegen path-ir rtaddrs
             fnoffs nd))
           (save-path
            (arm64:str :x0 :sp :offset (temp-slot td)))
           (flags-code
            (codegen flags-ir rtaddrs
             fnoffs nd))
           (save-flags
            (arm64:str :x0 :sp :offset (temp-slot (+ td 1))))
           (mode-code
            (codegen mode-ir rtaddrs
             fnoffs nd))
           (save-mode
            (arm64:str :x0 :sp :offset (temp-slot (+ td 2)))))
      (append-all
       (list path-code save-path flags-code
             save-flags mode-code save-mode
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:and* :x0 :x0 -8 :imm t) (arm64:add :x0 :x0 8 :imm t)
             (arm64:ldr :x1 :sp :offset (temp-slot (+ td 1)))
             (arm64:lsr :x1 :x1 4 :imm t)
             (arm64:ldr :x2 :sp :offset (temp-slot (+ td 2)))
             (arm64:lsr :x2 :x2 4 :imm t) (list (list :extern-call "_open"))
             (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'sys-write-ir)
    (let* ((fd-ir (cadr ir))
           (buf-ir (caddr ir))
           (len-ir (cadddr ir))
           (nd (+ td 3))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs nd))
           (save-fd
            (arm64:str :x0 :sp :offset (temp-slot td)))
           (buf-code
            (codegen buf-ir rtaddrs
             fnoffs nd))
           (save-buf
            (arm64:str :x0 :sp :offset (temp-slot (+ td 1))))
           (len-code
            (codegen len-ir rtaddrs
             fnoffs nd))
           (save-len
            (arm64:str :x0 :sp :offset (temp-slot (+ td 2)))))
      (append-all
       (list fd-code save-fd buf-code
             save-buf len-code save-len
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:ldr :x1 :sp :offset (temp-slot (+ td 1)))
             (arm64:and* :x1 :x1 -8 :imm t) (arm64:add :x1 :x1 8 :imm t)
             (arm64:ldr :x2 :sp :offset (temp-slot (+ td 2)))
             (arm64:lsr :x2 :x2 4 :imm t) (list (list :extern-call "_write"))
             (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'sys-write-char-ir)
    (let* ((fd-ir (cadr ir))
           (char-ir (caddr ir))
           (nd (+ td 2))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs nd))
           (save-fd
            (arm64:str :x0 :sp :offset (temp-slot td)))
           (char-code
            (codegen char-ir rtaddrs
             fnoffs nd))
           (save-char
            (arm64:str :x0 :sp :offset (temp-slot (+ td 1)))))
      (append-all
       (list fd-code save-fd char-code
             save-char
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:ldr :x3 :sp :offset (temp-slot (+ td 1)))
             (arm64:lsr :x3 :x3 4 :imm t)
             (arm64:strb :x3 :sp (temp-slot (+ td 1)))
             (arm64:add :x1 :sp (temp-slot (+ td 1)) :imm t)
             (arm64:movz :x2 1) (list (list :extern-call "_write"))
             (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'sys-read-byte-ir)
    (let* ((fd-ir (cadr ir))
           (nd (+ td 1))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list fd-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:add :x1 :sp (temp-slot td) :imm t)
             (arm64:movz :x2 1) (list (list :extern-call "_read")) (arm64:cmp :x0 1 :imm t)
             (arm64:b.lt 4) (arm64:ldrb :x0 :sp (temp-slot td))
             (arm64:lsl :x0 :x0 4 :imm t) (arm64:b 2) (arm64:sub :x0 :xzr 16 :imm t)))))
   ((has-tag ir 'sys-read-ir)
    (let* ((fd-ir (cadr ir))
           (buf-ir (caddr ir))
           (len-ir (cadddr ir))
           (nd (+ td 3))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs nd))
           (save-fd
            (arm64:str :x0 :sp :offset (temp-slot td)))
           (buf-code
            (codegen buf-ir rtaddrs
             fnoffs nd))
           (save-buf
            (arm64:str :x0 :sp :offset (temp-slot (+ td 1))))
           (len-code
            (codegen len-ir rtaddrs
             fnoffs nd))
           (save-len
            (arm64:str :x0 :sp :offset (temp-slot (+ td 2)))))
      (append-all
       (list fd-code save-fd buf-code
             save-buf len-code save-len
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:ldr :x1 :sp :offset (temp-slot (+ td 1)))
             (arm64:and* :x1 :x1 -8 :imm t) (arm64:add :x1 :x1 8 :imm t)
             (arm64:ldr :x2 :sp :offset (temp-slot (+ td 2)))
             (arm64:lsr :x2 :x2 4 :imm t) (list (list :extern-call "_read"))
             (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'sys-close-ir)
    (let* ((fd-ir (cadr ir))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs td)))
      (append-all
       (list fd-code (arm64:lsr :x0 :x0 4 :imm t)
             (list (list :extern-call "_close")) (arm64:lsl :x0 :x0 4 :imm t)))))
   ((has-tag ir 'mmap-ir)
    (let* ((addr-ir (cadr ir))
           (len-ir (caddr ir))
           (prot-ir (cadddr ir))
           (flags-ir (nth 4 ir))
           (fd-ir (nth 5 ir))
           (offset-ir (nth 6 ir))
           (nd (+ td 6))
           (addr-code
            (codegen addr-ir rtaddrs
             fnoffs nd))
           (len-code
            (codegen len-ir rtaddrs
             fnoffs nd))
           (prot-code
            (codegen prot-ir rtaddrs
             fnoffs nd))
           (flags-code
            (codegen flags-ir rtaddrs
             fnoffs nd))
           (fd-code
            (codegen fd-ir rtaddrs
             fnoffs nd))
           (offset-code
            (codegen offset-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list addr-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot td))
             len-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 1)))
             prot-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 2)))
             flags-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 3)))
             fd-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 4)))
             offset-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 5)))
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:ldr :x1 :sp :offset (temp-slot (+ td 1)))
             (arm64:ldr :x2 :sp :offset (temp-slot (+ td 2)))
             (arm64:ldr :x3 :sp :offset (temp-slot (+ td 3)))
             (arm64:ldr :x4 :sp :offset (temp-slot (+ td 4)))
             (arm64:ldr :x5 :sp :offset (temp-slot (+ td 5)))
             (list (list :extern-call "_mmap"))))))
   ((has-tag ir 'munmap-ir)
    (let* ((addr-ir (cadr ir))
           (len-ir (caddr ir))
           (nd (+ td 2))
           (addr-code
            (codegen addr-ir rtaddrs
             fnoffs nd))
           (len-code
            (codegen len-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list addr-code
             (arm64:str :x0 :sp :offset (temp-slot td))
             len-code (arm64:lsr :x0 :x0 4 :imm t) (arm64:mov :x1 :x0)
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (list (list :extern-call "_munmap"))))))
   ((has-tag ir 'mmap-jit-ir)
    (let* ((size-ir (cadr ir))
           (size-code
            (codegen size-ir rtaddrs
             fnoffs td)))
      (append-all
       (list size-code (arm64:lsr :x1 :x0 4 :imm t) (movz 0 0)
             (movz 2 7) (movz 3 6146) (arm64:movz :x4 65535)
             (arm64:movk :x4 65535 :lsl 16) (arm64:movk :x4 65535 :lsl 32)
             (arm64:movk :x4 65535 :lsl 48) (movz 5 0)
             (list (list :extern-call "_mmap"))))))
   ((has-tag ir
     'pthread-jit-write-protect-np-ir)
    (let* ((enabled-ir (cadr ir))
           (enabled-code
            (codegen enabled-ir rtaddrs
             fnoffs td)))
      (append-all
       (list enabled-code (arm64:lsr :x0 :x0 4 :imm t)
             (list (list :extern-call "_pthread_jit_write_protect_np"))))))
   ((has-tag ir 'sys-dcache-flush-ir)
    (let* ((start-ir (cadr ir))
           (size-ir (caddr ir))
           (nd (+ td 2))
           (start-code
            (codegen start-ir rtaddrs
             fnoffs nd))
           (size-code
            (codegen size-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list start-code
             (arm64:str :x0 :sp :offset (temp-slot td))
             size-code (arm64:lsr :x0 :x0 4 :imm t) (arm64:mov :x1 :x0)
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (list (list :extern-call "_sys_dcache_flush"))))))
   ((has-tag ir 'sys-icache-invalidate-ir)
    (let* ((start-ir (cadr ir))
           (size-ir (caddr ir))
           (nd (+ td 2))
           (start-code
            (codegen start-ir rtaddrs
             fnoffs nd))
           (size-code
            (codegen size-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list start-code
             (arm64:str :x0 :sp :offset (temp-slot td))
             size-code (arm64:lsr :x0 :x0 4 :imm t) (arm64:mov :x1 :x0)
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (list (list :extern-call "_sys_icache_invalidate"))))))
   ((has-tag ir 'funcall-ptr-ir)
    (let* ((ptr-ir (cadr ir))
           (ptr-code
            (codegen ptr-ir rtaddrs
             fnoffs td)))
      (append-all (list ptr-code (arm64:blr :x0)))))
   ((has-tag ir 'mem-set-byte-ir)
    (let* ((ptr-ir (cadr ir))
           (offset-ir (caddr ir))
           (byte-ir (cadddr ir))
           (nd (+ td 3))
           (ptr-code
            (codegen ptr-ir rtaddrs
             fnoffs nd))
           (offset-code
            (codegen offset-ir rtaddrs
             fnoffs nd))
           (byte-code
            (codegen byte-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list ptr-code
             (arm64:str :x0 :sp :offset (temp-slot td))
             offset-code (arm64:lsr :x0 :x0 4 :imm t)
             (arm64:str :x0 :sp :offset (temp-slot (+ td 1)))
             byte-code (arm64:lsr :x0 :x0 4 :imm t) (arm64:mov :x3 :x0)
             (arm64:ldr :x1 :sp :offset (temp-slot (+ td 1)))
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:add :x0 :x0 :x1) (arm64:strb :x3 :x0 0)))))
   ((has-tag ir 'mem-load-64-ir)
    (let* ((ptr-ir (cadr ir))
           (offset-ir (caddr ir))
           (nd (+ td 2))
           (ptr-code
            (codegen ptr-ir rtaddrs
             fnoffs nd))
           (offset-code
            (codegen offset-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list ptr-code
             (arm64:str :x0 :sp :offset (temp-slot td))
             offset-code (arm64:lsr :x0 :x0 4 :imm t) (arm64:mov :x1 :x0)
             (arm64:ldr :x0 :sp :offset (temp-slot td))
             (arm64:add :x0 :x0 :x1) (arm64:ldr :x0 :x0 :offset 0)))))
   ((has-tag ir 'buffer-to-string-ir)
    (let* ((buf-ir (cadr ir))
           (len-ir (caddr ir))
           (buf-slot (temp-slot td))
           (nd (+ td 1))
           (buf-code
            (codegen buf-ir rtaddrs
             fnoffs nd))
           (len-code
            (codegen len-ir rtaddrs
             fnoffs nd)))
      (append-all
       (list buf-code (arm64:str :x0 :sp :offset buf-slot)
             len-code (arm64:lsr :x5 :x0 4 :imm t)
             (arm64:ldr :x1 :sp :offset buf-slot) (arm64:and* :x1 :x1 -8 :imm t)
             (arm64:add :x1 :x1 8 :imm t) (gc-trigger-code)
             (arm64:str :x5 :heap :offset 0) (arm64:add :x4 :x5 23 :imm t)
             (arm64:and* :x4 :x4 -16 :imm t) (arm64:mov :x0 :heap) (arm64:add :heap :heap :x4)
             (arm64:add :x2 :x0 8 :imm t) (movz 3 0) (arm64:cmp :x3 :x5)
             (arm64:b.ge 6) (arm64:add :x4 :x1 :x3) (arm64:ldrb :x4 :x4 0)
             (strb-reg 4 2 3) (arm64:add :x3 :x3 1 :imm t) (arm64:b -6)
             (movz 4 4) (arm64:orr :x0 :x0 :x4)))))
   ((has-tag ir 'call-fn)
    (let* ((fn-name (cadr ir))
           (args (caddr ir))
           (num-args (length args))
           (arg-code
            (codegen-call-args args rtaddrs
             fnoffs td))
           (load-code
            (gen-arg-loads num-args td)))
      (append-all
       (list arg-code load-code
             (list (list :call-fn fn-name))))))
   ;; Tail call to other function - runs epilogue then jumps (no BL, no return)
   ;; This enables mutual tail recursion without stack growth
   ((has-tag ir 'tail-call-fn)
    (let* ((fn-name (cadr ir))
           (args (caddr ir))
           (num-args (length args))
           (arg-code
            (codegen-call-args args rtaddrs
             fnoffs td))
           (load-code
            (gen-arg-loads num-args td))
           ;; Run epilogue to restore caller's registers and deallocate frame
           (epilogue (fn-fixed-epilogue)))
      (append-all
       (list arg-code load-code epilogue
             (list (list :tail-call-fn fn-name))))))
   ((has-tag ir 'lambda-ref)
    (let* ((name (cadr ir))
           (free-offsets (caddr ir))
           (fn-entry
            (lookup-string name fnoffs))
           (fn-offset
            (if fn-entry
                (cdr fn-entry)
                0)))
      (if (null free-offsets)
          (append-all
           (list (gc-trigger-code)
                 (load-addr-8 0 (ash fn-offset 4))
                 (arm64:str :x0 :heap :offset 0) (movz 0 0)
                 (arm64:str :x0 :heap :offset 8) (arm64:mov :x0 :heap) (arm64:add :x0 :x0 5 :imm t)
                 (arm64:add :heap :heap 16 :imm t) (gc-trigger-code)))
          (let* ((capture-code
                  (build-captures free-offsets))
                 (xs (temp-slot td)))
            (append-all
             (list (arm64:str :closure :sp :offset xs)
                   capture-code (gc-trigger-code)
                   (arm64:str :x0 :heap :offset 8)
                   (load-addr-8 0 (ash fn-offset 4))
                   (arm64:str :x0 :heap :offset 0) (arm64:mov :x0 :heap)
                   (arm64:add :x0 :x0 5 :imm t) (arm64:add :heap :heap 16 :imm t)
                   (gc-trigger-code)
                   (arm64:ldr :closure :sp :offset xs)))))))
   ((has-tag ir 'fn-ref-ir)
    (let* ((name (cadr ir))
           (fn-entry
            (lookup-string name fnoffs))
           (fn-offset
            (if fn-entry
                (cdr fn-entry)
                0)))
      (append-all
       (list (gc-trigger-code)
             (load-addr-8 0 (ash fn-offset 4))
             (arm64:str :x0 :heap :offset 0) (movz 0 0)
             (arm64:str :x0 :heap :offset 8) (arm64:mov :x0 :heap) (arm64:add :x0 :x0 5 :imm t)
             (arm64:add :heap :heap 16 :imm t) (gc-trigger-code)))))
   ((has-tag ir 'funcall-ir)
    (let* ((fn-ir (cadr ir))
           (args (caddr ir))
           (num-args (length args))
           (fn-code
            (codegen fn-ir rtaddrs
             fnoffs td))
           (cs (temp-slot td))
           (nd (+ td 1))
           (arg-code
            (codegen-funcall-args args
             rtaddrs fnoffs nd 0))
           (load-code
            (gen-arg-loads num-args nd)))
      (append-all
       (list fn-code (arm64:str :x0 :sp :offset cs)
             arg-code load-code
             (arm64:ldr :x9 :sp :offset cs) (arm64:sub :x9 :x9 5 :imm t)
             (arm64:ldr :closure :x9 :offset 8) (arm64:ldr :x9 :x9 :offset 0)
             (arm64:lsr :x9 :x9 4 :imm t) (arm64:add :x9 :x9 :code-base) (arm64:blr :x9)))))
   ((has-tag ir 'get-intern-table-ir)
    (arm64:ldr :x0 :gc :offset 0))
   ((has-tag ir 'set-intern-table-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 0))))
   ((has-tag ir 'get-lambda-counter-ir)
    (arm64:ldr :x0 :gc :offset 8))
   ((has-tag ir 'set-lambda-counter-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 8))))
   ((has-tag ir 'get-symbol-counter-ir)
    (arm64:ldr :x0 :gc :offset 48))
   ((has-tag ir 'set-symbol-counter-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 48))))
   ((has-tag ir 'get-symbol-table-sym-ir)
    (arm64:ldr :x0 :gc :offset 56))
   ((has-tag ir 'set-symbol-table-sym-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 56))))
   ((has-tag ir 'get-packages-ir)
    (arm64:ldr :x0 :gc :offset 80))
   ((has-tag ir 'set-packages-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 80))))
   ((has-tag ir 'get-current-package-ir)
    (arm64:ldr :x0 :gc :offset 88))
   ((has-tag ir 'set-current-package-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 88))))
   ((has-tag ir 'get-global-vars-ir)
    (arm64:ldr :x0 :gc :offset 104))
   ((has-tag ir 'set-global-vars-ir)
    (let ((val-code
           (codegen (cadr ir) rtaddrs
            fnoffs td)))
      (append val-code (arm64:str :x0 :gc :offset 104))))
   ((has-tag ir 'loop-ir)
    (codegen (cadr ir) rtaddrs
     fnoffs td))
   ((has-tag ir 'continue-ir)
    (let* ((arg-irs (cadr ir))
           (nargs (length arg-irs))
           (args-code
            (codegen-tco-args arg-irs rtaddrs
             fnoffs td 0))
           (copy-code
            (codegen-tco-copy-args nargs 0))
           (branch-code (list (list :tco-branch))))
      (append-all
       (list args-code copy-code
             branch-code))))
   (t nil)))

(defun codegen-tco-args
       (arg-irs rtaddrs fnoffs
        td idx)
  "Evaluate args and store to temp slots for TCO continue.
   Uses temp slots at sp+0x40+idx*8 to avoid overwriting params."
  (if (null arg-irs)
      nil
      (let* ((arg-ir (car arg-irs))
             (arg-code
              (codegen arg-ir rtaddrs
               fnoffs td))
             (slot-offset (+ 64 (* idx 8)))
             (store-code
              (arm64:str :x0 :sp :offset slot-offset))
             (rest-code
              (codegen-tco-args (cdr arg-irs)
               rtaddrs fnoffs td
               (+ idx 1))))
        (append-all
         (list arg-code store-code
               rest-code)))))

(defun codegen-tco-copy-args (nargs idx)
  "Copy from temp slots to param slots (at x20 - idx*8).
   Must be done after all args are evaluated to handle cases like
   (f b a) where we're swapping parameters."
  (if (>= idx nargs)
      nil
      (let* ((slot-offset (+ 64 (* idx 8)))
             (load-code
              (arm64:ldr :x9 :sp :offset slot-offset))
             (param-offset (* idx 8))
             (store-code
              (append (arm64:sub :x10 :env param-offset :imm t)
                      (arm64:str :x9 :x10 :offset 0)))
             (rest-code
              (codegen-tco-copy-args nargs
               (+ idx 1))))
        (append-all
         (list load-code store-code
               rest-code)))))

(defun codegen-let-bindings
       (bindings rtaddrs fnoffs
        td idx)
  "Generate code to evaluate and store let bindings"
  (if (null bindings)
      nil
      (let* ((val-ir (car bindings))
             (val-code
              (codegen val-ir rtaddrs
               fnoffs td))
             (store-code
              (append (arm64:sub :x1 :env (* idx 8) :imm t)
                      (arm64:str :x0 :x1 :offset 0)))
             (rest-code
              (codegen-let-bindings (cdr bindings)
               rtaddrs fnoffs td
               (+ idx 1))))
        (append-all
         (list val-code store-code
               rest-code)))))

(defun codegen-progn-forms
       (forms rtaddrs fnoffs
        td)
  "Generate code for sequence of forms, return value of last"
  (if (null forms)
      nil
      (if (null (cdr forms))
          (codegen (car forms) rtaddrs
           fnoffs td)
          (let* ((first-code
                  (codegen (car forms)
                   rtaddrs fnoffs td))
                 (rest-code
                  (codegen-progn-forms (cdr forms)
                   rtaddrs fnoffs td)))
            (append first-code rest-code)))))

(defun spill-base (td)
  "Calculate spill area base for temp depth td.
   Spill area is 0x100-0x1F0 (240 bytes = 30 slots).
   Each nesting level gets 64 bytes (8 slots) of spill area."
  (+ 256 (* td 64)))

(defun codegen-call-args
       (args rtaddrs fnoffs
        td)
  "Generate code for function call arguments"
  (codegen-args-iter args rtaddrs
   fnoffs td 0))

(defun codegen-args-iter
       (args rtaddrs fnoffs
        td argnum)
  "Generate code for args, storing ALL args to spill slots.
   This ensures arg 0 isn't clobbered when evaluating later args.
   Uses td-based offset so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             (arg-code
              (codegen arg-ir rtaddrs
               fnoffs (+ td 1)))
             (spill-offset
              (+ (spill-base td)
                 (* argnum 8)))
             (save-code
              (arm64:str :x0 :sp :offset spill-offset)))
        (append-all
         (list arg-code save-code
               (codegen-args-iter (cdr args)
                rtaddrs fnoffs td
                (+ argnum 1)))))))

(defun gen-arg-loads (num-args td)
  "Generate code to load spilled args from spill area into registers x0-x7.
   Uses td-based offset to match where args were stored."
  (if (= num-args 0)
      nil
      (let ((base (spill-base td)))
        (labels ((gen-load (i acc)
                   (if (>= i num-args)
                       acc
                       (gen-load (+ i 1)
                        (append acc
                                (arm64:ldr i 31 :offset
                                           (+ base
                                              (* i 8))))))))
          (gen-load 0 nil)))))

(defun codegen-funcall-args
       (args rtaddrs fnoffs
        td argnum)
  "Generate code for funcall arguments.
   Uses td-based spill area so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             (arg-code
              (codegen arg-ir rtaddrs
               fnoffs (+ td 1))))
        (if (< argnum 8)
            (let* ((spill-offset
                    (+ (spill-base td)
                       (* argnum 8)))
                   (save-code
                    (arm64:str :x0 :sp :offset spill-offset)))
              (append-all
               (list arg-code save-code
                     (codegen-funcall-args (cdr args)
                      rtaddrs fnoffs td
                      (+ argnum 1)))))
            nil))))

(defun fn-fixed-prologue ()
  "Generate function prologue with fixed 1KB frame.
   Frame layout after prologue (0x400 bytes = 1024 bytes):
   sp+0x10:  x19, x20 (saved)
   sp+0x20:  x21, x22 (saved)
   sp+0x30:  x23, x24 (saved)
   sp+0x40:  temp slots (24 slots = 192 bytes, to 0x100)
   sp+0x100: spill area (32 slots = 256 bytes, to 0x200)
   sp+0x200: [free space for env slots to expand down]
   sp+0x380: environment base (x20) - allows 48+ env slots before collision
   sp+0x3F0: x29 (fp)
   sp+0x3F8: x30 (lr)
   Note: 1KB frame allows ~8K nested calls with 8MB stack."
  (append (arm64:sub :sp :sp 1024 :imm t) (arm64:str :fp :sp :offset 1008)
          (arm64:str :lr :sp :offset 1016) (arm64:add :fp :sp 0 :imm t)
          (arm64:stp :x19 :env :sp :offset 16) (arm64:stp :x21 :x22 :sp :offset 32)
          (arm64:stp :x23 :closure :sp :offset 48) (arm64:add :env :sp 896 :imm t)))

(defun fn-fixed-epilogue ()
  "Generate function epilogue for fixed 1KB frame"
  (append (arm64:ldp :x23 :closure :sp :offset 48) (arm64:ldp :x21 :x22 :sp :offset 32)
          (arm64:ldp :x19 :env :sp :offset 16) (arm64:ldr :fp :sp :offset 1008)
          (arm64:ldr :lr :sp :offset 1016) (arm64:add :sp :sp 1024 :imm t) (arm64:ret)))

(defun prologue () (fn-fixed-prologue))

(defun epilogue () (fn-fixed-epilogue))

(defun codegen-fn
       (fn rtaddrs fnoffs)
  "Generate code for a function.
   Accepts two formats:
   - Native: (name params body-ir param-base) - 4 elements
   - SBCL:   (name params body-ir free-vars free-offsets) - 5 elements
   For SBCL format, param-base = (length free-vars).
   Uses simple fixed frame layout. Supports TCO for self-recursive functions.

   When *use-register-allocation* is true, tries register-allocated codegen first,
   falling back to accumulator-based codegen if IR not fully supported.

   TCO Architecture:
   - Nanopass: apply-tco-to-function (optimize.lisp) transforms tail calls to loop-ir/continue-ir
   - Codegen: handles loop-ir and continue-ir as regular IR nodes, emits :tco-branch markers
   - Emission: resolve-tco-branches converts markers to actual B instructions"
  (let* ((params (cadr fn))
         (body-ir (caddr fn))
         (fourth (cadddr fn))
         (param-base
          (if (numberp fourth)
              fourth
              (if fourth
                  (length fourth)
                  0)))
         (capture-code
          (if (> param-base 0)
              (gen-capture-loads param-base)
              nil))
         (param-code
          (gen-param-stores params param-base
           0 nil))
         (prologue-size
          (code-size (fn-fixed-prologue)))
         (capture-size
          (if capture-code
              (code-size capture-code)
              0))
         (param-size
          (if param-code
              (code-size param-code)
              0))
         (loop-label-offset
          (+ prologue-size capture-size
             param-size))
         (body-code
          (codegen body-ir rtaddrs
           fnoffs 0))
         (all-code
          (append-all
           (list (fn-fixed-prologue) capture-code
                 param-code body-code
                 (fn-fixed-epilogue))))
         (resolved-code
          (resolve-tco-branches all-code
           loop-label-offset)))
    resolved-code))

(defun resolve-tco-branches
       (code loop-label-offset)
  "Resolve :tco-branch markers into actual B (unconditional branch) instructions.
   Each marker is (:tco-branch loop-label-offset) and needs to become a backward branch.
   Preserves :call-fn, :tail-call-fn, and :extern-call markers for later resolution.
   Returns flattened code with TCO markers replaced by B instructions."
  (labels ((emit-b-back (offset)
             (let* ((imm26 (logand (ash offset -2) 67108863))
                    (b-instr (logior 335544320 imm26)))
               (list (logand (ash b-instr -24) 255)
                     (logand (ash b-instr -16) 255)
                     (logand (ash b-instr -8) 255)
                     (logand b-instr 255))))
           (marker-p (item)
             (and (consp item)
                  (or (sym-eq (car item) :call-fn)
                      (sym-eq (car item) :tail-call-fn)
                      (sym-eq (car item) :extern-call)
                      (sym-eq (car item) :loop-start)
                      (sym-eq (car item) :loop-continue)
                      (sym-eq (car item) :block-start)
                      (sym-eq (car item) :block-end)
                      (sym-eq (car item) :return-from))))
           (process
               (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                    ((and (consp item)
                          (sym-eq (car item) :tco-branch))
                     (let* ((offset
                             (- loop-label-offset pos))
                            (b-bytes
                             (emit-b-back offset)))
                       (process (cdr items)
                        (+ pos 4)
                        (append (reverse b-bytes) acc))))
                    ((marker-p item)
                     (process (cdr items)
                      (+ pos 4)
                      (cons item acc)))
                    ((consp item)
                     (let* ((flattened
                             (process item 0 nil))
                            (size (length flattened)))
                       (process (cdr items)
                        (+ pos size)
                        (append (reverse flattened) acc))))
                    (t
                     (process (cdr items)
                      (+ pos 1)
                      (cons item acc))))))))
    (process code 0 nil)))

(defun gen-capture-loads (num-captures)
  "Generate code to load captured values from x24 cons list into env slots.
   x24 = (v0 . (v1 . (v2 . nil))) - load into offsets 0, 1, 2, etc."
  (labels ((gen-loads (idx acc)
             (if (>= idx num-captures)
                 acc
                 (let* ((offset (* idx 8))
                        (load-car
                         (append (arm64:sub :x9 :closure 1 :imm t) (arm64:ldr :x9 :x9 :offset 0)))
                        (store-env
                         (append (arm64:sub :x10 :env offset :imm t)
                                 (arm64:str :x9 :x10 :offset 0)))
                        (advance
                         (append (arm64:sub :x9 :closure 1 :imm t)
                                 (arm64:ldr :closure :x9 :offset 8))))
                   (gen-loads (+ idx 1)
                    (append-all
                     (list acc load-car
                           store-env advance)))))))
    (gen-loads 0 nil)))

(defun gen-param-stores
       (params base idx
        acc)
  "Generate stores from registers x0-x7 to environment slots"
  (if (null params)
      acc
      (if (< idx 8)
          (let* ((offset (* (+ base idx) 8))
                 (store
                  (append (arm64:sub :x9 :env offset :imm t)
                          (arm64:str idx 9 :offset 0))))
            (gen-param-stores (cdr params)
             base (+ idx 1)
             (append acc store)))
          acc)))

(defun code-size (code)
  "Calculate size of code in bytes, accounting for markers.
   Markers: :call-fn, :extern-call, :tco-branch, :loop-continue, :return-from = 4 bytes each.
   :loop-start, :block-start, :block-end = 0 bytes (position markers only)."
  (labels ((tally (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                    ((and (consp item)
                          (sym-eq (car item) :call-fn))
                     (tally (cdr items)
                      (+ acc 4)))
                    ((and (consp item)
                          (sym-eq (car item) :extern-call))
                     (tally (cdr items)
                      (+ acc 4)))
                    ((and (consp item)
                          (sym-eq (car item) :tco-branch))
                     (tally (cdr items)
                      (+ acc 4)))
                    ((and (consp item)
                          (sym-eq (car item) :loop-continue))
                     (tally (cdr items)
                      (+ acc 4)))
                    ((and (consp item)
                          (sym-eq (car item) :return-from))
                     (tally (cdr items)
                      (+ acc 4)))
                    ((and (consp item)
                          (sym-eq (car item) :loop-start))
                     (tally (cdr items) acc))
                    ((and (consp item)
                          (sym-eq (car item) :block-start))
                     (tally (cdr items) acc))
                    ((and (consp item)
                          (sym-eq (car item) :block-end))
                     (tally (cdr items) acc))
                    ((consp item)
                     (tally (cdr items)
                      (+ acc
                         (tally item 0))))
                    (t
                     (tally (cdr items)
                      (+ acc 1))))))))
    (tally code 0)))

(defun build-fnoffs-pass
       (fns offset fnoffs
        acc)
  "Build function offset table: ((name . byte-offset) ...)
   Uses fnoffs for accurate size calculation (may be nil for first pass)."
  (if (null fns)
      (reverse acc)
      (let* ((fn (car fns))
             (name (car fn))
             (code
              (codegen-fn fn nil fnoffs))
             (size (code-size code))
             (entry (cons name offset)))
        (build-fnoffs-pass (cdr fns)
         (+ offset size) fnoffs
         (cons entry acc)))))

(defun fnoffs-equal (a b)
  "Compare two fnoffs tables for equality"
  (cond ((and (null a) (null b)) t)
        ((or (null a) (null b)) nil)
        (t
         (let ((ea (car a))
               (eb (car b)))
           (if (and (equal (car ea) (car eb))
                    (= (cdr ea) (cdr eb)))
               (fnoffs-equal (cdr a) (cdr b))
               nil)))))

(defun build-fnoffs (fns offset)
  "Build function offset table with iteration until stable.
   Code size depends on function offsets (load-addr size varies),
   so we iterate until the table stabilizes."
  (labels ((iterate (prev-fnoffs iterations)
             (if (> iterations 10)
                 prev-fnoffs
                 (let ((new-fnoffs
                        (build-fnoffs-pass fns
                         offset prev-fnoffs nil)))
                   (if (fnoffs-equal prev-fnoffs
                        new-fnoffs)
                       new-fnoffs
                       (iterate new-fnoffs
                        (+ iterations 1)))))))
    (let ((first-pass
           (build-fnoffs-pass fns offset nil
            nil)))
      (iterate first-pass 1))))

(defun codegen-all-fns
       (fns rtaddrs fnoffs
        acc)
  "Generate code for all functions with fnoffs"
  (if (null fns)
      acc
      (let* ((fn (car fns))
             (code
              (codegen-fn fn rtaddrs
               fnoffs)))
        (codegen-all-fns (cdr fns) rtaddrs
         fnoffs (append acc code)))))

(defun codegen-main (mir rtaddrs)
  "Generate main code with prologue/epilogue"
  (append-all
   (list (prologue)
         (codegen mir rtaddrs nil 0)
         (epilogue))))

(defun resolve-calls-simple (code)
  "Simple resolve - just flatten the code list.
   For now, this just removes the :call-fn and :extern-call markers.
   Full version needs function offset table."
  (labels ((flatten (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item)
                            (or (sym-eq (car item) :call-fn)
                                (sym-eq (car item) :extern-call)))
                       (if (sym-eq (car item) :extern-call)
                           (flatten (cdr items)
                            (cons item acc))
                           (flatten (cdr items)
                            (append (reverse (arm64:bl 0)) acc)))
                       (if (consp item)
                           (flatten (cdr items)
                            (append (reverse item) acc))
                           (flatten (cdr items)
                            (cons item acc))))))))
    (flatten code nil)))

(defun deliver
       (source output-path
        &optional (heap-size 67108864))
  "Compile source string to native executable.
   Heap is allocated via mmap at runtime.
   HEAP-SIZE: runtime heap size in bytes (default 64MB).
   Supports: defun, lambda, funcall, GC runtime."
  (register-compiler-symbols)
  (reset-symbol-table)
  (reset-lambda-counter)
  (let* ((forms (read-all source))
         (result (compile-forms forms))
         (_)
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size 216)
         (main-lift-result
          (lift-lambdas main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         (defun-lift-result
          (lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         (all-lambdas
          (append main-lambdas defun-lambdas)))
    (let* ((lambda-as-defuns
            (lambdas-to-defuns all-lambdas nil))
           (all-fns-raw
            (append defuns lambda-as-defuns))
           (all-fns
            (apply-tco-to-all-functions all-fns-raw))
           (_)
           (main-code-temp
            (append-all
             (list (fn-fixed-prologue)
                   (codegen main-ir nil nil 0)
                   (fn-fixed-epilogue))))
           (main-size
            (code-size main-code-temp))
           (fnoffs
            (build-fnoffs all-fns main-size))
           (main-code
            (append-all
             (list (fn-fixed-prologue)
                   (codegen main-ir nil
                    fnoffs 0)
                   (fn-fixed-epilogue))))
           (fn-code
            (codegen-all-fns all-fns nil
             fnoffs nil))
           (gc-code (gc-runtime-code))
           (all-code
            (append main-code fn-code
                    gc-code))
           (bytes-with-markers
            (flatten-code-keep-markers-and-calls all-code))
           (extern-calls
            (collect-extern-calls bytes-with-markers))
           (imports
            (get-unique-imports extern-calls))
           (imports
            (if (null imports)
                '("_exit")
                imports))
           (code-offset (mmap-heap-code-offset))
           (exact-flat-size
            (count-actual-bytes bytes-with-markers))
           (exact-code-size
            (+ exact-flat-size wrapper-size))
           (stubs-offset-unaligned
            (+ code-offset exact-code-size))
           (stubs-offset
            (* (ceiling stubs-offset-unaligned 4) 4))
           (stub-size 12)
           (stub-alist
            (build-stub-alist imports
             stubs-offset stub-size))
           (fn-addr-base
            (+ code-offset wrapper-size))
           (fn-alist-base
            (build-fn-addr-alist fnoffs
             fn-addr-base nil))
           (gc-fn-alist
            (extract-fn-labels bytes-with-markers
             fn-addr-base))
           (fn-alist
            (append fn-alist-base gc-fn-alist))
           (flatten-result
            (flatten-all-calls bytes-with-markers
             fn-alist stub-alist
             fn-addr-base))
           (flat-code (car flatten-result))
           (wrapped-code
            (wrap-bytecode-with-mmap-heap flat-code
             heap-size)))
      (let ((all-fnoffs
             (append fnoffs
                     (mapcar
                      (lambda (entry)
                        (cons (car entry)
                              (- (cdr entry) fn-addr-base)))
                      gc-fn-alist))))
        (write-macho-executable-mmap-heap output-path
         wrapped-code imports all-fnoffs)))))

(defun deliver-file
       (source-path output-path
        &optional (heap-size 67108864))
  "Compile Lisp file to native executable.
   Usage: (habu:deliver-file \"program.lisp\" \"program\")"
  (deliver (native-read-file source-path)
   output-path heap-size))

(defun count-actual-bytes (items)
  "Count actual bytes in a flattened list, excluding markers.
   Markers are conses like (:extern-call ...), (:fn-label ...), etc.
   Note: placeholder zeros for call markers are already in the list."
  (labels ((count-bytes (lst acc)
             (if (null lst)
                 acc
                 (let ((item (car lst)))
                   (if (consp item)
                       (count-bytes (cdr lst)
                        acc)
                       (count-bytes (cdr lst)
                        (+ acc 1)))))))
    (count-bytes items 0)))

(defun build-fn-addr-alist
       (fnoffs base acc)
  "Convert fnoffs to absolute addresses"
  (if (null fnoffs)
      (reverse acc)
      (let* ((entry (car fnoffs))
             (name (car entry))
             (offset (cdr entry))
             (sb-alien:addr (+ base offset)))
        (build-fn-addr-alist (cdr fnoffs)
         base
         (cons (cons name sb-alien:addr) acc)))))

(defun flatten-code-keep-markers-and-calls (code)
  "Flatten code lists but keep both :extern-call, :call-fn, :tco-branch, :loop-start, :loop-continue and :fn-label markers with positions.
   ITERATIVE version using explicit work stack to avoid deep recursion."
  (let ((work-stack (list (list code 0 nil nil))))
    (loop (when (null work-stack) (return nil))
          (let* ((state (car work-stack))
                 (items (first state))
                 (pos (second state))
                 (acc (third state))
                 (parent (fourth state)))
            (setf work-stack (cdr work-stack))
            (cond
             ((null items)
              (if parent
                  (let* ((flattened (reverse acc))
                         (size (length flattened))
                         (parent-items (first parent))
                         (parent-pos (second parent))
                         (parent-acc (third parent))
                         (parent-parent (fourth parent)))
                    (push
                     (list parent-items
                           (+ parent-pos size)
                           (append (reverse flattened)
                                   parent-acc)
                           parent-parent)
                     work-stack))
                  (return (reverse acc))))
             (t
              (let ((item (car items)))
                (cond
                 ((and (consp item)
                       (sym-eq (car item) :extern-call))
                  (let ((marker
                         (list :extern-call (cadr item) pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((and (consp item) (sym-eq (car item) :call-fn))
                  (let ((marker
                         (list :call-fn (cadr item) pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((and (consp item)
                       (sym-eq (car item) :tco-branch))
                  (let ((marker
                         (list :tco-branch (cadr item) pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((and (consp item)
                       (sym-eq (car item) :loop-start))
                  (let ((marker (list :loop-start pos)))
                    (push
                     (list (cdr items) pos
                           (cons marker acc)
                           parent)
                     work-stack)))
                 ((and (consp item)
                       (sym-eq (car item) :loop-continue))
                  (let ((marker (list :loop-continue pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((and (consp item)
                       (sym-eq (car item) :block-start))
                  (let ((marker
                         (list :block-start (cadr item) pos)))
                    (push
                     (list (cdr items) pos
                           (cons marker acc)
                           parent)
                     work-stack)))
                 ((and (consp item) (sym-eq (car item) :block-end))
                  (let ((marker
                         (list :block-end (cadr item) pos)))
                    (push
                     (list (cdr items) pos
                           (cons marker acc)
                           parent)
                     work-stack)))
                 ((and (consp item)
                       (sym-eq (car item) :return-from))
                  (let ((marker
                         (list :return-from (cadr item) pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((and (consp item) (sym-eq (car item) :fn-label))
                  (let ((marker
                         (list :fn-label (cadr item) pos)))
                    (push
                     (list (cdr items) pos
                           (cons marker acc)
                           parent)
                     work-stack)))
                 ((and (consp item) (sym-eq (car item) :label))
                  (push
                   (list (cdr items) pos acc
                         parent)
                   work-stack))
                 ((consp item)
                  (push
                   (list item 0 nil
                         (list (cdr items) pos
                               acc parent))
                   work-stack))
                 (t
                  (push
                   (list (cdr items) (+ pos 1)
                         (cons item acc)
                         parent)
                   work-stack))))))))))

(defun flatten-all-calls
       (code fn-alist stub-alist
        code-base-addr)
  "Replace :call-fn, :extern-call, :loop-start/:loop-continue, :block-start/:block-end/:return-from markers with actual instructions.
   Returns (cons flattened-code positions)."
  (labels ((lookup-fn (name)
             (alist-lookup name fn-alist))
           (lookup-stub (name)
             (alist-lookup name stub-alist))
           (emit-bl
               (bl-addr target-addr acc)
             (let* ((rel-offset
                     (- target-addr bl-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s 67108863))
                    (bl-instr (logior 2483027968 off-m)))
               (cons (logand (ash bl-instr -24) 255)
                     (cons (logand (ash bl-instr -16) 255)
                           (cons (logand (ash bl-instr -8) 255)
                                 (cons (logand bl-instr 255)
                                       acc))))))
           (emit-b
               (b-addr target-addr acc)
             (let* ((rel-offset
                     (- target-addr b-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s 67108863))
                    (b-instr (logior 335544320 off-m)))
               (cons (logand (ash b-instr -24) 255)
                     (cons (logand (ash b-instr -16) 255)
                           (cons (logand (ash b-instr -8) 255)
                                 (cons (logand b-instr 255)
                                       acc))))))
           (collect-block-ends (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (if (and (consp item)
                            (sym-eq (car item) :block-end))
                       (collect-block-ends (cdr items)
                        (cons (cons (cadr item) (caddr item))
                              acc))
                       (collect-block-ends (cdr items)
                        acc)))))
           (lookup-block-end
               (block-id block-ends)
             (let ((entry
                    (assoc block-id block-ends :test #'equal)))
               (if entry
                   (cdr entry)
                   nil)))
           (process
               (items skip result
                positions loop-stack
                block-ends)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                    ((> skip 0)
                     (process (cdr items)
                      (- skip 1) result
                      positions loop-stack
                      block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :loop-start))
                     (let ((pos (cadr item)))
                       (process (cdr items) 0
                        result positions
                        (cons pos loop-stack)
                        block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :loop-continue))
                     (let* ((pos (cadr item))
                            (b-addr
                             (+ code-base-addr pos))
                            (target-pos (car loop-stack))
                            (target-addr
                             (+ code-base-addr target-pos))
                            (new-result
                             (emit-b b-addr
                              target-addr result)))
                       (process (cdr items) 4
                        new-result positions
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :block-start))
                     (process (cdr items) 0
                      result positions
                      loop-stack block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :block-end))
                     (process (cdr items) 0
                      result positions
                      loop-stack block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :return-from))
                     (let* ((block-id (cadr item))
                            (pos (caddr item))
                            (b-addr
                             (+ code-base-addr pos))
                            (target-pos
                             (lookup-block-end block-id
                              block-ends))
                            (target-addr
                             (+ code-base-addr target-pos))
                            (new-result
                             (emit-b b-addr
                              target-addr result)))
                       (process (cdr items) 4
                        new-result positions
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :extern-call))
                     (let* ((name (cadr item))
                            (pos (caddr item))
                            (bl-addr
                             (+ code-base-addr pos))
                            (stub-addr
                             (lookup-stub name))
                            (new-result
                             (if stub-addr
                                 (emit-bl bl-addr
                                  stub-addr result)
                                 (cons 148 (cons 0 (cons 0 (cons 0 result)))))))
                       (process (cdr items) 4
                        new-result
                        (cons (cons name pos)
                              positions)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :call-fn))
                     (let* ((name (cadr item))
                            (pos (caddr item))
                            (bl-addr
                             (+ code-base-addr pos))
                            (fn-addr
                             (lookup-fn name))
                            (new-result
                             (if fn-addr
                                 (emit-bl bl-addr
                                  fn-addr result)
                                 (cons 213 (cons 3 (cons 32 (cons 31 result)))))))
                       (process (cdr items) 4
                        new-result
                        (cons (cons name pos)
                              positions)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :fn-label))
                     (process (cdr items) 0
                      result positions
                      loop-stack block-ends))
                    ((and (consp item) (sym-eq (car item) :label))
                     (process (cdr items) 0
                      result positions
                      loop-stack block-ends))
                    (t
                     (process (cdr items) 0
                      (cons item result)
                      positions loop-stack
                      block-ends)))))))
    (let ((block-ends
           (collect-block-ends code nil)))
      (process code 0 nil nil nil
       block-ends))))

(defun extract-fn-labels (code base-addr)
  "Extract :fn-label markers from flattened code and build fn-alist.
   BASE-ADDR is the absolute address where code starts.
   Returns alist of (name . addr)."
  (labels ((collect (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item)
                            (sym-eq (car item) :fn-label))
                       (let* ((name (cadr item))
                              (pos (caddr item))
                              (sb-alien:addr (+ base-addr pos)))
                         (collect (cdr items)
                          (cons (cons name sb-alien:addr)
                                acc)))
                       (collect (cdr items)
                        acc))))))
    (collect code nil)))

(defun alist-lookup (key alist)
  "Look up key in alist, return value or nil"
  (if (null alist)
      nil
      (if (if (symbolp key)
              (sym-eq key (caar alist))
              (equal key (caar alist)))
          (cdar alist)
          (alist-lookup key (cdr alist)))))

(defun flatten-code-keep-markers (code)
  "Flatten nested code lists but keep :extern-call markers intact.
   Tracks position and transforms (:extern-call name) to (:extern-call name pos).
   Each marker followed by 4 zeros = 4 bytes total for BL instruction.
   ITERATIVE version using explicit work stack to avoid deep recursion."
  (let ((work-stack (list (list code 0 nil nil))))
    (loop (when (null work-stack) (return nil))
          (let* ((state (car work-stack))
                 (items (first state))
                 (pos (second state))
                 (acc (third state))
                 (parent (fourth state)))
            (setf work-stack (cdr work-stack))
            (cond
             ((null items)
              (if parent
                  (let* ((flattened (reverse acc))
                         (size (length flattened))
                         (parent-items (first parent))
                         (parent-pos (second parent))
                         (parent-acc (third parent))
                         (parent-parent (fourth parent)))
                    (push
                     (list parent-items
                           (+ parent-pos size)
                           (append (reverse flattened)
                                   parent-acc)
                           parent-parent)
                     work-stack))
                  (return (reverse acc))))
             (t
              (let ((item (car items)))
                (cond
                 ((and (consp item)
                       (sym-eq (car item) :extern-call))
                  (let ((marker
                         (list :extern-call (cadr item) pos)))
                    (push
                     (list (cdr items) (+ pos 4)
                           (cons 0
                                 (cons 0
                                       (cons 0
                                             (cons 0
                                                   (cons marker
                                                         acc)))))
                           parent)
                     work-stack)))
                 ((consp item)
                  (push
                   (list item 0 nil
                         (list (cdr items) pos
                               acc parent))
                   work-stack))
                 (t
                  (push
                   (list (cdr items) (+ pos 1)
                         (cons item acc)
                         parent)
                   work-stack))))))))))

(defun flatten-extern-calls
       (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using stub-alist.
   stub-alist is ((name . stub-addr) ...).
   Returns (cons flattened-code extern-call-positions).
   Native Habu version - SBCL uses hash-table version in compiler-sbcl.lisp."
  (labels ((lookup (name alist)
             (if (null alist)
                 nil
                 (if (string= name (caar alist))
                     (cdar alist)
                     (lookup name
                      (cdr alist)))))
           (emit-bl
               (bl-addr stub-addr acc)
             (let* ((rel-offset
                     (- stub-addr bl-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s 67108863))
                    (bl-instr (logior 2483027968 off-m)))
               (cons (logand (ash bl-instr -24) 255)
                     (cons (logand (ash bl-instr -16) 255)
                           (cons (logand (ash bl-instr -8) 255)
                                 (cons (logand bl-instr 255)
                                       acc))))))
           (process
               (items skip result
                positions)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                    ((> skip 0)
                     (process (cdr items)
                      (- skip 1) result
                      positions))
                    ((and (consp item)
                          (sym-eq (car item) :extern-call))
                     (let* ((name (cadr item))
                            (pos (caddr item))
                            (bl-addr
                             (+ code-base-addr pos))
                            (stub-addr
                             (lookup name
                              stub-alist))
                            (new-result
                             (if stub-addr
                                 (emit-bl bl-addr
                                  stub-addr result)
                                 (cons 148 (cons 0 (cons 0 (cons 0 result)))))))
                       (process (cdr items) 4
                        new-result
                        (cons (cons name pos)
                              positions))))
                    (t
                     (process (cdr items) 0
                      (cons item result)
                      positions)))))))
    (process code 0 nil nil)))

(defun resolve-calls (code fnoffs)
  "Resolve call, loop, and block markers to branch instructions.
   Handles: (:call-fn name), (:tail-call-fn name), (:loop-start), (:loop-continue),
            (:block-start id), (:block-end id), (:return-from id)
   Note: (:extern-call name) markers are kept as-is for later resolution.
   Native version using arm64 intrinsics."
  (labels ((calc-size (item)
             (cond
              ((and (consp item) (sym-eq (car item) :call-fn)) 4)
              ((and (consp item) (sym-eq (car item) :tail-call-fn))
               4)
              ((and (consp item) (sym-eq (car item) :extern-call))
               4)
              ((and (consp item) (sym-eq (car item) :loop-start))
               0)
              ((and (consp item)
                    (sym-eq (car item) :loop-continue))
               4)
              ((and (consp item) (sym-eq (car item) :block-start))
               0)
              ((and (consp item) (sym-eq (car item) :block-end)) 0)
              ((and (consp item) (sym-eq (car item) :return-from))
               4)
              ((and (consp item) (sym-eq (car item) :tco-branch))
               4)
              (t 1)))
           (lookup-fn (name fnoffs)
             (if (null fnoffs)
                 nil
                 (if (sym-eq name (caar fnoffs))
                     (cdar fnoffs)
                     (lookup-fn name
                      (cdr fnoffs)))))
           (collect-block-ends
               (items pos acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                    ((and (consp item)
                          (sym-eq (car item) :block-end))
                     (collect-block-ends (cdr items)
                      pos
                      (cons (cons (cadr item) pos)
                            acc)))
                    (t
                     (collect-block-ends (cdr items)
                      (+ pos
                         (calc-size item))
                      acc))))))
           (lookup-block-end
               (block-id block-ends)
             (if (null block-ends)
                 nil
                 (if (equal block-id (caar block-ends))
                     (cdar block-ends)
                     (lookup-block-end block-id
                      (cdr block-ends)))))
           (resolve-at
               (items pos acc
                loop-stack block-ends)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                    ((and (consp item)
                          (sym-eq (car item) :loop-start))
                     (resolve-at (cdr items)
                      pos acc
                      (cons pos loop-stack)
                      block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :loop-continue))
                     (let* ((loop-start (car loop-stack))
                            (rel-offset
                             (- loop-start pos))
                            (b-bytes
                             (arm64:b (ash rel-offset -2))))
                       (resolve-at (cdr items)
                        (+ pos 4)
                        (append (reverse b-bytes) acc)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :block-start))
                     (resolve-at (cdr items)
                      pos acc loop-stack
                      block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :block-end))
                     (resolve-at (cdr items)
                      pos acc loop-stack
                      block-ends))
                    ((and (consp item)
                          (sym-eq (car item) :return-from))
                     (let* ((block-id (cadr item))
                            (block-end-pos
                             (lookup-block-end block-id
                              block-ends))
                            (rel-offset
                             (- block-end-pos pos))
                            (b-bytes
                             (arm64:b (ash rel-offset -2))))
                       (resolve-at (cdr items)
                        (+ pos 4)
                        (append (reverse b-bytes) acc)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :tco-branch))
                     (let* ((target (cadr item))
                            (rel-offset
                             (- target pos))
                            (b-bytes
                             (arm64:b (ash rel-offset -2))))
                       (resolve-at (cdr items)
                        (+ pos 4)
                        (append (reverse b-bytes) acc)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :call-fn))
                     (let* ((fn-name (cadr item))
                            (fn-pos
                             (lookup-fn fn-name
                              fnoffs))
                            (fn-pos
                             (if fn-pos
                                 fn-pos
                                 0))
                            (rel-offset
                             (- fn-pos pos))
                            (bl-bytes
                             (arm64:bl (ash rel-offset -2))))
                       (resolve-at (cdr items)
                        (+ pos 4)
                        (append (reverse bl-bytes) acc)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :tail-call-fn))
                     (let* ((fn-name (cadr item))
                            (fn-pos
                             (lookup-fn fn-name
                              fnoffs))
                            (fn-pos
                             (if fn-pos
                                 fn-pos
                                 0))
                            (rel-offset
                             (- fn-pos pos))
                            (b-bytes
                             (arm64:b (ash rel-offset -2))))
                       (resolve-at (cdr items)
                        (+ pos 4)
                        (append (reverse b-bytes) acc)
                        loop-stack block-ends)))
                    ((and (consp item)
                          (sym-eq (car item) :extern-call))
                     (resolve-at (cdr items)
                      (+ pos 4)
                      (list* 0 0 0
                             (list :extern-call (cadr item)
                                   pos)
                             acc)
                      loop-stack block-ends))
                    (t
                     (resolve-at (cdr items)
                      (+ pos 1)
                      (cons item acc)
                      loop-stack block-ends)))))))
    (let ((block-ends
           (collect-block-ends code 0 nil)))
      (resolve-at code 0 nil nil
       block-ends))))

(in-package :habu)

(defun buf-zeros (count)
  "Create a list of COUNT zeros using pure recursion"
  (labels ((make-zeros (n acc)
             (if (<= n 0)
                 acc
                 (make-zeros (- n 1)
                  (cons 0 acc)))))
    (make-zeros count nil)))

(defun list-length (lst)
  "Pure version of length for lists"
  (labels ((len (l n)
             (if (null l)
                 n
                 (len (cdr l) (+ n 1)))))
    (len lst 0)))

(defun native-write-file (path content)
  "Write string CONTENT to file PATH (native Habu version)"
  (let* ((path-len (string-length path))
         (fd (sys-open path 1537 493)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written
                (sys-write fd content
                 len)))
          (sys-close fd)
          written)
        -1)))

(defun native-write-executable (path content)
  "Write executable file - uses mode 0755 for +x permission (native version)"
  (let ((fd (sys-open path 1537 493)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written
                (sys-write fd content
                 len)))
          (sys-close fd)
          written)
        -1)))

