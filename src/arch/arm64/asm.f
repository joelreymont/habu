\ asm.fs — ARM64 instruction encoders in the STANDALONE's Forth (operands -> u32).
\ Hex constants preserve the bit layout used by the native engine builder.
\ The standalone encodes ARM64 directly instead of relying on baked host output.
$FFFFFFFF constant ARM64-W32

: MSK ( n -- n ) ARM64-W32 and ;

variable ARM-BASE
variable ARM-RD
variable ARM-RN
variable ARM-RM
variable ARM-IMM
variable ARM-SH
variable ARM-X
variable ARM-Y
variable ARM-Z

: ARM-R3! ( n n n -- )
   ARM-RM ! ARM-RN ! ARM-RD ! ;

: ARM-R2! ( n n -- )
   ARM-RN ! ARM-RD ! ;

: ARM-I3! ( n n n -- )
   ARM-IMM ! ARM-RN ! ARM-RD ! ;

\ move-wide: rd imm16 hw -> u32
: MOVZHW ( n n n -- n )  21 lshift swap 5 lshift or or $D2800000 or MSK ;

: MOVKHW ( n n n -- n )  21 lshift swap 5 lshift or or $F2800000 or MSK ;

: MOVNHW ( n n n -- n )  21 lshift swap 5 lshift or or $92800000 or MSK ;

\ shifted-register 3-operand: rd rn rm
: RRR ( n n n n -- n )
   ARM-BASE ! ARM-R3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

: ENC-ADD ( n n n -- n ) $8B000000 RRR ;

: ENC-SUB ( n n n -- n ) $CB000000 RRR ;

: ENC-AND ( n n n -- n ) $8A000000 RRR ;

: ENC-ORR ( n n n -- n ) $AA000000 RRR ;

: ENC-EOR ( n n n -- n ) $CA000000 RRR ;

: ENC-MUL ( n n n -- n ) $9B007C00 RRR ;

: RRI ( n n n n -- n )
   ARM-BASE ! ARM-I3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-IMM @ 10 lshift or MSK ;

: RR ( n n n -- n )
   ARM-BASE ! ARM-R2!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or MSK ;

\ add/sub immediate: rd rn imm12
: ENC-ADDI ( n n n -- n ) $91000000 RRI ;

: ENC-SUBI ( n n n -- n ) $D1000000 RRI ;

\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI ( n n n -- n )
   ARM-SH ! ARM-RN ! ARM-RD !
   $D3400000 ARM-RD @ or  ARM-RN @ 5 lshift or
   $40 ARM-SH @ - $3F and 16 lshift or  $3F ARM-SH @ - 10 lshift or MSK ;

\ logical-shift-right immediate: rd rn sh
: ENC-LSRI ( n n n -- n )
   ARM-SH ! ARM-RN ! ARM-RD !
   $D340FC00 ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-SH @ 16 lshift or MSK ;

\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP ( n n -- n )
   ARM-RM ! ARM-RN !
   $EB00001F ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

\ compare immediate rn imm12
: ENC-CMPI ( n n -- n )
   ARM-IMM ! ARM-RN !
   $F100001F ARM-RN @ 5 lshift or  ARM-IMM @ 10 lshift or MSK ;

\ svc #imm  ; ret
: ENC-SVC ( n -- n ) $D4000001 swap 5 lshift or MSK ;

: ENC-RET ( -- n )  $D65F03C0 ;

\ load/store, unsigned offset. habu scales: x-regs by 8 (?SC8), w by 4, byte by 1.
: ENC-LDR ( n n n -- n ) 8 / $F9400000 RRI ;

: ENC-STR ( n n n -- n ) 8 / $F9000000 RRI ;

: ENC-LDRB ( n n n -- n ) $39400000 RRI ;

: ENC-STRB ( n n n -- n ) $39000000 RRI ;

: ENC-LDRW ( n n n -- n ) 4 / $B9400000 RRI ;

: ENC-STRW ( n n n -- n ) 4 / $B9000000 RRI ;

\ branches: delta is in WORDS (instruction-relative), sign-handled by the caller's mask.
: ENC-B ( n -- n ) $3FFFFFF and $14000000 or MSK ;

: ENC-BL ( n -- n ) $3FFFFFF and $94000000 or MSK ;

: ENC-BCOND ( n n -- n )
   ARM-IMM ! $7FFFF and 5 lshift $54000000 or ARM-IMM @ or MSK ;

: ENC-CBZ ( n n -- n )
   swap ARM-RD ! $7FFFF and 5 lshift $B4000000 or ARM-RD @ or MSK ;

: ENC-CBNZ ( n n -- n )
   swap ARM-RD ! $7FFFF and 5 lshift $B5000000 or ARM-RD @ or MSK ;

\ FP (double): operands in the D-register file
: ENC-FMOVXD ( n n -- n ) $9E670000 RR ;   \ X->D bits

: ENC-FMOVDX ( n n -- n ) $9E660000 RR ;   \ D->X bits

: ENC-FADD ( n n n -- n ) $1E602800 RRR ;

: ENC-FSUB ( n n n -- n ) $1E603800 RRR ;

: ENC-FMUL ( n n n -- n ) $1E600800 RRR ;

\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET ( n n -- n )
   ARM-IMM ! ARM-RD !
   $9A9F07E0 ARM-RD @ or  ARM-IMM @ 1 xor 12 lshift or MSK ;

\ data-processing 2-source: shift-by-register, divide
: ENC-LSLV ( n n n -- n ) $9AC02000 RRR ;

: ENC-LSRV ( n n n -- n ) $9AC02400 RRR ;

: ENC-ASRV ( n n n -- n ) $9AC02800 RRR ;

: ENC-SDIV ( n n n -- n ) $9AC00C00 RRR ;

: ENC-UDIV ( n n n -- n ) $9AC00800 RRR ;

\ arithmetic shift right immediate (SBFM)
: ENC-ASRI ( n n n -- n )
   ARM-SH ! ARM-RN ! ARM-RD !
   $9340FC00 ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-SH @ 16 lshift or MSK ;

\ logical immediates (nis = N<<12 | immr<<6 | imms)
: ENC-ANDI ( n n n -- n ) $92000000 RRI ;

: ENC-ORRI ( n n n -- n ) $B2000000 RRI ;

: ENC-EORI ( n n n -- n ) $D2000000 RRI ;

\ encodeBitMasks: plain mask -> nis. A valid mask is a power-of-2-sized
\ repeating element (2..64 bits) that is a rotated contiguous run of ones;
\ 0 and all-ones are not encodable (die 72).
variable PCX

: POPC64 ( n -- n )
   PCX !  0
   BEGIN PCX @ 0 <> WHILE  PCX @ 1 and +  PCX @ 1 rshift PCX !  REPEAT ;

: EMASK ( n -- n )   \ low-e-bits mask; lshift wraps at 64
   dup $40 = IF drop -1 ELSE 1 swap lshift 1 - THEN ;

variable REMSK
variable RORE-X
variable RORE-R
variable RORE-E

: RORE ( n n n -- n )
   RORE-E ! RORE-R ! RORE-X !
   RORE-E @ EMASK REMSK !
   RORE-X @ REMSK @ and dup  RORE-R @ rshift
   swap RORE-E @ RORE-R @ - lshift  or  REMSK @ and ;

: HALVES= ( n n -- bool )
   ARM-Y ! ARM-X !
   1 ARM-Y @ 2 / lshift 1 -  dup  ARM-X @ and
   swap  ARM-X @ ARM-Y @ 2 / rshift and  = ;

variable LELE

: LELEM ( n -- n n )
   ARM-X !
   $40 LELE !
   BEGIN  LELE @ 2 >  ARM-X @ LELE @ HALVES= and  WHILE  LELE @ 2 / LELE !  REPEAT
   ARM-X @ LELE @ EMASK and  LELE @ ;

variable LRI
variable LROT-ELEM
variable LROT-ONES
variable LROT-E

: LROT ( n n n -- n )
   LROT-E ! LROT-ONES ! LROT-ELEM !
   -1  0 LRI !
   BEGIN LRI @ LROT-E @ < WHILE
      1 LROT-ONES @ lshift 1 -  LRI @ LROT-E @ RORE  LROT-ELEM @ = IF
         drop LRI @  LROT-E @ LRI !
      ELSE
         LRI @ 1 + LRI !
      THEN
   REPEAT ;

: LIMM-PACK ( n n n -- n )
   ARM-Z ! ARM-Y ! ARM-X !
   ARM-Z @ $40 = IF $1000 ELSE 0 THEN
   ARM-X @ 6 lshift or
   ARM-Z @ 2 * 1 - $3F and $3F xor  ARM-Y @ 1 - or  or ;

variable LIE   variable LIONES

: LIMM-BAD ( -- )  s" asm: bad logical immediate" 72 die ;

: >LIMM ( n -- n )
   ARM-X !
   ARM-X @ 0 =  ARM-X @ -1 =  or IF LIMM-BAD THEN
   ARM-X @ LELEM LIE !                                  \ ( elem ), e in LIE
   dup POPC64 LIONES !
   LIONES @ LIE @ = IF LIMM-BAD THEN
   LIONES @ LIE @ LROT                               \ ( r | -1 )
   dup 0 < IF LIMM-BAD THEN
   LIONES @ LIE @ LIMM-PACK ;

\ indirect branches, trap, nop
: ENC-BLR ( n -- n ) $D63F0000 swap 5 lshift or MSK ;

: ENC-BR ( n -- n ) $D61F0000 swap 5 lshift or MSK ;

: ENC-BRK ( -- n ) $D4200000 ;

: ENC-NOP ( -- n ) $D503201F ;

\ cache maintenance to PoU (JIT coherency) + barriers
: ENC-ICIVAU ( n -- n ) $D50B7520 or MSK ;

: ENC-DCCVAU ( n -- n ) $D50B7B20 or MSK ;

: ENC-DSB-ISH ( -- n ) $D5033B9F ;

: ENC-ISB ( -- n ) $D5033FDF ;

\ adr rd, . + d  (d = BYTE offset from this instruction; word-aligned here so immlo=0)
: ENC-ADRD ( n -- n ) dup 3 and 29 lshift  swap 4 / $7FFFF and 5 lshift or ;

: ENC-ADR ( n n -- n )
   ARM-IMM ! ARM-RD !
   $10000000 ARM-RD @ or  ARM-IMM @ ENC-ADRD or MSK ;

\ FP (double, D-register file): engine-grade set, golden vs habu in t-sh-fp-enc
: ENC-FMOVDD ( n n -- n ) $1E604000 RR ;

: ENC-FDIV ( n n n -- n ) $1E601800 RRR ;

: ENC-FNEG ( n n -- n )  $1E614000 RR ;

: ENC-FABS ( n n -- n )  $1E60C000 RR ;

: ENC-FSQRT ( n n -- n )  $1E61C000 RR ;

: ENC-FCMP ( n n -- n )
   ARM-RM ! ARM-RN !
   $1E602000 ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

: ENC-FCMP0 ( n -- n ) $1E602008 swap 5 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) $9E620000 RR ;

: ENC-FCVTZS ( n n -- n ) $9E780000 RR ;
