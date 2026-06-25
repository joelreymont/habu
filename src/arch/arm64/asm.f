\ asm.fs — ARM64 instruction encoders in the STANDALONE's Forth (operands -> u32).
\ Decimal constants preserve the bit layout used by the native engine builder.
\ The standalone encodes ARM64 directly instead of relying on baked host output.
4294967295 constant W32

: MSK ( n -- n ) W32 and ;

\ move-wide: rd imm16 hw -> u32
: MOVZHW ( n n n -- n ) {: RD imm hw :} 3531603968 RD or  imm 5 lshift or  hw 21 lshift or MSK ;

: MOVKHW ( n n n -- n ) {: RD imm hw :} 4068474880 RD or  imm 5 lshift or  hw 21 lshift or MSK ;

: MOVNHW ( n n n -- n ) {: RD imm hw :} 2457862144 RD or  imm 5 lshift or  hw 21 lshift or MSK ;

\ shifted-register 3-operand: rd rn rm
: RRR ( n n n n -- n ) {: base RD RN RM :} base RD or  RN 5 lshift or  RM 16 lshift or MSK ;

: ENC-ADD ( n n n -- n ) {: RD RN RM :} 2332033024 RD RN RM RRR ;

: ENC-SUB ( n n n -- n ) {: RD RN RM :} 3405774848 RD RN RM RRR ;

: ENC-AND ( n n n -- n ) {: RD RN RM :} 2315255808 RD RN RM RRR ;

: ENC-ORR ( n n n -- n ) {: RD RN RM :} 2852126720 RD RN RM RRR ;

: ENC-EOR ( n n n -- n ) {: RD RN RM :} 3388997632 RD RN RM RRR ;

: ENC-MUL ( n n n -- n ) {: RD RN RM :} 2600500224 RD RN RM RRR ;

\ add/sub immediate: rd rn imm12
: ENC-ADDI ( n n n -- n ) {: RD RN imm :} 2432696320 RD or  RN 5 lshift or  imm 10 lshift or MSK ;

: ENC-SUBI ( n n n -- n ) {: RD RN imm :} 3506438144 RD or  RN 5 lshift or  imm 10 lshift or MSK ;

\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI ( n n n -- n ) {: RD RN sh :} 3544186880 RD or  RN 5 lshift or
   64 sh - 63 and 16 lshift or  63 sh - 10 lshift or MSK ;

\ logical-shift-right immediate: rd rn sh
: ENC-LSRI ( n n n -- n ) {: RD RN sh :} 3544251392 RD or  RN 5 lshift or  sh 16 lshift or MSK ;

\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP ( n n -- n ) {: RN RM :} 3942645791 RN 5 lshift or  RM 16 lshift or MSK ;

\ compare immediate rn imm12
: ENC-CMPI ( n n -- n ) {: RN imm :} 4043309087 RN 5 lshift or  imm 10 lshift or MSK ;

\ svc #imm  ; ret
: ENC-SVC ( n -- n ) {: imm :} 3556769793 imm 5 lshift or MSK ;

: ENC-RET ( -- n )  3596551104 ;

\ load/store, unsigned offset. habu scales: x-regs by 8 (?SC8), w by 4, byte by 1.
: ENC-LDR ( n n n -- n ) {: RD RN off :} 4181721088 RD or  RN 5 lshift or  off 8 / 10 lshift or MSK ;

: ENC-STR ( n n n -- n ) {: RD RN off :} 4177526784 RD or  RN 5 lshift or  off 8 / 10 lshift or MSK ;

: ENC-LDRB ( n n n -- n ) {: RD RN off :} 960495616  RD or  RN 5 lshift or  off 10 lshift or MSK ;

: ENC-STRB ( n n n -- n ) {: RD RN off :} 956301312  RD or  RN 5 lshift or  off 10 lshift or MSK ;

: ENC-LDRW ( n n n -- n ) {: RD RN off :} 3107979264 RD or  RN 5 lshift or  off 4 / 10 lshift or MSK ;

: ENC-STRW ( n n n -- n ) {: RD RN off :} 3103784960 RD or  RN 5 lshift or  off 4 / 10 lshift or MSK ;

\ branches: delta is in WORDS (instruction-relative), sign-handled by the caller's mask.
: ENC-B ( n -- n ) {: d26 :} 335544320  d26 $3FFFFFF and or MSK ;

: ENC-BL ( n -- n ) {: d26 :} 2483027968 d26 $3FFFFFF and or MSK ;

: ENC-BCOND ( n n -- n ) {: d19 cond :} 1409286144 d19 524287 and 5 lshift or  cond or MSK ;

: ENC-CBZ ( n n -- n ) {: rt d19 :}  3019898880 d19 524287 and 5 lshift or  rt or MSK ;

: ENC-CBNZ ( n n -- n ) {: rt d19 :}  3036676096 d19 524287 and 5 lshift or  rt or MSK ;

\ FP (double): operands in the D-register file
: ENC-FMOVXD ( n n -- n ) {: d n :} 2657550336 d or  n 5 lshift or MSK ;   \ X->D bits

: ENC-FMOVDX ( n n -- n ) {: d n :} 2657484800 d or  n 5 lshift or MSK ;   \ D->X bits

: ENC-FADD ( n n n -- n ) {: d n m :} 509618176 d or  n 5 lshift or  m 16 lshift or MSK ;

: ENC-FSUB ( n n n -- n ) {: d n m :} 509622272 d or  n 5 lshift or  m 16 lshift or MSK ;

: ENC-FMUL ( n n n -- n ) {: d n m :} 509609984 d or  n 5 lshift or  m 16 lshift or MSK ;

\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET ( n n -- n ) {: RD cond :}  2594113504 RD or  cond 1 xor 12 lshift or MSK ;

\ data-processing 2-source: shift-by-register, divide
: ENC-LSLV ( n n n -- n ) {: RD RN RM :} $9AC02000 RD RN RM RRR ;

: ENC-LSRV ( n n n -- n ) {: RD RN RM :} $9AC02400 RD RN RM RRR ;

: ENC-ASRV ( n n n -- n ) {: RD RN RM :} $9AC02800 RD RN RM RRR ;

: ENC-SDIV ( n n n -- n ) {: RD RN RM :} $9AC00C00 RD RN RM RRR ;

: ENC-UDIV ( n n n -- n ) {: RD RN RM :} $9AC00800 RD RN RM RRR ;

\ arithmetic shift right immediate (SBFM)
: ENC-ASRI ( n n n -- n ) {: RD RN sh :} $9340FC00 RD or  RN 5 lshift or  sh 16 lshift or MSK ;

\ logical immediates (nis = N<<12 | immr<<6 | imms)
: ENC-ANDI ( n n n -- n ) {: RD RN nis :} $92000000 RD or  RN 5 lshift or  nis 10 lshift or MSK ;

: ENC-ORRI ( n n n -- n ) {: RD RN nis :} $B2000000 RD or  RN 5 lshift or  nis 10 lshift or MSK ;

: ENC-EORI ( n n n -- n ) {: RD RN nis :} $D2000000 RD or  RN 5 lshift or  nis 10 lshift or MSK ;

\ encodeBitMasks: plain mask -> nis. A valid mask is a power-of-2-sized
\ repeating element (2..64 bits) that is a rotated contiguous run of ones;
\ 0 and all-ones are not encodable (die 72).
variable PCX

: POPC64 ( n -- n ) {: x :}
   x PCX !  0
   BEGIN PCX @ 0 <> WHILE  PCX @ 1 and +  PCX @ 1 rshift PCX !  REPEAT ;

: EMASK ( n -- n ) {: e :}   \ low-e-bits mask; lshift wraps at 64
   e 64 = IF -1 ELSE 1 e lshift 1 - THEN ;

variable REMSK

: RORE ( n n n -- n ) {: x r e :}
   e EMASK REMSK !
   x REMSK @ and dup  r rshift  swap e r - lshift  or  REMSK @ and ;

: HALVES= ( n n -- bool ) {: x e :}
   1 e 2 / lshift 1 -  dup  x and  swap  x e 2 / rshift and  = ;

variable LELE

: LELEM ( n -- n n ) {: x :}
   64 LELE !
   BEGIN  LELE @ 2 >  x LELE @ HALVES= and  WHILE  LELE @ 2 / LELE !  REPEAT
   x LELE @ EMASK and  LELE @ ;

variable LRI

: LROT ( n n n -- n ) {: elem ones e :}
   -1  0 LRI !
   BEGIN LRI @ e < WHILE
      1 ones lshift 1 -  LRI @ e RORE  elem = IF
         drop LRI @  e LRI !
      ELSE
         LRI @ 1 + LRI !
      THEN
   REPEAT ;

: LIMM-PACK ( n n n -- n ) {: r ones e :}
   e 64 = IF $1000 ELSE 0 THEN
   r 6 lshift or
   e 2 * 1 - 63 and 63 xor  ones 1 - or  or ;

variable LIE   variable LIONES

: LIMM-BAD ( -- )  s" asm: bad logical immediate" 72 die ;

: >LIMM ( n -- n ) {: mask :}
   mask 0 =  mask -1 =  or IF LIMM-BAD THEN
   mask LELEM LIE !                                  \ ( elem ), e in LIE
   dup POPC64 LIONES !
   LIONES @ LIE @ = IF LIMM-BAD THEN
   LIONES @ LIE @ LROT                               \ ( r | -1 )
   dup 0 < IF LIMM-BAD THEN
   LIONES @ LIE @ LIMM-PACK ;

\ indirect branches, trap, nop
: ENC-BLR ( n -- n ) {: RN :} $D63F0000 RN 5 lshift or MSK ;

: ENC-BR ( n -- n ) {: RN :} $D61F0000 RN 5 lshift or MSK ;

: ENC-BRK ( -- n ) $D4200000 ;

: ENC-NOP ( -- n ) $D503201F ;

\ cache maintenance to PoU (JIT coherency) + barriers
: ENC-ICIVAU ( n -- n ) {: rt :} $D50B7520 rt or MSK ;

: ENC-DCCVAU ( n -- n ) {: rt :} $D50B7B20 rt or MSK ;

: ENC-DSB-ISH ( -- n ) $D5033B9F ;

: ENC-ISB ( -- n ) $D5033FDF ;

\ adr rd, . + d  (d = BYTE offset from this instruction; word-aligned here so immlo=0)
: ENC-ADRD ( n -- n ) {: d :}  d 3 and 29 lshift  d 4 / $7FFFF and 5 lshift or ;

: ENC-ADR ( n n -- n ) {: RD d :}  $10000000 RD or  d ENC-ADRD or MSK ;

\ FP (double, D-register file): engine-grade set, golden vs habu in t-sh-fp-enc
: ENC-FMOVDD ( n n -- n ) {: d n :} $1E604000 d or  n 5 lshift or MSK ;

: ENC-FDIV ( n n n -- n ) {: d n m :} $1E601800 d or  n 5 lshift or  m 16 lshift or MSK ;

: ENC-FNEG ( n n -- n ) {: d n :}  $1E614000 d or  n 5 lshift or MSK ;

: ENC-FABS ( n n -- n ) {: d n :}  $1E60C000 d or  n 5 lshift or MSK ;

: ENC-FSQRT ( n n -- n ) {: d n :}  $1E61C000 d or  n 5 lshift or MSK ;

: ENC-FCMP ( n n -- n ) {: n m :}  $1E602000 n 5 lshift or  m 16 lshift or MSK ;

: ENC-FCMP0 ( n -- n ) {: n :}    $1E602008 n 5 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) {: d n :} $9E620000 d or  n 5 lshift or MSK ;

: ENC-FCVTZS ( n n -- n ) {: d n :} $9E780000 d or  n 5 lshift or MSK ;
