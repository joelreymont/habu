\ asm.fs — ARM64 instruction encoders in the STANDALONE's Forth (operands -> u32).
\ Ported from bootstrap/cg/asm.fs (decimal constants; same bit layout). First step of the
\ codegen port: the standalone can now ENCODE ARM64, not just run baked code. Verified
\ byte-for-byte against habu in test/t-sh-asm.fs.
4294967295 constant W32
: MSK W32 and ;
\ move-wide: rd imm16 hw -> u32
: MOVZHW {: rd imm hw :} 3531603968 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
: MOVKHW {: rd imm hw :} 4068474880 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
: MOVNHW {: rd imm hw :} 2457862144 rd or  imm 5 lshift or  hw 21 lshift or MSK ;
\ shifted-register 3-operand: rd rn rm
: RRR {: base rd rn rm :} base rd or  rn 5 lshift or  rm 16 lshift or MSK ;
: ENC-ADD  {: rd rn rm :} 2332033024 rd rn rm RRR ;
: ENC-SUB  {: rd rn rm :} 3405774848 rd rn rm RRR ;
: ENC-AND  {: rd rn rm :} 2315255808 rd rn rm RRR ;
: ENC-ORR  {: rd rn rm :} 2852126720 rd rn rm RRR ;
: ENC-EOR  {: rd rn rm :} 3388997632 rd rn rm RRR ;
: ENC-MUL  {: rd rn rm :} 2600500224 rd rn rm RRR ;
\ add/sub immediate: rd rn imm12
: ENC-ADDI {: rd rn imm :} 2432696320 rd or  rn 5 lshift or  imm 10 lshift or MSK ;
: ENC-SUBI {: rd rn imm :} 3506438144 rd or  rn 5 lshift or  imm 10 lshift or MSK ;
\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI {: rd rn sh :} 3544186880 rd or  rn 5 lshift or
   64 sh - 63 and 16 lshift or  63 sh - 10 lshift or MSK ;
\ logical-shift-right immediate: rd rn sh
: ENC-LSRI {: rd rn sh :} 3544251392 rd or  rn 5 lshift or  sh 16 lshift or MSK ;
\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP  {: rn rm :} 3942645791 rn 5 lshift or  rm 16 lshift or MSK ;
\ compare immediate rn imm12
: ENC-CMPI {: rn imm :} 4043309087 rn 5 lshift or  imm 10 lshift or MSK ;
\ svc #imm  ; ret
: ENC-SVC  {: imm :} 3556769793 imm 5 lshift or MSK ;
: ENC-RET  3596551104 ;
\ load/store, unsigned offset. habu scales: x-regs by 8 (?SC8), w by 4, byte by 1.
: ENC-LDR  {: rd rn off :} 4181721088 rd or  rn 5 lshift or  off 8 / 10 lshift or MSK ;
: ENC-STR  {: rd rn off :} 4177526784 rd or  rn 5 lshift or  off 8 / 10 lshift or MSK ;
: ENC-LDRB {: rd rn off :} 960495616  rd or  rn 5 lshift or  off 10 lshift or MSK ;
: ENC-STRB {: rd rn off :} 956301312  rd or  rn 5 lshift or  off 10 lshift or MSK ;
: ENC-LDRW {: rd rn off :} 3107979264 rd or  rn 5 lshift or  off 4 / 10 lshift or MSK ;
: ENC-STRW {: rd rn off :} 3103784960 rd or  rn 5 lshift or  off 4 / 10 lshift or MSK ;
\ branches: delta is in WORDS (instruction-relative), sign-handled by the caller's mask.
: ENC-B     {: d26 :}     335544320  d26 $3FFFFFF and or MSK ;
: ENC-BL    {: d26 :}     2483027968 d26 $3FFFFFF and or MSK ;
: ENC-BCOND {: d19 cond :} 1409286144 d19 524287 and 5 lshift or  cond or MSK ;
: ENC-CBZ   {: rt d19 :}  3019898880 d19 524287 and 5 lshift or  rt or MSK ;
: ENC-CBNZ  {: rt d19 :}  3036676096 d19 524287 and 5 lshift or  rt or MSK ;
\ FP (double): operands in the D-register file
: ENC-FMOVXD {: d n :} 2657550336 d or  n 5 lshift or MSK ;   \ X->D bits
: ENC-FMOVDX {: d n :} 2657484800 d or  n 5 lshift or MSK ;   \ D->X bits
: ENC-FADD {: d n m :} 509618176 d or  n 5 lshift or  m 16 lshift or MSK ;
: ENC-FSUB {: d n m :} 509622272 d or  n 5 lshift or  m 16 lshift or MSK ;
: ENC-FMUL {: d n m :} 509609984 d or  n 5 lshift or  m 16 lshift or MSK ;
\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET {: rd cond :}  2594113504 rd or  cond 1 xor 12 lshift or MSK ;
\ data-processing 2-source: shift-by-register, divide
: ENC-LSLV {: rd rn rm :} $9AC02000 rd rn rm RRR ;
: ENC-LSRV {: rd rn rm :} $9AC02400 rd rn rm RRR ;
: ENC-ASRV {: rd rn rm :} $9AC02800 rd rn rm RRR ;
: ENC-SDIV {: rd rn rm :} $9AC00C00 rd rn rm RRR ;
: ENC-UDIV {: rd rn rm :} $9AC00800 rd rn rm RRR ;
\ arithmetic shift right immediate (SBFM)
: ENC-ASRI {: rd rn sh :} $9340FC00 rd or  rn 5 lshift or  sh 16 lshift or MSK ;
\ logical immediates (nis = N<<12 | immr<<6 | imms)
: ENC-ANDI {: rd rn nis :} $92000000 rd or  rn 5 lshift or  nis 10 lshift or MSK ;
: ENC-ORRI {: rd rn nis :} $B2000000 rd or  rn 5 lshift or  nis 10 lshift or MSK ;
: ENC-EORI {: rd rn nis :} $D2000000 rd or  rn 5 lshift or  nis 10 lshift or MSK ;

\ encodeBitMasks: plain mask -> nis. A valid mask is a power-of-2-sized
\ repeating element (2..64 bits) that is a rotated contiguous run of ones;
\ 0 and all-ones are not encodable (die 72).
variable PCX

: POPC64 {: x :}  \ ( x -- n )
   x PCX !  0
   BEGIN PCX @ 0 <> WHILE  PCX @ 1 and +  PCX @ 1 rshift PCX !  REPEAT ;

: EMASK {: e :}   \ ( e -- mask ) low-e-bits mask; lshift wraps at 64
   e 64 = IF -1 ELSE 1 e lshift 1 - THEN ;

variable REMSK

: RORE {: x r e :}  \ ( x r e -- y )
   e EMASK REMSK !
   x REMSK @ and dup  r rshift  swap e r - lshift  or  REMSK @ and ;

: HALVES= {: x e :}  \ ( x e -- f )
   1 e 2 / lshift 1 -  dup  x and  swap  x e 2 / rshift and  = ;

variable LELE

: LELEM {: x :}  \ ( x -- elem e )
   64 LELE !
   BEGIN  LELE @ 2 >  x LELE @ HALVES= and  WHILE  LELE @ 2 / LELE !  REPEAT
   x LELE @ EMASK and  LELE @ ;

variable LRI

: LROT {: elem ones e :}  \ ( elem ones e -- r | -1 )
   -1  0 LRI !
   BEGIN LRI @ e < WHILE
      1 ones lshift 1 -  LRI @ e RORE  elem = IF
         drop LRI @  e LRI !
      ELSE
         LRI @ 1 + LRI !
      THEN
   REPEAT ;

: LIMM-PACK {: r ones e :}  \ ( r ones e -- nis )
   e 64 = IF $1000 ELSE 0 THEN
   r 6 lshift or
   e 2 * 1 - 63 and 63 xor  ones 1 - or  or ;

variable LIE   variable LIONES

: LIMM-BAD  s" asm: bad logical immediate" 72 die ;

: >LIMM {: mask :}  \ ( mask -- nis )
   mask 0 =  mask -1 =  or IF LIMM-BAD THEN
   mask LELEM LIE !                                  \ ( elem ), e in LIE
   dup POPC64 LIONES !
   LIONES @ LIE @ = IF LIMM-BAD THEN
   LIONES @ LIE @ LROT                               \ ( r | -1 )
   dup 0 < IF LIMM-BAD THEN
   LIONES @ LIE @ LIMM-PACK ;
\ indirect branches, trap, nop
: ENC-BLR {: rn :} $D63F0000 rn 5 lshift or MSK ;
: ENC-BR  {: rn :} $D61F0000 rn 5 lshift or MSK ;
: ENC-BRK $D4200000 ;
: ENC-NOP $D503201F ;
\ cache maintenance to PoU (JIT coherency) + barriers
: ENC-ICIVAU {: rt :} $D50B7520 rt or MSK ;
: ENC-DCCVAU {: rt :} $D50B7B20 rt or MSK ;
: ENC-DSB-ISH $D5033B9F ;
: ENC-ISB     $D5033FDF ;
\ adr rd, . + d  (d = BYTE offset from this instruction; word-aligned here so immlo=0)
: ENC-ADRD {: d :}  d 3 and 29 lshift  d 4 / $7FFFF and 5 lshift or ;
: ENC-ADR {: rd d :}  $10000000 rd or  d ENC-ADRD or MSK ;
\ FP (double, D-register file): engine-grade set, golden vs habu in t-sh-fp-enc
: ENC-FMOVDD {: d n :} $1E604000 d or  n 5 lshift or MSK ;
: ENC-FDIV  {: d n m :} $1E601800 d or  n 5 lshift or  m 16 lshift or MSK ;
: ENC-FNEG  {: d n :}  $1E614000 d or  n 5 lshift or MSK ;
: ENC-FABS  {: d n :}  $1E60C000 d or  n 5 lshift or MSK ;
: ENC-FSQRT {: d n :}  $1E61C000 d or  n 5 lshift or MSK ;
: ENC-FCMP  {: n m :}  $1E602000 n 5 lshift or  m 16 lshift or MSK ;
: ENC-FCMP0 {: n :}    $1E602008 n 5 lshift or MSK ;
: ENC-SCVTF  {: d n :} $9E620000 d or  n 5 lshift or MSK ;
: ENC-FCVTZS {: d n :} $9E780000 d or  n 5 lshift or MSK ;
