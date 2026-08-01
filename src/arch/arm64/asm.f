\ asm.fs — ARM64 instruction encoders in the STANDALONE's Forth (operands -> u32).
\ Hex constants preserve the bit layout used by the native engine builder.
\ The standalone encodes ARM64 directly instead of relying on baked host output.
\
\ Every encoder refuses an operand that does not fit its field, and every
\ encoder that divides a byte operand by an access scale refuses a value that
\ division would round down. Both refusals happen BEFORE any bit is packed, so
\ a bad operand ends the process with a diagnostic instead of running into the
\ neighbouring field and emitting a different instruction. The two operands
\ this file does not bound are bounded by the layer that computes them: a
\ branch or ADR displacement is a label distance, and src/arch/arm64/icode.f
\ checks its reach (?REL26, ?REL19, ?ADR) before handing it over, in words for
\ a branch and in bytes for ADR — which its word-counting position also makes
\ a multiple of four by construction.
$FFFFFFFF constant ARM64-W32

: MSK ( n -- n ) ARM64-W32 and ;

\ Every refusal in this file ends the process with this status, the same code
\ the label and branch layer in icode.fs reports, so one status means "the
\ assembler refused to encode that" wherever the refusal came from.
72 constant ASM-EXIT-RC

\ Operand bounds, one per kind of field, each derived from the WIDTH of the
\ field the encoder drops the operand into: an unsigned field of w bits holds
\ 0 .. 2^w - 1. They are written here once, so no encoder carries a bound of
\ its own and no bound can drift from the field it describes.
1  5 lshift constant REG-LIM     \ Rd/Rn/Rm/Rt register number
1 16 lshift constant IMM16-LIM   \ move-wide immediate, SVC number
1 12 lshift constant IMM12-LIM   \ add/sub/compare immediate, load/store offset field
1 13 lshift constant NIS-LIM     \ packed N:immr:imms of a logical immediate
1  6 lshift constant SHIFT-LIM   \ shift amount for a 64-bit register
1  4 lshift constant COND-LIM    \ condition code
1  2 lshift constant HW-LIM      \ move-wide shifted-half selector

\ True when v does not fit the unsigned field that holds 0 .. lim-1. Every
\ bound below is this one test; only the limit and the diagnostic differ.
: OUT? ( n n -- bool )  swap dup 0 < swap rot >= or ;

: ?REG ( n -- n )
   dup REG-LIM OUT? IF s" asm: register number out of range" ASM-EXIT-RC die THEN ;

: ?IMM16 ( n -- n )
   dup IMM16-LIM OUT? IF s" asm: 16-bit immediate out of range" ASM-EXIT-RC die THEN ;

: ?IMM12 ( n -- n )
   dup IMM12-LIM OUT? IF s" asm: 12-bit immediate out of range" ASM-EXIT-RC die THEN ;

: ?NIS ( n -- n )
   dup NIS-LIM OUT? IF s" asm: logical immediate out of range" ASM-EXIT-RC die THEN ;

: ?SHIFT ( n -- n )
   dup SHIFT-LIM OUT? IF s" asm: shift amount out of range" ASM-EXIT-RC die THEN ;

: ?COND ( n -- n )
   dup COND-LIM OUT? IF s" asm: condition code out of range" ASM-EXIT-RC die THEN ;

\ The vocabulary of that four-bit field, in the architecture's own order. It
\ lives beside the field it fills, so a caller that needs "less than" names the
\ condition rather than writing 11, and there is one place that says which
\ number each condition is. src/arch/arm64/mnem.f used to carry a second copy;
\ it now loads after this file and reads these.
 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

: ?HW ( n -- n )
   dup HW-LIM OUT? IF s" asm: move-wide half out of range" ASM-EXIT-RC die THEN ;

\ A byte operand the encoder is about to divide by its access scale. The
\ division rounds down, so a value it would round is refused here rather than
\ silently encoding the aligned operand below it.
: SCALE/ ( n n -- n )
   2dup mod 0 <> IF s" asm: operand is not a multiple of its scale" ASM-EXIT-RC die THEN
   / ;

\ x18 is Darwin platform-reserved: XNU zeroes it on any synchronous trap
\ return (page fault included), so emitted code must never hold live state
\ there. Fail closed at encode time for every X-register operand field.
18 constant ARM-RESERVED-REG

: XREG? ( n -- n )
   ?REG
   dup ARM-RESERVED-REG = IF s" asm: x18 is Darwin-reserved" ASM-EXIT-RC die THEN ;

: XR2 ( n n -- n n )  XREG? swap XREG? swap ;

: XR2ND ( n n -- n n )  swap XREG? swap ;

: XR3 ( n n n -- n n n )  XREG? rot XREG? rot XREG? rot ;

: XRDI ( n n n -- n n n )  rot XREG? rot XREG? rot ;

\ The D-register file uses the same 5-bit operand fields, but no member of it
\ is platform-reserved, so only the field bound applies there.
: DR2 ( n n -- n n )  ?REG swap ?REG swap ;

: DR3 ( n n n -- n n n )  ?REG rot ?REG rot ?REG rot ;

\ move-wide operands: destination register, 16-bit immediate, shifted half.
: XMW3 ( n n n -- n n n )  ?HW rot XREG? rot ?IMM16 rot ;

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
: MOVZHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $D2800000 or MSK ;

: MOVKHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $F2800000 or MSK ;

: MOVNHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $92800000 or MSK ;

\ shifted-register 3-operand: rd rn rm
: RRR ( n n n n -- n )
   ARM-BASE ! ARM-R3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

: ENC-ADD ( n n n -- n ) XR3 $8B000000 RRR ;

: ENC-SUB ( n n n -- n ) XR3 $CB000000 RRR ;

: ENC-AND ( n n n -- n ) XR3 $8A000000 RRR ;

: ENC-ORR ( n n n -- n ) XR3 $AA000000 RRR ;

: ENC-EOR ( n n n -- n ) XR3 $CA000000 RRR ;

\ A register-to-register move. ARM64 has no move form of its own: `mov xd, xm`
\ is `orr xd, xzr, xm`, which is what a disassembler prints back and what the
\ recovery emitter writes too. Operand 31 in a logical form is the zero
\ register, so the copy is that one form with 31 as its first source, written
\ once here rather than at each caller.
31 constant ARM-ZERO-REG
: ENC-MOV ( n n -- n ) ARM-ZERO-REG swap ENC-ORR ;

\ A negation, for the same reason and by the same route: ARM64 has no negate
\ form either, and `neg xd, xm` IS `sub xd, xzr, xm`. It is the last instruction
\ of the three a Habu comparison compiles to - compare, set one on the
\ condition, negate - because a Habu flag is all bits set and not one.
: ENC-NEG ( n n -- n ) ARM-ZERO-REG swap ENC-SUB ;

: ENC-MUL ( n n n -- n ) XR3 $9B007C00 RRR ;

: RRI ( n n n n -- n )
   ARM-BASE ! ARM-I3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-IMM @ 10 lshift or MSK ;

: RR ( n n n -- n )
   ARM-BASE ! ARM-R2!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or MSK ;

\ add/sub immediate: rd rn imm12
: ENC-ADDI ( n n n -- n ) XRDI ?IMM12 $91000000 RRI ;

: ENC-SUBI ( n n n -- n ) XRDI ?IMM12 $D1000000 RRI ;

\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI ( n n n -- n )
   XRDI ?SHIFT ARM-SH ! ARM-RN ! ARM-RD !
   $D3400000 ARM-RD @ or  ARM-RN @ 5 lshift or
   $40 ARM-SH @ - $3F and 16 lshift or  $3F ARM-SH @ - 10 lshift or MSK ;

\ logical-shift-right immediate: rd rn sh
: ENC-LSRI ( n n n -- n )
   XRDI ?SHIFT ARM-SH ! ARM-RN ! ARM-RD !
   $D340FC00 ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-SH @ 16 lshift or MSK ;

\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP ( n n -- n )
   XR2 ARM-RM ! ARM-RN !
   $EB00001F ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

\ compare immediate rn imm12
: ENC-CMPI ( n n -- n )
   XR2ND ?IMM12 ARM-IMM ! ARM-RN !
   $F100001F ARM-RN @ 5 lshift or  ARM-IMM @ 10 lshift or MSK ;

\ svc #imm  ; ret
: ENC-SVC ( n -- n ) ?IMM16 $D4000001 swap 5 lshift or MSK ;

: ENC-RET ( -- n )  $D65F03C0 ;

\ load/store, unsigned offset. habu scales: x-regs by 8 (?SC8), w by 4, byte by 1.
: ENC-LDR ( n n n -- n ) XRDI 8 SCALE/ ?IMM12 $F9400000 RRI ;

: ENC-STR ( n n n -- n ) XRDI 8 SCALE/ ?IMM12 $F9000000 RRI ;

: ENC-LDRB ( n n n -- n ) XRDI ?IMM12 $39400000 RRI ;

: ENC-STRB ( n n n -- n ) XRDI ?IMM12 $39000000 RRI ;

: ENC-LDRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9400000 RRI ;

: ENC-STRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9000000 RRI ;

: ENC-LDAR ( n n -- n ) XR2 5 lshift or $C8DFFC00 or MSK ;

: ENC-STLR ( n n -- n ) XR2 5 lshift or $C89FFC00 or MSK ;

\ branches: delta is in WORDS (instruction-relative), sign-handled by the caller's mask.
: ENC-B ( n -- n ) $3FFFFFF and $14000000 or MSK ;

: ENC-BL ( n -- n ) $3FFFFFF and $94000000 or MSK ;

: ENC-BCOND ( n n -- n )
   ?COND ARM-IMM ! $7FFFF and 5 lshift $54000000 or ARM-IMM @ or MSK ;

: ENC-CBZ ( n n -- n )
   XR2ND swap ARM-RD ! $7FFFF and 5 lshift $B4000000 or ARM-RD @ or MSK ;

: ENC-CBNZ ( n n -- n )
   XR2ND swap ARM-RD ! $7FFFF and 5 lshift $B5000000 or ARM-RD @ or MSK ;

\ FP (double): operands in the D-register file. DR2/DR3 bound every D field;
\ XREG?/XR2ND then adds the reserved-register refusal to the one X operand.
: ENC-FMOVXD ( n n -- n ) DR2 XREG? $9E670000 RR ;   \ X->D bits

: ENC-FMOVDX ( n n -- n ) DR2 XR2ND $9E660000 RR ;   \ D->X bits

: ENC-FADD ( n n n -- n ) DR3 $1E602800 RRR ;

: ENC-FSUB ( n n n -- n ) DR3 $1E603800 RRR ;

: ENC-FMUL ( n n n -- n ) DR3 $1E600800 RRR ;

\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET ( n n -- n )
   XR2ND ?COND ARM-IMM ! ARM-RD !
   $9A9F07E0 ARM-RD @ or  ARM-IMM @ 1 xor 12 lshift or MSK ;

\ data-processing 2-source: shift-by-register, divide
: ENC-LSLV ( n n n -- n ) XR3 $9AC02000 RRR ;

: ENC-LSRV ( n n n -- n ) XR3 $9AC02400 RRR ;

: ENC-ASRV ( n n n -- n ) XR3 $9AC02800 RRR ;

: ENC-SDIV ( n n n -- n ) XR3 $9AC00C00 RRR ;

: ENC-UDIV ( n n n -- n ) XR3 $9AC00800 RRR ;

\ arithmetic shift right immediate (SBFM)
: ENC-ASRI ( n n n -- n )
   XRDI ?SHIFT ARM-SH ! ARM-RN ! ARM-RD !
   $9340FC00 ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-SH @ 16 lshift or MSK ;

\ logical immediates (nis = N<<12 | immr<<6 | imms). `>LIMM` already builds nis
\ inside the 13-bit field, so ?NIS only ever fires for a caller that packs its
\ own value and hands it straight to an encoder.
: ENC-ANDI ( n n n -- n ) XRDI ?NIS $92000000 RRI ;

: ENC-ORRI ( n n n -- n ) XRDI ?NIS $B2000000 RRI ;

: ENC-EORI ( n n n -- n ) XRDI ?NIS $D2000000 RRI ;

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

: LIMM-BAD ( -- )  s" asm: bad logical immediate" ASM-EXIT-RC die ;

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
: ENC-BLR ( n -- n ) XREG? $D63F0000 swap 5 lshift or MSK ;

: ENC-BR ( n -- n ) XREG? $D61F0000 swap 5 lshift or MSK ;

: ENC-BRK ( -- n ) $D4200000 ;

: ENC-NOP ( -- n ) $D503201F ;

\ cache maintenance to PoU (JIT coherency) + barriers
: ENC-ICIVAU ( n -- n ) XREG? $D50B7520 or MSK ;

: ENC-DCCVAU ( n -- n ) XREG? $D50B7B20 or MSK ;

: ENC-DSB-ISH ( -- n ) $D5033B9F ;

: ENC-ISB ( -- n ) $D5033FDF ;

\ adr rd, . + d  (d = BYTE offset from this instruction; word-aligned here so immlo=0)
: ENC-ADRD ( n -- n ) dup 3 and 29 lshift  swap 4 / $7FFFF and 5 lshift or ;

: ENC-ADR ( n n -- n )
   XR2ND ARM-IMM ! ARM-RD !
   $10000000 ARM-RD @ or  ARM-IMM @ ENC-ADRD or MSK ;

\ FP (double, D-register file): engine-grade set, golden vs habu in t-sh-fp-enc
: ENC-FMOVDD ( n n -- n ) DR2 $1E604000 RR ;

: ENC-FDIV ( n n n -- n ) DR3 $1E601800 RRR ;

: ENC-FNEG ( n n -- n )  DR2 $1E614000 RR ;

: ENC-FABS ( n n -- n )  DR2 $1E60C000 RR ;

: ENC-FSQRT ( n n -- n )  DR2 $1E61C000 RR ;

: ENC-FCMP ( n n -- n )
   DR2 ARM-RM ! ARM-RN !
   $1E602000 ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

: ENC-FCMP0 ( n -- n ) ?REG $1E602008 swap 5 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) DR2 XREG? $9E620000 RR ;

: ENC-FCVTZS ( n n -- n ) DR2 XR2ND $9E780000 RR ;
