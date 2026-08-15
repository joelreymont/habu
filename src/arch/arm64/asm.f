\ asm.f — ARM64 instruction encoders, package A64ASM (operands -> u32).
\ Hex constants preserve the bit layout the native engine builder uses.
\
\ Every encoder screens its operand BEFORE packing a bit, so a bad operand dies
\ instead of overflowing the neighbouring field; branch and ADR displacements
\ are the exception, bounded by src/arch/arm64/icode.f (?REL26/?REL19/?ADR).

package A64ASM

$FFFFFFFF constant ARM64-W32

: MSK ( n -- n ) ARM64-W32 and ;

\ Every refusal in this file exits with this status, and so does the label and
\ branch layer in src/arch/arm64/icode.f.
72 constant ASM-EXIT-RC

public

1  5 lshift constant REG-LIM     \ Rd/Rn/Rm/Rt register number
1 16 lshift constant IMM16-LIM   \ move-wide immediate, SVC number
1 12 lshift constant IMM12-LIM   \ add/sub/compare immediate, load/store offset field
1  4 lshift constant COND-LIM    \ condition code
1  2 lshift constant HW-LIM      \ move-wide shifted-half selector

\ Nine bits at bit twelve, so a negative offset is its two's complement there.
1 9 lshift 1 - constant SIMM9-MASK
12 constant SIMM9-SHIFT

\ x18 is Darwin platform-reserved: XNU zeroes it on any synchronous trap return,
\ so emitted code must never hold live state there. Fail closed at encode time.
18 constant ARM-RESERVED-REG

 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

private

1 13 lshift constant NIS-LIM     \ packed N:immr:imms of a logical immediate
1  6 lshift constant SHIFT-LIM   \ shift amount for a 64-bit register
1  3 lshift constant LANEH-LIM   \ which 16-bit lane of a vector UMOV reads

\ True when v is outside the unsigned field that holds 0 .. lim-1.
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

\ Signed nine-bit field, -256 .. 255, counted in BYTES rather than access widths.
1 8 lshift constant SIMM9-LIM        \ the magnitude one side of the field holds

: ?SIMM9 ( n -- n )
   dup dup SIMM9-LIM negate < swap SIMM9-LIM >= or
   IF s" asm: 9-bit signed offset out of range" ASM-EXIT-RC die THEN ;

: ?COND ( n -- n )
   dup COND-LIM OUT? IF s" asm: condition code out of range" ASM-EXIT-RC die THEN ;

: ?HW ( n -- n )
   dup HW-LIM OUT? IF s" asm: move-wide half out of range" ASM-EXIT-RC die THEN ;

\ Three bits, not five: the two bits below the lane index are opcode and say the
\ lane is 16 bits wide.
: ?LANEH ( n -- n )
   dup LANEH-LIM OUT? IF s" asm: vector lane index out of range" ASM-EXIT-RC die THEN ;

\ Operand 31 in a logical or an add/sub form is the zero register.
31 constant ARM-ZERO-REG

: XREG? ( n -- n )
   ?REG
   dup ARM-RESERVED-REG = IF s" asm: x18 is Darwin-reserved" ASM-EXIT-RC die THEN ;

: XR2 ( n n -- n n )  XREG? swap XREG? swap ;

: XR2ND ( n n -- n n )  swap XREG? swap ;

: XR3 ( n n n -- n n n )  XREG? rot XREG? rot XREG? rot ;

: XR4 ( n n n n -- n n n n )  XR2 2swap XR2 2swap ;

: XRDI ( n n n -- n n n )  rot XREG? rot XREG? rot ;

\ The D-register file uses the same 5-bit operand fields, but no member of it
\ is platform-reserved, so only the field bound applies there.
: DR2 ( n n -- n n )  ?REG swap ?REG swap ;

: DR3 ( n n n -- n n n )  ?REG rot ?REG rot ?REG rot ;

\ The transfer names a D register (field bound only); the base names an X one.
: DRXN ( n n n -- n n n )  rot ?REG rot XREG? rot ;

\ move-wide operands: destination register, 16-bit immediate, shifted half.
: XMW3 ( n n n -- n n n )  ?HW rot XREG? rot ?IMM16 rot ;

variable ARM-BASE
variable ARM-RD
variable ARM-RN
variable ARM-RM
variable ARM-RA
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

\ shifted-register 3-operand: rd rn rm
: RRR ( n n n n -- n )
   ARM-BASE ! ARM-R3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-RM @ 16 lshift or MSK ;

\ rd rn rm ra; the addend's own five-bit field sits at bit ten.
: RRRA ( n n n n n -- n )
   ARM-BASE ! ARM-RA ! ARM-R3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or
   ARM-RM @ 16 lshift or  ARM-RA @ 10 lshift or MSK ;

: RRI ( n n n n -- n )
   ARM-BASE ! ARM-I3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-IMM @ 10 lshift or MSK ;

: RR ( n n n -- n )
   ARM-BASE ! ARM-R2!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or MSK ;

\ Unscaled SIGNED byte offset: the only spelling for an access below the base.
: RRSI ( n n n n -- n )
   ARM-BASE ! ARM-I3!
   ARM-BASE @ ARM-RD @ or  ARM-RN @ 5 lshift or
   ARM-IMM @ SIMM9-MASK and SIMM9-SHIFT lshift or MSK ;

\ encodeBitMasks: mask -> nis. Encodable = a power-of-2-sized repeating element
\ (2..64 bits) that is a rotated run of ones; 0 and all-ones are not.
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

\ This file is loaded as bare source by the encoder gate, without lib/prelude.f,
\ so `true` and `false` are spelled here as the comparisons the prelude uses.
: LIMM-YES ( -- bool ) 0 0= ;
: LIMM-NO  ( -- bool ) 0 0= 0= ;

\ Answers rather than dying, because >LIMM must die and LIMM? must choose. The
\ nis is 0 when false, so ignoring the flag encodes `and rd, rn, #1`.
: LIMM-TRY ( n -- n bool )
   ARM-X !
   ARM-X @ 0 =  ARM-X @ -1 =  or IF 0 LIMM-NO exit THEN
   ARM-X @ LELEM LIE !                                  \ ( elem ), e in LIE
   dup POPC64 LIONES !
   LIONES @ LIE @ = IF drop 0 LIMM-NO exit THEN
   LIONES @ LIE @ LROT                               \ ( r | -1 )
   dup 0 < IF drop 0 LIMM-NO exit THEN
   LIONES @ LIE @ LIMM-PACK LIMM-YES ;

public

: SCALE/ ( n n -- n )
   2dup mod 0 <> IF s" asm: operand is not a multiple of its scale" ASM-EXIT-RC die THEN
   / ;

\ move-wide: rd imm16 hw -> u32
: MOVZHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $D2800000 or MSK ;

: MOVKHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $F2800000 or MSK ;

: MOVNHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $92800000 or MSK ;

: ENC-ADD ( n n n -- n ) XR3 $8B000000 RRR ;

: ENC-SUB ( n n n -- n ) XR3 $CB000000 RRR ;

: ENC-AND ( n n n -- n ) XR3 $8A000000 RRR ;

: ENC-ORR ( n n n -- n ) XR3 $AA000000 RRR ;

: ENC-EOR ( n n n -- n ) XR3 $CA000000 RRR ;

\ Orr with the second source inverted: bit 21 is the shifted-register N bit.
: ENC-ORN ( n n n -- n ) XR3 $AA200000 RRR ;

\ ARM64 has no move form: `mov xd, xm` IS `orr xd, xzr, xm`.
: ENC-MOV ( n n -- n ) ARM-ZERO-REG swap ENC-ORR ;

\ ARM64 has no negate form: `neg xd, xm` IS `sub xd, xzr, xm`.
: ENC-NEG ( n n -- n ) ARM-ZERO-REG swap ENC-SUB ;

\ ARM64 has no complement form: `mvn xd, xm` IS `orn xd, xzr, xm`.
: ENC-MVN ( n n -- n ) ARM-ZERO-REG swap ENC-ORN ;

\ MUL is MADD with the addend field already full of register 31 (hence $7C00);
\ SMULH takes three operands - a high half of a product has nothing to add.
: ENC-MUL ( n n n -- n ) XR3 $9B007C00 RRR ;

: ENC-SMULH ( n n n -- n ) XR3 $9B407C00 RRR ;

: ENC-MADD ( n n n n -- n ) XR4 $9B000000 RRRA ;

: ENC-MSUB ( n n n n -- n ) XR4 $9B008000 RRRA ;

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

\ The two unscaled accesses, in the signed addressing mode RRSI packs.
: ENC-LDUR ( n n n -- n ) XRDI ?SIMM9 $F8400000 RRSI ;

: ENC-STUR ( n n n -- n ) XRDI ?SIMM9 $F8000000 RRSI ;

: ENC-LDRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9400000 RRI ;

: ENC-STRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9000000 RRI ;

\ The same four accesses for the D file. One opcode bit differs, bit 26 (V),
\ which says the transferred register is of the SIMD&FP file. DDI 0487 C6.2.
: ENC-LDRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD400000 RRI ;

: ENC-STRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD000000 RRI ;

: ENC-LDURD ( n n n -- n ) DRXN ?SIMM9 $FC400000 RRSI ;

: ENC-STURD ( n n n -- n ) DRXN ?SIMM9 $FC000000 RRSI ;

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

\ The V file IS the D file, so DR2 bounds these too; UMOV's destination is the
\ one general register here. UADDLV sums 16 bytes into 16 bits and cannot overflow.
: ENC-LD1V ( n n -- n ) DR2 XREG? $4C407000 RR ;

: ENC-UADDLV ( n n -- n ) DR2 $6E303800 RR ;

: ENC-UMOVH ( n n n -- n )
   ?LANEH ARM-IMM !  DR2 XR2ND ARM-R2!
   $0E023C00 ARM-RD @ or  ARM-RN @ 5 lshift or  ARM-IMM @ 18 lshift or MSK ;

\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET ( n n -- n )
   XR2ND ?COND ARM-IMM ! ARM-RD !
   $9A9F07E0 ARM-RD @ or  ARM-IMM @ 1 xor 12 lshift or MSK ;

\ rd takes rn when the condition holds and rm when it does not, with no branch.
\ The condition reads whatever last wrote the flags, Fcmp's NaN rule included.
: ENC-CSEL ( n n n n -- n )
   ?COND ARM-IMM !
   XR3 ARM-R3!
   $9A800000 ARM-RD @ or  ARM-RN @ 5 lshift or
   ARM-RM @ 16 lshift or  ARM-IMM @ 12 lshift or MSK ;

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

\ nis = N<<12 | immr<<6 | imms. >LIMM already builds it inside the 13-bit field,
\ so ?NIS only fires for a caller that packs its own value.
: ENC-ANDI ( n n n -- n ) XRDI ?NIS $92000000 RRI ;

: ENC-ORRI ( n n n -- n ) XRDI ?NIS $B2000000 RRI ;

: ENC-EORI ( n n n -- n ) XRDI ?NIS $D2000000 RRI ;

: >LIMM ( n -- n )
   LIMM-TRY 0= IF LIMM-BAD THEN ;

\ The set of encodable masks IS the set the packer can build a nis for.
: LIMM? ( n -- bool )
   LIMM-TRY swap drop ;

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

\ Csel on the D file: same four operand positions, ftype says double. No operand
\ reaches XREG? - d18 holds no platform state - and the condition reads the flags.
: ENC-FCSEL ( n n n n -- n )
   ?COND ARM-IMM !
   DR3 ARM-R3!
   $1E600C00 ARM-RD @ or  ARM-RN @ 5 lshift or
   ARM-RM @ 16 lshift or  ARM-IMM @ 12 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) DR2 XREG? $9E620000 RR ;

: ENC-FCVTZS ( n n -- n ) DR2 XR2ND $9E780000 RR ;

;package
