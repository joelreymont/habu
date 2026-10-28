\ asm.f — ARM64 instruction encoders, package A64ASM (operands -> u32).
\ Hex constants preserve the bit layout the native engine builder uses.
\
\ Every encoder screens its operand BEFORE packing a bit, so a bad operand dies
\ instead of overflowing the neighbouring field; branch and ADR displacements
\ are the exception, bounded by src/arch/arm64/icode.f (?REL26/?REL19/?ADR).
\ Runtime image readers reach RRR, RRI, ENC-CMP, ENC-BCOND and ENC-ADR through
\ tools/aot-startup-shape.f. Their operands stay local: a stripped image cannot
\ read this file's baked scratch.

package A64ASM

$FFFFFFFF constant ARM64-W32

: MSK ( n -- n ) ARM64-W32 and ;

\ Every refusal in this file exits with this status, and so does the label and
\ branch layer in src/arch/arm64/icode.f.
72 constant ASM-EXIT-RC

public

1  5 lshift constant REG-LIM     \ Rd/Rn/Rm/Rt register number
1 16 lshift constant IMM16-LIM   \ move-wide immediate, SVC number
1  8 lshift constant IMM8-LIM    \ floating move immediate
1 12 lshift constant IMM12-LIM   \ add/sub/compare immediate, load/store offset field
1  4 lshift constant COND-LIM    \ condition code
1  2 lshift constant HW-LIM      \ move-wide shifted-half selector

8 constant PAIR-SCALE
-512 constant PAIR-MIN
504 constant PAIR-MAX

\ Nine bits at bit twelve, so a negative offset is its two's complement there.
1 9 lshift 1 - constant SIMM9-MASK
12 constant SIMM9-SHIFT

\ x18 is platform-reserved on Darwin and ordinary on Linux.
18 constant ARM-X18

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

: ?IMM8 ( n -- n )
   dup IMM8-LIM OUT? IF s" asm: 8-bit immediate out of range" ASM-EXIT-RC die THEN ;

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

: ?SIMM7 ( n -- n )
   dup dup -64 < swap 64 >= or
   IF s" asm: 7-bit signed offset out of range" ASM-EXIT-RC die THEN ;

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
   dup ARM-X18 <> IF exit THEN
   HB-TARGET-LINUX? IF exit THEN
   HB-TARGET-MACOS? IF s" asm: x18 is Darwin-reserved" ASM-EXIT-RC die THEN
   s" asm: unknown target" ASM-EXIT-RC die ;

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

\ shifted-register 3-operand: rd rn rm
: RRR ( n n n n -- n )
   {: rd:n rn:n rm:n op:n :}
   op rd or  rn 5 lshift or  rm 16 lshift or MSK ;

\ rd rn rm ra; the addend's own five-bit field sits at bit ten.
: RRRA ( n n n n n -- n )
   {: rd:n rn:n rm:n ra:n op:n :}
   op rd or  rn 5 lshift or
   rm 16 lshift or  ra 10 lshift or MSK ;

: RRI ( n n n n -- n )
   {: rd:n rn:n imm:n op:n :}
   op rd or  rn 5 lshift or  imm 10 lshift or MSK ;

: RR ( n n n -- n )
   {: rd:n rn:n op:n :}
   op rd or  rn 5 lshift or MSK ;

\ Unscaled SIGNED byte offset: the only spelling for an access below the base.
: RRSI ( n n n n -- n )
   {: rd:n rn:n imm:n op:n :}
   op rd or  rn 5 lshift or
   imm SIMM9-MASK and SIMM9-SHIFT lshift or MSK ;

\ encodeBitMasks: mask -> nis. Encodable = a power-of-2-sized repeating element
\ (2..64 bits) that is a rotated run of ones; 0 and all-ones are not.
: POPC64 ( n -- n )
   0 swap
   BEGIN dup 0 <> WHILE
      dup 1 and rot + swap 1 rshift
   REPEAT drop ;

: EMASK ( n -- n )   \ low-e-bits mask; lshift wraps at 64
   dup $40 = IF drop -1 ELSE 1 swap lshift 1 - THEN ;

: RORE ( n n n -- n )
   {: x:n r:n e:n :}
   e EMASK {: mask:n :}
   x mask and dup r rshift
   swap e r - lshift or mask and ;

: HALVES= ( n n -- bool )
   {: x:n e:n :}
   1 e 2 / lshift 1 -  dup x and
   swap x e 2 / rshift and = ;

: LELEM ( n -- n n )
   $40
   BEGIN 2dup HALVES= over 2 > and WHILE 2 / REPEAT
   tuck EMASK and swap ;

: LROT ( n n n -- n )
   {: elem:n ones:n e:n :}
   e 0 max 0 ?do
      1 ones lshift 1 - i e RORE elem = IF
         i UNLOOP EXIT
      THEN
   loop
   -1 ;

: LIMM-PACK ( n n n -- n )
   {: rot:n ones:n e:n :}
   e $40 = IF $1000 ELSE 0 THEN
   rot 6 lshift or
   e 2 * 1 - $3F and $3F xor  ones 1 - or or ;

: LIMM-BAD ( -- )  s" asm: bad logical immediate" ASM-EXIT-RC die ;

\ This file is loaded as bare source by the encoder gate, without src/lib/prelude.f,
\ so `true` and `false` are spelled here as the comparisons the prelude uses.
: LIMM-YES ( -- bool ) 0 0= ;
: LIMM-NO  ( -- bool ) 0 0= 0= ;

\ Answers rather than dying, because >LIMM must die and LIMM? must choose. The
\ nis is 0 when false, so ignoring the flag encodes `and rd, rn, #1`.
: LIMM-TRY ( n -- n bool )
   {: mask:n :}
   mask 0 =  mask -1 =  or IF 0 LIMM-NO exit THEN
   mask LELEM {: elem:n e:n :}
   elem POPC64 {: ones:n :}
   ones e = IF 0 LIMM-NO exit THEN
   elem ones e LROT
   dup 0 < IF drop 0 LIMM-NO exit THEN
   ones e LIMM-PACK LIMM-YES ;

public

: SCALE/ ( n n -- n )
   2dup mod 0 <> IF s" asm: operand is not a multiple of its scale" ASM-EXIT-RC die THEN
   / ;

private

\ Plain GPR64 pair: Rt, Rt2, Rn, signed byte offset, opcode. No writeback.
: PAIR-WORD ( n n n n n -- n )
   {: rt:n rt2:n rn:n off:n op:n :}
   rt XREG?  rt2 XREG? 10 lshift or  rn XREG? 5 lshift or
   off PAIR-SCALE SCALE/ ?SIMM7 $7F and 15 lshift or  op or MSK ;

public

\ move-wide: rd imm16 hw -> u32
: MOVZHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $D2800000 or MSK ;

: MOVKHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $F2800000 or MSK ;

: MOVNHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $92800000 or MSK ;

: ENC-ADD ( n n n -- n ) XR3 $8B000000 RRR ;

: ENC-SUB ( n n n -- n ) XR3 $CB000000 RRR ;

\ The same add with the second source shifted left: `add xd, xn, xm, lsl #sh`.
\ The shift type field (bits 23:22) stays 00 = LSL; the amount sits at bit ten.
: ENC-ADD-LSL ( n n n n -- n )
   ?SHIFT {: sh:n :} XR3 sh 10 lshift $8B000000 or RRR ;

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

\ The same forms with the shift bit set: imm12 counts 4 KiB pages,
\ `add xd, xn, #imm12, lsl #12`.
: ENC-ADDI-LSL12 ( n n n -- n ) XRDI ?IMM12 $91400000 RRI ;

: ENC-SUBI-LSL12 ( n n n -- n ) XRDI ?IMM12 $D1400000 RRI ;

\ logical-shift-left immediate (LSL #sh via UBFM): rd rn sh
: ENC-LSLI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $D3400000 rd or  rn 5 lshift or
   $40 sh - $3F and 16 lshift or  $3F sh - 10 lshift or MSK ;

\ logical-shift-right immediate: rd rn sh
: ENC-LSRI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $D340FC00 rd or  rn 5 lshift or  sh 16 lshift or MSK ;

\ compare (shifted reg) rn rm  -> subs xzr
: ENC-CMP ( n n -- n )
   XR2 {: rn:n rm:n :}
   $EB00001F rn 5 lshift or  rm 16 lshift or MSK ;

\ compare immediate rn imm12
: ENC-CMPI ( n n -- n )
   XR2ND ?IMM12 {: rn:n imm:n :}
   $F100001F rn 5 lshift or  imm 10 lshift or MSK ;

\ compare negative immediate rn imm12 -> adds xzr: the flags of rn + imm12.
: ENC-CMNI ( n n -- n )
   XR2ND ?IMM12 {: rn:n imm:n :}
   $B100001F rn 5 lshift or  imm 10 lshift or MSK ;

\ svc #imm  ; ret
: ENC-SVC ( n -- n ) ?IMM16 $D4000001 swap 5 lshift or MSK ;

: ENC-RET ( -- n )  $D65F03C0 ;

\ load/store, unsigned offset. habu scales: x-regs by 8 (?SC8), w by 4, byte by 1.
: ENC-LDR ( n n n -- n ) XRDI 8 SCALE/ ?IMM12 $F9400000 RRI ;

: ENC-STR ( n n n -- n ) XRDI 8 SCALE/ ?IMM12 $F9000000 RRI ;

: ENC-LDP ( n n n n -- n )
   {: rt:n rt2:n rn:n off:n :}
   rt rt2 = IF s" asm: load pair repeats a destination" ASM-EXIT-RC die THEN
   rt rt2 rn off $A9400000 PAIR-WORD ;

: ENC-LDPPRE ( n n n n -- n )
   {: rt:n rt2:n rn:n off:n :}
   rt rt2 = IF s" asm: load pair repeats a destination" ASM-EXIT-RC die THEN
   \ Register 31 is SP as a base and XZR as a destination.
   rn 31 <> rt rn = rt2 rn = or and IF
      s" asm: pair writeback overlaps a destination" ASM-EXIT-RC die
   THEN
   rt rt2 rn off $A9C00000 PAIR-WORD ;

: ENC-STP ( n n n n -- n ) $A9000000 PAIR-WORD ;

\ Transfer at the old base, then apply the scaled signed byte writeback.
: ENC-STPPOST ( n n n n -- n )
   {: rt:n rt2:n rn:n off:n :}
   rt rn = rt2 rn = or IF
      s" asm: pair writeback overlaps a source" ASM-EXIT-RC die
   THEN
   rt rt2 rn off $A8800000 PAIR-WORD ;

: ENC-LDRB ( n n n -- n ) XRDI ?IMM12 $39400000 RRI ;

: ENC-STRB ( n n n -- n ) XRDI ?IMM12 $39000000 RRI ;

\ The two unscaled accesses, in the signed addressing mode RRSI packs.
: ENC-LDUR ( n n n -- n ) XRDI ?SIMM9 $F8400000 RRSI ;

: ENC-STUR ( n n n -- n ) XRDI ?SIMM9 $F8000000 RRSI ;

\ The same signed field again, with the base register written back, which is one
\ instruction for the pointer move a stack push, a stack pop and a link-register
\ frame each used two for. Bits 11:10 of the unscaled encoding above name the
\ mode: 00 is the plain offset LDUR/STUR pack, 01 is post-index (transfer at the
\ base, THEN base += imm) and 11 is pre-index (base += imm, then transfer
\ there). The offset is a byte count in both, so it is the amount the base
\ moves, not a scaled slot number.
: ENC-LDRPOST ( n n n -- n ) XRDI ?SIMM9 $F8400400 RRSI ;

: ENC-STRPOST ( n n n -- n ) XRDI ?SIMM9 $F8000400 RRSI ;

: ENC-LDRPRE ( n n n -- n ) XRDI ?SIMM9 $F8400C00 RRSI ;

: ENC-STRPRE ( n n n -- n ) XRDI ?SIMM9 $F8000C00 RRSI ;

: ENC-LDRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9400000 RRI ;

: ENC-STRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9000000 RRI ;

\ The same four accesses for the D file. One opcode bit differs, bit 26 (V),
\ which says the transferred register is of the SIMD&FP file. DDI 0487 C6.2.
: ENC-LDRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD400000 RRI ;

: ENC-STRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD000000 RRI ;

: ENC-LDURD ( n n n -- n ) DRXN ?SIMM9 $FC400000 RRSI ;

: ENC-STURD ( n n n -- n ) DRXN ?SIMM9 $FC000000 RRSI ;

\ The writeback modes the D file needs: a float push is a post-index store and a
\ float pop is a pre-index load, the same two bits in the same two positions.
: ENC-LDRDPRE ( n n n -- n ) DRXN ?SIMM9 $FC400C00 RRSI ;

: ENC-STRDPOST ( n n n -- n ) DRXN ?SIMM9 $FC000400 RRSI ;

: ENC-LDAR ( n n -- n ) XR2 5 lshift or $C8DFFC00 or MSK ;

: ENC-STLR ( n n -- n ) XR2 5 lshift or $C89FFC00 or MSK ;

\ branches: delta is in WORDS (instruction-relative), sign-handled by the caller's mask.
: ENC-B ( n -- n ) $3FFFFFF and $14000000 or MSK ;

: ENC-BL ( n -- n ) $3FFFFFF and $94000000 or MSK ;

: ENC-BCOND ( n n -- n )
   {: delta:n cond:n :}
   delta $7FFFF and 5 lshift $54000000 or  cond ?COND or MSK ;

: ENC-CBZ ( n n -- n )
   XR2ND {: rd:n delta:n :}
   delta $7FFFF and 5 lshift $B4000000 or rd or MSK ;

: ENC-CBNZ ( n n -- n )
   XR2ND {: rd:n delta:n :}
   delta $7FFFF and 5 lshift $B5000000 or rd or MSK ;

\ FP (double): operands in the D-register file. DR2/DR3 bound every D field;
\ XREG?/XR2ND then adds the reserved-register refusal to the one X operand.
: ENC-FMOVXD ( n n -- n ) DR2 XREG? $9E670000 RR ;   \ X->D bits

: ENC-FMOVDX ( n n -- n ) DR2 XR2ND $9E660000 RR ;   \ D->X bits

: ENC-FMOVDI ( n n -- n )
   {: rd:n imm:n :}
   $1E601000 rd ?REG or imm ?IMM8 13 lshift or MSK ;

: ENC-FADD ( n n n -- n ) DR3 $1E602800 RRR ;

: ENC-FSUB ( n n n -- n ) DR3 $1E603800 RRR ;

: ENC-FMUL ( n n n -- n ) DR3 $1E600800 RRR ;

\ The V file IS the D file, so DR2 bounds these too; UMOV's destination is the
\ one general register here. UADDLV sums 16 bytes into 16 bits and cannot overflow.
: ENC-LD1V ( n n -- n ) DR2 XREG? $4C407000 RR ;

: ENC-UADDLV ( n n -- n ) DR2 $6E303800 RR ;

: ENC-UMOVH ( n n n -- n )
   ?LANEH {: lane:n :}
   DR2 XR2ND {: rd:n rn:n :}
   $0E023C00 rd or  rn 5 lshift or  lane 18 lshift or MSK ;

\ conditional set ( rd cond -- w ): cset = csinc rd, xzr, xzr, invert(cond)
: ENC-CSET ( n n -- n )
   XR2ND ?COND {: rd:n cond:n :}
   $9A9F07E0 rd or  cond 1 xor 12 lshift or MSK ;

\ Csetm = csinv rd, xzr, xzr, invert(cond): a full-cell 0/-1 flag.
\ DDI 0596 CSETM excludes AL and NV from this conditional alias.
: ENC-CSETM ( n n -- n )
   XR2ND
   dup C-AL OUT? IF s" asm: mask condition out of range" ASM-EXIT-RC die THEN
   {: rd:n cond:n :}
   $DA9F03E0 rd or  cond 1 xor 12 lshift or MSK ;

\ rd takes rn when the condition holds and rm when it does not, with no branch.
\ The condition reads whatever last wrote the flags, Fcmp's NaN rule included.
: ENC-CSEL ( n n n n -- n )
   ?COND {: cond:n :}
   XR3 $9A800000 RRR  cond 12 lshift or MSK ;

\ data-processing 2-source: shift-by-register, divide
: ENC-LSLV ( n n n -- n ) XR3 $9AC02000 RRR ;

: ENC-LSRV ( n n n -- n ) XR3 $9AC02400 RRR ;

: ENC-ASRV ( n n n -- n ) XR3 $9AC02800 RRR ;

: ENC-SDIV ( n n n -- n ) XR3 $9AC00C00 RRR ;

: ENC-UDIV ( n n n -- n ) XR3 $9AC00800 RRR ;

\ arithmetic shift right immediate (SBFM)
: ENC-ASRI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $9340FC00 rd or  rn 5 lshift or  sh 16 lshift or MSK ;

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
   XR2ND {: rd:n imm:n :}
   $10000000 rd or  imm ENC-ADRD or MSK ;

\ FP (double, D-register file): engine-grade set, golden vs habu in t-sh-fp-enc
: ENC-FMOVDD ( n n -- n ) DR2 $1E604000 RR ;

: ENC-FDIV ( n n n -- n ) DR3 $1E601800 RRR ;

: ENC-FNEG ( n n -- n )  DR2 $1E614000 RR ;

: ENC-FABS ( n n -- n )  DR2 $1E60C000 RR ;

: ENC-FSQRT ( n n -- n )  DR2 $1E61C000 RR ;

: ENC-FCMP ( n n -- n )
   DR2 {: rn:n rm:n :}
   $1E602000 rn 5 lshift or  rm 16 lshift or MSK ;

: ENC-FCMP0 ( n -- n ) ?REG $1E602008 swap 5 lshift or MSK ;

\ Csel on the D file: same four operand positions, ftype says double. No operand
\ reaches XREG? - d18 holds no platform state - and the condition reads the flags.
: ENC-FCSEL ( n n n n -- n )
   ?COND {: cond:n :}
   DR3 $1E600C00 RRR  cond 12 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) DR2 XREG? $9E620000 RR ;

: ENC-FCVTZS ( n n -- n ) DR2 XR2ND $9E780000 RR ;

;package
