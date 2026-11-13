\ asm.f — ARM64 instruction encoders, package A64ASM (operands -> u32).
\ Every encoder screens its operands before packing a bit: an operand dies with
\ ASM-EXIT-RC, a branch or ADR distance throws E-A64EMIT-REACH.
require src/lib/error.f

package A64ASM

$FFFFFFFF constant ARM64-W32

: MSK ( n -- n ) ARM64-W32 and ;

\ An operand refusal here, and every refusal in src/arch/arm64/icode.f, exits
\ with this status.
72 constant ASM-EXIT-RC

public

1  5 lshift constant REG-LIM
1 16 lshift constant IMM16-LIM   \ move-wide immediate, SVC number
1  8 lshift constant IMM8-LIM    \ floating move immediate
1 12 lshift constant IMM12-LIM   \ add/sub/compare immediate, load/store offset field
1  4 lshift constant COND-LIM
1  2 lshift constant HW-LIM      \ move-wide shifted-half selector

8 constant PAIR-SCALE
-512 constant PAIR-MIN
504 constant PAIR-MAX

1 9 lshift 1 - constant SIMM9-MASK
12 constant SIMM9-SHIFT

18 constant ARM-X18

 0 constant C-EQ   1 constant C-NE   2 constant C-CS   3 constant C-CC
 4 constant C-MI   5 constant C-PL   6 constant C-VS   7 constant C-VC
 8 constant C-HI   9 constant C-LS  10 constant C-GE  11 constant C-LT
12 constant C-GT  13 constant C-LE  14 constant C-AL

33554432 constant REL26-HI  \ 2^25 instructions, exclusive: b/bl 26-bit field
  262144 constant REL19-HI  \ 2^18 instructions, exclusive: b.cond/cbz/cbnz 19-bit field
 1048576 constant ADR-HI    \ 2^20 bytes or pages, exclusive: adr/adrp 21-bit field

private

-33554432 constant REL26-LO  \ -2^25 instructions, inclusive
  -262144 constant REL19-LO  \ -2^18 instructions, inclusive
 -1048576 constant ADR-LO    \ -2^20 bytes or pages, inclusive

: REL26-OK? ( n -- bool )  dup REL26-LO >=  swap REL26-HI <  and ;
: REL19-OK? ( n -- bool )  dup REL19-LO >=  swap REL19-HI <  and ;
: ADR-OK?   ( n -- bool )  dup ADR-LO   >=  swap ADR-HI   <  and ;

: ?REACH ( bool -- )  0= IF E-A64EMIT-REACH throw THEN ;

\ A branch's byte distance as the instruction count its field holds.
: >INSNS ( n -- n )  dup 3 and 0= ?REACH  4 / ;
: >REL26 ( n -- n )  >INSNS dup REL26-OK? ?REACH ;
: >REL19 ( n -- n )  >INSNS dup REL19-OK? ?REACH ;

1 13 lshift constant NIS-LIM     \ packed N:immr:imms of a logical immediate
1  6 lshift constant SHIFT-LIM
1  3 lshift constant LANEH-LIM

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

\ Signed nine-bit field, -256 .. 255, counted in bytes, not access widths.
1 8 lshift constant SIMM9-LIM

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

\ UMOV's 16-bit lane index: three bits, since the two bits below it are opcode.
: ?LANEH ( n -- n )
   dup LANEH-LIM OUT? IF s" asm: vector lane index out of range" ASM-EXIT-RC die THEN ;

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

\ No D register is platform-reserved: only the field bound applies.
: DR2 ( n n -- n n )  ?REG swap ?REG swap ;

: DR3 ( n n n -- n n n )  ?REG rot ?REG rot ?REG rot ;

\ The transfer is a D register, the base an X register.
: DRXN ( n n n -- n n n )  rot ?REG rot XREG? rot ;

: XMW3 ( n n n -- n n n )  ?HW rot XREG? rot ?IMM16 rot ;

: RRR ( n n n n -- n )
   {: rd:n rn:n rm:n op:n :}
   op rd or  rn 5 lshift or  rm 16 lshift or MSK ;

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

: PAIR-WORD ( n n n n n -- n )
   {: rt:n rt2:n rn:n off:n op:n :}
   rt XREG?  rt2 XREG? 10 lshift or  rn XREG? 5 lshift or
   off PAIR-SCALE SCALE/ ?SIMM7 $7F and 15 lshift or  op or MSK ;

public

\ Operands: rd imm16 hw.
: MOVZHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $D2800000 or MSK ;

: MOVKHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $F2800000 or MSK ;

: MOVNHW ( n n n -- n )  XMW3 21 lshift swap 5 lshift or or $92800000 or MSK ;

: ENC-ADD ( n n n -- n ) XR3 $8B000000 RRR ;

: ENC-SUB ( n n n -- n ) XR3 $CB000000 RRR ;

\ add xd, xn, xm, lsl #sh: the shift type, bits 23:22, stays 00 (LSL).
: ENC-ADD-LSL ( n n n n -- n )
   ?SHIFT {: sh:n :} XR3 sh 10 lshift $8B000000 or RRR ;

: ENC-AND ( n n n -- n ) XR3 $8A000000 RRR ;

: ENC-ORR ( n n n -- n ) XR3 $AA000000 RRR ;

: ENC-EOR ( n n n -- n ) XR3 $CA000000 RRR ;

\ ORN: bit 21 is the shifted-register N bit, which inverts the second source.
: ENC-ORN ( n n n -- n ) XR3 $AA200000 RRR ;

: ENC-MOV ( n n -- n ) ARM-ZERO-REG swap ENC-ORR ;

: ENC-NEG ( n n -- n ) ARM-ZERO-REG swap ENC-SUB ;

: ENC-MVN ( n n -- n ) ARM-ZERO-REG swap ENC-ORN ;

\ MUL is MADD with the addend field set to register 31 ($7C00).
: ENC-MUL ( n n n -- n ) XR3 $9B007C00 RRR ;

: ENC-SMULH ( n n n -- n ) XR3 $9B407C00 RRR ;

: ENC-MADD ( n n n n -- n ) XR4 $9B000000 RRRA ;

: ENC-MSUB ( n n n n -- n ) XR4 $9B008000 RRRA ;

: ENC-ADDI ( n n n -- n ) XRDI ?IMM12 $91000000 RRI ;

: ENC-SUBI ( n n n -- n ) XRDI ?IMM12 $D1000000 RRI ;

\ imm12 counts 4 KiB pages: add xd, xn, #imm12, lsl #12.
: ENC-ADDI-LSL12 ( n n n -- n ) XRDI ?IMM12 $91400000 RRI ;

: ENC-SUBI-LSL12 ( n n n -- n ) XRDI ?IMM12 $D1400000 RRI ;

\ LSL and LSR immediate are UBFM aliases.
: ENC-LSLI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $D3400000 rd or  rn 5 lshift or
   $40 sh - $3F and 16 lshift or  $3F sh - 10 lshift or MSK ;

: ENC-LSRI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $D340FC00 rd or  rn 5 lshift or  sh 16 lshift or MSK ;

\ CMP is SUBS into xzr.
: ENC-CMP ( n n -- n )
   XR2 {: rn:n rm:n :}
   $EB00001F rn 5 lshift or  rm 16 lshift or MSK ;

: ENC-CMPI ( n n -- n )
   XR2ND ?IMM12 {: rn:n imm:n :}
   $F100001F rn 5 lshift or  imm 10 lshift or MSK ;

\ CMN is ADDS into xzr: the flags of rn + imm12.
: ENC-CMNI ( n n -- n )
   XR2ND ?IMM12 {: rn:n imm:n :}
   $B100001F rn 5 lshift or  imm 10 lshift or MSK ;

: ENC-SVC ( n -- n ) ?IMM16 $D4000001 swap 5 lshift or MSK ;

: ENC-RET ( -- n )  $D65F03C0 ;

\ Unsigned offsets are bytes, scaled to the field: x by 8, w by 4, bytes by 1.
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

: ENC-LDUR ( n n n -- n ) XRDI ?SIMM9 $F8400000 RRSI ;

: ENC-STUR ( n n n -- n ) XRDI ?SIMM9 $F8000000 RRSI ;

\ Bits 11:10 of the unscaled encoding name the mode: 00 plain offset (LDUR/STUR),
\ 01 post-index (transfer at the base, then base += imm), 11 pre-index (base +=
\ imm, then transfer there). The offset is the byte count the base moves.
: ENC-LDRPOST ( n n n -- n ) XRDI ?SIMM9 $F8400400 RRSI ;

: ENC-STRPOST ( n n n -- n ) XRDI ?SIMM9 $F8000400 RRSI ;

: ENC-LDRPRE ( n n n -- n ) XRDI ?SIMM9 $F8400C00 RRSI ;

: ENC-STRPRE ( n n n -- n ) XRDI ?SIMM9 $F8000C00 RRSI ;

: ENC-LDRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9400000 RRI ;

: ENC-STRW ( n n n -- n ) XRDI 4 SCALE/ ?IMM12 $B9000000 RRI ;

\ The D-file forms differ in bit 26 (V), the SIMD&FP file. DDI 0487 C6.2.
: ENC-LDRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD400000 RRI ;

: ENC-STRD ( n n n -- n ) DRXN 8 SCALE/ ?IMM12 $FD000000 RRI ;

: ENC-LDURD ( n n n -- n ) DRXN ?SIMM9 $FC400000 RRSI ;

: ENC-STURD ( n n n -- n ) DRXN ?SIMM9 $FC000000 RRSI ;

\ A float push is a post-index store and a float pop a pre-index load.
: ENC-LDRDPRE ( n n n -- n ) DRXN ?SIMM9 $FC400C00 RRSI ;

: ENC-STRDPOST ( n n n -- n ) DRXN ?SIMM9 $FC000400 RRSI ;

: ENC-LDAR ( n n -- n ) XR2 5 lshift or $C8DFFC00 or MSK ;

: ENC-STLR ( n n -- n ) XR2 5 lshift or $C89FFC00 or MSK ;

\ A branch distance is in bytes from the branch; one that is not a whole
\ instruction or that its field cannot hold throws E-A64EMIT-REACH.
: ENC-B ( n -- n ) >REL26 $3FFFFFF and $14000000 or MSK ;

: ENC-BL ( n -- n ) >REL26 $3FFFFFF and $94000000 or MSK ;


: ENC-BCOND ( n n -- n )
   {: bytes:n cond:n :}
   bytes >REL19 $7FFFF and 5 lshift $54000000 or  cond ?COND or MSK ;


: ENC-CBZ ( n n -- n )
   XR2ND {: rd:n bytes:n :}
   bytes >REL19 $7FFFF and 5 lshift $B4000000 or rd or MSK ;


: ENC-CBNZ ( n n -- n )
   XR2ND {: rd:n bytes:n :}
   bytes >REL19 $7FFFF and 5 lshift $B5000000 or rd or MSK ;


: ENC-FMOVXD ( n n -- n ) DR2 XREG? $9E670000 RR ;   \ X->D bits

: ENC-FMOVDX ( n n -- n ) DR2 XR2ND $9E660000 RR ;   \ D->X bits

: ENC-FMOVDI ( n n -- n )
   {: rd:n imm:n :}
   $1E601000 rd ?REG or imm ?IMM8 13 lshift or MSK ;

: ENC-FADD ( n n n -- n ) DR3 $1E602800 RRR ;

: ENC-FSUB ( n n n -- n ) DR3 $1E603800 RRR ;

: ENC-FMUL ( n n n -- n ) DR3 $1E600800 RRR ;

\ The V registers are the D registers. UADDLV sums 16 bytes into 16 bits and
\ cannot overflow.
: ENC-LD1V ( n n -- n ) DR2 XREG? $4C407000 RR ;

: ENC-UADDLV ( n n -- n ) DR2 $6E303800 RR ;

\ A post-indexed load advances its base by the bytes it moved: 16 per register.
: ENC-LD1V-POST ( n n -- n ) DR2 XREG? $4CDF7000 RR ;

: ENC-LD1V4-POST ( n n -- n ) DR2 XREG? $4CDF2000 RR ;   \ vt..vt+3, mod 32

\ LD2 and ST2 move 32 bytes, .4S lanes alternating between vt and vt+1.
: ENC-LD2V ( n n -- n ) DR2 XREG? $4C408800 RR ;

: ENC-ST2V ( n n -- n ) DR2 XREG? $4C008800 RR ;

: ENC-REV32V ( n n -- n ) DR2 $6E200800 RR ;   \ the bytes of each 32-bit lane

: ENC-ADD4S ( n n n -- n ) DR3 $4EA08400 RRR ;

: ENC-MOVV ( n n -- n ) DR2 dup $4EA01C00 RRR ;   \ orr vd.16b, vn.16b, vn.16b

\ FEAT_SHA256: H and H2 take the state halves in qd and qn and four schedule
\ words plus their constants in vm; SU0 and SU1 extend the schedule.
: ENC-SHA256H ( n n n -- n ) DR3 $5E004000 RRR ;

: ENC-SHA256H2 ( n n n -- n ) DR3 $5E005000 RRR ;

: ENC-SHA256SU0 ( n n -- n ) DR2 $5E282800 RR ;

: ENC-SHA256SU1 ( n n n -- n ) DR3 $5E006000 RRR ;

: ENC-UMOVH ( n n n -- n )
   ?LANEH {: lane:n :}
   DR2 XR2ND {: rd:n rn:n :}
   $0E023C00 rd or  rn 5 lshift or  lane 18 lshift or MSK ;

\ cset is csinc rd, xzr, xzr, invert(cond).
: ENC-CSET ( n n -- n )
   XR2ND ?COND {: rd:n cond:n :}
   $9A9F07E0 rd or  cond 1 xor 12 lshift or MSK ;

\ csetm is csinv rd, xzr, xzr, invert(cond): a 0/-1 cell. DDI 0596 excludes AL
\ and NV from this alias.
: ENC-CSETM ( n n -- n )
   XR2ND
   dup C-AL OUT? IF s" asm: mask condition out of range" ASM-EXIT-RC die THEN
   {: rd:n cond:n :}
   $DA9F03E0 rd or  cond 1 xor 12 lshift or MSK ;

\ The condition reads whatever last wrote the flags, Fcmp's NaN rule included.
: ENC-CSEL ( n n n n -- n )
   ?COND {: cond:n :}
   XR3 $9A800000 RRR  cond 12 lshift or MSK ;

: ENC-LSLV ( n n n -- n ) XR3 $9AC02000 RRR ;

: ENC-LSRV ( n n n -- n ) XR3 $9AC02400 RRR ;

: ENC-ASRV ( n n n -- n ) XR3 $9AC02800 RRR ;

: ENC-SDIV ( n n n -- n ) XR3 $9AC00C00 RRR ;

: ENC-UDIV ( n n n -- n ) XR3 $9AC00800 RRR ;

\ ASR immediate is an SBFM alias.
: ENC-ASRI ( n n n -- n )
   XRDI ?SHIFT {: rd:n rn:n sh:n :}
   $9340FC00 rd or  rn 5 lshift or  sh 16 lshift or MSK ;

\ >LIMM builds the nis inside the 13-bit field, so ?NIS fires only for a caller
\ that packs its own.
: ENC-ANDI ( n n n -- n ) XRDI ?NIS $92000000 RRI ;

: ENC-ORRI ( n n n -- n ) XRDI ?NIS $B2000000 RRI ;

: ENC-EORI ( n n n -- n ) XRDI ?NIS $D2000000 RRI ;

: >LIMM ( n -- n )
   LIMM-TRY 0= IF LIMM-BAD THEN ;

: LIMM? ( n -- bool )
   LIMM-TRY swap drop ;

: ENC-BLR ( n -- n ) XREG? $D63F0000 swap 5 lshift or MSK ;

: ENC-BR ( n -- n ) XREG? $D61F0000 swap 5 lshift or MSK ;

: ENC-BRK ( -- n ) $D4200000 ;

: ENC-NOP ( -- n ) $D503201F ;

\ Cache maintenance to the point of unification, for code coherency.
: ENC-ICIVAU ( n -- n ) XREG? $D50B7520 or MSK ;

: ENC-DCCVAU ( n -- n ) XREG? $D50B7B20 or MSK ;

: ENC-DSB-ISH ( -- n ) $D5033B9F ;

: ENC-DMB-ISH ( -- n ) $D5033BBF ;

: ENC-ISB ( -- n ) $D5033FDF ;

\ YIELD, HINT #1 (Arm ARM C6.2, YIELD): a spin-wait hint, no effect on state.
: ENC-YIELD ( -- n ) $D503203F ;

\ CASAL Xs, Xt, [Xn] (Arm ARM C6.2, CAS, CASA, CASAL, CASL): Xs holds the
\ expected value and receives the old one.
: ENC-CASAL ( n n n -- n )
   XR3 {: s:n t:n n:n :}
   s 16 lshift  n 5 lshift or  t or  $C8E0FC00 or MSK ;

\ LDADDAL Xs, Xt, [Xn] (Arm ARM C6.2, LDADD, LDADDA, LDADDAL, LDADDL): [Xn]
\ += Xs, Xt receives the old value.
: ENC-LDADDAL ( n n n -- n )
   XR3 {: s:n t:n n:n :}
   s 16 lshift  n 5 lshift or  t or  $F8E00000 or MSK ;

private

\ d fills the signed 21-bit immhi:immlo field: bytes for adr, pages for adrp.
: ENC-ADRD ( n -- n ) dup 3 and 29 lshift  swap 2 rshift $7FFFF and 5 lshift or ;

public


\ ENC-ADR and ENC-ADRP throw E-A64EMIT-REACH for a distance ADR-OK? refuses.
: ENC-ADR ( n n -- n )
   XR2ND {: rd:n imm:n :}
   imm ADR-OK? ?REACH
   $10000000 rd or  imm ENC-ADRD or MSK ;


\ pages is the signed 4 KiB page distance from the instruction's own page.
: ENC-ADRP ( n n -- n )
   XR2ND {: rd:n pages:n :}
   pages ADR-OK? ?REACH
   $90000000 rd or  pages ENC-ADRD or MSK ;


: ENC-FMOVDD ( n n -- n ) DR2 $1E604000 RR ;

: ENC-FDIV ( n n n -- n ) DR3 $1E601800 RRR ;

: ENC-FNEG ( n n -- n )  DR2 $1E614000 RR ;

: ENC-FABS ( n n -- n )  DR2 $1E60C000 RR ;

: ENC-FSQRT ( n n -- n )  DR2 $1E61C000 RR ;

: ENC-FCMP ( n n -- n )
   DR2 {: rn:n rm:n :}
   $1E602000 rn 5 lshift or  rm 16 lshift or MSK ;

: ENC-FCMP0 ( n -- n ) ?REG $1E602008 swap 5 lshift or MSK ;

: ENC-FCSEL ( n n n n -- n )
   ?COND {: cond:n :}
   DR3 $1E600C00 RRR  cond 12 lshift or MSK ;

: ENC-SCVTF ( n n -- n ) DR2 XREG? $9E620000 RR ;

: ENC-FCVTZS ( n n -- n ) DR2 XR2ND $9E780000 RR ;

;package
