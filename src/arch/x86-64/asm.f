split-str
\ asm.f — x86_64 instruction encoders, package X64ASM.
\ Operands are nominal types; every encoder screens an operand before it packs a
\ bit, and a rejection is E-OPERAND. Destination comes first; a memory form takes
\ the data register first and the memory operand second, stores included.
\ An instruction is 1..15 bytes with no value form, so every ENC-* takes the byte
\ sink, a vector<u8> (src/lib/vector.f), last and appends to it.

require src/lib/type/deftype.f
require src/lib/error.f
require src/lib/vector.f

package X64ASM
public

DEFTYPE R64
DEFTYPE R32
DEFTYPE R16
DEFTYPE R8
DEFTYPE XMM
DEFTYPE IMM8
DEFTYPE IMM32
DEFTYPE IMM64
DEFTYPE CONDITION
DEFTYPE REL
DEFTYPE MEM

E-X64ASM-OPERAND constant E-OPERAND

\ mov r64,imm64 (REX.W B8+r io) always carries REX, so its eight immediate bytes
\ start at this offset; the relocation writer patches there. It is the one
\ relocatable literal form (docs/xcomp.md "Machine model").
2 constant MOV-RI64-IMM-OFF

\ `call rel32` is E8 cd and `jmp rel32` E9 cd, with no prefix, so the
\ displacement of either always begins one byte into the instruction: where a
\ writer that links a call or tail branch it did not encode patches it.
1 constant CALL-REL32-OFF

private

\ ---- operand screening -------------------------------------------------------
: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then
   value ;

: REG-NUM ( n -- n )  0 15 RANGE ;

: ?COND ( n -- n )  0 15 RANGE ;

: ?SIMM8 ( n -- n )  -128 127 RANGE ;

\ A shift count. The hardware masks to six bits; a wider count is a caller bug.
: ?SHIFT-COUNT ( n -- n )  0 63 RANGE ;

\ A shuffle order or byte count, which nothing sign-extends.
: ?UIMM8 ( n -- n )  0 255 RANGE ;

\ An imm32 the machine sign-extends to 64 bits (C7 /0, 81 /n, 69 /r, rel32).
: ?SIMM32 ( n -- n )  -2147483648 2147483647 RANGE ;

\ An imm32 the machine zero-extends to 64 bits (B8+rd into a 32-bit register).
: ?UIMM32 ( n -- n )  0 $FFFFFFFF RANGE ;

\ ---- the byte sink -----------------------------------------------------------
\ Encoded length depends only on the word chosen and the memory operand's
\ displacement magnitude: no relaxation. A caller sizes an instruction by
\ encoding it into a scratch vector.
: EMIT-B ( n ^VEC:vector<u8> -- ) {: v:n s:^ :}
   v $FF and s VEC:PUSH c! ;

: EMIT-W ( n ^VEC:vector<u8> -- ) {: v:n s:^ :}
   v s EMIT-B  v 8 rshift s EMIT-B ;

: EMIT-D ( n ^VEC:vector<u8> -- ) {: v:n s:^ :}
   v s EMIT-W  v 16 rshift s EMIT-W ;

: EMIT-Q ( n ^VEC:vector<u8> -- ) {: v:n s:^ :}
   v s EMIT-D  v 32 rshift s EMIT-D ;

\ ---- operation width ---------------------------------------------------------
\ 16-bit operations carry the 66 prefix; 64-bit operations carry REX.W; 32-bit
\ operations carry neither and zero-extend their result into the full register.
0 constant W8   1 constant W16   2 constant W32   3 constant W64

: WIDTH-66? ( n -- bool )  W16 = ;

: WIDTH-W ( n -- n )  W64 = if 1 else 0 then ;

\ An r8 numbered 4..7 is spl, bpl, sil or dil, never ah, ch, dh or bh, so any r8
\ at or above 4 forces a REX prefix; the legacy high-byte registers are not
\ expressible.
: R8-FORCE ( n -- n )  4 >= if 1 else 0 then ;

\ ---- REX, ModRM and SIB ------------------------------------------------------
\ The REX byte is omitted entirely when every one of its fields is zero and no
\ operand forces it, which is what makes a 32-bit or 8-bit form short.
: EMIT-REX ( n n n n n ^VEC:vector<u8> -- ) {: w:n r:n x:n b:n force:n s:^ :}
   w r or x or b or force or 0= if exit then
   $40 w 3 lshift or r 2 lshift or x 1 lshift or b or s EMIT-B ;

: MODRM ( n n n -- n ) {: mod:n reg:n rm:n :}
   mod 6 lshift  reg 7 and 3 lshift or  rm 7 and or ;

: SIB ( n n n -- n ) {: ss:n index:n base:n :}
   ss 6 lshift  index 7 and 3 lshift or  base 7 and or ;

\ An opcode carries its escape bytes above it: xx, 0Fxx, 0F38xx or 0F3Axx.
: EMIT-OP ( n ^VEC:vector<u8> -- ) {: op:n s:^ :}
   op $10000 >= if op 16 rshift s EMIT-B then
   op $100 >= if op 8 rshift s EMIT-B then
   op s EMIT-B ;

\ ---- the packed memory operand -----------------------------------------------
\ A memory operand is one cell so it can be bound to a local like every other
\ operand: disp32 in bits 0..31, base in 32..36, index in 37..41, the scale
\ logarithm in 42..43 and the rip-relative bit in 44. MEM-NONE is the absent
\ register. The public constructors below are the only way to build one.
16 constant MEM-NONE
32 constant MEM-BASE-SHIFT
37 constant MEM-INDEX-SHIFT
42 constant MEM-SCALE-SHIFT
44 constant MEM-RIP-SHIFT

: MEM-FIELD ( n n n -- n ) {: v:n shift:n width:n :}
   v shift rshift  1 width lshift 1 -  and ;

: MEM-BASE ( mem -- n )   MEM>N MEM-BASE-SHIFT 5 MEM-FIELD ;

: MEM-INDEX ( mem -- n )  MEM>N MEM-INDEX-SHIFT 5 MEM-FIELD ;

: MEM-SS ( mem -- n )     MEM>N MEM-SCALE-SHIFT 2 MEM-FIELD ;

: MEM-RIP? ( mem -- bool ) MEM>N MEM-RIP-SHIFT 1 MEM-FIELD 0<> ;

: MEM-DISP ( mem -- n ) {: m:mem :}
   m MEM>N $FFFFFFFF and {: d:n :}
   d $80000000 and 0<> if d $100000000 - exit then
   d ;

: SCALE-LOG ( n -- n ) {: sc:n :}
   sc 1 = if 0 exit then
   sc 2 = if 1 exit then
   sc 4 = if 2 exit then
   sc 8 <> if E-OPERAND throw then
   3 ;

: MEM-PACK ( n n n n n -- mem ) {: base:n index:n ss:n disp:n rip:n :}
   disp $FFFFFFFF and
   base MEM-BASE-SHIFT lshift or
   index MEM-INDEX-SHIFT lshift or
   ss MEM-SCALE-SHIFT lshift or
   rip MEM-RIP-SHIFT lshift or >MEM ;

\ Every memory operand is re-screened on the way into an encoder, so a value
\ forged through the generated >MEM cast cannot reach a ModRM or SIB field.
: MEM-CHECK ( mem -- ) {: m:mem :}
   m MEM-INDEX {: ix:n :}
   ix MEM-NONE <> if
      ix REG-NUM drop
      ix 4 = if E-OPERAND throw then
   then
   m MEM-RIP? if
      m MEM-BASE MEM-NONE <> if E-OPERAND throw then
      ix MEM-NONE <> if E-OPERAND throw then
      exit
   then
   m MEM-BASE REG-NUM drop ;

: MEM-X-BIT ( mem -- n ) {: m:mem :}
   m MEM-INDEX {: ix:n :}
   ix MEM-NONE = if 0 exit then
   ix 3 rshift ;

: MEM-B-BIT ( mem -- n ) {: m:mem :}
   m MEM-BASE {: b:n :}
   b MEM-NONE = if 0 exit then
   b 3 rshift ;

\ mod 00 has no displacement, but rm 101 in mod 00 means rip-relative, so a base
\ of rbp or r13 spells its zero displacement as a disp8 of zero.
: MEM-MOD ( mem -- n ) {: m:mem :}
   m MEM-DISP {: d:n :}
   d 0= m MEM-BASE 7 and 5 <> and if 0 exit then
   d -128 >= d 127 <= and if 1 exit then
   2 ;

\ rm 100 means "a SIB byte follows", so a base of rsp or r12 needs one even with
\ no index at all.
: MEM-SIB? ( mem -- bool ) {: m:mem :}
   m MEM-INDEX MEM-NONE <> if true exit then
   m MEM-BASE 7 and 4 = ;

\ index 100 in a SIB byte means "no index".
: MEM-SIB-INDEX ( mem -- n ) {: m:mem :}
   m MEM-INDEX {: ix:n :}
   ix MEM-NONE = if 4 exit then
   ix ;

: EMIT-DISP ( n mem ^VEC:vector<u8> -- ) {: mod:n m:mem s:^ :}
   mod 1 = if m MEM-DISP s EMIT-B exit then
   mod 2 = if m MEM-DISP s EMIT-D then ;

: EMIT-MEM ( n mem ^VEC:vector<u8> -- ) {: reg:n m:mem s:^ :}
   m MEM-RIP? if
      0 reg 5 MODRM s EMIT-B
      m MEM-DISP s EMIT-D
      exit
   then
   m MEM-MOD {: mod:n :}
   m MEM-SIB? if
      mod reg 4 MODRM s EMIT-B
      m MEM-SS m MEM-SIB-INDEX m MEM-BASE SIB s EMIT-B
   else
      mod reg m MEM-BASE MODRM s EMIT-B
   then
   mod m s EMIT-DISP ;

\ ---- the two instruction shapes ----------------------------------------------
\ A register-to-register form: mod 11, the reg field holding either a register
\ or an opcode extension digit.
: RR-ENC ( n n n n n ^VEC:vector<u8> -- ) {: wd:n op:n reg:n rm:n force:n s:^ :}
   wd WIDTH-66? if $66 s EMIT-B then
   wd WIDTH-W  reg 3 rshift  0  rm 3 rshift  force s EMIT-REX
   op s EMIT-OP
   3 reg rm MODRM s EMIT-B ;

\ A memory form: the reg field as above, the rm field spelled by the ModRM, the
\ optional SIB and the displacement.
: RM-ENC ( n n n mem n ^VEC:vector<u8> -- ) {: wd:n op:n reg:n m:mem force:n s:^ :}
   m MEM-CHECK
   wd WIDTH-66? if $66 s EMIT-B then
   wd WIDTH-W  reg 3 rshift  m MEM-X-BIT  m MEM-B-BIT  force s EMIT-REX
   op s EMIT-OP
   reg m s EMIT-MEM ;

\ ---- the group-1 arithmetic/logic family -------------------------------------
\ One opcode family parameterised by a base: add 00, or 08, adc 10, sbb 18,
\ and 20, sub 28, xor 30, cmp 38. base+1 is the "to r/m" direction, base+3 the
\ "from r/m" direction, and base>>3 is the opcode extension digit the immediate
\ forms 81 and 83 select with.
: ALU-RR ( n r64 r64 ^VEC:vector<u8> -- ) {: base:n dst:r64 src:r64 s:^ :}
   W64 base 1 + src R64>N REG-NUM dst R64>N REG-NUM 0 s RR-ENC ;

: ALU-RM ( n r64 mem ^VEC:vector<u8> -- ) {: base:n dst:r64 m:mem s:^ :}
   W64 base 3 + dst R64>N REG-NUM m 0 s RM-ENC ;

: ALU-MR ( n r64 mem ^VEC:vector<u8> -- ) {: base:n src:r64 m:mem s:^ :}
   W64 base 1 + src R64>N REG-NUM m 0 s RM-ENC ;

\ The immediate is screened BEFORE the first byte reaches the sink, so a rejected
\ instruction leaves no prefix of itself behind. Every immediate form below
\ follows this order for the same reason.
: ALU-RI8 ( n r64 imm8 ^VEC:vector<u8> -- ) {: base:n dst:r64 v:imm8 s:^ :}
   v IMM8>N ?SIMM8 {: imm:n :}
   W64 $83 base 3 rshift dst R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-B ;

: ALU-MI8 ( n mem imm8 ^VEC:vector<u8> -- ) {: base:n m:mem v:imm8 s:^ :}
   v IMM8>N ?SIMM8 {: imm:n :}
   W64 $83 base 3 rshift m 0 s RM-ENC
   imm s EMIT-B ;

: ALU-RI32 ( n r64 imm32 ^VEC:vector<u8> -- ) {: base:n dst:r64 v:imm32 s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $81 base 3 rshift dst R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- shared shapes for the remaining families --------------------------------
: UNARY-F7 ( n r64 ^VEC:vector<u8> -- ) {: digit:n r:r64 s:^ :}
   W64 $F7 digit r R64>N REG-NUM 0 s RR-ENC ;

: UNARY-FF ( n r64 ^VEC:vector<u8> -- ) {: digit:n r:r64 s:^ :}
   W64 $FF digit r R64>N REG-NUM 0 s RR-ENC ;

: SHIFT-RI8 ( n r64 imm8 ^VEC:vector<u8> -- ) {: digit:n r:r64 v:imm8 s:^ :}
   v IMM8>N ?SHIFT-COUNT {: count:n :}
   W64 $C1 digit r R64>N REG-NUM 0 s RR-ENC
   count s EMIT-B ;

: SHIFT-CL ( n r64 ^VEC:vector<u8> -- ) {: digit:n r:r64 s:^ :}
   W64 $D3 digit r R64>N REG-NUM 0 s RR-ENC ;

: EXT-RM ( n n r64 mem ^VEC:vector<u8> -- ) {: wd:n op:n dst:r64 m:mem s:^ :}
   wd op dst R64>N REG-NUM m 0 s RM-ENC ;

\ An indirect jump or call is FF /digit with no REX.W: a near branch target is a
\ full 64-bit register already.
: BRANCH-REG ( n r64 ^VEC:vector<u8> -- ) {: digit:n r:r64 s:^ :}
   W32 $FF digit r R64>N REG-NUM 0 s RR-ENC ;

\ Mandatory SSE prefixes precede REX. Screen both registers before that prefix;
\ REX.W is set only by the signed 64-bit integer conversion forms.
: SSE-RR ( n n n n n ^VEC:vector<u8> -- ) {: prefix:n wd:n op:n reg:n rm:n s:^ :}
   reg REG-NUM {: rn:n :}  rm REG-NUM {: mn:n :}
   prefix s EMIT-B
   wd op rn mn 0 s RR-ENC ;

: SSE-RRI8 ( n n xmm xmm imm8 ^VEC:vector<u8> -- )
   {: prefix:n op:n d:xmm sr:xmm v:imm8 s:^ :}
   v IMM8>N ?UIMM8 {: imm:n :}
   prefix W32 op d XMM>N sr XMM>N s SSE-RR
   imm s EMIT-B ;

: SSE-MEM ( n n xmm mem ^VEC:vector<u8> -- )
   {: prefix:n op:n r:xmm m:mem s:^ :}
   r XMM>N REG-NUM {: rn:n :}  m MEM-CHECK
   prefix s EMIT-B
   0 rn 3 rshift m MEM-X-BIT m MEM-B-BIT 0 s EMIT-REX
   op s EMIT-OP
   rn m s EMIT-MEM ;

\ The SHA instructions take no mandatory prefix.
: SHA-RR ( n xmm xmm ^VEC:vector<u8> -- ) {: op:n d:xmm sr:xmm s:^ :}
   W32 op d XMM>N REG-NUM sr XMM>N REG-NUM 0 s RR-ENC ;

\ A locked 64-bit read-modify-write of memory. The lock prefix precedes REX, as
\ a mandatory SSE prefix does, so both operands are screened before it.
: LOCK-RM ( n r64 mem ^VEC:vector<u8> -- ) {: op:n r:r64 m:mem s:^ :}
   r R64>N REG-NUM {: rn:n :}  m MEM-CHECK
   $F0 s EMIT-B
   W64 op rn m 0 s RM-ENC ;

public

\ ---- the 64-bit register file ------------------------------------------------
\ RAX..R15 are r64; the type tails r64/r32/r16/r8 name the register files. The
\ word R8 is the ninth 64-bit register; the type r8 is the byte-register file.
: RAX ( -- r64 )  0 >R64 ;
: RCX ( -- r64 )  1 >R64 ;
: RDX ( -- r64 )  2 >R64 ;
: RBX ( -- r64 )  3 >R64 ;
: RSP ( -- r64 )  4 >R64 ;
: RBP ( -- r64 )  5 >R64 ;
: RSI ( -- r64 )  6 >R64 ;
: RDI ( -- r64 )  7 >R64 ;
: R8  ( -- r64 )  8 >R64 ;
: R9  ( -- r64 )  9 >R64 ;
: R10 ( -- r64 ) 10 >R64 ;
: R11 ( -- r64 ) 11 >R64 ;
: R12 ( -- r64 ) 12 >R64 ;
: R13 ( -- r64 ) 13 >R64 ;
: R14 ( -- r64 ) 14 >R64 ;
: R15 ( -- r64 ) 15 >R64 ;

\ ---- the SSE register file ---------------------------------------------------
: XMM0  ( -- xmm )  0 >XMM ;
: XMM1  ( -- xmm )  1 >XMM ;
: XMM2  ( -- xmm )  2 >XMM ;
: XMM3  ( -- xmm )  3 >XMM ;
: XMM4  ( -- xmm )  4 >XMM ;
: XMM5  ( -- xmm )  5 >XMM ;
: XMM6  ( -- xmm )  6 >XMM ;
: XMM7  ( -- xmm )  7 >XMM ;
: XMM8  ( -- xmm )  8 >XMM ;
: XMM9  ( -- xmm )  9 >XMM ;
: XMM10 ( -- xmm ) 10 >XMM ;
: XMM11 ( -- xmm ) 11 >XMM ;
: XMM12 ( -- xmm ) 12 >XMM ;
: XMM13 ( -- xmm ) 13 >XMM ;
: XMM14 ( -- xmm ) 14 >XMM ;
: XMM15 ( -- xmm ) 15 >XMM ;

\ ---- condition codes ---------------------------------------------------------
: C-O  ( -- condition )  0 >CONDITION ;
: C-NO ( -- condition )  1 >CONDITION ;
: C-B  ( -- condition )  2 >CONDITION ;
: C-AE ( -- condition )  3 >CONDITION ;
: C-E  ( -- condition )  4 >CONDITION ;
: C-NE ( -- condition )  5 >CONDITION ;
: C-BE ( -- condition )  6 >CONDITION ;
: C-A  ( -- condition )  7 >CONDITION ;
: C-S  ( -- condition )  8 >CONDITION ;
: C-NS ( -- condition )  9 >CONDITION ;
: C-P  ( -- condition ) 10 >CONDITION ;
: C-NP ( -- condition ) 11 >CONDITION ;
: C-L  ( -- condition ) 12 >CONDITION ;
: C-GE ( -- condition ) 13 >CONDITION ;
: C-LE ( -- condition ) 14 >CONDITION ;
: C-G  ( -- condition ) 15 >CONDITION ;

\ ---- memory operands ---------------------------------------------------------
\ The index may not be rsp: that encoding names "no index". The scale is 1, 2, 4
\ or 8.
: MEM-AT ( r64 -- mem ) {: b:r64 :}
   b R64>N REG-NUM MEM-NONE 0 0 0 MEM-PACK ;

: MEM-OFF ( r64 n -- mem ) {: b:r64 d:n :}
   b R64>N REG-NUM MEM-NONE 0 d ?SIMM32 0 MEM-PACK ;

: MEM-IDX ( r64 r64 n n -- mem ) {: b:r64 ix:r64 sc:n d:n :}
   ix R64>N REG-NUM {: ixn:n :}
   ixn 4 = if E-OPERAND throw then
   b R64>N REG-NUM ixn sc SCALE-LOG d ?SIMM32 0 MEM-PACK ;

\ The displacement is from the END of the instruction, so the caller adds the
\ instruction's own length when it resolves a symbol.
: MEM-RIP ( n -- mem ) {: d:n :}
   MEM-NONE MEM-NONE 0 d ?SIMM32 1 MEM-PACK ;

\ ---- group-1: add, or, adc, sbb, and, sub, xor, cmp --------------------------
\ One encoding per operation: no accumulator short forms (05 id, A9 id, 90+r)
\ and no D1 shift-by-one. llvm-mc prefers those, so test/compiler/x86-64-asm.f
\ pins registers other than rax and shift counts other than one.
: ENC-ADD-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $00 d sr s ALU-RR ;
: ENC-ADD-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $00 d m s ALU-RM ;
: ENC-ADD-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $00 sr m s ALU-MR ;
: ENC-ADD-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $00 d v s ALU-RI8 ;
: ENC-ADD-MI8 ( mem imm8 ^VEC:vector<u8> -- ) {: m:mem v:imm8 s:^ :}   $00 m v s ALU-MI8 ;
: ENC-ADD-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $00 d v s ALU-RI32 ;

: ENC-OR-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}   $08 d sr s ALU-RR ;
: ENC-OR-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}    $08 d m s ALU-RM ;
: ENC-OR-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}   $08 sr m s ALU-MR ;
: ENC-OR-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}    $08 d v s ALU-RI8 ;
: ENC-OR-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $08 d v s ALU-RI32 ;

: ENC-ADC-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $10 d sr s ALU-RR ;
: ENC-ADC-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $10 d m s ALU-RM ;
: ENC-ADC-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $10 sr m s ALU-MR ;
: ENC-ADC-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $10 d v s ALU-RI8 ;
: ENC-ADC-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $10 d v s ALU-RI32 ;

: ENC-SBB-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $18 d sr s ALU-RR ;
: ENC-SBB-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $18 d m s ALU-RM ;
: ENC-SBB-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $18 sr m s ALU-MR ;
: ENC-SBB-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $18 d v s ALU-RI8 ;
: ENC-SBB-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $18 d v s ALU-RI32 ;

: ENC-AND-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $20 d sr s ALU-RR ;
: ENC-AND-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $20 d m s ALU-RM ;
: ENC-AND-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $20 sr m s ALU-MR ;
: ENC-AND-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $20 d v s ALU-RI8 ;
: ENC-AND-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $20 d v s ALU-RI32 ;

: ENC-SUB-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $28 d sr s ALU-RR ;
: ENC-SUB-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $28 d m s ALU-RM ;
: ENC-SUB-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $28 sr m s ALU-MR ;
: ENC-SUB-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $28 d v s ALU-RI8 ;
: ENC-SUB-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $28 d v s ALU-RI32 ;

: ENC-XOR-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $30 d sr s ALU-RR ;
: ENC-XOR-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $30 d m s ALU-RM ;
: ENC-XOR-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $30 sr m s ALU-MR ;
: ENC-XOR-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $30 d v s ALU-RI8 ;
: ENC-XOR-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $30 d v s ALU-RI32 ;

: ENC-CMP-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}  $38 d sr s ALU-RR ;
: ENC-CMP-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}   $38 d m s ALU-RM ;
: ENC-CMP-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}  $38 sr m s ALU-MR ;
: ENC-CMP-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 v:imm8 s:^ :}   $38 d v s ALU-RI8 ;
: ENC-CMP-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :} $38 d v s ALU-RI32 ;

\ ---- test --------------------------------------------------------------------
\ test is its own opcode family: 85 /r both directions (it is commutative, so the
\ memory form is the one encoding) and F7 /0 id for the immediate. It has no
\ sign-extended imm8 form.
: ENC-TEST-RR ( r64 r64 ^VEC:vector<u8> -- ) {: l:r64 r:r64 s:^ :}
   W64 $85 r R64>N REG-NUM l R64>N REG-NUM 0 s RR-ENC ;

: ENC-TEST-MR ( r64 mem ^VEC:vector<u8> -- ) {: r:r64 m:mem s:^ :}
   W64 $85 r R64>N REG-NUM m 0 s RM-ENC ;

: ENC-TEST-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: r:r64 v:imm32 s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $F7 0 r R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- moves, loads and stores, all four widths --------------------------------
\ 89 stores the reg field into the rm operand, 8B loads it from there; the 8-bit
\ pair is 88 and 8A. A 32-bit form zero-extends its result into the full 64-bit
\ register, which is why the 32-bit move is the cheap zeroing and truncating one.
: ENC-MOV-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}
   W64 $89 sr R64>N REG-NUM d R64>N REG-NUM 0 s RR-ENC ;

: ENC-MOV-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $8B d R64>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}
   W64 $89 sr R64>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV32-RR ( r32 r32 ^VEC:vector<u8> -- ) {: d:r32 sr:r32 s:^ :}
   W32 $89 sr R32>N REG-NUM d R32>N REG-NUM 0 s RR-ENC ;

: ENC-MOV32-RM ( r32 mem ^VEC:vector<u8> -- ) {: d:r32 m:mem s:^ :}
   W32 $8B d R32>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV32-MR ( r32 mem ^VEC:vector<u8> -- ) {: sr:r32 m:mem s:^ :}
   W32 $89 sr R32>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV16-RR ( r16 r16 ^VEC:vector<u8> -- ) {: d:r16 sr:r16 s:^ :}
   W16 $89 sr R16>N REG-NUM d R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOV16-RM ( r16 mem ^VEC:vector<u8> -- ) {: d:r16 m:mem s:^ :}
   W16 $8B d R16>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV16-MR ( r16 mem ^VEC:vector<u8> -- ) {: sr:r16 m:mem s:^ :}
   W16 $89 sr R16>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV8-RR ( r8 r8 ^VEC:vector<u8> -- ) {: d:r8 sr:r8 s:^ :}
   sr R8>N REG-NUM {: sn:n :}
   d R8>N REG-NUM {: dn:n :}
   W8 $88 sn dn sn R8-FORCE dn R8-FORCE or s RR-ENC ;

: ENC-MOV8-RM ( r8 mem ^VEC:vector<u8> -- ) {: d:r8 m:mem s:^ :}
   d R8>N REG-NUM {: dn:n :}
   W8 $8A dn m dn R8-FORCE s RM-ENC ;

: ENC-MOV8-MR ( r8 mem ^VEC:vector<u8> -- ) {: sr:r8 m:mem s:^ :}
   sr R8>N REG-NUM {: sn:n :}
   W8 $88 sn m sn R8-FORCE s RM-ENC ;

\ C7 /0 id sign-extends its imm32 across the whole 64-bit register; B8+rd id
\ zero-extends, which is the only difference between the two constant forms and
\ the reason their immediates are screened against different ranges.
: ENC-MOV-RI32 ( r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 v:imm32 s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $C7 0 d R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

: ENC-MOV32-RI32 ( r32 imm32 ^VEC:vector<u8> -- ) {: d:r32 v:imm32 s:^ :}
   d R32>N REG-NUM {: n:n :}
   v IMM32>N ?UIMM32 {: imm:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $B8 n 7 and + s EMIT-B
   imm s EMIT-D ;

\ Like the register-to-memory stores, the source precedes the destination.
\ C7's signed imm32 literal writes exactly four bytes here (no REX.W).
\ A RIP displacement is relative to the end, including these immediate bytes.
: ENC-MOV32-MI32 ( imm32 mem ^VEC:vector<u8> -- ) {: v:imm32 m:mem s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W32 $C7 0 m 0 s RM-ENC
   imm s EMIT-D ;

\ REX.W sign-extends the literal to the cell it writes.
: ENC-MOV-MI32 ( imm32 mem ^VEC:vector<u8> -- ) {: v:imm32 m:mem s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $C7 0 m 0 s RM-ENC
   imm s EMIT-D ;

: ENC-MOV-RI64 ( r64 imm64 ^VEC:vector<u8> -- ) {: d:r64 v:imm64 s:^ :}
   d R64>N REG-NUM {: n:n :}
   1 0 0 n 3 rshift 0 s EMIT-REX
   $B8 n 7 and + s EMIT-B
   v IMM64>N s EMIT-Q ;

\ ---- scalar double precision -------------------------------------------------
\ ANDPD and XORPD operate on both 64-bit lanes; UCOMISD compares the low lane.
\ CMPUNORDSD writes the low lane all ones when the two doubles are unordered
\ and zero when not: CMPSD's predicate 3 is its trailing byte.
: ENC-MOVSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F10 d XMM>N sr XMM>N s SSE-RR ;
: ENC-MOVSD-RM ( xmm mem ^VEC:vector<u8> -- ) {: d:xmm m:mem s:^ :}
   $F2 $0F10 d m s SSE-MEM ;
: ENC-MOVSD-MR ( xmm mem ^VEC:vector<u8> -- ) {: sr:xmm m:mem s:^ :}
   $F2 $0F11 sr m s SSE-MEM ;
: ENC-ADDSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F58 d XMM>N sr XMM>N s SSE-RR ;
: ENC-SUBSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F5C d XMM>N sr XMM>N s SSE-RR ;
: ENC-MULSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F59 d XMM>N sr XMM>N s SSE-RR ;
: ENC-DIVSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F5E d XMM>N sr XMM>N s SSE-RR ;
: ENC-SQRTSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0F51 d XMM>N sr XMM>N s SSE-RR ;
: ENC-ANDPD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F54 d XMM>N sr XMM>N s SSE-RR ;
: ENC-XORPD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F57 d XMM>N sr XMM>N s SSE-RR ;
: ENC-UCOMISD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: l:xmm r:xmm s:^ :}
   $66 W32 $0F2E l XMM>N r XMM>N s SSE-RR ;
: ENC-CMPUNORDSD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $F2 W32 $0FC2 d XMM>N sr XMM>N s SSE-RR  3 s EMIT-B ;

\ Hardware conversions only: range/NaN handling for Habu's realint belongs to
\ lowering (src/compiler/native/select-x64.f EMIT-REALINT), not this byte
\ encoder.
: ENC-CVTSI2SD-RR ( xmm r64 ^VEC:vector<u8> -- ) {: d:xmm sr:r64 s:^ :}
   $F2 W64 $0F2A d XMM>N sr R64>N s SSE-RR ;
: ENC-CVTTSD2SI-RR ( r64 xmm ^VEC:vector<u8> -- ) {: d:r64 sr:xmm s:^ :}
   $F2 W64 $0F2C d R64>N sr XMM>N s SSE-RR ;

\ The eight bytes moved between the two files unchanged, a NaN's payload and a
\ signalling NaN included. The XMM register is the ModRM reg field in both
\ directions and the opcode says which way: 66 REX.W 0F 6E /r into it,
\ 66 REX.W 0F 7E /r out of it.
: ENC-MOVQ-XR ( xmm r64 ^VEC:vector<u8> -- ) {: d:xmm sr:r64 s:^ :}
   $66 W64 $0F6E d XMM>N sr R64>N s SSE-RR ;
: ENC-MOVQ-RX ( r64 xmm ^VEC:vector<u8> -- ) {: d:r64 sr:xmm s:^ :}
   $66 W64 $0F7E sr XMM>N d R64>N s SSE-RR ;

\ ---- packed integers and SHA-256 ----------------------------------------------
\ PSHUFB and PALIGNR are SSSE3's, PMOVZXDQ SSE4.1's. SHA256RNDS2 also reads
\ xmm0, which its encoding does not name.
: ENC-MOVDQU-RM ( xmm mem ^VEC:vector<u8> -- ) {: d:xmm m:mem s:^ :}
   $F3 $0F6F d m s SSE-MEM ;
: ENC-MOVDQU-MR ( xmm mem ^VEC:vector<u8> -- ) {: sr:xmm m:mem s:^ :}
   $F3 $0F7F sr m s SSE-MEM ;
: ENC-MOVDQA-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F6F d XMM>N sr XMM>N s SSE-RR ;
: ENC-PADDD-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0FFE d XMM>N sr XMM>N s SSE-RR ;
: ENC-PUNPCKLQDQ-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F6C d XMM>N sr XMM>N s SSE-RR ;
: ENC-PSHUFD-RRI8 ( xmm xmm imm8 ^VEC:vector<u8> -- ) {: d:xmm sr:xmm v:imm8 s:^ :}
   $66 $0F70 d sr v s SSE-RRI8 ;
: ENC-PSHUFB-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F3800 d XMM>N sr XMM>N s SSE-RR ;
: ENC-PALIGNR-RRI8 ( xmm xmm imm8 ^VEC:vector<u8> -- ) {: d:xmm sr:xmm v:imm8 s:^ :}
   $66 $0F3A0F d sr v s SSE-RRI8 ;
: ENC-PMOVZXDQ-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $66 W32 $0F3835 d XMM>N sr XMM>N s SSE-RR ;
: ENC-SHA256RNDS2-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $0F38CB d sr s SHA-RR ;
: ENC-SHA256MSG1-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $0F38CC d sr s SHA-RR ;
: ENC-SHA256MSG2-RR ( xmm xmm ^VEC:vector<u8> -- ) {: d:xmm sr:xmm s:^ :}
   $0F38CD d sr s SHA-RR ;

\ ---- widening moves ----------------------------------------------------------
\ movsxd is the 32-to-64 sign extension and has no zero-extending counterpart:
\ a plain 32-bit move already zero-extends.
: ENC-MOVZX-8-RR ( r64 r8 ^VEC:vector<u8> -- ) {: d:r64 sr:r8 s:^ :}
   sr R8>N REG-NUM {: sn:n :}
   W64 $0FB6 d R64>N REG-NUM sn sn R8-FORCE s RR-ENC ;

: ENC-MOVZX32-8-RR ( r32 r8 ^VEC:vector<u8> -- ) {: d:r32 sr:r8 s:^ :}
   sr R8>N REG-NUM {: sn:n :}
   W32 $0FB6 d R32>N REG-NUM sn sn R8-FORCE s RR-ENC ;

: ENC-MOVZX-8-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $0FB6 d m s EXT-RM ;

: ENC-MOVZX-16-RR ( r64 r16 ^VEC:vector<u8> -- ) {: d:r64 sr:r16 s:^ :}
   W64 $0FB7 d R64>N REG-NUM sr R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOVZX-16-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $0FB7 d m s EXT-RM ;

: ENC-MOVSX-8-RR ( r64 r8 ^VEC:vector<u8> -- ) {: d:r64 sr:r8 s:^ :}
   sr R8>N REG-NUM {: sn:n :}
   W64 $0FBE d R64>N REG-NUM sn sn R8-FORCE s RR-ENC ;

: ENC-MOVSX-8-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $0FBE d m s EXT-RM ;

: ENC-MOVSX-16-RR ( r64 r16 ^VEC:vector<u8> -- ) {: d:r64 sr:r16 s:^ :}
   W64 $0FBF d R64>N REG-NUM sr R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOVSX-16-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $0FBF d m s EXT-RM ;

: ENC-MOVSXD-RR ( r64 r32 ^VEC:vector<u8> -- ) {: d:r64 sr:r32 s:^ :}
   W64 $63 d R64>N REG-NUM sr R32>N REG-NUM 0 s RR-ENC ;

: ENC-MOVSXD-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $63 d m s EXT-RM ;

\ ---- address arithmetic ------------------------------------------------------
: ENC-LEA ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $8D d m s EXT-RM ;

\ ---- the 32-bit zeroing idiom ------------------------------------------------
\ xor of a register with itself is two bytes and clears the whole 64-bit
\ register because a 32-bit result zero-extends.
: ENC-XOR32-RR ( r32 r32 ^VEC:vector<u8> -- ) {: d:r32 sr:r32 s:^ :}
   W32 $31 sr R32>N REG-NUM d R32>N REG-NUM 0 s RR-ENC ;

\ ---- one-register arithmetic -------------------------------------------------
\ mul, imul, div and idiv here are the widening forms: they read rax (and rdx for
\ the divisions) implicitly and write the 128-bit result or the quotient and
\ remainder into rdx:rax. cqo sign-extends rax into rdx:rax to set up idiv.
: ENC-NOT ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   2 r s UNARY-F7 ;
: ENC-NEG ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   3 r s UNARY-F7 ;
: ENC-MUL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   4 r s UNARY-F7 ;
: ENC-IMUL1 ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 5 r s UNARY-F7 ;
: ENC-DIV ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   6 r s UNARY-F7 ;
: ENC-IDIV ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}  7 r s UNARY-F7 ;
: ENC-INC ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   0 r s UNARY-FF ;
: ENC-DEC ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}   1 r s UNARY-FF ;

: ENC-CQO ( ^VEC:vector<u8> -- ) {: s:^ :}
   1 0 0 0 0 s EMIT-REX
   $99 s EMIT-B ;

\ ---- the non-widening signed multiply ----------------------------------------
: ENC-IMUL-RR ( r64 r64 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 s:^ :}
   W64 $0FAF d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC ;

: ENC-IMUL-RM ( r64 mem ^VEC:vector<u8> -- ) {: d:r64 m:mem s:^ :}
   W64 $0FAF d m s EXT-RM ;

: ENC-IMUL-RRI8 ( r64 r64 imm8 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 v:imm8 s:^ :}
   v IMM8>N ?SIMM8 {: imm:n :}
   W64 $6B d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-B ;

: ENC-IMUL-RRI32 ( r64 r64 imm32 ^VEC:vector<u8> -- ) {: d:r64 sr:r64 v:imm32 s:^ :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $69 d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- shifts and rotates ------------------------------------------------------
\ C1 /n ib shifts by an immediate, D3 /n by the count in cl. rol 0, ror 1,
\ shl 4, shr 5, sar 7.
: ENC-ROL-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: r:r64 v:imm8 s:^ :} 0 r v s SHIFT-RI8 ;
: ENC-ROR-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: r:r64 v:imm8 s:^ :} 1 r v s SHIFT-RI8 ;
: ENC-SHL-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: r:r64 v:imm8 s:^ :} 4 r v s SHIFT-RI8 ;
: ENC-SHR-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: r:r64 v:imm8 s:^ :} 5 r v s SHIFT-RI8 ;
: ENC-SAR-RI8 ( r64 imm8 ^VEC:vector<u8> -- ) {: r:r64 v:imm8 s:^ :} 7 r v s SHIFT-RI8 ;

: ENC-ROL-CL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 0 r s SHIFT-CL ;
: ENC-ROR-CL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 1 r s SHIFT-CL ;
: ENC-SHL-CL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 4 r s SHIFT-CL ;
: ENC-SHR-CL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 5 r s SHIFT-CL ;
: ENC-SAR-CL ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 7 r s SHIFT-CL ;

\ ---- control flow ------------------------------------------------------------
\ Every relative displacement is measured in bytes from the END of the branch, and
\ the caller picks the width by picking the word.
: ENC-JMP-REL8 ( rel ^VEC:vector<u8> -- ) {: d:rel s:^ :}
   d REL>N ?SIMM8 {: delta:n :}
   $EB s EMIT-B  delta s EMIT-B ;

: ENC-JMP-REL32 ( rel ^VEC:vector<u8> -- ) {: d:rel s:^ :}
   d REL>N ?SIMM32 {: delta:n :}
   $E9 s EMIT-B  delta s EMIT-D ;

: ENC-CALL-REL32 ( rel ^VEC:vector<u8> -- ) {: d:rel s:^ :}
   d REL>N ?SIMM32 {: delta:n :}
   $E8 s EMIT-B  delta s EMIT-D ;

: ENC-JCC-REL8 ( condition rel ^VEC:vector<u8> -- ) {: c:condition d:rel s:^ :}
   c CONDITION>N ?COND {: cc:n :}
   d REL>N ?SIMM8 {: delta:n :}
   $70 cc + s EMIT-B  delta s EMIT-B ;

: ENC-JCC-REL32 ( condition rel ^VEC:vector<u8> -- ) {: c:condition d:rel s:^ :}
   c CONDITION>N ?COND {: cc:n :}
   d REL>N ?SIMM32 {: delta:n :}
   $0F80 cc + s EMIT-OP  delta s EMIT-D ;

: ENC-JMP-REG ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}  4 r s BRANCH-REG ;

: ENC-CALL-REG ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :} 2 r s BRANCH-REG ;

: ENC-RET ( ^VEC:vector<u8> -- ) {: s:^ :}  $C3 s EMIT-B ;

: ENC-SYSCALL ( ^VEC:vector<u8> -- ) {: s:^ :}  $0F05 s EMIT-OP ;

: ENC-CPUID ( ^VEC:vector<u8> -- ) {: s:^ :}  $0FA2 s EMIT-OP ;

: ENC-NOP ( ^VEC:vector<u8> -- ) {: s:^ :}  $90 s EMIT-B ;

: ENC-UD2 ( ^VEC:vector<u8> -- ) {: s:^ :}  $0F0B s EMIT-OP ;

\ setcc writes one byte, so its destination is a byte register and carries that
\ file's REX rule.
: ENC-SETCC ( condition r8 ^VEC:vector<u8> -- ) {: c:condition r:r8 s:^ :}
   r R8>N REG-NUM {: n:n :}
   W8 $0F90 c CONDITION>N ?COND + 0 n n R8-FORCE s RR-ENC ;

: ENC-CMOVCC ( condition r64 r64 ^VEC:vector<u8> -- ) {: c:condition d:r64 sr:r64 s:^ :}
   W64 $0F40 c CONDITION>N ?COND + d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC ;

\ ---- the stack and the exchange ----------------------------------------------
\ push and pop are already 64-bit, so they take REX only to reach r8..r15.
: ENC-PUSH ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}
   r R64>N REG-NUM {: n:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $50 n 7 and + s EMIT-B ;

: ENC-POP ( r64 ^VEC:vector<u8> -- ) {: r:r64 s:^ :}
   r R64>N REG-NUM {: n:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $58 n 7 and + s EMIT-B ;

\ xchg between two registers touches no memory, so nothing locks it. The first
\ operand takes the reg field, the second the rm field.
: ENC-XCHG-RR ( r64 r64 ^VEC:vector<u8> -- ) {: l:r64 r:r64 s:^ :}
   W64 $87 l R64>N REG-NUM r R64>N REG-NUM 0 s RR-ENC ;

\ ---- the atomic forms and the fence ------------------------------------------
\ Only the memory form of xchg is implicitly locked, so it takes no prefix;
\ xadd and cmpxchg take the f0 lock prefix. The register is the source, as in
\ the stores. xchg and xadd leave the old memory value in it; cmpxchg compares
\ rax with memory, stores the register only on a match, and leaves the old
\ memory value in rax.
: ENC-XCHG-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}
   W64 $87 sr R64>N REG-NUM m 0 s RM-ENC ;

: ENC-XCHG32-MR ( r32 mem ^VEC:vector<u8> -- ) {: sr:r32 m:mem s:^ :}
   W32 $87 sr R32>N REG-NUM m 0 s RM-ENC ;

: ENC-LOCK-XADD-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}
   $0FC1 sr m s LOCK-RM ;

: ENC-LOCK-CMPXCHG-MR ( r64 mem ^VEC:vector<u8> -- ) {: sr:r64 m:mem s:^ :}
   $0FB1 sr m s LOCK-RM ;

\ mfence is the group-15 opcode 0F AE with the register-form ModRM of digit 6.
: ENC-MFENCE ( ^VEC:vector<u8> -- ) {: s:^ :}
   $0FAE s EMIT-OP  3 6 0 MODRM s EMIT-B ;

;package
