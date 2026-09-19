\ asm.f — x86_64 instruction encoders, package X64ASM.
\
\ The same discipline as src/arch/arm32/asm.f and src/arch/arm64/asm.f: operands
\ are nominal types, every encoder screens an operand BEFORE it packs a bit, and
\ a refusal is this package's E-OPERAND. docs/embedded-encoders.md states the
\ operand order these constructors share with the ARM32 and C66x sets.
\
\ ONE DIFFERENCE FORCED BY THE ARCHITECTURE. An ARM32, ARM64 or C66x instruction
\ IS a 32-bit value, so those files encode to a value and a separate layer
\ (src/arch/arm64/mnem.f over icode.f EMITW) emits it. An x86_64 instruction is
\ one to fifteen bytes and has no value representation, so the encode and the
\ emit collapse into one layer here: every ENC-* word takes the byte sink as its
\ LAST operand and appends through lib/byte-buffer.f (BUF:APPEND-BYTE). There is
\ therefore no instruction nominal type in this package — the nominal typing is
\ entirely on the operands, which is where an ill-formed instruction is caught.
\
\ THE SIZE CONTRACT THE LOWERING LAYER RELIES ON. Every encoder is deterministic:
\ the emitted length is a function of the word the caller chose and of the
\ memory operand's own displacement magnitude, never of a peephole or a
\ relaxation pass. A caller that needs an instruction's size encodes it into a
\ scratch buffer and reads the length. Branch width is the caller's choice too:
\ rel8 and rel32 are separate words, and a rel is measured in bytes from the END
\ of the instruction, which is the x86 rule (ARM32 measures from the instruction
\ address plus 8 or 4).
\
\ WHAT IS DELIBERATELY NOT HERE. The accumulator short forms (05 id for
\ add rax/imm32, A9 for test rax/imm32, 90+r for xchg rax/reg) and the D1 /n
\ shift-by-one form are size optimisations, not capability, so each operation
\ has exactly one encoding here. llvm-mc prefers those short forms, which is why
\ test/compiler/x86-64-asm.f pins the general forms with registers other than
\ rax and with shift counts other than one.
\
\ THE 8-BIT REGISTER FILE IS THE REX-ONLY ONE. An r8 numbered 4..7 is spl, bpl,
\ sil or dil, never the legacy ah, ch, dh or bh, so any r8 operand at or above 4
\ forces a REX prefix even when no REX field would otherwise be set. The legacy
\ high-byte registers are not expressible.
\
\ The register name words RAX..R15 are 64-bit registers; the nominal type tails
\ r64/r32/r16/r8 name the four register FILES. `R8` the word is the ninth 64-bit
\ register; `r8` the type is the byte-register file.

require lib/type/deftype.f
require lib/errors.f
require lib/byte-buffer.f

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

\ `mov r64, imm64` is REX.W B8+r io, and its REX byte is always present, so the
\ eight immediate bytes always begin at this offset from the first byte of the
\ instruction. It is the one relocatable literal form the x86_64 design names
\ (docs/x86-64.md), and the relocation writer patches at this offset.
2 constant MOV-RI64-IMM-OFF

private

\ ---- operand screening -------------------------------------------------------
: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then
   value ;

: REG-NUM ( n -- n )  0 15 RANGE ;

: ?COND ( n -- n )  0 15 RANGE ;

\ An imm8 the machine sign-extends to the operation width.
: ?SIMM8 ( n -- n )  -128 127 RANGE ;

\ A shift count. The hardware masks to six bits; a wider count is a caller bug.
: ?SHIFT-COUNT ( n -- n )  0 63 RANGE ;

\ An imm32 the machine sign-extends to 64 bits (C7 /0, 81 /n, 69 /r, rel32).
: ?SIMM32 ( n -- n )  -2147483648 2147483647 RANGE ;

\ An imm32 the machine zero-extends to 64 bits (B8+rd into a 32-bit register).
: ?UIMM32 ( n -- n )  0 $FFFFFFFF RANGE ;

\ ---- the byte sink -----------------------------------------------------------
\ lib/byte-buffer.f is the tree's checked growable byte buffer; these four are
\ the little-endian appends this encoder needs and are private to it, so the
\ library's public surface is unchanged.
: EMIT-B ( n ptr a -- ) {: v:n s:ptr :}
   v $FF and s BUF:APPEND-BYTE ;

: EMIT-W ( n ptr a -- ) {: v:n s:ptr :}
   v s EMIT-B  v 8 rshift s EMIT-B ;

: EMIT-D ( n ptr a -- ) {: v:n s:ptr :}
   v s EMIT-W  v 16 rshift s EMIT-W ;

: EMIT-Q ( n ptr a -- ) {: v:n s:ptr :}
   v s EMIT-D  v 32 rshift s EMIT-D ;

\ ---- operation width ---------------------------------------------------------
\ 16-bit operations carry the 66 prefix; 64-bit operations carry REX.W; 32-bit
\ operations carry neither and zero-extend their result into the full register.
0 constant W8   1 constant W16   2 constant W32   3 constant W64

: WIDTH-66? ( n -- bool )  W16 = ;

: WIDTH-W ( n -- n )  W64 = if 1 else 0 then ;

\ An 8-bit register at or above 4 is spl/bpl/sil/dil or an r8b..r15b, and each
\ of those is spelled only with a REX prefix present.
: R8-FORCE ( n -- n )  4 >= if 1 else 0 then ;

\ ---- REX, ModRM and SIB ------------------------------------------------------
\ The REX byte is omitted entirely when every one of its fields is zero and no
\ operand forces it, which is what makes a 32-bit or 8-bit form short.
: EMIT-REX ( n n n n n ptr a -- ) {: w:n r:n x:n b:n force:n s:ptr :}
   w r or x or b or force or 0= if exit then
   $40 w 3 lshift or r 2 lshift or x 1 lshift or b or s EMIT-B ;

: MODRM ( n n n -- n ) {: mod:n reg:n rm:n :}
   mod 6 lshift  reg 7 and 3 lshift or  rm 7 and or ;

: SIB ( n n n -- n ) {: ss:n index:n base:n :}
   ss 6 lshift  index 7 and 3 lshift or  base 7 and or ;

\ A two-byte opcode is carried as 0Fxx, so its escape byte is its high byte.
: EMIT-OP ( n ptr a -- ) {: op:n s:ptr :}
   op 8 rshift $FF and {: esc:n :}
   esc 0<> if esc s EMIT-B then
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

: EMIT-DISP ( n mem ptr a -- ) {: mod:n m:mem s:ptr :}
   mod 1 = if m MEM-DISP s EMIT-B exit then
   mod 2 = if m MEM-DISP s EMIT-D then ;

: EMIT-MEM ( n mem ptr a -- ) {: reg:n m:mem s:ptr :}
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
: RR-ENC ( n n n n n ptr a -- ) {: wd:n op:n reg:n rm:n force:n s:ptr :}
   wd WIDTH-66? if $66 s EMIT-B then
   wd WIDTH-W  reg 3 rshift  0  rm 3 rshift  force s EMIT-REX
   op s EMIT-OP
   3 reg rm MODRM s EMIT-B ;

\ A memory form: the reg field as above, the rm field spelled by the ModRM, the
\ optional SIB and the displacement.
: RM-ENC ( n n n mem n ptr a -- ) {: wd:n op:n reg:n m:mem force:n s:ptr :}
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
: ALU-RR ( n r64 r64 ptr a -- ) {: base:n dst:r64 src:r64 s:ptr :}
   W64 base 1 + src R64>N REG-NUM dst R64>N REG-NUM 0 s RR-ENC ;

: ALU-RM ( n r64 mem ptr a -- ) {: base:n dst:r64 m:mem s:ptr :}
   W64 base 3 + dst R64>N REG-NUM m 0 s RM-ENC ;

: ALU-MR ( n r64 mem ptr a -- ) {: base:n src:r64 m:mem s:ptr :}
   W64 base 1 + src R64>N REG-NUM m 0 s RM-ENC ;

\ The immediate is screened BEFORE the first byte reaches the sink, so a refused
\ instruction leaves no prefix of itself behind. Every immediate form below
\ follows this order for the same reason.
: ALU-RI8 ( n r64 imm8 ptr a -- ) {: base:n dst:r64 v:imm8 s:ptr :}
   v IMM8>N ?SIMM8 {: imm:n :}
   W64 $83 base 3 rshift dst R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-B ;

: ALU-RI32 ( n r64 imm32 ptr a -- ) {: base:n dst:r64 v:imm32 s:ptr :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $81 base 3 rshift dst R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- shared shapes for the remaining families --------------------------------
: UNARY-F7 ( n r64 ptr a -- ) {: digit:n r:r64 s:ptr :}
   W64 $F7 digit r R64>N REG-NUM 0 s RR-ENC ;

: UNARY-FF ( n r64 ptr a -- ) {: digit:n r:r64 s:ptr :}
   W64 $FF digit r R64>N REG-NUM 0 s RR-ENC ;

: SHIFT-RI8 ( n r64 imm8 ptr a -- ) {: digit:n r:r64 v:imm8 s:ptr :}
   v IMM8>N ?SHIFT-COUNT {: count:n :}
   W64 $C1 digit r R64>N REG-NUM 0 s RR-ENC
   count s EMIT-B ;

: SHIFT-CL ( n r64 ptr a -- ) {: digit:n r:r64 s:ptr :}
   W64 $D3 digit r R64>N REG-NUM 0 s RR-ENC ;

: EXT-RM ( n n r64 mem ptr a -- ) {: wd:n op:n dst:r64 m:mem s:ptr :}
   wd op dst R64>N REG-NUM m 0 s RM-ENC ;

\ An indirect jump or call is FF /digit with no REX.W: a near branch target is a
\ full 64-bit register already.
: BRANCH-REG ( n r64 ptr a -- ) {: digit:n r:r64 s:ptr :}
   W32 $FF digit r R64>N REG-NUM 0 s RR-ENC ;

\ Mandatory SSE prefixes precede REX. Screen both registers before that prefix;
\ REX.W is set only by the signed 64-bit integer conversion forms.
: SSE-RR ( n n n n n ptr a -- ) {: prefix:n wd:n op:n reg:n rm:n s:ptr :}
   reg REG-NUM {: rn:n :}  rm REG-NUM {: mn:n :}
   prefix s EMIT-B
   wd op rn mn 0 s RR-ENC ;

: MOVSD-MEM ( n xmm mem ptr a -- ) {: op:n r:xmm m:mem s:ptr :}
   r XMM>N REG-NUM {: rn:n :}  m MEM-CHECK
   $F2 s EMIT-B
   0 rn 3 rshift m MEM-X-BIT m MEM-B-BIT 0 s EMIT-REX
   op s EMIT-OP
   rn m s EMIT-MEM ;

public

\ ---- the 64-bit register file ------------------------------------------------
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
\ Base, base plus displacement, base plus scaled index plus displacement, and
\ rip-relative. The index may not be rsp — that encoding names "no index" — and
\ the scale must be 1, 2, 4 or 8.
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
\ Five forms each: register to register, register from memory, memory from
\ register, register with a sign-extended imm8, register with an imm32.
: ENC-ADD-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $00 d sr s ALU-RR ;
: ENC-ADD-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $00 d m s ALU-RM ;
: ENC-ADD-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $00 sr m s ALU-MR ;
: ENC-ADD-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $00 d v s ALU-RI8 ;
: ENC-ADD-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $00 d v s ALU-RI32 ;

: ENC-OR-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}   $08 d sr s ALU-RR ;
: ENC-OR-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}    $08 d m s ALU-RM ;
: ENC-OR-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}   $08 sr m s ALU-MR ;
: ENC-OR-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}    $08 d v s ALU-RI8 ;
: ENC-OR-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $08 d v s ALU-RI32 ;

: ENC-ADC-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $10 d sr s ALU-RR ;
: ENC-ADC-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $10 d m s ALU-RM ;
: ENC-ADC-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $10 sr m s ALU-MR ;
: ENC-ADC-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $10 d v s ALU-RI8 ;
: ENC-ADC-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $10 d v s ALU-RI32 ;

: ENC-SBB-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $18 d sr s ALU-RR ;
: ENC-SBB-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $18 d m s ALU-RM ;
: ENC-SBB-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $18 sr m s ALU-MR ;
: ENC-SBB-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $18 d v s ALU-RI8 ;
: ENC-SBB-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $18 d v s ALU-RI32 ;

: ENC-AND-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $20 d sr s ALU-RR ;
: ENC-AND-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $20 d m s ALU-RM ;
: ENC-AND-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $20 sr m s ALU-MR ;
: ENC-AND-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $20 d v s ALU-RI8 ;
: ENC-AND-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $20 d v s ALU-RI32 ;

: ENC-SUB-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $28 d sr s ALU-RR ;
: ENC-SUB-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $28 d m s ALU-RM ;
: ENC-SUB-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $28 sr m s ALU-MR ;
: ENC-SUB-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $28 d v s ALU-RI8 ;
: ENC-SUB-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $28 d v s ALU-RI32 ;

: ENC-XOR-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $30 d sr s ALU-RR ;
: ENC-XOR-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $30 d m s ALU-RM ;
: ENC-XOR-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $30 sr m s ALU-MR ;
: ENC-XOR-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $30 d v s ALU-RI8 ;
: ENC-XOR-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $30 d v s ALU-RI32 ;

: ENC-CMP-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}  $38 d sr s ALU-RR ;
: ENC-CMP-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}   $38 d m s ALU-RM ;
: ENC-CMP-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}  $38 sr m s ALU-MR ;
: ENC-CMP-RI8 ( r64 imm8 ptr a -- ) {: d:r64 v:imm8 s:ptr :}   $38 d v s ALU-RI8 ;
: ENC-CMP-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :} $38 d v s ALU-RI32 ;

\ ---- test --------------------------------------------------------------------
\ test is its own opcode family: 85 /r both directions (it is commutative, so the
\ memory form is the one encoding) and F7 /0 id for the immediate. It has no
\ sign-extended imm8 form.
: ENC-TEST-RR ( r64 r64 ptr a -- ) {: l:r64 r:r64 s:ptr :}
   W64 $85 r R64>N REG-NUM l R64>N REG-NUM 0 s RR-ENC ;

: ENC-TEST-MR ( r64 mem ptr a -- ) {: r:r64 m:mem s:ptr :}
   W64 $85 r R64>N REG-NUM m 0 s RM-ENC ;

: ENC-TEST-RI32 ( r64 imm32 ptr a -- ) {: r:r64 v:imm32 s:ptr :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $F7 0 r R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- moves, loads and stores, all four widths --------------------------------
\ 89 stores the reg field into the rm operand, 8B loads it from there; the 8-bit
\ pair is 88 and 8A. A 32-bit form zero-extends its result into the full 64-bit
\ register, which is why the 32-bit move is the cheap zeroing and truncating one.
: ENC-MOV-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}
   W64 $89 sr R64>N REG-NUM d R64>N REG-NUM 0 s RR-ENC ;

: ENC-MOV-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $8B d R64>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV-MR ( r64 mem ptr a -- ) {: sr:r64 m:mem s:ptr :}
   W64 $89 sr R64>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV32-RR ( r32 r32 ptr a -- ) {: d:r32 sr:r32 s:ptr :}
   W32 $89 sr R32>N REG-NUM d R32>N REG-NUM 0 s RR-ENC ;

: ENC-MOV32-RM ( r32 mem ptr a -- ) {: d:r32 m:mem s:ptr :}
   W32 $8B d R32>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV32-MR ( r32 mem ptr a -- ) {: sr:r32 m:mem s:ptr :}
   W32 $89 sr R32>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV16-RR ( r16 r16 ptr a -- ) {: d:r16 sr:r16 s:ptr :}
   W16 $89 sr R16>N REG-NUM d R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOV16-RM ( r16 mem ptr a -- ) {: d:r16 m:mem s:ptr :}
   W16 $8B d R16>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV16-MR ( r16 mem ptr a -- ) {: sr:r16 m:mem s:ptr :}
   W16 $89 sr R16>N REG-NUM m 0 s RM-ENC ;

: ENC-MOV8-RR ( r8 r8 ptr a -- ) {: d:r8 sr:r8 s:ptr :}
   sr R8>N REG-NUM {: sn:n :}
   d R8>N REG-NUM {: dn:n :}
   W8 $88 sn dn sn R8-FORCE dn R8-FORCE or s RR-ENC ;

: ENC-MOV8-RM ( r8 mem ptr a -- ) {: d:r8 m:mem s:ptr :}
   d R8>N REG-NUM {: dn:n :}
   W8 $8A dn m dn R8-FORCE s RM-ENC ;

: ENC-MOV8-MR ( r8 mem ptr a -- ) {: sr:r8 m:mem s:ptr :}
   sr R8>N REG-NUM {: sn:n :}
   W8 $88 sn m sn R8-FORCE s RM-ENC ;

\ C7 /0 id sign-extends its imm32 across the whole 64-bit register; B8+rd id
\ zero-extends, which is the only difference between the two constant forms and
\ the reason their immediates are screened against different ranges.
: ENC-MOV-RI32 ( r64 imm32 ptr a -- ) {: d:r64 v:imm32 s:ptr :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $C7 0 d R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

: ENC-MOV32-RI32 ( r32 imm32 ptr a -- ) {: d:r32 v:imm32 s:ptr :}
   d R32>N REG-NUM {: n:n :}
   v IMM32>N ?UIMM32 {: imm:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $B8 n 7 and + s EMIT-B
   imm s EMIT-D ;

\ The relocatable literal. MOV-RI64-IMM-OFF names where its immediate starts.
: ENC-MOV-RI64 ( r64 imm64 ptr a -- ) {: d:r64 v:imm64 s:ptr :}
   d R64>N REG-NUM {: n:n :}
   1 0 0 n 3 rshift 0 s EMIT-REX
   $B8 n 7 and + s EMIT-B
   v IMM64>N s EMIT-Q ;

\ ---- scalar double precision -------------------------------------------------
\ Register operands are destination then source, as for the integer forms.
\ ANDPD and XORPD operate on both 64-bit lanes; UCOMISD compares the low lane.
: ENC-MOVSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F10 d XMM>N sr XMM>N s SSE-RR ;
: ENC-MOVSD-RM ( xmm mem ptr a -- ) {: d:xmm m:mem s:ptr :}
   $0F10 d m s MOVSD-MEM ;
: ENC-MOVSD-MR ( xmm mem ptr a -- ) {: sr:xmm m:mem s:ptr :}
   $0F11 sr m s MOVSD-MEM ;
: ENC-ADDSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F58 d XMM>N sr XMM>N s SSE-RR ;
: ENC-SUBSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F5C d XMM>N sr XMM>N s SSE-RR ;
: ENC-MULSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F59 d XMM>N sr XMM>N s SSE-RR ;
: ENC-DIVSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F5E d XMM>N sr XMM>N s SSE-RR ;
: ENC-SQRTSD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $F2 W32 $0F51 d XMM>N sr XMM>N s SSE-RR ;
: ENC-ANDPD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $66 W32 $0F54 d XMM>N sr XMM>N s SSE-RR ;
: ENC-XORPD-RR ( xmm xmm ptr a -- ) {: d:xmm sr:xmm s:ptr :}
   $66 W32 $0F57 d XMM>N sr XMM>N s SSE-RR ;
: ENC-UCOMISD-RR ( xmm xmm ptr a -- ) {: l:xmm r:xmm s:ptr :}
   $66 W32 $0F2E l XMM>N r XMM>N s SSE-RR ;

\ Hardware conversions only: range/NaN handling for Habu's realint belongs to
\ lowering, not this byte encoder.
: ENC-CVTSI2SD-RR ( xmm r64 ptr a -- ) {: d:xmm sr:r64 s:ptr :}
   $F2 W64 $0F2A d XMM>N sr R64>N s SSE-RR ;
: ENC-CVTTSD2SI-RR ( r64 xmm ptr a -- ) {: d:r64 sr:xmm s:ptr :}
   $F2 W64 $0F2C d R64>N sr XMM>N s SSE-RR ;

\ ---- widening moves ----------------------------------------------------------
\ Every one of these widens into a 64-bit register, so REX.W is always set. movsxd
\ is the 32-to-64 sign extension and has no zero-extending counterpart: a plain
\ 32-bit move already zero-extends.
: ENC-MOVZX-8-RR ( r64 r8 ptr a -- ) {: d:r64 sr:r8 s:ptr :}
   sr R8>N REG-NUM {: sn:n :}
   W64 $0FB6 d R64>N REG-NUM sn sn R8-FORCE s RR-ENC ;

: ENC-MOVZX-8-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $0FB6 d m s EXT-RM ;

: ENC-MOVZX-16-RR ( r64 r16 ptr a -- ) {: d:r64 sr:r16 s:ptr :}
   W64 $0FB7 d R64>N REG-NUM sr R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOVZX-16-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $0FB7 d m s EXT-RM ;

: ENC-MOVSX-8-RR ( r64 r8 ptr a -- ) {: d:r64 sr:r8 s:ptr :}
   sr R8>N REG-NUM {: sn:n :}
   W64 $0FBE d R64>N REG-NUM sn sn R8-FORCE s RR-ENC ;

: ENC-MOVSX-8-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $0FBE d m s EXT-RM ;

: ENC-MOVSX-16-RR ( r64 r16 ptr a -- ) {: d:r64 sr:r16 s:ptr :}
   W64 $0FBF d R64>N REG-NUM sr R16>N REG-NUM 0 s RR-ENC ;

: ENC-MOVSX-16-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $0FBF d m s EXT-RM ;

: ENC-MOVSXD-RR ( r64 r32 ptr a -- ) {: d:r64 sr:r32 s:ptr :}
   W64 $63 d R64>N REG-NUM sr R32>N REG-NUM 0 s RR-ENC ;

: ENC-MOVSXD-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $63 d m s EXT-RM ;

\ ---- address arithmetic ------------------------------------------------------
: ENC-LEA ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $8D d m s EXT-RM ;

\ ---- the 32-bit zeroing idiom ------------------------------------------------
\ Kept as the one 32-bit arithmetic form: xor of a register with itself is two
\ bytes and clears the whole 64-bit register because a 32-bit result zero-extends.
: ENC-XOR32-RR ( r32 r32 ptr a -- ) {: d:r32 sr:r32 s:ptr :}
   W32 $31 sr R32>N REG-NUM d R32>N REG-NUM 0 s RR-ENC ;

\ ---- one-register arithmetic -------------------------------------------------
\ mul, imul, div and idiv here are the widening forms: they read rax (and rdx for
\ the divisions) implicitly and write the 128-bit result or the quotient and
\ remainder into rdx:rax. cqo sign-extends rax into rdx:rax to set up idiv.
: ENC-NOT ( r64 ptr a -- ) {: r:r64 s:ptr :}   2 r s UNARY-F7 ;
: ENC-NEG ( r64 ptr a -- ) {: r:r64 s:ptr :}   3 r s UNARY-F7 ;
: ENC-MUL ( r64 ptr a -- ) {: r:r64 s:ptr :}   4 r s UNARY-F7 ;
: ENC-IMUL1 ( r64 ptr a -- ) {: r:r64 s:ptr :} 5 r s UNARY-F7 ;
: ENC-DIV ( r64 ptr a -- ) {: r:r64 s:ptr :}   6 r s UNARY-F7 ;
: ENC-IDIV ( r64 ptr a -- ) {: r:r64 s:ptr :}  7 r s UNARY-F7 ;
: ENC-INC ( r64 ptr a -- ) {: r:r64 s:ptr :}   0 r s UNARY-FF ;
: ENC-DEC ( r64 ptr a -- ) {: r:r64 s:ptr :}   1 r s UNARY-FF ;

: ENC-CQO ( ptr a -- ) {: s:ptr :}
   1 0 0 0 0 s EMIT-REX
   $99 s EMIT-B ;

\ ---- the non-widening signed multiply ----------------------------------------
: ENC-IMUL-RR ( r64 r64 ptr a -- ) {: d:r64 sr:r64 s:ptr :}
   W64 $0FAF d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC ;

: ENC-IMUL-RM ( r64 mem ptr a -- ) {: d:r64 m:mem s:ptr :}
   W64 $0FAF d m s EXT-RM ;

: ENC-IMUL-RRI8 ( r64 r64 imm8 ptr a -- ) {: d:r64 sr:r64 v:imm8 s:ptr :}
   v IMM8>N ?SIMM8 {: imm:n :}
   W64 $6B d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-B ;

: ENC-IMUL-RRI32 ( r64 r64 imm32 ptr a -- ) {: d:r64 sr:r64 v:imm32 s:ptr :}
   v IMM32>N ?SIMM32 {: imm:n :}
   W64 $69 d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC
   imm s EMIT-D ;

\ ---- shifts and rotates ------------------------------------------------------
\ C1 /n ib shifts by an immediate, D3 /n by the count in cl. rol 0, ror 1,
\ shl 4, shr 5, sar 7.
: ENC-ROL-RI8 ( r64 imm8 ptr a -- ) {: r:r64 v:imm8 s:ptr :} 0 r v s SHIFT-RI8 ;
: ENC-ROR-RI8 ( r64 imm8 ptr a -- ) {: r:r64 v:imm8 s:ptr :} 1 r v s SHIFT-RI8 ;
: ENC-SHL-RI8 ( r64 imm8 ptr a -- ) {: r:r64 v:imm8 s:ptr :} 4 r v s SHIFT-RI8 ;
: ENC-SHR-RI8 ( r64 imm8 ptr a -- ) {: r:r64 v:imm8 s:ptr :} 5 r v s SHIFT-RI8 ;
: ENC-SAR-RI8 ( r64 imm8 ptr a -- ) {: r:r64 v:imm8 s:ptr :} 7 r v s SHIFT-RI8 ;

: ENC-ROL-CL ( r64 ptr a -- ) {: r:r64 s:ptr :} 0 r s SHIFT-CL ;
: ENC-ROR-CL ( r64 ptr a -- ) {: r:r64 s:ptr :} 1 r s SHIFT-CL ;
: ENC-SHL-CL ( r64 ptr a -- ) {: r:r64 s:ptr :} 4 r s SHIFT-CL ;
: ENC-SHR-CL ( r64 ptr a -- ) {: r:r64 s:ptr :} 5 r s SHIFT-CL ;
: ENC-SAR-CL ( r64 ptr a -- ) {: r:r64 s:ptr :} 7 r s SHIFT-CL ;

\ ---- control flow ------------------------------------------------------------
\ Every relative displacement is measured in bytes from the END of the branch, and
\ the caller picks the width by picking the word.
: ENC-JMP-REL8 ( rel ptr a -- ) {: d:rel s:ptr :}
   d REL>N ?SIMM8 {: delta:n :}
   $EB s EMIT-B  delta s EMIT-B ;

: ENC-JMP-REL32 ( rel ptr a -- ) {: d:rel s:ptr :}
   d REL>N ?SIMM32 {: delta:n :}
   $E9 s EMIT-B  delta s EMIT-D ;

: ENC-CALL-REL32 ( rel ptr a -- ) {: d:rel s:ptr :}
   d REL>N ?SIMM32 {: delta:n :}
   $E8 s EMIT-B  delta s EMIT-D ;

: ENC-JCC-REL8 ( condition rel ptr a -- ) {: c:condition d:rel s:ptr :}
   c CONDITION>N ?COND {: cc:n :}
   d REL>N ?SIMM8 {: delta:n :}
   $70 cc + s EMIT-B  delta s EMIT-B ;

: ENC-JCC-REL32 ( condition rel ptr a -- ) {: c:condition d:rel s:ptr :}
   c CONDITION>N ?COND {: cc:n :}
   d REL>N ?SIMM32 {: delta:n :}
   $0F80 cc + s EMIT-OP  delta s EMIT-D ;

: ENC-JMP-REG ( r64 ptr a -- ) {: r:r64 s:ptr :}  4 r s BRANCH-REG ;

: ENC-CALL-REG ( r64 ptr a -- ) {: r:r64 s:ptr :} 2 r s BRANCH-REG ;

: ENC-RET ( ptr a -- ) {: s:ptr :}  $C3 s EMIT-B ;

: ENC-SYSCALL ( ptr a -- ) {: s:ptr :}  $0F05 s EMIT-OP ;

\ setcc writes one byte, so its destination is a byte register and carries that
\ file's REX rule.
: ENC-SETCC ( condition r8 ptr a -- ) {: c:condition r:r8 s:ptr :}
   r R8>N REG-NUM {: n:n :}
   W8 $0F90 c CONDITION>N ?COND + 0 n n R8-FORCE s RR-ENC ;

: ENC-CMOVCC ( condition r64 r64 ptr a -- ) {: c:condition d:r64 sr:r64 s:ptr :}
   W64 $0F40 c CONDITION>N ?COND + d R64>N REG-NUM sr R64>N REG-NUM 0 s RR-ENC ;

\ ---- the stack and the exchange ----------------------------------------------
\ push and pop are already 64-bit, so they take REX only to reach r8..r15.
: ENC-PUSH ( r64 ptr a -- ) {: r:r64 s:ptr :}
   r R64>N REG-NUM {: n:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $50 n 7 and + s EMIT-B ;

: ENC-POP ( r64 ptr a -- ) {: r:r64 s:ptr :}
   r R64>N REG-NUM {: n:n :}
   0 0 0 n 3 rshift 0 s EMIT-REX
   $58 n 7 and + s EMIT-B ;

\ xchg between two registers is implicitly locked and is the seq-cst swap the
\ runtime needs. The first operand takes the reg field, the second the rm field.
: ENC-XCHG-RR ( r64 r64 ptr a -- ) {: l:r64 r:r64 s:ptr :}
   W64 $87 l R64>N REG-NUM r R64>N REG-NUM 0 s RR-ENC ;

;package
