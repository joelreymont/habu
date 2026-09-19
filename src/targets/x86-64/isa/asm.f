\ Pure x86-64 integer/SSE2 instruction construction. No publication or execution.
require lib/prelude.f
require lib/type/deftype.f
require lib/errors.f

package X64ASM
public

DEFTYPE GPR
DEFTYPE XMM
DEFTYPE CONDITION
DEFTYPE DISPLACEMENT
DEFTYPE SCALE
DEFTYPE INSN-SIZE
DEFTYPE BYTE-INDEX

E-X64ASM-OPERAND constant E-OPERAND
E-X64ASM-SIZE constant E-SIZE
E-X64ASM-HOST constant E-HOST

\ Low holds bytes 0..7, high holds bytes 8..14; both are little-endian bit packs.
STRUCTURE instruction 0
   FIELD low n
   FIELD high n
   FIELD size insn-size
;STRUCTURE

\ mode 0: base [+ index*scale] + displacement; mode 1: RIP + displacement.
\ index = -1 means absent. There is deliberately no high-byte register family.
STRUCTURE memory 0
   FIELD base n
   FIELD index n
   FIELD scale scale
   FIELD displacement displacement
   FIELD mode n
;STRUCTURE

private

: HOST64 ( -- )
   1 cells 8 <> if E-HOST throw then ;

: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then value ;

: REG ( gpr -- n ) GPR>N 0 15 RANGE ;
: FPREG ( xmm -- n ) XMM>N 0 15 RANGE ;
: CC-CODE ( condition -- n ) CONDITION>N 0 15 RANGE ;
: DISP ( displacement -- n ) DISPLACEMENT>N -$80000000 $7FFFFFFF RANGE ;
: IMM32 ( n -- n ) -$80000000 $7FFFFFFF RANGE ;
: IMM8 ( n -- n ) 0 $FF RANGE ;
: S8? ( n -- bool ) dup -$80 >= swap $7F <= and ;

: SCALE-CODE ( scale -- n ) SCALE>N {: value:n :}
   value 1 = if 0 exit then value 2 = if 1 exit then
   value 4 = if 2 exit then value 8 = if 3 exit then E-OPERAND throw ;

: EMPTY ( -- instruction )
   HOST64 0 0 0 >INSN-SIZE X64ASM-INSTRUCTION:MAKE ;

: I+ ( instruction n -- instruction ) {: insn:instruction byte:n :}
   byte IMM8 drop
   insn X64ASM-INSTRUCTION:UNMAKE {: low:n high:n size:insn-size :}
   size INSN-SIZE>N 0 14 RANGE {: len:n :}
   len 8 < if
      low byte len 8 * lshift or high
   else
      low high byte len 8 - 8 * lshift or
   then
   len 1 + >INSN-SIZE X64ASM-INSTRUCTION:MAKE ;

: I32+ ( instruction n -- instruction ) {: insn:instruction value:n :}
   insn value $FF and I+ value 8 rshift $FF and I+
   value 16 rshift $FF and I+ value 24 rshift $FF and I+ ;

: I64+ ( instruction n -- instruction ) {: insn:instruction value:n :}
   insn value I32+ value 32 rshift I32+ ;

: REX+ ( instruction n n n n bool -- instruction )
   {: insn:instruction wide:n reg:n index:n base:n force:bool :}
   $40 wide 3 lshift or reg 3 rshift 2 lshift or
   index 3 rshift 1 lshift or base 3 rshift or {: rex:n :}
   rex $40 <> force or if insn rex I+ else insn then ;

: OPCODE+ ( instruction n n -- instruction )
   {: insn:instruction first:n second:n :}
   insn first I+ second -1 <> if second I+ then ;

: MOD-REG+ ( instruction n n -- instruction )
   {: insn:instruction reg:n rm:n :}
   insn $C0 reg 7 and 3 lshift or rm 7 and or I+ ;

: RR ( n n n n n n bool -- instruction )
   {: reg:n rm:n wide:n prefix:n first:n second:n force:bool :}
   EMPTY prefix 0 <> if prefix I+ then
   wide reg 0 rm force REX+ first second OPCODE+ reg rm MOD-REG+ ;

: BINARY ( gpr gpr n -- instruction ) {: dst:gpr src:gpr opcode:n :}
   src REG dst REG 1 0 opcode -1 false RR ;

: EXTENDED ( gpr gpr n -- instruction ) {: dst:gpr src:gpr opcode:n :}
   dst REG src REG 1 0 $0F opcode false RR ;

: UNARY ( gpr n -- instruction ) {: dst:gpr group:n :}
   group dst REG 1 0 $F7 -1 false RR ;

: GROUP-I32 ( gpr n n n -- instruction )
   {: dst:gpr value:n group:n accumulator:n :}
   dst REG {: reg:n :} value IMM32 drop
   value S8? if
      group reg 1 0 $83 -1 false RR value $FF and I+ exit
   then
   reg 0 = if
      EMPTY 1 0 0 0 false REX+ accumulator I+ value I32+
   else
      group reg 1 0 $81 -1 false RR value I32+
   then ;

: SHIFT-I8 ( gpr n n -- instruction )
   {: dst:gpr value:n group:n :}
   value IMM8 drop
   value 1 = if group dst REG 1 0 $D1 -1 false RR exit then
   group dst REG 1 0 $C1 -1 false RR value I+ ;

: SHIFT-CL ( gpr n -- instruction ) {: dst:gpr group:n :}
   group dst REG 1 0 $D3 -1 false RR ;

: SSE-RR ( xmm xmm n n -- instruction )
   {: dst:xmm src:xmm prefix:n opcode:n :}
   dst FPREG src FPREG 0 prefix $0F opcode false RR ;

: CHECK-MEM ( memory -- )
   X64ASM-MEMORY:UNMAKE
   {: base:n index:n scale:scale delta:displacement mode:n :}
   delta DISP drop scale SCALE-CODE drop
   mode 1 = if
      base 0 <> index -1 <> or scale SCALE>N 1 <> or
      if E-OPERAND throw then exit
   then
   mode 0 <> if E-OPERAND throw then base 0 15 RANGE drop
   index -1 = if
      scale SCALE>N 1 <> if E-OPERAND throw then exit
   then
   index 0 15 RANGE drop index 4 = if E-OPERAND throw then ;

: MOD-BITS ( n n -- n ) {: base:n delta:n :}
   delta 0= base 7 and 5 <> and if 0 exit then
   delta S8? if 1 else 2 then ;

: MEM-TAIL ( instruction n memory -- instruction )
   {: insn:instruction reg:n mem:memory :}
   mem X64ASM-MEMORY:UNMAKE
   {: base:n index:n scale:scale delta:displacement mode:n :}
   mode 1 = if
      insn reg 7 and 3 lshift 5 or I+ delta DISP I32+ exit
   then
   base delta DISP MOD-BITS {: md:n :}
   index -1 <> base 7 and 4 = or {: sib:bool :}
   insn md 6 lshift reg 7 and 3 lshift or
   sib if 4 else base 7 and then or I+
   sib if
      scale SCALE-CODE 6 lshift
      index -1 = if 4 else index 7 and then 3 lshift or
      base 7 and or I+
   then
   md 1 = if delta DISP $FF and I+ then
   md 2 = if delta DISP I32+ then ;

: MEM-OP ( n memory n n n n bool -- instruction )
   {: reg:n mem:memory wide:n prefix:n first:n second:n force:bool :}
   mem CHECK-MEM
   mem X64ASM-MEMORY:UNMAKE
   {: base:n index:n scale:scale delta:displacement mode:n :}
   EMPTY prefix 0 <> if prefix I+ then
   wide reg index -1 = if 0 else index then base force REX+
   first second OPCODE+ reg mem MEM-TAIL ;

public

: SIZE ( instruction -- insn-size )
   X64ASM-INSTRUCTION:UNMAKE {: low:n high:n size:insn-size :} size ;

\ Generated MAKE/converters are public: consumers must not trust forged sizes.
: VALIDATE ( instruction -- )
   HOST64 SIZE INSN-SIZE>N dup 1 < swap 15 > or if E-SIZE throw then ;

: BYTE ( instruction byte-index -- n ) {: insn:instruction index:byte-index :}
   insn VALIDATE
   insn X64ASM-INSTRUCTION:UNMAKE {: low:n high:n size:insn-size :}
   index BYTE-INDEX>N 0 size INSN-SIZE>N 1 - RANGE {: pos:n :}
   pos 8 < if low pos 8 * rshift else high pos 8 - 8 * rshift then $FF and ;

: BASE-MEM ( gpr displacement -- memory ) {: base:gpr delta:displacement :}
   base REG -1 1 >SCALE delta dup DISP drop 0 X64ASM-MEMORY:MAKE ;

: INDEXED ( gpr gpr scale displacement -- memory )
   {: base:gpr index:gpr scale:scale delta:displacement :}
   base REG index REG scale delta 0 X64ASM-MEMORY:MAKE
   dup CHECK-MEM ;

: RIP ( displacement -- memory ) {: delta:displacement :}
   delta DISP drop 0 -1 1 >SCALE delta 1 X64ASM-MEMORY:MAKE ;

: MOV64 ( gpr gpr -- instruction ) $89 BINARY ;
: ADD64 ( gpr gpr -- instruction ) $01 BINARY ;
: OR64 ( gpr gpr -- instruction ) $09 BINARY ;
: AND64 ( gpr gpr -- instruction ) $21 BINARY ;
: SUB64 ( gpr gpr -- instruction ) $29 BINARY ;
: XOR64 ( gpr gpr -- instruction ) $31 BINARY ;
: CMP64 ( gpr gpr -- instruction ) $39 BINARY ;
: TEST64 ( gpr gpr -- instruction ) $85 BINARY ;
: IMUL64 ( gpr gpr -- instruction ) $AF EXTENDED ;

: MOVABS ( gpr n -- instruction ) {: dst:gpr value:n :}
   dst REG {: reg:n :}
   EMPTY 1 0 0 reg false REX+ $B8 reg 7 and or I+ value I64+ ;

: ADD-I32 ( gpr n -- instruction ) 0 $05 GROUP-I32 ;
: OR-I32 ( gpr n -- instruction ) 1 $0D GROUP-I32 ;
: AND-I32 ( gpr n -- instruction ) 4 $25 GROUP-I32 ;
: SUB-I32 ( gpr n -- instruction ) 5 $2D GROUP-I32 ;
: XOR-I32 ( gpr n -- instruction ) 6 $35 GROUP-I32 ;
: CMP-I32 ( gpr n -- instruction ) 7 $3D GROUP-I32 ;
: NOT64 ( gpr -- instruction ) 2 UNARY ;
: NEG64 ( gpr -- instruction ) 3 UNARY ;
: DIV64 ( gpr -- instruction ) 6 UNARY ;
: IDIV64 ( gpr -- instruction ) 7 UNARY ;
: CQO ( -- instruction ) EMPTY $48 I+ $99 I+ ;

: SHL-I8 ( gpr n -- instruction ) 4 SHIFT-I8 ;
: SHR-I8 ( gpr n -- instruction ) 5 SHIFT-I8 ;
: SAR-I8 ( gpr n -- instruction ) 7 SHIFT-I8 ;
: SHL-CL ( gpr -- instruction ) 4 SHIFT-CL ;
: SHR-CL ( gpr -- instruction ) 5 SHIFT-CL ;
: SAR-CL ( gpr -- instruction ) 7 SHIFT-CL ;

: PUSH64 ( gpr -- instruction ) REG {: reg:n :}
   EMPTY 0 0 0 reg false REX+ $50 reg 7 and or I+ ;

: POP64 ( gpr -- instruction ) REG {: reg:n :}
   EMPTY 0 0 0 reg false REX+ $58 reg 7 and or I+ ;

: SETCC ( gpr condition -- instruction ) {: dst:gpr cond:condition :}
   dst REG {: reg:n :}
   0 reg 0 0 $0F $90 cond CC-CODE or reg 4 >= RR ;

\ Destination is the 32-bit view of dst (and hence zero-extends to 64 bits).
: MOVZX8 ( gpr gpr -- instruction ) {: dst:gpr src:gpr :}
   src REG {: source:n :}
   dst REG source 0 0 $0F $B6 source 4 >= RR ;

: LOAD64 ( gpr memory -- instruction ) {: dst:gpr mem:memory :}
   dst REG mem 1 0 $8B -1 false MEM-OP ;

: STORE64 ( gpr memory -- instruction ) {: src:gpr mem:memory :}
   src REG mem 1 0 $89 -1 false MEM-OP ;

: LEA64 ( gpr memory -- instruction ) {: dst:gpr mem:memory :}
   dst REG mem 1 0 $8D -1 false MEM-OP ;

: LOADU8 ( gpr memory -- instruction ) {: dst:gpr mem:memory :}
   dst REG mem 0 0 $0F $B6 false MEM-OP ;

: STORE8 ( gpr memory -- instruction ) {: src:gpr mem:memory :}
   src REG {: reg:n :} reg mem 0 0 $88 -1 reg 4 >= MEM-OP ;

\ Store exactly 32 bits. A trailing immediate changes the RIP-relative PC base.
: STORE-I32 ( memory n -- instruction ) {: mem:memory value:n :}
   value IMM32 drop 0 mem 0 0 $C7 -1 false MEM-OP value I32+ ;

: CALL32 ( displacement -- instruction )
   DISP {: delta:n :} EMPTY $E8 I+ delta I32+ ;

: JMP32 ( displacement -- instruction )
   DISP {: delta:n :} EMPTY $E9 I+ delta I32+ ;

: JCC32 ( condition displacement -- instruction )
   {: cond:condition delta:displacement :}
   cond CC-CODE drop delta DISP drop
   EMPTY $0F I+ $80 cond CC-CODE or I+ delta DISP I32+ ;

: CALL-REG ( gpr -- instruction ) REG 2 swap 0 0 $FF -1 false RR ;
: JMP-REG ( gpr -- instruction ) REG 4 swap 0 0 $FF -1 false RR ;
: RET ( -- instruction ) EMPTY $C3 I+ ;
: NOP ( -- instruction ) EMPTY $90 I+ ;
: UD2 ( -- instruction ) EMPTY $0F I+ $0B I+ ;

: MOVSD ( xmm xmm -- instruction ) $F2 $10 SSE-RR ;
: ADDSD ( xmm xmm -- instruction ) $F2 $58 SSE-RR ;
: MULSD ( xmm xmm -- instruction ) $F2 $59 SSE-RR ;
: SUBSD ( xmm xmm -- instruction ) $F2 $5C SSE-RR ;
: DIVSD ( xmm xmm -- instruction ) $F2 $5E SSE-RR ;
: SQRTSD ( xmm xmm -- instruction ) $F2 $51 SSE-RR ;
: ANDPD ( xmm xmm -- instruction ) $66 $54 SSE-RR ;
: XORPD ( xmm xmm -- instruction ) $66 $57 SSE-RR ;
: UCOMISD ( xmm xmm -- instruction ) $66 $2E SSE-RR ;

: LOADSD ( xmm memory -- instruction ) {: dst:xmm mem:memory :}
   dst FPREG mem 0 $F2 $0F $10 false MEM-OP ;

: STORESD ( xmm memory -- instruction ) {: src:xmm mem:memory :}
   src FPREG mem 0 $F2 $0F $11 false MEM-OP ;

: GPR>SD ( xmm gpr -- instruction ) {: dst:xmm src:gpr :}
   dst FPREG src REG 1 $F2 $0F $2A false RR ;

\ Hardware truncation only; not a complete Habu realint compatibility lowering.
: SD>GPR ( gpr xmm -- instruction ) {: dst:gpr src:xmm :}
   dst REG src FPREG 1 $F2 $0F $2C false RR ;

;package
