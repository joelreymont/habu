\ C66x 32-bit encodings from SPRUGH7; no packet scheduling or memory emission.
require lib/type/deftype.f
require lib/errors.f

package C6XASM
public

DEFTYPE GPR
DEFTYPE SIDE
DEFTYPE BYTE-OFFSET
DEFTYPE BRANCH-OFFSET
DEFTYPE INSTRUCTION

E-C6XASM-OPERAND constant E-OPERAND

private

: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then
   value ;

: REG-CODE ( gpr -- n )
   GPR>N 0 63 RANGE ;

: REG-NUMBER ( gpr -- n )
   REG-CODE 31 and ;

: REG-SIDE ( gpr -- n )
   REG-CODE 5 rshift ;

: CHECK-WORD ( instruction -- n )
   INSTRUCTION>N 0 $FFFFFFFF RANGE ;

: SAME-SIDE ( gpr gpr -- )
   REG-SIDE swap REG-SIDE <> if E-OPERAND throw then ;

: CROSS-BIT ( gpr gpr -- n )
   REG-SIDE swap REG-SIDE xor 12 lshift ;

: DST-FIELD ( gpr -- n ) {: dst:gpr :}
   dst REG-NUMBER 23 lshift dst REG-SIDE 1 lshift or ;

: MOVE-BITS ( gpr n n -- instruction ) {: dst:gpr value:n opcode:n :}
   dst DST-FIELD value $FFFF and 7 lshift or opcode or >INSTRUCTION ;

: L-BASE ( gpr gpr n -- n ) {: dst:gpr right:gpr opcode:n :}
   dst DST-FIELD right REG-NUMBER 18 lshift or
   dst right CROSS-BIT or opcode 5 lshift or $18 or ;

: L-REGS ( gpr gpr gpr n -- instruction )
   {: dst:gpr left:gpr right:gpr opcode:n :}
   dst left SAME-SIDE
   dst right opcode L-BASE left REG-NUMBER 13 lshift or >INSTRUCTION ;

: L-CONST ( gpr gpr n n -- instruction )
   {: dst:gpr right:gpr value:n opcode:n :}
   value -16 15 RANGE $1F and 13 lshift
   dst right opcode L-BASE or >INSTRUCTION ;

: WORD-OFFSET ( byte-offset -- n )
   BYTE-OFFSET>N -124 124 RANGE
   dup 4 mod 0 <> if E-OPERAND throw then ;

: MEMORY-MODE ( n -- n )
   0 >= if $200 else 0 then ;

: MEMORY-WORD ( gpr gpr byte-offset n -- instruction )
   {: data:gpr base:gpr offset:byte-offset opcode:n :}
   offset WORD-OFFSET {: bytes:n :}
   data DST-FIELD base REG-NUMBER 18 lshift or base REG-SIDE 7 lshift or
   bytes abs 4 / 13 lshift or bytes MEMORY-MODE or opcode or >INSTRUCTION ;

: CONDITION-FIELD ( gpr -- n )
   REG-CODE dup 32 >= if 32 - 0 2 RANGE 1 + exit then
   0 2 RANGE dup 0= if drop 6 else 3 + then ;

: PREDICATED ( instruction gpr bool -- instruction )
   {: opcode:instruction guard:gpr inverted:bool :}
   opcode CHECK-WORD dup $1FFE and 0= if E-OPERAND throw then
   $0FFFFFFF and guard CONDITION-FIELD 29 lshift or
   inverted if $10000000 or then >INSTRUCTION ;

public

: A-REG ( n -- gpr )
   0 31 RANGE >GPR ;

: B-REG ( n -- gpr )
   0 31 RANGE 32 + >GPR ;

: ENC-MVK ( gpr n -- instruction )
   -32768 32767 RANGE $28 MOVE-BITS ;

: ENC-MVKL ( gpr n -- instruction )
   0 $FFFFFFFF RANGE $28 MOVE-BITS ;

\ MVKH takes the complete 32-bit constant, matching TI assembly syntax.
: ENC-MVKH ( gpr n -- instruction )
   0 $FFFFFFFF RANGE 16 rshift $68 MOVE-BITS ;

: ENC-MVKLH ( gpr n -- instruction )
   0 $FFFF RANGE $68 MOVE-BITS ;

: ENC-ADD-L ( gpr gpr gpr -- instruction )
   3 L-REGS ;

: ENC-ADD-I5 ( gpr gpr n -- instruction )
   2 L-CONST ;

: ENC-SUB-L ( gpr gpr gpr -- instruction )
   7 L-REGS ;

: ENC-AND-L ( gpr gpr gpr -- instruction )
   123 L-REGS ;

: ENC-OR-L ( gpr gpr gpr -- instruction )
   127 L-REGS ;

: ENC-XOR-L ( gpr gpr gpr -- instruction )
   111 L-REGS ;

: ENC-CMPEQ-L ( gpr gpr gpr -- instruction )
   83 L-REGS ;

: ENC-MV-L ( gpr gpr -- instruction )
   0 126 L-CONST ;

\ Byte offsets select +/- ucst5 words without updating the base register.
: ENC-LDW ( gpr gpr byte-offset -- instruction )
   $64 MEMORY-WORD ;

: ENC-STW ( gpr gpr byte-offset -- instruction )
   $74 MEMORY-WORD ;

: ENC-B-REG ( gpr -- instruction ) {: target:gpr :}
   target REG-NUMBER 18 lshift $362 or
   target REG-SIDE 1 xor 12 lshift or >INSTRUCTION ;

\ Branch offsets are bytes from the containing 32-byte fetch packet's start.
: ENC-B-REL ( side branch-offset -- instruction )
   {: unit:side delta:branch-offset :}
   delta BRANCH-OFFSET>N -4194304 4194300 RANGE {: bytes:n :}
   bytes 4 mod 0 <> if E-OPERAND throw then
   bytes 4 / $1FFFFF and 7 lshift $10 or
   unit SIDE>N 0 1 RANGE 1 lshift or >INSTRUCTION ;

: ENC-NOP ( n -- instruction )
   1 9 RANGE 1 - 13 lshift >INSTRUCTION ;

\ Only A0..A2/B0..B2 can predicate these forms; NOP has no predicate field.
: WHEN-NONZERO ( instruction gpr -- instruction )
   0 0 <> PREDICATED ;

: WHEN-ZERO ( instruction gpr -- instruction )
   0 0= PREDICATED ;

\ The p bit joins the following instruction; packet legality belongs to scheduling.
: PARALLEL-NEXT ( instruction -- instruction )
   CHECK-WORD 1 or >INSTRUCTION ;

;package
