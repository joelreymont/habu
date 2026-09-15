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
DEFTYPE PREDICATE

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

\ ---- predication as a parameter ----------------------------------------------------------
\ The constructors below take a predicate last: ALWAYS, or a guard register
\ (A0..A2, B0..B2) tested for nonzero or zero.

: ALWAYS ( -- predicate ) 0 >PREDICATE ;
: IF-NONZERO ( gpr -- predicate ) CONDITION-FIELD 1 lshift >PREDICATE ;
: IF-ZERO ( gpr -- predicate ) CONDITION-FIELD 1 lshift 1 or >PREDICATE ;

private

: GUARD ( instruction predicate -- instruction ) {: opcode:instruction pred:predicate :}
   pred PREDICATE>N 0= if opcode exit then
   opcode CHECK-WORD $0FFFFFFF and pred PREDICATE>N 28 lshift or >INSTRUCTION ;

\ .L two-operand forms leave the src1 field zero; src2 may cross.
: L-UNARY ( gpr gpr n -- instruction ) {: dst:gpr src:gpr opcode:n :}
   dst src opcode L-BASE >INSTRUCTION ;

\ .S three-operand forms: src2 may cross, src1 sits on the unit's side.
: S-BASE ( gpr gpr n -- n ) {: dst:gpr src2:gpr opcode:n :}
   dst DST-FIELD src2 REG-NUMBER 18 lshift or dst src2 CROSS-BIT or opcode 6 lshift or $20 or ;

: S-REGS ( gpr gpr gpr n -- instruction ) {: dst:gpr src2:gpr count:gpr opcode:n :}
   dst count SAME-SIDE
   dst src2 opcode S-BASE count REG-NUMBER 13 lshift or >INSTRUCTION ;

: S-CONST ( gpr gpr n n -- instruction ) {: dst:gpr src2:gpr value:n opcode:n :}
   value 0 31 RANGE 13 lshift dst src2 opcode S-BASE or >INSTRUCTION ;

\ .S bit-field forms with constant positions have no cross path.
: S-FIELD ( gpr gpr n n n -- instruction ) {: dst:gpr src2:gpr csta:n cstb:n kind:n :}
   dst src2 SAME-SIDE
   dst DST-FIELD src2 REG-NUMBER 18 lshift or csta 0 31 RANGE 13 lshift or
   cstb 0 31 RANGE 8 lshift or kind 6 lshift or $08 or >INSTRUCTION ;

\ .D address arithmetic keeps every register on the unit's side.
: D-BASE ( gpr gpr n -- n ) {: dst:gpr base:gpr opcode:n :}
   dst base SAME-SIDE
   dst DST-FIELD base REG-NUMBER 18 lshift or opcode 7 lshift or $40 or ;

: D-REGS ( gpr gpr gpr n -- instruction ) {: dst:gpr base:gpr offset:gpr opcode:n :}
   dst offset SAME-SIDE
   dst base opcode D-BASE offset REG-NUMBER 13 lshift or >INSTRUCTION ;

: D-CONST ( gpr gpr n n -- instruction ) {: dst:gpr base:gpr value:n opcode:n :}
   value 0 31 RANGE 13 lshift dst base opcode D-BASE or >INSTRUCTION ;

\ Loads and stores: the byte offset is scaled to the access size; mode 0 adds
\ it to the base, mode 10 modifies the base afterwards; the sign of the
\ offset selects the negative or positive form of either.
: MEMORY-ACCESS ( gpr gpr byte-offset n n n -- instruction )
   {: data:gpr base:gpr offset:byte-offset scale:n mode:n opcode:n :}
   offset BYTE-OFFSET>N {: bytes:n :}
   bytes scale mod 0 <> if E-OPERAND throw then
   bytes abs scale / 0 31 RANGE {: count:n :}
   data DST-FIELD base REG-NUMBER 18 lshift or base REG-SIDE 7 lshift or count 13 lshift or
   mode bytes 0 >= if 1 else 0 then or 9 lshift or opcode or >INSTRUCTION ;

: PAIR ( gpr -- gpr ) dup REG-NUMBER 1 and 0 <> if E-OPERAND throw then ;

\ Doubleword access names the even register of the pair.
: PAIR-ACCESS ( gpr gpr byte-offset n n n -- instruction )
   {: data:gpr base:gpr offset:byte-offset scale:n mode:n opcode:n :}
   data PAIR base offset scale mode opcode MEMORY-ACCESS ;

public

: ENC-SUBC-L ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 75 L-REGS pred GUARD ;
: ENC-CMPGT-L ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 71 L-REGS pred GUARD ;
: ENC-CMPGTU-L ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 79 L-REGS pred GUARD ;
: ENC-CMPLT-L ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 87 L-REGS pred GUARD ;
: ENC-CMPLTU-L ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 95 L-REGS pred GUARD ;

\ Signed compares against a 5-bit constant, unsigned against a 4-bit one.
: ENC-CMPGT-I5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 70 L-CONST pred GUARD ;
: ENC-CMPLT-I5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 86 L-CONST pred GUARD ;
: ENC-CMPGTU-U4 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 0 15 RANGE 78 L-CONST pred GUARD ;
: ENC-CMPLTU-U4 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 0 15 RANGE 94 L-CONST pred GUARD ;

: ENC-ABS-L ( gpr gpr predicate -- instruction ) {: pred:predicate :} 26 L-UNARY pred GUARD ;
: ENC-NORM-L ( gpr gpr predicate -- instruction ) {: pred:predicate :} 99 L-UNARY pred GUARD ;

\ Shifts: dst, the value, then the count as a register or a 5-bit constant.
: ENC-SHL-S ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 51 S-REGS pred GUARD ;
: ENC-SHR-S ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 55 S-REGS pred GUARD ;
: ENC-SHRU-S ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 39 S-REGS pred GUARD ;
: ENC-SHL-U5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 50 S-CONST pred GUARD ;
: ENC-SHR-U5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 54 S-CONST pred GUARD ;
: ENC-SHRU-U5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 38 S-CONST pred GUARD ;

\ Bit fields: dst, src2, csta (leftmost shift), cstb (rightmost shift).
: ENC-EXTU-S ( gpr gpr n n predicate -- instruction ) {: pred:predicate :} 0 S-FIELD pred GUARD ;
: ENC-EXT-S ( gpr gpr n n predicate -- instruction ) {: pred:predicate :} 1 S-FIELD pred GUARD ;
: ENC-SET-S ( gpr gpr n n predicate -- instruction ) {: pred:predicate :} 2 S-FIELD pred GUARD ;
: ENC-CLR-S ( gpr gpr n n predicate -- instruction ) {: pred:predicate :} 3 S-FIELD pred GUARD ;

\ Byte and doubleword access: data, base, a byte offset (+/-31 bytes, or
\ +/-248 in multiples of 8), with ++ forms modifying the base afterwards.
: ENC-LDB ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 0 $24 MEMORY-ACCESS pred GUARD ;
: ENC-LDBU ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 0 $14 MEMORY-ACCESS pred GUARD ;
: ENC-STB ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 0 $34 MEMORY-ACCESS pred GUARD ;
: ENC-LDB++ ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 10 $24 MEMORY-ACCESS pred GUARD ;
: ENC-LDBU++ ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 10 $14 MEMORY-ACCESS pred GUARD ;
: ENC-STB++ ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 1 10 $34 MEMORY-ACCESS pred GUARD ;
: ENC-LDDW ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 8 0 $164 PAIR-ACCESS pred GUARD ;
: ENC-STDW ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 8 0 $144 PAIR-ACCESS pred GUARD ;
: ENC-LDDW++ ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 8 10 $164 PAIR-ACCESS pred GUARD ;
: ENC-STDW++ ( gpr gpr byte-offset predicate -- instruction ) {: pred:predicate :} 8 10 $144 PAIR-ACCESS pred GUARD ;

\ Address arithmetic: dst, base, then a register or 5-bit constant offset.
: ENC-ADDAB-D ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 48 D-REGS pred GUARD ;
: ENC-ADDAW-D ( gpr gpr gpr predicate -- instruction ) {: pred:predicate :} 56 D-REGS pred GUARD ;
: ENC-ADDAB-U5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 50 D-CONST pred GUARD ;
: ENC-ADDAW-U5 ( gpr gpr n predicate -- instruction ) {: pred:predicate :} 58 D-CONST pred GUARD ;

;package
