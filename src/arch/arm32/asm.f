\ Pure ARMv7-R A32 and ARMv7E-M Thumb instruction construction.
require lib/type/deftype.f
require lib/errors.f

package A32ASM
public

DEFTYPE GPR
DEFTYPE CONDITION
DEFTYPE BRANCH-OFFSET
DEFTYPE ARM-INSTRUCTION
DEFTYPE THUMB16-INSTRUCTION
DEFTYPE THUMB32-INSTRUCTION

E-A32ASM-OPERAND constant E-OPERAND

private

: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then
   value ;

: DATA-REG ( gpr -- n )
   GPR>N 0 14 RANGE ;

: LOW-REG ( gpr -- n )
   GPR>N 0 7 RANGE ;

: THUMB-MOV-REG ( gpr -- n )
   DATA-REG dup 13 = if E-OPERAND throw then ;

: IMM8 ( n -- n )
   0 $FF RANGE ;

: IMM16 ( n -- n )
   0 $FFFF RANGE ;

: ALIGNED ( n n -- n ) {: value:n alignment:n :}
   value alignment mod 0 <> if E-OPERAND throw then
   value ;

: ARM-RRR ( gpr gpr gpr n -- arm-instruction )
   {: dst:gpr left:gpr right:gpr opcode:n :}
   opcode dst DATA-REG 12 lshift or
   left DATA-REG 16 lshift or right DATA-REG or >ARM-INSTRUCTION ;

: ARM-RR ( gpr gpr n -- arm-instruction )
   {: dst:gpr source:gpr opcode:n :}
   opcode dst DATA-REG 12 lshift or source DATA-REG or >ARM-INSTRUCTION ;

: ARM-HALF ( gpr n n -- arm-instruction )
   {: dst:gpr immediate:n opcode:n :}
   immediate IMM16 {: bits:n :}
   opcode dst DATA-REG 12 lshift or
   bits $FFF and or bits 12 rshift 16 lshift or >ARM-INSTRUCTION ;

: ARM-IMM8 ( gpr gpr n n -- arm-instruction )
   {: dst:gpr source:gpr immediate:n opcode:n :}
   opcode dst DATA-REG 12 lshift or source DATA-REG 16 lshift or
   immediate IMM8 or >ARM-INSTRUCTION ;

: ARM-MEM ( gpr gpr n n -- arm-instruction )
   {: data:gpr base:gpr offset:n opcode:n :}
   offset -4095 4095 RANGE drop
   opcode data DATA-REG 12 lshift or base DATA-REG 16 lshift or
   offset 0 >= if $800000 or then offset abs or >ARM-INSTRUCTION ;

: ARM-BRANCH ( condition branch-offset n -- arm-instruction )
   {: cond:condition delta:branch-offset opcode:n :}
   delta BRANCH-OFFSET>N -33554432 33554428 RANGE 4 ALIGNED
   4 / $FFFFFF and opcode or
   cond CONDITION>N 0 14 RANGE 28 lshift or >ARM-INSTRUCTION ;

: THUMB-RRR ( gpr gpr gpr n -- thumb16-instruction )
   {: dst:gpr left:gpr right:gpr opcode:n :}
   opcode dst LOW-REG or left LOW-REG 3 lshift or
   right LOW-REG 6 lshift or >THUMB16-INSTRUCTION ;

: THUMB-MOV-HIGH ( n n -- n ) {: immediate:n opcode:n :}
   opcode immediate 12 rshift $F and or
   immediate 11 rshift 1 and 10 lshift or ;

: THUMB-MOV-LOW ( gpr n -- n ) {: dst:gpr immediate:n :}
   dst THUMB-MOV-REG 8 lshift immediate $FF and or
   immediate 8 rshift 7 and 12 lshift or ;

: THUMB-HALF ( gpr n n -- thumb32-instruction )
   {: dst:gpr immediate:n opcode:n :}
   immediate IMM16 opcode THUMB-MOV-HIGH 16 lshift
   dst immediate THUMB-MOV-LOW or >THUMB32-INSTRUCTION ;

: THUMB-MEM ( gpr gpr n n -- thumb16-instruction )
   {: data:gpr base:gpr offset:n opcode:n :}
   offset 0 124 RANGE 4 ALIGNED 4 / 6 lshift opcode or
   data LOW-REG or base LOW-REG 3 lshift or >THUMB16-INSTRUCTION ;

: THUMB-MEM-WIDE ( gpr gpr n n -- thumb32-instruction )
   {: data:gpr base:gpr offset:n opcode:n :}
   opcode data DATA-REG 12 lshift or base DATA-REG 16 lshift or
   offset 0 4095 RANGE or >THUMB32-INSTRUCTION ;

: THUMB-BRANCH-J ( n n -- n ) {: delta:n bit:n :}
   delta bit rshift delta 24 rshift xor 1 xor 1 and ;

: THUMB-BRANCH-HIGH ( n -- n ) {: delta:n :}
   $F000 delta 24 rshift 1 and 10 lshift or
   delta 12 rshift $3FF and or ;

: THUMB-BRANCH-LOW ( n n -- n ) {: delta:n opcode:n :}
   opcode delta 23 THUMB-BRANCH-J 13 lshift or
   delta 22 THUMB-BRANCH-J 11 lshift or
   delta 1 rshift $7FF and or ;

: THUMB-BRANCH-WIDE ( branch-offset n -- thumb32-instruction )
   {: delta:branch-offset opcode:n :}
   delta BRANCH-OFFSET>N -16777216 16777214 RANGE 2 ALIGNED {: bytes:n :}
   bytes THUMB-BRANCH-HIGH 16 lshift
   bytes opcode THUMB-BRANCH-LOW or >THUMB32-INSTRUCTION ;

public

: AL ( -- condition )
   14 >CONDITION ;

: ARM-MOV ( gpr gpr -- arm-instruction )
   $E1A00000 ARM-RR ;

: ARM-MOV-I8 ( gpr n -- arm-instruction ) {: dst:gpr immediate:n :}
   dst 0 >GPR immediate $E3A00000 ARM-IMM8 ;

: ARM-MOVW ( gpr n -- arm-instruction )
   $E3000000 ARM-HALF ;

: ARM-MOVT ( gpr n -- arm-instruction )
   $E3400000 ARM-HALF ;

: ARM-ADD ( gpr gpr gpr -- arm-instruction )
   $E0800000 ARM-RRR ;

: ARM-SUB ( gpr gpr gpr -- arm-instruction )
   $E0400000 ARM-RRR ;

: ARM-AND ( gpr gpr gpr -- arm-instruction )
   $E0000000 ARM-RRR ;

: ARM-ORR ( gpr gpr gpr -- arm-instruction )
   $E1800000 ARM-RRR ;

: ARM-EOR ( gpr gpr gpr -- arm-instruction )
   $E0200000 ARM-RRR ;

: ARM-ADD-I8 ( gpr gpr n -- arm-instruction )
   $E2800000 ARM-IMM8 ;

: ARM-SUB-I8 ( gpr gpr n -- arm-instruction )
   $E2400000 ARM-IMM8 ;

: ARM-CMP ( gpr gpr -- arm-instruction ) {: left:gpr right:gpr :}
   0 >GPR left right $E1500000 ARM-RRR ;

: ARM-LDR ( gpr gpr n -- arm-instruction )
   $E5100000 ARM-MEM ;

: ARM-STR ( gpr gpr n -- arm-instruction )
   $E5000000 ARM-MEM ;

: ARM-LDRB ( gpr gpr n -- arm-instruction )
   $E5500000 ARM-MEM ;

: ARM-STRB ( gpr gpr n -- arm-instruction )
   $E5400000 ARM-MEM ;

: ARM-BX ( gpr -- arm-instruction )
   DATA-REG $E12FFF10 or >ARM-INSTRUCTION ;

\ ARM branch offsets are relative to instruction address + 8, in bytes.
: ARM-B ( condition branch-offset -- arm-instruction )
   $0A000000 ARM-BRANCH ;

: ARM-BL ( condition branch-offset -- arm-instruction )
   $0B000000 ARM-BRANCH ;

: THUMB-MOVS ( gpr n -- thumb16-instruction ) {: dst:gpr immediate:n :}
   dst LOW-REG 8 lshift $2000 or immediate IMM8 or >THUMB16-INSTRUCTION ;

: THUMB-MOVW ( gpr n -- thumb32-instruction )
   $F240 THUMB-HALF ;

: THUMB-MOVT ( gpr n -- thumb32-instruction )
   $F2C0 THUMB-HALF ;

: THUMB-ADDS ( gpr gpr gpr -- thumb16-instruction )
   $1800 THUMB-RRR ;

: THUMB-SUBS ( gpr gpr gpr -- thumb16-instruction )
   $1A00 THUMB-RRR ;

: THUMB-CMP ( gpr gpr -- thumb16-instruction ) {: left:gpr right:gpr :}
   left LOW-REG right LOW-REG 3 lshift or $4280 or >THUMB16-INSTRUCTION ;

: THUMB-LDR ( gpr gpr n -- thumb16-instruction )
   $6800 THUMB-MEM ;

: THUMB-STR ( gpr gpr n -- thumb16-instruction )
   $6000 THUMB-MEM ;

: THUMB-LDR-WIDE ( gpr gpr n -- thumb32-instruction )
   $F8D00000 THUMB-MEM-WIDE ;

: THUMB-STR-WIDE ( gpr gpr n -- thumb32-instruction )
   $F8C00000 THUMB-MEM-WIDE ;

: THUMB-BX ( gpr -- thumb16-instruction )
   DATA-REG 3 lshift $4700 or >THUMB16-INSTRUCTION ;

\ Thumb offsets use instruction address + 4. Wide values hold first halfword high.
: THUMB-B-SHORT ( branch-offset -- thumb16-instruction )
   BRANCH-OFFSET>N -2048 2046 RANGE 2 ALIGNED
   2 / $7FF and $E000 or >THUMB16-INSTRUCTION ;

: THUMB-B-COND ( condition branch-offset -- thumb16-instruction )
   {: cond:condition delta:branch-offset :}
   delta BRANCH-OFFSET>N -256 254 RANGE 2 ALIGNED 2 / $FF and $D000 or
   cond CONDITION>N 0 13 RANGE 8 lshift or >THUMB16-INSTRUCTION ;

: THUMB-B-WIDE ( branch-offset -- thumb32-instruction )
   $9000 THUMB-BRANCH-WIDE ;

: THUMB-BL ( branch-offset -- thumb32-instruction )
   $D000 THUMB-BRANCH-WIDE ;

;package
