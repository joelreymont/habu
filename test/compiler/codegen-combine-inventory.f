\ codegen-combine-inventory.f - the combining lane's measurement, and the ways a
\ tool that counts instruction PAIRS can be wrong while looking right. One
\ concern: tools/codegen-combine-inventory.f.
\
\ WHY A CLASSIFIER HERE NEEDS ITS OWN SUITE. The tool decides what an emitted
\ four-byte word IS, and every wrong answer is silent: a decoder that read a
\ multiply-add as a multiply would report fusion opportunities that are already
\ fused, and one that read a byte load as an eight-byte load would report pairs
\ no pairing instruction could carry. Neither shows up as a failure anywhere -
\ they show up as a number somebody then plans work against. So the cases below
\ are built from the SHIPPED encoders and include the words most likely to be
\ mistaken for the ones being counted.
\
\ THE FIXTURE THAT MATTERS MOST IS THE MULTIPLY ALIAS. MUL is not an instruction
\ of its own on this machine: it is MADD with the zero register as its addend,
\ and the two are THE SAME WORD. formal/Common/Insn.v carries that fact as
\ `madd_mul_alias_at_xzr` and puts a Madd naming register 31 outside `wf` for it.
\ A classifier has to agree in both directions - the aliased word IS a multiply,
\ and a real multiply-add with a real addend is NOT one - or a combining pass
\ built on its counts would fuse instructions that are already fused. Both
\ directions are asserted below, and the equality of the two encodings is
\ asserted beside them so the case cannot quietly stop being about an alias.
\
\ AND THE PAIR PREDICATES ARE PINNED IN BOTH DIRECTIONS TOO. For each pattern
\ there is a pair that matches and a near-miss that must not: a dependence
\ through the wrong operand position, an addition that leaves the product live,
\ two loads one slot apart through DIFFERENT bases, two loads through the same
\ base that are two slots apart, and the load that overwrites the base the next
\ load reads. A predicate that answered true on shape alone passes none of them.
\
\ THE LAST GROUP IS THE REAL THING. Four routines the native chain really
\ compiled, counted through the same entry the report uses. SQUARE-SUM-N is the
\ shape the pattern exists for; ADD3-N has the additions and none of the
\ multiplies; C-MAD-N has BOTH a multiply and an addition and still counts zero,
\ because the register allocator put the addend in the register the multiply read
\ - which is the measurement that says where the combining pass has to run, and
\ would be lost if the suite only asserted totals.

require lib/test.f
require lib/prelude.f
require src/arch/arm64/asm.f
require tools/codegen-combine-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated4.f

package NCOMBINV-TEST

using NCOMBINV

private

\ The registers the fixtures below name. They are ordinary allocatable ones:
\ x18 is what no emitted routine may hold, and the classifiers screen it, so no
\ fixture may be built from it.
0 constant R0
1 constant R1
2 constant R2
3 constant R3
19 constant RBASE                  \ the data-stack pointer, which is what the
                                   \ chain's frame loads really go through
31 constant RZERO                  \ the zero register: MUL's addend field

public

: FORM-CASES ( -- )
   s" each shipped encoder's own output is recognised as its form" T-LABEL
   R0 R1 R2 ENC-MUL MUL? TTRUE
   R0 R1 R2 ENC-ADD ADD? TTRUE
   R0 R1 R2 ENC-SUB SUB? TTRUE
   R0 RBASE 8 ENC-LDR LDR? TTRUE
   R0 RBASE 8 ENC-STR STR? TTRUE

   s" and is not recognised as any of the others" T-LABEL
   R0 R1 R2 ENC-MUL ADD? TFALSE
   R0 R1 R2 ENC-MUL SUB? TFALSE
   R0 R1 R2 ENC-ADD MUL? TFALSE
   R0 R1 R2 ENC-SUB ADD? TFALSE
   R0 RBASE 8 ENC-LDR STR? TFALSE
   R0 RBASE 8 ENC-STR LDR? TFALSE

   s" a multiply-add whose addend is the zero register IS the multiply" T-LABEL
   R0 R1 R2 RZERO ENC-MADD  R0 R1 R2 ENC-MUL  T=
   R0 R1 R2 RZERO ENC-MADD MUL? TTRUE

   s" but a multiply-add with a real addend is not a multiply" T-LABEL
   R0 R1 R2 R3 ENC-MADD MUL? TFALSE
   R0 R1 R2 R3 ENC-MADD ADD? TFALSE
   R0 R1 R2 R3 ENC-MSUB MUL? TFALSE
   R0 R1 R2 R3 ENC-MSUB SUB? TFALSE

   s" the immediate forms are not the shifted-register ones" T-LABEL
   R0 R1 8 ENC-ADDI ADD? TFALSE
   R0 R1 8 ENC-SUBI SUB? TFALSE

   s" and the narrower accesses are not the eight-byte ones" T-LABEL
   R0 RBASE 8 ENC-LDRB LDR? TFALSE
   R0 RBASE 8 ENC-STRB STR? TFALSE
   R0 RBASE 8 ENC-LDRW LDR? TFALSE
   R0 RBASE 8 ENC-STRW STR? TFALSE

   s" a register move is an or, and is none of the counted forms" T-LABEL
   R0 R1 ENC-MOV ADD? TFALSE
   R0 R1 ENC-MOV MUL? TFALSE ;

: MADD-CASES ( -- )
   s" a multiply whose product the next addition overwrites is a safe pair" T-LABEL
   R0 R0 R0 ENC-MUL  R0 R1 R0 ENC-ADD  MADD-PAIR? TTRUE
   R0 R0 R0 ENC-MUL  R0 R1 R0 ENC-ADD  MADD-SAFE? TTRUE

   s" the product may arrive in either operand of the addition" T-LABEL
   R0 R0 R0 ENC-MUL  R0 R0 R1 ENC-ADD  MADD-PAIR? TTRUE

   s" an addition that leaves the product live is a pair but not a safe one" T-LABEL
   R0 R0 R0 ENC-MUL  R2 R1 R0 ENC-ADD  MADD-PAIR? TTRUE
   R0 R0 R0 ENC-MUL  R2 R1 R0 ENC-ADD  MADD-SAFE? TFALSE

   s" an addition that does not read the product is no pair at all" T-LABEL
   R0 R0 R0 ENC-MUL  R2 R1 R3 ENC-ADD  MADD-PAIR? TFALSE

   s" and neither is an addition after something that is not a multiply" T-LABEL
   R0 R1 R2 ENC-ADD  R0 R1 R0 ENC-ADD  MADD-PAIR? TFALSE ;

: MSUB-CASES ( -- )
   s" a subtraction that takes the product AWAY from something is the form" T-LABEL
   R0 R0 R0 ENC-MUL  R0 R1 R0 ENC-SUB  MSUB-PAIR? TTRUE

   s" one that takes something away FROM the product is not" T-LABEL
   R0 R0 R0 ENC-MUL  R0 R0 R1 ENC-SUB  MSUB-PAIR? TFALSE

   s" and an addition is not a subtraction" T-LABEL
   R0 R0 R0 ENC-MUL  R0 R1 R0 ENC-ADD  MSUB-PAIR? TFALSE ;

: PAIR-CASES ( -- )
   s" two loads one slot apart through one base could be carried together" T-LABEL
   R1 RBASE 0 ENC-LDR  R2 RBASE 8 ENC-LDR  LDP-PAIR? TTRUE
   R1 RBASE 8 ENC-LDR  R2 RBASE 0 ENC-LDR  LDP-PAIR? TTRUE

   s" two slots apart is not one slot apart" T-LABEL
   R1 RBASE 0 ENC-LDR  R2 RBASE 16 ENC-LDR  LDP-PAIR? TFALSE

   s" a different base is a different address" T-LABEL
   R1 RBASE 0 ENC-LDR  R2 R3 8 ENC-LDR  LDP-PAIR? TFALSE

   s" one instruction cannot write one register twice" T-LABEL
   R1 RBASE 0 ENC-LDR  R1 RBASE 8 ENC-LDR  LDP-PAIR? TFALSE

   s" and a load that overwrites the base the next load reads may not move" T-LABEL
   RBASE RBASE 0 ENC-LDR  R2 RBASE 8 ENC-LDR  LDP-PAIR? TFALSE

   s" stores have no such restriction: they write no register" T-LABEL
   R1 RBASE 0 ENC-STR  R1 RBASE 8 ENC-STR  STP-PAIR? TTRUE

   s" a store and a load are not a pair of either kind" T-LABEL
   R1 RBASE 0 ENC-LDR  R2 RBASE 8 ENC-STR  LDP-PAIR? TFALSE
   R1 RBASE 0 ENC-STR  R2 RBASE 8 ENC-LDR  STP-PAIR? TFALSE ;

: EMITTED-CASES ( -- )
   s" the row the pattern exists for holds one safe pair" T-LABEL
   s" CODEGEN-CORPUS:SQUARE-SUM-N" ROW!
   MADDS 1 T=
   MADD-SAFES 1 T=

   s" a row with additions and no multiply holds none" T-LABEL
   s" CODEGEN-CORPUS:ADD3-N" ROW!
   MULS 0 T=
   ADDS 2 T=
   MADDS 0 T=

   s" the row with BOTH a multiply and an addition still holds none" T-LABEL
   s" CODEGEN-CORPUS4:C-MAD-N" ROW!
   MULS 1 T=
   ADDS 1 T=
   MADDS 0 T=

   s" the five-call row holds four" T-LABEL
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" ROW!
   MADDS 4 T=
   MADD-SAFES 4 T=

   s" no row of either corpus holds a subtracting pair" T-LABEL
   s" CODEGEN-CORPUS:SQUARE-SUM-N" ROW!
   MSUBS 0 T=
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" ROW!
   MSUBS 0 T=

   s" a name nothing published is a refusal and not a quiet zero" T-LABEL
   [: s" CODEGEN-CORPUS:NO-SUCH-WORD-N" ROW! MADDS drop ;]
      E-CODEGEN-COMPARE-SUBJECT TTHROWSQ ;

: CASES ( -- )
   FORM-CASES
   MADD-CASES
   MSUB-CASES
   PAIR-CASES
   EMITTED-CASES ;

;using

;package

T-RESET
NCOMBINV-TEST:CASES
T-REPORT
