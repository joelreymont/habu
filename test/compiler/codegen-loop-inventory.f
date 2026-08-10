\ codegen-loop-inventory.f - the hoisting lane's measurement, and the ways a tool
\ that counts LOOPS can be wrong while looking right. One concern:
\ tools/codegen-loop-inventory.f.
\
\ WHY THIS TOOL NEEDS ITS OWN SUITE. Every wrong answer it can give is silent. A
\ move-wide classifier that mistook an ordinary arithmetic word for a literal
\ half would report hoisting opportunities that are not there; a back-edge rule
\ that counted the wrong branches would report loops a routine does not have and
\ charge instructions to bodies they are not in. Neither shows up as a failure
\ anywhere - they show up as a number somebody then plans a compiler pass
\ against. So the cases below are built from the SHIPPED encoders, and the
\ structural ones are built from the routine that actually caught the bug.
\
\ THE CASE THAT MATTERS MOST IS THE BACKWARD BRANCH THAT IS NOT A LOOP.
\ CODEGEN-CORPUS:BYTE-FIND returns -1 when its scan finds nothing. That -1 is a
\ four-instruction literal chain, the elaborator gives it a block of its own, and
\ the emitter lays that block out BEFORE the loop's exit stub branches down to
\ it. So the routine really does hold an unconditional branch to a LOWER address
\ which is an ordinary forward edge in the control-flow graph. The first version
\ of this tool called every backward branch a back edge, reported BYTE-FIND as
\ holding two loops, and charged that -1 chain to a loop body that never executes
\ it - nine invariant constants where the row has one. Both numbers are asserted
\ below, so a rule that goes back to reading the layout fails here rather than in
\ a report.
\
\ AND THE BODY IS ASSERTED SEPARATELY FROM THE SPAN. The emitter lays a loop's
\ blocks out contiguously but may lay other blocks among them, so the span
\ between a header and its back edge is not the loop. BYTE-FIND's body is
\ asserted at its cycle size rather than its span size; a tool that counted the
\ span would report a larger number and pass every other case in this file.
\
\ THE LAST GROUP IS THE REAL THING: routines the native chain really compiled,
\ counted through the same entry the report uses. BIG-CONSTS-N is the row the
\ whole measurement turns on - four sixty-four-bit literals rebuilt every turn -
\ and the rows with no loop at all are asserted too, because a tool that found
\ loops in straight-line code would be worse than one that found none.

require lib/test.f
require lib/prelude.f
require src/arch/arm64/asm.f
require src/compiler/native/branch.f
require tools/codegen-loop-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NLOOPINV-TEST
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM

using NLOOPINV

private

\ The registers the fixtures below name. x18 is what no emitted routine may hold
\ and the classifiers screen it, so no fixture is built from it.
0 constant R0
1 constant R1
2 constant R2
19 constant RBASE
31 constant RZERO

\ The four quarters a move-wide immediate can land in.
0 constant HW0
1 constant HW1

public

: FORM-CASES ( -- )
   s" each shipped move-wide encoder's own output is recognised as its form" T-LABEL
   R0 $1234 HW0 MOVZHW MOVZ? TTRUE
   R0 $1234 HW1 MOVZHW MOVZ? TTRUE
   R0 $1234 HW0 MOVKHW MOVK? TTRUE
   R0 $1234 HW1 MOVKHW MOVK? TTRUE

   s" and neither move-wide form is the other" T-LABEL
   R0 $1234 HW0 MOVZHW MOVK? TFALSE
   R0 $1234 HW0 MOVKHW MOVZ? TFALSE

   s" both answer the either-half question" T-LABEL
   R0 $1234 HW0 MOVZHW MOVE-WIDE? TTRUE
   R0 $1234 HW0 MOVKHW MOVE-WIDE? TTRUE

   s" and nothing else does" T-LABEL
   R0 R1 R2 ENC-ADD MOVE-WIDE? TFALSE
   R0 R1 R2 ENC-SUB MOVE-WIDE? TFALSE
   R0 R1 R2 ENC-MUL MOVE-WIDE? TFALSE
   R0 R1 8 ENC-ADDI MOVE-WIDE? TFALSE
   R0 RBASE 8 ENC-LDR MOVE-WIDE? TFALSE
   R0 RBASE 8 ENC-STR MOVE-WIDE? TFALSE
   R0 R1 ENC-MOV MOVE-WIDE? TFALSE ;

private

\ The displacement arithmetic, exercised through the encoders that write it. A
\ branch built to reach a target must read back as reaching exactly that target,
\ forwards and backwards, which is the property every walk over emitted code
\ rests on.
: COND-ROUNDTRIP ( n n -- ) {: at:n target:n :}
   target at - NBR:INSN-BYTES / {: d:n :}
   d 0 ENC-BCOND {: w:n :}
   w NBR:BCOND? TTRUE
   w NBR:COND? TTRUE
   at w NBR:COND-TARGET target T= ;

public

: COND-CASES ( -- )
   s" a conditional branch reads back as reaching the target it was built for" T-LABEL
   $1000 $1040 COND-ROUNDTRIP
   $1000 $1000 COND-ROUNDTRIP

   s" including one that points backwards" T-LABEL
   $1000 $0FC0 COND-ROUNDTRIP

   s" the compare-and-branch pair are each themselves and not the other" T-LABEL
   R0 4 ENC-CBZ NBR:CBZ? TTRUE
   R0 4 ENC-CBZ NBR:CBNZ? TFALSE
   R0 4 ENC-CBNZ NBR:CBNZ? TTRUE
   R0 4 ENC-CBNZ NBR:CBZ? TFALSE
   R0 4 ENC-CBZ NBR:COND? TTRUE
   R0 4 ENC-CBNZ NBR:COND? TTRUE

   s" and an unconditional branch is not a conditional one" T-LABEL
   4 ENC-B NBR:COND? TFALSE
   4 ENC-B NBR:B? TTRUE
   4 ENC-BL NBR:COND? TFALSE
   4 ENC-BL NBR:BL? TTRUE

   s" a return is a return and no kind of branch" T-LABEL
   $D65F03C0 NBR:RET? TTRUE
   $D65F03C0 NBR:B? TFALSE
   $D65F03C0 NBR:COND? TFALSE
   4 ENC-B NBR:RET? TFALSE ;

\ ---- the routines the chain really compiled ---------------------------------

: EMITTED-CASES ( -- )
   s" straight-line rows hold no loop, so no body and nothing to hoist" T-LABEL
   s" CODEGEN-CORPUS:ADD3-N" ROW!
   LOOPS 0 T=
   BODY-TOTAL 0 T=
   CONSTS-TOTAL 0 T=
   s" CODEGEN-CORPUS4:LADDER-N" ROW!
   LOOPS 0 T=
   BODY-TOTAL 0 T=

   s" the eight-deep guard ladder branches a great deal and loops not at all" T-LABEL
   s" CODEGEN-CORPUS4:LADDER-N" ROW!
   CONSTS-TOTAL 0 T=

   s" a recursive row is a call, not a loop" T-LABEL
   s" CODEGEN-CORPUS:FACT-N" ROW!
   LOOPS 0 T=

   \ The case the tool was rewritten for. The -1 block is laid out inside the
   \ loop's span and is reached only on the way out, so it is neither a second
   \ loop nor part of the body. A rule that read the layout answers 2 and 9 here.
   s" the row whose return block sits inside the loop span holds ONE loop" T-LABEL
   s" CODEGEN-CORPUS:BYTE-FIND-N" ROW!
   LOOPS 1 T=

   \ SEVEN AND NOT EIGHT SINCE THE CONSTANTS MOVED INTO THE INSTRUCTIONS. This
   \ tool reads the machine code the chain finally emits, and the combine pass
   \ now folds a small constant into the add or subtract that reads it, so the
   \ move-wide that used to stand in this body is not there to count. The body
   \ is one instruction shorter and holds no move-wide at all.
   \ AND ONE SHORTER AGAIN SINCE THE LATCH WENT. The emitter's collapse branches
   \ past a block that emits nothing before its terminator, which is exactly a
   \ counted loop's latch, so the conditional at the bottom of the body now names
   \ the header itself and IS the back edge. The back edge is not part of the
   \ body - it is the turn, not the work - so the instruction that used to be the
   \ body's last is now excluded, and every counted loop here reads one less.
   s" and that block's literal chain is not charged to the loop body" T-LABEL
   s" CODEGEN-CORPUS:BYTE-FIND-N" ROW!
   BODY-TOTAL 6 T=
   CONSTS-TOTAL 0 T=

   s" the counted byte loops hold one loop each" T-LABEL
   s" CODEGEN-CORPUS:BYTE-SUM-N" ROW!
   LOOPS 1 T=
   BODY-TOTAL 5 T=
   s" CODEGEN-CORPUS4:STORE-LOAD-N" ROW!
   LOOPS 1 T=
   BODY-TOTAL 5 T=

   \ THE SECOND SUBJECT HERE USED TO BE SUM-TO, whose body was three instructions,
   \ and it is now a routine with no loop in it at all: src/compiler/native/loop.f
   \ answers the sum of a counted loop's indices instead of running the loop. The
   \ row moved to the stepping loop, which writes memory and is therefore declined
   \ by that pass, and what SUM-TO reads now is measured on its own below - a
   \ subject that stopped having the shape a row is named for has to be re-aimed
   \ and re-stated, not quietly left to answer something else.
   s" and the loop the chain answers instead of running holds none" T-LABEL
   s" CODEGEN-CORPUS:SUM-TO-N" ROW!
   LOOPS 0 T=
   BODY-TOTAL 0 T=

   \ The row the measurement turns on: four sixty-four-bit literals, each a chain
   \ of four move-wide instructions, rebuilt on every turn. Sixteen of its
   \ seventeen in-body move-wides were chain members and the seventeenth was a
   \ small constant an immediate form could carry - which is now where it is, so
   \ sixteen chain members are what is left. That the count fell by exactly one
   \ is the point of the row: a wide literal is NOT foldable, and only the one
   \ small constant moved.
   s" the four-literal row rebuilds sixteen chain instructions a turn" T-LABEL
   s" CODEGEN-CORPUS4:BIG-CONSTS-N" ROW!
   LOOPS 1 T=
   BODY-TOTAL 26 T=
   CONSTS-TOTAL 16 T=
   FOLDABLE-TOTAL 0 T=

   \ THE CLASSIFIER STILL HAS TO ANSWER YES SOMEWHERE, or nothing tests it. What
   \ is left foldable-SHAPED after the fold is the constant whose reader is not
   \ an add or a subtract: this row's loop test compares against zero, and a
   \ comparison has no immediate form in this dialect, so the zero is still a
   \ move-wide small enough for an arithmetic field and the column still counts
   \ it. That is the residue the fold deliberately leaves, named here so it is
   \ measured rather than assumed.
   s" a small constant a comparison reads is still counted foldable" T-LABEL
   s" CODEGEN-CORPUS:COUNT-DOWN-N" ROW!
   CONSTS-TOTAL 1 T=
   FOLDABLE-TOTAL 1 T=

   s" no loop of this corpus holds a load the body leaves invariant" T-LABEL
   s" CODEGEN-CORPUS:BYTE-SUM-N" ROW!
   INV-LOADS-TOTAL 0 T=
   s" CODEGEN-CORPUS3:T-SUM-N" ROW!
   INV-LOADS-TOTAL 0 T=
   s" CODEGEN-CORPUS4:BIG-CONSTS-N" ROW!
   INV-LOADS-TOTAL 0 T=

   s" a body that stores is refused a hoistable load without asking further" T-LABEL
   s" CODEGEN-CORPUS4:STORE-LOAD-N" ROW!
   LOOPS 1 T=
   INV-LOADS-TOTAL 0 T=

   s" a name nothing published is a refusal and not a quiet zero" T-LABEL
   [: s" CODEGEN-CORPUS:NO-SUCH-WORD-N" ROW! LOOPS drop ;]
      E-CODEGEN-COMPARE-SUBJECT TTHROWSQ ;

: CASES ( -- )
   FORM-CASES
   COND-CASES
   EMITTED-CASES ;

;using
;using
;package

T-RESET
NLOOPINV-TEST:CASES
T-REPORT
