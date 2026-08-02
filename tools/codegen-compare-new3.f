\ codegen-compare-new3.f - the new code generator's column of the THIRD
\ comparison. One concern: what the new chain can make of the float corpus, and
\ what it is waiting for where it cannot.
\
\ THREE COMPILED AND SEVEN STILL GAPS. The float corpus was measured and
\ committed BEFORE the chain had a single float capability, so that the day it
\ gained one there was a table to read the advance against that nobody chose
\ afterwards. Two rows arrived with the scalar float leaf: SGD and SEG-1/SQRT
\ are straight-line float arithmetic over a locals frame - the first is one
\ multiply and one subtract, the second is a conversion, a square root and a
\ division. MAX-F arrived with the comparison leaf, and it is the first row of
\ this corpus with control flow in it: a float compare feeding a two-armed
\ branch, which the chain fuses into one Fcmp and one conditional branch. The
\ other seven need a loop, a call, a memory access, or a double placed somewhere
\ a straight line does not reach, and each declares every one of them below.
\
\ WHAT `floats` MEANT, MEASURED RATHER THAN ASSUMED, AND WHAT IT MEANS NOW.
\ Before the leaf, handing a float body to the migration the way
\ tools/codegen-compare-migrated2.f hands it an integer one failed in two
\ different stages:
\
\     : FADD-N ( r r -- r ) f+ ;      threw -8286  E-HIR-UNMODELED
\     : FLIT-N ( r -- r ) 1.0 f+ ;    threw -8404  E-NFEED-KIND
\
\ The tape had no kind for a real literal and the dialect had no model for a
\ float operation. One capability covered both, because a row that named only
\ the tape would have read as half a job and a row that named only the dialect
\ would have been wrong about where the first refusal happens. Both are gone:
\ the tape carries a real literal's own cell and the dialect has a double type
\ with seventeen operations over it, five of them comparisons. No row waits for
\ `floats` any more.
\
\ WHAT THE REMAINING SEVEN WAIT FOR, MEASURED THE SAME WAY. Every one of them
\ carries a double somewhere a straight line does not reach, and the refusal is
\ one stage further on than the two above - the elaborator's, when a double
\ reaches a position whose type was fixed before the value that would arrive
\ there was known:
\
\     : NMG-BAD3 ( r n -- r ) 0 ?do 1.0 f+ loop ;    threw -8580  E-NELAB-TYPE
\     : RELU-N ( r -- r ) {: x :} x f0< if 0.0 else x then ;
\                                                    threw -8580  E-NELAB-TYPE
\     : NMG-BAD2 ( r ptr a -- ) {: v:r b:ptr :} v 1.0 f+ b ! ;
\                                                    threw -8580  E-NELAB-TYPE
\
\ That is what `float-place` names, and it is why RELU-F and FROUND are still
\ gaps while MAX-F is a row. All three are one float comparison feeding one
\ branch, and the comparison compiles in all three; what differs is what crosses
\ the join. MAX-F's arms hand over `x` and `y`, which arrive in data-stack cells
\ and cross as cells; RELU-F's hand over `0.0` and `x`, and FROUND's hand over
\ two computed doubles. Dots habu-carry-a-double-570d2f5c and
\ habu-store-a-double-a31b313e carry it.
\
\ EVERY GAP NAMES EVERY CAPABILITY ITS ROW NEEDS, not the first that stops it -
\ tools/codegen-compare-gap.f's rule, and it matters more here than anywhere:
\ a reader planning the float work should see that the accumulation rows also
\ want a loop and a locals frame and memory access, and that T-REL-L2 also wants
\ calls, so that the day a double can be placed nobody is surprised by which
\ rows still do not move.
\
\ WHAT IS NOT DONE HERE. No body is respelled to buy a row, and no float body is
\ handed to the chain in the hope that some part of it survives: a row that
\ cannot be compiled is a gap that names the capability, which is a result. The
\ three bodies that ARE compiled are the corpus's own to the byte - not one
\ constant is respelled, which is more than either of the first two corpora could
\ say - and tools/codegen-compare-migrated3.f publishes them. The calibration row
\ below is the first corpus's empty call, already published by
\ tools/codegen-compare-migrated.f, measured again in this pass because a cost
\ is a ratio to a call timed on the same host at the same moment.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus3.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated3.f

package CODEGEN-NEW3

private

\ The calibration row, which is the first corpus's empty call measured again in
\ this pass. It returns nothing, so it has no output to compare; what it
\ measures is the floor of a call on this path, which every other new row would
\ be divided by - and which the report still needs even in a pass whose new
\ column is nothing but this row, because NORMALIZE divides each path by its own
\ floor.
: NOOP-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: CODEGEN-CORPUS:NOOP-N ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

\ ---- the seven declarations --------------------------------------------------
\ Each names the corpus word, the capability that stops it, and the rest of what
\ its body asks for. The capability vocabulary is tools/codegen-compare-gap.f's;
\ these rows read as a plan for the rest of the float campaign as much as an
\ account of today.

\ The three accumulations: a counted loop over a locals frame, a load per turn,
\ and float arithmetic between them.
: SUM-GAP ( -- )
   s" CODEGEN-CORPUS3:T-SUM" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

: DIST2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-DIST2" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

: NORM2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-NORM2" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

\ The step, which stores as well as loads.
: SGD-STEP-GAP ( -- )
   s" CODEGEN-CORPUS3:T-SGD!" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

\ The one that reaches its answers through two calls.
: REL-L2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-REL-L2" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:CALLS CODEGEN-GAP:GAP-ALSO ;

\ The one branch row that is still a gap, and the reason is the LITERAL in its
\ first arm. `x f0< if 0.0 else x then` compares and branches exactly as MAX-F
\ does - both compile - but its two arms hand the join a double and a cell, and
\ a double may not cross a block edge yet. It needs nothing else: the comparison
\ and the branch are built.
: RELU-GAP ( -- )
   s" CODEGEN-CORPUS3:RELU-F" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP ;

\ The float-to-integer conversion, in the body that really does it. Its two arms
\ both leave a computed double, so it waits for the same one thing RELU-F waits
\ for and for nothing else - the conversion itself and the comparison that
\ chooses the bias are both built.
: FROUND-GAP ( -- )
   s" CODEGEN-CORPUS3:FROUND" CODEGEN--GAP-CAP:FLOAT-PLACE CODEGEN-GAP:GAP ;

\ ---- the three measured rows -------------------------------------------------
\ The pinned inputs are the old column's, written as the same literals
\ tools/codegen-compare-cases3.f writes, so the two columns are handed the same
\ numbers and neither reads the other's. The outputs go to VECTOR-REAL, which
\ records the whole cell - so the comparison is bit for bit, and a sign of a
\ zero, a NaN payload or a rounding that differed would be reported rather than
\ absorbed.
\
\ 2^53+1, the smallest integer a double cannot hold, is written here as the same
\ literal the case list writes it as.
9007199254740993 constant WIDE-INT

\ The NaN, written as what it is: this engine has no literal for one, and the
\ survey at the head of tools/codegen-compare-corpus3.f records that the value
\ it computes is deterministic. It is spelled here the same way
\ tools/codegen-compare-cases3.f spells it, so the two columns are handed the
\ same bits.
: NAN ( -- r )
   0.0 0.0 f/ ;

: SGD-CASE ( -- )
   s" CODEGEN-CORPUS3:SGD" s" CODEGEN-CORPUS3:SGD-N"
   [: 1.0 0.5 0.25 CODEGEN-CORPUS3:SGD-N drop ;]
   [: 1.0 0.5 0.25 CODEGEN-CORPUS3:SGD-N CODEGEN-COMPARE:VECTOR-REAL
      -2.0 -0.5 0.25 CODEGEN-CORPUS3:SGD-N CODEGEN-COMPARE:VECTOR-REAL
      0.0 0.0 1.0 CODEGEN-CORPUS3:SGD-N CODEGEN-COMPARE:VECTOR-REAL
      -0.0 0.0 1.0 CODEGEN-CORPUS3:SGD-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: SEG-CASE ( -- )
   s" CODEGEN-CORPUS3:SEG-1/SQRT" s" CODEGEN-CORPUS3:SEG-1/SQRT-N"
   [: 4 CODEGEN-CORPUS3:SEG-1/SQRT-N drop ;]
   [: 4 CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL
      1 CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL
      2 CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL
      0 CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL
      -4 CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL
      WIDE-INT CODEGEN-CORPUS3:SEG-1/SQRT-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The branch row. Its pinned inputs are the old column's, in the old column's
\ order, and they are what makes this row a statement about the LOWERING rather
\ than about the arithmetic: both argument orders catch a branch taken the wrong
\ way, the two zeros catch a compare of bits instead of numbers, and a NaN in
\ each position catches a fused condition that is true on unordered - which is
\ what `f<` lowered under `lt` instead of `mi` would be. The outputs go to
\ VECTOR-REAL, so the negative zero is compared as the cell it is.
: MAX-F-CASE ( -- )
   s" CODEGEN-CORPUS3:MAX-F" s" CODEGEN-CORPUS3:MAX-F-N"
   [: 1.5 -2.5 CODEGEN-CORPUS3:MAX-F-N drop ;]
   [: 1.5 -2.5 CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL
      -2.5 1.5 CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL
      0.0 -0.0 CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL
      -0.0 0.0 CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL
      NAN 1.5 CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL
      1.5 NAN CODEGEN-CORPUS3:MAX-F-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: ALL-GAPS ( -- )
   SUM-GAP
   DIST2-GAP
   NORM2-GAP
   SGD-STEP-GAP
   REL-L2-GAP
   RELU-GAP
   FROUND-GAP ;

public

\ Measure what the chain can express, declare the rest, and check that between
\ them they account for all of it. Nothing is compiled today and the check is
\ what makes that a statement rather than a hope: a word neither compiled nor
\ declared is refused. Runs after the old column, whose rows every name here is
\ checked against.
: RUN ( -- )
   CODEGEN-GAP:RESET
   NOOP-CASE
   SGD-CASE
   SEG-CASE
   MAX-F-CASE
   ALL-GAPS
   CODEGEN-GAP:COVERAGE-CK ;

;package
