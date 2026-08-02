\ codegen-compare-new3.f - the new code generator's column of the THIRD
\ comparison. One concern: what the new chain can make of the float corpus, and
\ what it is waiting for where it cannot.
\
\ TWO COMPILED AND EIGHT STILL GAPS, WHICH IS WHAT THE SCALAR FLOAT LEAF WAS
\ ASKED FOR. The float corpus was measured and committed BEFORE the chain had a
\ single float capability, so that the day it gained one there was a table to
\ read the advance against that nobody chose afterwards. That day is this one:
\ SGD and SEG-1/SQRT are straight-line float arithmetic over a locals frame -
\ the first is one multiply and one subtract, the second is a conversion, a
\ square root and a division - and both now compile through the production
\ entry. The other eight need a loop, a branch, a call or a memory access as
\ well as floats, and each declares every one of them below.
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
\ with twelve operations over it. The eight rows that still name `floats` name
\ it because they need a float capability this leaf did not build - a float
\ COMPARISON for the branch rows, and a double that crosses a loop edge or a
\ memory cell for the accumulations - and each of them names the rest of what it
\ waits for beside it.
\
\ EVERY GAP NAMES EVERY CAPABILITY ITS ROW NEEDS, not the first that stops it -
\ tools/codegen-compare-gap.f's rule, and it matters more here than anywhere:
\ a reader planning the float work should see that the accumulation rows also
\ want a loop and a locals frame and memory access, that T-REL-L2 also wants
\ calls, and that the two branch rows also want a comparison, so that the day
\ floats land nobody is surprised by which rows still do not move.
\
\ WHAT IS NOT DONE HERE. No body is respelled to buy a row, and no float body is
\ handed to the chain in the hope that some part of it survives: a row that
\ cannot be compiled is a gap that names the capability, which is a result. The
\ two bodies that ARE compiled are the corpus's own to the byte - not one
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

\ ---- the eight declarations --------------------------------------------------
\ Each names the corpus word, the float capability, and the rest of what its
\ body asks for. The capability vocabulary is tools/codegen-compare-gap.f's;
\ these rows read as a plan for the float campaign as much as an account of
\ today.

\ The three accumulations: a counted loop over a locals frame, a load per turn,
\ and float arithmetic between them.
: SUM-GAP ( -- )
   s" CODEGEN-CORPUS3:T-SUM" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

: DIST2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-DIST2" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

: NORM2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-NORM2" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

\ The step, which stores as well as loads.
: SGD-STEP-GAP ( -- )
   s" CODEGEN-CORPUS3:T-SGD!" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:MEMORY CODEGEN-GAP:GAP-ALSO ;

\ The one that reaches its answers through two calls.
: REL-L2-GAP ( -- )
   s" CODEGEN-CORPUS3:T-REL-L2" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:CALLS CODEGEN-GAP:GAP-ALSO ;

\ The two branch rows. Their comparison is a float comparison, which is part of
\ the float capability, and the branch it feeds is control flow.
: RELU-GAP ( -- )
   s" CODEGEN-CORPUS3:RELU-F" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:COMPARISON CODEGEN-GAP:GAP-ALSO ;

: MAX-F-GAP ( -- )
   s" CODEGEN-CORPUS3:MAX-F" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:COMPARISON CODEGEN-GAP:GAP-ALSO ;

\ The float-to-integer conversion is a gap and the integer-to-float one is a row,
\ which is exactly the shape of what this leaf built: FROUND needs the conversion
\ AND a branch on a float comparison, and only the conversion is here.
: FROUND-GAP ( -- )
   s" CODEGEN-CORPUS3:FROUND" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:COMPARISON CODEGEN-GAP:GAP-ALSO ;

\ ---- the two measured rows ---------------------------------------------------
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

: ALL-GAPS ( -- )
   SUM-GAP
   DIST2-GAP
   NORM2-GAP
   SGD-STEP-GAP
   REL-L2-GAP
   RELU-GAP
   MAX-F-GAP
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
   ALL-GAPS
   CODEGEN-GAP:COVERAGE-CK ;

;package
