\ codegen-compare-new3.f - the new code generator's column of the THIRD
\ comparison. One concern: what the new chain can make of the float corpus, and
\ what it is waiting for where it cannot.
\
\ TEN GAPS AND NOTHING COMPILED, AND THAT IS THE RESULT THIS LEAF WAS ASKED FOR.
\ The float corpus is measured and committed BEFORE the chain has a single float
\ capability, so that the day it gains one there is a table to read the advance
\ against that nobody chose afterwards. Every one of the ten corpus words is
\ therefore declared a gap here, naming `floats` and everything else it also
\ needs. The account in tools/codegen-compare-gap.f refuses a pass in which some
\ corpus word is neither compiled nor declared, so "ten declared and none
\ compiled" is a statement about all ten rather than a harness that stopped
\ looking.
\
\ WHAT `floats` MEANS, MEASURED RATHER THAN ASSUMED. Handing a float body to the
\ migration the way tools/codegen-compare-migrated2.f hands it an integer one
\ fails in two different stages:
\
\     : FADD-N ( r r -- r ) f+ ;      throws -8286  E-HIR-UNMODELED
\     : FLIT-N ( r -- r ) 1.0 f+ ;    throws -8404  E-NFEED-KIND
\
\ The tape has no kind for a real literal - src/compiler/native/feed.f:174 says
\ so in as many words - and the dialect has no model for a float operation. One
\ capability covers both, because a row that named only the tape would read as
\ half a job and a row that named only the dialect would be wrong about where
\ the first refusal happens.
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
\ calibration row below is the first corpus's empty call, already published by
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

\ ---- the ten declarations ----------------------------------------------------
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

\ Straight-line float arithmetic over a locals frame: the smallest row of this
\ corpus, and the one the float campaign should be able to close first.
: SGD-GAP ( -- )
   s" CODEGEN-CORPUS3:SGD" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO ;

\ The two conversions. Both are straight-line over one local, and both need the
\ float division or the float compare that surrounds the conversion itself.
: SEG-GAP ( -- )
   s" CODEGEN-CORPUS3:SEG-1/SQRT" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:LOCALS CODEGEN-GAP:GAP-ALSO ;

: FROUND-GAP ( -- )
   s" CODEGEN-CORPUS3:FROUND" CODEGEN--GAP-CAP:FLOATS CODEGEN-GAP:GAP
   CODEGEN--GAP-CAP:CONTROL-FLOW CODEGEN-GAP:GAP-ALSO
   CODEGEN--GAP-CAP:COMPARISON CODEGEN-GAP:GAP-ALSO ;

: ALL-GAPS ( -- )
   SUM-GAP
   DIST2-GAP
   NORM2-GAP
   SGD-STEP-GAP
   REL-L2-GAP
   RELU-GAP
   MAX-F-GAP
   SGD-GAP
   SEG-GAP
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
   ALL-GAPS
   CODEGEN-GAP:COVERAGE-CK ;

;package
