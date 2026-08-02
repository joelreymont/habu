\ codegen-compare-new3.f - the new code generator's column of the THIRD
\ comparison. One concern: what the new chain can make of the float corpus, and
\ what it is waiting for where it cannot.
\
\ ELEVEN ROWS, NO GAPS, AND IT TOOK THREE LEAVES. The float corpus was measured
\ and committed BEFORE the chain had a single float capability, so that the day
\ it gained one there was a table to read the advance against that nobody chose
\ afterwards. Every row of it is now a measured row:
\
\   the scalar leaf     SGD and SEG-1/SQRT - straight-line float arithmetic over
\                       a locals frame, and the two conversions.
\   the comparison leaf MAX-F - a float compare feeding a two-armed branch, fused
\                       into one Fcmp and one conditional branch.
\   this leaf           the other seven, which are one thing under four names: a
\                       double placed where a straight line does not reach.
\
\ WHAT THE SEVEN REALLY NEEDED, MEASURED RATHER THAN GUESSED AT. Before this leaf
\ the chain refused each of them in the elaborator, at a position whose type had
\ to be stated before the value that would arrive there was known:
\
\     : NMG-BAD3 ( r n -- r ) 0 ?do 1.0 f+ loop ;    threw -8580  E-NELAB-TYPE
\     : RELU-N ( r -- r ) {: x :} x f0< if 0.0 else x then ;
\                                                    threw -8580  E-NELAB-TYPE
\
\ Both compile now, and so does every shape between them: a block argument takes
\ the type of the first value that reaches it and every later edge crosses to it,
\ and a value crossing a CALL crosses as the data-stack cell the machine stage
\ puts it in. Neither crossing computes anything - one FMOV between the register
\ files, the same eight bytes read the other way - which is why every row's
\ recorded output is the old column's to the bit.
\
\ ONE ROW COSTS MORE THAN THE ENGINE'S CODE, AND IT IS SAID HERE RATHER THAN LEFT
\ TO BE NOTICED. T-SGD! is 340 bytes against the engine's 448 and is SLOWER: its
\ loop body is three calls - two loads and a store - and its four locals, two
\ counters and accumulator are live across all of them, so each call writes seven
\ values into data-stack slots and reads them back. Nothing in a Habu word's
\ convention is callee-saved, so that discipline is correct; it is also paid three
\ times per turn here. Dot habu-narrow-what-a-5d6a0845 carries narrowing what
\ a call site saves to what the callee can really reach, and
\ tools/codegen-compare-test.f pins the row's direction so the day it moves
\ somebody has to look at it.
\
\ WHAT IS NOT DONE HERE. No body is respelled to buy a row, and no float body is
\ handed to the chain in the hope that some part of it survives. The ten bodies
\ are the corpus's own to the byte - not one constant is respelled, which is more
\ than either of the first two corpora could say - and
\ tools/codegen-compare-migrated3.f publishes them. The calibration row below is
\ the first corpus's empty call, already published by
\ tools/codegen-compare-migrated.f, measured again in this pass because a cost is
\ a ratio to a call timed on the same host at the same moment.

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

\ ---- no declarations left ----------------------------------------------------
\ The seven rows that were gaps are rows. What they were waiting for was one
\ thing under four names: a double placed where a straight line does not reach -
\ across a block edge, across a call, and round a loop's back edge - and it
\ landed as one rule. There is nothing here to declare, and CODEGEN-GAP:COVERAGE-CK
\ below is what says so rather than this sentence: a corpus word neither compiled
\ nor declared is refused, so an empty declaration list only passes when every
\ one of the eleven was measured.

\ ---- the ten measured rows ---------------------------------------------------
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

\ The infinity, the same way: spelled as the case list spells it, so both
\ columns divide the same one and FROUND's saturating row is handed the same
\ bits.
: INF ( -- r )
   1.0 0.0 f/ ;

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

\ ---- the five kernel rows ----------------------------------------------------
\ Each is handed the same pinned data the old column is - the corpus owns the
\ buffers and both columns step them - and the same three lengths. What each row
\ measures on top of the arithmetic is a double round a loop's back edge and
\ through a data-stack slot at every load, which is what the accumulation shape
\ IS on this engine.

: T-SUM-CASE ( -- )
   s" CODEGEN-CORPUS3:T-SUM" s" CODEGEN-CORPUS3:T-SUM-N"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SUM-N drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SUM-N
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:S-VEC CODEGEN-CORPUS3:SUM-LEN CODEGEN-CORPUS3:T-SUM-N
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC 0 CODEGEN-CORPUS3:T-SUM-N
      CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: T-DIST2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-DIST2" s" CODEGEN-CORPUS3:T-DIST2-N"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2-N drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2-N CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2-N CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC 0
      CODEGEN-CORPUS3:T-DIST2-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: T-NORM2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-NORM2" s" CODEGEN-CORPUS3:T-NORM2-N"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2-N drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2-N
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2-N
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC 0 CODEGEN-CORPUS3:T-NORM2-N
      CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The step, whose point is a side effect: what it RETURNS is nothing, so the
\ weight buffer is what is recorded - the four cells it wrote and the fifth,
\ which it must not have. The recorded call refills the buffer first for the
\ reason the old column's does: a step is not idempotent, and the timing loop
\ leaves the weights far from where they started.
: T-SGD-CASE ( -- )
   s" CODEGEN-CORPUS3:T-SGD!" s" CODEGEN-CORPUS3:T-SGD!-N"
   [: CODEGEN-CORPUS3:STEP-LR CODEGEN-CORPUS3:W-VEC CODEGEN-CORPUS3:G-VEC
      CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SGD!-N ;]
   [: CODEGEN-CORPUS3:W-RESET
      CODEGEN-CORPUS3:STEP-LR CODEGEN-CORPUS3:W-VEC CODEGEN-CORPUS3:G-VEC
      CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SGD!-N
      0 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      1 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      2 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      3 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      4 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: T-REL-L2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-REL-L2" s" CODEGEN-CORPUS3:T-REL-L2-N"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2-N drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2-N CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2-N CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2-N CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ ---- the two branch rows whose arms disagree about the class -----------------
\ RELU-F's arms hand the join a double and a cell and FROUND's hand it two
\ computed doubles, so between them they measure both halves of the join-type
\ rule. RELU-F's negative zero separates "answers x" from "answers the literal
\ 0.0" - two equal numbers in two cells - and its NaN pins which arm an
\ unordered comparison takes.
: RELU-CASE ( -- )
   s" CODEGEN-CORPUS3:RELU-F" s" CODEGEN-CORPUS3:RELU-F-N"
   [: -2.5 CODEGEN-CORPUS3:RELU-F-N drop ;]
   [: -2.5 CODEGEN-CORPUS3:RELU-F-N CODEGEN-COMPARE:VECTOR-REAL
      0.0 CODEGEN-CORPUS3:RELU-F-N CODEGEN-COMPARE:VECTOR-REAL
      -0.0 CODEGEN-CORPUS3:RELU-F-N CODEGEN-COMPARE:VECTOR-REAL
      1.5 CODEGEN-CORPUS3:RELU-F-N CODEGEN-COMPARE:VECTOR-REAL
      NAN CODEGEN-CORPUS3:RELU-F-N CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The one row whose answers are already numbers: FROUND is the way OUT of the
\ float domain, so its outputs go to VECTOR unchanged.
: FROUND-CASE ( -- )
   s" CODEGEN-CORPUS3:FROUND" s" CODEGEN-CORPUS3:FROUND-N"
   [: 2.5 CODEGEN-CORPUS3:FROUND-N drop ;]
   [: 2.5 CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR
      -2.5 CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR
      -0.0 CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR
      1.5 CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR
      INF CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR
      NAN CODEGEN-CORPUS3:FROUND-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

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
   RELU-CASE
   FROUND-CASE
   T-SUM-CASE
   T-DIST2-CASE
   T-NORM2-CASE
   T-SGD-CASE
   T-REL-L2-CASE
   CODEGEN-GAP:COVERAGE-CK ;

;package
