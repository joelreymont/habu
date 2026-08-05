\ codegen-compare-cases3.f - the case list of the THIRD codegen comparison.
\ One concern: which word of the third corpus is measured, on which pinned
\ inputs.
\
\ A case is three things: the subject word's name, a body that calls it once so
\ it can be timed, and a body that calls it on its pinned inputs and hands every
\ result to CODEGEN-COMPARE:VECTOR - or, for a float, to
\ CODEGEN-COMPARE:VECTOR-REAL, which records the cell the double already is.
\ That is tools/codegen-compare-cases.f's shape and nothing about it changes
\ here.
\
\ THE INPUTS ARE PINNED AND CHOSEN TO REVEAL THE SHAPE. A recorded output only
\ proves something if the input that produced it never moves, and it only proves
\ something INTERESTING if the inputs reach every way through the word. For
\ float work that means more than the usual three: the sign of a zero, a value
\ that is not a number, an infinity and an accumulation whose answer depends on
\ the order it was accumulated in are all things a code generator can get wrong
\ while every ordinary input still looks right. So:
\
\   T-SUM        three lengths of accumulation: the four-element vector with a
\                negative, a zero and two fractions; the sum vector, whose
\                answer is 2^53 folded left and 2^53+2 folded any other way, so
\                the row pins the ORDER and not only the arithmetic; and a
\                length of zero, which must be the accumulator's 0.0 and not
\                whatever the buffer holds.
\   T-DIST2      two vectors that differ in three of their four elements and
\                agree in the remaining one, so one term of the sum contributes
\                nothing; the same vector against itself, which must be a
\                positive zero; and a length of zero.
\   T-NORM2      a vector with a negative element, so a square that lost its
\                sign is visible; the zero vector; and a length of zero.
\   T-SGD!       one step of four weights against four gradients, read back as
\                FIVE cells - the four the step wrote and the one after them,
\                which the loop must not have touched. The gradients carry a
\                negative, so a step that added where it subtracts moves the
\                wrong way on one element and the right way on the others.
\   T-REL-L2     the ordinary case; a vector against itself, which is a zero
\                over a norm; a vector against the ZERO vector, which is a
\                positive infinity because division by zero does not trap; and
\                the zero vector against itself, which is the default NaN. The
\                last two are why this row exists at this length: they pin what
\                the engine does with a degenerate reference, and they are
\                deterministic - survey (5) and (6) at the head of the corpus.
\   RELU-F       a negative, a positive zero, a NEGATIVE zero, a positive, and a
\                NaN. The negative zero separates "answers x" from "answers the
\                literal 0.0", because the two are equal numbers in different
\                cells; the NaN pins which arm of the branch an unordered
\                comparison takes.
\   MAX-F        both argument orders, both zeros in both orders, and a NaN in
\                each position. A branch taken the wrong way answers the other
\                argument, and only the two orders together can see that; the
\                two zeros show the compare is a compare of numbers and not of
\                bits; the two NaN positions show the comparison is false either
\                way round rather than merely on one side.
\   SGD          an ordinary step; a step where the weight and the gradient are
\                both negative, so the update ADDS; a zero step; and a step from
\                a negative zero, which stays a negative zero.
\   SEG-1/SQRT   four, whose root is exact; one; two, whose root is not exact
\                and so pins the rounding; zero, which is a division by zero;
\                a NEGATIVE length, whose square root is a NaN; and 2^53+1,
\                which does not fit a double and so pins the conversion's
\                round-to-nearest.
\   FROUND       both halves at .5 in both signs, a negative zero, a positive
\                that rounds up, an infinity and a NaN. The last three are the
\                ones that separate a rounding word from the truncating f>s
\                underneath it: -0.0 answers 0, an infinity saturates, and a NaN
\                answers 0.
\
\ WHY THE STEP'S CASE RESETS AND THE TIMED BODY DOES NOT. T-SGD! is the one
\ subject that is not idempotent: running it twice moves the weights twice. The
\ recorded call therefore refills the buffer first, so what the row records is
\ one step from the pinned weights however many passes have run before it. The
\ TIMED body calls the subject alone, like every other row's, because a reset
\ inside it would be timing a fill as well as a step; the weights it leaves
\ behind are undone by the next recorded call's reset.
\
\ THE CALIBRATION ROW IS CODEGEN-CORPUS:NOOP, THE FIRST CORPUS'S EMPTY CALL, AND
\ IT IS MEASURED AGAIN IN THIS PASS, for the reason tools/codegen-compare-cases2.f
\ gives beside its own: a calibration row is the floor of a call on this path, an
\ empty word is an empty word, and what matters is that the floor is measured on
\ the same host at the same moment as the rows divided by it.

require tools/codegen-compare-core.f
require tools/codegen-compare-corpora.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus3.f
require tools/codegen-compare-new3.f

package CODEGEN-CASES3

private

\ The NaN and the infinity the cases below hand to a subject, written as what
\ they are: this engine has no literal for either, and the survey at the head of
\ tools/codegen-compare-corpus3.f records that both are deterministic.
: NAN ( -- r )
   0.0 0.0 f/ ;

: INF ( -- r )
   1.0 0.0 f/ ;

\ 2^53+1, the smallest integer a double cannot hold.
9007199254740993 constant WIDE-INT

: T-SUM-CASE ( -- )
   s" CODEGEN-CORPUS3:T-SUM"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SUM drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SUM
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:S-VEC CODEGEN-CORPUS3:SUM-LEN CODEGEN-CORPUS3:T-SUM
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC 0 CODEGEN-CORPUS3:T-SUM
      CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: T-DIST2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-DIST2"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2 drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2 CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-DIST2 CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC 0
      CODEGEN-CORPUS3:T-DIST2 CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: T-NORM2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-NORM2"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2 drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-NORM2
      CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC 0 CODEGEN-CORPUS3:T-NORM2
      CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

\ The one word of this corpus whose point is a side effect. What it RETURNS is
\ nothing at all, so the weight buffer is what is recorded: the four cells the
\ step wrote and the fifth, which it must not have.
: T-SGD-CASE ( -- )
   s" CODEGEN-CORPUS3:T-SGD!"
   [: CODEGEN-CORPUS3:STEP-LR CODEGEN-CORPUS3:W-VEC CODEGEN-CORPUS3:G-VEC
      CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SGD! ;]
   [: CODEGEN-CORPUS3:W-RESET
      CODEGEN-CORPUS3:STEP-LR CODEGEN-CORPUS3:W-VEC CODEGEN-CORPUS3:G-VEC
      CODEGEN-CORPUS3:VEC-LEN CODEGEN-CORPUS3:T-SGD!
      0 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      1 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      2 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      3 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL
      4 CODEGEN-CORPUS3:W-CELL CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: T-REL-L2-CASE ( -- )
   s" CODEGEN-CORPUS3:T-REL-L2"
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2 drop ;]
   [: CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2 CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:B-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2 CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:A-VEC CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2 CODEGEN-COMPARE:VECTOR-REAL
      CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:Z-VEC CODEGEN-CORPUS3:VEC-LEN
      CODEGEN-CORPUS3:T-REL-L2 CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: RELU-CASE ( -- )
   s" CODEGEN-CORPUS3:RELU-F"
   [: -2.5 CODEGEN-CORPUS3:RELU-F drop ;]
   [: -2.5 CODEGEN-CORPUS3:RELU-F CODEGEN-COMPARE:VECTOR-REAL
      0.0 CODEGEN-CORPUS3:RELU-F CODEGEN-COMPARE:VECTOR-REAL
      -0.0 CODEGEN-CORPUS3:RELU-F CODEGEN-COMPARE:VECTOR-REAL
      1.5 CODEGEN-CORPUS3:RELU-F CODEGEN-COMPARE:VECTOR-REAL
      NAN CODEGEN-CORPUS3:RELU-F CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: MAX-F-CASE ( -- )
   s" CODEGEN-CORPUS3:MAX-F"
   [: 1.5 -2.5 CODEGEN-CORPUS3:MAX-F drop ;]
   [: 1.5 -2.5 CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL
      -2.5 1.5 CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL
      0.0 -0.0 CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL
      -0.0 0.0 CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL
      NAN 1.5 CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL
      1.5 NAN CODEGEN-CORPUS3:MAX-F CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: SGD-CASE ( -- )
   s" CODEGEN-CORPUS3:SGD"
   [: 1.0 0.5 0.25 CODEGEN-CORPUS3:SGD drop ;]
   [: 1.0 0.5 0.25 CODEGEN-CORPUS3:SGD CODEGEN-COMPARE:VECTOR-REAL
      -2.0 -0.5 0.25 CODEGEN-CORPUS3:SGD CODEGEN-COMPARE:VECTOR-REAL
      0.0 0.0 1.0 CODEGEN-CORPUS3:SGD CODEGEN-COMPARE:VECTOR-REAL
      -0.0 0.0 1.0 CODEGEN-CORPUS3:SGD CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

: SEG-CASE ( -- )
   s" CODEGEN-CORPUS3:SEG-1/SQRT"
   [: 4 CODEGEN-CORPUS3:SEG-1/SQRT drop ;]
   [: 4 CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL
      1 CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL
      2 CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL
      0 CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL
      -4 CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL
      WIDE-INT CODEGEN-CORPUS3:SEG-1/SQRT CODEGEN-COMPARE:VECTOR-REAL ;]
   CODEGEN-COMPARE:MEASURE ;

\ The one row whose answers are already numbers: FROUND is the way OUT of the
\ float domain, so its outputs go to VECTOR unchanged.
: FROUND-CASE ( -- )
   s" CODEGEN-CORPUS3:FROUND"
   [: 2.5 CODEGEN-CORPUS3:FROUND drop ;]
   [: 2.5 CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR
      -2.5 CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR
      -0.0 CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR
      1.5 CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR
      INF CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR
      NAN CODEGEN-CORPUS3:FROUND CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: ALL-CASES ( -- )
   CODEGEN-CALIBRATE:OLD
   T-SUM-CASE
   T-DIST2-CASE
   T-NORM2-CASE
   T-SGD-CASE
   T-REL-L2-CASE
   RELU-CASE
   MAX-F-CASE
   SGD-CASE
   SEG-CASE
   FROUND-CASE ;

public

\ One measured pass over this corpus: the two columns of it, handed to the pass
\ machinery that every corpus shares.
: RUN ( -- )
   [: ALL-CASES ;] [: CODEGEN-NEW3:RUN ;] CODEGEN-COMPARE:PASS ;

;package

\ What this corpus is called, where its committed table lives, and which source
\ file that table is the measurement of. Stated here, beside the cases, so that
\ naming a corpus once brings everything about it with it; the drivers read it
\ back out of the register rather than naming this corpus a second time.
s" corpus3"
s" test/compiler/codegen-compare-baseline3.txt"
s" tools/codegen-compare-corpus3.f"
CODEGEN-CORPORA:DECLARE
