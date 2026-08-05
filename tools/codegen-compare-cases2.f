\ codegen-compare-cases2.f - the case list of the SECOND codegen comparison.
\ One concern: which word of the second corpus is measured, on which pinned
\ inputs.
\
\ A case is three things: the subject word's name, a body that calls it once so
\ it can be timed, and a body that calls it on its pinned inputs and hands every
\ result to CODEGEN-COMPARE:VECTOR so the outputs are recorded. That is
\ tools/codegen-compare-cases.f's shape and nothing about it changes here.
\
\ THE INPUTS ARE PINNED AND CHOSEN TO REVEAL THE SHAPE. A recorded output only
\ proves something if the input that produced it never moves, and it only proves
\ something INTERESTING if the inputs reach every way through the word. So:
\
\   TAG             three terms whose tag bits differ, including one whose bits
\                   are all set - a mask that dropped a bit answers the same as
\                   the right one on a small number and differently on 255.
\   WS?             one input per arm of the four-way `or`, and one that is
\                   none of them. A body that lost an arm still answers true for
\                   the three it kept, so a single space would not find it.
\   SYM-FOLD-C      five bytes: below the lower bound, ON it, ON the upper
\                   bound, above it, and one already lower case. Both boundaries
\                   are tested from both sides, which is what a `<` written as a
\                   `<=` moves.
\   MAX-DIM         both argument orders and the equal case. A branch taken the
\                   wrong way answers the other argument, and only the two
\                   orders together can see that.
\   COUNT-CHAR      a span whose matches are at the FIRST byte, in the middle
\                   and at the LAST byte, so a loop that runs one turn short or
\                   one turn long answers differently; plus a byte the span
\                   does not hold, and an empty span.
\   T-RES-WALK      the three ways out of its callee: a variable at the head of
\                   a two-link chain, a term that is not a variable, and a
\                   variable bound to nothing.
\   VEC-COPY-CELLS  one copy of four cells, read back as five - the four the
\                   copy moved and the one after them, which the loop must not
\                   have touched.
\
\ THE CALIBRATION ROW IS CODEGEN-CORPUS:NOOP, THE FIRST CORPUS'S EMPTY CALL, AND
\ IT IS MEASURED AGAIN IN THIS PASS. A calibration row is the floor of a call on
\ this path, and an empty word is an empty word: giving the second corpus a
\ second empty word would measure the same thing under another name and put two
\ numbers where the tables want one. Measuring it again in THIS pass is what
\ matters - a cost is a ratio to a call timed on the same host at the same
\ moment - and it also makes the two tables' cost columns readable against each
\ other.

require tools/codegen-compare-core.f
require tools/codegen-compare-corpora.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus2.f
require tools/codegen-compare-new2.f
require tools/codegen-compare-c2.f

package CODEGEN-CASES2

private

\ The byte span COUNT-CHAR is measured over: an `a` at the first byte, an `a` in
\ the middle and an `a` at the last one.
: SUBJECT$ ( -- ptr u8 n )
   s" aha aha aha" ;

: EMPTY$ ( -- ptr u8 n )
   s" " ;

97 constant LETTER-A              \ present in SUBJECT$, at both ends and between
122 constant LETTER-Z             \ absent from SUBJECT$

\ The five bytes WS? is measured on: one per arm, then one that is no arm.
32 constant WS-SPACE
9 constant WS-TAB
10 constant WS-LF
13 constant WS-CR
97 constant NOT-WS

\ The five bytes SYM-FOLD-C is measured on, around its two bounds.
64 constant BELOW-A               \ $40, one under the lower bound
65 constant EXACTLY-A             \ $41, the lower bound itself
90 constant EXACTLY-Z             \ $5A, the upper bound itself
91 constant ABOVE-Z               \ $5B, one over the upper bound
97 constant ALREADY-LOWER

: TAG-CASE ( -- )
   s" CODEGEN-CORPUS2:TAG"
   [: 9 CODEGEN-CORPUS2:TAG drop ;]
   [: 9 CODEGEN-CORPUS2:TAG CODEGEN-COMPARE:VECTOR
      24 CODEGEN-CORPUS2:TAG CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS2:TAG CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: WS-CASE ( -- )
   s" CODEGEN-CORPUS2:WS?"
   [: WS-SPACE CODEGEN-CORPUS2:WS? drop ;]
   [: WS-SPACE CODEGEN-CORPUS2:WS? CODEGEN-COMPARE:VECTOR-FLAG
      WS-TAB CODEGEN-CORPUS2:WS? CODEGEN-COMPARE:VECTOR-FLAG
      WS-LF CODEGEN-CORPUS2:WS? CODEGEN-COMPARE:VECTOR-FLAG
      WS-CR CODEGEN-CORPUS2:WS? CODEGEN-COMPARE:VECTOR-FLAG
      NOT-WS CODEGEN-CORPUS2:WS? CODEGEN-COMPARE:VECTOR-FLAG ;]
   CODEGEN-COMPARE:MEASURE ;

: SYM-FOLD-CASE ( -- )
   s" CODEGEN-CORPUS2:SYM-FOLD-C"
   [: EXACTLY-A CODEGEN-CORPUS2:SYM-FOLD-C drop ;]
   [: BELOW-A CODEGEN-CORPUS2:SYM-FOLD-C CODEGEN-COMPARE:VECTOR
      EXACTLY-A CODEGEN-CORPUS2:SYM-FOLD-C CODEGEN-COMPARE:VECTOR
      EXACTLY-Z CODEGEN-CORPUS2:SYM-FOLD-C CODEGEN-COMPARE:VECTOR
      ABOVE-Z CODEGEN-CORPUS2:SYM-FOLD-C CODEGEN-COMPARE:VECTOR
      ALREADY-LOWER CODEGEN-CORPUS2:SYM-FOLD-C CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: MAX-DIM-CASE ( -- )
   s" CODEGEN-CORPUS2:MAX-DIM"
   [: 3 7 CODEGEN-CORPUS2:MAX-DIM drop ;]
   [: 3 7 CODEGEN-CORPUS2:MAX-DIM CODEGEN-COMPARE:VECTOR
      7 3 CODEGEN-CORPUS2:MAX-DIM CODEGEN-COMPARE:VECTOR
      5 5 CODEGEN-CORPUS2:MAX-DIM CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: COUNT-CHAR-CASE ( -- )
   s" CODEGEN-CORPUS2:COUNT-CHAR"
   [: SUBJECT$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR drop ;]
   [: SUBJECT$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR CODEGEN-COMPARE:VECTOR
      SUBJECT$ LETTER-Z CODEGEN-CORPUS2:COUNT-CHAR CODEGEN-COMPARE:VECTOR
      EMPTY$ LETTER-A CODEGEN-CORPUS2:COUNT-CHAR CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: T-RES-WALK-CASE ( -- )
   s" CODEGEN-CORPUS2:T-RES-WALK"
   [: CODEGEN-CORPUS2:CHAIN-HEAD CODEGEN-CORPUS2:T-RES-WALK drop ;]
   [: CODEGEN-CORPUS2:CHAIN-HEAD CODEGEN-CORPUS2:T-RES-WALK CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS2:NOT-A-VAR CODEGEN-CORPUS2:T-RES-WALK CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS2:UNBOUND-VAR CODEGEN-CORPUS2:T-RES-WALK CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

\ The one word of this corpus whose point is a side effect. What it RETURNS is
\ nothing at all, so the destination buffer is what is recorded: the four cells
\ the copy moved and the fifth, which it must not have. Both columns would write
\ the SAME buffer, which is what makes the head-to-head check a statement about
\ the stores - and the new column has no row here today, for the reason
\ tools/codegen-compare-new2.f gives.
: VEC-COPY-CASE ( -- )
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS"
   [: CODEGEN-CORPUS2:COPY-FROM CODEGEN-CORPUS2:COPY-TO CODEGEN-CORPUS2:COPY-LEN
      CODEGEN-CORPUS2:VEC-COPY-CELLS ;]
   [: CODEGEN-CORPUS2:COPY-FROM CODEGEN-CORPUS2:COPY-TO CODEGEN-CORPUS2:COPY-LEN
      CODEGEN-CORPUS2:VEC-COPY-CELLS
      0 CODEGEN-CORPUS2:COPY-DST@ CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS2:COPY-DST@ CODEGEN-COMPARE:VECTOR
      2 CODEGEN-CORPUS2:COPY-DST@ CODEGEN-COMPARE:VECTOR
      3 CODEGEN-CORPUS2:COPY-DST@ CODEGEN-COMPARE:VECTOR
      4 CODEGEN-CORPUS2:COPY-DST@ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: ALL-CASES ( -- )
   CODEGEN-CALIBRATE:OLD
   TAG-CASE
   WS-CASE
   SYM-FOLD-CASE
   MAX-DIM-CASE
   COUNT-CHAR-CASE
   T-RES-WALK-CASE
   VEC-COPY-CASE ;

public

\ One measured pass over this corpus: the three columns of it, handed to the
\ pass machinery that every corpus shares.
: RUN ( -- )
   [: ALL-CASES ;] [: CODEGEN-NEW2:RUN ;] [: CODEGEN-C2:RUN ;] CODEGEN-COMPARE:PASS ;

;package

\ What this corpus is called, where its two committed tables live - the
\ engine's, then the chain's - and which source file both are the measurement
\ of. Stated here, beside the cases, so that naming a corpus once brings
\ everything about it with it; the drivers read it back out of the register
\ rather than naming this corpus a second time.
s" corpus2"
s" test/compiler/codegen-compare-baseline2.txt"
s" test/compiler/codegen-chain-baseline2.txt"
s" tools/codegen-compare-corpus2.f"
CODEGEN-CORPORA:DECLARE
