\ codegen-compare-cases.f - the case list of the codegen comparison.
\ One concern: which corpus word is measured, on which pinned inputs.
\
\ A case is three things: the subject word's name, a body that calls it once so
\ it can be timed, and a body that calls it on its pinned inputs and hands every
\ result to CODEGEN-COMPARE:VECTOR so the outputs are recorded.
\
\ The inputs are pinned for the same reason the corpus is pinned: a recorded
\ output only proves something if the input that produced it never moves. Each
\ case uses a small ordinary value plus, where the word has an interesting edge,
\ a second value that reaches it (an empty span, a zero argument, a miss).
\
\ The calibration case is first and says so. It measures an empty call, and
\ every other old row's cost is expressed as a multiple of it, so the table
\ survives being regenerated on a faster or slower machine.
\
\ The new code generator's column is measured in the same pass, from
\ tools/codegen-compare-new.f, on the same corpus words and the same pinned
\ inputs. It keeps its own calibration row for the reason given there: the two
\ paths are entered differently and only a ratio to a like call compares.

require tools/codegen-compare-core.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-new.f

package CODEGEN-CASES

private

: SUBJECT$ ( -- ptr u8 n )
   s" habu codegen baseline" ;

: EMPTY$ ( -- ptr u8 n )
   s" " ;

103 constant LETTER-G             \ present in SUBJECT$
122 constant LETTER-Z             \ absent from SUBJECT$

: CALIBRATION-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP"
   [: CODEGEN-CORPUS:NOOP ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE
   CODEGEN-COMPARE:CALIBRATE ;

: ADD3-CASE ( -- )
   s" CODEGEN-CORPUS:ADD3"
   [: 1 2 3 CODEGEN-CORPUS:ADD3 drop ;]
   [: 1 2 3 CODEGEN-CORPUS:ADD3 CODEGEN-COMPARE:VECTOR
      -5 5 7 CODEGEN-CORPUS:ADD3 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: SQUARE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:SQUARE-SUM"
   [: 3 4 CODEGEN-CORPUS:SQUARE-SUM drop ;]
   [: 3 4 CODEGEN-CORPUS:SQUARE-SUM CODEGEN-COMPARE:VECTOR
      -2 5 CODEGEN-CORPUS:SQUARE-SUM CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: MAX2-CASE ( -- )
   s" CODEGEN-CORPUS:MAX2"
   [: 3 4 CODEGEN-CORPUS:MAX2 drop ;]
   [: 3 4 CODEGEN-CORPUS:MAX2 CODEGEN-COMPARE:VECTOR
      9 -1 CODEGEN-CORPUS:MAX2 CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: LERP-CASE ( -- )
   s" CODEGEN-CORPUS:LERP"
   [: 10 20 50 CODEGEN-CORPUS:LERP drop ;]
   [: 10 20 50 CODEGEN-CORPUS:LERP CODEGEN-COMPARE:VECTOR
      0 100 25 CODEGEN-CORPUS:LERP CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: SUM-TO-CASE ( -- )
   s" CODEGEN-CORPUS:SUM-TO"
   [: 16 CODEGEN-CORPUS:SUM-TO drop ;]
   [: 16 CODEGEN-CORPUS:SUM-TO CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS:SUM-TO CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: COUNT-DOWN-CASE ( -- )
   s" CODEGEN-CORPUS:COUNT-DOWN"
   [: 16 CODEGEN-CORPUS:COUNT-DOWN drop ;]
   [: 16 CODEGEN-CORPUS:COUNT-DOWN CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CORPUS:COUNT-DOWN CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: FACT-CASE ( -- )
   s" CODEGEN-CORPUS:FACT"
   [: 10 CODEGEN-CORPUS:FACT drop ;]
   [: 10 CODEGEN-CORPUS:FACT CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS:FACT CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

\ The one corpus word whose whole point is a side effect. What it RETURNS is
\ `n 1+`, which a routine that never touched memory could compute just as well,
\ so the cell it bumped is recorded as an output beside the answer: after each
\ call the harness reads CODEGEN-CORPUS:BUMP-CELL@ and vectors it. The new
\ column bumps the very same cell and records it the same way, so the
\ head-to-head check is about the store and the load and not only about the
\ arithmetic between them.
: CELL-BUMP-CASE ( -- )
   s" CODEGEN-CORPUS:CELL-BUMP"
   [: 7 CODEGEN-CORPUS:CELL-BUMP drop ;]
   [: 7 CODEGEN-CORPUS:CELL-BUMP CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS:CELL-BUMP CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS:BUMP-CELL@ CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: BYTE-SUM-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-SUM"
   [: SUBJECT$ CODEGEN-CORPUS:BYTE-SUM drop ;]
   [: SUBJECT$ CODEGEN-CORPUS:BYTE-SUM CODEGEN-COMPARE:VECTOR
      EMPTY$ CODEGEN-CORPUS:BYTE-SUM CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: BYTE-FIND-CASE ( -- )
   s" CODEGEN-CORPUS:BYTE-FIND"
   [: SUBJECT$ LETTER-G CODEGEN-CORPUS:BYTE-FIND drop ;]
   [: SUBJECT$ LETTER-G CODEGEN-CORPUS:BYTE-FIND CODEGEN-COMPARE:VECTOR
      SUBJECT$ LETTER-Z CODEGEN-CORPUS:BYTE-FIND CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: ALL-CASES ( -- )
   CALIBRATION-CASE
   ADD3-CASE
   SQUARE-SUM-CASE
   MAX2-CASE
   LERP-CASE
   SUM-TO-CASE
   COUNT-DOWN-CASE
   FACT-CASE
   CELL-BUMP-CASE
   BYTE-SUM-CASE
   BYTE-FIND-CASE ;

public

\ Where this corpus's committed table lives, and which source file it is the
\ measurement of. Both are stated here, beside the cases, so that the runner
\ names a corpus once and everything about that corpus follows.
: BASELINE-PATH$ ( -- ptr u8 n )
   s" test/compiler/codegen-compare-baseline.txt" ;

: CORPUS-PATH$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus.f" ;

\ The old column first, then the new one: tools/codegen-compare-new.f checks
\ every name it writes down against a row the old column measured, so the old
\ rows have to be there before it runs.
: RUN ( -- )
   CODEGEN-COMPARE:RESET
   CODEGEN-COMPARE:PASS-BEGIN
   ALL-CASES
   CODEGEN-NEW:RUN
   CODEGEN-COMPARE:PASS-END
   CODEGEN-COMPARE:NORMALIZE ;

;package
