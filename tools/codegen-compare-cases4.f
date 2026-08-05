\ codegen-compare-cases4.f - the case list of the FOURTH codegen comparison.
\ One concern: which word of the fourth corpus is measured, on which pinned
\ inputs.
\
\ A case is three things: the subject word's name, a body that calls it once so
\ it can be timed, and a body that calls it on its pinned inputs and hands every
\ result to CODEGEN-COMPARE:VECTOR. That is tools/codegen-compare-cases.f's shape
\ and nothing about it changes here.
\
\ THE INPUTS ARE PINNED AND CHOSEN BEFORE THE NUMBERS WERE SEEN. That matters
\ more in this corpus than in the other three, because this one was built to make
\ a particular code generator lose: an input picked after a row was measured would
\ turn a benchmark into an argument. Each row's inputs reach every way through the
\ word, and the TIMED input is the one that runs the longest path, so a row that
\ looks fast because its timed input takes the short way out cannot happen here:
\
\   CALL-FAN     four values, chosen so that the mask and the exclusive-or in the
\                middle of the chain actually change the answer: 7 (which the mask
\                takes to zero), 0, -1 (every bit set, so the mask keeps three of
\                them) and 255. A chain that dropped one of the five calls answers
\                differently on at least one of the four.
\   CALL-FAN-BIG the same four values, for the same reason and one more: the
\                chain through five copies of `3 * 5 +` is 243x + 605, and a
\                chain that made only four of the five calls is 81x + 200, which
\                agrees with it at no whole number at all - so every one of the
\                four inputs catches a dropped call, and -1 and 255 also catch a
\                sign or a width lost on the way through.
\   CALL-PRESSURE
\                seven locals and a seed over the ordinary eight turns; the same
\                nine values with a length of ZERO, which must be the seed plus
\                the seven locals and nothing else; and all nine negative,
\                because the locals are added after the loop and a sign lost
\                there is invisible on positive inputs. Only the old column
\                measures this row; the new one declares it a gap.
\   CALL-LOOP-3  the ordinary eight turns; a length of ZERO, which must be the
\                seed plus the three locals and nothing else - that is the case
\                that catches a loop whose body ran once when it should not have;
\                and the same eight turns from negative locals and a negative
\                seed, because the three locals are added after the loop and a
\                sign lost there is invisible on positive inputs.
\   TINY-CALLEE  eight turns from zero, so the answer is exactly four times the
\                turn count and a dropped call shows up as a wrong multiple; a
\                length of zero, which must be the seed; and a negative seed.
\   WIDE-ARITY   the six arguments in order and in REVERSE order, because the
\                three subtractions are not symmetric and the reversal is what
\                catches two arguments read in the wrong places; all zeros; and
\                all negative.
\   LADDER       eight inputs, one for each way out of the chain and one that
\                falls through all of it: -5 and 0 take the first guard, 1 the
\                second, 3 the third, 20 the fifth, 100 the eighth, 127 the eighth
\                and 1000 none of them. The TIMED input is 1000 - the path that
\                runs every compare - because timing the short way out would
\                measure the first guard and call it a ladder.
\   PRESSURE     the record's fourteen fields over eight turns, and a length of
\                zero. Only the old column measures this row; the new one declares
\                it a gap.
\   BIG-CONSTS   eight turns; zero turns, which must be zero; and ONE turn, which
\                is the four literals combined with the index zero and is
\                therefore the case that pins the four constants themselves rather
\                than a sum over them.
\   MANY-LOCALS  eight locals over eight turns; a length of zero; and alternating
\                signs, so a local read out of the wrong slot changes the answer
\                instead of cancelling.
\   FLOAT-MIX    eight turns from zero, which is the sum of the halved indices and
\                so pins the truncation rather than just the arithmetic; zero
\                turns; and three turns from a negative seed.
\   STORE-LOAD   one run of eight turns over the corpus's own cell, read back as
\                the cell the step wrote AND the cell after it, which it must not
\                have; then a run of zero turns from the refilled cell, which must
\                answer the fill and leave it alone.
\
\ WHY THE STEP'S CASE RESETS AND THE TIMED BODY DOES NOT. STORE-LOAD is the one
\ subject that is not idempotent: running it twice steps the cell twice. The
\ recorded call therefore refills first, so what the row records is one run from
\ the pinned cell however many passes have run before it. The TIMED body calls the
\ subject alone, like every other row's, because a reset inside it would be timing
\ a fill as well as a step; the value it leaves behind is undone by the next
\ recorded call's reset.
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
require tools/codegen-compare-corpus4.f
require tools/codegen-compare-new4.f
require tools/codegen-compare-c4.f

package CODEGEN-CASES4

private

: CALL-FAN-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-FAN"
   [: 7 CODEGEN-CORPUS4:CALL-FAN drop ;]
   [: 7 CODEGEN-CORPUS4:CALL-FAN CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:CALL-FAN CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS4:CALL-FAN CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS4:CALL-FAN CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: CALL-FAN-BIG-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-FAN-BIG"
   [: 7 CODEGEN-CORPUS4:CALL-FAN-BIG drop ;]
   [: 7 CODEGEN-CORPUS4:CALL-FAN-BIG CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:CALL-FAN-BIG CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS4:CALL-FAN-BIG CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS4:CALL-FAN-BIG CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: CALL-PRESSURE-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-PRESSURE"
   [: 1 2 3 4 5 6 7 9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE drop ;]
   [: 1 2 3 4 5 6 7 9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE CODEGEN-COMPARE:VECTOR
      1 2 3 4 5 6 7 9 0 CODEGEN-CORPUS4:CALL-PRESSURE CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -4 -5 -6 -7 -9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: CALL-LOOP-3-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-LOOP-3"
   [: 1 2 3 7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3 drop ;]
   [: 1 2 3 7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3
      CODEGEN-COMPARE:VECTOR
      1 2 3 7 0 CODEGEN-CORPUS4:CALL-LOOP-3 CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: TINY-CALLEE-CASE ( -- )
   s" CODEGEN-CORPUS4:TINY-CALLEE"
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE drop ;]
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE
      CODEGEN-COMPARE:VECTOR
      5 0 CODEGEN-CORPUS4:TINY-CALLEE CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: WIDE-ARITY-CASE ( -- )
   s" CODEGEN-CORPUS4:WIDE-ARITY"
   [: 1 2 3 4 5 6 CODEGEN-CORPUS4:WIDE-ARITY drop ;]
   [: 1 2 3 4 5 6 CODEGEN-CORPUS4:WIDE-ARITY CODEGEN-COMPARE:VECTOR
      6 5 4 3 2 1 CODEGEN-CORPUS4:WIDE-ARITY CODEGEN-COMPARE:VECTOR
      0 0 0 0 0 0 CODEGEN-CORPUS4:WIDE-ARITY CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -4 -5 -6 CODEGEN-CORPUS4:WIDE-ARITY CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: LADDER-CASE ( -- )
   s" CODEGEN-CORPUS4:LADDER"
   [: 1000 CODEGEN-CORPUS4:LADDER drop ;]
   [: -5 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      3 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      20 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      100 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      127 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR
      1000 CODEGEN-CORPUS4:LADDER CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: PRESSURE-LOOP-CASE ( -- )
   s" CODEGEN-CORPUS4:PRESSURE-LOOP"
   [: CODEGEN-CORPUS4:REC CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:PRESSURE-LOOP drop ;]
   [: CODEGEN-CORPUS4:REC CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:PRESSURE-LOOP CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS4:REC 0 CODEGEN-CORPUS4:PRESSURE-LOOP
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: BIG-CONSTS-CASE ( -- )
   s" CODEGEN-CORPUS4:BIG-CONSTS"
   [: CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:BIG-CONSTS drop ;]
   [: CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:BIG-CONSTS CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:BIG-CONSTS CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:BIG-CONSTS CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: MANY-LOCALS-CASE ( -- )
   s" CODEGEN-CORPUS4:MANY-LOCALS"
   [: 1 2 3 4 5 6 7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS drop ;]
   [: 1 2 3 4 5 6 7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS
      CODEGEN-COMPARE:VECTOR
      1 2 3 4 5 6 7 8 0 CODEGEN-CORPUS4:MANY-LOCALS CODEGEN-COMPARE:VECTOR
      -1 2 -3 4 -5 6 -7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: FLOAT-MIX-CASE ( -- )
   s" CODEGEN-CORPUS4:FLOAT-MIX"
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:FLOAT-MIX drop ;]
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:FLOAT-MIX CODEGEN-COMPARE:VECTOR
      0 0 CODEGEN-CORPUS4:FLOAT-MIX CODEGEN-COMPARE:VECTOR
      -100 3 CODEGEN-CORPUS4:FLOAT-MIX CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: STORE-LOAD-CASE ( -- )
   s" CODEGEN-CORPUS4:STORE-LOAD"
   [: CODEGEN-CORPUS4:STEP-AT CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:STORE-LOAD drop ;]
   [: CODEGEN-CORPUS4:S-RESET
      CODEGEN-CORPUS4:STEP-AT CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:STORE-LOAD CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:STEP-CELL@ CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:STEP-CELL@ CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS4:S-RESET
      CODEGEN-CORPUS4:STEP-AT 0 CODEGEN-CORPUS4:STORE-LOAD
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: ALL-CASES ( -- )
   CODEGEN-CALIBRATE:OLD
   CALL-FAN-CASE
   CALL-FAN-BIG-CASE
   CALL-LOOP-3-CASE
   CALL-PRESSURE-CASE
   TINY-CALLEE-CASE
   WIDE-ARITY-CASE
   LADDER-CASE
   PRESSURE-LOOP-CASE
   BIG-CONSTS-CASE
   MANY-LOCALS-CASE
   FLOAT-MIX-CASE
   STORE-LOAD-CASE ;

public

\ One measured pass over this corpus: the three columns of it, handed to the
\ pass machinery that every corpus shares.
: RUN ( -- )
   [: ALL-CASES ;] [: CODEGEN-NEW4:RUN ;] [: CODEGEN-C4:RUN ;] CODEGEN-COMPARE:PASS ;

;package

\ What this corpus is called, where its two committed tables live - the
\ engine's, then the chain's - and which source file both are the measurement
\ of. Stated here, beside the cases, so that naming a corpus once brings
\ everything about it with it; the drivers read it back out of the register
\ rather than naming this corpus a second time.
s" corpus4"
s" test/compiler/codegen-compare-baseline4.txt"
s" test/compiler/codegen-chain-baseline4.txt"
s" tools/codegen-compare-corpus4.f"
CODEGEN-CORPORA:DECLARE
