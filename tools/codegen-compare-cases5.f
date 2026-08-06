\ codegen-compare-cases5.f - the case list of the FIFTH codegen comparison.
\ One concern: which word of the fifth corpus is measured, on which pinned
\ inputs.
\
\ A case is three things: the subject word's name, a body that calls it once so
\ it can be timed, and a body that calls it on its pinned inputs and hands every
\ result to CODEGEN-COMPARE:VECTOR. That is tools/codegen-compare-cases.f's shape
\ and nothing about it changes here.
\
\ THE INPUTS ARE PINNED AND CHOSEN BEFORE ANY NUMBER WAS SEEN. This corpus is
\ about where a call sits rather than about what it computes, so the inputs have
\ one job: to catch a call that was turned into a branch and lost something on
\ the way. A tail call that is emitted as a branch hands its own caller the
\ callee's answer directly, so every way the transformation can go wrong shows up
\ as a WRONG ANSWER rather than as a slow one - a dropped argument, an argument
\ read out of the wrong data-stack slot, a result narrowed to thirty-two bits, a
\ frame released a moment too early.
\
\   THE FIVE ARITY-ONE ROWS - TAIL-BIG, TAIL-WORK, NONTAIL, TAIL-MID and
\                TAIL-CHAIN - take the same four values, and TAIL-AFTER takes them
\                too: 7, 0, -1 and 255. They are the four
\                tools/codegen-compare-cases4.f pins for its own call rows and
\                they are here for the same reason: the mask and the exclusive-or
\                inside C5-LONG make each of the four answer differently - 7 is
\                inside the mask, 0 is where the exclusive-or is the whole answer,
\                -1 has every bit set so the mask keeps three of them, and 255
\                spans a byte boundary - and -1 and 255 also catch a sign or a
\                width lost on the way through a call.
\   THE TIMED INPUT IS 7 for every one of them. There is no short way out of any
\                body in this corpus - no guard, no loop, no early exit - so every
\                input runs the same path and the timed one is chosen for
\                agreement with the recorded set rather than to reach the long
\                path.
\   TAIL-PAIR    four pinned pairs, and the pair is what is pinned: 7 and 3, so
\                that the two arguments are different and neither is neutral, and
\                a routine that read them in the wrong order answers differently;
\                0 and 0, which is the pair the arithmetic cannot hide behind,
\                because what comes back is what the two literals contribute and
\                nothing else; -1 and 5, which puts the negative in the FIRST
\                argument, where it is multiplied; and 255 and -1, which puts it
\                in the second, where it goes through the exclusive-or. No two of
\                the four are a reordering of each other, so a swap of the two
\                arguments is caught. The timed pair is 7 and 3.
\
\ TWO VALUES COME BACK FROM TAIL-PAIR AND BOTH ARE RECORDED, top of stack first,
\ which is the order the two VECTOR calls take them off in. All three columns
\ record the same two values in that same order; the reference column has to read
\ its second one back through an accessor, and the head of
\ tools/codegen-compare-c5.f says why.
\
\ THERE IS NO ROW WITH A SIDE EFFECT IN THIS CORPUS, so no case resets anything
\ and every subject here is idempotent: a second measurement pass records what
\ the first one did.
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
require tools/codegen-compare-corpus5.f
require tools/codegen-compare-new5.f
require tools/codegen-compare-c5.f

package CODEGEN-CASES5

private

: TAIL-BIG-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-BIG"
   [: 7 CODEGEN-CORPUS5:TAIL-BIG drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-BIG CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-BIG CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-BIG CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-BIG CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: TAIL-WORK-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-WORK"
   [: 7 CODEGEN-CORPUS5:TAIL-WORK drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-WORK CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-WORK CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-WORK CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-WORK CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

\ The control row: the same call with work after it, which is not a tail call.
: NONTAIL-CASE ( -- )
   s" CODEGEN-CORPUS5:NONTAIL"
   [: 7 CODEGEN-CORPUS5:NONTAIL drop ;]
   [: 7 CODEGEN-CORPUS5:NONTAIL CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:NONTAIL CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:NONTAIL CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:NONTAIL CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: TAIL-MID-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-MID"
   [: 7 CODEGEN-CORPUS5:TAIL-MID drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-MID CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-MID CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-MID CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-MID CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: TAIL-CHAIN-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-CHAIN"
   [: 7 CODEGEN-CORPUS5:TAIL-CHAIN drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-CHAIN CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-CHAIN CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-CHAIN CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-CHAIN CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

\ Two values come back from every call, and both are recorded: the top of the
\ stack first, then the one under it.
: TAIL-PAIR-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-PAIR"
   [: 7 3 CODEGEN-CORPUS5:TAIL-PAIR 2drop ;]
   [: 7 3 CODEGEN-CORPUS5:TAIL-PAIR
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      0 0 CODEGEN-CORPUS5:TAIL-PAIR
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      -1 5 CODEGEN-CORPUS5:TAIL-PAIR
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      255 -1 CODEGEN-CORPUS5:TAIL-PAIR
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: TAIL-AFTER-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-AFTER"
   [: 7 CODEGEN-CORPUS5:TAIL-AFTER drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-AFTER CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-AFTER CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-AFTER CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-AFTER CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE ;

: ALL-CASES ( -- )
   CODEGEN-CALIBRATE:OLD
   TAIL-BIG-CASE
   TAIL-WORK-CASE
   NONTAIL-CASE
   TAIL-MID-CASE
   TAIL-CHAIN-CASE
   TAIL-PAIR-CASE
   TAIL-AFTER-CASE ;

public

\ One measured pass over this corpus: the three columns of it, handed to the
\ pass machinery that every corpus shares.
: RUN ( -- )
   [: ALL-CASES ;] [: CODEGEN-NEW5:RUN ;] [: CODEGEN-C5:RUN ;] CODEGEN-COMPARE:PASS ;

;package

\ What this corpus is called, where its two committed tables live - the
\ engine's, then the chain's - and which source file both are the measurement
\ of. Stated here, beside the cases, so that naming a corpus once brings
\ everything about it with it; the drivers read it back out of the register
\ rather than naming this corpus a second time.
s" corpus5"
s" test/compiler/codegen-compare-baseline5.txt"
s" test/compiler/codegen-chain-baseline5.txt"
s" tools/codegen-compare-corpus5.f"
CODEGEN-CORPORA:DECLARE
