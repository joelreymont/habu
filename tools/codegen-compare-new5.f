\ codegen-compare-new5.f - the new code generator's column of the FIFTH
\ comparison. One concern: what the new chain makes of the six tail-call shapes,
\ and what it refuses.
\
\ SIX ROWS AND NO GAPS. Every subject of tools/codegen-compare-corpus5.f
\ compiles: the chain accepts all six bodies and both callees, at arity one and
\ at arity two, with the eight registers every row of this corpus states. There
\ is nothing here for CODEGEN-GAP:GAP to declare, and that is itself the first
\ finding of the corpus - what the chain lacks at a tail call is not the ability
\ to compile one.
\
\ WHAT IT EMITTED THERE WHEN THIS CORPUS WAS WRITTEN, WHICH IS THE WHOLE POINT
\ OF THE CORPUS, READ OFF THE EMITTED CODE. A call in tail position was compiled
\ exactly like a call anywhere else. TAIL-BIG-N was twenty bytes and they were
\ these:
\
\     sub sp, sp, #16      reserve a frame
\     str x30, [sp, #16]   put the caller's return address in it
\     bl   C5-LONG-N       branch WITH LINK, and come back here
\     ldr x30, [sp, #16]   take the return address out again
\     add sp, sp, #16      give the frame back
\                          and the Ret in the slot the record's length points at
\
\ Five of those six are dead work at a tail call: this routine has nothing left
\ to do when the callee answers, so the callee could answer THIS routine's caller
\ through a plain B and none of the frame, the saved link register or the second
\ return would be needed. Every row of this column is a measurement of what those
\ five cost, taken before anything was done about them. What the chain emits NOW
\ is test/compiler/codegen-chain-baseline5.txt, which is where the number lives
\ and where an improvement is re-pinned; this paragraph is the starting point it
\ is read against.
\
\ WHAT THE BASELINE IS FOR. test/compiler/codegen-chain-baseline5.txt is where
\ this column stood when the corpus was written, and every row of it is a number
\ a tail branch should move. The control is NONTAIL, whose call is not in tail
\ position: its row must NOT move, and a change that shrank it shrank something
\ it had no right to. That is the one row of this corpus whose committed chain
\ size is a fence rather than a target, and it is the one whose branch-with-link
\ tools/codegen-compare-test.f asserts on both sides.
\
\ HOW THE ENGINE STANDS ON THE SAME ROWS, AND IT DOES NOT MOVE. Its emitter
\ refuses to copy either callee - the rule and the measurement are at the head of
\ tools/codegen-compare-corpus5.f - so it emits a real call at every site here,
\ and it does no tail-call transformation of any kind. On the pure shapes it and
\ the chain emitted the SAME five instructions when the corpus was written:
\ TAIL-BIG, TAIL-MID, TAIL-CHAIN and TAIL-PAIR twenty bytes in both columns and
\ TAIL-AFTER twenty-four in both. The chain was smaller only where a row has
\ arithmetic in it as well - TAIL-WORK and NONTAIL at 36 bytes against the
\ engine's 60 - because that is where the two differed at all. It is the cleanest
\ statement in the five corpora of what a call interface costs, precisely because
\ it is nearly the only thing the rows contain.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus5.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated5.f

package CODEGEN-NEW5

private

\ ---- the six rows -------------------------------------------------------------
\ The pinned inputs are the old column's, written as the same literals
\ tools/codegen-compare-cases5.f writes, so the two columns are handed the same
\ numbers and neither reads the other's.

: TAIL-BIG-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-BIG" s" CODEGEN-CORPUS5:TAIL-BIG-N"
   [: 7 CODEGEN-CORPUS5:TAIL-BIG-N drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-BIG-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-BIG-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-BIG-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-BIG-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: TAIL-WORK-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-WORK" s" CODEGEN-CORPUS5:TAIL-WORK-N"
   [: 7 CODEGEN-CORPUS5:TAIL-WORK-N drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-WORK-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-WORK-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-WORK-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-WORK-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The control row: its bytes are a fence, not a target.
: NONTAIL-CASE ( -- )
   s" CODEGEN-CORPUS5:NONTAIL" s" CODEGEN-CORPUS5:NONTAIL-N"
   [: 7 CODEGEN-CORPUS5:NONTAIL-N drop ;]
   [: 7 CODEGEN-CORPUS5:NONTAIL-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:NONTAIL-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:NONTAIL-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:NONTAIL-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: TAIL-MID-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-MID" s" CODEGEN-CORPUS5:TAIL-MID-N"
   [: 7 CODEGEN-CORPUS5:TAIL-MID-N drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-MID-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-MID-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-MID-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-MID-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: TAIL-CHAIN-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-CHAIN" s" CODEGEN-CORPUS5:TAIL-CHAIN-N"
   [: 7 CODEGEN-CORPUS5:TAIL-CHAIN-N drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-CHAIN-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-CHAIN-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-CHAIN-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-CHAIN-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ Both results are recorded, top of the stack first, exactly as the old column
\ records them.
: TAIL-PAIR-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-PAIR" s" CODEGEN-CORPUS5:TAIL-PAIR-N"
   [: 7 3 CODEGEN-CORPUS5:TAIL-PAIR-N 2drop ;]
   [: 7 3 CODEGEN-CORPUS5:TAIL-PAIR-N
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      0 0 CODEGEN-CORPUS5:TAIL-PAIR-N
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      -1 5 CODEGEN-CORPUS5:TAIL-PAIR-N
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR
      255 -1 CODEGEN-CORPUS5:TAIL-PAIR-N
      CODEGEN-COMPARE:VECTOR CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: TAIL-AFTER-CASE ( -- )
   s" CODEGEN-CORPUS5:TAIL-AFTER" s" CODEGEN-CORPUS5:TAIL-AFTER-N"
   [: 7 CODEGEN-CORPUS5:TAIL-AFTER-N drop ;]
   [: 7 CODEGEN-CORPUS5:TAIL-AFTER-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS5:TAIL-AFTER-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS5:TAIL-AFTER-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS5:TAIL-AFTER-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: COVERED-CASES ( -- )
   CODEGEN-CALIBRATE:NEW
   TAIL-BIG-CASE
   TAIL-WORK-CASE
   NONTAIL-CASE
   TAIL-MID-CASE
   TAIL-CHAIN-CASE
   TAIL-PAIR-CASE
   TAIL-AFTER-CASE ;

public

\ Measure what the chain can express, declare the rest, and check that between
\ them they account for all of it. Nothing is declared here because nothing is
\ refused; the account still runs, so a row that started being refused would be
\ an unaccounted row rather than a silently missing one.
: RUN ( -- )
   [: COVERED-CASES ;] CODEGEN-GAP:ACCOUNT ;

;package
