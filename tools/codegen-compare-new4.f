\ codegen-compare-new4.f - the new code generator's column of the FOURTH
\ comparison. One concern: what the new chain makes of the corpus built to beat
\ it, and what it refuses.
\
\ TEN ROWS AND ONE GAP. Nine of the ten subjects compile; PRESSURE-LOOP does not,
\ and what stops it is a refusal by name rather than a shortage of anything a
\ bigger number would buy. The row below declares it against the `loop-spill`
\ capability, and the account in tools/codegen-compare-gap.f says what that word
\ covers and why it is a capability rather than register pressure in general.
\
\ THE REFUSAL, MEASURED. The corpus's own text, handed to the same migration
\ entry every other row here uses, with the largest register pool the
\ architecture allows:
\
\     : PRESSURE-LOOP-N ( ptr n n -- n ) {: base:ptr len:n :}
\        0 len 0 ?do base @ base 8 + @ ... base 104 + @ + + + + + + + + + + + + + +
\        loop ;
\                                          18 registers   threw -8508  E-A64RA-SPILL
\
\ It is not the budget. The same body with THIRTEEN fields instead of fourteen
\ compiles at eighteen registers, and the fourteen-field body is refused at every
\ budget between one and eighteen; nineteen is not a budget at all, because
\ src/compiler/a64-effect.f refuses x18 in a routine's register set (E-A64EFF-GPR)
\ and the platform reserves it. So the wall is at fourteen values live at once
\ inside a loop, and it is exactly where src/compiler/native/regalloc.f says it
\ is: MB-SPILLABLE? will not take a class defined or read in a block that is
\ neither the entry nor the exit, because a frame access there would sit where the
\ memory order cannot be stated - and the body of a counted loop is such a block.
\ tools/codegen-compare-test.f runs that migration and checks the code, so the
\ paragraph above is a measurement rather than a claim.
\
\ ============================================================================
\ WHERE THE CHAIN LOSES, WHICH IS WHY THIS CORPUS EXISTS
\ ============================================================================
\ ONE of the nine compiled rows COSTS MORE than the code the engine's emitter
\ wrote for the same body: TINY-CALLEE, a counted loop whose body contains
\ NOTHING BUT CALLS. It is named here rather than left to be noticed, and
\ tools/codegen-compare-test.f pins it so the day it moves somebody has to look
\ at it. Measured on an idle twelve-core Apple Silicon host with the entry cost
\ taken off, over five passes:
\
\     TINY-CALLEE     old 58.6 - 60.8 ns     new 72.6 - 73.4 ns     about 1.21x
\
\ CALL-LOOP-3 WAS THE SECOND ONE AND IS NOT ANY MORE, which is the finding this
\ corpus was built to make possible. It measured old 45.9 - 48.8 against new 61.4
\ - 65.5, about 1.33x, and it now measures old 46.7 - 52.1 against new 48.5 -
\ 48.8. What moved it is the second half below.
\
\ THE FIRST HALF IS THE INLINER THE CHAIN HAS NOT GOT, and it is the whole of
\ what is left. The rule the engine applies is written out at the head of
\ tools/codegen-compare-corpus4.f: a callee of forty bytes of body or less, with
\ no branch in it, is copied into its caller verbatim and no call instruction is
\ emitted at all. Every callee this corpus has is exactly forty bytes of body.
\ Counted on the emitted machine code by tools/codegen-compare-test.f: the
\ engine's TINY-CALLEE contains ZERO Bl instructions and the chain's contains
\ four; the engine's CALL-LOOP-3 contains zero and the chain's three. Dot
\ habu-inline-a-small-83c310af carries it.
\
\ THE SECOND HALF WAS WHAT A CALL COSTS WHEN IT IS EMITTED, and it has been
\ narrowed. Nothing in a Habu word's convention is callee-saved, so a call site
\ used to put every live value out to a data-stack slot and read it back. It now
\ puts out only the ones the callee can really reach: a routine the chain
\ publishes records which registers its accepted allocation writes
\ (src/compiler/native/clobber.f), the allocator keeps a value that crosses a
\ call out of those registers, and the site saves the rest. Counted on the
\ emitted code, before and after:
\
\     CALL-LOOP-3-N   316 bytes, 19 stores and 23 loads  ->  196 bytes, 4 and 8
\     TINY-CALLEE-N   232 bytes, 13 stores and 14 loads  ->  168 bytes, 5 and 6
\
\ CALL-FAN-N DID NOT MOVE, and that is the right answer for it: its five calls
\ are a chain, each one's result being the next one's argument, so nothing at all
\ is live across them. It is 132 bytes with six stores and six loads either way,
\ and those are its own entry and exit and the argument and result of each call -
\ the FLOOR of what a call costs, which is not zero even when nothing needs
\ saving. Four of the instructions at each of its call boundaries are
\
\     sub x19, x19, #8      the data-stack pointer steps back over the value
\     ldr x0, [x19]         the value is read out of the slot
\     str x0, [x19]         and written straight back into the SAME slot
\     add x19, x19, #8      and the pointer steps forward again
\
\ whose net effect on the machine is nothing: the value the callee wants is
\ already in the slot the callee reads. Dot habu-fuse-a-call-36c0286e carries
\ fusing a reload with the store that puts the same value straight back in the
\ same place, and it is now the only thing between CALL-FAN-N and the floor.
\
\ ============================================================================
\ WHERE THE SUSPECTED WEAKNESS DID NOT BITE, AND WHY
\ ============================================================================
\ Seven rows were designed to be losses and are not. Two of them are DRAWS and
\ five are wins, and in every one of the five the same thing happens: the engine's
\ emitter is a stack machine and moves every intermediate through memory, so a row
\ whose body does real work pays the engine far more than the shape the row was
\ built to punish costs the chain.
\
\   CALL-FAN      A DRAW, and the most interesting answer in the corpus. It is
\                 five calls the engine does not make at all against five the
\                 chain makes, plus the twenty-four dead instructions above - and
\                 it measures old 4.96-5.53 ns against new 5.26-6.00 ns, with the
\                 chain ahead in one pass out of five. The dead quartet is nearly
\                 free in TIME: it is a store and a load to one address that the
\                 core forwards, over and over, while the engine's inlined copies
\                 are fifty real instructions. So the missing inliner costs SIZE
\                 here and not cycles - CALL-FAN-N is 132 bytes against 216 - and
\                 what turns it into a cost loss in the two rows above is the
\                 loop, whose carried state makes the boundary twelve instructions
\                 instead of four. Neither direction is asserted in the scheduled
\                 test, because a gate that turns on a twentieth of a nanosecond
\                 is a gate that fails for host load.
\   LADDER        also a draw: eight compares and eight branches are eight compares
\                 and eight branches, and the chain's are in registers while the
\                 engine's go through the data stack, which buys back about what
\                 the eight extra basic blocks cost. LADDER-N is one load, eight
\                 compare-and-branch pairs and one store - fifty-three instructions
\                 against the engine's four hundred bytes - and it is no faster.
\                 Not asserted either, and for the same reason.
\   WIDE-ARITY    the entry cost is the same on both sides - six values arrive in
\                 six data-stack slots either way - but the chain reads them into
\                 six registers once and computes in registers, while the engine
\                 copies each into its locals frame and then pushes and pops every
\                 intermediate. WIDE-ARITY-N is fifteen instructions in all.
\   BIG-CONSTS    both compilers materialise a sixty-four-bit literal with four
\                 move-wide instructions and neither has a constant pool, so the
\                 literals cost the same - and everything AROUND them does not.
\   MANY-LOCALS   eight locals live across a loop is eight registers, which the
\                 chain has; the engine reads each one out of its frame and pushes
\                 it, every turn.
\   FLOAT-MIX     the crossing the row was built to punish is one Fmov each way in
\                 the chain, and in the engine `s>f` and `f>s` are not inlined at
\                 all - src/habu/habu2.f gives a compile-state entry to f+, f-, f*
\                 and f/ only - so the engine pays two real CALLS a turn where the
\                 chain pays two register moves.
\   STORE-LOAD    the dependence chain is one load and one store a turn in both,
\                 and it is the same chain; what differs is the address arithmetic
\                 and the loop, both of which the chain keeps in registers.
\
\ AND THE ONE SENTENCE THE WHOLE CORPUS COMES TO. The new chain loses where a call
\ is made and there is nothing else in the body to hide it, and nowhere else; every
\ row whose body computes anything it wins, and it wins every row on size.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus4.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated4.f

package CODEGEN-NEW4

private

\ The calibration row, which is the first corpus's empty call measured again in
\ this pass. It returns nothing, so it has no output to compare; what it measures
\ is the floor of a call on this path, which every other new row is divided by.
: NOOP-CASE ( -- )
   s" CODEGEN-CORPUS:NOOP" s" CODEGEN-CORPUS:NOOP-N"
   [: CODEGEN-CORPUS:NOOP-N ;]
   [: ;]
   CODEGEN-COMPARE:MEASURE-NEW
   CODEGEN-COMPARE:CALIBRATE ;

\ ---- the one declaration ------------------------------------------------------
\ The row the chain refuses, against the capability it waits for. The refusal is
\ E-A64RA-SPILL at the largest register pool the architecture allows, and
\ tools/codegen-compare-test.f is where that is checked rather than described.
: GAPS ( -- )
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" CODEGEN--GAP-CAP:LOOP-SPILL CODEGEN-GAP:GAP ;

\ ---- the three call rows ------------------------------------------------------
\ The pinned inputs are the old column's, written as the same literals
\ tools/codegen-compare-cases4.f writes, so the two columns are handed the same
\ numbers and neither reads the other's.

: CALL-FAN-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-FAN" s" CODEGEN-CORPUS4:CALL-FAN-N"
   [: 7 CODEGEN-CORPUS4:CALL-FAN-N drop ;]
   [: 7 CODEGEN-CORPUS4:CALL-FAN-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:CALL-FAN-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS4:CALL-FAN-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS4:CALL-FAN-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: CALL-LOOP-3-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-LOOP-3" s" CODEGEN-CORPUS4:CALL-LOOP-3-N"
   [: 1 2 3 7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3-N drop ;]
   [: 1 2 3 7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3-N
      CODEGEN-COMPARE:VECTOR
      1 2 3 7 0 CODEGEN-CORPUS4:CALL-LOOP-3-N CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -7 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:CALL-LOOP-3-N
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: TINY-CALLEE-CASE ( -- )
   s" CODEGEN-CORPUS4:TINY-CALLEE" s" CODEGEN-CORPUS4:TINY-CALLEE-N"
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE-N drop ;]
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE-N
      CODEGEN-COMPARE:VECTOR
      5 0 CODEGEN-CORPUS4:TINY-CALLEE-N CODEGEN-COMPARE:VECTOR
      -3 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:TINY-CALLEE-N
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ ---- the two straight-line rows -----------------------------------------------

: WIDE-ARITY-CASE ( -- )
   s" CODEGEN-CORPUS4:WIDE-ARITY" s" CODEGEN-CORPUS4:WIDE-ARITY-N"
   [: 1 2 3 4 5 6 CODEGEN-CORPUS4:WIDE-ARITY-N drop ;]
   [: 1 2 3 4 5 6 CODEGEN-CORPUS4:WIDE-ARITY-N CODEGEN-COMPARE:VECTOR
      6 5 4 3 2 1 CODEGEN-CORPUS4:WIDE-ARITY-N CODEGEN-COMPARE:VECTOR
      0 0 0 0 0 0 CODEGEN-CORPUS4:WIDE-ARITY-N CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -4 -5 -6 CODEGEN-CORPUS4:WIDE-ARITY-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: LADDER-CASE ( -- )
   s" CODEGEN-CORPUS4:LADDER" s" CODEGEN-CORPUS4:LADDER-N"
   [: 1000 CODEGEN-CORPUS4:LADDER-N drop ;]
   [: -5 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      3 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      20 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      100 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      127 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR
      1000 CODEGEN-CORPUS4:LADDER-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ ---- the four loop rows with no call in them ----------------------------------

: BIG-CONSTS-CASE ( -- )
   s" CODEGEN-CORPUS4:BIG-CONSTS" s" CODEGEN-CORPUS4:BIG-CONSTS-N"
   [: CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:BIG-CONSTS-N drop ;]
   [: CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:BIG-CONSTS-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:BIG-CONSTS-N CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:BIG-CONSTS-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: MANY-LOCALS-CASE ( -- )
   s" CODEGEN-CORPUS4:MANY-LOCALS" s" CODEGEN-CORPUS4:MANY-LOCALS-N"
   [: 1 2 3 4 5 6 7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS-N drop ;]
   [: 1 2 3 4 5 6 7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS-N
      CODEGEN-COMPARE:VECTOR
      1 2 3 4 5 6 7 8 0 CODEGEN-CORPUS4:MANY-LOCALS-N CODEGEN-COMPARE:VECTOR
      -1 2 -3 4 -5 6 -7 8 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:MANY-LOCALS-N
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

: FLOAT-MIX-CASE ( -- )
   s" CODEGEN-CORPUS4:FLOAT-MIX" s" CODEGEN-CORPUS4:FLOAT-MIX-N"
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:FLOAT-MIX-N drop ;]
   [: 0 CODEGEN-CORPUS4:LOOP-LEN CODEGEN-CORPUS4:FLOAT-MIX-N CODEGEN-COMPARE:VECTOR
      0 0 CODEGEN-CORPUS4:FLOAT-MIX-N CODEGEN-COMPARE:VECTOR
      -100 3 CODEGEN-CORPUS4:FLOAT-MIX-N CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

\ The one row whose point is a side effect: what it returns is the cell it wrote,
\ and the cell after it is recorded too, because the step must not have touched
\ that one. The recorded call refills the cell first, for the reason the old
\ column's does: a step is not idempotent, and the timing loop leaves the cell far
\ from where it started.
: STORE-LOAD-CASE ( -- )
   s" CODEGEN-CORPUS4:STORE-LOAD" s" CODEGEN-CORPUS4:STORE-LOAD-N"
   [: CODEGEN-CORPUS4:STEP-AT CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:STORE-LOAD-N drop ;]
   [: CODEGEN-CORPUS4:S-RESET
      CODEGEN-CORPUS4:STEP-AT CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:STORE-LOAD-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:STEP-CELL@ CODEGEN-COMPARE:VECTOR
      1 CODEGEN-CORPUS4:STEP-CELL@ CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS4:S-RESET
      CODEGEN-CORPUS4:STEP-AT 0 CODEGEN-CORPUS4:STORE-LOAD-N
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

public

\ Measure what the chain can express, declare the rest, and check that between
\ them they account for all of it. Runs after the old column, whose rows every
\ name here is checked against.
: RUN ( -- )
   CODEGEN-GAP:RESET
   GAPS
   NOOP-CASE
   CALL-FAN-CASE
   CALL-LOOP-3-CASE
   TINY-CALLEE-CASE
   WIDE-ARITY-CASE
   LADDER-CASE
   BIG-CONSTS-CASE
   MANY-LOCALS-CASE
   FLOAT-MIX-CASE
   STORE-LOAD-CASE
   CODEGEN-GAP:COVERAGE-CK ;

;package
