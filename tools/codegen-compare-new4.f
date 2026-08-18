\ codegen-compare-new4.f - the new code generator's column of the FOURTH
\ comparison. One concern: what the new chain makes of the corpus built to beat
\ it, and what it refuses.
\
\ TWELVE ROWS AND NO GAP. Every subject of this corpus compiles, so there is
\ nothing here for CODEGEN-GAP:GAP to declare - the shape corpus 5 has always
\ had. PRESSURE-LOOP was the one row that did not compile, and it is what this
\ file's account below is about.
\
\ THE REFUSAL THAT WAS, AND WHAT ACTUALLY CLOSED IT. The corpus's own text, handed
\ to the same migration entry every other row here uses, threw -8508
\ E-A64RA-SPILL:
\
\     : PRESSURE-LOOP-N ( ptr n n -- n ) {: base:ptr len:n :}
\        0 len 0 ?do base @ base 8 + @ ... base 104 + @ + + + + + + + + + + + + + +
\        loop ;
\
\ It was never the budget. The same body with THIRTEEN fields compiled where the
\ fourteen-field body was refused, at every budget it was tried under, and the
\ pool is no longer a number anybody states: NABI:SCRATCH answers it. The wall was
\ fourteen values live at once inside a LOOP BODY, exactly where
\ src/compiler/native/regalloc.f says it is: MB-SPILLABLE? will not take a class
\ defined or read in a block that is neither the entry nor the exit, because a
\ frame access there would sit where the memory order cannot be stated.
\
\ SO THE ANSWER WAS NOT TO FIND THE VALUES A PLACE BUT TO STOP THE BODY HOLDING
\ THEM. Nothing in this body writes memory, calls, or can trap, and the address
\ each read computes cannot change with the turn - so the reads answer the same
\ bytes every turn and are work the loop repeats for no reason.
\ src/compiler/native/loop.f moves them, and the thirteen additions between them,
\ into the pre-header; what is left is one addition into one accumulator, which is
\ the shape that pass already replaces with the arithmetic the loop would have
\ computed. The row holds no loop and never holds fourteen values inside one,
\ which is also what the C twin does with the same program.
\
\ AND THE PRE-HEADER HAS A WALL OF ITS OWN, WELL ABOVE THIS ROW.
\ tools/codegen-spill-probe.f straddles it: twenty fields read a turn compile,
\ and above that the shape meets the migration context's mapping and the register
\ allocator in turn, which is why that file asserts a CODE and not just a
\ refusal. This corpus's fourteen sit well inside all of it.
\
\ CALL-PRESSURE WAS THE SECOND GAP AND IS NOW A ROW, AND WHAT CLOSED IT IS NOT
\ THE ALLOCATOR. That row holds eight values across a call inside a loop, and it
\ was refused with the same -8508 for as long as the elaborator handed EVERY
\ local a call could reach over at the call - operand, result, and a block
\ argument of every block on the path - whatever the callee did with its
\ registers. Handing a value over is what buys it a data-stack slot, and it is
\ worth buying only when there is no register the value could have stayed in.
\ There is one exactly when the callee published what it destroys
\ (src/compiler/native/clobber.f), so the elaborator now asks
\ (src/compiler/native/elaborate.f CALL-KEEPS?) and hands the local over only
\ when the answer is no. This row's callee is the chain's own C-LONG-N, which
\ published a record, so its eight values stay in registers the callee leaves
\ alone and the body compiles.
\
\ WHICH IS NOT THE SAME AS SAYING THE CROSSING NEVER COSTS ANYTHING. Against a
\ callee with NO record - every word the engine's own emitter compiled, and every
\ `execute` - the values still travel and the wall on this shape is still there,
\ one value lower. tools/codegen-spill-probe.f straddles both walls with the
\ corpus's own C-LONG compiled each way and pins both counts, so what the record
\ is worth is a measurement in a suite and not a sentence here.
\
\ THE WALL ON THIS SHAPE MOVED BY ONE BEFORE THAT, AND THE ROW WAS RE-DERIVED
\ RATHER THAN RE-PINNED. It was at seven until the selection stage began emitting
\ the add and subtract immediate forms, and then seven compiled - measured
\ through the migration entry, with the mechanism deliberately not guessed at;
\ the survey in tools/codegen-spill-probe.f says why. A row whose gap declaration
\ survived that would have been a false statement about the chain. The row was
\ therefore given an eighth surviving value - the trip count, read after the
\ loop, so the arity it is measured at did not change. It is a row now, and what
\ it measures is that the eight cross and cost nothing.
\
\ ============================================================================
\ WHERE THE CHAIN LOST, WHICH IS WHY THIS CORPUS EXISTS, AND WHAT CLOSED IT
\ ============================================================================
\ NONE of the ten compiled rows costs more than the code the engine's emitter
\ wrote for the same body. That was not true when this corpus was written: it was
\ built to find the shapes the chain handles worse, it found two, and both of them
\ turned on the same thing.
\
\ ON BYTES IT IS NOT NONE, AND CALL-FAN-BIG IS WHERE IT IS NOT. The chain's word
\ for that body is 88 bytes against the engine's 36, and the reason is the same
\ inlining decision that wins it the time: five copies of `3 * 5 +` are four
\ instructions each, and the five Bl they replace are one instruction each -
\ residency and placement having left the engine's call sites with no store, no
\ load and no pointer move to make. It is a declared known loss in
\ tools/codegen-compare-report.f. It used to name two dots that would close it,
\ and it names none now: both landed, they are what made those call sites one
\ instruction, and the size rule's re-derivation
\ (habu-measure-inline-cost-031e817e) concluded that no rule read while the
\ CALLEE is published can promise a copy is never bigger. The row is what the
\ rule's stated bound looks like at its widest, and it comes off this list only
\ if the rule tightens past this callee's body.
\
\ THE TWO ROWS, AND THE THREE STAGES THEY WENT THROUGH. Measured on an idle
\ twelve-core Apple Silicon host with the entry cost taken off, over five passes:
\
\                      when the corpus was written  after the call-save          after a small callee is
\                                                   narrowing                    copied into its caller
\     TINY-CALLEE      old 58.6-60.8  new 72.6-73.4  old 56.0  new 70.7           old 59.1-61.4  new 5.49-5.80
\     CALL-LOOP-3      old 45.9-48.8  new 61.4-65.5  old 46.7-52.1  new 48.5-48.8 old 49.4-52.1  new 5.79-5.97
\     CALL-FAN         old 4.96-5.53  new 5.26-6.00  unchanged                    old 5.23-5.45  new 0.36-0.38
\
\ WHAT THE FIRST STAGE WAS. Nothing in a Habu word's convention is callee-saved,
\ so a call site used to put every live value out to a data-stack slot and read it
\ back. It now puts out only the ones the callee can really reach: a routine the
\ chain publishes records which registers its accepted allocation writes
\ (src/compiler/native/clobber.f), the allocator keeps a value that crosses a call
\ out of those registers, and the site saves the rest. That took CALL-LOOP-3 from
\ about 1.33x slower to level and left TINY-CALLEE at about 1.20x.
\
\ AND WHAT THE SECOND WAS: THE CHAIN COPIES A SMALL CALLEE TOO, UNDER A RULE OF
\ ITS OWN. The rule the ENGINE applies is written out at the head of
\ tools/codegen-compare-corpus4.f - a callee of forty bytes of body or less, with
\ no branch in it, is copied into its caller verbatim - and forty bytes is a fact
\ about that emitter. The chain's rule is derived from its own calling convention
\ instead: a copy costs the callee's measured BODY, a call site of arity
\ (in -> out) costs at most one store per argument, one load per result, the
\ branch and the two pointer moves, and a routine is small when its body is
\ within that bound. src/compiler/native/inline.f states the arithmetic and
\ argues it; nothing here restates it. Such a body is recorded when the routine
\ is published (src/compiler/native/inline.f) and elaborated at the call site out
\ of the caller's own values (src/compiler/native/elaborate.f). Every callee this
\ corpus has qualifies under both rules.
\
\ WHAT IT REMOVES, COUNTED ON THE EMITTED CODE. Not just the branch: the whole
\ interface on both sides, and with it everything the CALLER was doing on account
\ of having a call in it at all - no frame, no saved link register, and no loop
\ counter or local travelling across every edge.
\
\     CALL-LOOP-3-N   316 bytes, 19 stores and 23 loads  ->  120 bytes, 1 and 5
\     TINY-CALLEE-N   232 bytes, 13 stores and 14 loads  ->  104 bytes, 1 and 2
\     CALL-FAN-N      132 bytes,  6 stores and  6 loads  ->   56 bytes, 1 and 1
\
\ The counts left are each word's own entry and exit and nothing else - one load
\ per declared argument and one store per declared result - which is the floor for
\ a word the engine's callers enter through the data stack. Counted by
\ tools/codegen-compare-test.f, whose Bl scan now reports ZERO branch-with-link
\ instructions in both columns for all three bodies.
\
\ THE DEAD QUARTET IS GONE WITH IT. A call site used to publish a value into a
\ slot the callee immediately read back out of the same slot:
\
\     sub x19, x19, #8      the data-stack pointer steps back over the value
\     ldr x0, [x19]         the value is read out of the slot
\     str x0, [x19]         and written straight back into the SAME slot
\     add x19, x19, #8      and the pointer steps forward again
\
\ Four instructions whose net effect on the machine was nothing, five times over
\ in CALL-FAN-N. Dot habu-fuse-a-call-36c0286e carries fusing such a pair where a
\ call really is emitted, and it is still open: what removed them HERE is that
\ there is no call at these sites any more.
\
\ ============================================================================
\ WHERE THE SUSPECTED WEAKNESS DID NOT BITE, AND WHY
\ ============================================================================
\ Eight rows were designed to be losses. ONE of them is a DRAW, six are wins, and
\ one - CALL-FAN-BIG, which was added after the others and is the only one of them
\ whose callee the engine will not copy - is a win on time and a LOSS on bytes.
\ In every one of the wins the same thing happens: the engine's emitter is a stack
\ machine and moves every intermediate through memory, so a row whose body does
\ real work pays the engine far more than the shape the row was built to punish
\ costs the chain.
\
\   CALL-FAN      A WIN NOW, and the row whose answer moved furthest. It was a
\                 DRAW for as long as the chain emitted five calls where the
\                 engine emitted none: old 4.96-5.53 ns against new 5.26-6.00 ns,
\                 with the chain ahead in one pass out of five, because the dead
\                 quartet at each boundary was nearly free in TIME - a store and a
\                 load to one address that the core forwards - while the engine's
\                 five copied bodies were fifty real instructions. What the chain
\                 was really paying there was SIZE, 132 bytes against 216. Copying
\                 the same five bodies takes it to 56 bytes and 0.36-0.38 ns
\                 against the engine's 5.23-5.45: the five copies are twelve
\                 instructions in all, because the caller's own registers are
\                 where each result already is.
\   CALL-FAN-BIG  THE ONE THAT BIT, and on the column nobody was watching. Its
\                 callee is two operations rather than one, which is past the
\                 engine's forty bytes of body and still inside the chain's own
\                 rule, so the engine emits five Bl - four bytes each, 36 with the
\                 frame - and the chain copies four instructions of body into
\                 five sites for 88. The chain is far faster for it and that is
\                 not the point: this row exists because the corpus's other call
\                 rows all use a callee BOTH generators copy, so none of them can
\                 see what a call site costs when the two rules disagree.
\   LADDER        a draw: eight compares and eight branches are eight compares
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
\ AND THE ONE SENTENCE THE WHOLE CORPUS COMES TO. The new chain used to lose where
\ a call was made and there was nothing else in the body to hide it, and nowhere
\ else. That was the corpus's finding, it was acted on twice - once by narrowing
\ what a call site saves and once by not making the call at all - and there is no
\ row of the four corpora left where the chain costs more.
\
\ ON SIZE IT DOES NOT WIN EVERY ROW, AND SAYING SO USED TO BE THE GAP. This
\ paragraph used to end "it still wins every row on size", which was already
\ untrue of CODEGEN-CORPUS2:T-RES-WALK when it was written: the byte columns were
\ printed side by side and compared by nothing, so the sentence a reader saw -
\ "rows the new column costs more on: none" - was only ever about a clock. The
\ byte columns are adjudicated now (tools/codegen-compare-report.f), the two rows
\ that lose are named there with the dot each dies with, and a third would fail
\ the run.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-core.f
require tools/codegen-compare-gap.f
require tools/codegen-compare-calibrate.f
require tools/codegen-compare-corpus.f
require tools/codegen-compare-corpus4.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated4.f

package CODEGEN-NEW4

private

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

\ Five sites over the callee the engine calls and the chain copies. The one row
\ of the four corpora where the chain's word is the LARGER of the two.
: CALL-FAN-BIG-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-FAN-BIG" s" CODEGEN-CORPUS4:CALL-FAN-BIG-N"
   [: 7 CODEGEN-CORPUS4:CALL-FAN-BIG-N drop ;]
   [: 7 CODEGEN-CORPUS4:CALL-FAN-BIG-N CODEGEN-COMPARE:VECTOR
      0 CODEGEN-CORPUS4:CALL-FAN-BIG-N CODEGEN-COMPARE:VECTOR
      -1 CODEGEN-CORPUS4:CALL-FAN-BIG-N CODEGEN-COMPARE:VECTOR
      255 CODEGEN-CORPUS4:CALL-FAN-BIG-N CODEGEN-COMPARE:VECTOR ;]
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

\ Eight values live across a call the loop really makes, read only after it. The
\ inputs are the old column's, and the third of them runs every crossing value
\ negative so a lost sign on a value that travelled would show.
: CALL-PRESSURE-CASE ( -- )
   s" CODEGEN-CORPUS4:CALL-PRESSURE" s" CODEGEN-CORPUS4:CALL-PRESSURE-N"
   [: 1 2 3 4 5 6 7 9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE-N drop ;]
   [: 1 2 3 4 5 6 7 9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE-N CODEGEN-COMPARE:VECTOR
      1 2 3 4 5 6 7 9 0 CODEGEN-CORPUS4:CALL-PRESSURE-N CODEGEN-COMPARE:VECTOR
      -1 -2 -3 -4 -5 -6 -7 -9 CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:CALL-PRESSURE-N CODEGEN-COMPARE:VECTOR ;]
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

\ ---- the five loop rows with no call in them ----------------------------------

\ Fourteen fields of one record read and added a turn. The pinned inputs are the
\ old column's, and the second of them is a trip count of zero, which is the edge
\ the closed form has to answer without running anything.
: PRESSURE-LOOP-CASE ( -- )
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" s" CODEGEN-CORPUS4:PRESSURE-LOOP-N"
   [: CODEGEN-CORPUS4:REC CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:PRESSURE-LOOP-N drop ;]
   [: CODEGEN-CORPUS4:REC CODEGEN-CORPUS4:LOOP-LEN
      CODEGEN-CORPUS4:PRESSURE-LOOP-N CODEGEN-COMPARE:VECTOR
      CODEGEN-CORPUS4:REC 0 CODEGEN-CORPUS4:PRESSURE-LOOP-N
      CODEGEN-COMPARE:VECTOR ;]
   CODEGEN-COMPARE:MEASURE-NEW ;

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

: COVERED-CASES ( -- )
   CODEGEN-CALIBRATE:NEW
   CALL-FAN-CASE
   CALL-FAN-BIG-CASE
   CALL-LOOP-3-CASE
   TINY-CALLEE-CASE
   CALL-PRESSURE-CASE
   WIDE-ARITY-CASE
   LADDER-CASE
   PRESSURE-LOOP-CASE
   BIG-CONSTS-CASE
   MANY-LOCALS-CASE
   FLOAT-MIX-CASE
   STORE-LOAD-CASE ;

public

\ Measure what the chain can express, declare the rest, and check that between
\ them they account for all of it. Runs after the old column, whose rows every
\ name here is checked against.
: RUN ( -- )
   [: COVERED-CASES ;] CODEGEN-GAP:ACCOUNT ;

;package
