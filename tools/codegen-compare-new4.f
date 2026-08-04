\ codegen-compare-new4.f - the new code generator's column of the FOURTH
\ comparison. One concern: what the new chain makes of the corpus built to beat
\ it, and what it refuses.
\
\ TWELVE ROWS AND TWO GAPS. Ten of the twelve subjects compile; PRESSURE-LOOP and
\ CALL-PRESSURE do not, and what stops each is a refusal by name rather than a
\ shortage of anything a bigger number would buy. Both rows below are declared
\ against the `loop-spill` capability, and the account in
\ tools/codegen-compare-gap.f says what that word covers and why it is a
\ capability rather than register pressure in general.
\
\ THE TWO REFUSALS ARE ONE REFUSAL REACHED TWO WAYS, which is why they name one
\ capability. PRESSURE-LOOP holds fourteen values live inside a loop and
\ CALL-PRESSURE holds seven across a call inside one; both are values the
\ allocator has to put somewhere other than a register while control is in the
\ body of a loop, and MB-SPILLABLE? refuses exactly that. The second row exists
\ because the first reaches the wall with a shape nobody writes - fourteen live
\ loads - while seven values live across a call in a loop is an ordinary program,
\ and a capability whose only witness is a contrived body is easy to leave.
\
\ THE FIRST REFUSAL, MEASURED. The corpus's own text, handed to the same migration
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
\ AND THE SECOND REFUSAL, MEASURED THE SAME WAY. CALL-PRESSURE's own text, at the
\ same eighteen registers, with the callee the chain published beside it so that
\ what is being refused is the body and not a missing routine:
\
\     : CALL-PRESSURE-N ( n n n n n n n n n -- n )
\        {: a:n b:n c:n d:n e:n f:n g:n seed:n len:n :}
\        seed len 0 ?do C-LONG-N loop a + b + c + d + e + f + g + ;
\                                          18 registers   threw -8508  E-A64RA-SPILL
\
\ It is not the budget here either, and the control is the neighbour: the same
\ body with SIX values live across the call instead of seven compiles at the same
\ eighteen. So the wall on this shape is at seven, the same allocator rule puts it
\ there, and tools/codegen-compare-test.f runs both migrations and checks both
\ codes. The two rows together say the refusal is about WHERE a value has to live
\ and not about how many there are: fourteen without a call, seven with one.
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
\ for that body is 96 bytes against the engine's 36, and the reason is the same
\ inlining decision that wins it the time: five copies of `3 * 5 +` with the
\ interface of each site around them are more instructions than five Bl. It is a
\ declared known loss in tools/codegen-compare-report.f, which names the two dots
\ that close it - habu-keep-a-pass-8025401f keeps a value in a register across the
\ site and habu-place-the-data-9f128e58 places the copied body's values in the
\ caller's own - and the entry has to be deleted when they land, because an entry
\ that has stopped losing is itself a finding.
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
\ instead: a call to a routine of arity (in -> out) costs `in + out + 3`
\ instructions at the site and the same again inside the routine, so a routine
\ whose whole emission is no longer than both halves together has a body no longer
\ than the call site's own half - and copying it in cannot make the site bigger.
\ Such a body is recorded when the routine is published
\ (src/compiler/native/inline.f) and elaborated at the call site out of the
\ caller's own values (src/compiler/native/elaborate.f). Every callee this corpus
\ has qualifies under both rules.
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
\                 frame - and the chain copies the body and its interface into
\                 five sites for 96. The chain is far faster for it and that is
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

\ ---- the one declaration ------------------------------------------------------
\ The row the chain refuses, against the capability it waits for. The refusal is
\ E-A64RA-SPILL at the largest register pool the architecture allows, and
\ tools/codegen-compare-test.f is where that is checked rather than described.
: GAPS ( -- )
   s" CODEGEN-CORPUS4:PRESSURE-LOOP" CODEGEN--GAP-CAP:LOOP-SPILL CODEGEN-GAP:GAP
   s" CODEGEN-CORPUS4:CALL-PRESSURE" CODEGEN--GAP-CAP:LOOP-SPILL CODEGEN-GAP:GAP ;

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

: COVERED-CASES ( -- )
   CODEGEN-CALIBRATE:NEW
   CALL-FAN-CASE
   CALL-FAN-BIG-CASE
   CALL-LOOP-3-CASE
   TINY-CALLEE-CASE
   WIDE-ARITY-CASE
   LADDER-CASE
   BIG-CONSTS-CASE
   MANY-LOCALS-CASE
   FLOAT-MIX-CASE
   STORE-LOAD-CASE ;

public

\ Measure what the chain can express, declare the rest, and check that between
\ them they account for all of it. Runs after the old column, whose rows every
\ name here is checked against.
: RUN ( -- )
   [: GAPS COVERED-CASES ;] CODEGEN-GAP:ACCOUNT ;

;package
