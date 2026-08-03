\ codegen-compare-corpus4.f - the FOURTH pinned word corpus the codegen
\ comparison measures: ten shapes chosen to make the new code generator LOSE.
\ One concern: the subject words themselves, and the structural advantage each
\ one is built to hand the old emitter.
\
\ WHY A FOURTH CORPUS AND NOT MORE ROWS IN THE OTHERS. The reason the third is
\ not more rows in the second, written out at the head of
\ tools/codegen-compare-corpus2.f: a committed table is the yardstick a compiler
\ change is read against, and adding a row to one is a change to the artifact the
\ measurement is compared with. This file is a corpus of its own with a committed
\ table of its own, measured by the same engine in the same run. None of the
\ first three tables moves.
\
\ WHY IT EXISTS, WHICH IS THE ONLY THING THAT MAKES IT DIFFERENT FROM THE OTHER
\ THREE. The first three corpora were surveys: the smallest honest example of
\ each shape, the words this system really runs hot, the float bodies the next
\ campaign needs. Every one of them was chosen before anybody knew which code
\ generator would win it, and on the three tables together the new chain wins
\ almost everything. A benchmark suite that its subject wins is exactly the suite
\ that has stopped doing work, so this corpus is chosen the other way round: each
\ row is a shape somebody had a reason to believe the new chain handles WORSE
\ than the engine's emitter, and the row exists to find out whether that is true.
\
\ A ROW THE NEW CHAIN WINS IS THEREFORE NOT A FAILURE OF THE ROW. It is the
\ answer to the question the row asked, and the account in
\ tools/codegen-compare-new4.f says for each of them why the suspected weakness
\ did not bite. What would be a failure is a row nobody could lose - a shape
\ picked because it flatters, or an input picked after the numbers were seen.
\ Every input below is pinned before the fact and every body is one both code
\ generators compile from the same text.
\
\ ============================================================================
\ THE OLD EMITTER'S ONE STRUCTURAL ADVANTAGE, MEASURED FROM ITS OWN SOURCE
\ ============================================================================
\ Everything in this corpus that turns on calls turns on this rule, so it is
\ written down here with the source that decides it and the measurement that
\ confirms it.
\
\ THE RULE. src/habu/habu2.f, C-CALL, is what the engine emits at a call site in
\ compile mode. It is handed the callee's code address in x11 and the callee's
\ code length in x12, and it decides between copying the callee's body into the
\ caller VERBATIM and emitting one direct BL. The threshold is one named
\ constant, `$28 constant INL-MAX` - forty bytes, ten AArch64 instructions - and
\ it is applied two ways:
\
\   a callee that OPENS WITH A PROLOGUE. C-CALL-BRANCH-NO-PROLOGUE reads the
\   first word at the callee and compares it with `$D10043FF`, which is
\   `sub sp, sp, #16`. When it matches, C-CALL-PROLOGUE-SPAN requires
\   `clen <= INL-MAX + 16` and takes the span [entry+8, entry+clen-8) - the body
\   with the two prologue words and the two epilogue words stripped. So the
\   inlined span is clen-16 bytes and the ceiling on it is exactly forty.
\
\   a callee with NO PROLOGUE. C-CALL-PLAIN-SPAN requires `clen <= INL-MAX` and
\   takes the whole of [entry, entry+clen), and C-CALL-REQUIRE-RET-SLOT then
\   demands that the word AT entry+clen still be `$D65F03C0`, a Ret. A word whose
\   return slot has been patched - which is what `does>` does - is never inlined.
\
\ Either way C-CALL-SCAN-SAFE then walks the span one instruction at a time and
\ gives up on the copy, falling back to the BL, if it meets a Bl, a B, a B.cond,
\ a Cbz/Cbnz, a Tbz/Tbnz, a Blr, a Br, a Ret or an Adr/Adrp. Only straight-line,
\ position-independent code is copied. C-CALL-COPY-INLINE then emits the span's
\ instructions into the caller one by one.
\
\ THE MEASUREMENT THAT CONFIRMS IT, taken with bin/hb on the four callees this
\ file publishes. Each of them is a literal and one binary operation, and each
\ occupies 56 bytes: sixteen of prologue and epilogue and forty of body, which is
\ INL-MAX exactly. CALL-FAN below calls five of them in a row and comes out at
\ 216 bytes = 5 * 40 + 16, with no Bl in it at all - every one of the five was
\ copied. Add a fifth instruction to a callee and it is 68 bytes, the body is 52,
\ and the call site emits a Bl instead.
\
\ WHAT THE NEW CHAIN DOES INSTEAD. It emits a Bl for every call, at every size,
\ always. There is no inliner in src/compiler/native/. That is the advantage the
\ call rows of this corpus are built to hand the engine, and the rows are sized
\ so that it is the call and not the arithmetic that is being measured: every
\ callee here is one the engine copies.
\
\ ============================================================================
\ THE TEN ROWS AND THE WEAKNESS EACH ONE IS LOOKING FOR
\ ============================================================================
\
\   CALL-FAN        five call sites over four callees the engine copies
\                   verbatim, in a straight line. The purest form of the
\                   inlining advantage: the engine emits no call at all and the
\                   chain emits five.
\   CALL-LOOP-3     three of those calls per turn of a counted loop, with three
\                   locals live across the whole loop and used only after it.
\                   Nothing in a Habu word's convention is callee-saved, so the
\                   chain has to put every live value somewhere safe at every
\                   call; the locals are there to make "every live value" a
\                   bigger number without adding any work to the loop body.
\                   It is the third corpus's T-SGD! shape with the arithmetic
\                   taken out, so that what is left is the discipline.
\   WIDE-ARITY      six arguments and five operations. A word's arguments arrive
\                   in the caller's data-stack slots, so a wide entry is one load
\                   per argument before anything computes; this row asks whether
\                   that entry cost is where the chain gives ground.
\   LADDER          eight sequential guarded early exits - src/core/checker.f's
\                   INT-WIDENS? shape - so the row is eight compares, eight
\                   branches and eight returns' worth of control flow with almost
\                   no data flow.
\   PRESSURE-LOOP   fourteen values loaded and held live at once inside a counted
\                   loop, which is more than the machine has registers for. The
\                   row asks what the chain does when it must put a loop-carried
\                   value in a frame slot.
\   BIG-CONSTS      four distinct sixty-four-bit literals per turn. Neither
\                   compiler has a constant pool, so each one is a chain of four
\                   move-wide instructions, four times a turn.
\   MANY-LOCALS     eight locals live across a counted loop and read one at a
\                   time inside it.
\   TINY-CALLEE     the smallest callee this corpus has, called four times per
\                   turn of a counted loop and nothing else in the body. The
\                   ratio of call overhead to useful work is as bad as a real
\                   program can make it.
\   FLOAT-MIX       an integer becomes a double and comes back every turn, so the
\                   loop crosses between the two register files twice a turn.
\   STORE-LOAD      a load and a store to ONE address every turn, each turn
\                   depending on the last. A memory-ordering row: there is
\                   nothing here a scheduler can move.
\
\ WHAT IS DELIBERATELY NOT HERE. No row is a program nobody would write, and no
\ row was tuned after its numbers were read. Two shapes that would have been
\ larger losses were left out on purpose: a body with five DIFFERENT callees,
\ because the migration entry stages at most four (src/compiler/native/migrate.f,
\ `4 constant CALLEES-MAX`, and a fifth is refused with E-NMIGRATE-STATE), and a
\ body of more than ten arguments, because a routine's place list holds ten
\ (src/compiler/a64-effect.f, SEQ-MAX-N, and an eleventh is refused with
\ E-A64EFF-SEQ). Both refusals are checked in tools/codegen-compare-test.f rather
\ than described here, and both have dots. Building a row on either would have
\ measured a ceiling in the migration entry instead of a code generator.
\
\ THE CORPUS IS PINNED, exactly as the first three are. Do not "improve" a word
\ here. Adding a word adds a row; changing a body invalidates the recorded
\ baseline for that word and must be done in the same change that regenerates
\ it, with the reason written down.
\
\ THE ROW WHOSE POINT IS A SIDE EFFECT is STORE-LOAD, and the cell it steps is
\ this file's own storage, published for reading - the discipline CELL-BUMP
\ established in the first corpus and T-SGD! kept in the third. Both columns step
\ the SAME cell. Because a step is not idempotent, the cell is refilled by
\ S-RESET before the recorded call, so a second measurement pass records what the
\ first one did rather than the state the timing loop left behind.

require lib/prelude.f

package CODEGEN-CORPUS4

public

\ ---- how much of the pinned data each row is measured over --------------------
\ Public because the case list hands these to the subjects, the way the second
\ corpus publishes COPY-LEN: the length a row is measured at is part of its
\ pinned input and belongs beside the data rather than retyped in another file.
\
\ Eight turns rather than the four the third corpus uses. Every loop row here is
\ measured against a row of the same corpus that has no loop in it, and eight
\ turns puts the loop body far enough above the entry that what the two columns
\ differ by is the body.
8 constant LOOP-LEN

\ The record PRESSURE-LOOP reads. Fourteen fields, because thirteen held live at
\ once is the most the chain places and fourteen is one more - the survey that
\ established that is at the head of tools/codegen-compare-new4.f.
14 constant ROW-CELLS

private

\ ---- the pinned data the rows are measured over ------------------------------
\ The record carries a negative field and a zero, so a sum that lost a sign or
\ skipped a field is visible in the recorded output rather than absorbed.
create ROW-REC ROW-CELLS cells allot

\ The cell STORE-LOAD steps, and one cell after it that the step must not touch:
\ a loop that ran one turn too far, or that stepped the wrong address, shows up
\ as a changed output rather than as nothing at all.
create STEP-CELL 2 cells allot

public

\ ---- the four callees the call rows reach through -----------------------------
\ Each is one literal and one binary operation, which is 56 bytes of the engine's
\ code - sixteen of prologue and epilogue and forty of body, INL-MAX exactly - so
\ each is a callee the engine copies into its caller verbatim rather than calling.
\ That is the whole point of them: they are not rows and they are not measured,
\ they are the thing the call rows call.
\
\ THEY ARE FOUR AND NOT FIVE because the migration entry stages at most four
\ callees for one definition. CALL-FAN below therefore has five call SITES over
\ these four, which is the same question about the same rule.
: C-ADD1 ( n -- n ) 1 + ;
: C-MUL2 ( n -- n ) 2 * ;
: C-AND7 ( n -- n ) 7 and ;
: C-XOR5 ( n -- n ) 5 xor ;

\ ---- the ten subjects --------------------------------------------------------

\ Five call sites in a straight line, over the four callees above. The engine
\ copies all five bodies in and emits no call; the chain emits five.
: CALL-FAN ( n -- n )
   C-ADD1 C-MUL2 C-AND7 C-XOR5 C-ADD1 ;

\ Three calls per turn with three locals live across the whole loop. The locals
\ are read only after the loop, so they add nothing to the body and everything to
\ what is live at each call.
: CALL-LOOP-3 ( n n n n n -- n ) {: a:n b:n c:n seed:n len:n :}
   seed
   len 0 ?do  C-ADD1 C-MUL2 C-XOR5  loop
   a + b + c + ;

\ Six arguments, five operations. The subtractions are paired so that no
\ argument is read twice and none is dead: a row that dropped one would answer
\ differently on the pinned inputs.
: WIDE-ARITY ( n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n :}
   a b -  c d - +  e f - + ;

\ src/core/checker.f:1285's INT-WIDENS? shape as an analogue: a chain of guarded
\ early exits, each one a compare against a literal and a return. The real one
\ has nine and mixes three different tests; this has eight of one test, because
\ what the row measures is the CHAIN and not which relation each link asks.
: LADDER ( n -- n ) {: x:n :}
   x 1 < if 0 exit then
   x 2 < if 1 exit then
   x 4 < if 2 exit then
   x 8 < if 3 exit then
   x 16 < if 4 exit then
   x 32 < if 5 exit then
   x 64 < if 6 exit then
   x 128 < if 7 exit then
   8 ;

\ Fourteen fields of one record loaded and held live at once, once per turn.
\ Every load happens before any of the additions, so all fourteen values are live
\ at the same instant, inside a block that is neither the one the routine is
\ entered through nor the one it leaves through.
: PRESSURE-LOOP ( ptr n n -- n ) {: base:ptr len:n :}
   0
   len 0 ?do
      base @  base 8 + @  base 16 + @  base 24 + @  base 32 + @
      base 40 + @  base 48 + @  base 56 + @  base 64 + @  base 72 + @
      base 80 + @  base 88 + @  base 96 + @  base 104 + @
      + + + + + + + + + + + + + +
   loop ;

\ Four distinct sixty-four-bit literals a turn, each combined with the loop index
\ so that no turn computes what the last one did. The literals are written in
\ decimal because the tape a migration records cannot read a hexadecimal
\ spelling back (dot habu-record-the-engine-79c570ed), and writing them in
\ decimal HERE is what lets the new column compile this body's own text rather
\ than a respelling of it.
: BIG-CONSTS ( n -- n ) {: len:n :}
   0
   len 0 ?do
      i 1234605616436508552 xor +
      i 7378697629483820646 xor +
      i -6148914691236517206 xor +
      i 1311768467294899695 xor +
   loop ;

\ Eight locals live across a counted loop and read one at a time inside it.
: MANY-LOCALS ( n n n n n n n n n -- n ) {: a:n b:n c:n d:n e:n f:n g:n h:n len:n :}
   0
   len 0 ?do  a + b + c + d + e + f + g + h +  loop ;

\ Four calls a turn and nothing else in the body. The callee is the smallest one
\ this corpus has, so the ratio of what a call costs to what it computes is the
\ worst a body can honestly make it.
: TINY-CALLEE ( n n -- n ) {: seed:n len:n :}
   seed
   len 0 ?do  C-ADD1 C-ADD1 C-ADD1 C-ADD1  loop ;

\ An integer becomes a double, is scaled, and comes back an integer, once a turn.
\ Both crossings are in the loop, so the body moves a value between the two
\ register files twice every turn.
: FLOAT-MIX ( n n -- n ) {: seed:n len:n :}
   seed
   len 0 ?do  i s>f 0.5 f* f>s +  loop ;

\ One address, read and written every turn, each turn depending on the last.
\ There is no reordering available to either compiler: the load of turn k+1 is
\ the store of turn k.
: STORE-LOAD ( ptr n n -- n ) {: cell:ptr len:n :}
   len 0 ?do  cell @ 3 +  cell !  loop
   cell @ ;

\ ---- what the harness reads the pinned data through --------------------------

: REC ( -- ptr n ) ROW-REC ;
: STEP-AT ( -- ptr n ) STEP-CELL ;

\ The two cells the step is recorded through: the one it writes and the one after
\ it, which it must not have.
: STEP-CELL@ ( n -- n ) {: k:n :}
   STEP-CELL k cells + @ ;

private

\ The pinned fill, in one place, so that the cell a step is recorded over is the
\ same cell every time. The timing loop below runs the step nearly two million
\ times and leaves it far from where it started, which is harmless precisely
\ because the recorded call refills it first.
: FILL-STEP ( -- )
   100 STEP-CELL !
   -7 STEP-CELL 1 cells + ! ;

public

: S-RESET ( -- )
   FILL-STEP ;

private

\ The record's fields: a negative, a zero, and twelve values no two of which are
\ equal, so a sum that dropped or doubled a field answers differently.
: FILL-REC ( -- )
   ROW-CELLS 0 ?do  i 7 * 3 +  ROW-REC i cells + !  loop
   -11 ROW-REC 5 cells + !
   0 ROW-REC 9 cells + ! ;

public

: SETUP ( -- )
   FILL-REC
   FILL-STEP ;

SETUP

;package
