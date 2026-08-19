\ codegen-compare-corpus5.f - the FIFTH pinned word corpus the codegen
\ comparison measures: six shapes about ONE thing, a call in tail position.
\ One concern: the subject words themselves, and where each one puts its call.
\
\ WHY A FIFTH CORPUS AND NOT MORE ROWS IN THE FOURTH. The reason the fourth is
\ not more rows in the second, written out at the head of
\ tools/codegen-compare-corpus2.f: a committed table is the yardstick a compiler
\ change is read against, and adding a row to one is a change to the artifact
\ the measurement is compared with. This file is a corpus of its own with a
\ committed table of its own, measured by the same engine in the same run. None
\ of the first four tables moves.
\
\ WHAT THIS CORPUS IS FOR, WHICH IS ALL SIX ROWS. A call in TAIL position - the
\ last thing a word does before it returns - needs none of the machinery a call
\ in the middle of a body needs. The caller has nothing left to do when the
\ callee answers, so the callee can answer the caller's own caller directly: the
\ frame can be given back BEFORE the branch, the link register need not be saved
\ at all, and the branch itself can be a plain B rather than a BL followed by a
\ return nobody reaches. WHEN THIS CORPUS WAS WRITTEN neither code generator did
\ any of that: both emitted the full interface at every site - reserve, save the
\ link, branch with link, load the link, release, return - and this corpus is the
\ measurement of that, row by row, so that the change which turns it into a
\ branch has a table it did not choose afterwards.
\
\ THE ROWS ARE NOT A SURVEY. Every one of them is the same question asked at a
\ different position: what does each generator emit for a call that IS in tail
\ position, and does it still emit the right thing for one that is not. There is
\ exactly one control row and it is named for what it is.
\
\ WHERE THE ANSWER LIVES AFTERWARDS. What either column emits for these bodies
\ NOW is its committed table and not this comment: the engine's in
\ the engine's committed table, which did not move, and the
\ chain's in test/compiler/codegen-chain-baseline5.txt, which is re-pinned
\ deliberately whenever the chain gets smaller. A reader who wants today's number
\ reads the table; what is written here is the question the rows were built to
\ ask.
\
\ ============================================================================
\ THE TWO CALLEES, AND WHY NEITHER GENERATOR WILL COPY EITHER OF THEM
\ ============================================================================
\ A corpus about calls can only measure a call the compilers really emit, so
\ both callees here are deliberately on the far side of both inlining rules.
\
\ THE ENGINE'S RULE is written out in full at the head of
\ tools/codegen-compare-corpus4.f and is not repeated here: forty bytes of body,
\ straight-line only, decided in src/habu/habu2.f's C-CALL. C5-LONG is eleven
\ operations and 420 bytes of engine code, C5-PAIR is 128; both are far past it,
\ measured with bin/hb.
\
\ THE CHAIN'S RULE is derived from its own calling convention instead
\ (src/compiler/native/inline.f, which states the arithmetic): a copy costs the
\ callee's measured body, a call site of arity (in -> out) costs at most
\ `in + out + 3` instructions, and a routine is small when its body is within
\ that bound. C5-LONG is arity (1 -> 1), so the bound on it is five
\ instructions; the chain's own C5-LONG-N is sixty bytes, which is fifteen.
\ C5-PAIR is arity (2 -> 2) and the bound is seven; C5-PAIR-N is sixty-four
\ bytes, which is sixteen. Both are past their own bound by more than double.
\
\ AND THAT IS A MEASUREMENT AND NOT A DERIVATION. Both columns' code for every
\ row of this corpus really does contain a branch-with-link, counted off the
\ emitted instructions in the old comparison's member: one for each of the six
\ single-call rows and two for TAIL-AFTER. A callee either was copied or was
\ not, and the instruction says which.
\
\ C5-LONG IS THE SAME BODY AS THE FOURTH CORPUS'S C-LONG, DELIBERATELY AND NOT
\ BY REUSE. It is written out again here rather than called across the corpus
\ boundary because each corpus is a pinned artifact of its own: a row of this
\ table must not move because somebody edited another table's callee, and a
\ shared callee is exactly that coupling. The body is the one already known to
\ be past both rules, which is why it is the one chosen.
\
\ ============================================================================
\ THE SIX ROWS AND WHERE EACH ONE PUTS ITS CALL
\ ============================================================================
\
\   TAIL-BIG    the pure shape: the whole body is one call, and that call is the
\               last thing the word does. This is the row the corpus exists for.
\   TAIL-WORK   work BEFORE the tail call, so the argument the callee reads is
\               computed rather than handed straight through. A generator that
\               only recognised a body consisting of nothing but a call would
\               answer this row and TAIL-BIG differently.
\   NONTAIL     THE CONTROL, and the only row here that is not a tail call: the
\               same call with work AFTER it. The caller still needs its frame
\               and its saved link register, so its emitted bytes must NOT move
\               when a tail call stops being one. A change that shrank this row
\               shrank something it had no right to.
\   TAIL-MID    a second copy of the pure shape, and the callee of the next row.
\   TAIL-CHAIN  a tail call to a word that is itself nothing but a tail call:
\               A calls B calls C, twice in a row. Two frames' worth of
\               machinery stand between the entry and the arithmetic.
\   TAIL-PAIR   the same pure shape at arity (2 -> 2), so that the corpus does
\               not measure arity one only. What crosses the call is two values
\               and what comes back is two values, and the second of them is
\               what the C column has to read through an accessor - see the head
\               of the old comparison's fifth C column.
\   TAIL-AFTER  a real call FOLLOWED by a tail call. The routine still needs its
\               frame and its saved link register for the FIRST call, so the
\               tail branch has to come after the epilogue that gives them back.
\               It is the row that says the two decisions are separate: a
\               generator that turned the last call into a branch while leaving
\               the frame standing, or that gave the frame back before the first
\               call, answers this row wrongly rather than slowly.
\
\ WHAT IS DELIBERATELY NOT HERE. No loop, no float, no memory and no wide arity:
\ every one of those is measured by one of the first four corpora, and a row
\ that mixed them in would measure the mixture. Every row here is straight-line,
\ which is why every one of them states the same eight registers in
\ tools/codegen-compare-migrated5.f. Self-recursion is not here either: a word
\ that tail-calls ITSELF is a loop written as a call, and turning that one into
\ a branch is a different capability from turning a call to another word into
\ one. It gets its own row when the chain has it.
\
\ THE CORPUS IS PINNED, exactly as the first four are. Do not "improve" a word
\ here. Adding a word adds a row; changing a body invalidates the recorded
\ baseline for that word and must be done in the same change that regenerates
\ it, with the reason written down.

require lib/prelude.f

package CODEGEN-CORPUS5

public

\ ---- the two callees every row reaches through --------------------------------
\ Neither is a row and neither is measured: they are the thing the rows call, and
\ they are this size so that what a row measures is a CALL and not an inliner.
\ The head of this file gives the two rules and the measurement that puts both
\ callees past both of them.

: C5-LONG ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ The same weight at arity (2 -> 2). Eleven operations again, arranged so that
\ what is left on the stack is exactly two values: the sum, and a second value
\ derived from it through the mask and the exclusive-or. Both results depend on
\ both arguments, so a row that lost one argument, or read the two in the wrong
\ order, answers differently on the pinned pairs.
: C5-PAIR ( n n -- n n ) {: a:n b:n :}
   a 3 * b 5 xor +  dup 7 and  over 11 * 13 xor + ;

\ ---- the six subjects ---------------------------------------------------------

\ The whole body is one call, in tail position. Today the chain reserves a
\ frame, saves the link register, branches with link, loads the link back,
\ releases the frame and returns; the branch is the only one of the six that has
\ to be there.
: TAIL-BIG ( n -- n ) C5-LONG ;

\ Work before the tail call. What the callee reads is computed in the caller, so
\ the shape is a tail call whose argument is not simply the caller's own.
: TAIL-WORK ( n -- n ) 1 + C5-LONG ;

\ THE CONTROL. The same call with work after it, which is not a tail call: this
\ word has something left to do when the callee answers, so it must keep its
\ frame, its saved link register and its own return. Its emitted bytes are what
\ says a change to the tail case left the non-tail case alone.
: NONTAIL ( n -- n ) C5-LONG 1 + ;

\ The second copy of the pure shape, and the callee of TAIL-CHAIN below.
: TAIL-MID ( n -- n ) C5-LONG ;

\ A tail call to a word that is itself nothing but a tail call.
: TAIL-CHAIN ( n -- n ) TAIL-MID ;

\ The pure shape at arity (2 -> 2).
: TAIL-PAIR ( n n -- n n ) C5-PAIR ;

\ A real call and then a tail call. The first one needs the frame and the saved
\ link register; the second one needs them gone.
: TAIL-AFTER ( n -- n ) C5-LONG C5-LONG ;

;package
