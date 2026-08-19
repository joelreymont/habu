\ codegen-compare-migrated2.f - the second corpus, compiled by the new chain and
\ published as ordinary words. One concern: putting the second corpus's
\ new-chain words into the dictionary before anything measures them.
\
\ This is tools/codegen-compare-migrated.f's discipline over the second corpus,
\ and everything that file says applies here: the source is handed to the engine,
\ the engine compiles it through the path it compiles every definition through,
\ the tape the chain then recompiles is the one the checker's own reader filled
\ while it certified that body, and the publication seam points the `-N` word's
\ dictionary record at the chain's emission. A body that is not well typed Habu
\ does not compile at all, and nothing here catches: a body the chain refuses is
\ a claim this file made and did not keep.
\
\ ALL SEVEN ARE HERE. VEC-COPY-CELLS was the odd one out while a call inside a
\ counted loop miscompiled; dot habu-save-the-loop-5f07e0c3 closed that, so the
\ table has no gap row left and the old comparison declared none.
\
\ TWO BODIES DIFFER FROM THE CORPUS'S IN THE SPELLING OF A CONSTANT, AND IN
\ NOTHING ELSE. This is the one liberty this file takes and it is listed in full
\ so that nobody has to go looking for it:
\
\   WS?         the corpus body names SP, TAB, LF and CR, which is how
\               lib/json-read.f writes it; this one writes 32, 9, 10 and 13.
\               A body the chain compiles may name at most ONE word outside the
\               dialect's vocabulary and this one names four, so there is no
\               spelling of it with the names in that the chain could read at all
\               (dot habu-let-a-migrated-77d34d82).
\   SYM-FOLD-C  the corpus body writes $41, $5A and $20, which is how
\               src/core/checker.f writes it; this one writes 65, 90 and 32. The
\               tape records an integer literal's value by reading the spelling
\               back with the stdlib's decimal reader, and that reader declines a
\               hexadecimal spelling, so the stage refuses the token with
\               E-NFEED-LITERAL rather than record it as zero
\               (src/compiler/native/feed.f:153, dot
\               habu-record-the-engine-79c570ed).
\
\ WHY THAT IS A SPELLING AND NOT A DIFFERENT PROGRAM, MEASURED RATHER THAN
\ ASSERTED. The engine compiles each body and its respelled twin to byte-identical
\ code: WS? is 428 bytes written either way and SYM-FOLD-C is 144 bytes written
\ either way, and both twins answer the same on every pinned input.
\ the old comparison's member held those twins and pinned the equality, so the
\ claim is a test rather than a sentence - if a future engine ever compiled a
\ named constant into something other than the literal behind it, the suite
\ reds and this note is what it points at. Nothing STRUCTURAL is respelled: no
\ operation is changed, no guard is dropped, no branch is moved.
\
\ THE TWO-WORD PROGRAM. T-RES-WALK's callee is migrated FIRST, so it is a
\ published record with an address of its own, and the caller's declaration reads
\ that address off the callee's dictionary record - the same authority the
\ publication seam wrote it from. Both halves are the chain's code, so the row
\ measures a chain-compiled word calling a chain-compiled word, which is the
\ shape the first corpus has only as recursion.
\
\ NOTHING IS STATED AT ALL. The two arities and the register budget used to ride
\ on the same line, and the callee's spelling, entry and effect with them. The
\ entry reads what the definition takes and leaves off the checker's
\ certificate, derives the pool from the machine, and resolves a callee through
\ the same reader. The SOURCE was the last thing left to state, and
\ NMIGRATE:NEXT takes that off the input stream too: every definition below is
\ written at top level, indented, highlighted and diffed like the rest of the
\ tree, and the engine's own parser is what decides where it ends.
\
\ TV-NEXT?-N IS THE ONE ROW STILL WRITTEN AS TEXT, because it is the one that
\ names a data word: NMIGRATE:DEFINE-DATA takes that spelling, and no entry can
\ take it off the stream while the address the body pushes is a fact the caller
\ has to state. Dot habu-let-a-migrated-77d34d82 carries the move.
\
\ A LINT ANNOTATION SITS ABOVE THE ENTRY IT SPEAKS FOR, never inside the
\ definition, so that what the engine reads is the corpus's text and nothing
\ else: the reader starts at the `:` that follows NMIGRATE:NEXT, so a comment
\ written above the entry is outside the source the chain compiles.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus2.f

\ The definitions land in the corpus's own package, reopened: the `-N` words are
\ CODEGEN-CORPUS2 publics, and TV-NEXT?-N names the private table it shares with
\ the word it is compared against.
package CODEGEN-CORPUS2
public

\ A routine with control flow needs more, and says why. A block argument and
\ every value handed to it across an edge are one class holding one register for
\ the whole span between them, so a loop's carried values each hold a register
\ from the pre-header to the latch whether or not they are read in between.

\ A loop that carries TWO values round its back edge needs more again.
\ COUNT-CHAR carries a counter and a cursor and reads a byte between them, and
\ it is refused with E-A64RA-SPILL at nine registers and accepted at ten; twelve
\ is the next even number above that floor. TV-NEXT? reaches a table, compares
\ against two constants and answers a pair, and takes the same budget.

NMIGRATE:NEXT : TAG-N ( n -- n ) 7 and ;

\ lib/json-read.f:252 with its four constants written as their values - the
\ first of the two substitutions the head of this file lists.
NMIGRATE:NEXT
: WS?-N ( n -- bool ) dup 32 = over 9 = or over 10 = or swap 13 = or ;

\ src/core/checker.f:3542 with its three hexadecimal literals written in
\ decimal - the second substitution. Two range tests, each leaving the word from
\ the middle through `exit`, which is the shape.
NMIGRATE:NEXT
: SYM-FOLD-C-N ( n -- n )
   {: c:n :}
   c 65 < if c exit then
   c 90 > if c exit then
   c 32 or ;

NMIGRATE:NEXT : MAX-DIM-N ( n n -- n ) {: a:n b:n :} a b > if a else b then ;

\ typed-local-lint: allow-bare-local - `u` and `c` are the corpus's own
\ spelling, and a migrated body renames no local.
NMIGRATE:NEXT
: COUNT-CHAR-N ( ptr u8 n n -- n )
   {: a:ptr u c :}
   0 0 begin dup u < while dup a + c@ c = if swap 1+ swap then 1+ repeat drop ;

\ The walk's callee. Its one name outside the dialect is the corpus's own
\ binding table, and its spelling is all the chain is told: the engine answers
\ where that table is. Both columns therefore walk the SAME table, which is what
\ makes the head-to-head check a statement about the loads. It is the one row
\ the stream entry cannot take, for the reason the header gives.
s" : TV-NEXT?-N ( n -- n bool ) dup 7 and 1 = 0= if 0 0= 0= exit then dup 3 rshift cells TV-TABLE + @ dup -1 = if drop 0 0= 0= else nip 0 0= then ;"
s" TV-TABLE"
NMIGRATE:DEFINE-DATA

\ The walk itself: a loop whose test is a call. Its routine declares the direct
\ call and the frame its caller's return address goes in, for the reason
\ src/compiler/native/abi.f gives - the first call destroys the return address,
\ so it has to have somewhere to live.
NMIGRATE:NEXT : T-RES-WALK-N ( n -- n ) begin TV-NEXT?-N while repeat ;

\ The copy's callee: a pointer and a cell index in, the address of that cell
\ out. A `ptr n` is one cell of the caller's stack, so its convention is two
\ values in and one out, which is what the declaration below says.
NMIGRATE:NEXT : CELL-FIELD-N ( ptr n n -- ptr n ) cells + ;

\ The copy itself: a counted loop with TWO calls in its body, one working out
\ the address it reads and one the address it writes, and three locals live
\ across both of them. It is the shape that made dot habu-save-the-loop-5f07e0c3
\ - the loop's counters and the caller's locals crossed the call in registers the
\ callee's contract declares destroyed, so the loop miscounted and the store went
\ through a clobbered address.
\
\ WHY ITS BUDGET IS THE WIDEST OF THE SIX. Two of its locals and the loop's index
\ and limit are all live across both calls, so each holds a register from the
\ header to the latch, and the call sites need their own on top of that. It is
\ refused with E-A64RA-SPILL at twelve registers and accepted at fourteen; the
\ budget is a budget, and dot habu-choose-the-register-a95390ac carries taking
\ the number off the routine rather than off a line here.

NMIGRATE:NEXT
: VEC-COPY-CELLS-N ( ptr n ptr n n -- )
   {: src:ptr dst:ptr len:n :}
   len 0 ?do src i CELL-FIELD-N @ dst i CELL-FIELD-N ! loop ;

;package
