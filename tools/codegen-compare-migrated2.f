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
\ table has no gap row left and tools/codegen-compare-new2.f declares none.
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
\ tools/codegen-compare-test.f holds those twins and pins the equality, so the
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
\ WHAT IS STILL STATED. The two arities and the register budget, on the same line
\ as the source, and for the caller the callee's spelling, entry and effect. The
\ checker knows the arities and cannot yet hand them to a recorded unit (dot
\ habu-bind-checker-env-ed4f9f87); the budget is a budget (dot
\ habu-choose-the-register-a95390ac); the callee's three facts are the caller's
\ statement until dot habu-resolve-a-callee-0340dfde lands, and the entry is read
\ off the record here so that at least that one is not typed in twice.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus2.f

package CODEGEN-MIGRATED2

private

4 constant REGS                   \ registers a straight-line corpus routine may use

\ A routine with control flow needs more, and says why. A block argument and
\ every value handed to it across an edge are one class holding one register for
\ the whole span between them, so a loop's carried values each hold a register
\ from the pre-header to the latch whether or not they are read in between.
8 constant LOOP-REGS

\ A loop that carries TWO values round its back edge needs more again.
\ COUNT-CHAR carries a counter and a cursor and reads a byte between them, and
\ it is refused with E-A64RA-SPILL at nine registers and accepted at ten; twelve
\ is the next even number above that floor. TV-NEXT? reaches a table, compares
\ against two constants and answers a pair, and takes the same budget.
12 constant WIDE-REGS

: TAG ( -- )
   s" : TAG-N ( n -- n ) 7 and ;" 1 1 REGS NMIGRATE:DEFINE ;

\ lib/json-read.f:252 with its four constants written as their values - the
\ first of the two substitutions the head of this file lists.
: WS? ( -- )
   s" : WS?-N ( n -- bool ) dup 32 = over 9 = or over 10 = or swap 13 = or ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ src/core/checker.f:3542 with its three hexadecimal literals written in
\ decimal - the second substitution. Two range tests, each leaving the word from
\ the middle through `exit`, which is the shape.
: SYM-FOLD-C ( -- )
   s" : SYM-FOLD-C-N ( n -- n ) {: c:n :} c 65 < if c exit then c 90 > if c exit then c 32 or ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE ;

: MAX-DIM ( -- )
   s" : MAX-DIM-N ( n n -- n ) {: a:n b:n :} a b > if a else b then ;"
   2 1 REGS NMIGRATE:DEFINE ;

: COUNT-CHAR ( -- )
   s" : COUNT-CHAR-N ( ptr u8 n n -- n ) {: a:ptr u c :} 0 0 begin dup u < while dup a + c@ c = if swap 1+ swap then 1+ repeat drop ;"
   3 1 WIDE-REGS NMIGRATE:DEFINE ;

\ The walk's callee. Its one name outside the dialect is the corpus's own
\ binding table, and its spelling is all the chain is told: the engine answers
\ where that table is. Both columns therefore walk the SAME table, which is what
\ makes the head-to-head check a statement about the loads.
: TV-NEXT? ( -- )
   s" : TV-NEXT?-N ( n -- n bool ) dup 7 and 1 = 0= if 0 0= 0= exit then dup 3 rshift cells TV-TABLE + @ dup -1 = if drop 0 0= 0= else nip 0 0= then ;"
   s" TV-TABLE"
   1 2 WIDE-REGS NMIGRATE:DEFINE-DATA ;

\ The walk itself: a loop whose test is a call. Its routine declares the direct
\ call and the frame its caller's return address goes in, for the reason
\ src/compiler/native/abi.f gives - the first call destroys the return address,
\ so it has to have somewhere to live.
: T-RES-WALK ( -- )
   s" TV-NEXT?-N" s" CODEGEN-CORPUS2:TV-NEXT?-N" CODEGEN-COMPARE:CODE-ENTRY 1 2 NMIGRATE:CALLEE
   s" : T-RES-WALK-N ( n -- n ) begin TV-NEXT?-N while repeat ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE-CALLING ;

\ The copy's callee: a pointer and a cell index in, the address of that cell
\ out. A `ptr n` is one cell of the caller's stack, so its convention is two
\ values in and one out, which is what the declaration below says.
: CELL-FIELD ( -- )
   s" : CELL-FIELD-N ( ptr n n -- ptr n ) cells + ;"
   2 1 REGS NMIGRATE:DEFINE ;

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
14 constant COPY-REGS

: VEC-COPY-CELLS ( -- )
   s" CELL-FIELD-N" s" CODEGEN-CORPUS2:CELL-FIELD-N" CODEGEN-COMPARE:CODE-ENTRY 2 1 NMIGRATE:CALLEE
   s" : VEC-COPY-CELLS-N ( ptr n ptr n n -- ) {: src:ptr dst:ptr len:n :} len 0 ?do src i CELL-FIELD-N @ dst i CELL-FIELD-N ! loop ;"
   3 0 COPY-REGS NMIGRATE:DEFINE-CALLING ;

public

\ Publish all seven and the two callees. It is one word rather than nine
\ top-level lines because a migration claims code space at the engine's free
\ slot, and the interpreter uses that slot for the line it is running. Each
\ callee is published before the caller that names it.
: RUN ( -- )
   TAG
   WS?
   SYM-FOLD-C
   MAX-DIM
   COUNT-CHAR
   TV-NEXT?
   T-RES-WALK
   CELL-FIELD
   VEC-COPY-CELLS ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS2 publics, and TV-NEXT?-N can name the private table it shares
\ with the word it is compared against.
package CODEGEN-CORPUS2
public

CODEGEN-MIGRATED2:RUN

;package
