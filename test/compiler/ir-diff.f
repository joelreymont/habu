\ ir-diff.f - checked structural-diff tests.
\
\ Proves the sections 5.6 and 6.6 contract of src/compiler/ir/diff.f: two frozen
\ modules are compared by what they MEAN, not by the text they render to, and the
\ report names the rows that differ.
\
\ THE FIXTURE THAT DECIDES SEMANTIC FROM TEXTUAL. ADDED-CASE compares the shared
\ fixture module with the same module plus one more interned symbol whose bytes
\ sort second. Every canonical ordinal after it shifts by one, so a diff built by
\ rendering both modules and comparing the text would report a changed line for
\ every symbol, type, attribute and operation whose number moved. The truth is
\ that one symbol was added, so this test requires EXACTLY ONE difference and
\ pins the line. It is the fixture that reds if the comparison is ever made
\ textual.
\
\ THE FIXTURE THAT DECIDES CANONICAL FROM LITERAL. EQUIV-CASE compares the same
\ module built along two topological insertion orders. Every stored ordinal
\ differs and the two modules mean the same thing, so a comparison that read
\ stored ordinals instead of the content behind them would report differences
\ where there are none. It requires zero.
\
\ THE REST. SELF-CASE requires that a module against itself is an empty report
\ and zero differences, which is what makes the count the equality predicate.
\ OP-CASE changes one operation - the tagged constant carries the other integer
\ under its z-tag key - and requires exactly that one difference, so a real
\ change is not swallowed. SWAP-CASE swaps two operations of the entry block and
\ pins the whole report, because program order is the program and must stay
\ visible. Then the refusals a checked caller can reach: a span too short, a
\ module whose context has torn down, and a canonical table that numbers the
\ other module.

require lib/test.f
require src/compiler/ir/diff.f
require test/compiler/ir-module-fixture.f

package IR-DIFF-TEST
private

$4000 constant REPORT-CAP

create RB REPORT-CAP allot            \ the report under test
create EX REPORT-CAP allot            \ the expected report, built line by line

variable EXU

\ ---- the expected report -----------------------------------------------------
: EX-RESET ( -- )
   0 EXU ! ;

: EX-BYTE ( n -- )
   {: b:n :}
   EXU @ REPORT-CAP >= if E-IR-DIFF-ROOM throw then
   b EX EXU @ + c!
   EXU @ 1+ EXU ! ;

: LINE ( ptr u8 n -- )
   {: p:ptr u:n :}
   u 0 ?do
      p i + c@ EX-BYTE
   loop
   $0A EX-BYTE ;

: EX$ ( -- ptr u8 n )
   EX EXU @ ;

\ ---- comparing one pair ------------------------------------------------------
: TABLE-OF ( IR-CTX:ctx IR-BUILD:module -- IR-CANON:table )
   IR-CANON:CANON ;

: COMPARE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:module n -- n n )
   {: c:IR-CTX:ctx ma:IR-BUILD:module mb:IR-BUILD:module room:n :}
   c ma TABLE-OF {: xa:IR-CANON:table :}
   c mb TABLE-OF {: xb:IR-CANON:table :}
   ma xa mb xb RB room IR-DIFF:DIFF ;

\ The same module built twice under whatever knob the caller has set, so a case
\ states only what it changed.
: PAIR-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma mb REPORT-CAP COMPARE ;

: RB$ ( n -- ptr u8 n )
   {: u:n :}
   RB u ;

\ ---- a module against itself -------------------------------------------------
: SELF-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m m REPORT-CAP COMPARE ;

: SELF-CASE ( -- )
   s" a module against itself is no differences and no report" T-LABEL
   IR-FIXTURE:BND [: SELF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= ;

\ ---- two insertion orders of one module --------------------------------------
: EQUIV-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 1 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma mb REPORT-CAP COMPARE ;

: EQUIV-CASE ( -- )
   s" two topological build orders are no differences" T-LABEL
   IR-FIXTURE:BND [: EQUIV-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= ;

\ ---- one added symbol --------------------------------------------------------
: ADDED-BODY ( IR-CTX:ctx -- n ptr u8 n ptr u8 n )
   {: c:IR-CTX:ctx :}
   EX-RESET
   s\" symbol + s1 \"b-tag\"" LINE
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   IR-FIXTURE:EXTRA-SYMBOL!
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   IR-FIXTURE:RESET
   c ma mb REPORT-CAP COMPARE {: u:n hits:n :}
   hits
   u RB$
   EX$ ;

: ADDED-CASE ( -- )
   s" one added symbol is one difference, not a renumbered table" T-LABEL
   IR-FIXTURE:BND [: ADDED-BODY ;] IR-CTX:WITH-CONTEXT
   T$= 1 T= ;

\ ---- one changed operation ---------------------------------------------------
: OP-BODY ( IR-CTX:ctx -- n ptr u8 n ptr u8 n )
   {: c:IR-CTX:ctx :}
   EX-RESET
   s" op o1 attr 1 value - int(7) + int(-3)" LINE
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   IR-FIXTURE:CHANGED-ATTR!
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   IR-FIXTURE:RESET
   c ma mb REPORT-CAP COMPARE {: u:n hits:n :}
   hits
   u RB$
   EX$ ;

: OP-CASE ( -- )
   s" one changed operation attribute is one named difference" T-LABEL
   IR-FIXTURE:BND [: OP-BODY ;] IR-CTX:WITH-CONTEXT
   T$= 1 T= ;

\ ---- two swapped operations --------------------------------------------------
\ Program order is the program, so swapping the entry block's first two
\ operations moves the opcode and the attribute set of both, and the branch that
\ names the constant it hands on now names a different value.
: SWAP-EXPECT ( -- )
   EX-RESET
   s\" op o0 opcode - \"hir.const\" + \"hir.tagged\"" LINE
   s" op o0 attrs - 0 + 2" LINE
   s\" op o1 opcode - \"hir.tagged\" + \"hir.const\"" LINE
   s" op o1 attrs - 2 + 0" LINE
   s" op o2 operand 0 - 0 + 1" LINE ;

: SWAP-BODY ( IR-CTX:ctx -- n ptr u8 n ptr u8 n )
   {: c:IR-CTX:ctx :}
   SWAP-EXPECT
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 1 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma mb REPORT-CAP COMPARE {: u:n hits:n :}
   hits
   u RB$
   EX$ ;

: SWAP-CASE ( -- )
   s" swapping two operations is reported operation by operation" T-LABEL
   IR-FIXTURE:BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT
   T$= 5 T= ;

\ ---- refusals ----------------------------------------------------------------
\ A report span one byte shorter than the report needs.
: ROOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 1 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma mb REPORT-CAP COMPARE drop {: u:n :}
   c ma mb u 1- COMPARE drop drop ;

: ROOM-RUN ( -- )
   IR-FIXTURE:BND [: ROOM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A module whose own context has torn down. It is presented with the live
\ module's canonical table, because canonicalizing a dead module is IR-CANON's
\ refusal and would never reach this one.
: INNER-MODULE ( IR-CTX:ctx -- IR-BUILD:module )
   0 0 IR-FIXTURE:MODULE-OF ;

: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: live:IR-BUILD:module :}
   c live TABLE-OF {: x:IR-CANON:table :}
   IR-FIXTURE:BND [: INNER-MODULE ;] IR-CTX:WITH-CONTEXT {: dead:IR-BUILD:module :}
   live x dead x RB REPORT-CAP IR-DIFF:DIFF drop drop ;

: STALE-RUN ( -- )
   IR-FIXTURE:BND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A canonical table handed in with the other module.
: MISPAIR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma TABLE-OF {: xa:IR-CANON:table :}
   c mb TABLE-OF {: xb:IR-CANON:table :}
   ma xb mb xa RB REPORT-CAP IR-DIFF:DIFF drop drop ;

: MISPAIR-RUN ( -- )
   IR-FIXTURE:BND [: MISPAIR-BODY ;] IR-CTX:WITH-CONTEXT ;

: ROOM-CASE ( -- )
   s" a span one byte short of the report rejects" T-LABEL
   [: ROOM-RUN ;] E-IR-DIFF-ROOM TTHROWSQ ;

: STALE-CASE ( -- )
   s" a module whose context has torn down rejects" T-LABEL
   [: STALE-RUN ;] E-IR-DIFF-STALE TTHROWSQ ;

: MISPAIR-CASE ( -- )
   s" a canonical table that numbers the other module rejects" T-LABEL
   [: MISPAIR-RUN ;] E-IR-CANON-OWNER TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   IR-FIXTURE:RESET
   IR-FIXTURE:BND [: drop SELF-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop EQUIV-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop ADDED-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop OP-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop SWAP-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop ROOM-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop STALE-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop MISPAIR-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-DIFF-TEST:RUN
