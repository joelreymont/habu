\ codegen-compare-migrated.f - the corpus, compiled by the new chain and
\ published as ordinary words. One concern: putting eleven new-chain words into
\ the dictionary before anything measures them.
\
\ WHY THE NEW COLUMN NEEDS THIS FILE AT ALL. The comparison used to enter the
\ new chain's routines through an address on the data stack, because the chain
\ had nowhere to publish them: `execute` on a number, which is one indirect
\ branch the old column never paid, and the whole nanosecond half of the table
\ carried a paragraph telling the reader to subtract it. There is a publication
\ seam now, so a new-chain routine can be a dictionary record like any other -
\ and then both columns are entered by whatever the engine does for an ordinary
\ word, which is the only way the two costs are about the emitted code.
\
\ WHY IT IS LOADED HERE AND NOT WHERE THE ROWS ARE MEASURED. A call site is
\ resolved when the CALLER is compiled: the engine writes the callee's address
\ into a direct branch, or copies the callee's body into the caller outright.
\ tools/codegen-compare-cases.f is compiled first and its bodies therefore call
\ the corpus words the engine's own emitter compiled; this file publishes the
\ new-chain words next; tools/codegen-compare-new.f is compiled last and its
\ bodies call these. Both files' bodies are ordinary checked Habu naming
\ ordinary words, so neither knows anything about how the other's code was made.
\
\ THE `-N` TAIL IS THE SAME PROGRAM, COMPILED THE OTHER WAY. Each definition
\ below is its corpus word's body, word for word, under a name of its own so
\ both are alive at once - the old column has to keep something to compare
\ against. They are declared in the corpus's own package, reopened, because
\ CELL-BUMP's cell is private to it and the point of that word is that BOTH
\ columns bump the SAME piece of memory.
\
\ AND THEY ARE REAL CHECKED DEFINITIONS. The engine compiles each one through
\ the path it compiles every definition through - the interpreter, the checker,
\ the old emitter - and the tape the chain then recompiles is the one the
\ checker's own reader filled while it certified that body. The previous shape of
\ this measurement retyped each body as a bare line for a fixture lexer to
\ re-read; nothing checked those lines, and a divergence from the corpus body
\ was a reader's job to catch. Here a body that is not a well typed Habu
\ definition does not compile at all.
\
\ WHAT IS STILL STATED. The two arities and the register budget, on the same
\ line as the source. The checker knows the arities and cannot yet hand them to a
\ recorded unit (dot habu-bind-checker-env-ed4f9f87); the budget is a budget (dot
\ habu-choose-the-register-a95390ac).

require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-corpus.f

package CODEGEN-MIGRATED

private

4 constant REGS                   \ registers a straight-line corpus routine may use

\ A routine with control flow needs more, and says why. A block argument and
\ every value handed to it across an edge are one class holding one register for
\ the whole span between them, so a loop's carried values each hold a register
\ from the pre-header to the latch whether or not they are read in between.
8 constant LOOP-REGS

: NOOP ( -- )
   s" : NOOP-N ( -- ) ;" 0 0 REGS NMIGRATE:DEFINE ;

: ADD3 ( -- )
   s" : ADD3-N ( n n n -- n ) + + ;" 3 1 REGS NMIGRATE:DEFINE ;

: SQUARE-SUM ( -- )
   s" : SQUARE-SUM-N ( n n -- n ) dup * swap dup * + ;" 2 1 REGS NMIGRATE:DEFINE ;

: MAX2 ( -- )
   s" : MAX2-N ( n n -- n ) 2dup < if swap then drop ;" 2 1 REGS NMIGRATE:DEFINE ;

: LERP ( -- )
   s" : LERP-N ( n n n -- n ) {: a:n b:n t:n :} b a - t * 100 / a + ;"
   3 1 REGS NMIGRATE:DEFINE ;

: SUM-TO ( -- )
   s" : SUM-TO-N ( n -- n ) 0 swap 0 ?do i + loop ;" 1 1 LOOP-REGS NMIGRATE:DEFINE ;

: COUNT-DOWN ( -- )
   s" : COUNT-DOWN-N ( n -- n ) begin 1- dup 0 <= until ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE ;

\ The one word whose point is a side effect. The cell it bumps is the corpus's
\ own, so this routine and the interpreted word write the same memory. Its
\ spelling is all the chain is told: the address is the engine's answer, asked
\ for in the scope this line runs in - which is the corpus's own package, where
\ that spelling denotes the corpus's private cell and nothing else.
: CELL-BUMP ( -- )
   s" : CELL-BUMP-N ( n -- n ) BUMP-CELL ! BUMP-CELL @ 1+ dup BUMP-CELL ! ;"
   s" BUMP-CELL"
   1 1 REGS NMIGRATE:DEFINE-DATA ;

: BYTE-SUM ( -- )
   s" : BYTE-SUM-N ( ptr u8 n -- n ) {: a:ptr u:n :} 0 u 0 ?do i a + c@ + loop ;"
   2 1 LOOP-REGS NMIGRATE:DEFINE ;

\ The recursion, which is also the plain word-call-and-return shape: the only
\ corpus routine that is not a leaf, so its contract declares the call and the
\ frame its caller's return address goes in.
: FACT ( -- )
   s" : FACT-N ( n -- n ) dup 1 <= if drop 1 exit then dup 1- RECURSE * ;"
   1 1 LOOP-REGS NMIGRATE:DEFINE ;

: BYTE-FIND ( -- )
   s" : BYTE-FIND-N ( ptr u8 n n -- n ) {: a:ptr u:n c:n :} u 0 ?do i a + c@ c = if i unloop exit then loop -1 ;"
   3 1 LOOP-REGS NMIGRATE:DEFINE ;

public

\ Publish all eleven. It is one word rather than eleven top-level lines because
\ a migration claims code space at the engine's free slot, and the interpreter
\ uses that slot for the line it is running.
\
\ Nothing here catches. A body the chain cannot compile is a claim this file
\ makes and does not keep, and it must surface as the refusing stage's own error
\ rather than as a column that is quietly one row shorter.
: RUN ( -- )
   NOOP
   ADD3
   SQUARE-SUM
   MAX2
   LERP
   SUM-TO
   COUNT-DOWN
   CELL-BUMP
   BYTE-SUM
   FACT
   BYTE-FIND ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS publics, and CELL-BUMP-N can name the private cell it shares
\ with the word it is compared against.
package CODEGEN-CORPUS
public

CODEGEN-MIGRATED:RUN

;package
