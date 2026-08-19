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
\ the old comparison's case list was compiled first and its bodies therefore called
\ the corpus words the engine's own emitter compiled; this file publishes the
\ new-chain words next; its chain column was compiled last and its
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
\ NOTHING IS STATED AT ALL. The two arities used to ride on the same line and the
\ register budget beside them; the entry reads what the definition takes and
\ leaves off the checker's certificate now, and derives the pool from the
\ machine, so neither is a number a row here could get wrong. The SOURCE was the
\ last thing left to state, and NMIGRATE:NEXT takes that off the input stream
\ too: every definition below is written at top level, indented, highlighted and
\ diffed like the rest of the tree, and the engine's own parser is what decides
\ where it ends.
\
\ CELL-BUMP-N IS THE ONE ROW STILL WRITTEN AS TEXT, because it is the one that
\ names a data word: NMIGRATE:DEFINE-DATA takes that spelling, and no entry can
\ take it off the stream while the address the body pushes is a fact the caller
\ has to state. Dot habu-parse-a-migrated-b38a83d9 carries the move.
\
\ NOTHING HERE CATCHES. A body the chain cannot compile is a claim this file
\ makes and does not keep, and it must surface as the refusing stage's own error
\ rather than as a column that is quietly one row shorter.

require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-corpus.f

\ The definitions land in the corpus's own package, reopened: the `-N` words are
\ CODEGEN-CORPUS publics, and CELL-BUMP-N names the private cell it shares with
\ the word it is compared against.
package CODEGEN-CORPUS
public

\ A routine with control flow needs more, and says why. A block argument and
\ every value handed to it across an edge are one class holding one register for
\ the whole span between them, so a loop's carried values each hold a register
\ from the pre-header to the latch whether or not they are read in between.

NMIGRATE:NEXT : NOOP-N ( -- ) ;

NMIGRATE:NEXT : ADD3-N ( n n n -- n ) + + ;

NMIGRATE:NEXT : SQUARE-SUM-N ( n n -- n ) dup * swap dup * + ;

NMIGRATE:NEXT : MAX2-N ( n n -- n ) 2dup < if swap then drop ;

NMIGRATE:NEXT
: LERP-N ( n n n -- n )
   {: a:n b:n t:n :}
   b a - t * 100 / a + ;

NMIGRATE:NEXT : SUM-TO-N ( n -- n ) 0 swap 0 ?do i + loop ;

NMIGRATE:NEXT : COUNT-DOWN-N ( n -- n ) begin 1- dup 0 <= until ;

\ The one word whose point is a side effect. The cell it bumps is the corpus's
\ own, so this routine and the interpreted word write the same memory. Its
\ spelling is all the chain is told: the address is the engine's answer, asked
\ for in the scope this line runs in - which is the corpus's own package, where
\ that spelling denotes the corpus's private cell and nothing else. It is the
\ one row the stream entry cannot take, for the reason the header gives.
s" : CELL-BUMP-N ( n -- n ) BUMP-CELL ! BUMP-CELL @ 1+ dup BUMP-CELL ! ;"
s" BUMP-CELL"
NMIGRATE:DEFINE-DATA

NMIGRATE:NEXT
: BYTE-SUM-N ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 u 0 ?do i a + c@ + loop ;

\ The recursion, which is also the plain word-call-and-return shape: the only
\ corpus routine that is not a leaf, so its contract declares the call and the
\ frame its caller's return address goes in.
NMIGRATE:NEXT
: FACT-N ( n -- n )
   dup 1 <= if drop 1 exit then
   dup 1- RECURSE * ;

NMIGRATE:NEXT
: BYTE-FIND-N ( ptr u8 n n -- n )
   {: a:ptr u:n c:n :}
   u 0 ?do i a + c@ c = if i unloop exit then loop
   -1 ;

;package
