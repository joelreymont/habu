\ codegen-compare-migrated5.f - the fifth corpus's bodies, compiled by the new
\ chain and published as ordinary words. One concern: putting the fifth corpus's
\ new-chain words into the dictionary before anything measures them.
\
\ This is tools/codegen-compare-migrated.f's discipline over the fifth corpus,
\ and everything that file says applies here: the source is handed to the engine,
\ the engine compiles it through the path it compiles every definition through,
\ the tape the chain then recompiles is the one the checker's own reader filled
\ while it certified that body, and the publication seam points the `-N` word's
\ dictionary record at the chain's emission. A body that is not well typed Habu
\ does not compile at all, and nothing here catches: a body the chain refuses is
\ a claim this file made and did not keep.
\
\ ALL SIX ROWS ARE HERE, and so are both callees. There is no gap in this
\ corpus: every row the corpus declares is measured.
\
\ THE SUBSTITUTIONS, AND THERE IS ONLY THE ONE KIND. Every body carries `-N` on
\ the name it defines, which is the migration's own convention, and a row that
\ calls writes `C5-LONG-N`, `C5-PAIR-N` or `TAIL-MID-N` where the corpus writes
\ those names without it - the discipline tools/codegen-compare-migrated2.f
\ established when it migrated CELL-FIELD for VEC-COPY-CELLS to call: the new
\ column's word is the new chain's code all the way down, so a row measures a
\ program of the chain's making rather than a chain-compiled shell around the
\ engine's calls. Nothing else changes: no constant is respelled, no operation is
\ changed, no local is renamed and no annotation is added or removed.
\
\ TAIL-MID-N IS BOTH A ROW AND A CALLEE, which is why it is written before
\ TAIL-CHAIN-N registers it. A call site is given the address its callee's
\ dictionary record already carries, and TAIL-MID-N's record carries the chain's
\ emission only after its own migration has run - so the order below publishes it
\ first, and TAIL-CHAIN-N is a chain routine calling a chain routine calling a
\ chain routine.
\
\ NO ROW STATES A REGISTER BUDGET. Every subject of this corpus is straight-line
\ - there is no loop in the corpus at all, by design - and the rows used to state
\ eight apiece; the migration entry derives the pool from NABI:SCRATCH now, so
\ every corpus is measured under the same one.
\
\ AND NO ROW STATES ITS SOURCE EITHER. Every definition below used to be a string
\ this file built and handed over. NMIGRATE:NEXT takes it off the input stream
\ instead, so each one is written at top level - indented, highlighted and diffed
\ like the rest of the tree - and the engine's own parser is what decides where
\ it ends.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus5.f

\ The definitions land in the corpus's own package, reopened: the `-N` words are
\ CODEGEN-CORPUS5 publics, beside the words they are compared against.
package CODEGEN-CORPUS5
public

\ ---- the two callees every row reaches through --------------------------------
\ The corpus's own two, verbatim but for the `-N`. They are not rows and they are
\ not measured: they are what the rows call, migrated so that the new column's
\ rows are the new chain's code all the way down.

NMIGRATE:NEXT
: C5-LONG-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

NMIGRATE:NEXT
: C5-PAIR-N ( n n -- n n )
   {: a:n b:n :}
   a 3 * b 5 xor + dup 7 and over 11 * 13 xor + ;

\ ---- the six rows -------------------------------------------------------------

\ The pure shape: one call, in tail position, and nothing else.
NMIGRATE:NEXT : TAIL-BIG-N ( n -- n ) C5-LONG-N ;

\ Work before the tail call.
NMIGRATE:NEXT : TAIL-WORK-N ( n -- n ) 1 + C5-LONG-N ;

\ The control: work after the call, so the call is not in tail position.
NMIGRATE:NEXT : NONTAIL-N ( n -- n ) C5-LONG-N 1 + ;

\ The second copy of the pure shape, and the callee of TAIL-CHAIN-N.
NMIGRATE:NEXT : TAIL-MID-N ( n -- n ) C5-LONG-N ;

\ A tail call to a word that is itself nothing but a tail call.
NMIGRATE:NEXT : TAIL-CHAIN-N ( n -- n ) TAIL-MID-N ;

\ The pure shape at arity (2 -> 2).
NMIGRATE:NEXT : TAIL-PAIR-N ( n n -- n n ) C5-PAIR-N ;

\ A real call and then a tail call.
NMIGRATE:NEXT : TAIL-AFTER-N ( n -- n ) C5-LONG-N C5-LONG-N ;

;package
