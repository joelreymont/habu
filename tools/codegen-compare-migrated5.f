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
\ corpus: tools/codegen-compare-new5.f measures every row the corpus declares.
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
\ TAIL-MID-N IS BOTH A ROW AND A CALLEE, which is why it is defined before
\ TAIL-CHAIN-N registers it. A call site is given the address its callee's
\ dictionary record already carries, and TAIL-MID-N's record carries the chain's
\ emission only after its own migration has run - so RUN below publishes it
\ first, and TAIL-CHAIN-N is a chain routine calling a chain routine calling a
\ chain routine.
\
\ THE REGISTER BUDGET IS EIGHT, ONCE, FOR EVERY ROW. Every subject of this
\ corpus is straight-line - there is no loop in the corpus at all, by design -
\ and eight is what the other four corpora's straight-line rows state. Dot
\ habu-choose-the-register-a95390ac carries taking the number off the routine
\ instead of the caller.

require lib/errors.f
require lib/prelude.f
require src/compiler/native/migrate.f
require tools/codegen-compare-core.f
require tools/codegen-compare-corpus5.f

package CODEGEN-MIGRATED5

private

8 constant REGS                   \ general registers a straight-line routine may use

\ ---- the two callees every row reaches through --------------------------------
\ The corpus's own two, verbatim but for the `-N`. They are not rows and they are
\ not measured: they are what the rows call, migrated so that the new column's
\ rows are the new chain's code all the way down.

: C5-LONG ( -- )
   s" : C5-LONG-N ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   1 1 REGS NMIGRATE:DEFINE ;

: C5-PAIR ( -- )
   s" : C5-PAIR-N ( n n -- n n ) {: a:n b:n :} a 3 * b 5 xor + dup 7 and over 11 * 13 xor + ;"
   2 2 REGS NMIGRATE:DEFINE ;

\ ---- the six rows -------------------------------------------------------------

\ The pure shape: one call, in tail position, and nothing else.
: TAIL-BIG ( -- )
   s" : TAIL-BIG-N ( n -- n ) C5-LONG-N ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ Work before the tail call.
: TAIL-WORK ( -- )
   s" : TAIL-WORK-N ( n -- n ) 1 + C5-LONG-N ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ The control: work after the call, so the call is not in tail position.
: NONTAIL ( -- )
   s" : NONTAIL-N ( n -- n ) C5-LONG-N 1 + ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ The second copy of the pure shape, and the callee of TAIL-CHAIN-N.
: TAIL-MID ( -- )
   s" : TAIL-MID-N ( n -- n ) C5-LONG-N ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ A tail call to a word that is itself nothing but a tail call.
: TAIL-CHAIN ( -- )
   s" : TAIL-CHAIN-N ( n -- n ) TAIL-MID-N ;"
   1 1 REGS NMIGRATE:DEFINE ;

\ The pure shape at arity (2 -> 2).
: TAIL-PAIR ( -- )
   s" : TAIL-PAIR-N ( n n -- n n ) C5-PAIR-N ;"
   2 2 REGS NMIGRATE:DEFINE ;

\ A real call and then a tail call.
: TAIL-AFTER ( -- )
   s" : TAIL-AFTER-N ( n -- n ) C5-LONG-N C5-LONG-N ;"
   1 1 REGS NMIGRATE:DEFINE ;

public

\ Publish the six, and the two callees they reach through first, because a call
\ site is given the address its callee's record already carries - and TAIL-MID-N
\ before TAIL-CHAIN-N for the same reason. It is one word rather than nine
\ top-level lines because a migration claims code space at the engine's free
\ slot, and the interpreter uses that slot for the line it is running.
: RUN ( -- )
   C5-LONG
   C5-PAIR
   TAIL-BIG
   TAIL-WORK
   NONTAIL
   TAIL-MID
   TAIL-CHAIN
   TAIL-PAIR
   TAIL-AFTER ;

;package

\ The definitions land where the current wordlist points when RUN executes, so
\ the corpus's package is reopened around the call: the `-N` words become
\ CODEGEN-CORPUS5 publics, beside the words they are compared against.
package CODEGEN-CORPUS5
public

CODEGEN-MIGRATED5:RUN

;package
