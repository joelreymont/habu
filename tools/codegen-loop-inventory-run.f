\ codegen-loop-inventory-run.f - the hoisting lane's measurement, run over every
\ corpus row the chain compiles. One concern: naming those rows and adding their
\ loop counts up.
\
\ It is a separate file from tools/codegen-loop-inventory.f for the reason the
\ combining lane's runner gives: the counting is about instructions and belongs
\ with the decoder, while WHICH routines are worth counting is a fact about this
\ corpus and changes when the corpus does. The row list is the same one
\ tools/codegen-combine-inventory-run.f names, so the two inventories describe
\ one set of routines and a row added to the corpus arrives in both.
\
\ WHAT TO READ OFF IT. `loops` is how many back edges the chain's compilation of
\ the row holds, so a row with none is one neither transform can reach at all.
\ `body` is how many instructions stand inside those loops, which is what an
\ unroll would duplicate and the denominator every unrolling decision needs.
\ `const` and `invld` are the two hoistable shapes: literal halves rebuilt every
\ turn, and loads whose address the body never changes in a body that writes no
\ memory. A column of zeroes across every row is a measured zero and is the
\ deliverable - not a reason to go looking for a friendlier corpus.
\
\ THE ROWS ARE THE MIGRATED ONES. Each corpus row has two compilations, the
\ engine's emitter and the native chain's, and both transforms would belong to
\ the chain, so every name carries the `-N` the migration publishes under.
\ Loading the five migration files is what compiles them; there is nothing to
\ call.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-loop-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NLOOPINV-RUN

private

variable T-INSNS
variable T-LOOPS
variable T-BODY
variable T-CONSTS
variable T-FOLDABLE
variable T-INV-LOADS
variable T-ROWS
variable T-LOOPY

\ One labelled number, on the row's own line: the report is one line per routine
\ so that a reader can sort it.
: FIELD ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v FMT:.INT ;

\ One row's counts, and the same numbers added into the totals. They are read
\ once each and used twice so that a report line and the total below it can never
\ disagree about what was measured.
: TALLY ( -- )
   NCOMBINV:INSNS T-INSNS +!
   NLOOPINV:LOOPS T-LOOPS +!
   NLOOPINV:BODY-TOTAL T-BODY +!
   NLOOPINV:CONSTS-TOTAL T-CONSTS +!
   NLOOPINV:FOLDABLE-TOTAL T-FOLDABLE +!
   NLOOPINV:INV-LOADS-TOTAL T-INV-LOADS +!
   NLOOPINV:LOOPS 0 > if 1 T-LOOPY +! then
   1 T-ROWS +! ;

: COUNTS. ( -- )
   s"  insns " NCOMBINV:INSNS FIELD
   s"  loops " NLOOPINV:LOOPS FIELD
   s"  body " NLOOPINV:BODY-TOTAL FIELD
   s"  const " NLOOPINV:CONSTS-TOTAL FIELD
   s"  fold " NLOOPINV:FOLDABLE-TOTAL FIELD
   s"  invld " NLOOPINV:INV-LOADS-TOTAL FIELD ;

public

: ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NLOOPINV:ROW!
   a u type
   COUNTS.
   cr
   TALLY ;

private

: CORPUS1 ( -- )
   s" CODEGEN-CORPUS:NOOP-N" ROW
   s" CODEGEN-CORPUS:ADD3-N" ROW
   s" CODEGEN-CORPUS:SQUARE-SUM-N" ROW
   s" CODEGEN-CORPUS:MAX2-N" ROW
   s" CODEGEN-CORPUS:LERP-N" ROW
   s" CODEGEN-CORPUS:SUM-TO-N" ROW
   s" CODEGEN-CORPUS:COUNT-DOWN-N" ROW
   s" CODEGEN-CORPUS:CELL-BUMP-N" ROW
   s" CODEGEN-CORPUS:BYTE-SUM-N" ROW
   s" CODEGEN-CORPUS:FACT-N" ROW
   s" CODEGEN-CORPUS:BYTE-FIND-N" ROW ;

: CORPUS2 ( -- )
   s" CODEGEN-CORPUS2:TAG-N" ROW
   s" CODEGEN-CORPUS2:SYM-FOLD-C-N" ROW
   s" CODEGEN-CORPUS2:MAX-DIM-N" ROW
   s" CODEGEN-CORPUS2:COUNT-CHAR-N" ROW
   s" CODEGEN-CORPUS2:T-RES-WALK-N" ROW
   s" CODEGEN-CORPUS2:CELL-FIELD-N" ROW
   s" CODEGEN-CORPUS2:VEC-COPY-CELLS-N" ROW ;

: CORPUS3 ( -- )
   s" CODEGEN-CORPUS3:SGD-N" ROW
   s" CODEGEN-CORPUS3:MAX-F-N" ROW
   s" CODEGEN-CORPUS3:T-AT-N" ROW
   s" CODEGEN-CORPUS3:T-GET-N" ROW
   s" CODEGEN-CORPUS3:T-SET-N" ROW
   s" CODEGEN-CORPUS3:T-SUM-N" ROW
   s" CODEGEN-CORPUS3:T-DIST2-N" ROW
   s" CODEGEN-CORPUS3:T-NORM2-N" ROW
   s" CODEGEN-CORPUS3:T-REL-L2-N" ROW
   s" CODEGEN-CORPUS3:RELU-F-N" ROW
   s" CODEGEN-CORPUS3:FROUND-N" ROW ;

: CORPUS4 ( -- )
   s" CODEGEN-CORPUS4:C-ADD1-N" ROW
   s" CODEGEN-CORPUS4:C-MUL2-N" ROW
   s" CODEGEN-CORPUS4:C-AND7-N" ROW
   s" CODEGEN-CORPUS4:C-XOR5-N" ROW
   s" CODEGEN-CORPUS4:C-MAD-N" ROW
   s" CODEGEN-CORPUS4:C-LONG-N" ROW
   s" CODEGEN-CORPUS4:CALL-FAN-N" ROW
   s" CODEGEN-CORPUS4:CALL-FAN-BIG-N" ROW
   s" CODEGEN-CORPUS4:CALL-LOOP-3-N" ROW
   s" CODEGEN-CORPUS4:TINY-CALLEE-N" ROW
   s" CODEGEN-CORPUS4:WIDE-ARITY-N" ROW
   s" CODEGEN-CORPUS4:LADDER-N" ROW
   s" CODEGEN-CORPUS4:BIG-CONSTS-N" ROW
   s" CODEGEN-CORPUS4:MANY-LOCALS-N" ROW
   s" CODEGEN-CORPUS4:FLOAT-MIX-N" ROW
   s" CODEGEN-CORPUS4:STORE-LOAD-N" ROW ;

: CORPUS5 ( -- )
   s" CODEGEN-CORPUS5:C5-LONG-N" ROW
   s" CODEGEN-CORPUS5:C5-PAIR-N" ROW
   s" CODEGEN-CORPUS5:TAIL-BIG-N" ROW
   s" CODEGEN-CORPUS5:TAIL-WORK-N" ROW
   s" CODEGEN-CORPUS5:NONTAIL-N" ROW
   s" CODEGEN-CORPUS5:TAIL-MID-N" ROW
   s" CODEGEN-CORPUS5:TAIL-CHAIN-N" ROW
   s" CODEGEN-CORPUS5:TAIL-PAIR-N" ROW
   s" CODEGEN-CORPUS5:TAIL-AFTER-N" ROW ;

: TOTAL. ( -- )
   s" TOTAL rows " type T-ROWS @ .
   s"  with-loops " T-LOOPY @ FIELD
   s"  insns " T-INSNS @ FIELD
   s"  loops " T-LOOPS @ FIELD
   s"  body " T-BODY @ FIELD
   s"  const " T-CONSTS @ FIELD
   s"  fold " T-FOLDABLE @ FIELD
   s"  invld " T-INV-LOADS @ FIELD
   cr ;

public

: MAIN ( -- )
   CORPUS1
   CORPUS2
   CORPUS3
   CORPUS4
   CORPUS5
   TOTAL. ;

;package

NLOOPINV-RUN:MAIN
