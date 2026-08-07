\ codegen-branch-inventory-run.f - the branch-collapse lane's measurement, run
\ over every corpus row the chain compiles. One concern: naming those rows and
\ adding their chain counts up.
\
\ It is a separate file from tools/codegen-branch-inventory.f for the reason the
\ loop and combining lanes' runners give: the counting is about instructions and
\ belongs with the decoder, while WHICH routines are worth counting is a fact
\ about this corpus and changes when the corpus does. The row list is the one
\ tools/codegen-loop-inventory-run.f and tools/codegen-combine-inventory-run.f
\ name, so the three inventories describe one set of routines and a row added to
\ the corpus arrives in all of them.
\
\ WHAT TO READ OFF IT. `b` is how many unconditional branches the row's
\ compilation holds that stay inside it - the population a collapse works on.
\ `chains` is how many branches of it go to another branch, which is what a
\ retarget removes and therefore the deliverable this lane is measured by: it is
\ expected to fall to zero and the byte columns are expected to follow. `idle` is
\ an unconditional branch to the very next instruction, which
\ src/compiler/native/emit.f deletes as it emits; a non-zero reading there is not
\ an opportunity but a bug in that pass, and it is printed so that it cannot go
\ unnoticed while the other two columns are being read.
\
\ THE ROWS ARE THE MIGRATED ONES. Each corpus row has two compilations, the
\ engine's emitter and the native chain's, and the collapse belongs to the chain,
\ so every name carries the `-N` the migration publishes under. Loading the five
\ migration files is what compiles them; there is nothing to call.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-branch-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NBRINV-RUN

private

variable T-BYTES
variable T-HOPS
variable T-CHAINS
variable T-IDLE
variable T-ROWS
variable T-CHAINY

: FIELD ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v FMT:.INT ;

public

\ One row's counts, and the same numbers added into the totals. Each is read once
\ and used twice so that a report line and the total below it cannot disagree
\ about what was measured.
: ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NTAILPROBE:CODE-BYTES {: by:n :}
   a u NBRINV:HOPS {: hp:n :}
   a u NBRINV:CHAINS {: ch:n :}
   a u NBRINV:IDLE {: id:n :}
   a u type
   s"  bytes " by FIELD
   s"  b " hp FIELD
   s"  chains " ch FIELD
   s"  idle " id FIELD
   cr
   by T-BYTES +!
   hp T-HOPS +!
   ch T-CHAINS +!
   id T-IDLE +!
   ch 0 > if 1 T-CHAINY +! then
   1 T-ROWS +! ;

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
   s"  with-chains " T-CHAINY @ FIELD
   s"  bytes " T-BYTES @ FIELD
   s"  b " T-HOPS @ FIELD
   s"  chains " T-CHAINS @ FIELD
   s"  idle " T-IDLE @ FIELD
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

NBRINV-RUN:MAIN
