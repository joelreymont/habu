\ codegen-callsite-inventory-run.f - the call-site measurement, run over every
\ corpus row the chain compiles. One concern: naming those rows and adding their
\ counts up.
\
\ It is a separate file from tools/codegen-callsite-inventory.f for the reason
\ the other two runners give: the counting is about instructions and belongs with
\ the decoders, while WHICH routines are worth counting is a fact about this
\ corpus and changes when the corpus does. The row list is the one
\ tools/codegen-loop-inventory-run.f and tools/codegen-combine-inventory-run.f
\ name, so the three inventories describe one set of routines and a row added to
\ the corpus arrives in all of them.
\
\ WHAT TO READ OFF IT. `site` is the marshalling beside a call and is what a
\ register calling convention would remove; `own` is the routine's own entry and
\ exit crossing and is what it would not, because every routine the chain
\ publishes is an ordinary dictionary record the engine can enter. The TOTAL line
\ is the one the lane's decision rests on: a small `site` total across the whole
\ corpus says the round trip the convention was scoped to delete is already
\ absent, and where it is absent the reason is worth knowing - a tail call leaves
\ its arguments where the callee already reads them, and the chain's own inliner
\ has copied most of the remaining callees into their callers.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-callsite-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NSITEINV-RUN

private

variable T-INSNS
variable T-CALLS
variable T-SITE
variable T-OWN
variable T-ROWS
variable T-CALLERS

: FIELD ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v FMT:.INT ;

\ One row's counts, and the same numbers added into the totals. They are read
\ once each and used twice so that a report line and the total below it can never
\ disagree about what was measured.
: TALLY ( -- )
   NCOMBINV:INSNS T-INSNS +!
   NSITEINV:CALLS T-CALLS +!
   NSITEINV:SITE T-SITE +!
   NSITEINV:OWN T-OWN +!
   NSITEINV:SITE 0 > if 1 T-CALLERS +! then
   1 T-ROWS +! ;

: COUNTS. ( -- )
   s"  insns " NCOMBINV:INSNS FIELD
   s"  calls " NSITEINV:CALLS FIELD
   s"  site " NSITEINV:SITE FIELD
   s"  own " NSITEINV:OWN FIELD ;

public

: ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NSITEINV:ROW!
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
   s"  with-site " T-CALLERS @ FIELD
   s"  insns " T-INSNS @ FIELD
   s"  calls " T-CALLS @ FIELD
   s"  site " T-SITE @ FIELD
   s"  own " T-OWN @ FIELD
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

NSITEINV-RUN:MAIN
