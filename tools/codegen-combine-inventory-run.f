\ codegen-combine-inventory-run.f - the combining lane's measurement, run over
\ every corpus row the chain compiles. One concern: naming those rows and adding
\ their counts up.
\
\ It is a separate file from tools/codegen-combine-inventory.f for the reason the
\ tail probe gives for taking its names as arguments: the counting is about
\ instructions and belongs with the decoder, while WHICH routines are worth
\ counting is a fact about this corpus and changes when the corpus does.
\
\ THE ROWS ARE THE MIGRATED ONES, WHICH IS THE WHOLE POINT. Each corpus row has
\ two compilations - the engine's emitter and the native chain's - and the
\ combining pass belongs to the chain. So every name below carries the `-N` the
\ migration publishes under, and what is counted is the chain's code. Loading the
\ five migration files is what compiles them; there is nothing to call.
\
\ WHAT TO READ OFF IT. A pattern with a column of zeroes here has no consumer in
\ this corpus and is not built - that is the measurement the re-scoped dot asks
\ for, and reporting it is the deliverable, not a reason to go looking for a
\ better corpus. A pattern with a nonzero `safe` column has consumers that an
\ adjacency rewrite alone would catch. A pattern whose `pairs` column exceeds its
\ `safe` column has more consumers still, reachable only before the register
\ allocator has reused the registers - which is where the pass runs.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-combine-inventory.f
require tools/codegen-compare-migrated.f
require tools/codegen-compare-migrated2.f
require tools/codegen-compare-migrated3.f
require tools/codegen-compare-migrated4.f
require tools/codegen-compare-migrated5.f

package NCOMBINV-RUN

private

variable T-INSNS
variable T-MUL
variable T-ADD
variable T-SUB
variable T-LDR
variable T-STR
variable T-MADD
variable T-MADD-SAFE
variable T-MSUB
variable T-MSUB-SAFE
variable T-LDP
variable T-STP
variable T-SHIFT
variable T-SHIFT-FOLD
variable T-BITFIELD
variable T-ROWS

\ One labelled number, on the row's own line: the report is one line per routine
\ so that a reader can sort it, and the engine's `.` ends a line of its own.
: FIELD ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   a u type v FMT:.INT ;

\ One row's counts, and the same numbers added into the totals. They are read
\ once each and used twice so that a report line and the total below it can
\ never disagree about what was measured.
: TALLY ( -- )
   NCOMBINV:INSNS T-INSNS +!
   NCOMBINV:MULS T-MUL +!
   NCOMBINV:ADDS T-ADD +!
   NCOMBINV:SUBS T-SUB +!
   NCOMBINV:LDRS T-LDR +!
   NCOMBINV:STRS T-STR +!
   NCOMBINV:MADDS T-MADD +!
   NCOMBINV:MADD-SAFES T-MADD-SAFE +!
   NCOMBINV:MSUBS T-MSUB +!
   NCOMBINV:MSUB-SAFES T-MSUB-SAFE +!
   NCOMBINV:LDPS T-LDP +!
   NCOMBINV:STPS T-STP +!
   NCOMBINV:SHIFTS T-SHIFT +!
   NCOMBINV:SHIFT-FOLDS T-SHIFT-FOLD +!
   NCOMBINV:BITFIELDS T-BITFIELD +!
   1 T-ROWS +! ;

: COUNTS. ( -- )
   s"  insns " NCOMBINV:INSNS FIELD
   s"  mul " NCOMBINV:MULS FIELD
   s"  add " NCOMBINV:ADDS FIELD
   s"  sub " NCOMBINV:SUBS FIELD
   s"  ldr " NCOMBINV:LDRS FIELD
   s"  str " NCOMBINV:STRS FIELD
   s"  madd " NCOMBINV:MADDS FIELD
   s"  madd-safe " NCOMBINV:MADD-SAFES FIELD
   s"  msub " NCOMBINV:MSUBS FIELD
   s"  msub-safe " NCOMBINV:MSUB-SAFES FIELD
   s"  ldp " NCOMBINV:LDPS FIELD
   s"  stp " NCOMBINV:STPS FIELD
   s"  shift " NCOMBINV:SHIFTS FIELD
   s"  shift-fold " NCOMBINV:SHIFT-FOLDS FIELD
   s"  bitfield " NCOMBINV:BITFIELDS FIELD ;

public

: ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NCOMBINV:ROW!
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
   s"  insns " T-INSNS @ FIELD
   s"  mul " T-MUL @ FIELD
   s"  add " T-ADD @ FIELD
   s"  sub " T-SUB @ FIELD
   s"  ldr " T-LDR @ FIELD
   s"  str " T-STR @ FIELD
   s"  madd " T-MADD @ FIELD
   s"  madd-safe " T-MADD-SAFE @ FIELD
   s"  msub " T-MSUB @ FIELD
   s"  msub-safe " T-MSUB-SAFE @ FIELD
   s"  ldp " T-LDP @ FIELD
   s"  stp " T-STP @ FIELD
   s"  shift " T-SHIFT @ FIELD
   s"  shift-fold " T-SHIFT-FOLD @ FIELD
   s"  bitfield " T-BITFIELD @ FIELD
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

NCOMBINV-RUN:MAIN
