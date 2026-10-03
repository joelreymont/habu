\ numeric.f - runs the numeric golden rows of test/wasm/numeric-rows.f natively.
\
\ Each row's program runs in a fork of this process (SUBJECT:RUN), which
\ captures the child's stdout: `f.` writes to the descriptor rather than
\ through the genio output funnel `.` and `u.` use, so only the descriptor sees
\ every printer. The row is green when the child exits 0, printed exactly the
\ row's bytes and wrote nothing to stderr. The wasm-numeric-aot row runs this
\ file again after test/compiler/aot-mode.f, so the rows' words are compiled by
\ the optimizing compiler as well as the default tier.

require lib/test.f
require lib/string.f
require lib/test/outcome.f
require lib/test/subject.f

package WASM-NUMERIC-TEST
private

$400 constant CAP
create OUT CAP allot
create ERR CAP allot
30000 constant TIMEOUT-MS               \ a hang guard: a row runs in milliseconds

public

: ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   src su OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   name nu T-LABEL  src su OUT outu LEN>N ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   name nu T-LABEL  OUT outu LEN>N want wu T$=
   name nu T-LABEL  erru LEN>N 0 T= ;

;package

\ SUBJECT:RUN forks this process, so the rows run with no package open.
T-RESET
using WASM-NUMERIC-TEST
include test/wasm/numeric-rows.f
;using
T-REPORT
