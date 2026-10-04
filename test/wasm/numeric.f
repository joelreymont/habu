\ numeric.f - runs the numeric golden rows of test/wasm/numeric-rows.f natively.
\
\ Each row runs in a fork of this process (SUBJECT:RUN), which captures the
\ child's stdout and stderr. The fork is for isolation: each row will be its own
\ module on Wasm, so here each runs in a fresh copy of this image, and no row
\ can affect another's dictionary, stack or base. The child evaluates the row's
\ SOURCE as a closed program and then NAME, as the Wasm driver builds the module
\ and then calls its entry. The row is green when the child exits 0, printed
\ exactly the row's bytes and wrote nothing to stderr. The wasm-numeric-aot row
\ runs this file again after test/compiler/aot-mode.f, so the rows' words are
\ compiled by the optimizing compiler as well as the default tier.

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

\ The row the next fork runs; the child inherits these cells.
PTR-VARIABLE SRC-A
variable SRC-U
PTR-VARIABLE NAME-A
variable NAME-U

public

\ The child's whole program: SUBJECT:RUN evaluates the text that names it.
: ROW-EVAL ( -- )
   SRC-A @ SRC-U @ evaluate-closed
   NAME-A @ NAME-U @ evaluate-closed ;

: ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nu:n src:ptr su:n want:ptr wu:n :}
   src SRC-A !  su SRC-U !  name NAME-A !  nu NAME-U !
   s" WASM-NUMERIC-TEST:ROW-EVAL"
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
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
