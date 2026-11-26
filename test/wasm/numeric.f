\ numeric.f - runs the numeric golden rows of test/wasm/numeric-rows.f natively.
\ Each row runs in a fork of this process (SUBJECT:RUN), which captures the
\ child's stdout and stderr: each row will be its own module on Wasm, so no row
\ may affect another's dictionary, stack or base. The child evaluates the row's
\ SOURCE as a closed program and then NAME, as the Wasm driver builds the module
\ and calls its entry.

require src/lib/test/assert.f
require src/lib/string.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/process.f

package WASM-NUMERIC-TEST
private

$400 constant CAP
create OUT CAP allot
create ERR CAP allot

\ The row the next fork runs; the child inherits these cells.
PTR-VARIABLE SRC-A
variable SRC-U
PTR-VARIABLE NAME-A
variable NAME-U

public

\ The child's whole program: SUBJECT:RUN evaluates the text that names it.
: ROW-EVAL ( -- )
   SRC-A @ SRC-U @ >str evaluate-closed
   NAME-A @ NAME-U @ >str evaluate-closed ;

: ROW ( str str str -- )
   {: name:str src:str want:str :}
   src STR:DATA SRC-A !  src STR:LEN SRC-U !
   name STR:DATA NAME-A !  name STR:LEN NAME-U !
   s" WASM-NUMERIC-TEST:ROW-EVAL"
   OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN {: outu:len erru:len oc :}
   name T-LABEL  src OUT outu LEN>N >str ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   name T-LABEL  OUT outu LEN>N >str want T$=
   name T-LABEL  erru LEN>N 0 T= ;

;package

\ SUBJECT:RUN forks this process, so the rows run with no package open.
T-RESET
using WASM-NUMERIC-TEST
include test/wasm/numeric-rows.f
;using
T-REPORT
