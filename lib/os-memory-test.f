\ os-memory-test.f - OS-MEMORY:PAGE-SIZE against the host's own answer.
\ `getconf PAGESIZE` asks the same kernel from a separate process. PAGE-SIZE
\ must equal it before and after IMAGE-LIFECYCLE:PREPARE, which forgets the
\ resolved getpagesize address (lib/ffi-abi.f FORGET-SYMBOLS), so the second
\ query resolves the binding again as a restored image does
\ (docs/native-applications.md "Process page size").
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/process-argv.f
require lib/process-env.f
require lib/image-lifecycle.f
require lib/test.f
require lib/test/outcome.f
require lib/os-memory.f

package OS-MEMORY-TEST

64 constant OUT-CAP
256 constant ERR-CAP
10000 constant TIMEOUT-MS
FS-PATH-CAP BUFFER: GETCONF-PATH
OUT-CAP BUFFER: OUT
ERR-CAP BUFFER: ERR

: GETCONF ( -- ptr u8 len )
   s" getconf" >LEN GETCONF-PATH FIND-EXECUTABLE MATCH option
      none OF
         2 S\" os-memory-test: getconf is not on PATH\n" write drop
         E-PROC-PATH throw
      ENDOF
      some OF ENDOF
   ;MATCH GETCONF-PATH swap ;

: HOST-PAGE-SIZE ( -- n )
   PROC-ARGV-RESET
   s" PAGESIZE" >LEN PROC-ARGV+
   GETCONF OUT OUT-CAP >LEN ERR ERR-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME
   0 T-OUTCOME-EXITED= drop LEN>N {: outu:n :}
   OUT outu TRIM STR>NUMBER? MATCH option
      none OF E-PROC-OUTPUT throw ENDOF
      some OF ENDOF
   ;MATCH ;

: SURVIVES-PREPARE ( -- )
   HOST-PAGE-SIZE {: want:n :}
   s" PAGE-SIZE is the host's page size" T-LABEL
   OS-MEMORY:PAGE-SIZE want T=
   IMAGE-LIFECYCLE:PREPARE
   s" PAGE-SIZE resolves again after image preparation" T-LABEL
   OS-MEMORY:PAGE-SIZE want T= ;

T-RESET
SURVIVES-PREPARE
T-REPORT

;package
