\ run-worker-diag-all-strict.f - resident all-strict diagnostic worker.

require lib/vector.f
require lib/source.f
require tools/json.f
require tools/gate-json-assert-core.f
require lib/date.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/diag-origin-core.f
require tools/json-only-core.f
require tools/signature-lint-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/trust-lint-core.f
require tools/public-signatures-core.f
require tools/check-all-errors-core.f
require lib/argv.f
require tools/check-core.f
require test/gate-common-lib.f
require test/gate-diagnostics-lib.f
require test/gate-diagnostics-all-strict-lib.f

TRW-LOAD-DONE

package TEST-RUN

variable FORK-ID

: RUN-ID ( idx -- ) {: idx:idx :}
   idx IDX>N TR-RESIDENT-ID !
   idx IDX>N case
      10 of GATE-DIAGNOSTICS:REPAIR endof
      11 of GATE-DIAGNOSTICS:UNDEFINED-PRIMARY endof
      12 of GATE-DIAGNOSTICS:ALL-STRICT endof
      13 of GATE-DIAGNOSTICS:FILE-UNSAFE endof
      E-TBL-BOUNDS throw
   endcase ;

: FORK-RUN ( -- )
   FORK-ID @ >IDX RUN-ID ;

: CHILD-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      10 of s" native checker diagnostics repair slice" endof
      11 of s" native checker diagnostics undef-primary slice" endof
      12 of s" native checker diagnostics all-strict slice" endof
      13 of s" native checker diagnostics file-unsafe slice" endof
      E-TBL-BOUNDS throw
   endcase ;

: START-FORK ( idx -- ) {: idx:idx :}
   idx IDX>N FORK-ID !
   idx CHILD-LABEL idx TRW-CHILD-TEST
   idx CHILD-LABEL TR-TIMEOUT-MS [: FORK-RUN ;] GT-POOL-START-FORK ;

: DIAG-ALL ( -- )
   GT-POOL-RESET
   12 >IDX START-FORK
   11 >IDX START-FORK
   10 >IDX START-FORK
   13 >IDX START-FORK
   GT-POOL-DRAIN ;

' DIAG-ALL

;package

execute
