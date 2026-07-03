\ run-worker-diag-all-strict.f - resident all-strict diagnostic worker.

require lib/vector.f
require lib/source.f
require tools/json.f
require tools/gate-json-assert-core.f
require tools/date.f
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
require tools/argv.f
require tools/check-core.f
require test/gate-common-lib.f
require test/gate-diagnostics-lib.f
require test/gate-diagnostics-all-strict-lib.f

TRW-LOAD-DONE

variable TRWD-FORK-ID

: TRWD-RUN-ID ( idx -- ) {: idx:idx :}
   idx IDX>N TR-RESIDENT-ID !
   idx IDX>N case
      10 of GDX-REPAIR-SLICE endof
      11 of GDX-UNDEF-PRIMARY-SLICE endof
      12 of GDX-ALL-STRICT-SLICE endof
      13 of GDX-FILE-UNSAFE-SLICE endof
      E-TBL-BOUNDS throw
   endcase ;

: TRWD-FORK-RUN ( -- )
   TRWD-FORK-ID @ >IDX TRWD-RUN-ID ;

: TRWD-CHILD-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      10 of s" native checker diagnostics repair slice" endof
      11 of s" native checker diagnostics undef-primary slice" endof
      12 of s" native checker diagnostics all-strict slice" endof
      13 of s" native checker diagnostics file-unsafe slice" endof
      E-TBL-BOUNDS throw
   endcase ;

: TRWD-START-FORK ( idx -- ) {: idx:idx :}
   idx IDX>N TRWD-FORK-ID !
   idx TRWD-CHILD-LABEL idx TRW-CHILD-TEST
   idx TRWD-CHILD-LABEL TR-TIMEOUT-MS [: TRWD-FORK-RUN ;] GT-POOL-START-FORK ;

: TRWD-ALL ( -- )
   GT-POOL-RESET
   12 >IDX TRWD-START-FORK
   11 >IDX TRWD-START-FORK
   10 >IDX TRWD-START-FORK
   13 >IDX TRWD-START-FORK
   GT-POOL-DRAIN ;

TRWD-ALL
