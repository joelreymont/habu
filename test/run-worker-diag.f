\ run-worker-diag.f - resident diagnostic phase worker.

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

TEST-RUN:TRW-LOAD-DONE

package TEST-RUN

: DIAG ( -- )
   TEST-RUN:RESIDENT case
      10 of GATE-DIAGNOSTICS:REPAIR endof
      11 of GATE-DIAGNOSTICS:UNDEFINED-PRIMARY endof
      13 of GATE-DIAGNOSTICS:FILE-UNSAFE endof
      E-TBL-BOUNDS throw
   endcase ;

' DIAG

;package

execute
