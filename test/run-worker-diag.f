\ run-worker-diag.f - resident diagnostic phase worker.

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

TRW-LOAD-DONE

: TRWD-RUN ( -- )
   TR-RESIDENT-ID @ case
      10 of GDX-REPAIR-SLICE endof
      11 of GDX-UNDEF-PRIMARY-SLICE endof
      13 of GDX-FILE-UNSAFE-SLICE endof
      E-TBL-BOUNDS throw
   endcase ;

TRWD-RUN
