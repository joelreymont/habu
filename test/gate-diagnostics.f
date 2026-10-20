\ gate-diagnostics.f - entry wrapper for checker diagnostic contracts.
\
require test/gate-common.f
require tools/gate-json-assert-core.f
require src/lib/date.f
require src/lib/vector.f
require src/lib/source.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require tools/diag-origin-core.f
require tools/json-only-core.f
require tools/checked-boundary-lint-core.f
require tools/reserved-name-lint-core.f
require tools/public-signatures-core.f
require tools/check-all-errors-core.f
require src/lib/argv.f
require tools/check-core.f
require test/gate-diagnostics-lib.f
include test/gate-diagnostics-entry-lib.f

package GATE-DIAGNOSTICS

' RUN

;package

execute
