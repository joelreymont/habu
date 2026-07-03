\ gate-diagnostics.f - entry wrapper for checker diagnostic contracts.
\
\ Load after test/gate-common.f.

require tools/json.f
require tools/gate-json-assert-core.f
require tools/date.f
require lib/vector.f
require lib/source.f
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
include test/gate-diagnostics-lib.f
include test/gate-diagnostics-entry-lib.f

GDX-DISPATCH
