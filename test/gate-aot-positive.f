\ gate-aot-positive.f - entry wrapper for AOT positive checks.
\
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/process-env.f
require tools/build-fixpoint.f
require tools/cli-run.f
require tools/hb-build-lib.f
require tools/aot-call-report-lib.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/aot-lint-core.f
require tools/hb-build-direct-lints.f
require tools/json.f
require tools/gate-json-assert-core.f
require test/gate-common.f
require test/gate-build-common.f
require test/gate-build-hbb.f
include test/gate-aot-positive-lib.f

AOT-POSITIVE:RUN
