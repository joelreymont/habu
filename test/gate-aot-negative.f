\ gate-aot-negative.f - entry wrapper for AOT rejection checks.
\
require lib/source.f
require tools/json.f
require tools/gate-json-assert-core.f
require test/gate-common.f
include test/gate-aot-negative-lib.f

AOT-NEGATIVE:RUN
