\ run-worker-dict.f - resident dictionary/checker phase worker.

require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require test/gate-common-lib.f
require test/gate-dictionary-lib.f

: TRWK-UNDER! ( -- )
   TR-UNDER-READY @ 0= if exit then
   TR-UNDER$ GE-HB! ;

: TRWK-RUN ( -- )
   TRWK-UNDER!
   GD-MAIN ;

TRWK-RUN
