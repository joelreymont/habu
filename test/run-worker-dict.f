\ run-worker-dict.f - resident dictionary/checker phase worker.

require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require lib/json-write.f
require tools/lint/source-lex.f
require tools/check-all-errors-core.f
require test/gate-common-lib.f
require test/gate-dictionary-lib.f

TEST:TRW-LOAD-DONE

package TEST

using GATE-COMMON

: UNDER! ( -- )
   TR-UNDER-READY @ 0= if exit then
   UNDER$ GE-HB! ;

: DICT ( -- )
   UNDER!
   GATE-DICTIONARY:RUN ;

' DICT

;package

execute
