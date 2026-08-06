\ run-worker-aot.f - resident AOT phase worker.

require lib/source.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require tools/hb-build-lib.f
require tools/aot-call-report-lib.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/json-writer.f
require tools/lint/source-lex.f
require tools/aot-lint-core.f
require tools/signature-lint-core.f
require tools/hb-build-direct-lints.f
require tools/json.f
require tools/gate-json-assert-core.f
require test/gate-common-lib.f
require test/gate-build-common.f
require test/gate-build-hbb.f
require test/gate-aot-positive-lib.f

using JSON

TEST:TRW-LOAD-DONE

package AOT-POS-WORKER

: SETUP ( -- )
   TEST:BUILD-CACHE$ BUILD-CACHE:ROOT! ;

public

: RUN ( -- )
   SETUP
   TEST:RESIDENT case
      7 of AOT-POSITIVE:RUN endof
      E-TBL-BOUNDS throw
   endcase ;

;package

AOT-POS-WORKER:RUN

;using
