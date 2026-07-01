\ run-worker-engine.f - resident engine phase worker.

require lib/source.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/gate-common-lib.f
require test/gate-engine-lib.f

: TRWE-UNDER! ( -- )
   TR-UNDER-READY @ 0= if exit then
   TR-RESIDENT-ID @ >IDX TR-PHASE-UNDER? if
      TR-UNDER$ GE-HB!
      TR-UNDER$ GE-CANDIDATE-PATH!
      exit
   then
   0 GE-HB-U !
   0 GE-CAND-U ! ;

: TRWE-RUN ( -- )
   TRWE-UNDER!
   TR-RESIDENT-ID @ case
      5 of GENG-REPAIR-SLICE endof
      9 of GENG-FIXTURES-SLICE endof
      16 of GENG-RUNTIME-SLICE endof
      21 of GENG-VALIDATE-SLICE endof
      E-TBL-BOUNDS throw
   endcase ;

TRWE-RUN
