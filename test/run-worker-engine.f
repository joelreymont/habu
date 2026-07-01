\ run-worker-engine.f - resident engine phase worker.

require lib/source.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require test/gate-common-lib.f
require test/gate-engine-lib.f

variable TRWE-FORK-ID

: TRWE-UNDER! ( -- )
   TR-UNDER-READY @ 0= if exit then
   TR-RESIDENT-ID @ >IDX TR-PHASE-UNDER? if
      TR-UNDER$ GE-HB!
      TR-UNDER$ GE-CANDIDATE-PATH!
      exit
   then
   0 GE-HB-U !
   0 GE-CAND-U ! ;

: TRWE-RUN-ID ( idx -- ) {: idx:idx :}
   idx IDX>N TR-RESIDENT-ID !
   TRWE-UNDER!
   idx IDX>N case
      5 of GENG-REPAIR-SLICE endof
      9 of GENG-FIXTURES-SLICE endof
      16 of GENG-RUNTIME-SLICE endof
      21 of GENG-VALIDATE-SLICE endof
      E-TBL-BOUNDS throw
   endcase ;

: TRWE-FORK-RUN ( -- )
   TRWE-FORK-ID @ >IDX TRWE-RUN-ID ;

: TRWE-CHILD-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      5 of s" native engine repair slice" endof
      9 of s" native engine fixture slice" endof
      16 of s" native engine runtime slice" endof
      21 of s" native engine candidate validation slice" endof
      E-TBL-BOUNDS throw
   endcase ;

: TRWE-START-FORK ( idx -- ) {: idx:idx :}
   idx IDX>N TRWE-FORK-ID !
   idx TR-PHASE-TEST
   idx TRWE-CHILD-LABEL TR-TIMEOUT-MS [: TRWE-FORK-RUN ;] GT-POOL-START-FORK ;

: TRWE-POST-CANDIDATE ( -- )
   GT-POOL-RESET
   TR-PRE-REPAIR @ 0= if 5 >IDX TRWE-START-FORK then
   9 >IDX TRWE-START-FORK
   16 >IDX TRWE-START-FORK
   21 >IDX TRWE-START-FORK
   GT-POOL-DRAIN ;

: TRWE-RUN ( -- )
   TR-RESIDENT-ID @ case
      9 of TRWE-POST-CANDIDATE endof
      5 of 5 >IDX TRWE-RUN-ID endof
      16 of 16 >IDX TRWE-RUN-ID endof
      21 of 21 >IDX TRWE-RUN-ID endof
      E-TBL-BOUNDS throw
   endcase ;

TRWE-RUN
