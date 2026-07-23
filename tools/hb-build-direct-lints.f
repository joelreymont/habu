\ hb-build-direct-lints.f - in-process lint hooks for hb-build gate callers.
\ Load after lint cores and tools/hb-build-lib.f.

package HB-BUILD-DIRECT-LINTS
private

: LINT-EXIT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc HBB-EXIT ;

: SIGNATURE-LINT ( -- )
   SIGNATURE-LINT-RESET
   HBB-JSON @ SL-JSON!
   2 >FD SL-OUT-FD!
   HBB-SRC$ SIGNATURE-LINT-FILE
   SIGNATURE-LINT-FINISH ;

: AOT-LINT ( -- )
   AOT-LINT:RESET
   HBB-JSON @ AOT-LINT:JSON!
   2 >FD AOT-LINT:OUT-FD!
   HBB-SRC$ AOT-LINT:FILE
   AOT-LINT:FINISH ;

: RUN-AOT ( -- )
   [: AOT-LINT ;] catch LINT-EXIT ;

: RUN-SIGNATURE ( -- )
   [: SIGNATURE-LINT ;] catch LINT-EXIT ;

: INSTALL ( -- )
   [: RUN-AOT ;] is HBB-AOT-LINT-HOOK
   [: RUN-SIGNATURE ;] is HBB-SIGNATURE-LINT-HOOK ;

INSTALL

;package
