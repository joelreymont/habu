\ hb-build-direct-lints.f - in-process lint hooks for hb-build gate callers.
\ Load after lint cores and tools/hb-build-lib.f.

: HBB-LINT-EXIT ( n -- ) {: rc:n :}
   rc 0= if exit then
   rc HBB-EXIT ;

: HBB-SIGNATURE-LINT-DIRECT ( -- )
   SIGNATURE-LINT-RESET
   HBB-JSON @ SL-JSON!
   2 >FD SL-OUT-FD!
   HBB-SRC$ SIGNATURE-LINT-FILE
   SIGNATURE-LINT-FINISH ;

: HBB-AOT-LINT-DIRECT ( -- )
   AOT-LINT-RESET
   HBB-JSON @ AL-JSON!
   2 >FD AL-OUT-FD!
   HBB-SRC$ AOT-LINT-FILE
   AOT-LINT-FINISH ;

: HBB-RUN-AOT-LINT-DIRECT ( -- )
   [: HBB-AOT-LINT-DIRECT ;] catch HBB-LINT-EXIT ;

: HBB-RUN-SIGNATURE-LINT-DIRECT ( -- )
   [: HBB-SIGNATURE-LINT-DIRECT ;] catch HBB-LINT-EXIT ;

: HBB-INSTALL-DIRECT-LINTS ( -- )
   [: HBB-RUN-AOT-LINT-DIRECT ;] is HBB-AOT-LINT-HOOK
   [: HBB-RUN-SIGNATURE-LINT-DIRECT ;] is HBB-SIGNATURE-LINT-HOOK ;

HBB-INSTALL-DIRECT-LINTS
