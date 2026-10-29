\ checker-decl-depth0-bad.f - a declaration rollback that cannot complete fails
\ closed: the body hands its declaration-event frame back, then closes a checker
\ scope it never opened, emptying the rollback stack. Prepare refuses the
\ declaration (E-FRAME), DECL-EVENT's saved token names no live frame, and
\ DECLARATION-TRANSACTION:RUN dies `declaration rollback failed` (exit 76).
\ Negative case of test/program-diagnostics-test.f.

package CHECKER-DECL-DEPTH0-BAD

: BODY ( -- )
   DECL-EVENT:CURRENT DECL-EVENT:ROLLBACK
   CHECKER-SCOPE-DONE ;

: RUN-IT ( -- )
   [: BODY ;] DECLARATION-TRANSACTION:RUN ;

: MAIN ( -- )
   s" CHECKER-DECL-DEPTH0-ARMED" type cr
   RUN-IT ;

MAIN

;package
