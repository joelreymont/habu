\ checker-decl-depth0-bad.f - a declaration rollback that cannot complete fails
\ closed.
\
\ The body hands its declaration-event frame back and then closes a checker
\ scope it never opened, which retires the declaration's own checker frame and
\ leaves the rollback stack empty. The checker frame's prepare refuses the
\ declaration (E-FRAME), and the rollback cannot restore DECL-EVENT: its saved
\ token no longer names a live frame. With a savepoint left live and nothing to
\ restore it from, DECLARATION-TRANSACTION:RUN dies `declaration rollback
\ failed` (exit 76) instead of returning.
\
\ Loaded as a negative case by test/program-diagnostics-test.f.

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
