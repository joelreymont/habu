\ checker-decl-nested-bad.f - a nested declaration body that destroys its own
\ checker rollback frame is refused fail-closed.
\
\ The body first hands the coordinator's declaration-event frame back, which
\ lowers the product-field transaction depth to the value its checker frame
\ recorded, so the ordinary product-field guard no longer intercepts what comes
\ next. It then closes a checker scope it never opened. That pop retires the
\ INNER declaration's own frame, leaving the ENCLOSING declaration's frame on
\ top. The inner PREPARE must refuse that frame: it carries the declaration tag
\ but the enclosing coordinator's depth, and accepting it would let the inner
\ FINALIZE discard the enclosing declaration's savepoint. Rollback then finds no
\ frame of its own and dies with the named diagnostic (exit 76).
\
\ Registered as a negative gate case in test/candidate-validation.f. The ARMED
\ marker is the only stdout line, so a build that accepts the wrong coordinator
\ depth returns from the inner declaration, prints LEAKED-PAST-INNER, and fails
\ the exact-marker check even though it also ends up at exit 76.

package CHECKER-DECL-NESTED-BAD

: INNER-BODY ( -- )
   DECL-EVENT:CURRENT DECL-EVENT:ROLLBACK
   CHECKER-SCOPE-DONE ;

: INNER ( -- )
   [: INNER-BODY ;] GENERATED-DECL:RUN ;

: OUTER-BODY ( -- )
   INNER
   s" LEAKED-PAST-INNER" type cr ;

: OUTER ( -- )
   [: OUTER-BODY ;] GENERATED-DECL:RUN ;

: MAIN ( -- )
   s" CHECKER-DECL-NESTED-ARMED" type cr
   OUTER ;

MAIN

;package
