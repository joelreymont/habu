\ checker-decl-depth0-bad.f - the declaration rollback walk stays inside the
\ checker frame arena when a body leaves no frames at all.
\
\ Same destruction as test/checker-decl-nested-bad.f, but at the OUTERMOST
\ declaration and after the frame arena has been grown, so the record below the
\ arena base is freshly mapped memory instead of dictionary bytes. The body hands
\ its declaration-event frame back and then closes a checker scope it never
\ opened, which retires the declaration's own frame and leaves the rollback stack
\ empty. A rollback that peeks at the record below the base reads that fresh
\ memory, believes it found an ordinary scope, and pops past the arena; the
\ measured result before the walk was bounded was SIGSEGV (exit 134). The walk
\ must be bounded by the live depth and fail closed with the named diagnostic
\ (exit 76) instead.
\
\ Registered as a negative gate case in test/candidate-validation.f.

package CHECKER-DECL-DEPTH0-BAD

\ Well past the boot frame capacity, so the frame arena is reallocated before the
\ declaration runs and the record below its base is no longer dictionary bytes.
40 constant GROW-SCOPES

: PUSH-SCOPES ( n -- )
   BEGIN dup 0 > WHILE
      CHECKER-SCOPE-START
      1 -
   REPEAT
   drop ;

: POP-SCOPES ( n -- )
   BEGIN dup 0 > WHILE
      CHECKER-SCOPE-DONE
      1 -
   REPEAT
   drop ;

: GROW-FRAMES ( -- )
   GROW-SCOPES PUSH-SCOPES
   GROW-SCOPES POP-SCOPES ;

: BODY ( -- )
   DECL-EVENT:CURRENT DECL-EVENT:ROLLBACK
   CHECKER-SCOPE-DONE ;

: RUN-IT ( -- )
   [: BODY ;] GENERATED-DECL:RUN ;

: MAIN ( -- )
   GROW-FRAMES
   s" CHECKER-DECL-DEPTH0-ARMED" type cr
   RUN-IT ;

MAIN

;package
