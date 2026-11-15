\ replay-scope.f - the check tool's replay scope for a test that binds names
\ only the checker holds. A row CHECK!, CHECK-UNJUDGED! or a declaration replay
\ records has no engine record, so compiled code binds nothing to its name
\ (src/core/checker.f "ONE LOOKUP BINDS A NAME"); only a replay binds it,
\ through the codeless record the checker's overlay (CHECKER-OVERLAY) publishes
\ while open.

require src/lib/error.f
require src/habu/layout.f
require src/core/checker-owner-guard.f

package TOOL-REPLAY

\ The declaration owner's field at off, as verify-source.f OWNER-XT reads it.
: OWNER-XT ( n -- n )
   {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= if E-COMP-OWNER throw then ;

\ The owner record stores raw execution tokens; the window's start and done
\ take and leave nothing.
CAST: ACTION ( n -- [ -- ] )

: WINDOW-START ( -- )
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF OWNER-XT ACTION execute ;

: WINDOW-DONE ( -- )
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF OWNER-XT ACTION execute ;

public

\ RUN runs q in a checker scope, inside the window the declaration owner's
\ start opens; the window's done runs however q ends, and the scope's close
\ undoes every record it published.
: RUN ( [ -- ] -- ) {: q :}
   [: WINDOW-START q [: WINDOW-DONE ;] finally ;] CHECK:SCOPED ;

;package
