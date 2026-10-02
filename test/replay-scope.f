\ replay-scope.f - the check tool's replay scope for a test that binds names
\ only the checker holds.
\
\ A row CHECK!, CHECK-UNJUDGED! or a declaration replay records has no engine
\ record, so compiled code binds nothing to its name (src/core/checker.f "ONE
\ LOOKUP BINDS A NAME"); only a replay binds it, over the checker's own records
\ (REPLAY-BIND). Mirror authority is the verifier window's alone
\ (CHECKER-PKG-MIRROR-AUTHORITY?): a package-neutral scope declares top level
\ and grants none. So OPEN does what tools/check-core.f CHK-RUN-NOMINAL-LINTS
\ does - a neutral scope, then the window the declaration owner's start opens in
\ it - and CLOSE undoes both.

require lib/errors.f
require src/habu/layout.f
require src/core/checker-owner-guard.f

package REPLAY-SCOPE

\ The declaration owner's field at off, as verify-source.f OWNER-XT reads it.
: OWNER-XT ( n -- n )
   {: off:n :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   off CELL + CHECKER-OWNER-GUARD:VALIDATE
   off + CELL-VIEW @ dup 0= if E-NCOMP-OWNER throw then ;

\ The owner record stores raw execution tokens; the window's start and done
\ take and leave nothing.
CAST: ACTION ( n -- [ -- ] )

public

: OPEN ( -- )
   CHECKER-SCOPE-START-NEUTRAL
   NCOMP-DISPATCH:DECL-VERIFY-START-OFF OWNER-XT ACTION execute ;

: CLOSE ( -- )
   NCOMP-DISPATCH:DECL-VERIFY-DONE-OFF OWNER-XT ACTION execute
   CHECKER-SCOPE-DONE ;

;package
