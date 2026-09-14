\ Finally preserves grouped results and runs cleanup on both return paths.
require lib/test.f
require test/checker-assert.f
require lib/executable-build.f

package NFIN
public

PRODUCT point 0 FIELD x n FIELD y n ;PRODUCT
NEWTYPE angle 0
NEWTYPE side 0
CAST: >ANGLE ( n -- angle )
CAST: ANGLE>N ( angle -- n )
CAST: >SIDE ( n -- side )
CAST: SIDE>N ( side -- n )

variable CLEANED
: CLEAN ( -- ) 1 CLEANED +! ;
: BODY ( -- point angle side )
   17 25 NFIN-POINT:MAKE 3 >ANGLE 4 >SIDE ;

: NAMED ( -- point angle side ) ['] BODY ['] CLEAN finally ;
: QUOTED ( -- point angle side ) [: BODY ;] [: CLEAN ;] finally ;
: KEPT ( point -- point point angle side ) ['] BODY ['] CLEAN finally ;
: APPLY ( [ -- point angle side ] -- point angle side ) ['] CLEAN finally ;
\ A trusted boundary retains its quotation effect without ordinary call rows.
\ Executable-build's WITH uses this shape to bracket engine scope operations.
TRUSTED: TRUSTED-APPLY ( R [ R -- S ] -- S ) [: ;] finally ;

: CHECK-POSE ( point angle side -- )
   SIDE>N 4 T= ANGLE>N 3 T=
   NFIN-POINT:UNMAKE 25 T= 17 T= ;

: FAIL-BODY ( point -- point )
   drop 17 25 NFIN-POINT:MAKE -71 throw ;
: THROWING ( point -- point ) ['] FAIL-BODY ['] CLEAN finally ;
: FAIL-CLEAN ( -- ) CLEAN -72 throw ;
: CLEANUP-THROWS ( point -- point ) [: ;] ['] FAIL-CLEAN finally ;
: DEAD-BODY ( -- ) [: -73 throw ;] ['] CLEAN finally ;
: DEAD-CLEANUP ( -- ) [: ;] [: CLEAN -74 throw ;] finally ;

: RUN ( -- )
   T-RESET
   0 CLEANED !
   s" named and literal finally bodies preserve multicell results" T-LABEL
   NAMED CHECK-POSE
   QUOTED CHECK-POSE
   ['] BODY APPLY CHECK-POSE
   ['] BODY TRUSTED-APPLY CHECK-POSE
   ['] BODY EXECUTABLE-BUILD:WITH CHECK-POSE
   s" an unrelated product survives below the body window" T-LABEL
   6 7 NFIN-POINT:MAKE KEPT CHECK-POSE NFIN-POINT:UNMAKE 7 T= 6 T=
   CLEANED @ 4 T=
   s" a throwing body still runs cleanup and restores its stack depth" T-LABEL
   1 2 NFIN-POINT:MAKE ['] THROWING catch -71 T=
   NFIN-POINT:UNMAKE 25 T= 17 T=
   CLEANED @ 5 T=
   s" cleanup throws after a body with a grouped result" T-LABEL
   8 9 NFIN-POINT:MAKE ['] CLEANUP-THROWS catch -72 T=
   NFIN-POINT:UNMAKE 9 T= 8 T=
   CLEANED @ 6 T=
   s" literal throw-only quotations need no returned value layout" T-LABEL
   ['] DEAD-BODY catch -73 T=
   ['] DEAD-CLEANUP catch -74 T=
   CLEANED @ 8 T=
   s" cleanup cannot borrow a body result" T-LABEL
   s" BAD ( -- NFIN:point NFIN:angle NFIN:side ) [: NFIN:BODY ;] [: drop ;] finally"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

RUN
;package
