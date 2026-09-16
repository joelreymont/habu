\ Checked multi-error control collects rejects and restores ordinary checking.
require lib/test.f
require test/checker-assert.f

package MULTI-ERROR-API-TEST
private

\ MULTI-ERR-BEGIN/END are checker-internal state transitions. Keep the test's
\ whitebox access at two named boundaries while the assertions remain checked.
: ME-MULTI-BEGIN ( -- ) MULTI-ERR-BEGIN ;
: ME-MULTI-END ( -- n ) MULTI-ERR-END ;

: REJECT ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;


: RUN ( -- )
   T-RESET
   ME-MULTI-BEGIN
   ME-MULTI-END 0 T=
   ME-MULTI-BEGIN
   s" ME-BAD-ONE ( n -- n ) drop" REJECT
   s" ME-BAD-TWO ( n -- n ) dup" REJECT
   ME-MULTI-END 2 T=
   s" ME-BAD-AFTER ( n -- n ) drop" REJECT
   ME-MULTI-END 2 T=
   ME-MULTI-BEGIN
   ME-MULTI-END 0 T=
   \ The checker-state writers remain unavailable to ordinary checked code.
   s" int-mark is refused outside a trusted boundary" T-LABEL
   s" ME-MARK ( -- ) 0 int-mark" CHECK-QUIET-CANDIDATE! 0 T=
   s" min-in-mark is refused outside a trusted boundary" T-LABEL
   s" ME-MIN-MARK ( -- ) 0 0 min-in-mark" CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

RUN
;package
