\ Checked multi-error control collects rejects and restores ordinary checking.
require lib/test.f
require test/checker-assert.f

package MULTI-ERROR-API-TEST
private

: REJECT ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;


: RUN ( -- )
   T-RESET
   MULTI-ERR-BEGIN
   MULTI-ERR-END 0 T=
   MULTI-ERR-BEGIN
   s" ME-BAD-ONE ( n -- n ) drop" REJECT
   s" ME-BAD-TWO ( n -- n ) dup" REJECT
   MULTI-ERR-END 2 T=
   s" ME-BAD-AFTER ( n -- n ) drop" REJECT
   MULTI-ERR-END 2 T=
   MULTI-ERR-BEGIN
   MULTI-ERR-END 0 T=
   \ Marking capabilities still have no checked effect.
   s" ME-MARK ( -- ) 0 int-mark" CHECK-QUIET-CANDIDATE! 1 T=
   s" ME-MIN-MARK ( -- ) 0 0 min-in-mark" CHECK-QUIET-CANDIDATE! 1 T=
   T-REPORT ;

RUN
;package
