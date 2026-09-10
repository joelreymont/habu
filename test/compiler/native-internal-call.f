\ Internal engine calls use the compiler's authorization, not public lookup.
require lib/test.f
require test/checker-assert.f

package INTERNAL-CALL-TEST
private

\ Compilation is the regression; truncation runs only in the build driver.
TRUSTED: TRUNCATE ( n -- ) seed-ndict! ;


: RUN ( -- )
   T-RESET
   s" seed-ndict!" 0 search-wl 0 T=
   s" seed-ndict!" NDICT:CALL-TARGET 0 T=
   s" BAD-INTERNAL ( n -- ) seed-ndict!" CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;

RUN
;package
