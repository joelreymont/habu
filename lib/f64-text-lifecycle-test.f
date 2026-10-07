\ f64-text-lifecycle-test.f - the process-local C locale across image preparation.
require lib/f64-text.f
require lib/test.f

package F64-TEXT
using IEEE754
using result


: LIFECYCLE-USE ( -- )
   s" 1.5" PARSE MATCH result
      ok OF F64>BITS $3FF8000000000000 T= ENDOF
      err OF drop -9899 throw ENDOF
   ;MATCH ;


: LIFECYCLE-ASSERT-LIVE ( -- )
   C-LOCALE @ 0 T<> REGISTERED @ 1 T= ;


: LIFECYCLE-ASSERT-CLEAR ( -- )
   C-LOCALE @ 0 T= REGISTERED @ 0 T= ;


\ First use makes the locale and registers its release; preparing an image
\ frees it, and the next use makes a new one through the re-resolved calls.
: LIFECYCLE-RUN ( -- )
   T-RESET
   IMAGE-LIFECYCLE:PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE LIFECYCLE-ASSERT-LIVE
   IMAGE-LIFECYCLE:PREPARE LIFECYCLE-ASSERT-CLEAR
   LIFECYCLE-USE LIFECYCLE-ASSERT-LIVE
   IMAGE-LIFECYCLE:PREPARE LIFECYCLE-ASSERT-CLEAR
   T-REPORT ;


LIFECYCLE-RUN
;using
;using
;package
