require lib/f64-text.f
require lib/test.f

package F64-TEXT-TEST
using F64-TEXT
using IEEE754
using result

create OUTPUT MAX-BYTES allot

: FORMAT-OK ( r -- n )
   OUTPUT MAX-BYTES FORMAT MATCH result
      ok OF ENDOF err OF drop -9899 throw ENDOF
   ;MATCH ;

: PARSE-OK ( ptr u8 n -- r )
   PARSE MATCH result ok OF ENDOF err OF drop -9899 throw ENDOF ;MATCH ;

: EXPECT ( n ptr u8 n -- ) {: bits:n expected:ptr len:n :}
   bits BITS>F64 FORMAT-OK {: actual:n :}
   OUTPUT actual expected len STR= TTRUE
   OUTPUT actual PARSE-OK F64>BITS bits T= ;

: ROUNDTRIP ( n -- ) {: bits:n :}
   bits BITS>F64 FORMAT-OK OUTPUT swap PARSE-OK F64>BITS bits T= ;

: ERROR-CODE ( result<r,fault> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF MATCH fault
         malformed OF 1 ENDOF trailing OF 2 ENDOF nonfinite OF 3 ENDOF
         capacity OF 4 ENDOF runtime OF 5 ENDOF
      ;MATCH ENDOF
   ;MATCH ;

: FORMAT-ERROR ( result<n,fault> -- n )
   MATCH result
      ok OF drop 0 ENDOF
      err OF MATCH fault
         malformed OF 1 ENDOF trailing OF 2 ENDOF nonfinite OF 3 ENDOF
         capacity OF 4 ENDOF runtime OF 5 ENDOF
      ;MATCH ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   0 s" 0" EXPECT $8000000000000000 s" -0" EXPECT
   $3FF0000000000000 s" 1" EXPECT $BFF8000000000000 s" -1.5" EXPECT
   $4090000000000000 s" 1024" EXPECT $3FB999999999999A s" 0.1" EXPECT
   $3EE4F8B588E368F1 s" 0.00001" EXPECT
   $39E0000000000000 s" 0.000000000000000000000000000006310887241768095" EXPECT
   $430C6BF526340002 s" 1000000000000000.3" EXPECT
   $C30C6BF526340002 s" -1000000000000000.3" EXPECT
   s" +.5e+1" PARSE-OK F64>BITS $4014000000000000 T=
   s" -1e-9999" PARSE-OK F64>BITS $8000000000000000 T=
   s" 4.9406564584124654e-324" PARSE-OK F64>BITS 1 T=
   s" 1.00000000000000011102230246251565404236316680908203125" PARSE-OK F64>BITS $3FF0000000000000 T=
   s" 1.00000000000000011102230246251565404236316680908203126" PARSE-OK F64>BITS $3FF0000000000001 T=
   s" 2.4703282292062327e-324" PARSE-OK F64>BITS 0 T=
   s" 2.4703282292062328e-324" PARSE-OK F64>BITS 1 T=
   s" 1e9999" PARSE ERROR-CODE 3 T=
   s" NaN" PARSE ERROR-CODE 3 T= s" -infinity" PARSE ERROR-CODE 3 T=
   s" " PARSE ERROR-CODE 1 T= s" ." PARSE ERROR-CODE 1 T=
   s" 1e+" PARSE ERROR-CODE 1 T= s"  1" PARSE ERROR-CODE 1 T=
   s" 1x" PARSE ERROR-CODE 2 T= s" 1 " PARSE ERROR-CODE 2 T=
   $5A OUTPUT c! 1.5 OUTPUT 2 FORMAT FORMAT-ERROR 4 T= OUTPUT c@ $5A T=
   1 ROUNDTRIP 2 ROUNDTRIP $FFFFFFFFFFFFF ROUNDTRIP $10000000000000 ROUNDTRIP
   $7FEFFFFFFFFFFFFF ROUNDTRIP $FFEFFFFFFFFFFFFF ROUNDTRIP
   2048 0 do
      i 6364136223846793005 * 1442695040888963407 +
      $FFEFFFFFFFFFFFFF and ROUNDTRIP
   loop
   T-REPORT ;

RUN
;using
;using
;using
;package

\ Reopen the owning package to witness restoration of the native thread state.
package F64-TEXT
using FFI
using MEM

create STATE-OUTPUT MAX-BYTES allot
s" freelocale" SYMBOL-FIND constant FREELOCALE-FN
TRUSTED: FREELOCALE-CALL ( -- ) ARGS REG-LENS 1 FREELOCALE-FN ffi-call-bounded drop ;
: FREE-LOCALE ( n -- ) RESET 0 VALUE! FREELOCALE-CALL ;
: TEST-LOCALE ( -- n )
   s" C.UTF-8" CSTRING {: name:ptr :}
   RESET 2 0 VALUE! name 1 READABLE! 0 2 VALUE! NEWLOCALE-CALL ;

: STATE-PARSE ( ptr u8 n n -- ) {: expected:n :}
   PARSE MATCH result
      ok OF IEEE754:F64>BITS expected T= ENDOF
      err OF drop -9899 throw ENDOF
   ;MATCH ;

: STATE-FORMAT ( n -- ) {: capacity:n :}
   0.1 STATE-OUTPUT capacity FORMAT MATCH result
      ok OF 3 T= STATE-OUTPUT 3 s" 0.1" STR= TTRUE ENDOF
      err OF MATCH fault
         capacity OF capacity 2 T= ENDOF
         malformed OF -9899 throw ENDOF trailing OF -9899 throw ENDOF
         nonfinite OF -9899 throw ENDOF runtime OF -9899 throw ENDOF
      ;MATCH ENDOF
   ;MATCH ;

: RESTORATION ( -- )
   GET-ROUND {: prior-round:n :}
   TEST-LOCALE {: locale:n :} locale 0 T<>
   locale USE-LOCALE {: prior-locale:n :}
   4 0 do
      i $400000 * {: mode:n :} mode SET-ROUND 0 T=
      s" 0.1" $3FB999999999999A STATE-PARSE
      GET-ROUND mode T= 0 USE-LOCALE locale T=
      MAX-BYTES STATE-FORMAT GET-ROUND mode T= 0 USE-LOCALE locale T=
      2 STATE-FORMAT GET-ROUND mode T= 0 USE-LOCALE locale T=
      s" 1e9999" PARSE MATCH result
         ok OF drop -9899 throw ENDOF
         err OF MATCH fault
            nonfinite OF ENDOF malformed OF -9899 throw ENDOF
            trailing OF -9899 throw ENDOF capacity OF -9899 throw ENDOF
            runtime OF -9899 throw ENDOF
         ;MATCH ENDOF
      ;MATCH
      GET-ROUND mode T= 0 USE-LOCALE locale T=
   loop
   prior-round SET-ROUND 0 T= prior-locale USE-LOCALE 0 T<>
   locale FREE-LOCALE T-REPORT ;

RESTORATION
;using
;using
;package
