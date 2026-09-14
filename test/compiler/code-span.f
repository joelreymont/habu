require lib/test.f
require src/habu/code-span.f

package CODE-SPAN-TEST

: RUN ( -- )
   T-RESET
   s" legacy final slots and explicit complete spans" T-LABEL
   0 CODE-SPAN:BYTES 4 T=
   12 CODE-SPAN:BYTES 16 T=
   16 CODE-SPAN:EXACT dup CODE-SPAN:BODY 16 T=
   CODE-SPAN:BYTES 16 T=
   s" malformed encoded lengths refuse before code reads" T-LABEL
   -1 CODE-SPAN:VALID? TFALSE
   $100000000 CODE-SPAN:VALID? TFALSE
   CODE-SPAN:FULL CODE-SPAN:VALID? TFALSE
   $80000003 CODE-SPAN:VALID? TFALSE
   $80000004 CODE-SPAN:VALID? TTRUE
   T-REPORT ;

' RUN
;package
execute
