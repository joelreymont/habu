\ Exits share the function return from branches and straight-line code.
require lib/test.f

package NATIVE-EXIT-TEST

: CHOOSE ( option<n> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF 10 + exit ENDOF
   ;MATCH ;

: BOTH ( option<n> -- n )
   MATCH option
      none OF 1 exit ENDOF
      some OF drop 2 exit ENDOF
   ;MATCH ;

: BRANCH ( n -- n )
   dup 0 < if drop -1 exit else 1+ then ;

: LAST ( n -- n )
   1+ exit ;

: RUN ( -- )
   T-RESET
   OPTION:NONE CHOOSE 0 T=
   32 OPTION:SOME CHOOSE 42 T=
   OPTION:NONE BOTH 1 T=
   99 OPTION:SOME BOTH 2 T=
   -2 BRANCH -1 T=
   41 BRANCH 42 T=
   41 LAST 42 T=
   T-REPORT ;

RUN
;package
