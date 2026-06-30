\ prelude-test.f - coverage for the prelude conveniences.
\ Load after lib/errors.f lib/string.f lib/test.f lib/float.f lib/prelude.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/prelude.f

: PRE-RUN ( -- )
   T-RESET
   true  TTRUE
   false TFALSE
   5 0<> TTRUE
   0 0<> TFALSE
   7.0 fdrop                     \ fdrop leaves a clean stack
   7.0 fdup f= TTRUE             \ fdup duplicates the top float
   3.0 9.0 fover f> TTRUE fdrop  \ fover copies 2nd to top: 3.0 9.0 3.0
   3.0 5.0 f<= TTRUE   5.0 3.0 f<= TFALSE
   5.0 3.0 f>= TTRUE   3.0 5.0 f>= TFALSE ;

PRE-RUN
T-REPORT
