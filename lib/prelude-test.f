\ prelude-test.f - coverage for the prelude conveniences.
\ Load after lib/errors.f lib/string.f lib/test.f lib/float.f lib/prelude.f.

: PRE-RUN ( -- )
   T-RESET
   true  TTRUE
   false TFALSE
   5 0<> TTRUE
   0 0<> TFALSE
   7.0 fdrop ;                   \ fdrop leaves a clean stack

PRE-RUN
T-REPORT
