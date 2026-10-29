\ set-test.f — the fold-buffer bound of tools/lint/intern.f: E-LINT-INTERN-CAP
\ one byte past INTERN-FOLD-CAP, and the exact buffer interns.

require src/lib/errors.f
require tools/lint/intern.f
require src/lib/fmt.f                        \ FMT:.INT

package LINT-INTERN
private

variable TEST-N
: ASSERT  ( bool -- )
   0= if
      s" set-test failed at assertion " type TEST-N @ FMT:.INT cr
      s" set-test failed" 1 die
   then
   TEST-N @ 1+ TEST-N ! ;
: ASSERT=  ( n n -- )  = ASSERT ;

INTERN-FOLD-CAP 1+ constant FOLD-OVER
create FOLD-BUF FOLD-OVER allot
variable FOLD-I

\ E-LINT-INTERN-CAP marks real bounds only. The fold buffer is the one a
\ caller can reach: a string one byte past it throws, the exact buffer interns,
\ so an off-by-one in the guard fails this test in one direction or the other.
: FOLD-FILL  ( -- )
   0 FOLD-I !
   begin FOLD-I @ FOLD-OVER < while
      $41 FOLD-BUF FOLD-I @ + c!
      FOLD-I @ 1+ FOLD-I !
   repeat ;
: FOLD-PAST-CAP   ( -- )  FOLD-BUF FOLD-OVER INTERN-FOLD drop ;
: FOLD?-PAST-CAP  ( -- )  FOLD-BUF FOLD-OVER INTERN-FOLD? drop ;
: TEST-FOLD-BOUND  ( -- )
   INTERN-RESET  FOLD-FILL
   [: FOLD-PAST-CAP ;] catch E-LINT-INTERN-CAP ASSERT=
   [: FOLD?-PAST-CAP ;] catch E-LINT-INTERN-CAP ASSERT=
   INTERN# 0 ASSERT=
   FOLD-BUF INTERN-FOLD-CAP INTERN-FOLD 0 ASSERT=
   FOLD-BUF INTERN-FOLD-CAP INTERN-FOLD? ASSERT
   0 INTERN$ nip INTERN-FOLD-CAP ASSERT=
   0 INTERN$ drop c@ $61 ASSERT= ;

: SET-TEST  ( -- )
   1 TEST-N !
   TEST-FOLD-BOUND
   INTERN-RESET
   s" set-test: ok (" type TEST-N @ 1- FMT:.INT s"  assertions)" type cr ;

SET-TEST

;package
