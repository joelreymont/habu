\ array-test.f - focused tests for checked stdlib array helpers.
\ Run: cat lib/errors.f lib/array.f lib/array-test.f | bin/hb

1 constant AT-EX-FAIL

variable AT-CASE
variable AT-FAIL

create AT-EMPTY
create AT-ONE 42 ,
create AT-NORMAL 3 , -1 , 4 , 4 , 2 ,
create AT-WORK 5 cells allot

: AT= {: got want :} ( n n -- )
   AT-CASE @ 1 + AT-CASE !
   got want <> IF
      [char] F emit AT-CASE @ .
      AT-FAIL @ 1 + AT-FAIL !
   THEN ;

: AT-WORK1 ( n -- )
   AT-WORK ! ;

: AT-WORK4 ( n n n n -- )
   AT-WORK 3 cells + !
   AT-WORK 2 cells + !
   AT-WORK cell+ !
   AT-WORK ! ;

: AT-WORK5 ( n n n n n -- )
   AT-WORK 4 cells + !
   AT-WORK 3 cells + !
   AT-WORK 2 cells + !
   AT-WORK cell+ !
   AT-WORK ! ;

: AT-CHECK-INDEX-VALID ( -- )
   5 0 A-CHECK-INDEX
   5 4 A-CHECK-INDEX ;

: AT-CHECK-INDEX-NEG ( -- )
   5 -1 A-CHECK-INDEX ;

: AT-CHECK-INDEX-HIGH ( -- )
   5 5 A-CHECK-INDEX ;

: AT-CHECK-INDEX-EMPTY ( -- )
   0 0 A-CHECK-INDEX ;

: AT-CHECK-RANGE-VALID ( -- )
   5 0 0 A-CHECK-RANGE
   5 5 0 A-CHECK-RANGE
   5 1 3 A-CHECK-RANGE ;

: AT-CHECK-RANGE-NEG-START ( -- )
   5 -1 1 A-CHECK-RANGE ;

: AT-CHECK-RANGE-NEG-COUNT ( -- )
   5 1 -1 A-CHECK-RANGE ;

: AT-CHECK-RANGE-HIGH-START ( -- )
   5 6 0 A-CHECK-RANGE ;

: AT-CHECK-RANGE-OVERRUN ( -- )
   5 3 3 A-CHECK-RANGE ;

: AT-CHECK-NONEMPTY-VALID ( -- )
   1 A-CHECK-NONEMPTY ;

: AT-CHECK-NONEMPTY-EMPTY ( -- )
   0 A-CHECK-NONEMPTY ;

: AT-CHECK-NONEMPTY-NEG ( -- )
   -1 A-CHECK-NONEMPTY ;

: AT-AFETCH-HIGH ( -- )
   AT-NORMAL 5 5 A@ drop ;

: AT-ASTORE-HIGH ( -- )
   9 AT-WORK 5 5 A! ;

: AT-APLUS-HIGH ( -- )
   1 AT-WORK 5 5 A+! ;

: AT-ASWAP-HIGH ( -- )
   AT-WORK 5 0 5 A-SWAP ;

: AT-ASWAP-NEG ( -- )
   AT-WORK 5 -1 0 A-SWAP ;

: AT-LAST-EMPTY ( -- )
   0 LAST-INDEX drop ;

: AT-MIRROR-HIGH ( -- )
   5 5 MIRROR-INDEX drop ;

: AT-MIN-EMPTY ( -- )
   AT-EMPTY 0 A-MIN drop ;

: AT-MAX-EMPTY ( -- )
   AT-EMPTY 0 A-MAX drop ;

: AT-ARGMAX-EMPTY ( -- )
   AT-EMPTY 0 A-ARGMAX drop ;

: AT-TEST-CHECKS ( -- )
   ['] AT-CHECK-INDEX-VALID catch 0 AT=
   ['] AT-CHECK-INDEX-NEG catch E-A-BOUNDS AT=
   ['] AT-CHECK-INDEX-HIGH catch E-A-BOUNDS AT=
   ['] AT-CHECK-INDEX-EMPTY catch E-A-BOUNDS AT=
   ['] AT-CHECK-RANGE-VALID catch 0 AT=
   ['] AT-CHECK-RANGE-NEG-START catch E-A-BOUNDS AT=
   ['] AT-CHECK-RANGE-NEG-COUNT catch E-A-BOUNDS AT=
   ['] AT-CHECK-RANGE-HIGH-START catch E-A-BOUNDS AT=
   ['] AT-CHECK-RANGE-OVERRUN catch E-A-BOUNDS AT=
   ['] AT-CHECK-NONEMPTY-VALID catch 0 AT=
   ['] AT-CHECK-NONEMPTY-EMPTY catch E-A-EMPTY AT=
   ['] AT-CHECK-NONEMPTY-NEG catch E-A-BOUNDS AT= ;

: AT-TEST-ACCESS ( -- )
   AT-ONE 1 0 A@ 42 AT=
   AT-NORMAL 5 0 A@ 3 AT=
   AT-NORMAL 5 2 A@ 4 AT=
   ['] AT-AFETCH-HIGH catch E-A-BOUNDS AT=
   0 AT-WORK1
   99 AT-WORK 1 0 A!
   AT-WORK 1 0 A@ 99 AT=
   ['] AT-ASTORE-HIGH catch E-A-BOUNDS AT= ;

: AT-TEST-PROMOTED-HELPERS ( -- )
   1 2 3 4 AT-WORK4
   5 AT-WORK 4 1 A+!
   AT-WORK 4 1 A@ 7 AT=
   ['] AT-APLUS-HIGH catch E-A-BOUNDS AT=
   AT-WORK 4 0 3 A-SWAP
   AT-WORK 4 0 A@ 4 AT=
   AT-WORK 4 3 A@ 1 AT=
   ['] AT-ASWAP-HIGH catch E-A-BOUNDS AT=
   ['] AT-ASWAP-NEG catch E-A-BOUNDS AT=
   5 LAST-INDEX 4 AT=
   ['] AT-LAST-EMPTY catch E-A-EMPTY AT=
   5 1 MIRROR-INDEX 3 AT=
   ['] AT-MIRROR-HIGH catch E-A-BOUNDS AT=
   4 EVEN? -1 AT=
   5 EVEN? 0 AT=
   -2 EVEN? -1 AT=
   -3 EVEN? 0 AT= ;

: AT-TEST-SCALARS ( -- )
   AT-EMPTY 0 A-SUM 0 AT=
   AT-ONE 1 A-SUM 42 AT=
   AT-NORMAL 5 A-SUM 12 AT=
   ['] AT-MIN-EMPTY catch E-A-EMPTY AT=
   AT-ONE 1 A-MIN 42 AT=
   AT-NORMAL 5 A-MIN -1 AT=
   ['] AT-MAX-EMPTY catch E-A-EMPTY AT=
   AT-ONE 1 A-MAX 42 AT=
   AT-NORMAL 5 A-MAX 4 AT=
   AT-EMPTY 0 A-COUNT-EVEN 0 AT=
   AT-ONE 1 A-COUNT-EVEN 1 AT=
   AT-NORMAL 5 A-COUNT-EVEN 3 AT=
   ['] AT-ARGMAX-EMPTY catch E-A-EMPTY AT=
   AT-ONE 1 A-ARGMAX 0 AT=
   AT-NORMAL 5 A-ARGMAX 2 AT= ;

: AT-TEST-REVERSE ( -- )
   88 AT-WORK1
   AT-WORK 0 A-REVERSE!
   88 AT-WORK1
   AT-WORK 1 A-REVERSE!
   AT-WORK 1 0 A@ 88 AT=
   1 2 3 4 AT-WORK4
   AT-WORK 4 A-REVERSE!
   AT-WORK 4 0 A@ 4 AT=
   AT-WORK 4 1 A@ 3 AT=
   AT-WORK 4 2 A@ 2 AT=
   AT-WORK 4 3 A@ 1 AT= ;

: AT-TEST-PREFIX ( -- )
   77 AT-WORK1
   AT-WORK 0 A-PREFIX-SUM!
   77 AT-WORK1
   AT-WORK 1 A-PREFIX-SUM!
   AT-WORK 1 0 A@ 77 AT=
   3 -1 4 1 5 AT-WORK5
   AT-WORK 5 A-PREFIX-SUM!
   AT-WORK 5 0 A@ 3 AT=
   AT-WORK 5 1 A@ 2 AT=
   AT-WORK 5 2 A@ 6 AT=
   AT-WORK 5 3 A@ 7 AT=
   AT-WORK 5 4 A@ 12 AT= ;

: AT-TEST-RUNMAX ( -- )
   66 AT-WORK1
   AT-WORK 0 A-RUNMAX!
   66 AT-WORK1
   AT-WORK 1 A-RUNMAX!
   AT-WORK 1 0 A@ 66 AT=
   2 7 1 9 3 AT-WORK5
   AT-WORK 5 A-RUNMAX!
   AT-WORK 5 0 A@ 2 AT=
   AT-WORK 5 1 A@ 7 AT=
   AT-WORK 5 2 A@ 7 AT=
   AT-WORK 5 3 A@ 9 AT=
   AT-WORK 5 4 A@ 9 AT= ;

: AT-TEST-FILL ( -- )
   123 AT-WORK 0 A-FILL!
   -5 AT-WORK 1 A-FILL!
   AT-WORK 1 0 A@ -5 AT=
   -6 AT-WORK 5 A-FILL!
   AT-WORK 5 0 A@ -6 AT=
   AT-WORK 5 1 A@ -6 AT=
   AT-WORK 5 2 A@ -6 AT=
   AT-WORK 5 3 A@ -6 AT=
   AT-WORK 5 4 A@ -6 AT= ;

: AT-REPORT ( -- )
   AT-FAIL @ 0 = IF s" array-test: ok" type cr exit THEN
   AT-FAIL @ . s" array-test: failures" type cr
   s" array-test: failures" AT-EX-FAIL die ;

: AT-MAIN ( -- )
   AT-TEST-CHECKS
   AT-TEST-ACCESS
   AT-TEST-PROMOTED-HELPERS
   AT-TEST-SCALARS
   AT-TEST-REVERSE
   AT-TEST-PREFIX
   AT-TEST-RUNMAX
   AT-TEST-FILL
   AT-REPORT ;

AT-MAIN
