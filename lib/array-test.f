\ array-test.f - focused tests for checked stdlib array helpers.
\ Run: cat lib/errors.f lib/array.f lib/array-test.f | bin/hb

1 constant AT-EX-FAIL

variable AT-CASE
variable AT-FAIL
variable AT-START-DEPTH
variable AT-ACTUAL-DEPTH

create AT-EMPTY
create AT-ONE 42 ,
create AT-NORMAL 3 , -1 , 4 , 4 , 2 ,
create AT-WORK 5 cells allot
create AT-ACTUAL 32 cells allot

: AT= {: got want :} ( n n -- )
   AT-CASE @ 1 + AT-CASE !
   got want <> if
      [char] F emit AT-CASE @ .
      AT-FAIL @ 1 + AT-FAIL !
   then ;

\ T{ -> }T intentionally models an arbitrary stack tail, which is a
\ metaprogramming test boundary; reusable array helpers below are checked.
0 set-check
: T{ ( -- )
   depth AT-START-DEPTH ! ;

: -> ( R -- )
   depth AT-START-DEPTH @ - dup AT-ACTUAL-DEPTH !
   0 ?do
      AT-ACTUAL i cells + !
   loop ;

: }T ( R -- )
   depth AT-START-DEPTH @ - dup AT-ACTUAL-DEPTH @ AT=
   0 ?do
      AT-ACTUAL i cells + @ AT=
   loop ;
' HB-CHECK-HOOK set-check

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

: AT-WORK5@ ( -- n n n n n )
   AT-WORK 5 0 A@
   AT-WORK 5 1 A@
   AT-WORK 5 2 A@
   AT-WORK 5 3 A@
   AT-WORK 5 4 A@ ;

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

: AT-AMAP-NEG ( -- )
   AT-WORK -1 [: 1+ ;] A-MAP! ;

: AT-AMAPI-NEG ( -- )
   AT-WORK -1 [: + ;] A-MAPI! ;

: AT-AFOLD-NEG ( -- )
   AT-WORK -1 0 [: + ;] A-FOLD drop ;

: AT-AFOLDI-NEG ( -- )
   AT-WORK -1 0 [: + + ;] A-FOLDI drop ;

: AT-ASCAN-NEG ( -- )
   AT-WORK -1 0 [: + ;] A-SCAN! ;

: AT-ASCAN1-NEG ( -- )
   AT-WORK -1 [: + ;] A-SCAN1! ;

: AT-AREVERSE-RANGE-OVERRUN ( -- )
   AT-WORK 5 3 3 A-REVERSE-RANGE! ;

: AT-AFIND-NEG ( -- )
   AT-WORK -1 [: 0= ;] A-FIND-INDEX drop ;

: AT-AFINDI-NEG ( -- )
   AT-WORK -1 [: drop 0= ;] A-FIND-INDEXI drop ;

: AT-AMAX-INDEX-EMPTY ( -- )
   AT-EMPTY 0 A-MAX-INDEX drop ;

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

: AT-TEST-MAP-COMBINATORS ( -- )
   33 AT-WORK1
   T{ AT-WORK 0 [: 1+ ;] A-MAP! -> }T
   T{ AT-WORK 1 0 A@ -> 33 }T
   7 AT-WORK1
   T{ AT-WORK 1 [: 2 * ;] A-MAP! -> }T
   T{ AT-WORK 1 0 A@ -> 14 }T
   1 -2 3 0 5 AT-WORK5
   T{ AT-WORK 5 [: dup * ;] A-MAP! -> }T
   T{ AT-WORK5@ -> 1 4 9 0 25 }T
   ['] AT-AMAP-NEG catch E-A-BOUNDS AT=
   44 AT-WORK1
   T{ AT-WORK 0 [: + ;] A-MAPI! -> }T
   T{ AT-WORK 1 0 A@ -> 44 }T
   7 AT-WORK1
   T{ AT-WORK 1 [: + ;] A-MAPI! -> }T
   T{ AT-WORK 1 0 A@ -> 7 }T
   10 10 10 10 10 AT-WORK5
   T{ AT-WORK 5 [: + ;] A-MAPI! -> }T
   T{ AT-WORK5@ -> 10 11 12 13 14 }T
   ['] AT-AMAPI-NEG catch E-A-BOUNDS AT= ;

: AT-TEST-FOLD-COMBINATORS ( -- )
   T{ AT-EMPTY 0 100 [: + ;] A-FOLD -> 100 }T
   T{ AT-ONE 1 0 [: + ;] A-FOLD -> 42 }T
   T{ AT-NORMAL 5 0 [: + ;] A-FOLD -> 12 }T
   ['] AT-AFOLD-NEG catch E-A-BOUNDS AT=
   T{ AT-EMPTY 0 100 [: + + ;] A-FOLDI -> 100 }T
   T{ AT-ONE 1 0 [: + + ;] A-FOLDI -> 42 }T
   T{ AT-NORMAL 5 0 [: * + ;] A-FOLDI -> 27 }T
   ['] AT-AFOLDI-NEG catch E-A-BOUNDS AT= ;

: AT-TEST-SCAN-COMBINATORS ( -- )
   90 AT-WORK1
   T{ AT-WORK 0 0 [: + ;] A-SCAN! -> }T
   T{ AT-WORK 1 0 A@ -> 90 }T
   4 AT-WORK1
   T{ AT-WORK 1 10 [: + ;] A-SCAN! -> }T
   T{ AT-WORK 1 0 A@ -> 14 }T
   3 1 4 1 5 AT-WORK5
   T{ AT-WORK 5 0 [: + ;] A-SCAN! -> }T
   T{ AT-WORK5@ -> 3 4 8 9 14 }T
   ['] AT-ASCAN-NEG catch E-A-BOUNDS AT=
   91 AT-WORK1
   T{ AT-WORK 0 [: + ;] A-SCAN1! -> }T
   T{ AT-WORK 1 0 A@ -> 91 }T
   4 AT-WORK1
   T{ AT-WORK 1 [: + ;] A-SCAN1! -> }T
   T{ AT-WORK 1 0 A@ -> 4 }T
   3 1 4 1 5 AT-WORK5
   T{ AT-WORK 5 [: + ;] A-SCAN1! -> }T
   T{ AT-WORK5@ -> 3 4 8 9 14 }T
   ['] AT-ASCAN1-NEG catch E-A-BOUNDS AT= ;

: AT-TEST-INDEX-COMBINATORS ( -- )
   1 2 3 4 5 AT-WORK5
   T{ AT-WORK 5 0 0 A-REVERSE-RANGE! -> }T
   T{ AT-WORK 5 0 A@ AT-WORK 5 4 A@ -> 1 5 }T
   T{ AT-WORK 5 2 1 A-REVERSE-RANGE! -> }T
   T{ AT-WORK5@ -> 1 2 3 4 5 }T
   T{ AT-WORK 5 1 3 A-REVERSE-RANGE! -> }T
   T{ AT-WORK5@ -> 1 4 3 2 5 }T
   ['] AT-AREVERSE-RANGE-OVERRUN catch E-A-BOUNDS AT=
   T{ AT-EMPTY 0 [: 4 = ;] A-FIND-INDEX -> -1 }T
   T{ AT-ONE 1 [: 42 = ;] A-FIND-INDEX -> 0 }T
   T{ AT-NORMAL 5 [: 4 = ;] A-FIND-INDEX -> 2 }T
   T{ AT-NORMAL 5 [: 9 = ;] A-FIND-INDEX -> -1 }T
   ['] AT-AFIND-NEG catch E-A-BOUNDS AT=
   T{ AT-EMPTY 0 [: drop 0 = ;] A-FIND-INDEXI -> -1 }T
   T{ AT-ONE 1 [: drop 0 = ;] A-FIND-INDEXI -> 0 }T
   T{ AT-NORMAL 5 [: drop 3 = ;] A-FIND-INDEXI -> 3 }T
   ['] AT-AFINDI-NEG catch E-A-BOUNDS AT=
   ['] AT-AMAX-INDEX-EMPTY catch E-A-EMPTY AT=
   T{ AT-ONE 1 A-MAX-INDEX -> 0 }T
   T{ AT-NORMAL 5 A-MAX-INDEX -> 2 }T
   1 5 5 2 AT-WORK4
   T{ AT-WORK 4 A-MAX-INDEX -> 1 }T ;

: AT-REPORT ( -- )
   AT-FAIL @ 0 = if s" array-test: ok" type cr exit then
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
   AT-TEST-MAP-COMBINATORS
   AT-TEST-FOLD-COMBINATORS
   AT-TEST-SCAN-COMBINATORS
   AT-TEST-INDEX-COMBINATORS
   AT-REPORT ;

AT-MAIN
