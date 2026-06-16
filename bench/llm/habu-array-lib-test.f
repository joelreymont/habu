\ habu-array-lib-test.f - focused tests for checked benchmark array helpers.
\ Run: cat bench/llm/habu-array-lib.f bench/llm/habu-array-lib-test.f | bin/hb

1 constant HATL-EX-FAIL
variable HATL-N
variable HATL-FAIL

: HATL-ASSERT ( bool -- )
   HATL-N @ 1+ HATL-N !
   0= IF
      s" habu-array-lib-test: assertion " type HATL-N @ . s"  failed" type cr
      HATL-FAIL @ 1+ HATL-FAIL !
   THEN ;

: HATL-ASSERT= ( n n -- )
   = HATL-ASSERT ;

here 1 , 2 , 3 , constant HATL-A

HATL-A 0 A@ 1 HATL-ASSERT=
HATL-A 2 A@ 3 HATL-ASSERT=
9 HATL-A 1 A!
HATL-A 1 A@ 9 HATL-ASSERT=
5 HATL-A 1 A+!
HATL-A 1 A@ 14 HATL-ASSERT=
HATL-A 0 2 A-SWAP
HATL-A 0 A@ 3 HATL-ASSERT=
HATL-A 2 A@ 1 HATL-ASSERT=
5 LAST-INDEX 4 HATL-ASSERT=
5 1 MIRROR-INDEX 3 HATL-ASSERT=
4 EVEN? HATL-ASSERT
5 EVEN? 0= HATL-ASSERT

: HATL-REPORT ( -- )
   HATL-FAIL @ 0= IF s" habu-array-lib-test: ok" type cr exit THEN
   HATL-FAIL @ . s" habu-array-lib-test: failures" type cr
   s" habu-array-lib-test: failures" HATL-EX-FAIL die ;

HATL-REPORT
