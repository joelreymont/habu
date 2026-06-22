\ habu-array-lib-test.f - focused tests for checked benchmark array helpers.
\ Run: bin/hb --load lib/errors.f lib/array.f bench/llm/habu-array-lib.f bench/llm/habu-array-lib-test.f

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

HATL-A 3 >LEN 0 >IDX A@ 1 HATL-ASSERT=
HATL-A 3 >LEN 2 >IDX A@ 3 HATL-ASSERT=
9 HATL-A 3 >LEN 1 >IDX A!
HATL-A 3 >LEN 1 >IDX A@ 9 HATL-ASSERT=
5 HATL-A 3 >LEN 1 >IDX A+!
HATL-A 3 >LEN 1 >IDX A@ 14 HATL-ASSERT=
HATL-A 3 >LEN 0 >IDX 2 >IDX A-SWAP
HATL-A 3 >LEN 0 >IDX A@ 3 HATL-ASSERT=
HATL-A 3 >LEN 2 >IDX A@ 1 HATL-ASSERT=
5 >LEN LAST-INDEX IDX>N 4 HATL-ASSERT=
5 >LEN 1 >IDX MIRROR-INDEX IDX>N 3 HATL-ASSERT=
4 EVEN? HATL-ASSERT
5 EVEN? 0= HATL-ASSERT

: HATL-REPORT ( -- )
   HATL-FAIL @ 0= IF s" habu-array-lib-test: ok" type cr exit THEN
   HATL-FAIL @ . s" habu-array-lib-test: failures" type cr
   s" habu-array-lib-test: failures" HATL-EX-FAIL die ;

HATL-REPORT
