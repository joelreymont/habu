\ stdlib-time-test.f - focused tests for lib/time.f.
\ Run: cat lib/errors.f lib/time.f tools/stdlib-time-test.f | bin/hb

variable #TIME-FAIL
variable #TIME-CASE
1 constant TIME-TEST-EX-FAIL

: T= {: got want :} ( n n -- )
   #TIME-CASE @ 1+ #TIME-CASE !
   got want <> IF
      [char] F emit #TIME-CASE @ .
      #TIME-FAIL @ 1+ #TIME-FAIL !
   THEN ;

: TTRUE ( bool -- )
   #TIME-CASE @ 1+ #TIME-CASE !
   0= IF
      [char] F emit #TIME-CASE @ .
      #TIME-FAIL @ 1+ #TIME-FAIL !
   THEN ;

: TIME-TEST-EPOCH ( -- bool )
   TIME-EPOCH-SECONDS 1600000000 > ;

: TIME-TEST-MONO-ORDER ( -- bool )
   TIME-MONO-NS TIME-MONO-NS <= ;

: TIME-TEST-MONO-ELAPSED ( -- n )
   TIME-MONO-NS 0 100000 0 do i + loop drop TIME-MONO-NS swap - ;

TIME-EPOCH-SECONDS 1600000000 > TTRUE
TIME-TEST-EPOCH TTRUE
TIME-TEST-MONO-ORDER TTRUE
TIME-TEST-MONO-ELAPSED 0 > TTRUE

: TIME-TEST-REPORT ( -- )
   #TIME-FAIL @ 0= IF s" stdlib-time-test: ok" type cr exit THEN
   #TIME-FAIL @ . s" stdlib-time-test: failures" type cr
   s" stdlib-time-test: failures" TIME-TEST-EX-FAIL die ;

TIME-TEST-REPORT
