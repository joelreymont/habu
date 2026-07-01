\ stdlib-date-test.f - focused tests for lib/date.f.

require lib/errors.f
require lib/date.f

variable #DATE-FAIL
variable #DATE-CASE
32 constant DATE-TEST-BUF-LEN
1 constant DATE-TEST-EX-FAIL

create DATE-TEST-BUF DATE-TEST-BUF-LEN allot

: T= {: got want :} ( n n -- )
   #DATE-CASE @ 1+ #DATE-CASE !
   got want <> IF
      [char] F emit #DATE-CASE @ .
      #DATE-FAIL @ 1+ #DATE-FAIL !
   THEN ;

: TTRUE ( bool -- )
   #DATE-CASE @ 1+ #DATE-CASE !
   0= IF
      [char] F emit #DATE-CASE @ .
      #DATE-FAIL @ 1+ #DATE-FAIL !
   THEN ;

: DATE-TEST-STR= {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: T$= ( ptr u8 n ptr u8 n -- )
   DATE-TEST-STR= TTRUE ;

: DATE-PARSE= {: a:ptr u days :} ( ptr u8 n n -- )
   a u PARSE-YMD TTRUE
   days T= ;

: DATE-PARSE-BAD ( ptr u8 n -- )
   PARSE-YMD 0= TTRUE
   drop ;

: DATE-FORMAT= {: days a:ptr u :} ( n ptr u8 n -- )
   days DATE-TEST-BUF DATE-TEST-BUF-LEN FORMAT-YMD
   a u T$= ;

: DATE-TIMESTAMP= {: seconds a:ptr u :} ( n ptr u8 n -- )
   seconds DATE-TEST-BUF DATE-TEST-BUF-LEN FORMAT-EPOCH-UTC
   a u T$= ;

: DATE-ROUNDTRIP {: y m d :} ( n n n -- )
   y m d YMD>DAYS DAYS>YMD
   d T=
   m T=
   y T= ;

: DATE-N-OK ( -- )
   s" 9876" drop 0 4 DATE-N TTRUE
   9876 T= ;

: DATE-N-BAD ( -- )
   s" 98x6" drop 0 4 DATE-N 0= TTRUE
   drop ;

$30 DATE-DIGIT? TTRUE
$39 DATE-DIGIT? TTRUE
$2F DATE-DIGIT? 0= TTRUE
$3A DATE-DIGIT? 0= TTRUE
DATE-N-OK
DATE-N-BAD

0 DAYS>YMD 1 T= 1 T= 1970 T=
1970 1 1 YMD>DAYS 0 T=
2000 2 29 YMD>DAYS 11016 T=
2026 6 16 YMD>DAYS 20620 T=
1900 2 28 YMD>DAYS -25509 T=

2000 LEAP-YEAR? TTRUE
1900 LEAP-YEAR? 0= TTRUE
2024 2 MONTH-DAYS 29 T=
2026 2 MONTH-DAYS 28 T=
2026 13 1 VALID-YMD? 0= TTRUE
2026 2 29 VALID-YMD? 0= TTRUE
2024 2 29 VALID-YMD? TTRUE

s" 1970-01-01" 0 DATE-PARSE=
s" 2026-06-16" 20620 DATE-PARSE=
s" 2024-02-29" 19782 DATE-PARSE=
s" 2026-6-16" DATE-PARSE-BAD
s" 2026-02-29" DATE-PARSE-BAD
s" 2026-12-32" DATE-PARSE-BAD
s" 2026/06/16" DATE-PARSE-BAD

0 s" 1970-01-01" DATE-FORMAT=
20620 s" 2026-06-16" DATE-FORMAT=
-25509 s" 1900-02-28" DATE-FORMAT=

0 s" 1970-01-01T00:00:00Z" DATE-TIMESTAMP=
90061 s" 1970-01-02T01:01:01Z" DATE-TIMESTAMP=

1970 1 1 DATE-ROUNDTRIP
2000 2 29 DATE-ROUNDTRIP
1900 2 28 DATE-ROUNDTRIP
2026 6 16 DATE-ROUNDTRIP

0 DATE-TEST-BUF DATE-LEN 1- ' FORMAT-YMD catch E-TIME-CAPACITY T=
drop drop drop
-1 DATE-TEST-BUF DATE-TEST-BUF-LEN ' FORMAT-EPOCH-UTC catch E-TIME-RANGE T=
drop drop drop
0 DATE-TEST-BUF DATE-TIME-LEN 1- ' FORMAT-EPOCH-UTC catch E-TIME-CAPACITY T=
drop drop drop

: DATE-TEST-REPORT ( -- )
   #DATE-FAIL @ 0= IF s" stdlib-date-test: ok" type cr exit THEN
   #DATE-FAIL @ . s" stdlib-date-test: failures" type cr
   s" stdlib-date-test: failures" DATE-TEST-EX-FAIL die ;

DATE-TEST-REPORT
