\ date-test.f - focused tests for tools/date.f.
\ Run: cat tools/date.f tools/date-test.f | bin/hb

variable DATE-TEST-N
variable DATE-TEST-FAIL
32 constant DATE-TEST-BUF-LEN
1 constant DATE-TEST-EX-FAIL

create DATE-TEST-BUF DATE-TEST-BUF-LEN allot

: DATE-ASSERT ( bool -- )
   DATE-TEST-N @ 1 + DATE-TEST-N !
   0= IF
      s" date-test: assertion " type DATE-TEST-N @ . s"  failed" type cr
      DATE-TEST-FAIL @ 1 + DATE-TEST-FAIL !
   THEN ;

: DATE-ASSERT= ( n n -- )
   = DATE-ASSERT ;

: DATE-STR= {: a:ptr u b:ptr v :} ( ptr u8 n ptr u8 n -- bool )
   u v <> IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a + c@ over b + c@ <> IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: DATE-ASSERT$ ( ptr u8 n ptr u8 n -- )
   DATE-STR= DATE-ASSERT ;

: DATE-PARSE= {: a:ptr u days :} ( ptr u8 n n -- )
   a u PARSE-YMD DATE-ASSERT
   days DATE-ASSERT= ;

: DATE-PARSE-BAD ( ptr u8 n -- )
   PARSE-YMD 0= DATE-ASSERT
   drop ;

: DATE-FORMAT= {: days a:ptr u :} ( n ptr u8 n -- )
   days DATE-TEST-BUF DATE-TEST-BUF-LEN FORMAT-YMD
   a u DATE-ASSERT$ ;

: DATE-TIMESTAMP= {: seconds a:ptr u :} ( n ptr u8 n -- )
   seconds DATE-TEST-BUF DATE-TEST-BUF-LEN FORMAT-EPOCH-UTC
   a u DATE-ASSERT$ ;

: DATE-ROUNDTRIP {: y m d :} ( n n n -- )
   y m d YMD>DAYS DAYS>YMD
   d DATE-ASSERT=
   m DATE-ASSERT=
   y DATE-ASSERT= ;

0 DAYS>YMD 1 DATE-ASSERT= 1 DATE-ASSERT= 1970 DATE-ASSERT=
1970 1 1 YMD>DAYS 0 DATE-ASSERT=
2000 2 29 YMD>DAYS 11016 DATE-ASSERT=
2026 6 16 YMD>DAYS 20620 DATE-ASSERT=
1900 2 28 YMD>DAYS -25509 DATE-ASSERT=

2000 LEAP-YEAR? DATE-ASSERT
1900 LEAP-YEAR? 0= DATE-ASSERT
2024 2 MONTH-DAYS 29 DATE-ASSERT=
2026 2 MONTH-DAYS 28 DATE-ASSERT=
2026 13 1 VALID-YMD? 0= DATE-ASSERT
2026 2 29 VALID-YMD? 0= DATE-ASSERT
2024 2 29 VALID-YMD? DATE-ASSERT

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

: DATE-REPORT ( -- )
   DATE-TEST-FAIL @ 0= IF s" date-test: ok" type cr exit THEN
   DATE-TEST-FAIL @ . s" date-test: failures" type cr
   s" date-test: failures" DATE-TEST-EX-FAIL die ;

DATE-REPORT
