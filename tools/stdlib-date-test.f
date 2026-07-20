\ stdlib-date-test.f - focused tests for lib/date.f.
\
\ Uses the shared lib/test.f assert vocabulary (T= / TTRUE / T$= / T-RESET /
\ T-REPORT), like the sibling lib/property-test.f. A test file loaded into the
\ resident in-process gate image (GSI-INCLUDE, stdlib/tail-fast) shares one
\ dictionary with the already-loaded test framework, so defining a private T= /
\ TTRUE / T$= here collides (duplicate definition: T=). Reusing the framework
\ words composes cleanly standalone, spawned, and in-process.

require lib/errors.f
require lib/date.f
require lib/test.f

32 constant DATE-TEST-BUF-LEN

create DATE-TEST-BUF DATE-TEST-BUF-LEN allot

: DATE-PARSE= {: a:ptr u:n days:n :} ( ptr u8 n n -- )   \ valid date -> SOME days
   a u DATE:PARSE-YMD MATCH option
     none OF 0 0= 0= TTRUE ENDOF                      \ NONE = unexpected parse failure -> false
     some OF days T= ENDOF                            \ SOME day -> compare to expected
   ;MATCH ;

: DATE-PARSE-BAD ( ptr u8 n -- )                     \ invalid date -> NONE
   DATE:PARSE-YMD MATCH option
     none OF 0 0= TTRUE ENDOF                         \ NONE = correctly rejected -> true
     some OF drop 0 0= 0= TTRUE ENDOF                 \ SOME = unexpected parse success -> false
   ;MATCH ;

: DATE-FORMAT= {: days:n a:ptr u:n :} ( n ptr u8 n -- )
   days DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-YMD
   a u T$= ;

: DATE-TIMESTAMP= {: seconds:n a:ptr u:n :} ( n ptr u8 n -- )
   seconds DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC
   a u T$= ;

: DATE-ROUNDTRIP {: y:n m:n d:n :} ( n n n -- )
   y m d DATE:YMD>DAYS DATE:DAYS>YMD
   d T=
   m T=
   y T= ;

: DATE-N-OK ( -- )                                  \ all digits -> SOME parsed value
   s" 9876" drop 0 4 DATE:N MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH  9876 T= ;

: DATE-N-BAD ( -- )                                 \ a non-digit -> NONE
   s" 98x6" drop 0 4 DATE:N MATCH option
     none OF -1 ENDOF
     some OF drop 0 ENDOF
   ;MATCH  -1 T= ;

: PARSE-YMD-OK ( -- )                               \ valid date -> SOME epoch day
   s" 2026-06-16" DATE:PARSE-YMD MATCH option
     none OF -1 ENDOF
     some OF ENDOF
   ;MATCH  20620 T= ;

: PARSE-YMD-BAD ( -- )                              \ invalid date -> NONE
   s" 2026/06/16" DATE:PARSE-YMD MATCH option
     none OF -1 ENDOF
     some OF drop 0 ENDOF
   ;MATCH  -1 T= ;

T-RESET

$30 DATE:DIGIT? TTRUE
$39 DATE:DIGIT? TTRUE
$2F DATE:DIGIT? 0= TTRUE
$3A DATE:DIGIT? 0= TTRUE
DATE-N-OK
DATE-N-BAD
PARSE-YMD-OK
PARSE-YMD-BAD

0 DATE:DAYS>YMD 1 T= 1 T= 1970 T=
1970 1 1 DATE:YMD>DAYS 0 T=
2000 2 29 DATE:YMD>DAYS 11016 T=
2026 6 16 DATE:YMD>DAYS 20620 T=
1900 2 28 DATE:YMD>DAYS -25509 T=

2000 DATE:LEAP-YEAR? TTRUE
1900 DATE:LEAP-YEAR? 0= TTRUE
2024 2 DATE:MONTH-DAYS 29 T=
2026 2 DATE:MONTH-DAYS 28 T=
2026 13 1 DATE:VALID-YMD? 0= TTRUE
2026 2 29 DATE:VALID-YMD? 0= TTRUE
2024 2 29 DATE:VALID-YMD? TTRUE

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

0 DATE-TEST-BUF DATE:LEN 1- ' DATE:FORMAT-YMD catch E-TIME-CAPACITY T=
drop drop drop
-1 DATE-TEST-BUF DATE-TEST-BUF-LEN ' DATE:FORMAT-EPOCH-UTC catch E-TIME-RANGE T=
drop drop drop
0 DATE-TEST-BUF DATE:TIME-LEN 1- ' DATE:FORMAT-EPOCH-UTC catch E-TIME-CAPACITY T=
drop drop drop

T-REPORT
