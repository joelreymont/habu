\ stdlib-date-test.f - focused tests for src/lib/date.f on src/lib/test.f's assert
\ words: the test shares one dictionary with the loaded framework, so a private
\ T= here is a duplicate definition.

require src/lib/errors.f
require src/lib/string.f
require src/lib/date.f
require src/lib/test.f
require src/lib/task.f

32 constant DATE-TEST-BUF-LEN

create DATE-TEST-BUF DATE-TEST-BUF-LEN allot

create DATE-WORK-BUF-A DATE-TEST-BUF-LEN allot
create DATE-WORK-BUF-B DATE-TEST-BUF-LEN allot
TASK:MIN-STACK TASK:TASK DATE-WORKER-A
TASK:MIN-STACK TASK:TASK DATE-WORKER-B
TASK:SEMAPHORE DATE-READY
TASK:SEMAPHORE DATE-START
20000 constant DATE-ROUNDS

: DATE-ROUND ( n n n n ptr u8 n n ptr u8 n ptr u8 -- bool )
   {: y:n m:n d:n days:n datea:ptr dateu:n seconds:n timea:ptr timeu:n buf:ptr :}
   y m d DATE:YMD>DAYS days <> if 0 0= 0= exit then
   days DATE:DAYS>YMD {: goty:n gotm:n gotd:n :}
   goty y <> gotm m <> or gotd d <> or if 0 0= 0= exit then
   datea dateu DATE:PARSE-YMD MATCH option
     none OF 0 0= 0= exit ENDOF
     some OF days <> if 0 0= 0= exit then ENDOF
   ;MATCH
   days buf DATE-TEST-BUF-LEN DATE:FORMAT-YMD datea dateu STR= 0= if 0 0= 0= exit then
   seconds buf DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC timea timeu STR= ;

: DATE-WORK-A ( -- )
   DATE-READY TASK:SIGNAL
   DATE-START TASK:WAIT
   0 0=
   DATE-ROUNDS 0 ?do
      1900 2 28 -25509 s" 1900-02-28" 0 s" 1970-01-01T00:00:00Z" DATE-WORK-BUF-A
      DATE-ROUND and
   loop
   if 0 else 1 then TASK:RETURN ;

: DATE-WORK-B ( -- )
   DATE-READY TASK:SIGNAL
   DATE-START TASK:WAIT
   0 0=
   DATE-ROUNDS 0 ?do
      2024 2 29 19782 s" 2024-02-29" 90061 s" 1970-01-02T01:01:01Z" DATE-WORK-BUF-B
      DATE-ROUND and
   loop
   if 0 else 1 then TASK:RETURN ;

: DATE-JOIN-OK ( result<n,n> -- )
   MATCH result
     ok OF 0 T= ENDOF
     err OF 0 T= ENDOF
   ;MATCH ;

: DATE-CONCURRENT ( -- )
   0 DATE-READY TASK:SEMAPHORE-INIT
   0 DATE-START TASK:SEMAPHORE-INIT
   ['] DATE-WORK-A DATE-WORKER-A TASK:ACTIVATE
   ['] DATE-WORK-B DATE-WORKER-B TASK:ACTIVATE
   DATE-READY TASK:WAIT
   DATE-READY TASK:WAIT
   DATE-START TASK:SIGNAL
   DATE-START TASK:SIGNAL
   DATE-WORKER-A TASK:JOIN DATE-JOIN-OK
   DATE-WORKER-B TASK:JOIN DATE-JOIN-OK
   DATE-START TASK:SEMAPHORE-DESTROY
   DATE-READY TASK:SEMAPHORE-DESTROY ;

: DATE-PARSE= {: a:ptr u:n days:n :} ( ptr u8 n n -- )
   a u DATE:PARSE-YMD MATCH option
     none OF 0 0= 0= TTRUE ENDOF                      \ an unexpected NONE fails
     some OF days T= ENDOF
   ;MATCH ;

: DATE-PARSE-BAD ( ptr u8 n -- )
   DATE:PARSE-YMD MATCH option
     none OF 0 0= TTRUE ENDOF
     some OF drop 0 0= 0= TTRUE ENDOF                 \ an unexpected SOME fails
   ;MATCH ;

: DATE-FORMAT= {: days:n a:ptr u:n :} ( n ptr u8 n -- )
   days DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-YMD
   a u T$= ;

: DATE-TIMESTAMP= {: seconds:n a:ptr u:n :} ( n ptr u8 n -- )
   seconds DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC
   a u T$= ;

: DATE-N-BAD ( -- )
   s" 98x6" drop 0 4 DATE:N MATCH option
     none OF -1 ENDOF
     some OF drop 0 ENDOF
   ;MATCH  -1 T= ;

T-RESET

DATE-N-BAD

\ Ordinary parse fixtures do not exercise Gregorian century exceptions.
2000 DATE:LEAP-YEAR? TTRUE
1900 DATE:LEAP-YEAR? 0= TTRUE
2026 13 1 DATE:VALID-YMD? 0= TTRUE

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

\ FORMAT-YMD writes four year digits, so it formats years 0 through 9999 and
\ refuses any other year rather than writing its low four digits.
2932896 s" 9999-12-31" DATE-FORMAT=
-719468 s" 0000-03-01" DATE-FORMAT=
253402300799 s" 9999-12-31T23:59:59Z" DATE-TIMESTAMP=
: DATE-YMD-AT ( n -- ) DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-YMD 2drop ;
: DATE-YMD-PAST-9999 ( -- ) 2932897 DATE-YMD-AT ;
: DATE-YMD-FAR ( -- ) 3000000 DATE-YMD-AT ;
: DATE-YMD-FAR-BACK ( -- ) -1000000 DATE-YMD-AT ;
' DATE-YMD-PAST-9999 catch E-TIME-RANGE T=
' DATE-YMD-FAR catch E-TIME-RANGE T=
' DATE-YMD-FAR-BACK catch E-TIME-RANGE T=
\ Days before 0000-03-01 fall in the era before year 0; the era step floors, so
\ year 0's January and February round-trip and the day before them is year -1.
s" 0000-01-01" -719528 DATE-PARSE=
s" 0000-02-29" -719469 DATE-PARSE=
-719528 s" 0000-01-01" DATE-FORMAT=
-719469 s" 0000-02-29" DATE-FORMAT=
: DATE-YMD-BEFORE-0 ( -- ) -719529 DATE-YMD-AT ;
: DATE-EPOCH-PAST-9999 ( -- ) 253402300800 DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC 2drop ;
' DATE-YMD-BEFORE-0 catch E-TIME-RANGE T=
' DATE-EPOCH-PAST-9999 catch E-TIME-RANGE T=

: DATE-YMD-SHORT ( -- ) 0 DATE-TEST-BUF DATE:LEN 1- DATE:FORMAT-YMD 2drop ;
: DATE-EPOCH-NEG ( -- ) -1 DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC 2drop ;
: DATE-EPOCH-SHORT ( -- ) 0 DATE-TEST-BUF DATE:TIME-LEN 1- DATE:FORMAT-EPOCH-UTC 2drop ;
' DATE-YMD-SHORT catch E-TIME-CAPACITY T=
' DATE-EPOCH-NEG catch E-TIME-RANGE T=
' DATE-EPOCH-SHORT catch E-TIME-CAPACITY T=

1900 2 28 -25509 s" 1900-02-28" 0 s" 1970-01-01T00:00:00Z" DATE-WORK-BUF-A DATE-ROUND TTRUE
2024 2 29 19782 s" 2024-02-29" 90061 s" 1970-01-02T01:01:01Z" DATE-WORK-BUF-B DATE-ROUND TTRUE
DATE-CONCURRENT

T-REPORT
