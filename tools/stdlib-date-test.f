\ stdlib-date-test.f - focused tests for src/lib/date.f on the words of
\ src/lib/test/assert.f: the test shares one dictionary with the loaded
\ framework, so a private T= here is a duplicate definition.

require src/lib/error.f
require src/lib/string.f
require src/lib/date.f
require src/lib/test/assert.f
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

: DATE-ROUND ( n n n n str n str ^u8 -- bool )
   {: y:n m:n d:n days:n dates:str seconds:n times:str buf:^ :}
   y m d DATE:YMD>DAYS days <> if 0 0= 0= exit then
   days DATE:DAYS>YMD {: goty:n gotm:n gotd:n :}
   goty y <> gotm m <> or gotd d <> or if 0 0= 0= exit then
   dates DATE:PARSE-YMD MATCH option
     none OF 0 0= 0= exit ENDOF
     some OF days <> if 0 0= 0= exit then ENDOF
   ;MATCH
   days buf DATE-TEST-BUF-LEN DATE:FORMAT-YMD dates STR:= 0= if 0 0= 0= exit then
   seconds buf DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC times STR:= ;

: DATE-WORK-A ( -- )
   DATE-READY TASK:SIGNAL-SEM
   DATE-START TASK:WAIT-SEM
   0 0=
   DATE-ROUNDS 0 ?do
      1900 2 28 -25509 s" 1900-02-28" 0 s" 1970-01-01T00:00:00Z" DATE-WORK-BUF-A
      DATE-ROUND and
   loop
   if 0 else 1 then TASK:RETURN ;

: DATE-WORK-B ( -- )
   DATE-READY TASK:SIGNAL-SEM
   DATE-START TASK:WAIT-SEM
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
   DATE-READY TASK:WAIT-SEM
   DATE-READY TASK:WAIT-SEM
   DATE-START TASK:SIGNAL-SEM
   DATE-START TASK:SIGNAL-SEM
   DATE-WORKER-A TASK:JOIN DATE-JOIN-OK
   DATE-WORKER-B TASK:JOIN DATE-JOIN-OK
   DATE-START TASK:SEMAPHORE-DESTROY
   DATE-READY TASK:SEMAPHORE-DESTROY ;

: DATE-PARSE= ( str n -- )
   {: s:str days:n :}
   s DATE:PARSE-YMD MATCH option
     none OF 0 0= 0= TTRUE ENDOF                      \ an unexpected NONE fails
     some OF days T= ENDOF
   ;MATCH ;

: DATE-PARSE-BAD ( str -- )
   DATE:PARSE-YMD MATCH option
     none OF 0 0= TTRUE ENDOF
     some OF drop 0 0= 0= TTRUE ENDOF                 \ an unexpected SOME fails
   ;MATCH ;

: DATE-FORMAT= ( n str -- )
   {: days:n s:str :}
   days DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-YMD
   s T$= ;

: DATE-TIMESTAMP= ( n str -- )
   {: seconds:n s:str :}
   seconds DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC
   s T$= ;

: DATE-N-BAD ( -- )
   s" 98x6" STR:DATA 0 4 DATE:N MATCH option
     none OF -1 ENDOF
     some OF drop 0 ENDOF
   ;MATCH  -1 T= ;

: DATE-YEAR-AT ( n -- n )   \ epoch seconds of June 1st of a year
   6 1 DATE:YMD>DAYS DATE:SECONDS-DAY * ;

: DATE-HTTP-AT= ( str n n -- )
   {: s:str now:n want:n :}
   s now DATE:PARSE-HTTP MATCH option
     none OF false TTRUE ENDOF                        \ an unexpected NONE fails
     some OF want T= ENDOF
   ;MATCH ;

: DATE-HTTP= ( str n -- ) 2026 DATE-YEAR-AT swap DATE-HTTP-AT= ;

: DATE-HTTP-BAD-AT ( str n -- )
   DATE:PARSE-HTTP MATCH option
     none OF true TTRUE ENDOF
     some OF drop false TTRUE ENDOF                   \ an unexpected SOME fails
   ;MATCH ;

: DATE-HTTP-BAD ( str -- ) 2026 DATE-YEAR-AT DATE-HTTP-BAD-AT ;

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
: DATE-YMD-AT ( n -- ) DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-YMD drop ;
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
: DATE-EPOCH-PAST-9999 ( -- ) 253402300800 DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC drop ;
' DATE-YMD-BEFORE-0 catch E-TIME-RANGE T=
' DATE-EPOCH-PAST-9999 catch E-TIME-RANGE T=

: DATE-YMD-SHORT ( -- ) 0 DATE-TEST-BUF DATE:LEN 1- DATE:FORMAT-YMD drop ;
: DATE-EPOCH-NEG ( -- ) -1 DATE-TEST-BUF DATE-TEST-BUF-LEN DATE:FORMAT-EPOCH-UTC drop ;
: DATE-EPOCH-SHORT ( -- ) 0 DATE-TEST-BUF DATE:TIME-LEN 1- DATE:FORMAT-EPOCH-UTC drop ;
' DATE-YMD-SHORT catch E-TIME-CAPACITY T=
' DATE-EPOCH-NEG catch E-TIME-RANGE T=
' DATE-EPOCH-SHORT catch E-TIME-CAPACITY T=

1900 2 28 -25509 s" 1900-02-28" 0 s" 1970-01-01T00:00:00Z" DATE-WORK-BUF-A DATE-ROUND TTRUE
2024 2 29 19782 s" 2024-02-29" 90061 s" 1970-01-02T01:01:01Z" DATE-WORK-BUF-B DATE-ROUND TTRUE
DATE-CONCURRENT

\ The three forms RFC 9110 section 5.6.7 gives, each 1994-11-06T08:49:37Z.
s" Sun, 06 Nov 1994 08:49:37 GMT" 784111777 DATE-HTTP=
s" Sunday, 06-Nov-94 08:49:37 GMT" 784111777 DATE-HTTP=
s" Sun Nov  6 08:49:37 1994" 784111777 DATE-HTTP=
\ A two-digit asctime day, a day name the date lacks, a leap second
\ and the range's first and last seconds.
s" Sun Nov 06 08:49:37 1994" 784111777 DATE-HTTP=
s" Wednesday, 06-Nov-94 08:49:37 GMT" 784111777 DATE-HTTP=
s" Sun, 06 Nov 1994 08:49:60 GMT" 784111799 DATE-HTTP=
s" Fri, 31 Dec 9999 23:59:59 GMT" 253402300799 DATE-HTTP=
s" Thu, 01 Jan 1970 00:00:00 GMT" 0 DATE-HTTP=
s" Fri Dec 31 23:59:59 9999" 253402300799 DATE-HTTP=
\ RFC 850's year is the latest that puts the timestamp at most 50 years ahead.
s" Thursday, 01-Jan-70 00:00:00 GMT" 2019 DATE-YEAR-AT 0 DATE-HTTP-AT=
s" Tuesday, 01-Jan-69 00:00:00 GMT" 2019 DATE-YEAR-AT 3124224000 DATE-HTTP-AT=
s" Tuesday, 01-Jan-69 00:00:00 GMT" 3124224000 DATE-HTTP=
s" Wednesday, 01-Jan-76 00:00:00 GMT" 3345062400 DATE-HTTP=
s" Saturday, 01-Jan-77 00:00:00 GMT" 220924800 DATE-HTTP=
s" Monday, 01-Jan-20 00:00:00 GMT" 2080 DATE-YEAR-AT 4733510400 DATE-HTTP-AT=
s" Monday, 01-Jun-76 00:00:00 GMT" 3358195200 DATE-HTTP=
s" Tuesday, 01-Jun-76 00:00:01 GMT" 202435201 DATE-HTTP=
s" Sunday, 01-Jan-61 00:00:00 GMT" 2010 DATE-YEAR-AT DATE-HTTP-BAD-AT
s" Saturday, 01-Jan-00 00:00:00 GMT" 10000 DATE-YEAR-AT DATE-HTTP-BAD-AT
\ A now before 1970 that is not a whole day: 1969-12-31T12:00:00Z.
s" Wednesday, 31-Dec-19 13:00:00 GMT" -43200 DATE-HTTP-BAD-AT
s" Tuesday, 31-Dec-19 11:00:00 GMT" -43200 1577790000 DATE-HTTP-AT=
\ 2100 has no 29 February, so 00 is 2000.
s" Tuesday, 29-Feb-00 00:00:00 GMT" 2050 DATE-YEAR-AT 951782400 DATE-HTTP-AT=

s" " DATE-HTTP-BAD
s" 120" DATE-HTTP-BAD
s" 1994-11-06T08:49:37Z" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:37 GMT " DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:37 GM" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:37" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:37 UTC" DATE-HTTP-BAD
s" Sunday, 06-Nov-94 08:49:37" DATE-HTTP-BAD
s" Sun Nov  6 08:49:37 1994 " DATE-HTTP-BAD
\ A day name must suit the form.
s" Sux, 06 Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06-Nov-94 08:49:37 GMT" DATE-HTTP-BAD
s" Sundax, 06-Nov-94 08:49:37 GMT" DATE-HTTP-BAD
s" , 06-Nov-94 08:49:37 GMT" DATE-HTTP-BAD
s" Snu Nov  6 08:49:37 1994" DATE-HTTP-BAD
s" sun, 06 Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
\ Dates that do not exist, clocks out of range and years before 1970.
s" Thu, 31 Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 00 Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sunday, 29-Feb-25 08:49:37 GMT" DATE-HTTP-BAD
s" Sun Nov  0 08:49:37 1994" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 24:00:00 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:60:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:61 GMT" DATE-HTTP-BAD
s" Wed, 31 Dec 1969 23:59:59 GMT" DATE-HTTP-BAD
s" Wed Dec 31 23:59:59 1969" DATE-HTTP-BAD
\ Month names are case-sensitive.
s" Sun, 06 nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sunday, 06-NOV-94 08:49:37 GMT" DATE-HTTP-BAD
s" Sun nov  6 08:49:37 1994" DATE-HTTP-BAD
\ A stray character in each field or separator.
s" Sun, 0x Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nox 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 19x4 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 0x:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:4x:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08:49:3x GMT" DATE-HTTP-BAD
s" Sun; 06 Nov 1994 08:49:37 GMT" DATE-HTTP-BAD
s" Sun, 06 Nov 1994 08-49:37 GMT" DATE-HTTP-BAD
s" Sunday, 0x-Nov-94 08:49:37 GMT" DATE-HTTP-BAD
s" Sunday, 06-Nov-9x 08:49:37 GMT" DATE-HTTP-BAD
s" Sunday, 06 Nov-94 08:49:37 GMT" DATE-HTTP-BAD
s" Sunday, 06-Nov-94 08:49:3x GMT" DATE-HTTP-BAD
s" Sun Nov  x 08:49:37 1994" DATE-HTTP-BAD
s" Sun Nov x6 08:49:37 1994" DATE-HTTP-BAD
s" Sun Nov  6 08:49:37 199x" DATE-HTTP-BAD
s" Sun Nov  6 0x:49:37 1994" DATE-HTTP-BAD
s" Sun Nov- 6 08:49:37 1994" DATE-HTTP-BAD

T-REPORT
