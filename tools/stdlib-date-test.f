\ stdlib-date-test.f - focused tests for lib/date.f.
\
\ Uses the shared lib/test.f assert vocabulary (T= / TTRUE / T$= / T-RESET /
\ T-REPORT), like the sibling lib/property-test.f. The test shares one dictionary
\ with the already-loaded test framework, so defining a private T= /
\ TTRUE / T$= here collides (duplicate definition: T=). Reusing the framework
\ words composes cleanly standalone, spawned, and in-process.

require lib/errors.f
require lib/date.f
require lib/test.f
require lib/task.f

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

: DATE-N-BAD ( -- )                                 \ a non-digit -> NONE
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

0 DATE-TEST-BUF DATE:LEN 1- ' DATE:FORMAT-YMD catch E-TIME-CAPACITY T=
drop drop drop
-1 DATE-TEST-BUF DATE-TEST-BUF-LEN ' DATE:FORMAT-EPOCH-UTC catch E-TIME-RANGE T=
drop drop drop
0 DATE-TEST-BUF DATE:TIME-LEN 1- ' DATE:FORMAT-EPOCH-UTC catch E-TIME-CAPACITY T=
drop drop drop

1900 2 28 -25509 s" 1900-02-28" 0 s" 1970-01-01T00:00:00Z" DATE-WORK-BUF-A DATE-ROUND TTRUE
2024 2 29 19782 s" 2024-02-29" 90061 s" 1970-01-02T01:01:01Z" DATE-WORK-BUF-B DATE-ROUND TTRUE
DATE-CONCURRENT

T-REPORT
