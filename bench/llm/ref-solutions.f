\ ref-solutions.f — certified habu answer key for the array-algorithm benchmark.
\ Proves every array task is FEASIBLE in habu and that the io-vector
\ ground truth in the harness=array rows of tasks.tsv is correct. Verify two ways:
\   tools/check.sh bench/llm/ref-solutions.f   -> rc 0 (all defs certify)
\   bin/hb < bench/llm/ref-solutions.f         -> prints REF-OK (all io-vectors pass)
variable BI  variable BV

: ARR-SUM    ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ + loop ;
: ARR-MAX    ( ptr a n -- i64 ) {: arr:ptr len :} arr @ len 1 ?do i cells arr + @ max loop ;
: ARR-MIN    ( ptr a n -- i64 ) {: arr:ptr len :} arr @ len 1 ?do i cells arr + @ min loop ;
: ARGMAX     ( ptr a n -- i64 ) {: arr:ptr len :} arr @ BV ! 0 BI ! len 1 ?do i cells arr + @ BV @ > if i cells arr + @ BV ! i BI ! then loop BI @ ;
: COUNT-EVEN ( ptr a n -- i64 ) {: arr:ptr len :} 0 len 0 ?do i cells arr + @ 2 mod 0= if 1+ then loop ;
: REVERSE    ( ptr a n -- ) {: arr:ptr len :} len 2 / 0 ?do i cells arr + @ len 1 - i - cells arr + @ i cells arr + ! len 1 - i - cells arr + ! loop ;
: PREFIXSUM  ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ + i cells arr + ! loop ;
: SQ-EACH    ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ dup * i cells arr + ! loop ;
: NEGATE-EACH ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ negate i cells arr + ! loop ;
: RUNMAX     ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ max i cells arr + ! loop ;
: FIND-FIRST-NEG ( ptr a n -- i64 ) {: arr:ptr len :} -1 BI ! len 0 ?do BI @ -1 = if i cells arr + @ 0 < if i BI ! then then loop BI @ ;
: ABS-EACH ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ abs i cells arr + ! loop ;
: ADD-INDEX ( ptr a n -- ) {: arr:ptr len :} len 0 ?do i cells arr + @ i + i cells arr + ! loop ;
: PREFIX-PROD ( ptr a n -- ) {: arr:ptr len :} len 1 ?do i 1 - cells arr + @ i cells arr + @ * i cells arr + ! loop ;
: REVERSE-INNER ( ptr a n -- ) {: arr:ptr len :} len 2 <= if exit then len 2 - 2 / 0 ?do 1 i + cells arr + @ len 2 - i - cells arr + @ 1 i + cells arr + ! len 2 - i - cells arr + ! loop ;

32 constant REF-DATE-BUF-LEN
create REF-DATE-BUF REF-DATE-BUF-LEN allot

: REF-BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: DATE-PARSE-OK? ( -- bool )
   s" 2026-06-16" PARSE-YMD swap 20620 = and ;

: DATE-FORMAT-OK? ( -- bool )
   20620 REF-DATE-BUF REF-DATE-BUF-LEN FORMAT-YMD
   s" 2026-06-16" REF-BYTES= ;

: EPOCH-UTC-OK? ( -- bool )
   90061 REF-DATE-BUF REF-DATE-BUF-LEN FORMAT-EPOCH-UTC
   s" 1970-01-02T01:01:01Z" REF-BYTES= ;

: MONO-ELAPSED? ( -- bool )
   TIME-MONO-NS 0 100000 0 do i + loop drop TIME-MONO-NS swap - 0 >= ;

: INVALID-DATE? ( -- bool )
   s" 2026-02-29" PARSE-YMD 0= swap drop ;

0 set-check  variable AP  variable #BAD  0 #BAD !
: G= ( got want ) <> if 1 #BAD +! then ;

\ array->scalar checks
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-SUM 14 G=
here 1 , 2 , 3 , AP !          AP @ 3 ARR-SUM 6 G=
here -2 , -3 , AP !            AP @ 2 ARR-SUM -5 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-MAX 5 G=
here -5 , -2 , -9 , AP !       AP @ 3 ARR-MAX -2 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARR-MIN 1 G=
here -5 , -2 , -9 , AP !       AP @ 3 ARR-MIN -9 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 ARGMAX 4 G=
here 1 , 5 , 5 , 2 , AP !      AP @ 4 ARGMAX 1 G=
here 9 , 1 , 1 , AP !          AP @ 3 ARGMAX 0 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 COUNT-EVEN 1 G=
here 2 , 4 , 6 , AP !          AP @ 3 COUNT-EVEN 3 G=
here 0 , 2 , 0 , AP !          AP @ 3 COUNT-EVEN 3 G=

\ array->array checks (in place; read back each cell)
here 1 , 2 , 3 , AP !  AP @ 3 REVERSE  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 2 G= AP @ 2 cells + @ 1 G=
here 1 , 2 , AP !      AP @ 2 REVERSE  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 1 G=
here 3 , 1 , 4 , 1 , 5 , AP !  AP @ 5 PREFIXSUM  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 4 G= AP @ 2 cells + @ 8 G= AP @ 3 cells + @ 9 G= AP @ 4 cells + @ 14 G=
here 2 , -1 , 3 , AP !  AP @ 3 PREFIXSUM  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 4 G=
here -2 , 3 , AP !  AP @ 2 SQ-EACH  AP @ 0 cells + @ 4 G= AP @ 1 cells + @ 9 G=
here -2 , 0 , 7 , AP !  AP @ 3 NEGATE-EACH  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 0 G= AP @ 2 cells + @ -7 G=
here 2 , 7 , 1 , AP !  AP @ 3 RUNMAX  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 7 G= AP @ 2 cells + @ 7 G=
here 5 , 4 , 3 , AP !  AP @ 3 RUNMAX  AP @ 0 cells + @ 5 G= AP @ 1 cells + @ 5 G= AP @ 2 cells + @ 5 G=
here 3 , -1 , 4 , -2 , AP !  AP @ 4 FIND-FIRST-NEG 1 G=
here 1 , 2 , 3 , AP !        AP @ 3 FIND-FIRST-NEG -1 G=
here -5 , AP !               AP @ 1 FIND-FIRST-NEG 0 G=
here 0 , -1 , AP !           AP @ 2 FIND-FIRST-NEG 1 G=
here 3 , -1 , -4 , 0 , AP !  AP @ 4 ABS-EACH  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 4 G= AP @ 3 cells + @ 0 G=
here -2 , 3 , AP !           AP @ 2 ABS-EACH  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 3 G=
here 3 , 1 , 4 , AP !        AP @ 3 ADD-INDEX  AP @ 0 cells + @ 3 G= AP @ 1 cells + @ 2 G= AP @ 2 cells + @ 6 G=
here 0 , 0 , 0 , AP !        AP @ 3 ADD-INDEX  AP @ 0 cells + @ 0 G= AP @ 1 cells + @ 1 G= AP @ 2 cells + @ 2 G=
here 2 , 3 , 4 , AP !        AP @ 3 PREFIX-PROD  AP @ 0 cells + @ 2 G= AP @ 1 cells + @ 6 G= AP @ 2 cells + @ 24 G=
here -1 , 2 , -3 , AP !      AP @ 3 PREFIX-PROD  AP @ 0 cells + @ -1 G= AP @ 1 cells + @ -2 G= AP @ 2 cells + @ 6 G=
here 1 , 2 , 3 , 4 , 5 , AP !  AP @ 5 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 4 G= AP @ 2 cells + @ 3 G= AP @ 3 cells + @ 2 G= AP @ 4 cells + @ 5 G=
here 1 , 2 , 3 , 4 , AP !    AP @ 4 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 3 G= AP @ 2 cells + @ 2 G= AP @ 3 cells + @ 4 G=
here 1 , 2 , AP !            AP @ 2 REVERSE-INNER  AP @ 0 cells + @ 1 G= AP @ 1 cells + @ 2 G=

\ stdlib date/time checks
DATE-PARSE-OK? -1 G=
DATE-FORMAT-OK? -1 G=
EPOCH-UTC-OK? -1 G=
MONO-ELAPSED? -1 G=
INVALID-DATE? -1 G=

: REP #BAD @ 0= if ." REF-OK" else ." REF-FAIL bad=" #BAD @ . then cr ; REP
