\ array-test.f - focused tests for the ARRAY cell-array helpers.
\ Run: bin/hb --load src/lib/array-test.f

require src/lib/array.f
require src/lib/test/assert.f

create AT-EMPTY
create AT-ONE 42 ,
create AT-NORMAL 3 , -1 , 4 , 4 , 2 ,
create AT-WORK 5 cells allot

: AT-WORK1 ( n -- )
   AT-WORK ! ;

: AT-WORK5 ( n n n n n -- )
   AT-WORK 4 cells + !
   AT-WORK 3 cells + !
   AT-WORK 2 cells + !
   AT-WORK cell+ !
   AT-WORK ! ;

: AT-A-SUM ( ^n n -- n ) {: arr:^ len :}
   arr len >LEN ARRAY:A-SUM ;

: AT-A-COUNT-EVEN ( ^n n -- n ) {: arr:^ len :}
   arr len >LEN ARRAY:A-COUNT-EVEN COUNT>N ;

: AT-A-PREFIX-SUM! ( ^n n -- ) {: arr:^ len :}
   arr len >LEN ARRAY:A-PREFIX-SUM! ;

: AT-WORK5@ ( -- n n n n n )
   AT-WORK @
   AT-WORK cell+ @
   AT-WORK 2 cells + @
   AT-WORK 3 cells + @
   AT-WORK 4 cells + @ ;

: AT-TEST-SCALARS ( -- )
   AT-EMPTY 0 AT-A-SUM 0 T=
   AT-ONE 1 AT-A-SUM 42 T=
   AT-NORMAL 5 AT-A-SUM 12 T=
   AT-EMPTY 0 AT-A-COUNT-EVEN 0 T=
   AT-ONE 1 AT-A-COUNT-EVEN 1 T=
   AT-NORMAL 5 AT-A-COUNT-EVEN 3 T= ;

: AT-TEST-PREFIX ( -- )
   77 AT-WORK1
   AT-WORK 0 AT-A-PREFIX-SUM!
   77 AT-WORK1
   AT-WORK 1 AT-A-PREFIX-SUM!
   AT-WORK @ 77 T=
   3 -1 4 1 5 AT-WORK5
   AT-WORK 5 AT-A-PREFIX-SUM!
   AT-WORK5@ 12 T= 7 T= 6 T= 2 T= 3 T= ;

: AT-MAIN ( -- )
   AT-TEST-SCALARS
   AT-TEST-PREFIX
   T-REPORT ;

T-RESET
AT-MAIN
