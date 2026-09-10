\ combinators.f — checked iterators preserve the callback's entire stack row.

require lib/test.f

package COMBINATORS-TEST

create ITEMS 3 cells allot

: SEED ( -- )
   3 ITEMS !  4 ITEMS cell+ !  5 ITEMS 2 cells + ! ;

: MAP-ROW ( -- n )
   10 ITEMS 3 [: swap 1+ swap 2 * ;] MAP ;

: SUM ( -- n )
   ITEMS 3 0 [: + ;] FOLD ;

: EACH-ROW ( -- n n )
   10 0 ITEMS 3 [: + swap 1+ swap ;] EACH ;

: FOLD-ROW ( -- n n )
   10 ITEMS 3 0 [: + swap 1+ swap ;] FOLD ;

: EMPTY-MAP ( -- n )
   10 ITEMS 0 [: swap 1+ swap 2 * ;] MAP ;

: EMPTY-EACH ( -- n )
   10 ITEMS 0 [: + ;] EACH ;

: EMPTY-FOLD ( -- n n )
   10 ITEMS 0 7 [: + swap 1+ swap ;] FOLD ;

: TWICE ( n -- n )
   2 [: 1+ ;] TIMES ;

: NESTED-TIMES ( -- n )
   0 3 [: TWICE ;] TIMES ;

: EMPTY-TIMES ( -- n )
   10 0 [: 1+ ;] TIMES ;

T-RESET
SEED
MAP-ROW 13 T=
ITEMS @ 6 T=
ITEMS cell+ @ 8 T=
ITEMS 2 cells + @ 10 T=
SUM 24 T=
EACH-ROW 24 T= 13 T=
FOLD-ROW 24 T= 13 T=
EMPTY-MAP 10 T=
EMPTY-EACH 10 T=
EMPTY-FOLD 7 T= 10 T=
SUM 24 T=
NESTED-TIMES 6 T=
EMPTY-TIMES 10 T=
T-REPORT

;package
