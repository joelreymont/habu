\ An explicit created-body grid survives a span-valued does> publication.
\ Run: bin/hb --load test/cell-body-e2e.f
require src/lib/test.f
require src/lib/span.f
require src/core/cell-body.f

package CELL-BODY-TEST

: GRID-MAKE ( n -- )
   create , 2 , 2 CELL-BODY
   does> ( -- SPAN:span<u8> ) cell+ byte-view CELL SPAN:MAKE ;

0 GRID-MAKE ZERO-GRID
$C00000 GRID-MAKE LARGE-GRID
0 GRID-MAKE REPLACED-GRID
0 GRID-MAKE RETIRED-GRID

: RETIRED-BODY ( -- ptr u8 )
   RETIRED-GRID SPAN:$ drop CELL negate + ;

undefine RETIRED-GRID
2 cells negate allot
create RETIRED-RAW 2 cells allot

0 GRID-MAKE SHRUNK-GRID

: SHRUNK-BODY ( -- ptr u8 )
   SHRUNK-GRID SPAN:$ drop CELL negate + ;

2 cells negate allot
create SHRUNK-RAW 2 cells allot

undefine REPLACED-GRID
: REPLACED-GRID ( -- n ) 9 ;

TYPED-VARIABLE FAILED-BASE ptr u8

create EMPTY-GRID

: EMPTY-BODY ( -- ptr u8 ) EMPTY-GRID BYTE-VIEW ;

: RUN ( -- )
   s" 0 , 2 , 2 CELL-BODY -123 throw" TEST-EVAL:RC -123 T=
   s" create EMPTY-RAW 2 cells allot" evaluate-closed
   EMPTY-GRID EMPTY-BODY = TTRUE
   s" 0 GRID-MAKE FAILED-GRID : FAILED-BODY ( -- ptr u8 ) FAILED-GRID SPAN:$ drop CELL negate + ; FAILED-BODY FAILED-BASE ! -124 throw"
      TEST-EVAL:RC -124 T=
   s" create FAILED-RAW 2 cells allot" evaluate-closed
   s" FAILED-RAW FAILED-BASE @ =" TEST-EVAL:FLAG TTRUE
   s" EMPTY-RAW EMPTY-BODY =" TEST-EVAL:FLAG TTRUE
   SHRUNK-RAW SHRUNK-BODY = TTRUE
   RETIRED-RAW RETIRED-BODY = TTRUE
   REPLACED-GRID 9 T=
   ZERO-GRID SPAN:LEN CELL T=
   LARGE-GRID SPAN:LEN CELL T=
   s" create CB-BAD-ZERO 0 CELL-BODY" TEST-EVAL:RC 7121 T=
   s" create CB-BAD-SHORT 0 , 2 , 1 CELL-BODY" TEST-EVAL:RC 7121 T=
   s" create CB-BAD-OVER 0 , 2 , $7FFFFFFFFFFFFFFF CELL-BODY"
      TEST-EVAL:RC 7121 T=
   s" create CB-RETIRED 0 , 2 , undefine CB-RETIRED 2 CELL-BODY"
      TEST-EVAL:RC 7121 T= ;

T-RESET
RUN
;package

T-REPORT
