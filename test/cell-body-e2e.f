\ An explicit created-body grid survives a span-valued does> publication, and
\ the cells `,` wrote from a created body's start up to the first other
\ allocation are that body's grid.
require src/lib/test.f
require src/lib/span.f
require src/core/cell-body.f

\ A loaded file's end closes the created body it left pending, before its
\ loader's next `,`; the start of another file's load closes the loader's
\ pending body before the loaded file's first `,`.
require test/fixtures/cell-body-tail.f
3 ,
package CELL-BODY-TEST
create LOAD-START 1 ,
;package
require test/fixtures/cell-body-load.f
2 ,

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

variable SEEN-OFF
variable SEEN-BYTES

: SEE-GRID ( ptr u8 n -- )
   {: base:ptr bytes:n :}
   base data-base BYTE-VIEW - SEEN-OFF @ = IF bytes SEEN-BYTES ! THEN ;

\ The bytes of the storage grid that starts at the body, 0 for none.
TRUSTED: GRID-BYTES ( ptr u8 -- n )
   data-base BYTE-VIEW - SEEN-OFF !  0 SEEN-BYTES !
   [: SEE-GRID ;] CHECKER-STORAGE-CELLS
   SEEN-BYTES @ ;

create ADJACENT-C 3 ,
create ADJACENT-D 4 ,
create RESTART 1 , 8 allot 2 ,
variable SHRUNK-VAR -8 allot 5 ,

TYPED-VARIABLE FAILED-BASE ptr u8

create EMPTY-GRID

: EMPTY-BODY ( -- ptr u8 ) EMPTY-GRID BYTE-VIEW ;

: RUN ( -- )
   s" 0 , 2 , 2 CELL-BODY -123 throw" TEST-EVAL:RC -123 T=
   s" create EMPTY-RAW 2 cells allot" evaluate-closed
   EMPTY-GRID EMPTY-BODY = TTRUE
   CELL-BODY-TAIL:TAIL-GRID BYTE-VIEW GRID-BYTES CELL T=
   LOAD-START BYTE-VIEW GRID-BYTES CELL T=
   ADJACENT-C BYTE-VIEW GRID-BYTES CELL T=
   ADJACENT-D BYTE-VIEW GRID-BYTES CELL T=
   RESTART BYTE-VIEW GRID-BYTES CELL T=
   SHRUNK-VAR BYTE-VIEW GRID-BYTES CELL T=
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
