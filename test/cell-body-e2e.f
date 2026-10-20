\ An explicit created-body grid survives a span-valued does> publication.
\ Run: bin/hb --load test/cell-body-e2e.f
require src/lib/test.f
require src/lib/span.f
require src/core/cell-body.f

package CELL-BODY-OTHER
public
: SAME ( -- n ) 7 ;
;package

package CELL-BODY-OWNER
public
create SAME 0 , 2 ,
;package

package CELL-BODY-OTHER
public
2 CELL-BODY
undefine SAME
;package

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

0 GRID-MAKE LIVE-GRID

: LIVE-BODY ( -- ptr u8 )
   LIVE-GRID SPAN:$ drop CELL negate + ;

create LIVE-RAW 2 cells allot
2 cells negate allot
create LIVE-REUSE 2 cells allot

: REPLACED-BODY ( -- ptr u8 )
   REPLACED-GRID SPAN:$ drop CELL negate + ;

undefine REPLACED-GRID
: REPLACED-GRID ( -- n ) 9 ;

: ZERO-BODY ( -- ptr u8 )
   ZERO-GRID SPAN:$ drop CELL negate + ;

: LARGE-BODY ( -- ptr u8 )
   LARGE-GRID SPAN:$ drop CELL negate + ;

: AMBIENT-BODY ( -- ptr u8 )
   CELL-BODY-OWNER:SAME BYTE-VIEW ;

variable ZERO-HITS
variable LARGE-HITS
variable REPLACED-HITS
variable SHRUNK-HITS
variable RETIRED-HITS
variable LIVE-HITS
variable FAILED-HITS
TYPED-VARIABLE FAILED-BASE ptr u8
variable AMBIENT-HITS

: SEE-CELLS ( ptr u8 n -- )
   {: base:ptr count:n :}
   base ZERO-BODY = if
      count 2 cells = if 1 ZERO-HITS +! then
   then
   base LARGE-BODY = if
      count 2 cells = if 1 LARGE-HITS +! then
   then
   base REPLACED-BODY = if
      count 2 cells = if 1 REPLACED-HITS +! then
   then
   base SHRUNK-BODY = if
      count 2 cells = if 1 SHRUNK-HITS +! then
   then
   base RETIRED-BODY = if
      count 2 cells = if 1 RETIRED-HITS +! then
   then
   base LIVE-BODY = if
      count 2 cells = if 1 LIVE-HITS +! then
   then
   base FAILED-BASE @ = if
      count 2 cells = if 1 FAILED-HITS +! then
   then
   base AMBIENT-BODY = if
      count 2 cells = if 1 AMBIENT-HITS +! then
   then ;

TRUSTED: SCAN-GRIDS ( -- )
   [: SEE-CELLS ;] CHECKER-STORAGE-CELLS ;

CAST: ACTION-XT ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )

: OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

: UNIT-MARK ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ ACTION-XT execute ;

: UNIT-EXPORT ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;

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
   LIVE-RAW LIVE-REUSE = TTRUE
   0 ZERO-HITS ! 0 LARGE-HITS ! 0 REPLACED-HITS !
   0 SHRUNK-HITS ! 0 RETIRED-HITS !
   0 LIVE-HITS ! 0 FAILED-HITS ! 0 AMBIENT-HITS !
   SCAN-GRIDS
   ZERO-HITS @ 1 T= LARGE-HITS @ 1 T= REPLACED-HITS @ 0 T=
   SHRUNK-HITS @ 0 T= RETIRED-HITS @ 0 T=
   LIVE-HITS @ 1 T= FAILED-HITS @ 0 T=
   AMBIENT-HITS @ 1 T=
   REPLACED-GRID 9 T=
   ZERO-GRID SPAN:LEN CELL T=
   LARGE-GRID SPAN:LEN CELL T=
   s" create CB-BAD-ZERO 0 CELL-BODY" TEST-EVAL:RC 7121 T=
   s" create CB-BAD-SHORT 0 , 2 , 1 CELL-BODY" TEST-EVAL:RC 7121 T=
   s" create CB-BAD-OVER 0 , 2 , $7FFFFFFFFFFFFFFF CELL-BODY"
      TEST-EVAL:RC 7121 T=
   s" create CB-RETIRED 0 , 2 , undefine CB-RETIRED 2 CELL-BODY"
      TEST-EVAL:RC 7121 T= ;

public
EXPORT GRID-MAKE
EXPORT UNIT-MARK
EXPORT UNIT-EXPORT

T-RESET
RUN
;package

: CELL-BODY-EXPORT-CHECK ( -- )
   [: CELL-BODY-TEST:UNIT-EXPORT 2drop ;] catch 0 T= ;

CELL-BODY-TEST:UNIT-MARK
s" 0 CELL-BODY-TEST:GRID-MAKE EXPORT-GRID 2 cells negate allot" evaluate-closed
CELL-BODY-EXPORT-CHECK
s" 2 cells negate allot" evaluate-closed
T-REPORT
