require src/lib/span.f
require src/lib/memory.f
require src/core/cell-body.f

package CELL-BODY-GRID

: GRID-MAKE ( n -- )
   create , 2 , 2 CELL-BODY
   does> ( -- SPAN:span<u8> ) cell+ byte-view CELL SPAN:MAKE ;

\ The same body laid by `,` alone: the two cells are the created body's grid.
: IMPLICIT-MAKE ( n -- )
   create , 2 ,
   does> ( -- SPAN:span<u8> ) cell+ byte-view CELL SPAN:MAKE ;

public
: OCC-GAP ( -- n ) 1 ;
s" CELL-BODY-GRID:OCC-GAP" FORGET-DEFS-FROM
private

0 GRID-MAKE ZERO
$C00000 GRID-MAKE LARGE
0 GRID-MAKE SHIFTED
create RAW 2 cells allot

: BODY ( SPAN:span<u8> -- ptr u8 ) SPAN:$ drop CELL negate + ;

variable RETIRED-OFF
: RETIRED-BODY ( -- ptr u8 ) data-base RETIRED-OFF @ + BYTE-VIEW ;
: FORGET-RETIRED ( -- )
   s" CELL-BODY-GRID:RETIRED" FORGET-DEFS-FROM ;
public
0 GRID-MAKE RETIRED
: SAVE-RETIRED ( -- )
   RETIRED BODY data-base BYTE-VIEW - RETIRED-OFF ! ;
SAVE-RETIRED
FORGET-RETIRED
private
2 cells negate allot
create REUSED 2 cells allot
\ The reclaimed raw cells retain the retired grid's old bytes.
0 REUSED CELL-VIEW !
0 REUSED CELL + CELL-VIEW !

variable GRID-HITS
\ IMPLICIT is the file's last created word, so its body is still pending at the
\ file's end; this cell locates it for SEE-GRID.
variable IMPLICIT-OFF
: IMPLICIT-BODY ( -- ptr u8 ) data-base IMPLICIT-OFF @ + BYTE-VIEW ;

: SEE-GRID ( ptr u8 n -- )
   {: base:ptr count:n :}
   base REUSED BYTE-VIEW = IF
      s" retired cell grid remained in saved storage" 76 die
   THEN
   count 2 cells = IF
      base ZERO BODY = base LARGE BODY = or base IMPLICIT-BODY = or IF
         1 GRID-HITS +!
      THEN
   THEN ;

public
TRUSTED: GRID-COUNT ( -- n )
   REUSED BYTE-VIEW RETIRED-BODY <> IF
      s" retired cell body was not reused" 76 die
   THEN
   0 GRID-HITS !
   [: SEE-GRID ;] CHECKER-STORAGE-CELLS
   GRID-HITS @ ;

private

: ARM ( -- )
   MEM-ALLOC-64K {: addr:ptr bytes:n :}
   bytes drop
   addr BYTE-VIEW NULL-PTR BYTE-VIEW - {: bits:n :}
   bits 8 lshift SHIFTED BODY CELL-VIEW !
   bits 56 rshift SHIFTED BODY CELL + CELL-VIEW ! ;

ARM

public
\ A fresh build-time mapping's address as an integer.
: MAPPED ( -- n )
   MEM-ALLOC-64K {: addr:ptr bytes:n :}
   bytes drop
   addr BYTE-VIEW NULL-PTR BYTE-VIEW - ;

: BAD-ALIGNED ( -- )
   MEM-ALLOC-64K {: addr:ptr bytes:n :}
   bytes drop
   addr BYTE-VIEW NULL-PTR BYTE-VIEW - ZERO BODY CELL-VIEW ! ;

: BAD-RAW ( -- )
   MEM-ALLOC-64K {: addr:ptr bytes:n :}
   bytes drop
   addr BYTE-VIEW NULL-PTR BYTE-VIEW - {: bits:n :}
   bits 8 lshift RAW CELL-VIEW !
   bits 56 rshift RAW CELL + CELL-VIEW ! ;

: VALUE ( -- n n n )
   ZERO BODY CELL-VIEW @
   LARGE BODY CELL-VIEW @
   ZERO SPAN:LEN ;

private
0 IMPLICIT-MAKE IMPLICIT
: SAVE-IMPLICIT ( -- ) IMPLICIT BODY data-base BYTE-VIEW - IMPLICIT-OFF ! ;
SAVE-IMPLICIT
;package
