\ Explicit representation views preserve addresses and define access width.
require lib/test.f
require lib/memory.f
require lib/table.f
require test/checker-assert.f

package POINTER-VIEW-TEST
private

NEWTYPE item 0
CAST: >ITEM ( n -- item )
CAST: ITEM>N ( item -- n )
1 LAYOUT-BUFFER ITEMS item

: REJECT ( ptr u8 n -- )
   2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T= ;

: MAPPED-CELL ( -- )
   1 cells MEM-ALLOC-BYTES {: buf len:n :}
   buf CELL-VIEW BYTE-VIEW buf = TTRUE
   $123456789ABCDEF buf CELL-VIEW !
   buf CELL-VIEW @ $123456789ABCDEF T=
   $5A buf c!
   buf c@ $5A T=
   buf len munmap 0 T= ;

: RUN ( -- )
   T-RESET
   17 >ITEM 0 ITEMS !
   0 ITEMS BYTE-VIEW c@ 17 T=
   0 ITEMS @ ITEM>N 17 T=
   MAPPED-CELL
   s" BAD-IMPLICIT-VIEW ( ptr a -- ptr u8 ) " REJECT
   s" BAD-IMPLICIT-FETCH ( ptr a -- n ) @" REJECT
   s" BAD-BYTE-CELL ( ptr u8 -- n ) @" REJECT
   s" BAD-TABLE-ELEMENT ( ptr item count count idx idx -- n ) TBL:N@" REJECT
   s" BAD-TABLE-STORE ( n ptr item count count idx idx -- ) TBL:N!" REJECT
   T-REPORT ;

RUN
;package
