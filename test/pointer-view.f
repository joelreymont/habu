\ Explicit representation views preserve addresses and define access width.
require src/lib/test.f
require src/lib/memory.f
require src/lib/array.f

package POINTER-VIEW-TEST
private

NEWTYPE item
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
   s" BAD-NOMINAL-VIEW ( ptr u8 -- ptr item ) CELL-VIEW" REJECT
   s" BAD-ARRAY-READ ( ptr item len -- n ) ARRAY:A-SUM" REJECT
   s" BAD-ARRAY-STORE ( ptr item len -- ) ARRAY:A-PREFIX-SUM!" REJECT
   T-REPORT ;

RUN
;package
