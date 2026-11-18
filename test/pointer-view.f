split-str
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

: REJECT ( ^u8 n -- )
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
   s" BAD-IMPLICIT-VIEW ( ^a -- ^u8 ) " REJECT
   s" BAD-IMPLICIT-FETCH ( ^a -- n ) @" REJECT
   s" BAD-BYTE-CELL ( ^u8 -- n ) @" REJECT
   s" BAD-NOMINAL-VIEW ( ^u8 -- ^item ) CELL-VIEW" REJECT
   s" BAD-ARRAY-READ ( ^item len -- n ) ARRAY:A-SUM" REJECT
   s" BAD-ARRAY-STORE ( ^item len -- ) ARRAY:A-PREFIX-SUM!" REJECT
   T-REPORT ;

RUN
;package
