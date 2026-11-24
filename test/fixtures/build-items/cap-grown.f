\ cap-grown.f - a program that keeps more address rows than the band holds: a
\ buffer of 70000 quotation cells, the last one stored by the build.

require src/habu/address-cells.f

package BITEM-CAP-GROWN
private
70000 TYPED-BUFFER QS [ n -- n ]
69999 constant LAST
: STORE ( -- ) [: 41 + ;] LAST QS ! ;
STORE
variable MARKED

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;

public
: PRINT ( -- )
   1 LAST QS @ execute .
   ROWS ADDRESS-CELLS:BOOT-CAP > bool>n .
   ROWS {: before:n :}  MARKED ptr-cell-mark  ROWS before - . ;
;package

: MAIN ( -- ) BITEM-CAP-GROWN:PRINT ;
