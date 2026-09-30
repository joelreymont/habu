\ A heap for test/snapshot-writer.f that ends three bytes into a cell, the
\ second cell of its group, with the first zero. The grid rounds the heap up to
\ whole cells, and the loader stores only the part of that last cell below the
\ extent, so the warm image must read the three bytes back and restore the
\ extent exactly. The parent includes this file rather than requiring it: a
\ surviving require record is appended to the heap at capture and ends it on a
\ cell (src/core/include.f REQUIRE-REG:PERSIST).
require src/habu/cell-grid.f

package SNAP-WRITER-TAIL
public

3 constant TAIL-BYTES
variable AT                          \ the tail's DATA offset

: TAIL-BYTE ( n -- n ) 1+ $11 * ;

: RESTORED-BYTE ( n -- n ) {: k:n :}
   data-base AT @ + k + c@ ;

\ Zero when the warm image holds the tail and ends where it ended; otherwise
\ one bit per wrong byte, then 8 for a moved extent.
: RESTORED ( -- n )
   0
   TAIL-BYTES 0 ?do
      i RESTORED-BYTE i TAIL-BYTE <> if 1 i lshift or then
   loop
   here data-base - AT @ TAIL-BYTES + <> if 8 or then ;

\ Laid last, since everything above allots: DP moves to the next group, one
\ zero cell, then the tail's bytes.
: LAY ( -- )
   here data-base - DATA-START - negate CELL-GRID:GROUP-SPAN 1- and allot
   CELL-GRID:CELL-BYTES allot
   here data-base - AT !
   TAIL-BYTES 0 ?do  i TAIL-BYTE c,  loop ;

;package

SNAP-WRITER-TAIL:LAY
