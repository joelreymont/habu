\ A heap for test/snapshot-writer.f that ends three bytes into a cell, the
\ second cell of its group, with the first zero. The grid rounds the heap up to
\ whole cells, and the loader stores only the part of that last cell below the
\ extent, so the warm image must read the three bytes back and restore the
\ extent exactly. The capture's prepare appends what the session left to
\ persist: a checker store it grew past its DATA room (src/core/checker.f
\ REG-PERSIST-MOVE, USIGS-SNAPSHOT-PERSIST) and the require records
\ (src/core/include.f REQUIRE-REG:PERSIST). So the parent runs that prepare,
\ then LAY, then the save, whose own prepare finds nothing left to move.
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

\ Laid last, after the capture's prepare: DP moves to the next group, one zero
\ cell, then the tail's bytes.
: LAY ( -- )
   here data-base - DATA-START - negate CELL-GRID:GROUP-SPAN 1- and allot
   CELL-GRID:CELL-BYTES allot
   here data-base - AT !
   TAIL-BYTES 0 ?do  i TAIL-BYTE c,  loop ;

;package
