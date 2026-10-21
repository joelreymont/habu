\ A heap for test/snapshot-writer.f that the cell grid would enlarge. Every
\ cell below has bit 63 set, so the grid would store each of its eight bytes as
\ a ten-byte value. The 48 MiB run outweighs the engine heap's zero cells;
\ the writer must keep the heap's bytes.
package SNAP-WRITER-DENSE
public

48 1024 * 1024 * constant DENSE-BYTES
1 63 lshift constant TOP-BIT

create DENSE DENSE-BYTES allot

: PATTERN ( n -- n ) TOP-BIT or ;

: FILL ( -- )
   DENSE-BYTES 8 / 0 ?do  i PATTERN  DENSE i 8 * + !  loop ;

\ The cells the warm image does not hold as FILL stored them.
: MISMATCHES ( -- n )
   0  DENSE-BYTES 8 / 0 ?do  DENSE i 8 * + @ i PATTERN <> if 1+ then  loop ;

;package

SNAP-WRITER-DENSE:FILL
