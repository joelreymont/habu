\ A heap for test/snapshot-writer.f whose snapshot stores it as the cell grid:
\ a million zero bytes between two nonzero cells, and two cells of a table the
\ engine bakes into its seed window zeroed. The warm image must read the hole
\ and the zeroed seed cells back as zero, because the grid does not store a
\ zero cell and the seed has put the table's digits there before the restore.
require lib/string.f
require src/habu/cell-grid.f

package SNAP-WRITER-HOLE
public

1000000 constant HOLE-BYTES
-1 constant LO-VALUE                 \ bit 63 set: a ten-byte value
1234567 constant HI-VALUE

create HOLE HOLE-BYTES 16 + allot

: HOLE-LO ( -- n ) HOLE @ ;
: HOLE-HI ( -- n ) HOLE HOLE-BYTES + 8 + @ ;

\ Every cell strictly between the two, or-ed together.
: HOLE-OR ( -- n )
   0  HOLE-BYTES 8 / 0 ?do  HOLE 8 + i 8 * + @ or  loop ;

\ The first two cells of lib/string.f's STR-MAX-I64$, whose digits the seed
\ restores before the snapshot heap is laid over them, and the digit after
\ them, which the fixture leaves alone.
: SEED-CELLS ( -- n ) STR-MAX-I64$ @ STR-MAX-I64$ 8 + @ or ;
: SEED-NEXT ( -- n ) STR-MAX-I64$ 16 + c@ ;

\ Zero when the warm image holds what this file stored; otherwise one bit per
\ wrong answer, in the order of the words above.
: RESTORED ( -- n )
   0
   HOLE-LO LO-VALUE <> if 1 or then
   HOLE-OR 0<> if 2 or then
   HOLE-HI HI-VALUE <> if 4 or then
   SEED-CELLS 0<> if 8 or then
   SEED-NEXT [char] 8 <> if 16 or then ;

\ The grid's presence map has pad bits only when its group count is not a
\ multiple of eight, and the parent doctors one. The heap ends at the last
\ group the save writes, a few KiB past this file, so this file ends its heap
\ a little over two groups into the span one map byte covers.
CELL-GRID:GROUP-SPAN CELL-GRID:CELL-BITS * constant MAP-BYTE-SPAN
CELL-GRID:GROUP-SPAN 2 * 1024 + constant END-IN-SPAN

: HEAP-END ( -- n ) here data-base - DATA-START - ;

;package

SNAP-WRITER-HOLE:LO-VALUE SNAP-WRITER-HOLE:HOLE !
SNAP-WRITER-HOLE:HI-VALUE SNAP-WRITER-HOLE:HOLE SNAP-WRITER-HOLE:HOLE-BYTES + 8 + !
0 STR-MAX-I64$ !
0 STR-MAX-I64$ 8 + !
SNAP-WRITER-HOLE:END-IN-SPAN SNAP-WRITER-HOLE:HEAP-END -
   SNAP-WRITER-HOLE:MAP-BYTE-SPAN 1- and allot
