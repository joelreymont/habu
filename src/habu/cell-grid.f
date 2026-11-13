\ cell-grid.f - a span of DATA cells in image form. Emitted reader:
\ snap-decode-x64.f DECODE,, which refuses a malformed grid.

package CELL-GRID
public

\ Image form, given the group count G and the stored group bytes S: a presence
\ map of ceil(G/8) bytes (group g in bit g mod 8 of byte g div 8, low bit
\ first), the present groups of GROUP-BYTES bitmap bytes in order, the last
\ zero-padded, then one unsigned LEB128 per present cell in cell order. It is
\ canonical: a present cell is never zero, a present group holds a present cell,
\ the last group is present and every value has one width.
8 constant CELL-BYTES                \ the grid's cell: the DATA cell a declared address sits on
8 constant CELL-BITS                 \ cells one bitmap byte covers
CELL-BYTES CELL-BITS * constant BM-BYTE-SPAN
64 constant GROUP-BYTES              \ bitmap bytes one presence bit covers
GROUP-BYTES BM-BYTE-SPAN * constant GROUP-SPAN   \ DATA bytes such a group covers
10 constant VMAX                     \ an unsigned LEB128 of a whole cell is at most ten bytes

\ A CELL'S VALUE IS AN UNSIGNED LEB128: seven bits a byte, low group first, high
\ bit set while more groups follow.
\ A VALUE IS A WHOLE CELL, so every bit pattern a cell can hold is a field this
\ format expresses: an address, a small count and a cell of packed bytes alike.
\ A negative Habu cell is the unsigned value with bit 63 set and encodes in VMAX
\ bytes, the widest this format has.
: CELL-VLEN ( n -- n ) {: v:n :}
   VMAX 1 ?do
      v i 7 * rshift 0= if i unloop exit then
   loop
   VMAX ;

;package
