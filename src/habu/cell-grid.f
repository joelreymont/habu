\ cell-grid.f - a span of DATA cells in image form; the only Forth that writes or
\ reads it. Writers: AOT capture (aot-decl.f AOT-WINDOW, aot-link-arm.f
\ BUILD-SPARSE-DATA), the snapshot heap when smaller (snap-lib.f ENCODE-HEAP).
\ Emitted readers: habu2.f AOT-WINDOW:APPLY-CELLS, aot-link-arm.f EMIT-DATA-COPY,
\ habu2.f EM-SNAPSHOT-DECODE-HEAP, which refuses any non-canonical byte: another
\ engine built its image.

package CELL-GRID
\ Callers size and own every buffer.
public

\ Image form, framed by each writer with the group count G and stored group
\ bytes S: a presence map of ceil(G/8) bytes (group g in bit g mod 8 of byte
\ g div 8, low bit first), the present groups of GROUP-BYTES bitmap bytes in
\ order, the last zero-padded, then one unsigned LEB128 per present cell in cell
\ order. It is canonical: a present cell is never zero, a present group holds a
\ present cell, the last group is present and every value has one width.
\ A bitmap over cells, not extents: the release engine's window holds 772,892
\ content bytes in 256,128 non-zero extents. Bytes for the whole window:
\   (a) varint (gap, length) run rows plus their bytes        1,285,454
\   (b) this format: cell bitmap plus one varint per cell       964,570
\   (c) (b) with raw 8-byte cells instead of varints          2,535,017
\   (d) a per-4-KiB-page choice between (a) and (b)             843,675
\ (d) is 9.4% below (b) but costs two decoders and a tag byte a page.
\ Grouping: 69,906 of the bitmap's 130,792 bytes are all clear; 2,044 groups
\ cost a 256-byte presence map and save 63,360 bytes of bitmap.
8 constant CELL-BYTES                \ the grid's cell: the DATA cell a declared address sits on
8 constant CELL-BITS                 \ cells one bitmap byte covers
CELL-BYTES CELL-BITS * constant BM-BYTE-SPAN
64 constant GROUP-BYTES              \ bitmap bytes one presence bit covers
GROUP-BYTES BM-BYTE-SPAN * constant GROUP-SPAN   \ DATA bytes such a group covers
10 constant VMAX                     \ an unsigned LEB128 of a whole cell is at most ten bytes

\ A CELL'S VALUE IS AN UNSIGNED LEB128: seven bits a byte, low group first, high
\ bit set while more groups follow. tools/engine-size.f and tools/image-size-lib.f
\ mirror the reader for the images they walk.
\ A VALUE IS A WHOLE CELL, so every bit pattern a cell can hold is a field this
\ format expresses: an address, a small count and a cell of packed bytes alike.
\ A negative Habu cell is the unsigned value with bit 63 set and encodes in VMAX
\ bytes, the widest this format has.
: CELL-VLEN ( n -- n ) {: v:n :}
   VMAX 1 ?do
      v i 7 * rshift 0= if i unloop exit then
   loop
   VMAX ;

: CELL-V! ( n ptr u8 -- n ) {: v:n p:ptr :}   \ answers the bytes written
   v CELL-VLEN {: w:n :}
   w 0 ?do
      v i 7 * rshift $7F and {: g:n :}
      i 1+ w < if g $80 or else g then  p i + c!
   loop
   w ;

\ The width, or 0 for a varint that does not end within `avail`, runs past VMAX,
\ or wastes a byte on a zero high group - one encoding per value, so a round trip
\ through this format is an identity rather than a resemblance.
private

: CELL-VW@ ( ptr u8 n -- n ) {: p:ptr avail:n :}
   avail VMAX min 0 ?do
      p i + c@ $80 and 0= if
         i 1+ {: w:n :}
         w 1 > p w 1- + c@ 0= and if 0 unloop exit then
         w unloop exit
      then
   loop
   0 ;

: CELL-VV@ ( ptr u8 n -- n ) {: p:ptr w:n :}
   0 w 0 ?do  p i + c@ $7F and  i 7 * lshift or  loop ;

public

\ Value and width, with a width of 0 for every malformation above. The tenth
\ group holds bit 63 alone, so a tenth byte above one is a value no cell held.
: CELL-V@ ( ptr u8 n -- n n ) {: p:ptr avail:n :}
   p avail CELL-VW@ {: w:n :}
   w 0= if 0 0 exit then
   w VMAX = if p VMAX 1- + c@ 1 > if 0 0 exit then then
   p w CELL-VV@ {: v:n :}
   v w ;

\ A writer first builds the FLAT bitmap, one bit a cell with cell c in bit c mod
\ 8 of byte c div 8, ending at the byte of its last present cell, and this turns
\ it into the image form. The flat form is the writers' working state: an AOT
\ merge appends to it (src/habu/aot-file.f PLACE-WDATA) and it never reaches an
\ image.

\ Groups a flat bitmap of `len` bytes covers, and the presence map's bytes.
: GROUPS ( n -- n ) GROUP-BYTES 1- + GROUP-BYTES / ;

: PMAP-BYTES ( n -- n ) CELL-BITS 1- + CELL-BITS / ;

: GROUP-SET? ( ptr u8 n n -- bool ) {: bm:ptr len:n g:n :}
   g GROUP-BYTES * {: at:n :}
   len at - GROUP-BYTES min 0 ?do
      bm at + i + c@ 0<> if true unloop exit then
   loop
   false ;

\ Bytes the present groups store: what a writer reserves after the map.
: STORED-BYTES ( ptr u8 n -- n ) {: bm:ptr len:n :}
   0  len GROUPS 0 ?do  bm len i GROUP-SET? if GROUP-BYTES + then  loop ;

private

: MAP-BIT! ( n ptr u8 -- ) {: g:n map:ptr :}
   map g CELL-BITS / + {: at:ptr :}
   at c@  1 g CELL-BITS mod lshift or  at c! ;

\ Group g's bitmap bytes, zero past the flat bitmap's end.
: COPY-GROUP ( ptr u8 n n ptr u8 -- ) {: bm:ptr len:n g:n to:ptr :}
   g GROUP-BYTES * {: at:n :}
   GROUP-BYTES 0 ?do
      at i + len < if bm at + i + c@ else 0 then  to i + c!
   loop ;

public

\ The flat bitmap in; its presence map and present groups written at `dst`,
\ which holds the group count's PMAP-BYTES plus STORED-BYTES. Answers the group
\ count and the stored bytes. The last group holds the last present cell, so
\ it is present, and no group past the count sets a map bit.
: COMPACT ( ptr u8 n ptr u8 -- n n ) {: bm:ptr len:n dst:ptr :}
   len GROUPS {: g:n :}
   g PMAP-BYTES {: pm:n :}
   pm 0 ?do 0 dst i + c! loop
   0  g 0 ?do
      bm len i GROUP-SET? if
         dup {: stored:n :}
         i dst MAP-BIT!
         bm len i  dst pm + stored +  COPY-GROUP
         GROUP-BYTES +
      then
   loop
   g swap ;

;package
