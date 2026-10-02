\ cell-grid.f — the image form of a span of DATA cells, and the only Forth that
\ writes or reads it: a presence bitmap over the span's eight-byte cells,
\ grouped so that an all-clear run of the bitmap is not stored at all, then one
\ unsigned LEB128 per present cell in cell order. A zero cell costs nothing but
\ its bit, and an all-zero group of cells costs nothing at all.
\
\ Two writers use it. The AOT capture stores a window's DATA in an engine or a
\ stripped image (src/habu/aot-decl.f AOT-WINDOW, src/habu/aot-lib.f
\ BUILD-SPARSE-DATA), and the snapshot writer stores a --repl image's heap when
\ this form is smaller than the heap's own bytes (src/habu/snap-lib.f
\ ENCODE-HEAP). The emitted readers are src/habu/habu2.f AOT-WINDOW:APPLY-CELLS
\ for the baked window, src/habu/aot-lib.f EMIT-DATA-COPY for a stripped image
\ and src/habu/habu2.f EM-SNAPSHOT-DECODE-HEAP for a snapshot heap, the one
\ reader that refuses every non-canonical byte because the image it reads was
\ not built with the engine that reads it.
\
\ Image form, framed by each writer with the group count G and the stored group
\ bytes S:
\   [presence map, ceil(G/8) bytes: group g in bit g mod 8 of byte g div 8, low
\    bit first][the present groups, GROUP-BYTES bitmap bytes each in group
\    order, the last zero-padded][one unsigned LEB128 per present cell, in cell
\    order]
\ The form is canonical: a present cell is never zero, a present group holds a
\ present cell, the last group is present and every value has one width.
\
\ WHY A BITMAP OVER CELLS AND NOT EXTENTS. DATA is tables of cells holding
\ small numbers, so its non-zero bytes come in ones and twos: the release
\ engine's window holds 772,892 content bytes in 256,128 maximal non-zero
\ extents, about 1.7 bytes each. Any extent format pays a header per extent -
\ the varint (gap, length) rows this replaced cost 512,563 bytes for those
\ 256,128 extents - while a bitmap pays ONE BIT PER CELL whether the cell is
\ present or not, and a present cell then pays only its own varint. Measured on
\ the release engine (4,063,424 bytes), bytes for the whole captured window:
\   (a) varint (gap, length) run rows plus their bytes        1,285,454
\   (b) this format: cell bitmap plus one varint per cell       964,570
\   (c) (b) with raw 8-byte cells instead of varints          2,535,017
\   (d) a per-4-KiB-page choice between (a) and (b)             843,675
\ (d) is 9.4% of the DATA class below (b) and costs two decoders and a tag byte
\ a page, so the one encoding is (b).
\ WHY THE BITMAP IS GROUPED. Most of a bitmap is zero, because most of a span is
\ room `allot`ed and never written, and it is zero in runs: the release engine's
\ 130,792 bitmap bytes include 69,906 that are all clear. A presence bit per
\ GROUP-BYTES bitmap bytes (512 cells, GROUP-SPAN bytes of DATA) drops a group
\ that holds no present cell: measured on that engine, 2,044 groups cost a
\ 256-byte presence map and save 63,360 bytes of bitmap.
\ The words below hold nothing between calls, so a caller sizes and owns every
\ buffer: the AOT capture's fixed caps are its own, and a snapshot sizes its
\ scratch from the content it encodes.

package CELL-GRID
public

8 constant CELL-BYTES                \ the grid's cell: the DATA cell a declared address sits on
8 constant CELL-BITS                 \ cells one bitmap byte covers
CELL-BYTES CELL-BITS * constant BM-BYTE-SPAN
64 constant GROUP-BYTES              \ bitmap bytes one presence bit covers
GROUP-BYTES BM-BYTE-SPAN * constant GROUP-SPAN   \ DATA bytes such a group covers
10 constant VMAX                     \ an unsigned LEB128 of a whole cell is at most ten bytes

\ ---- the value codec -------------------------------------------------------
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

\ ---- the grouped bitmap ------------------------------------------------------
\ A writer first builds the FLAT bitmap, one bit a cell with cell c in bit c mod
\ 8 of byte c div 8, ending at the byte of its last present cell, and this turns
\ it into the image form. The flat form is the writers' working state: an AOT
\ merge appends to it (src/habu/aot-file.f PLACE-WDATA) and it never reaches an
\ image.

\ Groups a flat bitmap of `len` bytes covers, and the presence map's bytes.
: GROUPS ( n -- n ) GROUP-BYTES 1- + GROUP-BYTES / ;

: PMAP-BYTES ( n -- n ) CELL-BITS 1- + CELL-BITS / ;

\ Whether group g of the flat bitmap holds a present cell.
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
