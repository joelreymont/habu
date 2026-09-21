\ The window DATA cell value's varint codec, at the widths and the malformations
\ the engine's own content never reaches. src/habu/aot-decl.f package AOT-WINDOW
\ is the authority for the grammar; the two emitted decoders (habu2.f
\ APPLY-CELLS for the baked window, aot-lib.f EMIT-VGET for a stripped image's
\ own DATA) are the same grammar in ARM64 and are pinned where they are emitted
\ (test/gate-aot-image.f CHECK-VGET) and exercised by every build.
require lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package AOT-CELL-VALUE-TEST
using AOT-WINDOW

create VBUF 16 allot

: B! ( n n -- ) {: v:n at:n :} v VBUF at + c! ;

\ Write, then read back: the width the encoder charges, the width it wrote, the
\ width the reader spent and the value it answers are one number and one value.
: ROUND ( n n -- ) {: v:n want:n :}
   v CELL-VLEN want T=
   v VBUF CELL-V! want T=
   VBUF 16 CELL-V@ {: got:n w:n :}
   w want T=
   got v T= ;

\ Every malformation answers width 0 and value 0, so a caller that checks the
\ width cannot be handed a plausible value from a cell this format never wrote.
: REFUSED ( n -- ) {: avail:n :}
   VBUF avail CELL-V@ {: got:n w:n :}
   w 0 T=
   got 0 T= ;

: WIDTHS ( -- )
   s" seven bits travel in one byte" T-LABEL
   0 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   1 1 ROUND
   s" seven bits travel in one byte" T-LABEL
   $7F 1 ROUND
   s" the eighth bit opens a second group" T-LABEL
   $80 2 ROUND
   s" fourteen bits still fit two groups" T-LABEL
   $3FFF 2 ROUND
   s" the fifteenth bit opens a third group" T-LABEL
   $4000 3 ROUND
   s" twenty-eight bits still fit four groups" T-LABEL
   $FFFFFFF 4 ROUND
   s" a full u32 spends five groups" T-LABEL
   $FFFFFFFF 5 ROUND
   s" fifty-six bits fit eight groups" T-LABEL
   $FFFFFFFFFFFFFF 8 ROUND
   s" the fifty-seventh bit opens a ninth group" T-LABEL
   $100000000000000 9 ROUND
   s" sixty-three bits still fit nine groups" T-LABEL
   $7FFFFFFFFFFFFFFF 9 ROUND
   s" a cell with its top bit set is the widest value" T-LABEL
   -1 VMAX ROUND
   s" ... and so is the cell that holds only that bit" T-LABEL
   $8000000000000000 VMAX ROUND ;

: TRUNCATED ( -- )
   $80 VBUF CELL-V! 2 T=
   s" a varint that does not end inside the section is refused" T-LABEL
   1 REFUSED ;

: EMPTY ( -- )
   s" no bytes left is not a value" T-LABEL
   0 REFUSED ;

: PADDED ( -- )
   $81 0 B!  $00 1 B!
   s" a value padded with a zero high group is refused" T-LABEL
   2 REFUSED ;

: OVERLONG ( -- )
   VMAX 0 ?do $80 i B! loop  $00 VMAX B!
   s" a varint that runs past ten bytes is refused" T-LABEL
   VMAX 1+ REFUSED ;

: TOO-WIDE ( -- )
   VMAX 1- 0 ?do $80 i B! loop  $02 VMAX 1- B!
   s" a value no cell could have held is refused" T-LABEL
   VMAX REFUSED ;

\ The grid the bitmap covers is the DATA cell grid, and the bitmap's own cap is
\ the span cap's arithmetic rather than a second guess at how big a window gets.
: GRID ( -- )
   s" a bitmap byte covers eight cells of window" T-LABEL
   CELL-BYTES CELL-BITS * BM-BYTE-SPAN T=
   s" the bitmap cap is the span cap in bitmap bytes" T-LABEL
   SPAN-CAP BM-BYTE-SPAN / BM-CAP T= ;

: RUN ( -- )
   T-RESET
   WIDTHS  TRUNCATED  EMPTY  PADDED  OVERLONG  TOO-WIDE  GRID
   T-REPORT ;

RUN
;using
;package
