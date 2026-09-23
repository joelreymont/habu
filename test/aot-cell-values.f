\ The window DATA cell value's varint codec, at the widths and the malformations
\ the engine's own content never reaches. src/habu/image-cells.f owns the
\ grammar; image-cell-arm64.f emits the shared
\ startup decoder for both baked and stripped DATA. Its instructions are pinned
\ (test/gate-aot-image.f CHECK-VGET) and exercised by every build.
require lib/test.f
require lib/memory.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f

package AOT-CELL-VALUE-TEST
using AOT-WINDOW
using IMAGE-CELLS

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

: BOUNDED ( -- )
   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED {: p:ptr cap:n :}
   $7F p cap + 1- c!
   s" a one-byte value at the last readable byte needs no tenth-byte read" T-LABEL
   p cap + 1- 1 CELL-V@ 1 T= $7F T=
   s" empty and negative extents read no bytes" T-LABEL
   p cap + 0 CELL-V@ 0 T= 0 T=
   p cap + -1 CELL-V@ 0 T= 0 T=
   p cap MEM-RELEASE-GUARDED ;

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

create FLAT 129 allot
: GROUPS ( -- )
   129 0 ?do 0 FLAT i + c! loop
   s" empty DATA has no map or stored group" T-LABEL
   FLAT 0 BM-COMPACT
   CBM-GROUPS @ 0 T= CBM-LEN 0 T=
   $81 FLAT c! 1 FLAT 128 + c!
   s" the absent middle group is omitted and the last group is padded" T-LABEL
   FLAT 129 BM-COMPACT
   CBM-GROUPS @ 3 T= CBM-STORED @ 128 T= CBM-LEN 129 T=
   CBM-BUF c@ 5 T=
   CBM-BUF 1+ c@ $81 T=
   CBM-BUF 65 + c@ 1 T=
   129 2 ?do i 65 <> if CBM-BUF i + c@ 0 T= then loop ;

8195 constant RAW-SIZE
create RAW RAW-SIZE allot
create RESTORED RAW-SIZE allot
create PACKED 11000 allot
variable PACKED-U
variable VALUES-OFF

: PACK ( -- )
   RAW RAW-SIZE FLAT IMAGE-CELLS:BITMAP! {: bm:n values:n :}
   FLAT bm PACKED 8 + IMAGE-CELLS:BM! {: groups:n stored:n :}
   groups PACKED IMAGE-CELLS:U32!
   stored PACKED 4 + IMAGE-CELLS:U32!
   8 groups IMAGE-CELLS:PMAP-BYTES + stored + VALUES-OFF !
   RAW RAW-SIZE PACKED VALUES-OFF @ + IMAGE-CELLS:VALUES!
   dup values T= VALUES-OFF @ + PACKED-U ! ;

: UNPACK ( n -- n )
   PACKED swap RESTORED RAW-SIZE IMAGE-CELLS:READ ;

: DATA-ROUNDTRIP ( -- )
   RAW-SIZE 0 ?do 0 RAW i + c! 0 RESTORED i + c! loop
   $7F RAW c!
   $80 RAW 4088 + CELL-VIEW !
   -1 RAW 4096 + CELL-VIEW !
   $7F RAW RAW-SIZE + 1- c!
   PACK
   s" grouped DATA round-trips page edges, whole cells and a partial tail" T-LABEL
   PACKED-U @ 218 T=
   PACKED-U @ UNPACK PACKED-U @ T=
   RAW RAW-SIZE RESTORED RAW-SIZE STR= TTRUE
   s" every truncated prefix refuses without reading past its extent" T-LABEL
   PACKED-U @ 0 ?do i UNPACK 0 T= loop
   s" group count and stored bytes must agree with the decoded extent" T-LABEL
   4 PACKED IMAGE-CELLS:U32! PACKED-U @ UNPACK 0 T=
   3 PACKED IMAGE-CELLS:U32!
   191 PACKED 4 + IMAGE-CELLS:U32! PACKED-U @ UNPACK 0 T=
   192 PACKED 4 + IMAGE-CELLS:U32!
   s" unused presence bits are zero" T-LABEL
   $87 PACKED 8 + c! PACKED-U @ UNPACK 0 T=
   7 PACKED 8 + c!
   s" a present cell cannot encode zero" T-LABEL
   0 PACKED VALUES-OFF @ + c! PACKED-U @ UNPACK 0 T=
   $7F PACKED VALUES-OFF @ + c!
   s" a partial last cell cannot store beyond the decoded extent" T-LABEL
   $1000000 PACKED PACKED-U @ + 4 - CELL-V! 4 T=
   PACKED-U @ UNPACK 0 T= ;

: RUN ( -- )
   T-RESET
   WIDTHS  TRUNCATED  EMPTY  PADDED  OVERLONG  TOO-WIDE  GRID GROUPS BOUNDED
   DATA-ROUNDTRIP
   T-REPORT ;

RUN
;using
;using
;package
