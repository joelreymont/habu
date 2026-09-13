\ Grow past the former fixed band without losing either old or new declarations.
require lib/test.f
require src/habu/address-cells.f

package ADDRESS-CELL-CAP-GROWN

75900 constant ROW-N
create SLOTS ROW-N cells allot
create SAVED ROW-N cells allot
variable BASE-N
variable CUT

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
: HEADER@ ( n -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + + @ ;
: EXPECT-ROWS ( -- )
   BASE-N @ 0 ?do i ADDRESS-CELLS:ROW@ SAVED i cells + @ T= loop
   ROW-N BASE-N @ - 0 ?do
      BASE-N @ i + ADDRESS-CELLS:ROW@
      SLOTS i cells + data-base - SNAP-RELOC:XTCELL-DATA-TAG or T=
   loop ;

: RUN ( -- )
   T-RESET
   ADDRESS-CELLS:CURRENT? TTRUE
   addr-cells-abi ADDRESS-CELLS:ABI-VERSION T=
   ROWS dup BASE-N !
   ROW-N >= if s" address-cell-growth: fixture has no new rows" 76 die then
   BASE-N @ 0 ?do i ADDRESS-CELLS:ROW@ SAVED i cells + ! loop
   ROW-N BASE-N @ - 0 ?do SLOTS i cells + ptr-cell-mark loop
   ROWS ROW-N T=
   ADDRESS-CELLS:CAP-FIELD HEADER@ ROW-N >= TTRUE
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   EXPECT-ROWS
   SLOTS ptr-cell-mark
   SLOTS ROW-N BASE-N @ - 1- cells + ptr-cell-mark
   ROWS ROW-N T=
   here data-base - CUT !
   ADDRESS-CELLS:PERSIST
   ADDRESS-CELLS:MODE-FIELD HEADER@ 0 T=
   EXPECT-ROWS
   here ADDRESS-CELLS:BASE-FIELD HEADER@
   ADDRESS-CELLS:PERSIST
   ADDRESS-CELLS:BASE-FIELD HEADER@ T= here = TTRUE
   EXPECT-ROWS
   CUT @ ADDRESS-CELLS:KEEP-BELOW
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   ROWS ROW-N T=
   CUT @ here data-base - - allot
   EXPECT-ROWS
   ADDRESS-CELLS:PERSIST
   EXPECT-ROWS
   \ Live rows fit below this cut; unused backing capacity crosses it.
   ADDRESS-CELLS:BASE-FIELD HEADER@ ROWS cells + CUT !
   CUT @ ADDRESS-CELLS:KEEP-BELOW
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   CUT @ here data-base - - allot
   EXPECT-ROWS
   ADDRESS-CELLS:PERSIST
   EXPECT-ROWS
   T-REPORT ;

RUN
;package
