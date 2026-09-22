\ Exact ordered rows survive indexed collisions, growth and owner invalidation.
\ Tier 1 first: the rows read here are what a quotation store registers under
\ the optimizing backend, which also compiles the registrar required below.
1 set-tier

require lib/test.f
require src/habu/address-cells.f

package ADDRESS-CELL-INDEX
using ADDRESS-CELLS
using SNAP-RELOC

40000 constant EXTRA
EXTRA 2 / constant HALF
variable FIRST-OFF
variable BASE-N
DYNAMIC-BUFFER SAVED-STORAGE n
create BYTE-SLOTS 16 allot

: COUNT ( -- n ) LIVE-SPAN nip ;
: INDEX@ ( -- n ) data-base INDEX-CELL + @ ;
: LOCK@ ( -- n ) data-base LOCK-CELL + @ ;
: SAVED ( -- ptr n ) 0 SAVED-STORAGE ;
: SLOT ( n -- ptr n ) cells FIRST-OFF @ + data-base + ;
\ Only odd test slots store quotations; the shared buffer exposes raw cells.
TRUSTED: QUOT-SLOT ( ptr n -- ptr [ -- ] ) ;
: EMPTY ( -- ) ;

: MARK ( n -- ) {: row:n :}
   row 1 and 0<> if ['] EMPTY row SLOT QUOT-SLOT xt!
   else row SLOT ptr-cell-mark then ;
: RAW ( n -- n ) {: row:n :}
   row SLOT data-base -
   row 1 and 0= if XTCELL-DATA-TAG or then ;

: BYTE-KEYS ( -- )
   COUNT {: before:n :}
   BYTE-SLOTS ptr-cell-mark
   BYTE-SLOTS 1+ ptr-cell-mark
   BYTE-SLOTS 2 + ptr-cell-mark
   COUNT before 3 + T=
   3 0 ?do
      before i + ROW@
      BYTE-SLOTS i + data-base - XTCELL-DATA-TAG or T=
      BYTE-SLOTS i + ptr-cell-mark
   loop
   COUNT before 3 + T= ;

: PREFIX ( -- )
   BASE-N @ 0 ?do i ROW@ SAVED i cells + @ T= loop ;
: FORWARD ( n -- )
   0 ?do BASE-N @ i + ROW@ i RAW T= loop ;
: REVERSED ( -- )
   PREFIX HALF FORWARD
   HALF 0 ?do
      BASE-N @ HALF + i + ROW@ EXTRA 1- i - RAW T=
   loop ;

: PREPARE ( -- )
   COUNT dup BASE-N ! SAVED-STORAGE-RESERVE
   here data-base - 1+ FIRST-OFF !
   FIRST-OFF @ 7 and 0<> TTRUE
   EXTRA cells 8 + allot
   BASE-N @ 0 ?do
      i ROW@ dup SAVED i cells + !
      XTCELL-OFF-MASK and FIRST-OFF @ < TTRUE
   loop ;

: GROW ( -- )
   EXTRA 0 ?do i MARK loop
   COUNT BASE-N @ EXTRA + T=
   INDEX@ 0<> TTRUE LOCK@ 0 T=
   PREFIX EXTRA FORWARD
   EXTRA 0 ?do i MARK loop
   COUNT BASE-N @ EXTRA + T=
   PREFIX EXTRA FORWARD ;

: REWIND ( -- )
   \ A DATA-backed vector must secure its full capacity before the cut.
   PERSIST INDEX@ 0 T=
   0 MARK INDEX@ 0<> TTRUE
   FIRST-OFF @ HALF cells + KEEP-BELOW
   INDEX@ 0 T= LOCK@ 0 T=
   COUNT BASE-N @ HALF + T=
   PREFIX HALF FORWARD
   FIRST-OFF @ HALF cells + here data-base - - allot
   HALF cells 8 + allot
   HALF 0 ?do EXTRA 1- i - MARK loop
   COUNT BASE-N @ EXTRA + T=
   REVERSED
   \ The same count now has a different suffix order. Every lookup must use
   \ the rebuilt ordinals, including exact-offset collisions and both kinds.
   EXTRA 0 ?do i MARK loop
   COUNT BASE-N @ EXTRA + T=
   REVERSED ;

: SAME-COUNT ( -- )
   here data-base - KEEP-BELOW
   INDEX@ 0 T= LOCK@ 0 T=
   COUNT BASE-N @ EXTRA + T=
   EXTRA 1- MARK INDEX@ 0<> TTRUE
   REVERSED
   PERSIST INDEX@ 0 T=
   EXTRA 1- MARK INDEX@ 0<> TTRUE
   here data-base - {: before:n :}
   PERSIST INDEX@ 0 T= LOCK@ 0 T=
   here data-base - before T=
   COUNT BASE-N @ EXTRA + T=
   REVERSED ;

: RANGE-END ( -- )
   COUNT {: before:n :}
   data-base XTCELL-OFF-MAX + dup ptr-cell-mark 1- ptr-cell-mark
   COUNT before 2 + T=
   before ROW@ XTCELL-OFF-MAX XTCELL-DATA-TAG or T=
   before 1+ ROW@ XTCELL-OFF-MAX 1- XTCELL-DATA-TAG or T=
   data-base XTCELL-OFF-MAX + dup ptr-cell-mark 1- ptr-cell-mark
   COUNT before 2 + T= ;

: RUN ( -- )
   T-RESET
   CURRENT? TTRUE
   BYTE-KEYS PREPARE GROW REWIND SAME-COUNT RANGE-END
   SAVED-STORAGE-RELEASE
   T-REPORT ;

RUN
;using
;using
;package
