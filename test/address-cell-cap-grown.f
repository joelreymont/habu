\ Run on the first candidate built after a layout change. The emitted registrar
\ must admit 40000 total rows, including the retained runtime's declarations.
require lib/test.f

package ADDRESS-CELL-CAP-GROWN

40000 constant ROW-N
create SLOTS ROW-N cells allot

: ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: RUN ( -- )
   T-RESET
   ROW-N ROWS - {: remaining:n :}
   remaining 0 <= if s" address-cell-cap-grown: no free test rows" 76 die then
   remaining 0 ?do SLOTS i cells + ptr-cell-mark loop
   ROWS ROW-N T=
   SLOTS ptr-cell-mark
   ROWS ROW-N T=
   T-REPORT ;

RUN
;package
