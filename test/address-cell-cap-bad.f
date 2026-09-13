\ The shared address-cell registrar refuses the first declaration past its cap.

require lib/errors.f

package ADDRESS-CELL-CAP-BAD

SNAP-RELOC:XTCELL-CAP 1+ constant SLOT-N
create SLOTS SLOT-N cells allot

: ROWS ( -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: GO ( -- )
   SNAP-RELOC:XTCELL-CAP ROWS - {: remaining:n :}
   remaining 0 <= if s" address-cell-cap: no free rows" 76 die then
   remaining 0 ?do SLOTS i cells + ptr-cell-mark loop
   ROWS SNAP-RELOC:XTCELL-CAP <> if
      s" address-cell-cap: advertised limit was not reached" 76 die
   then
   \ A duplicate is still legal at capacity and must not consume another row.
   SLOTS ptr-cell-mark
   ROWS SNAP-RELOC:XTCELL-CAP <> if
      s" address-cell-cap: duplicate changed the row count" 76 die
   then
   s" ADDRESS-CELL-CAP-ARMED" type cr
   SLOTS remaining cells + ptr-cell-mark
   s" MARKED" type cr ;

GO

;package
