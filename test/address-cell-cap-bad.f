\ The shared address-cell registrar refuses the first declaration past its cap.

require lib/errors.f

package ADDRESS-CELL-CAP-BAD

SNAP-RELOC:XTCELL-CAP 1+ constant SLOT-N
create SLOTS SLOT-N cells allot

: GO ( -- )
   s" ADDRESS-CELL-CAP-ARMED" type cr
   SLOT-N 0 ?do SLOTS i cells + ptr-cell-mark loop
   s" MARKED" type cr ;

GO

;package
