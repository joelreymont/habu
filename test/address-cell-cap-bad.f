\ Reject an overflowing header capacity before multiplying or reading a row.
require lib/errors.f
require src/habu/address-cells.f

package ADDRESS-CELL-CAP-BAD
create SLOT 0 ,
: GO ( -- )
   ADDRESS-CELLS:MAX-ROWS 1+
   data-base SNAP-RELOC:XTCELL-N-CELL + ADDRESS-CELLS:CAP-FIELD + !
   s" ADDRESS-CELL-CAP-ARMED" type cr
   SLOT ptr-cell-mark
   s" MARKED" type cr ;
GO
;package
