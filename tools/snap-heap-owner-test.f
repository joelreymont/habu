require lib/test.f
require tools/snap-heap-owner.f

package SNAP-HEAP-OWNER
public
variable OWNER-CELL
16 BUFFER: OWNER-BUFFER
: OWNER-USE ( -- ptr n ) OWNER-CELL ;
private

: OWNER-CASE ( -- )
   T-RESET
   s" a created cell and buffer remain heap owners with compact DATA carriers" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-CELL" XREF-FIND CREATED? TTRUE
   s" SNAP-HEAP-OWNER:OWNER-BUFFER" XREF-FIND CREATED? TTRUE
   s" the shared decoder recovers the cell's actual address" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-CELL" XREF-FIND CHAIN-VALUE XREF-N>REC OWNER-CELL = TTRUE
   s" an ordinary word using the cell is not its owner" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-USE" XREF-FIND CREATED? TFALSE
   T-REPORT ;
OWNER-CASE
;package
