\ address-cell-kind-bad.f - one persisted cell cannot have two address kinds.

require lib/errors.f

package ADDRESS-CELL-KIND-BAD

PERSISTED-PTR-VARIABLE SLOT

: TARGET ( -- n ) 4711 ;

\ PERSISTED-PTR-VARIABLE declared SLOT as DATA; xt! must refuse the contradictory XT kind
\ before storing anything into the cell.
\ The contradictory kind is forged onto SLOT's raw cell view.
CAST: >XT-CELL ( ptr n -- ptr [ -- n ] )
: SLOT-AS-XT ( -- ptr [ -- n ] ) SLOT BYTE-VIEW CELL-VIEW >XT-CELL ;

: GO ( -- )
   s" ADDRESS-CELL-KIND-ARMED" type cr
   [: TARGET ;] SLOT-AS-XT xt!
   s" STORED" type cr ;

GO

;package
