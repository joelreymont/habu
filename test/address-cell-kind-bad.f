\ address-cell-kind-bad.f - one persisted cell cannot have two address kinds.

require src/lib/errors.f

package ADDRESS-CELL-KIND-BAD

PERSISTED-PTR-VARIABLE SLOT

: TARGET ( -- n ) 4711 ;

\ PERSISTED-PTR-VARIABLE declared SLOT as DATA; xt! must refuse the contradictory XT kind
\ before storing anything into the cell.
\ The contradictory kind is forged onto SLOT's cell by a private cast.
CAST: >XT-CELL ( ptr ptr n -- ptr [ -- n ] )
: SLOT-AS-XT ( -- ptr [ -- n ] ) SLOT >XT-CELL ;

: GO ( -- )
   s" ADDRESS-CELL-KIND-ARMED" type cr
   [: TARGET ;] SLOT-AS-XT xt!
   s" STORED" type cr ;

GO

;package
