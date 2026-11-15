\ address-cell-kind-bad.f - one persisted cell cannot have two address kinds.

require src/lib/error.f

package ADDRESS-CELL-KIND-BAD

PERSISTED-PTR-VARIABLE SLOT

: TARGET ( -- n ) 4711 ;

\ PERSISTED-PTR-VARIABLE declared SLOT as DATA; xt! must refuse the contradictory XT kind
\ before storing anything into the cell.
CAST: >XT-CELL ( ^^n -- ^[ -- n ] )
: SLOT-AS-XT ( -- ^[ -- n ] ) SLOT >XT-CELL ;

: GO ( -- )
   s" ADDRESS-CELL-KIND-ARMED" type cr
   [: TARGET ;] SLOT-AS-XT xt!
   s" STORED" type cr ;

GO

;package
