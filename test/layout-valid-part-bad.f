\ layout-valid-part-bad.f — a part NFETCH-CHECK with a BAD-TAG that returns
\ does not replace the trap an enum fetch of an out-of-domain tag calls.

package LAYOUT-VALID-PART

package NFETCH-CHECK
public
: BAD-TAG ( n n -- ) 2drop ;
;package

ENUM lv-color red green ;ENUM
TYPED-VARIABLE SLOT lv-color

CAST: N>COLOR ( n -- lv-color )

: GET ( ptr lv-color -- lv-color ) @ ;

: GO ( -- )
   7 N>COLOR SLOT !
   s" LAYOUT-VALID-PART-ARMED" type cr
   SLOT GET drop ;

GO

;package
