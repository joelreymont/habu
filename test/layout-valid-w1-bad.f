\ layout-valid-w1-bad.f — W1 enum fetch rejects a forged tag before returning it.

package LAYOUT-VALID-W1

ENUM lv-color red green ;ENUM
1 LAYOUT-BUFFER BUF lv-color

TRUSTED: RAW ( ptr lv-color -- ptr n ) ;

: GET ( -- lv-color )
   0 BUF @ ;

: CORRUPT ( -- )
   2 0 BUF RAW ! ;

: GO ( -- )
   CORRUPT
   s" LAYOUT-VALID-ARMED" type cr
   GET drop ;

GO

;package
