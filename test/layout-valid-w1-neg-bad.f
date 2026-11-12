\ layout-valid-w1-neg-bad.f — W1 enum fetch rejects a forged negative tag.

package LAYOUT-VALID-W1-NEG

ENUM lv-color red green ;ENUM
1 LAYOUT-BUFFER BUF lv-color

CAST: RAW ( ptr lv-color -- ptr n )

: GET ( -- lv-color )
   0 BUF @ ;

: CORRUPT ( -- )
   -1 0 BUF RAW ! ;

: GO ( -- )
   CORRUPT
   s" LAYOUT-VALID-ARMED" type cr
   GET drop ;

GO

;package
