\ layout-valid-product-bad.f — nested product enum tag is validated on fetch.

package LAYOUT-VALID-PRODUCT

ENUM lvp-color red green ;ENUM
PRODUCT lvp-pixel 0
   FIELD color lvp-color
   FIELD count n
;PRODUCT
1 LAYOUT-BUFFER BUF lvp-pixel

TRUSTED: RAW ( ptr lvp-pixel -- ptr n ) ;

: GET ( -- lvp-pixel )
   0 BUF @ ;

: CORRUPT ( -- )
   2 0 BUF RAW ! ;

: GO ( -- )
   CORRUPT
   s" LAYOUT-VALID-ARMED" type cr
   GET drop ;

GO

end-package
