\ hb-build preseed supplies hostile tags; every pointer is allocated normally.
package NATIVE-FETCH-SUBJECT
public
ENUM shade dark light ;ENUM
SUMTYPE inner 0
   VARIANT color shade ;VARIANT
   VARIANT number n ;VARIANT
;SUMTYPE
SUMTYPE outer 0
   VARIANT tree inner ;VARIANT
   VARIANT number n ;VARIANT
;SUMTYPE
SUMTYPE empty 0
   VARIANT first ;VARIANT
   VARIANT second ;VARIANT
;SUMTYPE
private

1 LAYOUT-BUFFER SCALAR shade
1 LAYOUT-BUFFER EMPTY empty
1 LAYOUT-BUFFER NESTED outer

public

: SCALAR-FETCH ( shade -- )
   0 SCALAR ! 0 SCALAR @ drop s" typed fetch returned" type cr ;

: EMPTY-FETCH ( empty -- )
   0 EMPTY ! 0 EMPTY @ drop s" typed fetch returned" type cr ;

: NESTED-FETCH ( outer -- )
   0 NESTED ! 0 NESTED @ drop s" typed fetch returned" type cr ;

;package

: MAIN ( -- ) ;
