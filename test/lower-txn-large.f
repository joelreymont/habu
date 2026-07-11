\ lower-txn-large.f - dynamic lowering transaction capacity regression.

require lib/errors.f
require src/core/bytes.f

package LOWER-TXN-LARGE-TEST

$4000 constant SRC-CAP
64 constant FETCH-N

create SRC SRC-CAP allot
variable SRC-U

: ROOM ( n -- ) {: add:n :}
   add 0 < if E-STR-BOUNDS throw then
   SRC-U @ add + SRC-CAP > if E-STR-CAPACITY throw then ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u ROOM
   a SRC SRC-U @ + u BYTE-COPY
   SRC-U @ u + SRC-U ! ;

: LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1+ ROOM
   a u APPEND
   $A SRC SRC-U @ + c!
   SRC-U @ 1+ SRC-U ! ;

: TYPES ( -- )
   s" package LTXN-LARGE" LINE
   s" public" LINE
   s" $10000 constant OLD-CERT-CAP" LINE
   s" 76 constant CERT-FAIL-RC" LINE
   s" SUMTYPE level-zero 0 VARIANT value n ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-one 0 VARIANT value LTXN-LARGE:level-zero ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-two 0 VARIANT value LTXN-LARGE:level-one ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-three 0 VARIANT value LTXN-LARGE:level-two ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-four 0 VARIANT value LTXN-LARGE:level-three ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-five 0 VARIANT value LTXN-LARGE:level-four ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-six 0 VARIANT value LTXN-LARGE:level-five ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-seven 0 VARIANT value LTXN-LARGE:level-six ;VARIANT ;SUMTYPE" LINE
   s" SUMTYPE level-eight 0 VARIANT value LTXN-LARGE:level-seven ;VARIANT ;SUMTYPE" LINE
   s" 1 LAYOUT-BUFFER MEM level-eight" LINE
   s" private" LINE ;

: MAKERS ( -- )
   s" : MAKE-0 ( n -- level-zero ) construct level-zero value ;" LINE
   s" : MAKE-1 ( n -- level-one ) MAKE-0 construct level-one value ;" LINE
   s" : MAKE-2 ( n -- level-two ) MAKE-1 construct level-two value ;" LINE
   s" : MAKE-3 ( n -- level-three ) MAKE-2 construct level-three value ;" LINE
   s" : MAKE-4 ( n -- level-four ) MAKE-3 construct level-four value ;" LINE
   s" : MAKE-5 ( n -- level-five ) MAKE-4 construct level-five value ;" LINE
   s" : MAKE-6 ( n -- level-six ) MAKE-5 construct level-six value ;" LINE
   s" : MAKE-7 ( n -- level-seven ) MAKE-6 construct level-seven value ;" LINE
   s" : MAKE-8 ( n -- level-eight ) MAKE-7 construct level-eight value ;" LINE ;

: SCAN ( -- )
   s" : SCAN ( ptr level-eight -- level-eight )" LINE
   \ Each nested fetch carries 136 validation cells; 65 fetches exceed 64 KiB.
   FETCH-N 0 ?do s"   dup @ drop" LINE loop
   s"   @ ;" LINE ;

: ASSERT-CERT ( -- )
   s" LOWER-CERT:CELL-COUNT constant SCAN-CERT-N" LINE
   s" : ASSERT-CERT ( -- )" LINE
   s\"   SCAN-CERT-N cells OLD-CERT-CAP <= if s\" lower-txn-large: certificate did not exceed old cap\" CERT-FAIL-RC die then ;" LINE
   s" ASSERT-CERT" LINE ;

: TAIL ( -- )
   s" : INIT ( -- ) 7 MAKE-8 0 MEM ! ;" LINE
   s" public" LINE
   s" : RUN ( -- ) INIT 0 MEM SCAN drop ;" LINE
   s" private" LINE
   s" end-package" LINE
   s" LTXN-LARGE:RUN" LINE
   s\" s\" lower-txn-large: ok\" type cr" LINE ;

: SOURCE$ ( -- ptr u8 n )
   0 SRC-U !
   TYPES
   MAKERS
   SCAN
   ASSERT-CERT
   TAIL
   SRC SRC-U @ ;

public

: RUN ( -- )
   SOURCE$ INCLUDE-EVALUATE ;

end-package

LOWER-TXN-LARGE-TEST:RUN
