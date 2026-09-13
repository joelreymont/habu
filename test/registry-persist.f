\ Registry persistence preserves complete DATA allocations and copies growth.
require lib/test.f

package REGISTRY-PERSIST-TEST

create BUFFER $20 allot
PTR-VARIABLE STORE

\ Private access to the checker-owned persistence seam.
TRUSTED: DATA-SPAN? ( ptr u8 n -- bool ) REG-DATA-SPAN? ;
TRUSTED: PERSIST ( n n -- n bool ) STORE -rot REG-PERSIST-MOVE ;
TRUSTED: ALLOC ( n -- ptr u8 ) ARENA-ALLOC ;
TRUSTED: RELEASE ( ptr u8 n -- n ) munmap ;


: SPANS ( -- )
   s" a DATA allocation includes its complete capacity" T-LABEL
   BUFFER $20 DATA-SPAN? TTRUE
   here 0 DATA-SPAN? TTRUE
   here 1 DATA-SPAN? TFALSE
   BUFFER -1 DATA-SPAN? TFALSE
   BUFFER $7FFFFFFFFFFFFFFF DATA-SPAN? TFALSE
   data-base 1- 1 DATA-SPAN? TFALSE ;


: RETAIN ( -- )
   BUFFER STORE !
   here {: end:ptr :}
   8 $20 PERSIST TFALSE 0 T=
   STORE @ BUFFER = TTRUE
   here end = TTRUE ;


: GROW ( -- )
   $1000 ALLOC {: old:ptr :}
   old STORE !
   $12345678 old CELL-VIEW !
   $AB old $1F + c!
   old $1000 DATA-SPAN? TFALSE
   here {: start:ptr :}
   $20 $1000 PERSIST TTRUE start old - T=
   STORE @ start = TTRUE
   STORE @ CELL-VIEW @ $12345678 T=
   STORE @ $1F + c@ $AB T=
   here {: end:ptr :}
   $20 $20 PERSIST TFALSE 0 T=
   STORE @ start = TTRUE
   here end = TTRUE
   old $1000 RELEASE 0 T= ;


: RUN ( -- )
   T-RESET
   SPANS T-NEXT
   RETAIN T-NEXT
   GROW T-NEXT
   T-REPORT ;

RUN
;package
