\ Registry persistence preserves complete DATA allocations and copies growth.
require lib/test.f

package REGISTRY-PERSIST-TEST

create BUFFER $20 allot
PTR-VARIABLE STORE

\ Private access to the checker-owned persistence seam.
TRUSTED: DATA-SPAN? ( ptr u8 n -- bool ) REG-DATA-SPAN? ;
TRUSTED: PERSIST ( n n -- n bool ) STORE -rot REG-PERSIST-MOVE ;
TRUSTED: ALLOC ( n -- ptr u8 ) ARENA-ALLOC ;
: RELEASE ( ptr u8 n -- n ) munmap ;
TRUSTED: EFFECTS ( -- ptr u8 n ) USIGS USIGS-SNAPSHOT-SIZE ;
TRUSTED: PERSIST-EFFECTS ( -- ) USIGS-SNAPSHOT-PERSIST ;
TRUSTED: GROW-EFFECTS ( -- ) USIGS-CAP-U @ 1+ USIGS-ENSURE ;
TRUSTED: CONTROLS ( -- ptr u8 n ) NORETS NORET-END @ CELL + ;
TRUSTED: PERSIST-CONTROLS ( -- ) NORET-SNAPSHOT-PERSIST ;
TRUSTED: GROW-CONTROLS ( -- ) NORET-CAP-U @ 1+ NORET-ENSURE ;


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

\ Exercise both real stores, including all live bytes and their terminators.
\ Persistence must be stable again after a subsequent runtime allocation.
: EFFECT-CAPTURES ( -- )
   s" unchanged effect capture reuses DATA; runtime growth remains complete" T-LABEL
   EFFECTS {: old:ptr used:n :}
   PERSIST-EFFECTS
   EFFECTS {: saved:ptr size:n :}
   old used saved size CORE-STR= TTRUE
   here {: end:ptr :}
   PERSIST-EFFECTS
   EFFECTS used T= saved = TTRUE
   here end = TTRUE
   GROW-EFFECTS
   EFFECTS {: grown:ptr grown-size:n :}
   grown saved <> TTRUE
   saved size grown grown-size CORE-STR= TTRUE
   PERSIST-EFFECTS
   EFFECTS {: final:ptr final-size:n :}
   grown grown-size final final-size CORE-STR= TTRUE
   here {: final-end:ptr :}
   PERSIST-EFFECTS
   EFFECTS used T= final = TTRUE
   here final-end = TTRUE ;

: CONTROL-CAPTURES ( -- )
   s" unchanged control capture reuses DATA; runtime growth remains complete" T-LABEL
   CONTROLS {: old:ptr used:n :}
   PERSIST-CONTROLS
   CONTROLS {: saved:ptr size:n :}
   old used saved size CORE-STR= TTRUE
   here {: end:ptr :}
   PERSIST-CONTROLS
   CONTROLS used T= saved = TTRUE
   here end = TTRUE
   GROW-CONTROLS
   CONTROLS {: grown:ptr grown-size:n :}
   grown saved <> TTRUE
   saved size grown grown-size CORE-STR= TTRUE
   PERSIST-CONTROLS
   CONTROLS {: final:ptr final-size:n :}
   grown grown-size final final-size CORE-STR= TTRUE
   here {: final-end:ptr :}
   PERSIST-CONTROLS
   CONTROLS used T= final = TTRUE
   here final-end = TTRUE ;


: RUN ( -- )
   T-RESET
   SPANS T-NEXT
   RETAIN T-NEXT
   GROW T-NEXT
   EFFECT-CAPTURES T-NEXT
   CONTROL-CAPTURES T-NEXT
   T-REPORT ;

RUN
;package
