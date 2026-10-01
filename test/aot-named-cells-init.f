\ Load after WINDOW-CLOSE: only the resulting stored cells are captured.
package NAMED-CELLS-INIT
\ This fixture needs a null serialized XT, which has no checked quotation
\ constructor, so the integer 0 is cast to one; CHECK reads it but never calls it.
CAST: >XT ( n -- [ n -- n ] )
: ZERO-XT ( -- [ n -- n ] ) 0 >XT ;
: RUN ( -- )
   ['] 0<> NAMED-CELLS-WINDOW:GLOBAL-SLOT xt!
   ['] CODE-RECLAIM:FLOOR-FROM NAMED-CELLS-WINDOW:PUBLIC-SLOT xt!
   ['] NAMED-CELLS-WINDOW:LOCAL NAMED-CELLS-WINDOW:LOCAL-SLOT xt!
   ZERO-XT NAMED-CELLS-WINDOW:NULL-SLOT xt!
   NAMED-CELLS-WINDOW:VALUE NAMED-CELLS-WINDOW:DATA-SLOT !
   $5A5A NAMED-CELLS-WINDOW:VALUE ! ;
RUN
;package
