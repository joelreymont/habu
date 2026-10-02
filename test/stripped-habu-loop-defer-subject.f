\ A defer the interpret loop written in Habu compiled (src/habu/interpret.f), in
\ a stripped image: the image calls it, re-aims it with `is` and calls it again.
\ The loop keeps none of its lookup state past OUTER:INTERPRET, so the image
\ holds no dictionary pointer of the loop's that the capture cannot declare.
require src/habu/interpret.f

package STRIPPED-HABU-LOOP

s" defer ACTION ( n -- n ) : INSTALL ( [ n -- n ] -- ) is ACTION ;" OUTER:INTERPRET
s" : CALL ( n -- n ) ACTION ; : TRIPLE ( n -- n ) 3 * ;" OUTER:INTERPRET
s" : FIRST ( -- ) [: TRIPLE ;] INSTALL ; FIRST" OUTER:INTERPRET

: INC ( n -- n ) 1+ ;

public
: RUN ( -- )
   17 CALL . cr
   ['] INC INSTALL
   17 CALL . cr ;

;package

: MAIN ( -- ) STRIPPED-HABU-LOOP:RUN ;
