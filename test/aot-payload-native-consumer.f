\ The partial seed publishes the imported code and effects before this file.
package PAYLOAD-NATIVE-CONSUMER

: EQ ( n n -- ) <> if 79 throw then ;


: TRUE! ( bool -- ) 0= if 79 throw then ;


: BUMP-TWICE ( n -- n ) PAYLOAD-NATIVE:BUMP PAYLOAD-NATIVE:BUMP ;


: SUM-PAIR ( n n -- n )
   PAYLOAD--NATIVE-PAIR:MAKE PAYLOAD-NATIVE:PAIR-SUM ;


: RUN ( -- )
   41 PAYLOAD-NATIVE:BUMP 42 EQ
   40 BUMP-TWICE 42 EQ
   17 25 SUM-PAIR 42 EQ
   s" PAYLOAD-NATIVE:PAIR-SUM" EFFECT-QUERY TRUE!
   EFFECT-DIN-CELLS 2 EQ
   EFFECT-DOUT-CELLS 1 EQ
   s" WRONG ( ptr u8 -- ptr u8 ) PAYLOAD-NATIVE:BUMP" CHECK! 0= TRUE!
   s" WRONG-PAIR ( n -- n ) PAYLOAD-NATIVE:PAIR-SUM" CHECK! 0= TRUE!
   s" native graph fresh consumer: ok" type cr ;

RUN
;package
