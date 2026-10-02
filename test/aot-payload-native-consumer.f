\ The partial seed publishes the imported code and effects before this file.
package PAYLOAD-NATIVE-CONSUMER

: EQ ( n n -- ) <> if 79 throw then ;


: TRUE! ( bool -- ) 0= if 79 throw then ;


: BUMP-TWICE ( n -- n ) PAYLOAD-NATIVE:BUMP PAYLOAD-NATIVE:BUMP ;


: SUM-PAIR ( n n -- n )
   PAYLOAD--NATIVE-PAIR:MAKE PAYLOAD-NATIVE:PAIR-SUM ;

: ASSERTED-CALL ( n -- n ) PAYLOAD-NATIVE:ASSERTED ;

: VIEW-COPY-CALL ( read-view<p,q,a> -- read-view<p,q,a> )
   PAYLOAD-NATIVE:VIEW-COPY drop ;
: VIEW-SUM-CALL ( read-view<p,q,a> -- read-view<p,q,a> )
   PAYLOAD-NATIVE:VIEW-WRAP PAYLOAD-NATIVE:VIEW-UNWRAP ;


: RUN ( -- )
   41 PAYLOAD-NATIVE:BUMP 42 EQ
   40 BUMP-TWICE 42 EQ
   17 25 SUM-PAIR 42 EQ
   42 ASSERTED-CALL 42 EQ
   \ The first reference asks the lazy intake for the row before its refusal.
   s" ABI-REFUSED ( n -- n ) PAYLOAD-NATIVE:ABI-ONLY" CHECK! 0= TRUE!
   s" PAYLOAD-NATIVE:ABI-ONLY" EFFECT-QUERY TRUE!
   EFFECT-DIN-CELLS 1 EQ EFFECT-DOUT-CELLS 1 EQ
   s" PAYLOAD-NATIVE:ABI-ONLY" CHECKER-RESOLVES? 0= TRUE!
   s" PAYLOAD-NATIVE:PAIR-SUM" EFFECT-QUERY TRUE!
   EFFECT-DIN-CELLS 2 EQ
   EFFECT-DOUT-CELLS 1 EQ
   s" WRONG ( ptr u8 -- ptr u8 ) PAYLOAD-NATIVE:BUMP" CHECK! 0= TRUE!
   s" WRONG-PAIR ( n -- n ) PAYLOAD-NATIVE:PAIR-SUM" CHECK! 0= TRUE!
   s" an imported generic quotation setter accepts a concrete callback" type cr
   s" QUOTE-OK ( [ n -- n ] -- ) PAYLOAD-NATIVE:QUOTE-STORE" CHECK! -1 EQ
   s" an imported generic quotation setter retains scope restrictions" type cr
   s" QUOTE-BAD ( [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- ) PAYLOAD-NATIVE:QUOTE-STORE" CHECK! 0= TRUE!
   s" imported views preserve their element type" type cr
   s" VIEW-BAD ( read-view<p,q,u8> -- read-view<p,q,n> ) PAYLOAD-NATIVE:VIEW-COPY drop" CHECK! 0= TRUE!
   s" native graph fresh consumer: ok" type cr ;

RUN
;package
