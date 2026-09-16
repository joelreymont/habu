\ Per-site row instantiation through the production native compiler.
require lib/test.f
require lib/memory.f

package GENERIC-CALL-TEST

: APPLY ( R [ R -- S ] -- S ) execute ;
: APPLY-SAME ( R [ R -- R ] -- R ) execute ;
: APPLY-TO ( R [ R n -- S ] n -- S ) swap execute ;
: KEEP ( a -- a ) ;

: CONSUME ( n n -- ) 2drop ;
: CONSUMING ( n n -- ) [: CONSUME ;] APPLY ;
: PRODUCING ( -- n n n ) [: 17 29 43 ;] APPLY ;
: REPLACING ( n -- n ) [: 7 * 3 + ;] APPLY-SAME ;
: PREFIX ( n n -- n n ) [: 7 * 3 + ;] APPLY-SAME ;
: NON-TOP ( n n -- n ) [: 11 * + ;] swap APPLY-TO ;
: FORWARDED ( n n -- n ) [: 11 * + ;] KEEP APPLY ;

: BYTE-CONSUMER ( n ptr u8 NUM:alloc-byte-len -- )
   2drop drop ;

: WITH-BYTES-CONSUMING ( n -- ) {: bytes:n :}
   bytes bytes MEM:BYTES-ALLOC-LEN [: BYTE-CONSUMER ;] MEM:WITH-BYTES ;

: WITH-BYTES-RESULTS ( n -- n n n )
   MEM:BYTES-ALLOC-LEN [: 2drop 17 29 43 ;] MEM:WITH-BYTES ;

: WITH-BYTES-EARLY ( n -- )
   dup 0= if drop exit then
   {: bytes:n :}
   bytes bytes MEM:BYTES-ALLOC-LEN [: BYTE-CONSUMER ;] MEM:WITH-BYTES ;

: RETURNING-QUOT ( -- [ n n -- n ] ) [: 11 * + ;] KEEP ;
: RETURNED ( n n -- n ) RETURNING-QUOT execute ;

: RUN ( -- )
   T-RESET
   5 9 CONSUMING
   PRODUCING 43 T= 29 T= 17 T=
   5 REPLACING 38 T=
   91 5 PREFIX 38 T= 91 T=
   5 9 NON-TOP 104 T=
   5 9 FORWARDED 104 T=
   16 WITH-BYTES-CONSUMING
   16 WITH-BYTES-RESULTS 43 T= 29 T= 17 T=
   0 WITH-BYTES-EARLY
   16 WITH-BYTES-EARLY
   5 9 RETURNED 104 T=
   T-REPORT ;

RUN
;package
