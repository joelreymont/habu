\ array.f - checked cell-array helpers.

: A-CHECK-INDEX ( n n -- ) {: len ix :}
   len 0 < IF E-A-BOUNDS throw THEN
   ix 0 < IF E-A-BOUNDS throw THEN
   ix len >= IF E-A-BOUNDS throw THEN ;

: A-CHECK-RANGE ( n n n -- ) {: len start cnt :}
   len 0 < IF E-A-BOUNDS throw THEN
   start 0 < IF E-A-BOUNDS throw THEN
   cnt 0 < IF E-A-BOUNDS throw THEN
   start len > IF E-A-BOUNDS throw THEN
   cnt len start - > IF E-A-BOUNDS throw THEN ;

: A-CHECK-NONEMPTY ( n -- ) {: len :}
   len 0 < IF E-A-BOUNDS throw THEN
   len 0= IF E-A-EMPTY throw THEN ;

: A@ ( ptr a n n -- a ) {: arr:ptr len ix :}
   len ix A-CHECK-INDEX
   arr ix cells + @ ;

: A! ( a ptr a n n -- ) {: value arr:ptr len ix :}
   len ix A-CHECK-INDEX
   value arr ix cells + ! ;

: A+! ( n ptr a n n -- ) {: delta arr:ptr len ix :}
   arr len ix A@ delta +
   arr len ix A! ;

: A-SWAP ( ptr a n n n -- ) {: arr:ptr len ix jx :}
   arr len ix A@
   arr len jx A@
   arr len ix A!
   arr len jx A! ;

: LAST-INDEX ( n -- n ) {: len :}
   len A-CHECK-NONEMPTY
   len 1 - ;

: MIRROR-INDEX ( n n -- n ) {: len ix :}
   len ix A-CHECK-INDEX
   len 1 - ix - ;

: EVEN? ( n -- bool )
   1 and 0= ;

: A-SUM ( ptr n n -- n ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   0
   len 0 ?do
      arr i cells + @ +
   loop ;

: A-MIN ( ptr n n -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   arr @
   len 1 ?do
      arr i cells + @ min
   loop ;

: A-MAX ( ptr n n -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   arr @
   len 1 ?do
      arr i cells + @ max
   loop ;

: A-COUNT-EVEN ( ptr n n -- n ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   0
   len 0 ?do
      arr i cells + @ 2 mod 0= IF 1+ THEN
   loop ;

: A-ARGMAX ( ptr n n -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   0 arr @
   len 1 ?do
      arr i cells + @ over > IF
         2drop
         i arr i cells + @
      THEN
   loop
   drop ;

: A-REVERSE! ( ptr a n -- ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 2 / 0 ?do
      arr i cells + @
      arr len 1 - i - cells + @
      arr i cells + !
      arr len 1 - i - cells + !
   loop ;

: A-PREFIX-SUM! ( ptr n n -- ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 1 <= IF exit THEN
   len 1 ?do
      arr i 1 - cells + @
      arr i cells + @ +
      arr i cells + !
   loop ;

: A-RUNMAX! ( ptr n n -- ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 1 <= IF exit THEN
   len 1 ?do
      arr i 1 - cells + @
      arr i cells + @ max
      arr i cells + !
   loop ;

: A-FILL! ( a ptr a n -- ) {: value arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 0 ?do
      value arr i cells + !
   loop ;
