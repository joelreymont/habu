\ array.f - checked cell-array helpers.

: A-CHECK-INDEX ( n n -- ) {: len ix :}
   len 0 < if E-A-BOUNDS throw then
   ix 0 < if E-A-BOUNDS throw then
   ix len >= if E-A-BOUNDS throw then ;

: A-CHECK-RANGE ( n n n -- ) {: len start cnt :}
   len 0 < if E-A-BOUNDS throw then
   start 0 < if E-A-BOUNDS throw then
   cnt 0 < if E-A-BOUNDS throw then
   start len > if E-A-BOUNDS throw then
   cnt len start - > if E-A-BOUNDS throw then ;

: A-CHECK-NONEMPTY ( n -- ) {: len :}
   len 0 < if E-A-BOUNDS throw then
   len 0= if E-A-EMPTY throw then ;

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
      arr i cells + @ 2 mod 0= if 1+ then
   loop ;

: A-ARGMAX ( ptr n n -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   0 arr @
   len 1 ?do
      arr i cells + @ over > if
         2drop
         i arr i cells + @
      then
   loop
   drop ;

: A-MAX-INDEX ( ptr n n -- n )
   A-ARGMAX ;

: A-REVERSE-RANGE! ( ptr a n n n -- ) {: arr:ptr len start cnt :}
   len start cnt A-CHECK-RANGE
   cnt 2 / 0 ?do
      arr len start i + start cnt + 1 - i - A-SWAP
   loop ;

: A-REVERSE! ( ptr a n -- ) {: arr:ptr len :}
   arr len 0 len A-REVERSE-RANGE! ;

: A-PREFIX-SUM! ( ptr n n -- ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 1 <= if exit then
   len 1 ?do
      arr i 1 - cells + @
      arr i cells + @ +
      arr i cells + !
   loop ;

: A-RUNMAX! ( ptr n n -- ) {: arr:ptr len :}
   len 0 len A-CHECK-RANGE
   len 1 <= if exit then
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

: A-MAP! ( ptr a n [ a -- a ] -- ) {: arr:ptr len q :}
   len 0 len A-CHECK-RANGE
   arr len q MAP ;

: A-MAPI! ( ptr a n [ n a -- a ] -- ) {: arr:ptr len q :}
   len 0 len A-CHECK-RANGE
   len 0 ?do
      i arr len i A@ q execute
      arr len i A!
   loop ;

: A-FOLD ( ptr a n b [ b a -- b ] -- b ) {: arr:ptr len acc q :}
   len 0 len A-CHECK-RANGE
   arr len acc q FOLD ;

: A-FOLDI ( ptr a n b [ b n a -- b ] -- b ) {: arr:ptr len acc q :}
   len 0 len A-CHECK-RANGE
   acc len 0 ?do
      i arr len i A@ q execute
   loop ;

: A-SCAN! ( ptr n n n [ n n -- n ] -- ) {: arr:ptr len acc q :}
   len 0 len A-CHECK-RANGE
   acc len 0 ?do
      arr len i A@ q execute
      dup arr len i A!
   loop
   drop ;

: A-SCAN1! ( ptr n n [ n n -- n ] -- ) {: arr:ptr len q :}
   len 0 len A-CHECK-RANGE
   len 1 <= if exit then
   arr len 0 A@
   len 1 ?do
      arr len i A@ q execute
      dup arr len i A!
   loop
   drop ;

: A-FIND-INDEX ( ptr a n [ a -- bool ] -- n ) {: arr:ptr len q :}
   len 0 len A-CHECK-RANGE
   len 0 ?do
      arr len i A@ q execute if i exit then
   loop
   -1 ;

: A-FIND-INDEXI ( ptr a n [ n a -- bool ] -- n ) {: arr:ptr len q :}
   len 0 len A-CHECK-RANGE
   len 0 ?do
      i arr len i A@ q execute if i exit then
   loop
   -1 ;
