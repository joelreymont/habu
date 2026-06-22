\ array.f - checked cell-array helpers.

: A-CHECK-INDEX ( len idx -- ) {: len ix :}
   len LEN>N 0 < if E-A-BOUNDS throw then
   ix IDX>N 0 < if E-A-BOUNDS throw then
   ix IDX>N len LEN>N >= if E-A-BOUNDS throw then ;

: A-CHECK-RANGE ( len idx count -- ) {: len start cnt :}
   len LEN>N 0 < if E-A-BOUNDS throw then
   start IDX>N 0 < if E-A-BOUNDS throw then
   cnt COUNT>N 0 < if E-A-BOUNDS throw then
   start IDX>N len LEN>N > if E-A-BOUNDS throw then
   cnt COUNT>N len LEN>N start IDX>N - > if E-A-BOUNDS throw then ;

: A-CHECK-NONEMPTY ( len -- ) {: len :}
   len LEN>N 0 < if E-A-BOUNDS throw then
   len LEN>N 0= if E-A-EMPTY throw then ;

: A-CHECK-WHOLE ( len -- ) {: len :}
   len 0 >IDX len LEN>N >COUNT A-CHECK-RANGE ;

: A@ ( ptr a len idx -- a ) {: arr:ptr len ix :}
   len ix A-CHECK-INDEX
   arr ix IDX>N cells + @ ;

: A! ( a ptr a len idx -- ) {: value arr:ptr len ix :}
   len ix A-CHECK-INDEX
   value arr ix IDX>N cells + ! ;

: A+! ( n ptr a len idx -- ) {: delta arr:ptr len ix :}
   arr len ix A@ delta +
   arr len ix A! ;

: A-SWAP ( ptr a len idx idx -- ) {: arr:ptr len ix jx :}
   arr len ix A@
   arr len jx A@
   arr len ix A!
   arr len jx A! ;

: LAST-INDEX ( len -- idx ) {: len :}
   len A-CHECK-NONEMPTY
   len LEN>N 1 - >IDX ;

: MIRROR-INDEX ( len idx -- idx ) {: len ix :}
   len ix A-CHECK-INDEX
   len LEN>N 1 - ix IDX>N - >IDX ;

: EVEN? ( n -- bool )
   1 and 0= ;

: A-SUM ( ptr n len -- n ) {: arr:ptr len :}
   len A-CHECK-WHOLE
   0
   len LEN>N 0 ?do
      arr i cells + @ +
   loop ;

: A-MIN ( ptr n len -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   arr @
   len LEN>N 1 ?do
      arr i cells + @ min
   loop ;

: A-MAX ( ptr n len -- n ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   arr @
   len LEN>N 1 ?do
      arr i cells + @ max
   loop ;

: A-COUNT-EVEN ( ptr n len -- count ) {: arr:ptr len :}
   len A-CHECK-WHOLE
   0
   len LEN>N 0 ?do
      arr i cells + @ 2 mod 0= if 1+ then
   loop >COUNT ;

: A-ARGMAX ( ptr n len -- idx ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   0 >IDX arr @
   len LEN>N 1 ?do
      arr i cells + @ over > if
         2drop
         i >IDX arr i cells + @
      then
   loop
   drop ;

: A-MAX-INDEX ( ptr n len -- idx )
   A-ARGMAX ;

: A-REVERSE-RANGE! ( ptr a len idx count -- ) {: arr:ptr len start cnt :}
   len start cnt A-CHECK-RANGE
   cnt COUNT>N 2 / 0 ?do
      arr len
      start IDX>N i + >IDX
      start IDX>N cnt COUNT>N + 1 - i - >IDX
      A-SWAP
   loop ;

: A-REVERSE! ( ptr a len -- ) {: arr:ptr len :}
   arr len 0 >IDX len LEN>N >COUNT A-REVERSE-RANGE! ;

: A-PREFIX-SUM! ( ptr n len -- ) {: arr:ptr len :}
   len A-CHECK-WHOLE
   len LEN>N 1 <= if exit then
   len LEN>N 1 ?do
      arr i 1 - cells + @
      arr i cells + @ +
      arr i cells + !
   loop ;

: A-RUNMAX! ( ptr n len -- ) {: arr:ptr len :}
   len A-CHECK-WHOLE
   len LEN>N 1 <= if exit then
   len LEN>N 1 ?do
      arr i 1 - cells + @
      arr i cells + @ max
      arr i cells + !
   loop ;

: A-FILL! ( a ptr a len -- ) {: value arr:ptr len :}
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      value arr i cells + !
   loop ;

: A-MAP! ( ptr a len [ a -- a ] -- ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   arr len LEN>N q MAP ;

: A-MAPI! ( ptr a len [ idx a -- a ] -- ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      i >IDX arr len i >IDX A@ q execute
      arr len i >IDX A!
   loop ;

: A-FOLD ( ptr a len b [ b a -- b ] -- b ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   arr len LEN>N acc q FOLD ;

: A-FOLDI ( ptr a len b [ b idx a -- b ] -- b ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   acc len LEN>N 0 ?do
      i >IDX arr len i >IDX A@ q execute
   loop ;

: A-SCAN! ( ptr n len n [ n n -- n ] -- ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   acc len LEN>N 0 ?do
      arr len i >IDX A@ q execute
      dup arr len i >IDX A!
   loop
   drop ;

: A-SCAN1! ( ptr n len [ n n -- n ] -- ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   len LEN>N 1 <= if exit then
   arr len 0 >IDX A@
   len LEN>N 1 ?do
      arr len i >IDX A@ q execute
      dup arr len i >IDX A!
   loop
   drop ;

: A-FIND-INDEX ( ptr a len [ a -- bool ] -- n ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      arr len i >IDX A@ q execute if i unloop exit then
   loop
   -1 ;

: A-FIND-INDEXI ( ptr a len [ idx a -- bool ] -- n ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      i >IDX arr len i >IDX A@ q execute if i unloop exit then
   loop
   -1 ;
