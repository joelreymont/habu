\ array.f - checked cell-array helpers.

require lib/adt/option.f                        \ option<idx> for A-FIND-INDEX / A-FIND-INDEXI (switchover wave A)

: A-LEN ( n -- len )
   dup 0 < if E-A-BOUNDS throw then
   >LEN ;

: A-IDX ( n -- idx )
   dup 0 < if E-A-BOUNDS throw then
   >IDX ;

: A-COUNT ( n -- count )
   dup 0 < if E-A-BOUNDS throw then
   >COUNT ;

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
   len 0 A-IDX len LEN>N A-COUNT A-CHECK-RANGE ;

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
   len LEN>N 1 - A-IDX ;

: MIRROR-INDEX ( len idx -- idx ) {: len ix :}
   len ix A-CHECK-INDEX
   len LEN>N 1 - ix IDX>N - A-IDX ;

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
   loop A-COUNT ;

: A-ARGMAX ( ptr n len -- idx ) {: arr:ptr len :}
   len A-CHECK-NONEMPTY
   0 A-IDX arr @
   len LEN>N 1 ?do
      arr i cells + @ over > if
         2drop
         i A-IDX arr i cells + @
      then
   loop
   drop ;

: A-MAX-INDEX ( ptr n len -- idx )
   A-ARGMAX ;

: A-REVERSE-RANGE! ( ptr a len idx count -- ) {: arr:ptr len start cnt :}
   len start cnt A-CHECK-RANGE
   cnt COUNT>N 2 / 0 ?do
      arr len
      start IDX>N i + A-IDX
      start IDX>N cnt COUNT>N + 1 - i - A-IDX
      A-SWAP
   loop ;

: A-REVERSE! ( ptr a len -- ) {: arr:ptr len :}
   arr len 0 A-IDX len LEN>N A-COUNT A-REVERSE-RANGE! ;

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
      i A-IDX arr len i A-IDX A@ q execute
      arr len i A-IDX A!
   loop ;

: A-FOLD ( ptr a len b [ b a -- b ] -- b ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   arr len LEN>N acc q FOLD ;

: A-FOLDI ( ptr a len b [ b idx a -- b ] -- b ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   acc len LEN>N 0 ?do
      i A-IDX arr len i A-IDX A@ q execute
   loop ;

: A-SCAN! ( ptr n len n [ n n -- n ] -- ) {: arr:ptr len acc q :}
   len A-CHECK-WHOLE
   acc len LEN>N 0 ?do
      arr len i A-IDX A@ q execute
      dup arr len i A-IDX A!
   loop
   drop ;

: A-SCAN1! ( ptr n len [ n n -- n ] -- ) {: arr:ptr len q :}
   len A-CHECK-WHOLE
   len LEN>N 1 <= if exit then
   arr len 0 A-IDX A@
   len LEN>N 1 ?do
      arr len i A-IDX A@ q execute
      dup arr len i A-IDX A!
   loop
   drop ;

\ typed-local-lint: allow-bare-local - q keeps the predicate quotation effect from the stack signature.
: A-FIND-INDEX ( ptr a len [ a -- bool ] -- option<idx> ) {: arr:ptr len:len q :}   \ SOME first value-matching index, else NONE
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      arr len i A-IDX A@ q execute if i >IDX OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;

\ typed-local-lint: allow-bare-local - q keeps the predicate quotation effect from the stack signature.
: A-FIND-INDEXI ( ptr a len [ idx a -- bool ] -- option<idx> ) {: arr:ptr len:len q :}   \ SOME first index-aware match, else NONE
   len A-CHECK-WHOLE
   len LEN>N 0 ?do
      i A-IDX arr len i A-IDX A@ q execute if i >IDX OPTION:SOME unloop exit then
   loop
   OPTION:NONE ;
