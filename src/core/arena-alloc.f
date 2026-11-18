split-str
\ arena-alloc.f - checker arenas on fresh anonymous pages: allocation and
\ growth.

\ `map-anon` answers a pointer whose element type the caller picks.
: ARENA-ALLOC ( n -- ^a )
   dup map-anon 0 <> IF drop s" a checker arena" REJECTION:MAP-STOP THEN nip ;

: ARENA-ALLOC-BYTES ( n -- ^u8 ) ARENA-ALLOC ;

\ Callers reset their counters per definition, so no tail init is needed.
: ARENA-BYTES-GROW ( ^a n n -- ^a ) {: base:^ oldbytes:n newbytes:n :}
   newbytes ARENA-ALLOC {: nb:^ :}
   base nb oldbytes CELL + 1- CELL / ARENA-CELLS-COPY
   base oldbytes ARENA-ABANDON
   nb ;

\ Fits need bytes in the mapping pv holds; capv holds its byte count, 0 before
\ the first mapping. A grow moves the bytes, so an address into them goes stale.
: ARENA-ROOM ( ^^a ^n n n -- ) {: pv:^ capv:^ first:n need:n :}
   capv @ 0= IF first ARENA-ALLOC pv !  first capv ! THEN
   capv @ BEGIN dup need < WHILE
      dup -1 1 rshift 2 / > IF drop need ELSE 2 * THEN
   REPEAT {: nc:n :}
   nc capv @ > IF pv @ capv @ nc ARENA-BYTES-GROW pv !  nc capv ! THEN ;
