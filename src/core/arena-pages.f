\ arena-pages.f - growable stores on fresh anonymous pages: allocation, copy,
\ zeroing, growth and release.

\ A cell no binding has reached.
-1 constant UNBOUND

\ --- growable checker arenas --------------------------------------------
\ `map-anon` answers a pointer whose element type the caller picks.
: ARENA-ALLOC ( n -- ^a )
   map-anon 0 <> IF drop s" checker: arena mmap failed" 76 die THEN ;

: ARENA-ALLOC-BYTES ( n -- ^u8 ) ARENA-ALLOC ;

\ An address's DATA offset, measured from the null so neither pointer is viewed.
: DATA-OFF ( ^a -- n ) NULL-PTR - data-base NULL-PTR - - ;

: ARENA-COPY ( ^u8 ^u8 n -- ) {: src:^ dst:^ n:n :}
   0
   begin dup CELL + n <= while
      src over + CELL-VIEW @  over dst swap + CELL-VIEW !
      CELL +
   repeat
   begin dup n < while
      src over + c@  over dst swap + c!
      1 +
   repeat
   drop ;

: ARENA-ZERO ( ^u8 n -- ) {: dst:^ u:n :}
   u CELL / CELL * {: bulk:n :}
   0
   begin dup bulk < while
      0 over dst + CELL-VIEW ! CELL +
   repeat
   begin dup u < while
      0 over dst + c! 1+
   repeat drop ;

\ A caller holding a byte count passes it rounded up to cells. The rounded read
\ stays mapped because every source is a page-granular map-anon mapping or lies
\ inside DATA; the rounded write stays inside the destination because that is a
\ fresh ARENA-ALLOC (whole pages), CHECKER-REG:GROW's whole cells at a build's
\ `here`, or an SR-TAKE, which starts the next take on a cell boundary past it.
: ARENA-CELLS-COPY ( ^a ^a n -- ) {: src:^ dst:^ n:n :}
   n 0 ?do src i cells + @ dst i cells + ! loop ;

: ARENA-CELLS-UNBOUND ( ^n n n -- ) {: base:^ from:n to:n :}
   from begin dup to < while {: i:n :}
      UNBOUND i cells base + !
      i 1 +
   repeat drop ;

: ARENA-CELLS-ZERO ( ^n n n -- ) {: base:^ from:n to:n :}
   from begin dup to < while {: i:n :}
      0 i cells base + !
      i 1 +
   repeat drop ;

\ Validates the allocation capacity, not only the live prefix being copied. The
\ subtraction follows the ordered bounds checks, so the end address cannot wrap.
: REG-DATA-SPAN? ( ^u8 n -- bool ) {: base:^ cap:n :}
   cap 0 < 0=  base data-base < 0= and  base here <= and
   dup IF drop cap here base - <= THEN ;

: ARENA-UNMAP ( ^a n -- )
   {: base:^ bytes:n :}
   bytes 0= IF EXIT THEN
   base bytes munmap 0 <> IF s" checker: arena munmap failed" 76 die THEN ;

\ A store in image DATA is zeroed: it is still baked, so its bytes do not
\ depend on how writes and the grow interleave, and a stale pointer into it
\ reads zeros.
: ARENA-ABANDON ( ^a n -- ) {: base:^ bytes:n :}
   base BYTE-VIEW bytes REG-DATA-SPAN? IF base BYTE-VIEW bytes ARENA-ZERO EXIT THEN
   base bytes ARENA-UNMAP ;

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
