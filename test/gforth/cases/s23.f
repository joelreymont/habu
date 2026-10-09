\ set-preflight installs once (habu1.f:3747 BSETPREFLIGHT): with its cell
\ cleared by `0 set-check` a live code entry installs, the same xt again
\ changes nothing, and another is refused on fd 2 with exit 70.
: PF ( ptr u8 n ptr u8 n bool -- ) drop 2drop 2drop ;
: PG ( ptr u8 n ptr u8 n bool -- ) drop 2drop 2drop ;
0 set-check
' PF set-preflight 1 .
' PF set-preflight 2 .
' PG set-preflight ." replaced" cr
