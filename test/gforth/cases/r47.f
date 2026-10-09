\ A hook that answers 0 drops a definition with no signature, a definer
\ included, and the load goes on (habu2.f EM-COMPILE-PUBLISH-HOOKED): F is
\ then undefined.
: NOPE ( ptr u8 n -- n ) 2drop 0 ;
' NOPE set-check
: F 1 ;
: MK create , does> ( -- n ) @ ;
." after" cr
F
