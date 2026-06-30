\ warm-image-gate-stats.f - gate stats hook for warm-image-lib.f.
\
\ Load after test/gate-stats.f and tools/warm-image-lib.f.

: WIGS-EVENT ( ptr u8 n -- )
   GS-EVENT ;

: WIGS-INSTALL ( -- )
   [: WIGS-EVENT ;] is WI-EVENT ;

WIGS-INSTALL
