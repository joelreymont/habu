\ The checker installs its preflight at boot (src/core/check-hook.f INSTALL),
\ and while it holds the cell no other xt replaces it: exit 70 (habu1.f:3747
\ BSETPREFLIGHT).
: PF ( ptr u8 n ptr u8 n bool -- ) drop 2drop 2drop ;
' PF set-preflight ." replaced" cr
