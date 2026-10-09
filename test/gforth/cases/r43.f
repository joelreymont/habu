\ ' of an internal engine word is refused before the top-row hook hears the
\ tick (habu2.f:5773 C-TICK).
: SHOW ( ptr u8 n n n -- ) . . type cr ;
' SHOW set-top-check
1 drop
' int-mark drop
." after" cr
