\ An internal engine word is refused at top level before the min-in gate and
\ before the top-row hook hears it (habu2.f:10163 LINTERNAL).
: SHOW ( ptr u8 n n n -- ) . . type cr ;
' SHOW set-top-check
1 drop
int-mark
." after" cr
