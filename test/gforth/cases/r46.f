\ run-rc has no specification row, so its record states no inputs: the top-row
\ hook hears it, then its body's read of pathz under the floor is refused.
: SHOW ( ptr u8 n n n -- ) . . type cr ;
' SHOW set-top-check
run-rc
." after" cr
