\ Loaded inside the child REPL before a line deliberately fails.
require src/habu/address-cells.f
package REPL-ROWS
variable BASE-N
variable BASE-DP
: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
public
: SAVE ( -- ) ROWS BASE-N ! here NULL-PTR - BASE-DP ! ;
\ The failed line's declaration keeps its row and its data.
: KEPT ( -- )
   ROWS BASE-N @ 1+ = here NULL-PTR - BASE-DP @ > and
   if s" rows-kept-pass" else s" rows-kept-fail" then type cr ;
\ A declaration made after the failed line adds one row more.
: ADDED ( -- )
   ROWS BASE-N @ 2 + =
   if s" rows-added-pass" else s" rows-added-fail" then type cr ;
;package
