\ Loaded inside the child REPL before a line deliberately fails.
require src/habu/address-cells.f
package REPL-ROWS
variable BASE-N
variable BASE-DP
: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
public
: SAVE ( -- ) ROWS BASE-N ! here data-base - BASE-DP ! ;
: RESTORED ( -- )
   ROWS BASE-N @ = here data-base - BASE-DP @ = and
   if s" rows-restored-pass" else s" rows-restored-fail" then type cr ;
: REUSED ( -- )
   ROWS BASE-N @ 1+ =
   ROWS 1- ADDRESS-CELLS:ROW@ SNAP-RELOC:XTCELL-OFF-MASK and BASE-DP @ = and
   if s" rows-reused-pass" else s" rows-reused-fail" then type cr ;
;package
