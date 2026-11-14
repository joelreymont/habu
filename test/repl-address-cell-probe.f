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
\ A declaration made at SAVE's here reuses its bytes: its cell starts at most a cell past it.
: REUSED ( -- )
   ROWS 1- ADDRESS-CELLS:ROW@ SNAP-RELOC:XTCELL-OFF-MASK and
   BASE-DP @ - {: d:n :}
   ROWS BASE-N @ 1+ =  d 0 >= and  d CELL <= and
   if s" rows-reused-pass" else s" rows-reused-fail" then type cr ;
;package
