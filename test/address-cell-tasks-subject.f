\ Concurrent first stores of both declared pointer kinds, with real growth.
\ Tier 1 first: the concurrent stores under test are lowered by the optimizing
\ backend; the parent spawns this file with no tier of its own.
1 set-tier

require lib/test.f
require lib/task.f
require src/habu/address-cells.f
package ADDRESS-CELL-TASKS
private
1024 constant EACH
4 constant WORKERS
EACH WORKERS * constant WAVE-N
ADDRESS-CELLS:BOOT-CAP 2 * WAVE-N 2 * + constant SLOT-N
create SLOTS SLOT-N cells allot
create SEEN SLOT-N allot
DYNAMIC-BUFFER BEFORE-STORAGE n
here data-base - negate 7 and allot
variable READY
variable START
variable OFFSET
variable BASE-N
variable USED
variable START-CAP
TASK:MIN-STACK TASK:TASK A
TASK:MIN-STACK TASK:TASK B
TASK:MIN-STACK TASK:TASK C
TASK:MIN-STACK TASK:TASK D
\ Only odd slots hold quotations; the ordinary array exposes raw cells.
TRUSTED: QUOT-SLOT ( ptr n -- ptr [ -- ] ) ;
: EMPTY ( -- ) ;
: MARK ( n -- ) {: k:n :}
   k 1 and 0<> if
      ['] EMPTY SLOTS k cells + QUOT-SLOT xt!
   else SLOTS k cells + ptr-cell-mark then ;
: WORK ( n -- ) {: worker:n :}
   1 READY atomic-add drop
   begin START atomic@ 0= while TASK:PAUSE repeat
   EACH 0 ?do OFFSET @ worker EACH * + i + MARK loop ;
: WA ( -- ) 0 WORK ;
: WB ( -- ) 1 WORK ;
: WC ( -- ) 2 WORK ;
: WD ( -- ) 3 WORK ;
: JOIN ( ptr n -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat
   worker TASK:KILL ;
: COUNT ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;
: BEFORE ( -- ptr n ) 0 BEFORE-STORAGE ;
: HEADER@ ( n -- n ) data-base SNAP-RELOC:XTCELL-N-CELL + + @ ;
: PRIME ( -- )
   ['] EMPTY A TASK:ACTIVATE A JOIN
   ['] EMPTY B TASK:ACTIVATE B JOIN
   ['] EMPTY C TASK:ACTIVATE C JOIN
   ['] EMPTY D TASK:ACTIVATE D JOIN ;
: WAVE ( n -- ) {: added:n :}
   0 READY ! 0 START !
   COUNT added + {: expected:n :}
   ['] WA A TASK:ACTIVATE
   ['] WB B TASK:ACTIVATE
   ['] WC C TASK:ACTIVATE
   ['] WD D TASK:ACTIVATE
   begin READY atomic@ WORKERS < while TASK:PAUSE repeat
   1 START atomic!
   A JOIN B JOIN C JOIN D JOIN
   COUNT expected T= ;
: EXPECT-ROWS ( -- )
   BASE-N @ 0 ?do i ADDRESS-CELLS:ROW@ BEFORE i cells + @ T= loop
   USED @ 0 ?do 0 SEEN i + c! loop
   COUNT BASE-N @ ?do
      i ADDRESS-CELLS:ROW@ {: row:n :}
      row SNAP-RELOC:XTCELL-OFF-MASK and SLOTS data-base - - {: off:n :}
      off 0 >= off USED @ cells < and off 7 and 0= and 0= if
         s" address-cell-tasks: unexpected row location" 76 die
      then
      off CELL / {: slot:n :}
      row SNAP-RELOC:XTCELL-DATA-TAG and
      slot 1 and 0= if SNAP-RELOC:XTCELL-DATA-TAG else 0 then T=
      SEEN slot + dup c@ 1+ swap c!
   loop
   USED @ 0 ?do SEEN i + c@ 1 T= loop ;
public
: RUN ( -- )
   T-RESET PRIME
   COUNT dup BASE-N !
   ADDRESS-CELLS:CAP-FIELD HEADER@ dup START-CAP !
   \ Capture may already leave fewer than half a wave of spare rows.
   WAVE-N 2 / - swap - 0 max {: fill:n :}
   fill WAVE-N 2 * + SLOT-N > if
      s" address-cell-tasks: fixture cannot force growth" 76 die
   then
   BASE-N @ BEFORE-STORAGE-RESERVE
   BASE-N @ 0 ?do i ADDRESS-CELLS:ROW@ BEFORE i cells + ! loop
   fill 0 ?do i MARK loop
   fill OFFSET ! fill WAVE-N + USED !
   s" concurrent first stores grow the current backing" T-LABEL
   WAVE-N WAVE
   ADDRESS-CELLS:CAP-FIELD HEADER@ START-CAP @ > TTRUE
   ADDRESS-CELLS:MODE-FIELD HEADER@ 1 T=
   EXPECT-ROWS
   s" concurrent same-kind stores remain idempotent" T-LABEL
   0 WAVE EXPECT-ROWS
   s" a later wave adds every distinct row" T-LABEL
   WAVE-N OFFSET +! WAVE-N USED +!
   WAVE-N WAVE EXPECT-ROWS
   SLOTS 1 cells + QUOT-SLOT @ execute
   data-base ADDRESS-CELLS:LOCK-CELL + @ 0 T=
   BEFORE-STORAGE-RELEASE
   T-REPORT s" address-cell-tasks: ok" type cr ;
;package
