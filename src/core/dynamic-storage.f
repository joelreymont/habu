\ dynamic-storage.f - checked transient allocation behind DYNAMIC-BUFFER
\ and HEAP. A buffer record is a head cell holding the mapping (`ptr ptr a`)
\ and the control block right behind it (capacity and registry slot + 1;
\ `ptr n`); a DYNAMIC-BUFFER's block follows them with its element width in
\ bytes. No word views a pointer cell as bytes or cells. Release zeroes the
\ record, so no process address survives capture; the head and the control
\ cells are scratch, so a save or a file's object holds every record released
\ and the registry empty even while this process keeps them. It loads below the
\ core-prefix mark:
\ rewound generated declarations call it.
package DYNAMIC-STORAGE
public

\ A refused map and a refused unmap.
7138 constant E-MAP
7139 constant E-UNMAP

\ The cells of every control block: src/core/layout-buffer.f allots
\ DBUF-CTRL-BYTES for a record, the head cell and these behind it.
DBUF-CTRL-BYTES CELL / 1- constant CONTROL-CELLS

private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
E-LAYOUT-BUFFER constant E-SIZE    \ src/core/layout-buffer.f's size refusal

$40 constant REG-INIT

\ Keep the existing cell-rounded capacity contract: 100 byte elements reserve
\ 104 addressable bytes; COPY below preserves every capacity byte.
: CELL-CEIL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: part:n :}
   part 0= if bytes exit then
   CELL part - {: pad:n :}
   bytes MAX-BYTES pad - > if E-SIZE throw then
   bytes pad + ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need CELL-CEIL exit then
   need old 2 * 64 max max CELL-CEIL ;

\ The old and fresh mappings do not overlap. Every capacity is CELL-CEIL'd and
\ every registry extent is whole rows of cells, so the byte count is a cell
\ multiple and copying whole cells preserves the entire capacity, a fractional
\ element's tail included. The cells move as `a`, so pointers move as pointers.
: COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

\ One registry per runtime instance. Empty capacity is retained until capture,
\ so the pointer itself is the authority on whether the mapping is live.
\ Membership follows live allocation, a precompiled reserve after restore
\ included. Row i holds a record's head address at cell 2i and its control
\ block's address at cell 2i+1; REG-CAP counts rows.
PTR-U8-TABLE-VARIABLE REG
variable REG-CAP   REG-CAP scratch-cell-mark
variable REG-U   REG-U scratch-cell-mark
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- ) begin 0 1 MUTEX atomic-cas 0= until ;
: UNLOCK ( -- ) 0 MUTEX atomic! ;

: REG-BYTES ( n -- n ) 2 cells * ;
: ROW-HEAD ( n -- ptr ptr ptr a ) 2 * REG @ swap ptr-field ;
: ROW-CTL ( n -- ptr ptr n ) 2 * 1+ REG @ swap ptr-field ;
: REG-ENTRY ( n -- ptr ptr a ) ROW-HEAD @ ;
: REG-CTL ( n -- ptr n ) ROW-CTL @ ;

\ The control block's cell after the capacity, which is its first cell.
: SLOT ( ptr n -- ptr n ) CELL + ;

: REG-MAP ( n -- ptr ptr u8 )
   REG-BYTES map-anon 0< if drop E-MAP throw then ;

: REG-CLOSE ( -- )
   REG @ NULL-PTR = if exit then
   REG @ REG-CAP @ REG-BYTES munmap 0< if E-UNMAP throw then
   NULL-PTR REG !
   0 REG-CAP ! ;

\ Capacity is secured before RESERVE publishes either mapping or membership.
\ A failed grow leaves the old registry and every handle intact.
: REG-ROOM ( -- )
   REG @ NULL-PTR = if
      REG-INIT REG-MAP REG !
      REG-INIT REG-CAP !
      exit
   then
   REG-U @ REG-CAP @ < if exit then
   REG-CAP @ {: old:n :}
   old 2 * {: cap:n :}
   cap REG-MAP {: fresh:ptr :}
   REG @ fresh old REG-BYTES COPY
   REG @ old REG-BYTES munmap 0< if
      fresh cap REG-BYTES munmap drop E-UNMAP throw
   then
   fresh REG !
   cap REG-CAP ! ;

: REGISTER ( ptr ptr a ptr n -- ) {: cb:ptr ctl:ptr :}
   cb REG-U @ ROW-HEAD !
   ctl REG-U @ ROW-CTL !
   REG-U @ 1+ dup ctl SLOT ! REG-U ! ;

\ Swap removal is bounded and repairs the moved record's private slot.
: UNREGISTER ( ptr n -- ) {: ctl:ptr :}
   ctl SLOT @ 1- {: at:n :}
   REG-U @ 1- {: last:n :}
   at last <> if
      last REG-ENTRY at ROW-HEAD !
      last REG-CTL {: moved:ptr :}
      moved at ROW-CTL !
      at 1+ moved SLOT !
   then
   NULL-PTR last ROW-HEAD !
   NULL-PTR last ROW-CTL !
   last REG-U !
   0 ctl SLOT ! ;

\ The mapping and its capacity change together: PUBLISH and RELEASE are their
\ only writers, each under LOCK.
: PUBLISH ( ptr a n ptr ptr a ptr n -- )
   {: fresh:ptr cap:n cb:ptr ctl:ptr :}
   fresh cb !
   cap ctl ! ;

\ Nothing in the record changes until every fallible operation has succeeded.
: GROW ( n ptr ptr a ptr n n -- )
   {: need:n cb:ptr ctl:ptr old:n :}
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop E-MAP throw then {: fresh:ptr :}
   old 0= if
      \ Distinct controls may allocate concurrently. Keep their mapping work
      \ outside the lock; registry room and publication form one commit.
      LOCK
      [: REG-ROOM ;] catch {: rc:n :}
      rc 0 <> if
         UNLOCK fresh cap munmap drop rc throw
      then
      fresh cap cb ctl PUBLISH
      cb ctl REGISTER
      UNLOCK
      exit
   then
   cb @ fresh old COPY
   LOCK
   cb @ old munmap 0< if
      UNLOCK fresh cap munmap drop E-UNMAP throw
   then
   fresh cap cb ctl PUBLISH
   UNLOCK ;

public

: RESERVE ( n ptr ptr a ptr n n -- ) {: count:n cb:ptr ctl:ptr width:n :}
   count width * {: need:n :}
   ctl @ {: old:n :}
   need old <= if exit then
   need cb ctl old GROW ;

: RELEASE ( ptr ptr a ptr n -- ) {: cb:ptr ctl:ptr :}
   ctl @ {: cap:n :}
   cap 0= if exit then
   LOCK
   cb @ cap munmap 0< if UNLOCK E-UNMAP throw then
   ctl UNREGISTER
   NULL-PTR cb !
   0 ctl !
   UNLOCK ;


\ A zero-filled record of `bytes` on its own mapping, its pointee free: only HEAP's
\ generated NAME-NEW calls it, passing its type's width (checker.f ARENA-CALL-REFUSE?).
: RECORD-NEW ( n -- ptr a )
   map-anon 0< if drop E-MAP throw then ;  INTRINSIC-RECORD-NEW INTRINSIC


\ Unmaps a record RECORD-NEW mapped with the same `bytes`; a refused unmap is
\ E-UNMAP.
: RECORD-RELEASE ( ptr a n -- )
   munmap 0< if E-UNMAP throw then ;  INTRINSIC-RECORD-RELEASE INTRINSIC


\ Runs a fork with the registry held; PROC-FORK:RAW is its caller. Growth,
\ release and membership changes each take several stores under this lock, and a
\ child forked mid-change could neither wait for that task nor carry on where it
\ stopped. Held, nobody is inside when the process is copied, and parent and
\ child each free their own copy.
: ACROSS-FORK ( [ -- n ] -- n )
   LOCK [: UNLOCK ;] finally ;

: REGISTERED-N ( -- n ) REG-U @ ;

private

\ Each routine takes a DYNAMIC-BUFFER's block by its head, or by the cell
\ holding the head, last.
1 cells constant CTL-OFF
CONTROL-CELLS 1+ cells constant WIDTH-OFF

: HEAD-CTL ( ptr ptr a -- ptr n ) CTL-OFF + byte-view cell-view ;
: HEAD-WIDTH ( ptr ptr a -- n ) WIDTH-OFF + byte-view cell-view @ ;

\ Element idx of the block. Growth can replace the mapping, so it fetches the
\ head on every access; the width is read inline so an access stays one call
\ deep.
: ELEMENT ( n ptr ptr a -- ptr a )
   {: idx:n cb:ptr :}
   cb @  idx  cb WIDTH-OFF + byte-view cell-view @  * + ;

\ Reserves count elements in the block the cell holds.
: ENSURE ( n ptr ptr ptr a -- )
   {: count:n b:ptr :}
   b @ {: cb:ptr :}
   count cb  cb HEAD-CTL  cb HEAD-WIDTH  RESERVE ;

\ Releases the block the cell holds.
: EMPTY ( ptr ptr ptr a -- ) @ dup HEAD-CTL RELEASE ;

\ Whether row i's record lies wholly inside the span. A record is its head (one
\ cell) and its control block (CONTROL-CELLS cells): it belongs to the span when
\ both lie wholly inside it, and a span that cuts through either, or holds
\ exactly one of them, is refused. Offsets are distances, never views: the head
\ is a pointer cell, so its distance is taken from NULL-PTR.
: PART-IN? ( n n n -- bool ) {: off:n extent:n size:n :}
   off 0 < if
      off extent + 0 > if E-SIZE throw then
      0 0 <> exit
   then
   off size >= if 0 0 <> exit then
   extent size off - > if E-SIZE throw then
   0 0= ;

: MEMBER? ( n ptr u8 n -- bool ) {: row:n start:ptr size:n :}
   row REG-ENTRY NULL-PTR - start NULL-PTR - - CELL size PART-IN? {: head-in:bool :}
   row REG-CTL byte-view start - CONTROL-CELLS cells size PART-IN? {: ctl-in:bool :}
   head-in if
      ctl-in 0= if E-SIZE throw then
   else
      ctl-in if E-SIZE throw then
   then
   head-in ;

\ Refuse a cut through any live control record before releasing a member.
: RANGE-CHECK ( ptr u8 n -- ) {: start:ptr size:n :}
   size 0 < if E-SIZE throw then
   size 0= if exit then
   REG-U @ 0 ?do
      i start size MEMBER? drop
   loop ;

public

\ Removal shrinks the registry, so each successful release consumes its last
\ entry. A refused unmap preserves that entry for the next call.
\ Its caller runs it after every task has stopped, so registry walks need no
\ second lock. Concurrent mutation of one buffer remains its caller's
\ responsibility; the lock protects membership of distinct controls.
: RELEASE-ALL ( -- )
   begin REG-U @ 0 > while
      REG-U @ 1- {: last:n :}
      last REG-ENTRY last REG-CTL RELEASE
   repeat
   REG-CLOSE ;

\ A retained runtime can own both capture-window and writer buffers. Only
\ records wholly in this DATA span belong to that captured value; an empty span
\ holds none.
: RELEASE-RANGE ( ptr u8 n -- ) {: start:ptr size:n :}
   start size RANGE-CHECK
   size 0 > if
      REG-U @
      begin dup 0 > while
         1- dup {: row:n :}
         row start size MEMBER? if row REG-ENTRY row REG-CTL RELEASE then
      repeat drop
   then
   REG-U @ 0= if REG-CLOSE then ;

seal-package

;package
