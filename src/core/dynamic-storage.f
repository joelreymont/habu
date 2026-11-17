\ dynamic-storage.f - checked transient allocation behind DYNAMIC-BUFFER
\ and HEAP. A buffer record is a head cell holding the mapping (`^^a`)
\ and the control block right behind it (capacity and registry slot + 1;
\ `^n`); a DYNAMIC-BUFFER's block follows them with its element width in
\ bytes. No word views a pointer cell as bytes or cells. Release zeroes the
\ record, so no process address survives capture; the head and the control
\ cells are scratch, so a save or a file's object holds every record released
\ and the registry empty even while this process keeps them. It loads below the
\ core-prefix mark:
\ rewound generated declarations call it.
package DYNAMIC-STORAGE
public

\ The code of a map the system refused.
7138 constant E-MAP

2 constant CONTROL-CELLS

private

$7FFFFFFFFFFFFFFF constant MAX-BYTES

$40 constant REG-INIT

\ Keep the existing cell-rounded capacity contract: 100 byte elements reserve
\ 104 addressable bytes; COPY below preserves every capacity byte.
: CELL-CEIL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: part:n :}
   part 0= if bytes exit then
   bytes CELL part - + ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need CELL-CEIL exit then
   need old 2 * 64 max max CELL-CEIL ;

\ The old and fresh mappings do not overlap. Every capacity is CELL-CEIL'd and
\ every registry extent is whole rows of cells, so the byte count is a cell
\ multiple and copying whole cells preserves the entire capacity, a fractional
\ element's tail included. The cells move as `a`, so pointers move as pointers.
: COPY ( ^a ^a n -- ) {: src:^ dst:^ bytes:n :}
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
: ROW-HEAD ( n -- ^^^a ) 2 * REG @ swap ptr-field ;
: ROW-CTL ( n -- ^^n ) 2 * 1+ REG @ swap ptr-field ;
: REG-ENTRY ( n -- ^^a ) ROW-HEAD @ ;
: REG-CTL ( n -- ^n ) ROW-CTL @ ;

\ The control block's cell after the capacity, which is its first cell.
: SLOT ( ^n -- ^n ) CELL + ;

\ The record of a map of n bytes the system refused, thrown as E-MAP: a caller
\ that can go on without the storage catches it, and nothing prints.
: MAP-REJECT ( n -- ) {: bytes:n :}
   bytes s" " REJECTION:MAP-REFUSED
   REJECTION:HELP
   s" reserve fewer elements, or release storage the program no longer needs"
   REJECTION:EMIT-STR
   REJECTION:SAVE  E-MAP REJECTION:PASS ;

: REG-MAP ( n -- ^^u8 )
   REG-BYTES {: bytes:n :}
   bytes map-anon 0< if drop bytes MAP-REJECT then ;

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
   cap REG-MAP {: fresh:^ :}
   REG @ fresh old REG-BYTES COPY
   \ GROW holds the lock, and the exit actions die runs may take it.
   REG @ old REG-BYTES munmap 0< if
      UNLOCK
      s" dynamic storage: the system refused to unmap this process's own mapping"
      EXIT-STATUS:SYSTEM die
   then
   fresh REG !
   cap REG-CAP ! ;

: REGISTER ( ^^a ^n -- ) {: cb:^ ctl:^ :}
   cb REG-U @ ROW-HEAD !
   ctl REG-U @ ROW-CTL !
   REG-U @ 1+ dup ctl SLOT ! REG-U ! ;

\ Swap removal is bounded and repairs the moved record's private slot.
: UNREGISTER ( ^n -- ) {: ctl:^ :}
   ctl SLOT @ 1- {: at:n :}
   REG-U @ 1- {: last:n :}
   at last <> if
      last REG-ENTRY at ROW-HEAD !
      last REG-CTL {: moved:^ :}
      moved at ROW-CTL !
      at 1+ moved SLOT !
   then
   NULL-PTR last ROW-HEAD !
   NULL-PTR last ROW-CTL !
   last REG-U !
   0 ctl SLOT ! ;

\ The mapping and its capacity change together: PUBLISH and RELEASE are their
\ only writers, each under LOCK.
: PUBLISH ( ^a n ^^a ^n -- )
   {: fresh:^ cap:n cb:^ ctl:^ :}
   fresh cb !
   cap ctl ! ;

\ Nothing in the record changes until every fallible operation has succeeded.
: GROW ( n ^^a ^n n -- )
   {: need:n cb:^ ctl:^ old:n :}
   need old CAPACITY {: cap:n :}
   cap map-anon 0< if drop cap MAP-REJECT then {: fresh:^ :}
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
      UNLOCK
      s" dynamic storage: the system refused to unmap this process's own mapping"
      EXIT-STATUS:SYSTEM die
   then
   fresh cap cb ctl PUBLISH
   UNLOCK ;

public

: RESERVE ( n ^^a ^n n -- ) {: count:n cb:^ ctl:^ width:n :}
   count width * {: need:n :}
   ctl @ {: old:n :}
   need old <= if exit then
   need cb ctl old GROW ;

: RELEASE ( ^^a ^n -- ) {: cb:^ ctl:^ :}
   ctl @ {: cap:n :}
   cap 0= if exit then
   LOCK
   cb @ cap munmap 0< if
      UNLOCK
      s" dynamic storage: the system refused to unmap this process's own mapping"
      EXIT-STATUS:SYSTEM die
   then
   ctl UNREGISTER
   NULL-PTR cb !
   0 ctl !
   UNLOCK ;


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

: HEAD-CTL ( ^^a -- ^n ) CTL-OFF + byte-view cell-view ;
: HEAD-WIDTH ( ^^a -- n ) WIDTH-OFF + byte-view cell-view @ ;

\ Element idx of the block. Growth can replace the mapping, so it fetches the
\ head on every access; the width is read inline so an access stays one call
\ deep.
: ELEMENT ( n ^^a -- ^a )
   {: idx:n cb:^ :}
   cb @  idx  cb WIDTH-OFF + byte-view cell-view @  * + ;

\ Reserves count elements in the block the cell holds.
: ENSURE ( n ^^^a -- )
   {: count:n b:^ :}
   b @ {: cb:^ :}
   count cb  cb HEAD-CTL  cb HEAD-WIDTH  RESERVE ;

\ Releases the block the cell holds.
: EMPTY ( ^^^a -- ) @ dup HEAD-CTL RELEASE ;

\ A HEAP child's routines, each taking the cell holding its record's width in
\ bytes last: a zero-filled record on its own mapping, and its unmap.
: MAP-RECORD ( ^n -- ^a )
   @ {: bytes:n :}
   bytes map-anon 0< if drop bytes MAP-REJECT then ;

: UNMAP-RECORD ( ^a ^n -- )
   @ munmap 0< if
      s" dynamic storage: the system refused to unmap this process's own mapping"
      EXIT-STATUS:SYSTEM die
   then ;

private

CAST: AS-CHILD-RUN ( n -- [ ^u8 n n n ^u8 n -- n ] )
\ The children are made over the entries of ELEMENT, ENSURE and EMPTY.
CAST: AS-ENTRY ( [ R -- S ] -- n )

: CHILD ( ^u8 n n n ^u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

EXPORT CHECK:CHECKER-DYNAMIC-INFO
EXPORT CHECK:CHECKER-LBUF-NAME-OK?
EXPORT CHECK:CHECKER-STORAGE-NAME-REFUSE
EXPORT CHECK:CHECKER-STORAGE-REJECT
EXPORT CHECK:CHECKER-STORAGE-TYPE-REJECT
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:STG-NO-NAME
: DECLARE ( -- )
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   LBUF:STORAGE-LINE-TOKEN {: na:^ nu:n :}
   nu 0= if
      s" DYNAMIC-BUFFER" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu na nu REJECTION:NOT-A-NAME
   LBUF:STORAGE-PARSE-TYPE {: ta:^ tu:n :}
   na nu na nu LBUF:LBUF-NAME-OK? 0= if exit then
   na nu s" -RESERVE" LBUF:LBUF-SUFFIX-OK? 0= if exit then
   na nu s" -RELEASE" LBUF:LBUF-SUFFIX-OK? 0= if exit then
   ta tu CHECKER-DYNAMIC-INFO 0= if
      drop na nu ta tu CHECKER-STORAGE-TYPE-REJECT exit
   then
   {: bytes:n :}
   na nu CHILD-ABI:DYNAMIC ['] ELEMENT AS-ENTRY ta tu CHILD drop
   here {: v:^ :}
   WIDTH-OFF SCRATCH-ALLOT  bytes ,
   na nu s" -RESERVE" LBUF:LBUF-SUFFIXED CHILD-ABI:RESERVE
   ['] ENSURE AS-ENTRY s" " CHILD drop  v LBUF:LDEFER-CELL
   na nu s" -RELEASE" LBUF:LBUF-SUFFIXED CHILD-ABI:RELEASE
   ['] EMPTY AS-ENTRY s" " CHILD drop  v LBUF:LDEFER-CELL ;

\ `HEAP NAME t` declares NAME-NEW ( -- ^t ), a fresh zero-filled record of
\ `t`, and NAME-RELEASE ( ^t -- ), which unmaps it; a pointer kept across
\ RELEASE is stale, and releasing it again can unmap whatever was mapped there
\ since. Nothing registers a record: release it before an image save. Each
\ record is its own OS mapping, so the OS rounds it up to a page; each child
\ holds the declared type's width.
: DECLARE-HEAP ( -- )
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   LBUF:STORAGE-LINE-TOKEN {: na:^ nu:n :}
   nu 0= if
      s" HEAP" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu na nu REJECTION:NOT-A-NAME
   LBUF:STORAGE-PARSE-TYPE {: ta:^ tu:n :}
   na nu CHECKER-LBUF-NAME-OK? 0= if exit then
   na nu LBUF:SEAL-NAME
   na nu s" -NEW" LBUF:LBUF-SUFFIX-OK? 0= if exit then
   na nu s" -RELEASE" LBUF:LBUF-SUFFIX-OK? 0= if exit then
   ta tu CHECKER-DYNAMIC-INFO 0= if
      drop na nu ta tu CHECKER-STORAGE-TYPE-REJECT exit
   then
   {: bytes:n :}
   na nu s" -NEW" LBUF:LBUF-SUFFIXED CHILD-ABI:HEAP-NEW
   ['] MAP-RECORD AS-ENTRY ta tu CHILD drop  bytes ,
   na nu s" -RELEASE" LBUF:LBUF-SUFFIXED CHILD-ABI:HEAP-RELEASE
   ['] UNMAP-RECORD AS-ENTRY ta tu CHILD drop  bytes , ;

global

: DYNAMIC-BUFFER ( -- ) DECLARE ;
INTRINSIC-DECLARATION INTRINSIC
: HEAP ( -- ) DECLARE-HEAP ;
INTRINSIC-DECLARATION INTRINSIC

seal-package

;package
