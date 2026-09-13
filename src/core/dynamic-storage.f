\ Checked transient allocation behind DYNAMIC-BUFFER declarations.
\ A private control record holds mapping, byte capacity and registry slot + 1.
\ Membership follows live allocation, including a precompiled reserve after
\ restore. Release zeroes the record; no process address survives capture.
\ This runtime belongs below the core-prefix mark: generated declarations in
\ a rewound build source already call it, before lib/errors.f is available.
package DYNAMIC-STORAGE
private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
7121 constant E-SIZE
7138 constant E-MAP
7139 constant E-UNMAP

$40 constant REG-INIT
1 constant REG-HEAD
3 cells constant CONTROL-BYTES

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need exit then
   need old 2 * 64 max max ;

: COPY ( ptr n ptr n n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

\ One registry per runtime instance. Empty capacity is retained until capture,
\ so the pointer itself is the authority on whether the mapping is live.
create REG 0 ,
variable REG-U
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- ) begin 0 1 MUTEX atomic-cas 0= until ;
: UNLOCK ( -- ) 0 MUTEX atomic! ;

: REG@ ( -- ptr n ) REG 0 ptr-field @ ;
: REG-CAP ( -- n ) REG@ @ ;
: REG-BYTES ( n -- n ) REG-HEAD + CELL EXTENT ;
: REG-AT ( n -- ptr ptr n ) REG-HEAD + REG@ swap ptr-field ;
: REG-ENTRY ( n -- ptr n ) REG-AT @ ;
: SLOT ( ptr n -- ptr n ) 2 cells + ;

: REG-MAP ( n -- ptr n )
   REG-BYTES map-anon 0< if drop E-MAP throw then ;

: REG-CLOSE ( -- )
   REG@ 0= if exit then
   REG@ REG-CAP REG-BYTES munmap 0< if E-UNMAP throw then
   NULL-PTR REG 0 ptr-field ! ;

\ Capacity is secured before RESERVE publishes either mapping or membership.
\ A failed grow leaves the old registry and every handle intact.
: REG-ROOM ( -- )
   REG@ 0= if
      REG-INIT REG-MAP {: fresh:ptr :}
      REG-INIT fresh !
      fresh REG 0 ptr-field !
      exit
   then
   REG-U @ REG-CAP < if exit then
   REG-CAP {: old:n :}
   old MAX-BYTES CELL / 1- 2 / > if E-SIZE throw then
   old 2 * {: cap:n :}
   cap REG-MAP {: fresh:ptr :}
   REG@ fresh old REG-BYTES COPY
   cap fresh !
   REG@ old REG-BYTES munmap 0< if
      fresh cap REG-BYTES munmap drop E-UNMAP throw
   then
   fresh REG 0 ptr-field ! ;

: REGISTER ( ptr n -- ) {: cb:ptr :}
   cb REG-U @ REG-AT !
   REG-U @ 1+ dup cb SLOT ! REG-U ! ;

\ Swap removal is bounded and repairs the moved record's private handle.
: UNREGISTER ( ptr n -- ) {: cb:ptr :}
   cb SLOT @ 1- {: at:n :}
   REG-U @ 1- {: last:n :}
   at last <> if
      last REG-ENTRY {: moved:ptr :}
      moved at REG-AT !
      at 1+ moved SLOT !
   then
   NULL-PTR last REG-AT !
   last REG-U !
   0 cb SLOT ! ;

public

: RESERVE ( n ptr n n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb cell+ @ {: old:n :}
   need old <= if exit then
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
      fresh cb 0 ptr-field !
      cap cb cell+ !
      cb REGISTER
      UNLOCK
      exit
   then
   old 0 > if
      cb 0 ptr-field @ fresh old COPY
      cb 0 ptr-field @ old munmap 0< if
         fresh cap munmap drop E-UNMAP throw
      then
   then
   fresh cb 0 ptr-field !
   cap cb cell+ ! ;

: RELEASE ( ptr n -- ) {: cb:ptr :}
   cb cell+ @ {: cap:n :}
   cap 0= if exit then
   cb 0 ptr-field @ cap munmap 0< if E-UNMAP throw then
   LOCK
   cb UNREGISTER
   NULL-PTR cb 0 ptr-field !
   0 cb cell+ !
   UNLOCK ;

: REGISTERED-N ( -- n ) REG-U @ ;
: DIRTY-N ( -- n ) REG-U @ ;

private

\ Refuse a cut through a live control record before releasing any member.
: RANGE-CHECK ( ptr u8 n -- ) {: start:ptr size:n :}
   size 0 < if E-SIZE throw then
   size 0= if exit then
   REG-U @ 0 ?do
      i REG-ENTRY byte-view start - {: off:n :}
      off 0 < if
         off CONTROL-BYTES + 0 > if E-SIZE throw then
      else
         off size < if
            CONTROL-BYTES size off - > if E-SIZE throw then
         then
      then
   loop ;

public

\ Removal shrinks the registry, so each successful release consumes its last
\ entry. A refused unmap preserves that entry for the next preparation attempt.
\ Capture runs after tasks stop, as IMAGE-LIFECYCLE:PREPARE requires, so registry
\ walks need no second lock. Concurrent mutation of one buffer remains its
\ caller's responsibility; the lock protects membership of distinct controls.
: RELEASE-ALL ( -- )
   begin REG-U @ 0 > while REG-U @ 1- REG-ENTRY RELEASE repeat
   REG-CLOSE ;

\ A retained runtime can own both capture-window and writer buffers. Only
\ records wholly in this DATA span belong to that captured value.
: RELEASE-RANGE ( ptr u8 n -- ) {: start:ptr size:n :}
   start size RANGE-CHECK
   REG-U @
   begin dup 0 > while
      1- dup REG-ENTRY {: cb:ptr :}
      cb byte-view start - {: off:n :}
      off 0 >= off size < and if cb RELEASE then
   repeat drop
   REG-U @ 0= if REG-CLOSE then ;

;package
