\ Checked transient allocation behind DYNAMIC-BUFFER declarations.
\ A private control record holds mapping, byte capacity and registry slot + 1.
\ Membership follows live allocation, including a precompiled reserve after
\ restore. Release zeroes the record; no process address survives capture.
\ THE RECORD HEAD IS A DECLARED POINTER CELL (dot habu-refuse-a-ptr-5ad2734e):
\ a raw storage cell never holds an address, so the mapping lives in the
\ PTR-VARIABLE the definer generates and the record arrives here as `ptr ptr a`,
\ a pointer to that cell. `a` stays a quantifier of these words, so a buffer
\ whose cells hold pointers moves them as pointers. The two count cells behind
\ the head hold numbers, so they are reached through an explicit cell view of
\ the head (CTL) rather than through its pointer type.
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

\ Both regions hold the same element type — an abandoned mapping and its
\ replacement, or the old and new registry — so the copy stays parametric in it
\ and a buffer of pointers moves its cells as pointers.
: COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr bytes:n :}
   bytes CELL / 0 ?do src i cells + @ dst i cells + ! loop ;

\ One registry per runtime instance. Empty capacity is retained until capture,
\ so the pointer itself is the authority on whether the mapping is live. The
\ registry holds the address of a mapping, so its own cell is declared too.
PTR-VARIABLE REG
variable REG-U
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- ) begin 0 1 MUTEX atomic-cas 0= until ;
: UNLOCK ( -- ) 0 MUTEX atomic! ;

: REG@ ( -- ptr n ) REG @ ;
: REG-CAP ( -- n ) REG@ @ ;
: REG-BYTES ( n -- n ) REG-HEAD + CELL EXTENT ;
\ A registry entry holds one control record's head, so the entry's own type is
\ a pointer to that declared pointer cell.
: REG-AT ( n -- ptr ptr ptr a ) REG-HEAD + REG@ swap ptr-field ;
: REG-ENTRY ( n -- ptr ptr a ) REG-AT @ ;

\ The count cells sit behind the declared head: view the record's bytes as cells
\ and address them by offset, which keeps the head's pointer type off the counts.
: CTL ( ptr ptr a n -- ptr n ) {: cb:ptr off:n :} cb byte-view off + cell-view ;
: CAP ( ptr ptr a -- ptr n ) CELL CTL ;
: SLOT ( ptr ptr a -- ptr n ) 2 cells CTL ;

: REG-MAP ( n -- ptr n )
   REG-BYTES map-anon 0< if drop E-MAP throw then ;

: REG-CLOSE ( -- )
   REG@ 0= if exit then
   REG@ REG-CAP REG-BYTES munmap 0< if E-UNMAP throw then
   NULL-PTR REG ! ;

\ Capacity is secured before RESERVE publishes either mapping or membership.
\ A failed grow leaves the old registry and every handle intact.
: REG-ROOM ( -- )
   REG@ 0= if
      REG-INIT REG-MAP {: fresh:ptr :}
      REG-INIT fresh !
      fresh REG !
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
   fresh REG ! ;

: REGISTER ( ptr ptr a -- ) {: cb:ptr :}
   cb REG-U @ REG-AT !
   REG-U @ 1+ dup cb SLOT ! REG-U ! ;

\ Swap removal is bounded and repairs the moved record's private handle.
: UNREGISTER ( ptr ptr a -- ) {: cb:ptr :}
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

: RESERVE ( n ptr ptr a n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb CAP @ {: old:n :}
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
      fresh cb !
      cap cb CAP !
      cb REGISTER
      UNLOCK
      exit
   then
   old 0 > if
      cb @ fresh old COPY
      cb @ old munmap 0< if
         fresh cap munmap drop E-UNMAP throw
      then
   then
   fresh cb !
   cap cb CAP ! ;

: RELEASE ( ptr ptr a -- ) {: cb:ptr :}
   cb CAP @ {: cap:n :}
   cap 0= if exit then
   cb @ cap munmap 0< if E-UNMAP throw then
   LOCK
   cb UNREGISTER
   NULL-PTR cb !
   0 cb CAP !
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
