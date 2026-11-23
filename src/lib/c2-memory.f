split-str
\ Scoped C2 allocation, nonowning loans, and append-only byte allocations owned
\ by an explicit checked owner handle (rules in docs/design.md "Ownership"). The
\ WITH- scope words declare raw effects until their exact native image entries
\ receive the checker-owned scope kinds. Cleanup runs on a task-local frame
\ chain: a frame is PENDING before any allocation can return, LIVE once its
\ acquisitions are recorded, then CLOSING -> CLOSED as disposers run; at most
\ FRAME-CAP nest (E-C2-CAPACITY).
\ STORAGE CLASS. TASK-LOCAL: the chain's head cells and its FRAME-CAP frames are
\ TASK:+USER and TASK:+USERS rows.
require src/lib/error.f
require src/lib/task.f
require src/lib/memory.f
require src/lib/num-types.f

package C2-MEM
private

CAST: INIT-LEN>N ( NUM:alloc-byte-len -- n )

0 constant CLOSED
1 constant PENDING
2 constant LIVE
3 constant CLOSING
1 constant BYTES-FRAME
2 constant INIT-FRAME
32 constant FRAME-CAP

\ Each append allocation, and each chunk that holds them, starts with
\ this header; its bytes follow it.
STRUCTURE append DERIVE addr
   FIELD next ^append
   FIELD length n
   FIELD disposer [ ^u8 NUM:alloc-byte-len -- ]
;STRUCTURE

: APPEND-HEADER-BYTES ( -- n ) [type] append width>n ;
MEM-MAX-N APPEND-HEADER-BYTES 2 * - CELL 1- - constant MAX-APPEND

\ c2-init-stow and c2-records-stow write state, resource, length and
\ original-bound at byte offsets 0, 16, 24 and 40: the field order is fixed.
STRUCTURE frame DERIVE addr
   FIELD state n
   FIELD prev n
   FIELD resource ^u8
   FIELD length n
   FIELD disposer [ ^u8 NUM:alloc-byte-len -- ]
   FIELD original-bound n
   FIELD kind n
   FIELD append-head ^append
   FIELD append-pending ^append
   FIELD append-cursor ^u8
   FIELD append-remain n
;STRUCTURE

STRUCTURE meta DERIVE addr
   FIELD head n FIELD depth n FIELD hook n FIELD loan-depth n FIELD init-bound n
;STRUCTURE

\ +USERS resolves this row through the calling task's data-base. Main has the
\ same row in its own data region, although TASK:SELF names no TCB there.
TASK:+USER META-ROW meta
FRAME-CAP TASK:+USERS FRAMES frame

: HEAD-CELL ( -- ^n ) META-ROW META-HEAD ;
: DEPTH-CELL ( -- ^n ) META-ROW META-DEPTH ;
: HOOK-CELL ( -- ^n ) META-ROW META-HOOK ;
: LOAN-DEPTH-CELL ( -- ^n ) META-ROW META-LOAN-DEPTH ;
: INIT-BOUND-CELL ( -- ^n ) META-ROW META-INIT-BOUND ;

CAST: >APPEND ( ^u8 -- ^append )

: HEAD-FRAME ( -- ^frame )
   HEAD-CELL @ 1- FRAMES ;

: LOAN-ENTER ( -- ) 1 LOAN-DEPTH-CELL +! ;
: LOAN-LEAVE ( -- ) -1 LOAN-DEPTH-CELL +! ;

\ Called only on a live BYTES head, whose length MEM:ALLOC-BYTES returned. Its
\ CLOSING state while the adapter runs is a halted task's one cleanup record.
: DISPOSE-HEAD ( -- )
   HEAD-FRAME {: frame:^ :}
   frame FRAME-RESOURCE @
   frame FRAME-LENGTH @ MEM:BYTES-ALLOC-LEN
   frame FRAME-DISPOSER @ execute ;

\ c2-init-stow or c2-records-stow stores an INIT head's extent, 0 for an empty
\ table. The enclosing BYTES frame owns the mapping and its release.
: INIT-CLEAR ( -- )
   HEAD-FRAME {: frame:^ :}
   frame FRAME-RESOURCE @ {: resource:^ :}
   frame FRAME-LENGTH @ 0 ?do 0 resource i + c! loop ;

: DISPOSE-APPEND ( -- )
   HEAD-FRAME FRAME-APPEND-PENDING @ {: header:^ :}
   header BYTE-VIEW APPEND-HEADER-BYTES +
   header APPEND-LENGTH @ MEM:BYTES-ALLOC-LEN
   header APPEND-DISPOSER @ execute ;

: RELEASE-APPEND-CHUNK ( ^u8 NUM:alloc-byte-len -- )
   swap APPEND-HEADER-BYTES - swap MEM:RELEASE-BYTES ;

: FIRST-ERROR ( n n -- n ) {: first:n rc:n :}
   first 0<> if first else rc then ;

: CLOSE-APPENDS ( -- n )
   0
   begin HEAD-FRAME FRAME-APPEND-HEAD @ dup NULL-PTR <> while
      {: header:^ :}
      header APPEND-NEXT @ HEAD-FRAME FRAME-APPEND-HEAD !
      header HEAD-FRAME FRAME-APPEND-PENDING !
      [: DISPOSE-APPEND ;] catch FIRST-ERROR
      NULL-PTR HEAD-FRAME FRAME-APPEND-PENDING !
   repeat drop ;

: CLOSE-BYTES ( -- n )
   NULL$ drop HEAD-FRAME FRAME-APPEND-CURSOR !
   0 HEAD-FRAME FRAME-APPEND-REMAIN !
   CLOSE-APPENDS
   [: DISPOSE-HEAD ;] catch FIRST-ERROR ;

: CLOSE ( -- )
   TASK:DEFER-ENTER
   HEAD-CELL @ 0= if TASK:DEFER-LEAVE E-C2-STATE throw then
   HEAD-FRAME {: frame:^ :}
   frame FRAME-STATE @ {: state:n :}
   state PENDING <> state LIVE <> and if
      TASK:DEFER-LEAVE E-C2-STATE throw
   then
   CLOSING frame FRAME-STATE !
   state LIVE = if
      frame FRAME-KIND @ BYTES-FRAME = if CLOSE-BYTES
      else INIT-CLEAR 0 then
   else 0 then {: rc:n :}
   NULL$ drop frame FRAME-RESOURCE !
   CLOSED frame FRAME-STATE !
   frame FRAME-PREV @ HEAD-CELL !
   -1 DEPTH-CELL +!
   TASK:DEFER-LEAVE
   state PENDING = if TASK:DEFER-LEAVE then
   rc 0<> if rc throw then ;

\ A task's first scope registers this as its one TASK:AT-EXIT hook (main
\ registers none). It closes every remaining frame innermost first, past a
\ disposer's error; TASK's exit chain keeps the body's first error, or this
\ drain's. A halt at TASK:PAUSE unwinds through every scope's c2-invoke cleanup
\ and FINISH serves a halt left pending by a deferred close, so this has no
\ reachable case left.
: DRAIN ( -- )
   0
   begin HEAD-CELL @ 0 <> while
      [: CLOSE ;] catch FIRST-ERROR
   repeat
   dup 0<> if throw then drop ;

: REGISTER-EXIT ( -- )
   TASK:SELF-N 0= if exit then
   HOOK-CELL @ 0<> if exit then
   [: DRAIN ;] TASK:SELF TASK:AT-EXIT
   1 HOOK-CELL ! ;

\ Registration and capacity precede all acquisition: the pending row exists
\ before an allocation can return, and halt is deferred (TASK:DEFER-ENTER)
\ across its transfer to LIVE and while a close runs. A pending frame disposes
\ only what it recorded.
: OPEN ( -- )
   DEPTH-CELL @ FRAME-CAP >= if E-C2-CAPACITY throw then
   REGISTER-EXIT
   TASK:DEFER-ENTER
   DEPTH-CELL @ FRAMES {: frame:^ :}
   HEAD-CELL @ frame FRAME-PREV !
   BYTES-FRAME frame FRAME-KIND !
   NULL-PTR frame FRAME-APPEND-HEAD !
   NULL-PTR frame FRAME-APPEND-PENDING !
   NULL$ drop frame FRAME-APPEND-CURSOR !
   0 frame FRAME-APPEND-REMAIN !
   PENDING frame FRAME-STATE !
   DEPTH-CELL @ 1+ dup DEPTH-CELL ! HEAD-CELL ! ;

\ Straight-line, nonthrowing transfer immediately after MEM returns the pair.
: RECORD ( ^u8 NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- )
   {: resource:^ size dispose :}
   HEAD-FRAME {: frame:^ :}
   resource frame FRAME-RESOURCE !
   size INIT-LEN>N frame FRAME-LENGTH !
   dispose frame FRAME-DISPOSER !
   LIVE frame FRAME-STATE !
   TASK:DEFER-LEAVE ;

: INIT-OPEN ( -- )
   OPEN
   INIT-FRAME HEAD-FRAME FRAME-KIND ! ;

\ BIND discovers authority only in this task's live BYTES chain. The base is
\ supplied by WITH-INIT, which already holds the unique root byte view.
: ROOT-FROM ( ^u8 n -- ^frame ) {: base:^ id:n :}
   id 0= if E-C2-STATE throw then
   id 1- FRAMES {: frame:^ :}
   frame FRAME-STATE @ LIVE =
   frame FRAME-KIND @ BYTES-FRAME = and
   frame FRAME-RESOURCE @ base = and if frame exit then
   base frame FRAME-PREV @ recurse ;

: ROOT-FRAME ( ^u8 -- ^frame ) HEAD-CELL @ ROOT-FROM ;

: FRAME-IN? ( ^frame n -- bool ) {: target:^ id:n :}
   id 0= if false exit then
   id 1- FRAMES {: frame:^ :}
   frame target = if
      frame FRAME-STATE @ LIVE =
      frame FRAME-KIND @ BYTES-FRAME = and exit then
   target frame FRAME-PREV @ recurse ;

: LIVE-BYTES-FRAME? ( ^frame -- bool ) HEAD-CELL @ FRAME-IN? ;

CAST: >NODE-CELL ( ^u8 -- ^^frame )

: BIND-STATE ( str -- str )
   {: base:^ bound:n :}
   base ROOT-FRAME base >NODE-CELL !
   base bound ;

: OWNER-FRAME ( ^u8 -- ^frame )
   >NODE-CELL @ dup LIVE-BYTES-FRAME? 0= if E-C2-STATE throw then ;

: APPEND-RECORD-BYTES ( n -- n )
   CELL 1- + CELL negate and APPEND-HEADER-BYTES + ;

\ A chunk begins with a release node in the same LIFO chain as its payloads.
\ The next chunk leaves any unused tail of the old one until owner close.
: APPEND-CHUNK ( ^frame n -- ) {: frame:^ record-bytes:n :}
   record-bytes APPEND-HEADER-BYTES + MEM-64K max
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: bytes:^ length:NUM:alloc-byte-len :}
   bytes >APPEND {: chunk:^ :}
   \ Register ownership before any further fallible operation or yield.
   frame FRAME-APPEND-HEAD @ chunk APPEND-NEXT !
   length INIT-LEN>N chunk APPEND-LENGTH !
   [: RELEASE-APPEND-CHUNK ;] chunk APPEND-DISPOSER !
   chunk frame FRAME-APPEND-HEAD !
   bytes APPEND-HEADER-BYTES + frame FRAME-APPEND-CURSOR !
   length INIT-LEN>N APPEND-HEADER-BYTES - frame FRAME-APPEND-REMAIN ! ;

: APPEND-BODY ( ^frame NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- str )
   {: frame:^ size:NUM:alloc-byte-len dispose :}
   size INIT-LEN>N {: bytes:n :}
   bytes MAX-APPEND > if E-MEM-SIZE throw then
   bytes APPEND-RECORD-BYTES {: record-bytes:n :}
   frame FRAME-APPEND-REMAIN @ record-bytes < if
      frame record-bytes APPEND-CHUNK
   then
   frame FRAME-APPEND-CURSOR @ {: at:^ :}
   at >APPEND {: header:^ :}
   frame FRAME-APPEND-HEAD @ header APPEND-NEXT !
   bytes header APPEND-LENGTH !
   dispose header APPEND-DISPOSER !
   header frame FRAME-APPEND-HEAD !
   at record-bytes + frame FRAME-APPEND-CURSOR !
   frame FRAME-APPEND-REMAIN @ record-bytes - frame FRAME-APPEND-REMAIN !
   at APPEND-HEADER-BYTES + bytes ;

: APPEND ( ^frame NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- ^u8 n )
   TASK:DEFER-ENTER
   [: APPEND-BODY ;] [: TASK:DEFER-LEAVE ;] finally ;

\ c2-init-stow is the narrow machine transfer: it validates the byte view and
\ descriptors, arms the clear-only frame, then moves the fixed cell bundle.
\ INIT-RUN's c2-invoke body catches a rejection and retires the pending frame.
: INIT-STOW ( R str n [ R str -- S str | U -- U ] stow-layout | U -- R str [ R str -- S str | U -- U ] | U )
   HEAD-FRAME BYTE-VIEW c2-init-stow
   TASK:DEFER-LEAVE ;

: RECORDS-STOW ( R str n n [ R str -- S str | U -- U ] stow-layout | U -- R str [ R str -- S str | U -- U ] | U )
   HEAD-FRAME BYTE-VIEW c2-records-stow
   TASK:DEFER-LEAVE ;

: INIT-CLOSE ( -- )
   HEAD-FRAME FRAME-ORIGINAL-BOUND @ INIT-BOUND-CELL !
   CLOSE ;

: INIT-RESTORE ( R str -- R str )
   drop INIT-BOUND-CELL @ ;

: ACQUIRE-BYTES ( NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- ^u8 NUM:alloc-byte-len )
   {: size dispose :}
   size MEM:ALLOC-BYTES {: resource:^ actual :}
   resource actual dispose RECORD
   resource actual ;

: RUN-BODY ( R [ R -- S ] -- S ) execute ;

\ The outermost scope serves a halt that stayed pending while its cleanup ran
\ under DEFER-ENTER, carrying the code its body and cleanup selected, so a
\ catch around the scope never resumes and the task's error is that code. A
\ halt already unwinding through the scope continues from c2-invoke's tail.
: FINISH ( n -- )
   HEAD-CELL @ 0= if TASK:HALT-UNWIND else drop then ;

: INIT-RUN ( R str n [ R str -- S str | U -- U ] stow-layout | U -- S str | U )
   INIT-OPEN
   [: INIT-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

: RECORDS-RUN ( R str n n [ R str -- S str | U -- U ] stow-layout | U -- S str | U )
   INIT-OPEN
   [: RECORDS-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

: INVOKE ( R [ R -- S | U -- U ] | U -- S | U )
   [: RUN-BODY ;] [: CLOSE ;] [: FINISH ;] c2-invoke ;

: RUN ( R [ R -- S | U -- U ] | U -- S | U )
   OPEN
   INVOKE ;

CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

: ALLOC-RUN ( R NUM:alloc-byte-len [ R str -- S str | U -- U ] | U -- S | U )
   {: body :}
   [: MEM:RELEASE-BYTES ;] ACQUIRE-BYTES {: resource:^ size:NUM:alloc-byte-len :}
   resource size ALLOC-LEN>N body execute 2drop ;

\ A child callback's returned control cells carry no disposal authority.
: LOAN-BODY ( R str [ R str -- S str ] -- S str )
   execute ;

: LOAN-RUN ( R str [ R str -- S str | U -- U ] | U -- S str | U )
   LOAN-ENTER
   [: LOAN-BODY ;] [: LOAN-LEAVE ;] finally ;

\ The view representation casts: a view is two cells, base and byte bound.
\ Only C2-MEM's private section may pack; a pack keeps or narrows the cells it
\ unpacked, or wraps the fresh allocation APPEND returns (ALLOC-DISPOSE).
CAST: READ-UNPACK ( read-view<p,q,u8> -- ^u8 n )
CAST: MUT-UNPACK ( mut-view<p,q,a,u8> -- ^u8 n )
CAST: READ-PACK ( ^u8 n -- read-view<p,q,u8> )
CAST: MUT-PACK ( ^u8 n -- mut-view<p,q,a,u8> )

public

: WITH-MUT ( R NUM:alloc-byte-len [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S | U )
   [: ALLOC-RUN ;] RUN ;

: WITH-READ ( R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S ^u8 n | U )
   {: parent:^ bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

: WITH-MUT-LOAN ( R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S ^u8 n | U )
   {: parent:^ bound:n callback :}
   parent bound callback LOAN-RUN 2drop parent bound ;

\ WITH-INIT's callback may declare `forall<i inside [l,T],[ ... ]>`: fresh `i`
\ is inside the byte view's lifetime `l` and every scope dependency of the
\ initial record `T`. It takes a live unique byte view and a complete,
\ non-unique, non-stale fixed-cell product; a short capacity, incompatible
\ field schema, open, unique or stale record, raw or shared authority,
\ incompatible callback, or escape of `i` refuses. Return clears the
\ initialized extent and restores the original view and bound; throw and task
\ halt close the scope.
: WITH-INIT ( R ^u8 n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- S ^u8 n | U )
   INIT-RUN ;

\ WITH-RECORDS gives a table one owning scope: a unique byte view, a
\ nonnegative count (zero is valid) and a committed, non-owning `DERIVE init`
\ seed whose count times record size fits the original bound. Under
\ `forall<i inside [l,T],...>` its callback gets opaque linear
\ `records<p,i,a,T>`. Closing the table clears the whole initialized extent
\ once and restores the original bound.
: WITH-RECORDS ( R ^u8 n n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- S ^u8 n | U )
   RECORDS-RUN ;

\ WITH-RECORD takes a zero-based index below the count and lends
\ `mut-view<p,j,a,init<i,T>>` under `forall<j inside i,...>`; `j` cannot
\ escape, and the element is neither reconstructed nor cleared on return.
: WITH-RECORD ( R ^u8 n n [ R ^u8 n -- S ^u8 n | U -- U ] n n n | U -- S ^u8 n | U )
   {: width:n stride:n align:n :}
   {: base:^ count:n index:n callback :}
   base index stride * + stride callback LOAN-RUN 2drop
   base count ;

\ WITH-FIELD name borrows an initialized field of the receiver's committed
\ instantiated schema: a live unique `mut-view<p,i,a,init<i,T>>` lends a child
\ with field type `F`, fresh scope `j` inside `i`, init scope `i` and region
\ `field(a,name)`; throw and task halt close the child loan.
\ The field offset and extent are emitted from the checker's committed layout
\ fact for this exact call. The callback owns only that span; the parent's
\ original base and bound return unchanged even when the field was edited.
: WITH-FIELD ( R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] n n | U -- S ^u8 n | U )
   {: off:n extent:n :}
   {: base:^ bound:n callback :}
   base off + extent callback LOAN-RUN 2drop
   base bound ;

\ The index is below the view's bound.
: BYTE@ ( read-view<p,q,u8> n -- u8 read-view<p,q,u8> )
   {: idx:n :}
   dup READ-UNPACK drop {: base:^ :}
   base idx + c@ swap ;

\ The index is below the view's bound.
: MUT-BYTE@ ( mut-view<p,q,a,u8> n -- u8 mut-view<p,q,a,u8> )
   swap MUT-UNPACK {: idx:n base:^ bound:n :}
   base idx + c@ base bound MUT-PACK ;

\ The index is below the view's bound.
: MUT-BYTE! ( mut-view<p,q,a,u8> n u8 -- mut-view<p,q,a,u8> )
   rot MUT-UNPACK {: idx:n value:u8 base:^ bound:n :}
   value base idx + c!
   base bound MUT-PACK ;

\ The offset and count lie within the view's bound.
: SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> )
   {: off:n count:n :}
   READ-UNPACK drop {: base:^ :}
   base off + count READ-PACK ;

\ The count lies within the view's bound.
: PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> )
   {: count:n :}
   MUT-UNPACK drop {: base:^ :}
   base count MUT-PACK ;

: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   dup READ-UNPACK nip swap ;

: MUT-LENGTH ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   MUT-UNPACK {: base:^ bound:n :}
   bound base bound MUT-PACK ;

STRUCTURE owner-state OPAQUE FIELD node ^frame ;STRUCTURE
STRUCTURE owner<a,b,c> OPAQUE FIELD state mut-view<a,b,c,init<b,owner-state>> ;STRUCTURE

private

CAST: OWNER-VIEW-UNPACK ( mut-view<p,i,a,init<i,owner-state>> -- ^u8 n )
CAST: OWNER-VIEW-PACK ( ^u8 n -- mut-view<p,i,a,init<i,owner-state>> )
\ APPEND stores a raw disposer and calls it with the allocation's base and
\ length; calling the cast disposer packs those cells into the view it takes.
CAST: >CELL-DISPOSER ( [ mut-view<p,q,a,u8> -- ] -- [ ^u8 n -- ] )
CAST: >ALLOC-DISPOSER ( [ ^u8 n -- ] -- [ ^u8 NUM:alloc-byte-len -- ] )

public

: OWNER-SIZE ( -- NUM:alloc-byte-len ) 8 MEM:BYTES-ALLOC-LEN ;
: SEED-OWNER ( -- owner-state ) NULL-PTR OWNER-STATE-MAKE ;
: BIND ( mut-view<p,i,a,init<i,owner-state>> -- owner<p,i,a> ) OWNER-VIEW-UNPACK BIND-STATE OWNER-VIEW-PACK OWNER-MAKE ;
: UNBIND ( owner<p,i,a> -- mut-view<p,i,a,init<i,owner-state>> ) OWNER-UNMAKE ;

\ The disposer consumes the allocation's unique view at the owner's close.
: ALLOC-DISPOSE ( owner<p,i,a> NUM:alloc-byte-len [ mut-view<p,p,fresh-region-b,u8> -- ] -- owner<p,i,a> mut-view<p,p,fresh-region-b,u8> )
   >CELL-DISPOSER >ALLOC-DISPOSER {: size:NUM:alloc-byte-len dispose :}
   OWNER-UNMAKE OWNER-VIEW-UNPACK {: base:^ bound:n :}
   base OWNER-FRAME size dispose APPEND {: resource:^ extent:n :}
   base bound OWNER-VIEW-PACK OWNER-MAKE resource extent MUT-PACK ;

: PUBLISH ( mut-view<p,l,a,u8> -- read-view<p,l,u8> ) MUT-UNPACK READ-PACK ;

: ALLOC ( owner<p,i,a> NUM:alloc-byte-len -- owner<p,i,a> mut-view<p,p,fresh-region-a,u8> )
   [: PUBLISH drop ;] ALLOC-DISPOSE ;

\ Only C2-MEM's private section packs a view, so no later source may reopen it.
seal-package

;package
