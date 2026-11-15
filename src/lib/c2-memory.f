\ Scoped C2 allocation, nonowning loans, and append-only byte allocations owned
\ by an explicit checked owner handle (rules in docs/design.md "Ownership"). The
\ WITH- scope words declare raw effects until their exact native image entries
\ receive the checker-owned scope kinds. Cleanup runs on a task-local frame
\ chain: a frame is PENDING before any allocation can return, LIVE once its
\ acquisitions are recorded, then CLOSING -> CLOSED as disposers run; at most
\ FRAME-CAP nest (E-C2-CAPACITY).
\ STORAGE CLASS. TASK-LOCAL: the frame chain is one TASK:+USERS byte row
\ (OWNER-REGION).
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
24 constant APPEND-HEADER-BYTES
MEM-MAX-N APPEND-HEADER-BYTES 2 * - CELL 1- - constant MAX-APPEND
32 constant FRAME-CAP
88 constant FRAME-BYTES
40 constant META-BYTES
META-BYTES FRAME-CAP FRAME-BYTES * + constant REGION-BYTES

\ +USERS resolves this row through the calling task's data-base. Main has the
\ same row in its own data region, although TASK:SELF names no TCB there.
REGION-BYTES TASK:+USERS OWNER-REGION u8

: HEAD-CELL ( -- ^n ) 0 OWNER-REGION CELL-VIEW ;
: DEPTH-CELL ( -- ^n ) 8 OWNER-REGION CELL-VIEW ;
: HOOK-CELL ( -- ^n ) 16 OWNER-REGION CELL-VIEW ;
: LOAN-DEPTH-CELL ( -- ^n ) 24 OWNER-REGION CELL-VIEW ;
: INIT-BOUND-CELL ( -- ^n ) 32 OWNER-REGION CELL-VIEW ;

: FRAME-AT ( n -- ^n )
   FRAME-BYTES * META-BYTES + OWNER-REGION CELL-VIEW ;

: STATE-CELL ( ^n -- ^n ) ;
: PREV-CELL ( ^n -- ^n ) 8 + ;

\ OWNER-REGION is a byte row. The casts below retype its cells' pointees;
\ they are C2-MEM's private words, so nothing outside C2-MEM can name them.
CAST: >PTR-CELL ( ^n -- ^^u8 )
CAST: >DISPOSER-CELL ( ^n -- ^[ ^u8 NUM:alloc-byte-len -- ] )
CAST: >APPEND-PTR-CELL ( ^u8 -- ^^u8 )
CAST: >APPEND-LEN-CELL ( ^u8 -- ^n )
CAST: >APPEND-DISPOSER-CELL ( ^u8 -- ^[ ^u8 NUM:alloc-byte-len -- ] )

: RESOURCE-CELL ( ^n -- ^^u8 ) 16 + >PTR-CELL ;
: LENGTH-CELL ( ^n -- ^n ) 24 + ;
: DISPOSER-CELL ( ^n -- ^[ ^u8 NUM:alloc-byte-len -- ] ) 32 + >DISPOSER-CELL ;
: ORIGINAL-BOUND-CELL ( ^n -- ^n ) 40 + ;
: KIND-CELL ( ^n -- ^n ) 48 + ;
: APPEND-HEAD-CELL ( ^n -- ^^u8 ) 56 + >PTR-CELL ;
: APPEND-PENDING-CELL ( ^n -- ^^u8 ) 64 + >PTR-CELL ;
: APPEND-CURSOR-CELL ( ^n -- ^^u8 ) 72 + >PTR-CELL ;
: APPEND-REMAIN-CELL ( ^n -- ^n ) 80 + ;

: APPEND-NEXT-CELL ( ^u8 -- ^^u8 ) >APPEND-PTR-CELL ;
: APPEND-LENGTH-CELL ( ^u8 -- ^n ) 8 + >APPEND-LEN-CELL ;
: APPEND-DISPOSER-CELL ( ^u8 -- ^[ ^u8 NUM:alloc-byte-len -- ] ) 16 + >APPEND-DISPOSER-CELL ;

: HEAD-FRAME ( -- ^n )
   HEAD-CELL @ 1- FRAME-AT ;

: LOAN-ENTER ( -- ) 1 LOAN-DEPTH-CELL +! ;
: LOAN-LEAVE ( -- ) -1 LOAN-DEPTH-CELL +! ;

\ Called only on a live BYTES head, whose length MEM:ALLOC-BYTES returned. Its
\ CLOSING state while the adapter runs is a halted task's one cleanup record.
: DISPOSE-HEAD ( -- )
   HEAD-FRAME {: frame:^ :}
   frame RESOURCE-CELL @
   frame LENGTH-CELL @ MEM:BYTES-ALLOC-LEN
   frame DISPOSER-CELL @ execute ;

\ c2-init-stow or c2-records-stow stores an INIT head's extent, 0 for an empty
\ table. The enclosing BYTES frame owns the mapping and its release.
: INIT-CLEAR ( -- )
   HEAD-FRAME {: frame:^ :}
   frame RESOURCE-CELL @ {: resource:^ :}
   frame LENGTH-CELL @ 0 ?do 0 resource i + c! loop ;

: DISPOSE-APPEND ( -- )
   HEAD-FRAME APPEND-PENDING-CELL @ {: header:^ :}
   header APPEND-HEADER-BYTES +
   header APPEND-LENGTH-CELL @ MEM:BYTES-ALLOC-LEN
   header APPEND-DISPOSER-CELL @ execute ;

: RELEASE-APPEND-CHUNK ( ^u8 NUM:alloc-byte-len -- )
   swap APPEND-HEADER-BYTES - swap MEM:RELEASE-BYTES ;

: FIRST-ERROR ( n n -- n ) {: first:n rc:n :}
   first 0<> if first else rc then ;

: CLOSE-APPENDS ( -- n )
   0
   begin HEAD-FRAME APPEND-HEAD-CELL @ dup NULL$ drop <> while
      {: header:^ :}
      header APPEND-NEXT-CELL @ HEAD-FRAME APPEND-HEAD-CELL !
      header HEAD-FRAME APPEND-PENDING-CELL !
      [: DISPOSE-APPEND ;] catch FIRST-ERROR
      NULL$ drop HEAD-FRAME APPEND-PENDING-CELL !
   repeat drop ;

: CLOSE-BYTES ( -- n )
   NULL$ drop HEAD-FRAME APPEND-CURSOR-CELL !
   0 HEAD-FRAME APPEND-REMAIN-CELL !
   CLOSE-APPENDS
   [: DISPOSE-HEAD ;] catch FIRST-ERROR ;

: CLOSE ( -- )
   TASK:DEFER-ENTER
   HEAD-CELL @ 0= if TASK:DEFER-LEAVE E-C2-STATE throw then
   HEAD-FRAME {: frame:^ :}
   frame STATE-CELL @ {: state:n :}
   state PENDING <> state LIVE <> and if
      TASK:DEFER-LEAVE E-C2-STATE throw
   then
   CLOSING frame STATE-CELL !
   state LIVE = if
      frame KIND-CELL @ BYTES-FRAME = if CLOSE-BYTES
      else INIT-CLEAR 0 then
   else 0 then {: rc:n :}
   NULL$ drop frame RESOURCE-CELL !
   CLOSED frame STATE-CELL !
   frame PREV-CELL @ HEAD-CELL !
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
   DEPTH-CELL @ FRAME-AT {: frame:^ :}
   HEAD-CELL @ frame PREV-CELL !
   BYTES-FRAME frame KIND-CELL !
   NULL$ drop frame APPEND-HEAD-CELL !
   NULL$ drop frame APPEND-PENDING-CELL !
   NULL$ drop frame APPEND-CURSOR-CELL !
   0 frame APPEND-REMAIN-CELL !
   PENDING frame STATE-CELL !
   DEPTH-CELL @ 1+ dup DEPTH-CELL ! HEAD-CELL ! ;

\ Straight-line, nonthrowing transfer immediately after MEM returns the pair.
: RECORD ( ^u8 NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- )
   {: resource:^ size dispose :}
   HEAD-FRAME {: frame:^ :}
   resource frame RESOURCE-CELL !
   size INIT-LEN>N frame LENGTH-CELL !
   dispose frame DISPOSER-CELL !
   LIVE frame STATE-CELL !
   TASK:DEFER-LEAVE ;

: INIT-OPEN ( -- )
   OPEN
   INIT-FRAME HEAD-FRAME KIND-CELL ! ;

\ BIND discovers authority only in this task's live BYTES chain. The base is
\ supplied by WITH-INIT, which already holds the unique root byte view.
: ROOT-FROM ( ^u8 n -- ^n ) {: base:^ id:n :}
   id 0= if E-C2-STATE throw then
   id 1- FRAME-AT {: frame:^ :}
   frame STATE-CELL @ LIVE =
   frame KIND-CELL @ BYTES-FRAME = and
   frame RESOURCE-CELL @ base = and if frame exit then
   base frame PREV-CELL @ recurse ;

: ROOT-FRAME ( ^u8 -- ^n ) HEAD-CELL @ ROOT-FROM ;

: FRAME-IN? ( ^n n -- bool ) {: target:^ id:n :}
   id 0= if false exit then
   id 1- FRAME-AT {: frame:^ :}
   frame target = if
      frame STATE-CELL @ LIVE =
      frame KIND-CELL @ BYTES-FRAME = and exit then
   target frame PREV-CELL @ recurse ;

: LIVE-BYTES-FRAME? ( ^n -- bool ) HEAD-CELL @ FRAME-IN? ;

CAST: >NODE-CELL ( ^u8 -- ^^n )

: BIND-STATE ( ^u8 n -- ^u8 n )
   {: base:^ bound:n :}
   base ROOT-FRAME base >NODE-CELL !
   base bound ;

: OWNER-FRAME ( ^u8 -- ^n )
   >NODE-CELL @ dup LIVE-BYTES-FRAME? 0= if E-C2-STATE throw then ;

: APPEND-RECORD-BYTES ( n -- n )
   CELL 1- + CELL negate and APPEND-HEADER-BYTES + ;

\ A chunk begins with a release node in the same LIFO chain as its payloads.
\ The next chunk leaves any unused tail of the old one until owner close.
: APPEND-CHUNK ( ^n n -- ) {: frame:^ record-bytes:n :}
   record-bytes APPEND-HEADER-BYTES + MEM-64K max
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: chunk:^ length:NUM:alloc-byte-len :}
   \ Register ownership before any further fallible operation or yield.
   frame APPEND-HEAD-CELL @ chunk APPEND-NEXT-CELL !
   length INIT-LEN>N chunk APPEND-LENGTH-CELL !
   [: RELEASE-APPEND-CHUNK ;] chunk APPEND-DISPOSER-CELL !
   chunk frame APPEND-HEAD-CELL !
   chunk APPEND-HEADER-BYTES + frame APPEND-CURSOR-CELL !
   length INIT-LEN>N APPEND-HEADER-BYTES - frame APPEND-REMAIN-CELL ! ;

: APPEND-BODY ( ^n NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- ^u8 n )
   {: frame:^ size:NUM:alloc-byte-len dispose :}
   size INIT-LEN>N {: bytes:n :}
   bytes MAX-APPEND > if E-MEM-SIZE throw then
   bytes APPEND-RECORD-BYTES {: record-bytes:n :}
   frame APPEND-REMAIN-CELL @ record-bytes < if
      frame record-bytes APPEND-CHUNK
   then
   frame APPEND-CURSOR-CELL @ {: header:^ :}
   frame APPEND-HEAD-CELL @ header APPEND-NEXT-CELL !
   bytes header APPEND-LENGTH-CELL !
   dispose header APPEND-DISPOSER-CELL !
   header frame APPEND-HEAD-CELL !
   header record-bytes + frame APPEND-CURSOR-CELL !
   frame APPEND-REMAIN-CELL @ record-bytes - frame APPEND-REMAIN-CELL !
   header APPEND-HEADER-BYTES + bytes ;

: APPEND ( ^n NUM:alloc-byte-len [ ^u8 NUM:alloc-byte-len -- ] -- ^u8 n )
   TASK:DEFER-ENTER
   [: APPEND-BODY ;] [: TASK:DEFER-LEAVE ;] finally ;

\ c2-init-stow is the narrow machine transfer: it validates the byte view and
\ descriptors, arms the clear-only frame, then moves the fixed cell bundle.
\ INIT-RUN's c2-invoke body catches a refusal and retires the pending frame.
TRUSTED: INIT-STOW ( R ^u8 n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U )
   HEAD-FRAME c2-init-stow
   TASK:DEFER-LEAVE ;

TRUSTED: RECORDS-STOW ( R ^u8 n n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U )
   HEAD-FRAME c2-records-stow
   TASK:DEFER-LEAVE ;

: INIT-CLOSE ( -- )
   HEAD-FRAME ORIGINAL-BOUND-CELL @ INIT-BOUND-CELL !
   CLOSE ;

: INIT-RESTORE ( R ^u8 n -- R ^u8 n )
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

TRUSTED: INIT-RUN ( R ^u8 n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- S ^u8 n | U )
   INIT-OPEN
   [: INIT-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

TRUSTED: RECORDS-RUN ( R ^u8 n n n [ R ^u8 n -- S ^u8 n | U -- U ] stow-layout | U -- S ^u8 n | U )
   INIT-OPEN
   [: RECORDS-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

TRUSTED: INVOKE ( R [ R -- S | U -- U ] | U -- S | U )
   [: RUN-BODY ;] [: CLOSE ;] [: FINISH ;] c2-invoke ;

: RUN ( R [ R -- S | U -- U ] | U -- S | U )
   OPEN
   INVOKE ;

CAST: ALLOC-LEN>N ( NUM:alloc-byte-len -- n )

: ALLOC-RUN ( R NUM:alloc-byte-len [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S | U )
   {: body :}
   [: MEM:RELEASE-BYTES ;] ACQUIRE-BYTES {: resource:^ size:NUM:alloc-byte-len :}
   resource size ALLOC-LEN>N body execute 2drop ;

\ A child callback's returned control cells carry no disposal authority.
: LOAN-BODY ( R ^u8 n [ R ^u8 n -- S ^u8 n ] -- S ^u8 n )
   execute ;

: LOAN-RUN ( R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S ^u8 n | U )
   LOAN-ENTER
   [: LOAN-BODY ;] [: LOAN-LEAVE ;] finally ;

: CHECK-INDEX ( n n -- ) {: idx:n bound:n :}
   idx 0 < idx bound >= or if E-SPAN-RANGE throw then ;

: CHECK-SLICE ( n n n -- ) {: off:n count:n bound:n :}
   \ Subtract only after the offset is inside the bound; off + count may wrap.
   off 0 < count 0 < or off bound > or if E-SPAN-RANGE throw then
   count bound off - > if E-SPAN-RANGE throw then ;

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

\ WITH-RECORD checks a zero-based index and lends `mut-view<p,j,a,init<i,T>>`
\ under `forall<j inside i,...>`; `j` cannot escape, and the element is
\ neither reconstructed nor cleared on return.
: WITH-RECORD ( R ^u8 n n [ R ^u8 n -- S ^u8 n | U -- U ] n n n | U -- S ^u8 n | U )
   {: width:n stride:n align:n :}
   {: base:^ count:n index:n callback :}
   index count CHECK-INDEX
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
   off 0 < extent 0 <= or off bound > or if E-SPAN-RANGE throw then
   extent bound off - > if E-SPAN-RANGE throw then
   base off + extent callback LOAN-RUN 2drop
   base bound ;

: BYTE@ ( read-view<p,q,u8> n -- u8 read-view<p,q,u8> )
   {: idx:n :}
   dup READ-UNPACK {: base:^ bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ swap ;

: MUT-BYTE@ ( mut-view<p,q,a,u8> n -- u8 mut-view<p,q,a,u8> )
   swap MUT-UNPACK {: idx:n base:^ bound:n :}
   idx bound CHECK-INDEX
   base idx + c@ base bound MUT-PACK ;

: MUT-BYTE! ( mut-view<p,q,a,u8> n u8 -- mut-view<p,q,a,u8> )
   rot MUT-UNPACK {: idx:n value:u8 base:^ bound:n :}
   idx bound CHECK-INDEX
   value base idx + c!
   base bound MUT-PACK ;

: SLICE ( read-view<p,q,u8> n n -- read-view<p,q,u8> )
   {: off:n count:n :}
   READ-UNPACK {: base:^ bound:n :}
   off count bound CHECK-SLICE
   base off + count READ-PACK ;

: PREFIX ( mut-view<p,q,a,u8> n -- mut-view<p,q,a,u8> )
   {: count:n :}
   MUT-UNPACK {: base:^ bound:n :}
   0 count bound CHECK-SLICE
   base count MUT-PACK ;

: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   dup READ-UNPACK nip swap ;

: MUT-LENGTH ( mut-view<p,q,a,u8> -- n mut-view<p,q,a,u8> )
   MUT-UNPACK {: base:^ bound:n :}
   bound base bound MUT-PACK ;

STRUCTURE owner-state OPAQUE FIELD node ^n ;STRUCTURE
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
: SEED-OWNER ( -- owner-state ) NULL$ drop CELL-VIEW OWNER-STATE-MAKE ;
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
