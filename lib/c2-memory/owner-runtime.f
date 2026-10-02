\ Private task-local cleanup for the checked owner surface under construction.
require lib/errors.f
require lib/task.f
require lib/memory.f
require lib/image-lifecycle.f

package C2-MEM
private

\ The source loaded below owns this contiguous private runtime band. Its
\ executable records are hidden only after c2-memory.f and c2-owner.f have
\ compiled their legitimate direct calls.
ndict@ constant RUNTIME-FIRST

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

\ +USER resolves this row through the calling task's data-base. Main has the
\ same row in its own data region, although TASK:SELF names no TCB there.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and REGION-BYTES TASK:+USER OWNER-REGION drop

: HEAD-CELL ( -- ptr n ) OWNER-REGION ;
: DEPTH-CELL ( -- ptr n ) OWNER-REGION 8 + ;
: HOOK-CELL ( -- ptr n ) OWNER-REGION 16 + ;
: LOAN-DEPTH-CELL ( -- ptr n ) OWNER-REGION 24 + ;
: INIT-BOUND-CELL ( -- ptr n ) OWNER-REGION 32 + ;

: FRAME-AT ( n -- ptr n )
   FRAME-BYTES * OWNER-REGION META-BYTES + + ;

: STATE-CELL ( ptr n -- ptr n ) ;
: PREV-CELL ( ptr n -- ptr n ) 8 + ;

\ These three slots have stable types although +USER publishes a raw byte row.
\ Only this package may refine the addresses; no capability or address leaves it.
TRUSTED: RESOURCE-CELL ( ptr n -- ptr ptr u8 ) 16 + ;
TRUSTED: LENGTH-CELL ( ptr n -- ptr NUM:alloc-byte-len ) 24 + ;
TRUSTED: DISPOSER-CELL ( ptr n -- ptr [ ptr u8 NUM:alloc-byte-len -- ] ) 32 + ;
TRUSTED: ORIGINAL-BOUND-CELL ( ptr n -- ptr n ) 40 + ;
TRUSTED: KIND-CELL ( ptr n -- ptr n ) 48 + ;
TRUSTED: APPEND-HEAD-CELL ( ptr n -- ptr ptr u8 ) 56 + ;
TRUSTED: APPEND-PENDING-CELL ( ptr n -- ptr ptr u8 ) 64 + ;
TRUSTED: APPEND-CURSOR-CELL ( ptr n -- ptr ptr u8 ) 72 + ;
: APPEND-REMAIN-CELL ( ptr n -- ptr n ) 80 + ;

TRUSTED: APPEND-NEXT-CELL ( ptr u8 -- ptr ptr u8 ) ;
TRUSTED: APPEND-LENGTH-CELL ( ptr u8 -- ptr NUM:alloc-byte-len ) 8 + ;
TRUSTED: APPEND-DISPOSER-CELL ( ptr u8 -- ptr [ ptr u8 NUM:alloc-byte-len -- ] ) 16 + ;

: HEAD-FRAME ( -- ptr n )
   HEAD-CELL @ 1- FRAME-AT ;

: CAPTURE-GUARD ( -- )
   HEAD-CELL @ 0<> LOAN-DEPTH-CELL @ 0<> or if E-C2-CAPTURE throw then ;

: LOAN-ENTER ( -- ) 1 LOAN-DEPTH-CELL +! ;
: LOAN-LEAVE ( -- ) -1 LOAN-DEPTH-CELL +! ;

: REGISTER-CAPTURE-GUARD ( -- )
   [: CAPTURE-GUARD ;] IMAGE-LIFECYCLE:REGISTER-PERSISTENT ;

\ Called only with a live head. Keeping that head in CLOSING while the adapter
\ runs gives a halted task's exit path one authoritative cleanup record.
: DISPOSE-HEAD ( -- )
   HEAD-FRAME {: frame:ptr :}
   frame RESOURCE-CELL @
   frame LENGTH-CELL @
   frame DISPOSER-CELL @ execute ;

: DISPOSE-APPEND ( -- )
   HEAD-FRAME APPEND-PENDING-CELL @ {: header:ptr :}
   header APPEND-HEADER-BYTES +
   header APPEND-LENGTH-CELL @
   header APPEND-DISPOSER-CELL @ execute ;

: RELEASE-APPEND-CHUNK ( ptr u8 NUM:alloc-byte-len -- )
   swap APPEND-HEADER-BYTES - swap MEM:RELEASE-BYTES ;

: FIRST-ERROR ( n n -- n ) {: first:n rc:n :}
   first 0<> if first else rc then ;

: CLOSE-APPENDS ( -- n )
   0
   begin HEAD-FRAME APPEND-HEAD-CELL @ dup NULL$ drop <> while
      {: header:ptr :}
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
   HEAD-FRAME {: frame:ptr :}
   frame STATE-CELL @ {: state:n :}
   state PENDING <> state LIVE <> and if
      TASK:DEFER-LEAVE E-C2-STATE throw
   then
   CLOSING frame STATE-CELL !
   state LIVE = if
      frame KIND-CELL @ BYTES-FRAME = if CLOSE-BYTES
      else [: DISPOSE-HEAD ;] catch then
   else 0 then {: rc:n :}
   NULL$ drop frame RESOURCE-CELL !
   CLOSED frame STATE-CELL !
   frame PREV-CELL @ HEAD-CELL !
   -1 DEPTH-CELL +!
   TASK:DEFER-LEAVE
   state PENDING = if TASK:DEFER-LEAVE then
   rc 0<> if rc throw then ;

\ Every remaining frame is closed even when a disposer reports an error. TASK's
\ exit chain already keeps the body's first error, or this drain's first error.
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

\ Registration and capacity precede all acquisition. The pending row exists
\ before an allocation can return, and deferral covers its transfer to LIVE.
: OPEN ( -- )
   DEPTH-CELL @ FRAME-CAP >= if E-C2-CAPACITY throw then
   REGISTER-EXIT
   TASK:DEFER-ENTER
   DEPTH-CELL @ FRAME-AT {: frame:ptr :}
   HEAD-CELL @ frame PREV-CELL !
   BYTES-FRAME frame KIND-CELL !
   NULL$ drop frame APPEND-HEAD-CELL !
   NULL$ drop frame APPEND-PENDING-CELL !
   NULL$ drop frame APPEND-CURSOR-CELL !
   0 frame APPEND-REMAIN-CELL !
   PENDING frame STATE-CELL !
   DEPTH-CELL @ 1+ dup DEPTH-CELL ! HEAD-CELL ! ;

\ Straight-line, nonthrowing transfer immediately after MEM returns the pair.
: RECORD ( ptr u8 NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- )
   {: resource:ptr size dispose :}
   HEAD-FRAME {: frame:ptr :}
   resource frame RESOURCE-CELL !
   size frame LENGTH-CELL !
   dispose frame DISPOSER-CELL !
   LIVE frame STATE-CELL !
   TASK:DEFER-LEAVE ;

\ This disposer clears only the initialized extent. The enclosing allocation
\ frame still owns the mapping and its eventual release.
: INIT-CLEAR ( ptr u8 NUM:alloc-byte-len -- )
   {: resource:ptr extent:NUM:alloc-byte-len :}
   extent INIT-LEN>N 0 ?do 0 resource i + c! loop ;

: INIT-OPEN ( -- )
   OPEN
   INIT-FRAME HEAD-FRAME KIND-CELL !
   [: INIT-CLEAR ;] HEAD-FRAME DISPOSER-CELL ! ;

\ BIND discovers authority only in this task's live BYTES chain. The base is
\ supplied by WITH-INIT, which already holds the unique root byte view.
: ROOT-FROM ( ptr u8 n -- ptr n ) {: base:ptr id:n :}
   id 0= if E-C2-STATE throw then
   id 1- FRAME-AT {: frame:ptr :}
   frame STATE-CELL @ LIVE =
   frame KIND-CELL @ BYTES-FRAME = and
   frame RESOURCE-CELL @ base = and if frame exit then
   base frame PREV-CELL @ recurse ;

: ROOT-FRAME ( ptr u8 -- ptr n ) HEAD-CELL @ ROOT-FROM ;

: FRAME-IN? ( ptr n n -- bool ) {: target:ptr id:n :}
   id 0= if false exit then
   id 1- FRAME-AT {: frame:ptr :}
   frame target = if
      frame STATE-CELL @ LIVE =
      frame KIND-CELL @ BYTES-FRAME = and exit then
   target frame PREV-CELL @ recurse ;

: LIVE-BYTES-FRAME? ( ptr n -- bool ) HEAD-CELL @ FRAME-IN? ;

TRUSTED: BIND-STATE ( ptr u8 n -- ptr u8 n )
   {: base:ptr bound:n :}
   base ROOT-FRAME base !
   base bound ;

TRUSTED: OWNER-FRAME ( ptr u8 -- ptr n )
   @ dup LIVE-BYTES-FRAME? 0= if E-C2-STATE throw then ;

: APPEND-RECORD-BYTES ( n -- n )
   CELL 1- + CELL negate and APPEND-HEADER-BYTES + ;

\ A chunk begins with a release node in the same LIFO chain as its payloads.
\ The next chunk leaves any unused tail of the old one until owner close.
: APPEND-CHUNK ( ptr n n -- ) {: frame:ptr record-bytes:n :}
   record-bytes APPEND-HEADER-BYTES + MEM-64K max
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: chunk:ptr length:NUM:alloc-byte-len :}
   \ Register ownership before any further fallible operation or yield.
   frame APPEND-HEAD-CELL @ chunk APPEND-NEXT-CELL !
   length chunk APPEND-LENGTH-CELL !
   [: RELEASE-APPEND-CHUNK ;] chunk APPEND-DISPOSER-CELL !
   chunk frame APPEND-HEAD-CELL !
   chunk APPEND-HEADER-BYTES + frame APPEND-CURSOR-CELL !
   length INIT-LEN>N APPEND-HEADER-BYTES - frame APPEND-REMAIN-CELL ! ;

: APPEND-BODY ( ptr n NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 n )
   {: frame:ptr size:NUM:alloc-byte-len dispose :}
   size INIT-LEN>N {: bytes:n :}
   bytes MAX-APPEND > if E-MEM-SIZE throw then
   bytes APPEND-RECORD-BYTES {: record-bytes:n :}
   frame APPEND-REMAIN-CELL @ record-bytes < if
      frame record-bytes APPEND-CHUNK
   then
   frame APPEND-CURSOR-CELL @ {: header:ptr :}
   frame APPEND-HEAD-CELL @ header APPEND-NEXT-CELL !
   size header APPEND-LENGTH-CELL !
   dispose header APPEND-DISPOSER-CELL !
   header frame APPEND-HEAD-CELL !
   header record-bytes + frame APPEND-CURSOR-CELL !
   frame APPEND-REMAIN-CELL @ record-bytes - frame APPEND-REMAIN-CELL !
   header APPEND-HEADER-BYTES + bytes ;

: APPEND ( ptr n NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 n )
   TASK:DEFER-ENTER
   [: APPEND-BODY ;] [: TASK:DEFER-LEAVE ;] finally ;

\ c2-init-stow is the narrow machine transfer: it validates the byte view and
\ descriptors, arms the clear-only frame, then moves the fixed cell bundle.
\ INIT-RUN's c2-invoke body catches a refusal and retires the pending frame.
TRUSTED: INIT-STOW ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U )
   HEAD-FRAME c2-init-stow
   TASK:DEFER-LEAVE ;

TRUSTED: RECORDS-STOW ( R ptr u8 n n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U )
   HEAD-FRAME c2-records-stow
   TASK:DEFER-LEAVE ;

: INIT-CLOSE ( -- )
   HEAD-FRAME ORIGINAL-BOUND-CELL @ INIT-BOUND-CELL !
   CLOSE ;

TRUSTED: INIT-RESTORE ( R ptr u8 n -- R ptr u8 n )
   drop INIT-BOUND-CELL @ ;

: ACQUIRE-BYTES ( NUM:alloc-byte-len [ ptr u8 NUM:alloc-byte-len -- ] -- ptr u8 NUM:alloc-byte-len )
   {: size dispose :}
   size MEM:ALLOC-BYTES {: resource:ptr actual :}
   resource actual dispose RECORD
   resource actual ;

: RUN-BODY ( R [ R -- S ] -- S ) execute ;

: FINISH ( n -- ) {: rc:n :}
   HEAD-CELL @ 0= if
      TASK:HALTED? if rc TASK:EXIT-FAILURE TASK:PAUSE then
   then ;

TRUSTED: INIT-RUN ( R ptr u8 n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   INIT-OPEN
   [: INIT-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

TRUSTED: RECORDS-RUN ( R ptr u8 n n n [ R ptr u8 n -- S ptr u8 n | U -- U ] n n n | U -- S ptr u8 n | U )
   INIT-OPEN
   [: RECORDS-STOW RUN-BODY ;] [: INIT-CLOSE ;] [: FINISH ;] c2-invoke
   INIT-RESTORE ;

TRUSTED: INVOKE ( R [ R -- S | U -- U ] | U -- S | U )
   [: RUN-BODY ;] [: CLOSE ;] [: FINISH ;] c2-invoke ;

: RUN ( R [ R -- S | U -- U ] | U -- S | U )
   OPEN
   INVOKE ;

ndict@ constant RUNTIME-END

TRUSTED: HIDE-RUNTIME ( -- )
   RUNTIME-END RUNTIME-FIRST ?do
      i XREF-REC XREF-FLAGS DKIND:MASK and 0= if i int-mark then
   loop ;
ndict@ 1- constant HIDE-RUNTIME-ID

;package

\ Declaration-time guard stays armed after a refused capture and is carried
\ into emitted images. PREPARE calls it after one-shot task cleanup.
package C2-MEM
REGISTER-CAPTURE-GUARD
;package
