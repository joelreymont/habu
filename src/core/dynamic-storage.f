\ Checked transient allocation behind DYNAMIC-BUFFER declarations.
\ A control record is three cells: mapping, exact byte capacity and registry
\ slot + 1. The element width lives in the generated accessor, never in the
\ record.
\ Membership follows live allocation, including a precompiled reserve after
\ restore. Release zeroes the record; no process address survives capture.
\ THE RECORD HEAD IS A DECLARED POINTER CELL (dot habu-refuse-a-ptr-5ad2734e):
\ a raw storage cell never holds an address, so the mapping lives in the
\ PTR-VARIABLE the definer generates and the record arrives here as `ptr ptr a`,
\ a pointer to that cell. `a` stays a quantifier of these words, so a buffer
\ whose cells hold pointers moves them as pointers. The numeric cells behind
\ the head are reached through an explicit cell view of the head (CTL) rather
\ than through its pointer type.
\ This runtime belongs below the core-prefix mark: generated declarations in
\ a rewound build source already call it, before src/lib/errors.f is available.
package DYNAMIC-STORAGE
public

\ A refused map and a refused unmap, which src/arch/wasm/kernel-words.f's
\ provider throws too.
7138 constant E-MAP
7139 constant E-UNMAP

\ Every control record.s cells: src/core/layout-buffer.f allots DBUF-CTRL-BYTES.
DBUF-CTRL-BYTES CELL / constant CONTROL-CELLS

private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
E-LAYOUT-BUFFER constant E-SIZE    \ src/core/layout-buffer.f's size and bounds refusals
E-LAYOUT-BOUNDS constant E-BOUNDS

$40 constant REG-INIT
1 constant REG-HEAD

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

\ Keep the existing cell-rounded capacity contract: 100 byte elements reserve
\ 104 addressable bytes. The accessor admits only complete elements of its
\ width within the capacity; COPY below preserves every capacity byte.
: CELL-CEIL ( n -- n ) {: bytes:n :}
   bytes CELL mod {: part:n :}
   part 0= if bytes exit then
   CELL part - {: pad:n :}
   bytes MAX-BYTES pad - > if E-SIZE throw then
   bytes pad + ;

: CAPACITY ( n n -- n ) {: need:n old:n :}
   old MAX-BYTES 2 / > if need CELL-CEIL exit then
   need old 2 * 64 max max CELL-CEIL ;

\ The old and fresh mappings do not overlap. Preserve the entire exact byte
\ capacity, including a fractional element's tail.
: COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr bytes:n :}
   src byte-view dst byte-view bytes BYTE-COPY ;

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
\ A registry row is one cell: a declared control head.
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
   old MAX-BYTES CELL / REG-HEAD - > if E-SIZE throw then
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

: BOUNDS ( n n -- n ) {: idx:n count:n :}
   idx 0 < if E-BOUNDS throw then
   idx count >= if E-BOUNDS throw then
   idx ;

\ The checked offset and current mapping are one address operation. Growth can
\ replace the mapping, so every access fetches the head after checking capacity.
\ A negative index is refused before the record is read. The width guard keeps
\ native and Wasm identical: a division by zero answers 0 on ARM64 and traps
\ on Wasm.
: ADDRESS ( n ptr ptr a n -- ptr a ) {: idx:n cb:ptr width:n :}
   width 0 <= if E-SIZE throw then
   idx 0 < if E-BOUNDS throw then
   idx cb CAP @ width / >= if E-BOUNDS throw then
   cb @ idx width * + ;

: PUBLISH ( ptr a n ptr ptr a -- ) {: fresh:ptr cap:n cb:ptr :}
   fresh cb !
   cap cb CAP ! ;

\ Nothing in the control changes until every fallible operation has succeeded.
: GROW ( n ptr ptr a n -- ) {: need:n cb:ptr old:n :}
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
      fresh cap cb PUBLISH
      cb REGISTER
      UNLOCK
      exit
   then
   cb @ fresh old COPY
   LOCK
   cb @ old munmap 0< if
      UNLOCK fresh cap munmap drop E-UNMAP throw
   then
   fresh cap cb PUBLISH
   UNLOCK ;

: RESERVE ( n ptr ptr a n -- ) {: count:n cb:ptr width:n :}
   count width EXTENT {: need:n :}
   cb CAP @ {: old:n :}
   need old <= if exit then
   need cb old GROW ;

: RELEASE ( ptr ptr a -- ) {: cb:ptr :}
   cb CAP @ {: cap:n :}
   cap 0= if exit then
   LOCK
   cb @ cap munmap 0< if UNLOCK E-UNMAP throw then
   cb UNREGISTER
   NULL-PTR cb !
   0 cb CAP !
   UNLOCK ;

\ Runs a fork with the registry held; PROC-FORK:RAW is its caller. A buffer's
\ growth or release unmaps its old mapping before it publishes the new one or
\ clears the control, a registry growth does the same to the registry, and a
\ membership change is several stores. Each runs under this lock, since a child
\ forked while another task was inside could neither wait for that task nor
\ carry on where it stopped. Held, nobody is inside when the process is copied,
\ and parent and child each free their own copy.
: ACROSS-FORK ( [ -- n ] -- n )
   LOCK [: UNLOCK ;] finally ;

: REGISTERED-N ( -- n ) REG-U @ ;
: DIRTY-N ( -- n ) REG-U @ ;

\ THE THREE CELLS THIS REGISTRY IS, handed one at a time to a claim the caller
\ supplies. The registry is per runtime instance, so an instance that starts with
\ no mapping, no members and an open lock is correct - and that is the zero a
\ fresh anonymous mapping already holds. A stripped AOT image therefore owns
\ these three cells outright instead of being refused for naming them
\ (src/habu/aot-owned-cells.f, dot habu-give-a-stripped-c1a6664a).
\ THE NAMING HAPPENS HERE BECAUSE THE CELLS ARE DECLARED HERE. That list admits a
\ cell only by name, the three are private, and a private cell of this package
\ stays private: this hands out these three and nothing else - no capacity, no
\ entry, and no address that outlives the call.
: OWNED-CELLS ( [ ptr u8 -- ] -- ) {: claim :}
   REG BYTE-VIEW claim execute
   REG-U BYTE-VIEW claim execute
   MUTEX BYTE-VIEW claim execute ;

private

\ Refuse a cut through any live control record before releasing a member.
: RANGE-CHECK ( ptr u8 n -- ) {: start:ptr size:n :}
   size 0 < if E-SIZE throw then
   size 0= if exit then
   CONTROL-CELLS cells {: extent:n :}
   REG-U @ 0 ?do
      i REG-ENTRY byte-view start - {: off:n :}
      off 0 < if
         off extent + 0 > if E-SIZE throw then
      else
         off size < if
            extent size off - > if E-SIZE throw then
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
   begin REG-U @ 0 > while
      REG-U @ 1- REG-ENTRY RELEASE
   repeat
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
