\ Checked transient allocation behind DYNAMIC-BUFFER declarations.
\ A control record is a declared head and a numeric control block. The head,
\ NAME#base, is the PTR-VARIABLE the definer generates: it holds the mapping, so
\ it arrives here as `ptr ptr a`, a pointer to that cell, and `a` stays a
\ quantifier of these words, so a buffer whose cells hold pointers moves them as
\ pointers. The control block, NAME#ctl, is CONTROL-CELLS numbers right behind
\ the head: exact byte capacity, registry slot + 1, a width tag and the element
\ bound for that width; it arrives as `ptr n`. No word here views a pointer cell
\ as bytes or cells (dot habu-refuse-a-ptr-5ad2734e): the head is only fetched
\ and stored as a pointer, and the counts are only read as numbers.
\ The element width lives in the generated accessor; the block only caches a
\ bound for one.
\ THE BOUND IS WRITTEN ONLY WITH THE CAPACITY: PUBLISH stores the tag (the
\ reserving width) and the bound (capacity / tag) beside the capacity, and
\ RELEASE zeroes all three, each under LOCK; no other word writes the capacity,
\ and it never changes between those writes. ADDRESS uses the bound only when
\ the tag equals its own width, so the bound it reads is exactly its capacity /
\ width. Any other record - grown through RESERVE with a foreign width, empty,
\ released, or a stripped image's zero mapping - answers by the exact division.
\ Membership follows live allocation, including a precompiled reserve after
\ restore. Release zeroes the record; no process address survives capture.
\ This runtime belongs below the core-prefix mark: generated declarations in
\ a rewound build source already call it, before src/lib/errors.f is available.
package DYNAMIC-STORAGE
public

\ A refused map and a refused unmap, which src/arch/wasm/kernel-words.f's
\ provider throws too.
7138 constant E-MAP
7139 constant E-UNMAP

\ The cells of every control block: src/core/layout-buffer.f allots
\ DBUF-CTRL-BYTES for a record, the head cell and these behind it.
DBUF-CTRL-BYTES CELL / 1- constant CONTROL-CELLS

private

$7FFFFFFFFFFFFFFF constant MAX-BYTES
E-LAYOUT-BUFFER constant E-SIZE    \ src/core/layout-buffer.f's size and bounds refusals
E-LAYOUT-BOUNDS constant E-BOUNDS

$40 constant REG-INIT

: EXTENT ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES width / > if E-SIZE throw then
   count width * ;

\ Keep the existing cell-rounded capacity contract: 100 byte elements reserve
\ 104 addressable bytes. The accessor admits only complete elements of its
\ width within the capacity, whether it reads the cached bound or divides;
\ COPY below preserves every capacity byte.
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
\ so the pointer itself is the authority on whether the mapping is live. A row
\ is two cells: row i holds a record's head address at cell 2i and its control
\ block's address at cell 2i+1. Both are addresses, so the registry is a
\ declared table of pointer cells and its capacity, in rows, lives in REG-CAP.
PTR-U8-TABLE-VARIABLE REG
variable REG-CAP
variable REG-U
here data-base - negate 7 and allot
variable MUTEX

: LOCK ( -- ) begin 0 1 MUTEX atomic-cas 0= until ;
: UNLOCK ( -- ) 0 MUTEX atomic! ;

: REG-BYTES ( n -- n ) 2 cells EXTENT ;
: ROW-HEAD ( n -- ptr ptr ptr a ) 2 * REG @ swap ptr-field ;
: ROW-CTL ( n -- ptr ptr n ) 2 * 1+ REG @ swap ptr-field ;
: REG-ENTRY ( n -- ptr ptr a ) ROW-HEAD @ ;
: REG-CTL ( n -- ptr n ) ROW-CTL @ ;

\ The control block's cells after the capacity, which is its first cell.
: SLOT ( ptr n -- ptr n ) CELL + ;
2 cells constant TAG-OFF
3 cells constant BOUND-OFF
: TAG ( ptr n -- ptr n ) TAG-OFF + ;
: BOUND ( ptr n -- ptr n ) BOUND-OFF + ;

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

\ The registry's own cell, for OWNED-CELLS below: the claim takes a cell's bytes.
CAST: CELL-BYTES ( ptr ptr ptr u8 -- ptr u8 )

public

: BOUNDS ( n n -- n ) {: idx:n count:n :}
   idx 0 < if E-BOUNDS throw then
   idx count >= if E-BOUNDS throw then
   idx ;

\ The checked offset and current mapping are one address operation. Growth can
\ replace the mapping, so every access fetches the head after checking capacity.
\ A negative index is refused before the record is read. A record tagged with
\ this width bounds the index by its cached bound; any other is bounded by the
\ exact division. The width guard sits on that division and keeps native and
\ Wasm identical: a division by zero answers 0 on ARM64 and traps on Wasm.
\ Since the index is checked first, a width <= 0 with a negative index, and a
\ width of 0 on an empty record (tag 0), answer E-BOUNDS rather than E-SIZE;
\ generated accessors always pass a positive width. The tag and bound are
\ read inline at their offsets: TAG and BOUND are calls, and two of them per
\ access cost more than the division they replace.
: ADDRESS ( n ptr ptr a ptr n n -- ptr a ) {: idx:n cb:ptr ctl:ptr width:n :}
   idx 0 < if E-BOUNDS throw then
   ctl TAG-OFF + @ width = if
      idx ctl BOUND-OFF + @ >= if E-BOUNDS throw then
   else
      width 0 <= if E-SIZE throw then
      idx ctl @ width / >= if E-BOUNDS throw then
   then
   cb @ idx width * + ;

\ The capacity, its width tag and the bound for that width change together.
: PUBLISH ( ptr a n ptr ptr a ptr n n -- )
   {: fresh:ptr cap:n cb:ptr ctl:ptr width:n :}
   fresh cb !
   cap ctl !
   width ctl TAG !
   cap width / ctl BOUND ! ;

\ Nothing in the record changes until every fallible operation has succeeded.
: GROW ( n ptr ptr a ptr n n n -- )
   {: need:n cb:ptr ctl:ptr old:n width:n :}
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
      fresh cap cb ctl width PUBLISH
      cb ctl REGISTER
      UNLOCK
      exit
   then
   cb @ fresh old COPY
   LOCK
   cb @ old munmap 0< if
      UNLOCK fresh cap munmap drop E-UNMAP throw
   then
   fresh cap cb ctl width PUBLISH
   UNLOCK ;

: RESERVE ( n ptr ptr a ptr n n -- ) {: count:n cb:ptr ctl:ptr width:n :}
   count width EXTENT {: need:n :}
   ctl @ {: old:n :}
   need old <= if exit then
   need cb ctl old width GROW ;

: RELEASE ( ptr ptr a ptr n -- ) {: cb:ptr ctl:ptr :}
   ctl @ {: cap:n :}
   cap 0= if exit then
   LOCK
   cb @ cap munmap 0< if UNLOCK E-UNMAP throw then
   ctl UNREGISTER
   NULL-PTR cb !
   0 ctl !
   0 ctl TAG !
   0 ctl BOUND !
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

\ THE FOUR CELLS THIS REGISTRY IS, handed one at a time to a claim the caller
\ supplies. The registry is per runtime instance, so an instance that starts with
\ no mapping, no row capacity, no members and an open lock is correct - and that
\ is the zero a fresh anonymous mapping already holds: REG-CAP's zero is the
\ capacity of the absent mapping, and REG-ROOM maps afresh on the first reserve.
\ A stripped AOT image therefore owns these four cells outright instead of being
\ refused for naming them (src/habu/aot-owned-cells.f, dot
\ habu-give-a-stripped-c1a6664a).
\ THE NAMING HAPPENS HERE BECAUSE THE CELLS ARE DECLARED HERE. That list admits a
\ cell only by name, the four are private, and a private cell of this package
\ stays private: this hands out these four and nothing else - no entry, and no
\ address that outlives the call.
: OWNED-CELLS ( [ ptr u8 -- ] -- ) {: claim :}
   REG CELL-BYTES claim execute
   REG-CAP BYTE-VIEW claim execute
   REG-U BYTE-VIEW claim execute
   MUTEX BYTE-VIEW claim execute ;

private

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
\ entry. A refused unmap preserves that entry for the next preparation attempt.
\ Capture runs after tasks stop, as IMAGE-LIFECYCLE:PREPARE requires, so registry
\ walks need no second lock. Concurrent mutation of one buffer remains its
\ caller's responsibility; the lock protects membership of distinct controls.
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

;package
