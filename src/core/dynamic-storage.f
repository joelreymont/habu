\ dynamic-storage.f - checked transient allocation behind DYNAMIC-BUFFER and
\ ARENA. A buffer record is the head NAME#base (a PTR-VARIABLE holding the
\ mapping, `ptr ptr a`) and the control block NAME#ctl behind it (capacity,
\ registry slot + 1, width tag, bound for that width; `ptr n`). No word views a
\ pointer cell as bytes or cells. Release zeroes the record, so no process
\ address survives capture. It loads below the core-prefix mark: rewound
\ generated declarations call it.
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
\ so the pointer itself is the authority on whether the mapping is live.
\ Membership follows live allocation, a precompiled reserve after restore
\ included. Row i holds a record's head address at cell 2i and its control
\ block's address at cell 2i+1; REG-CAP counts rows.
PTR-U8-TABLE-VARIABLE REG
variable REG-CAP
variable REG-U
\ The newest live arena's first chunk, NULL-PTR when none is live (ARENA-NEW).
PTR-VARIABLE ARENAS
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

\ Growth can replace the mapping, so every access fetches the head after checking
\ capacity. The width guard on the exact division keeps native and Wasm
\ identical: a division by zero answers 0 on ARM64 and traps on Wasm. Since the
\ index is checked first, a width <= 0 with a negative index, and a width of 0 on
\ an empty record (tag 0), answer E-BOUNDS rather than E-SIZE; generated
\ accessors always pass a positive width. The tag and bound are read inline: two
\ calls per access cost more than the division they replace.
: ADDRESS ( n ptr ptr a ptr n n -- ptr a ) {: idx:n cb:ptr ctl:ptr width:n :}
   idx 0 < if E-BOUNDS throw then
   ctl TAG-OFF + @ width = if
      idx ctl BOUND-OFF + @ >= if E-BOUNDS throw then
   else
      width 0 <= if E-SIZE throw then
      idx ctl @ width / >= if E-BOUNDS throw then
   then
   cb @ idx width * + ;

\ The capacity, its width tag and the bound (capacity / tag) change together:
\ PUBLISH and RELEASE are their only writers, each under LOCK. ADDRESS uses the
\ bound only when the tag equals its width; any other record (grown with a
\ foreign width, empty, released, or a stripped image's zero mapping) answers by
\ exact division.
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

\ ---- arenas ------------------------------------------------------------------
\ An ARENA declaration (src/core/arena.f) is a record whose one field is an
\ arena-head (src/core/type-family.f REGISTER-ARENA-HEAD), one cell holding the
\ newest chunk's base, NULL-PTR while the arena is empty. The checker refuses
\ to fetch or store a head, or any layout holding one, whole (checker.f
\ IN-PLACE?), and refuses any cast that reaches one outside this module
\ (checker.f CAST-REACHES-HEAD?), so no typed access and no other cast copies
\ or forges a head; address arithmetic still reaches one (docs/forth.md "Raw
\ storage never holds an address"). A chunk is ARENA-HDR bytes of header cells, then its records; the
\ first holds ARENA-FIRST records, each next one twice its predecessor's, and
\ records never move. The first chunk carries the declaration's name and links
\ the arena into ARENAS, which capture refuses while it is not empty
\ (ARENA-GUARD). NEW and RELEASE take no lock on the owner: tasks sharing one
\ arena serialise NAME-NEW and NAME-RELEASE themselves.
\ ARENA-NEW and ARENA-RELEASE are INTRINSIC-tagged: checked code may call or
\ tick them only inside the generated NAME-NEW and NAME-RELEASE, the definitions
\ LBUF-EVAL arms (checker.f ARENA-CALL-REFUSE?, E-ARENA-CALL). Those reach the
\ head through the engine's own `field-project`, a name no sealed-engine
\ definition takes (STRUCTURE-MAKE:PROJECT-NAME$), so each head serves one
\ declared type and a record's size always comes from it.

7211 constant E-ARENA-LIVE

private

64 constant ARENA-HDR
32 constant ARENA-FIRST

CAST: >N-CELL ( ptr u8 -- ptr n )
CAST: >P-CELL ( ptr u8 -- ptr ptr u8 )
CAST: >HEAD-CELL ( ptr arena-head -- ptr ptr u8 )

\ The header cells, each at its offset from the chunk base.
: CH-PREV ( ptr u8 -- ptr ptr u8 ) >P-CELL ;
: CH-USED ( ptr u8 -- ptr n ) CELL + >N-CELL ;
: CH-CAP ( ptr u8 -- ptr n ) 2 cells + >N-CELL ;
: CH-BYTES ( ptr u8 -- ptr n ) 3 cells + >N-CELL ;
: CH-LINK-PREV ( ptr u8 -- ptr ptr u8 ) 4 cells + >P-CELL ;
: CH-LINK-NEXT ( ptr u8 -- ptr ptr u8 ) 5 cells + >P-CELL ;
: CH-NAME-A ( ptr u8 -- ptr ptr u8 ) 6 cells + >P-CELL ;
: CH-NAME-U ( ptr u8 -- ptr n ) 7 cells + >N-CELL ;


: ARENA-EXTENT ( n n -- n )
   {: count:n width:n :}
   count 0 < width 0 <= or if E-SIZE throw then
   count MAX-BYTES ARENA-HDR - width / > if E-SIZE throw then
   count width * ARENA-HDR + ;


: ARENA-LINK ( ptr u8 -- )
   {: first:ptr :}
   ARENAS @ {: top:ptr :}
   top first CH-LINK-NEXT !
   top NULL-PTR <> if first top CH-LINK-PREV ! then
   first ARENAS ! ;


: ARENA-UNLINK ( ptr u8 -- )
   {: first:ptr :}
   first CH-LINK-PREV @ {: before:ptr :}
   first CH-LINK-NEXT @ {: after:ptr :}
   before NULL-PTR = if after ARENAS ! else after before CH-LINK-NEXT ! then
   after NULL-PTR <> if before after CH-LINK-PREV ! then ;


: ARENA-OLDEST ( ptr u8 -- ptr u8 )
   begin dup CH-PREV @ NULL-PTR <> while CH-PREV @ repeat ;

public


\ The next zeroed record of `width` bytes in the given owner's arena. A full or
\ empty arena maps its next chunk first: a refused size or mapping throws with
\ nothing written, and the head is published last. The record's pointee is
\ free in this row; the call gate above admits only the generated NAME-NEW,
\ which passes its type's width and binds the pointee from its own signature.
: ARENA-NEW ( ptr arena-head n ptr u8 n -- ptr a )
   {: owner:ptr width:n name:ptr nameu:n :}
   owner >HEAD-CELL {: cell:ptr :}
   cell @ {: head:ptr :}
   head NULL-PTR <> if
      head CH-USED @ {: at:n :}
      at head CH-CAP @ < if
         at 1+ head CH-USED !
         cell 0 ptr-field @ ARENA-HDR + at width * + exit
      then
   then
   head NULL-PTR = if ARENA-FIRST else head CH-CAP @ 2 * then {: cap:n :}
   cap width ARENA-EXTENT {: bytes:n :}
   bytes map-anon 0< if drop E-MAP throw then byte-view {: fresh:ptr :}
   cap fresh CH-CAP !
   bytes fresh CH-BYTES !
   1 fresh CH-USED !
   head NULL-PTR = if
      name fresh CH-NAME-A !
      nameu fresh CH-NAME-U !
      LOCK fresh ARENA-LINK fresh cell ! UNLOCK
   else
      head fresh CH-PREV !
      fresh cell !
   then
   cell 0 ptr-field @ ARENA-HDR + ;  INTRINSIC-ARENA-NEW INTRINSIC


\ Empties the given owner's arena: its head is cleared and it leaves ARENAS
\ before any chunk is unmapped, so a refused unmap (E-UNMAP, after every chunk
\ was tried) leaves no live arena naming the chunk.
: ARENA-RELEASE ( ptr arena-head -- )
   {: owner:ptr :}
   owner >HEAD-CELL {: cell:ptr :}
   cell @ {: head:ptr :}
   head NULL-PTR = if exit then
   LOCK head ARENA-OLDEST ARENA-UNLINK NULL-PTR cell ! UNLOCK
   0 head
   begin dup NULL-PTR <> while
      dup CH-PREV @ swap dup CH-BYTES @ munmap 0< if swap 1+ swap then
   repeat drop
   0 <> if E-UNMAP throw then ;  INTRINSIC-ARENA-RELEASE INTRINSIC


\ Capture refuses a live arena: its records would be lost with the mapping.
\ IMAGE-LIFECYCLE:PREPARE calls this last, after every hook has run, so a hook
\ can release its arena.
: ARENA-GUARD ( -- )
   ARENAS @ {: top:ptr :}
   top NULL-PTR = if exit then
   2 s" hb: capture: live arena " write drop
   2 top CH-NAME-A @ top CH-NAME-U @ write drop
   2 S\" \n" write drop
   E-ARENA-LIVE throw ;


\ Runs a fork with the registry held; PROC-FORK:RAW is its caller. Growth,
\ release and membership changes each take several stores under this lock, and a
\ child forked mid-change could neither wait for that task nor carry on where it
\ stopped. Held, nobody is inside when the process is copied, and parent and
\ child each free their own copy.
: ACROSS-FORK ( [ -- n ] -- n )
   LOCK [: UNLOCK ;] finally ;

: REGISTERED-N ( -- n ) REG-U @ ;
: DIRTY-N ( -- n ) REG-U @ ;

\ The five cells these registries are, handed one at a time to the caller's
\ claim. A fresh instance's zero (no mapping, no rows, no members, no live arena,
\ open lock) is correct, so a stripped AOT image owns these cells outright
\ (src/habu/aot-owned-cells.f). The naming happens here because the cells are
\ private here: this hands out the five and nothing else, no entry and no
\ address that outlives the call.
: OWNED-CELLS ( [ ptr u8 -- ] -- ) {: claim :}
   REG CELL-BYTES claim execute
   REG-CAP BYTE-VIEW claim execute
   REG-U BYTE-VIEW claim execute
   ARENAS byte-view claim execute
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
