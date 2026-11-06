\ intern.f - the permanent address of a string literal's bytes; equal bodies
\ within a segment share one. Bodies live in DATA, since a DATA address means the
\ same after a snapshot restore and an mmap one does not. Tier 1 interns through
\ src/compiler/native/elaborate.f EMIT-STRING, tier 0 through
\ NSTR-ABI:INTERN-CELL (set when this file loads); fetch descriptors
\ (src/compiler/native/fetch.f KEEP) are bodies under the same rule. A build's
\ data space keeps its strings at `here` ("a build's data space" below).

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/image-lifecycle.f
require src/habu/layout.f
require src/habu/boot-cells.f

package INTERN
private

\ ---- the store ---------------------------------------------------------------
\ A pool is a chain of segments, each an owner record, its row tables and an
\ arena. A capture or a retained compiler opens a pool, and the store opens the
\ next segment at `here` when the last cannot take a body. A body larger than an
\ arena is refused by name, and so is one a closed window's pool has no room for
\ (WINDOW-CLOSE). A body interned again after its segment filled is copied into
\ the next one.
$80000 constant ARENA-CAP            \ 512 KB of bodies per segment
8192 constant ROWS-MAX               \ distinct bodies per segment
ROWS-MAX 2 * constant SLOTS          \ a power of two, twice ROWS-MAX

\ An owner is a pool's first segment, which selects the pool, a later segment of
\ one, or the compact, already-full rows a native build imports. A first segment
\ is closed once its window is latched: the pool opens no segment after that.
0 constant ROLE-IMPORTED
1 constant ROLE-HEAD
2 constant ROLE-MORE
3 constant ROLE-CLOSED

$38 constant OWNER-BYTES

\ The elaborator stages the arena address as an ordinary integer literal: its
\ distance from the null pointer.
: PTR>N ( ptr a -- n ) NULL-PTR - ;

\ Each owner owns its metadata as well as its bytes. Forward links leave a new
\ capture free of references into the older owners excluded from that capture.
\ The two complete row tables reserve 128 KiB per segment for that lasting
\ authority. A link holds its target's distance from the link's own cell, 0 for
\ none: a native build moves the captured window as one block and relocates no
\ raw cell, so the distance holds wherever the block lands, and a rewind of DATA
\ or a capture of the window has no pointer mark to drop.
: NEXT-FIELD ( ptr u8 -- ptr n ) CELL-VIEW ;            \ the next owner of any pool
: ARENA-FIELD ( ptr u8 -- ptr n ) 1 cells + CELL-VIEW ;
: CAP-FIELD ( ptr u8 -- ptr n ) 2 cells + CELL-VIEW ;
: ROWS-FIELD ( ptr u8 -- ptr n ) 3 cells + CELL-VIEW ;
: USED-FIELD ( ptr u8 -- ptr n ) 4 cells + CELL-VIEW ;
: MORE-FIELD ( ptr u8 -- ptr n ) 5 cells + CELL-VIEW ;  \ this pool's next segment
: ROLE-FIELD ( ptr u8 -- ptr n ) 6 cells + CELL-VIEW ;
: OFFSETS ( ptr u8 -- ptr n ) OWNER-BYTES + CELL-VIEW ;

: LENGTHS ( ptr u8 -- ptr n ) {: owner:ptr :}
   owner OFFSETS owner CAP-FIELD @ cells + ;

: LINK@ ( ptr n -- ptr u8 ) {: cell:ptr :}
   cell @ {: d:n :}
   d 0= if NULL-PTR exit then
   cell BYTE-VIEW d + ;

: LINK! ( ptr u8 ptr n -- ) {: target:ptr cell:ptr :}
   target PTR>N cell PTR>N - cell ! ;

: NEXT@ ( ptr u8 -- ptr u8 ) NEXT-FIELD LINK@ ;
: ARENA@ ( ptr u8 -- ptr u8 ) ARENA-FIELD LINK@ ;
: MORE@ ( ptr u8 -- ptr u8 ) MORE-FIELD LINK@ ;

: OWNER-SIZE ( n -- n ) cells 2 * OWNER-BYTES + ;

: CLEAR-CELLS ( ptr u8 n -- )
   {: buf:ptr bytes:n :}
   bytes CELL / 0 ?do 0 buf i cells + CELL-VIEW ! loop ;

: OWNER-INIT ( ptr u8 ptr u8 n n -- ) {: owner:ptr arena:ptr cap:n role:n :}
   \ Allot leaves old bytes intact. Unused rows and arena capacity belong to
   \ this new owner too, and must not retain the build host's scratch values.
   owner cap OWNER-SIZE CLEAR-CELLS
   role ROLE-IMPORTED <> if arena ARENA-CAP CELL + CLEAR-CELLS then
   0 owner NEXT-FIELD !
   arena owner ARENA-FIELD LINK!
   cap owner CAP-FIELD !
   0 owner ROWS-FIELD !  0 owner USED-FIELD !
   0 owner MORE-FIELD !
   role owner ROLE-FIELD ! ;

create BOOT-OWNER ROWS-MAX OWNER-SIZE allot
\ The separate empty-row byte gives it an address no nonempty row can own,
\ without charging a byte to BYTES or reducing the arena's body capacity.
create BOOT-ARENA ARENA-CAP CELL + allot
BOOT-OWNER BOOT-ARENA ROWS-MAX ROLE-HEAD OWNER-INIT

PERSISTED-PTR-VARIABLE FIRST-P       \ every owner, oldest first, through NEXT
PERSISTED-PTR-VARIABLE ACTIVE-P      \ the active pool's first segment
PERSISTED-PTR-VARIABLE WRITE-P       \ and its last, which takes new bodies
BOOT-OWNER FIRST-P !  BOOT-OWNER ACTIVE-P !  BOOT-OWNER WRITE-P !

: STRING-ARENA ( -- ptr u8 ) WRITE-P @ ARENA@ ;
: R-OFF ( -- ptr n ) WRITE-P @ OFFSETS ;
: R-LEN ( -- ptr n ) WRITE-P @ LENGTHS ;
: USED ( -- ptr n ) WRITE-P @ USED-FIELD ;
: ROWS ( -- ptr n ) WRITE-P @ ROWS-FIELD ;

create SLOT SLOTS cells allot       \ the last segment's hash slots: row index plus one
variable PROBE
variable HV

\ Definition capture records each successful call, including pool hits and
\ results the emitter later discards. The pool owns the immutable body bytes.
DYNAMIC-BUFFER SOURCE-ADDR n
DYNAMIC-BUFFER SOURCE-LEN n
variable SOURCE-N
variable SOURCE-ON

: BASE ( -- n )
   STRING-ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   STRING-ARENA k ROW-OFF +  k ROW-LEN ;

: OWNER-ROW$ ( ptr u8 n -- ptr u8 n ) {: owner:ptr row:n :}
   owner ARENA@ owner OFFSETS row cells + @ +
   owner LENGTHS row cells + @ ;

: LAST-OWNER ( -- ptr u8 )
   FIRST-P @
   begin dup NEXT@ 0= 0= while NEXT@ repeat ;

\ `here` is past the owner just laid out, its rows and its arena. A segment can
\ open inside work later undone (a failed evaluation or REPL line, a rolled-back
\ declaration) while its addresses are already compiled into published routines,
\ so every published owner raises the DATA floor past itself and no rewind of
\ DATA frees it.
: FLOOR-RAISE ( -- )
   here PTR>N data-base PTR>N -  data-base DATA-FLOOR-CELL + ! ;

: PUBLISH ( ptr u8 -- ) {: owner:ptr :}
   owner LAST-OWNER NEXT-FIELD LINK!
   FLOOR-RAISE ;

: NEW-SEGMENT ( n -- ptr u8 ) {: role:n :}
   align here {: owner:ptr :}
   ROWS-MAX OWNER-SIZE allot
   here {: arena:ptr :}
   ARENA-CAP CELL + allot
   owner arena ROWS-MAX role OWNER-INIT
   owner ;

: ROW-CONTAINS? ( n ptr u8 n -- bool ) {: address:n body:ptr size:n :}
   body PTR>N {: start:n :}
   address start < if false exit then
   size 0= if address start = exit then
   address start - size < ;

\ Reverse order also handles the older seed's shared empty/next-row address:
\ the later nonempty row owns its bytes, while a terminal empty row still fits.
: FIND-OWNER-ROW ( n ptr u8 -- ptr u8 n bool ) {: address:n owner:ptr :}
   owner ROWS-FIELD @ {: rows:n :}
   rows 0 ?do
      owner rows 1- i - OWNER-ROW$ {: body:ptr size:n :}
      address body size ROW-CONTAINS? if body size true unloop exit then
   loop
   NULL-PTR 0 false ;

: IMPORT-CHECK ( n ptr n ptr n -- ) {: rows:n offsets:ptr lengths:ptr :}
   rows 0 < rows ROWS-MAX > or if E-NSTR-BODY throw then
   rows 0 ?do
      offsets i cells + @ {: off:n :}
      lengths i cells + @ {: size:n :}
      off 0 < off ARENA-CAP > or if E-NSTR-BODY throw then
      size 0 < size ARENA-CAP off - > or if E-NSTR-BODY throw then
   loop ;

\ ---- finding a body that is already here --------------------------------------
\ FNV-1a; the top bit can run into the sign.
: FNV ( ptr u8 n -- n )
   {: a:ptr u:n :}
   $811C9DC5 HV !
   u 0 ?do
      a i + c@ HV @ xor  $1000193 *  HV !
   loop
   HV @ ;

\ Masked by an `and` against a positive constant so a hash whose top bit ran
\ into the sign still lands inside the table.
: HASH ( ptr u8 n -- n ) FNV SLOTS 1- and ;

: PROBE-NEXT ( -- )
   PROBE @ 1+ SLOTS 1- and PROBE ! ;

: SLOT@ ( -- n )
   PROBE @ cells SLOT + @ ;

\ Ends at the body's row or at an empty slot, which always exists (FREE-SLOT
\ says why).
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   begin SLOT@ 0<> while
      SLOT@ 1- ROW$ a u STR= if SLOT@ 1- exit then
      PROBE-NEXT
   repeat
   -1 ;

\ Ends at an empty slot, which always exists: the segment written holds fewer
\ than ROWS-MAX rows when a body is placed (ADD grows a full one first, SWITCH
\ places at most ROWS-MAX), and SLOTS is twice ROWS-MAX.
: FREE-SLOT ( ptr u8 n -- n )
   HASH PROBE !
   begin SLOT@ 0= 0= while PROBE-NEXT repeat
   PROBE @ ;

: SLOT-CLEAR ( -- )
   SLOTS 0 ?do 0 i cells SLOT + ! loop ;

\ ---- a build's data space ------------------------------------------------------
\ Open while BOOT:IMAGE-BASE-CELL is nonzero (boot-cells.f says when it is 0); a
\ task's region starts at 0. The table grows, so it is mapped outside DATA; the
\ space's close frees it (CLEANUP).
variable SPACE-IMAGE     \ the image base the table serves, 0 for no table
variable SPACE-TABLE     \ the slots' address, as its distance from NULL-PTR
variable SPACE-SLOTS     \ a power of two
variable SPACE-FILLED
SPACE-IMAGE IMAGE-LIFECYCLE:PROCESS-CELL
SPACE-TABLE IMAGE-LIFECYCLE:PROCESS-CELL
1024 constant FIRST-SLOTS

: IMAGE-BASE ( -- n ) data-base BOOT:IMAGE-BASE-CELL + @ ;
: SPACE? ( -- bool ) IMAGE-BASE 0<> ;

: CELL-AT ( n n -- ptr n )
   {: base:n k:n :}
   NULL-PTR BYTE-VIEW base + k cells + CELL-VIEW ;

: SLOT-CELL ( n -- ptr n ) SPACE-TABLE @ swap CELL-AT ;

\ A slot holds a body's address, 0 when empty; the length is the cell before it.
: BODY$ ( n -- ptr u8 n )
   {: body:n :}
   NULL-PTR BYTE-VIEW body +
   body CELL - 0 CELL-AT @ ;

: SLOT-ENDS? ( ptr u8 n n -- bool )
   {: a:ptr u:n k:n :}
   k SLOT-CELL @ {: body:n :}
   body 0= if true exit then
   body BODY$ a u STR= ;

\ The slot holding the body equal to a u, else the empty slot it goes in: one
\ exists, since the table is never more than half full (SPACE-ADD).
: SPACE-SLOT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u FNV SPACE-SLOTS @ 1- and
   begin dup a u rot SLOT-ENDS? 0= while 1+ SPACE-SLOTS @ 1- and repeat ;

: SLOT-PUT ( n -- )
   {: body:n :}
   body  body BODY$ SPACE-SLOT SLOT-CELL ! ;

: TABLE-MAP ( n -- n ) MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS NULL-PTR - ;

: TABLE-RELEASE ( n n -- )
   {: base:n slots:n :}
   base 0 CELL-AT slots MEM:CELLS-ALLOC-COUNT MEM:RELEASE-CELLS ;

public

\ Frees the table of the build whose space is closing (LINKER:WITH's close).
: CLEANUP ( -- )
   SPACE-IMAGE @ 0= if exit then
   SPACE-TABLE @ SPACE-SLOTS @ TABLE-RELEASE
   0 SPACE-TABLE !  0 SPACE-IMAGE ! ;

private

: SPACE-OPEN ( -- )
   CLEANUP
   FIRST-SLOTS TABLE-MAP SPACE-TABLE !
   FIRST-SLOTS SPACE-SLOTS !  0 SPACE-FILLED !
   IMAGE-BASE SPACE-IMAGE ! ;

: SPACE-GROW ( -- )
   SPACE-TABLE @ {: old:n :}
   SPACE-SLOTS @ {: slots:n :}
   slots 2 * TABLE-MAP SPACE-TABLE !
   slots 2 * SPACE-SLOTS !
   slots 0 ?do
      old i CELL-AT @ dup 0<> if SLOT-PUT else drop then
   loop
   old slots TABLE-RELEASE ;

\ One `allot` checks the bounds and zeroes before the copy; no `,`, so the
\ length cell never joins a created body's cell grid (BCOMMA).
: SPACE-ADD ( ptr u8 n -- n )
   {: a:ptr u:n :}
   SPACE-FILLED @ 1+ 2 * SPACE-SLOTS @ > if SPACE-GROW then
   align here NULL-PTR - {: head:n :}
   CELL u + allot
   u head 0 CELL-AT !
   head CELL + {: address:n :}
   a  NULL-PTR BYTE-VIEW address +  u BYTE-COPY
   address SLOT-PUT
   1 SPACE-FILLED +!
   address ;

\ The refusal comes before any lookup: a found address may sit above a lowered
\ DP.
: SPACE-STRING ( ptr u8 n -- n )
   {: a:ptr u:n :}
   data-base DPHW-CELL + @ 0<> if E-INTERN-REWIND throw then
   SPACE-IMAGE @ IMAGE-BASE <> if SPACE-OPEN then
   a u SPACE-SLOT SLOT-CELL @ {: body:n :}
   body 0<> if body exit then
   a u SPACE-ADD ;

\ ---- putting one in ------------------------------------------------------------
\ The last segment cannot take the body: open the pool's next one behind it. A
\ closed pool's would land past its window's end, outside the image.
: GROW ( -- )
   ACTIVE-P @ ROLE-FIELD @ ROLE-CLOSED = if E-NSTR-CAP throw then
   ROLE-MORE NEW-SEGMENT {: seg:ptr :}
   seg PUBLISH
   seg WRITE-P @ MORE-FIELD LINK!
   seg WRITE-P !
   SLOT-CLEAR ;

: ROOM? ( n -- bool ) {: u:n :}
   ROWS @ ROWS-MAX <  u ARENA-CAP USED @ - <=  and ;

\ A body no segment can hold is refused before anything moves, so the refusal
\ leaves no trace.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   u ARENA-CAP > if E-NSTR-CAP throw then
   u ROOM? 0= if GROW then
   a u FREE-SLOT {: s:n :}
   USED @ {: off:n :}
   u 0= if ARENA-CAP else off then {: stored:n :}
   a  STRING-ARENA stored +  u BYTE-COPY
   stored ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   ROWS @ 1+ s cells SLOT + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

\ A caller may select an existing pool, but cannot manufacture an owner.
: OWNER? ( ptr u8 -- bool ) {: owner:ptr :}
   FIRST-P @
   begin dup 0= 0= while
      dup owner = if drop true exit then
      NEXT@
   repeat
   drop false ;

: LAST-SEGMENT ( ptr u8 -- ptr u8 )
   begin dup MORE@ 0= 0= while MORE@ repeat ;

: ACTIVE-SEGMENT? ( ptr u8 -- bool ) {: owner:ptr :}
   ACTIVE-P @
   begin dup 0= 0= while
      dup owner = if drop true exit then
      MORE@
   repeat
   drop false ;

: BODY-BYTES ( ptr u8 -- n ) {: owner:ptr :}
   0  owner ROWS-FIELD @ 0 ?do  owner LENGTHS i cells + @ +  loop ;

\ The bodies outside the active pool, by rows and bytes, duplicates counted: no
\ more than these can a link copy into the pool (src/habu/aot-closure.f
\ MAPPED-DATA), since equal bodies share a row within the segment written.
: FOREIGN-ROWS ( -- n )
   0 FIRST-P @
   begin dup 0= 0= while
      dup ACTIVE-SEGMENT? 0= if dup ROWS-FIELD @ rot + swap then
      NEXT@
   repeat
   drop ;

: FOREIGN-BYTES ( -- n )
   0 FIRST-P @
   begin dup 0= 0= while
      dup ACTIVE-SEGMENT? 0= if dup BODY-BYTES rot + swap then
      NEXT@
   repeat
   drop ;

: SEGMENT ( n -- ptr u8 ) {: k:n :}
   k 0 < if E-NSTR-BODY throw then
   ACTIVE-P @
   k 0 ?do
      MORE@  dup 0= if E-NSTR-BODY throw then
   loop ;

: SOURCE-RECORD ( n n -- n ) {: address:n size:n :}
   SOURCE-ON @ if
      SOURCE-N @ 1+ SOURCE-ADDR-RESERVE
      SOURCE-N @ 1+ SOURCE-LEN-RESERVE
      address SOURCE-N @ SOURCE-ADDR !
      size SOURCE-N @ SOURCE-LEN !
      1 SOURCE-N +!
   then
   address ;

public

: ACTIVE ( -- ptr u8 ) ACTIVE-P @ ;

: SOURCE-BEGIN ( -- )
   0 SOURCE-N ! -1 SOURCE-ON ! ;

: SOURCE-END ( -- )
   0 SOURCE-ON ! 0 SOURCE-N ! ;

: SOURCE-COUNT ( -- n ) SOURCE-N @ ;

: SOURCE@ ( n -- ptr u8 n ) {: row:n :}
   row 0 < row SOURCE-N @ >= or if E-NSTR-BODY throw then
   row SOURCE-ADDR @ NULL-PTR BYTE-VIEW + row SOURCE-LEN @ ;

: SOURCE-CLOSE ( -- )
   SOURCE-END
   SOURCE-ADDR-RELEASE SOURCE-LEN-RELEASE ;

: SWITCH ( ptr u8 -- ) {: owner:ptr :}
   owner OWNER? 0= if E-NSTR-BODY throw then
   \ A pool is selected by its first segment: imported owners are compact,
   \ already-full metadata, and a later segment belongs to its pool's first.
   owner ROLE-FIELD @ {: role:n :}
   role ROLE-HEAD <> role ROLE-CLOSED <> and if E-NSTR-BODY throw then
   owner ACTIVE-P !
   owner LAST-SEGMENT WRITE-P !
   SLOT-CLEAR
   ROWS @ 0 ?do
      i 1+  i ROW$ FREE-SLOT cells SLOT + !
   loop ;

\ The capture driver calls this after latching D0, before evaluating source.
\ Old pools stay allocated because published routines still hold their bytes.
: WINDOW-OPEN ( -- )
   ROLE-HEAD NEW-SEGMENT dup PUBLISH SWITCH ;

\ The stripped-image driver calls this before it latches the window's end, with
\ the window's pool active (src/habu/aot-window-latch.f AOT-DATA-SPAN). The link
\ then copies into this pool every body it reaches in another, and each copy has
\ to land inside the window: the last segment keeps room for every body outside
\ the pool, up to a whole segment's, and the pool opens no segment afterwards. A
\ link that outgrows that room is refused with E-NSTR-CAP.
: WINDOW-CLOSE ( -- )
   FOREIGN-ROWS ROWS-MAX min  ROWS-MAX ROWS @ -  >
   FOREIGN-BYTES ARENA-CAP min  ARENA-CAP USED @ -  >  or
   if GROW then
   ROLE-CLOSED ACTIVE-P @ ROLE-FIELD ! ;

\ `s" "` is a body: it gets a row and an address like any other.
: STRING ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   SPACE? if a u SPACE-STRING u SOURCE-RECORD exit then
   a u FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + u SOURCE-RECORD exit then
   a u ADD {: row:n :}
   BASE row ROW-OFF + u SOURCE-RECORD ;

: INTERN-SOURCE ( ptr u8 n -- n n )
   STRING
   SOURCE-ON @ if SOURCE-N @ negate 1- else -1 then ;

\ Tier 0 interns through this cell (src/habu/habu2.f LNSTRINTERN); xt! declares it.
: INSTALL ( -- )
   ['] STRING data-base NSTR-ABI:INTERN-CELL + xt! ;

private

\ Only the build driver may import the retained owner's row tables; it proves
\ the arena belongs to the capture first. The driver finds this private word by
\ name after the target loads, since the target's pool did not exist when the
\ driver compiled. A public wrapper would let any source import literal
\ ownership, and an engine cell costs a fixed slot and a layout row, so the
\ capture keeps the name on purpose
\ (src/habu/aot-capture.f ACAP-KEEP?).
: IMPORT-ROWS ( ptr u8 n ptr n ptr n -- )
   {: arena:ptr rows:n source-off:ptr source-len:ptr :}
   rows source-off source-len IMPORT-CHECK
   align here {: owner:ptr :}
   rows OWNER-SIZE allot
   owner arena rows ROLE-IMPORTED OWNER-INIT
   source-off BYTE-VIEW owner OFFSETS BYTE-VIEW rows cells BYTE-COPY
   source-len BYTE-VIEW owner LENGTHS BYTE-VIEW rows cells BYTE-COPY
   rows owner ROWS-FIELD !
   owner PUBLISH ;

\ FILE selection predicts literal placements across its entire chosen tree.
\ The planned rows borrow bodies from owned prepared ranges; no pool or DATA
\ byte changes until the corresponding range is installed.
DYNAMIC-BUFFER PLAN-P ptr u8
DYNAMIC-BUFFER PLAN-U n
DYNAMIC-BUFFER PLAN-ADDR n
variable PLAN-N
variable PLAN-FIRST
variable PLAN-ROWS
variable PLAN-USED
variable PLAN-BASE
variable PLAN-DATA-END
variable PLAN-LIVE
variable PLAN-ON

: PLAN-SEARCH ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   PLAN-N @ PLAN-FIRST @ ?do
      i PLAN-P @ i PLAN-U @ a u STR= if
         i PLAN-ADDR @ true unloop exit
      then
   loop
   PLAN-LIVE @ if
      a u FIND {: row:n :}
      row 0 >= if PLAN-BASE @ row ROW-OFF + true exit then
   then
   0 false ;

: PLAN-GROW ( -- )
   ACTIVE-P @ ROLE-FIELD @ ROLE-CLOSED = if E-NSTR-CAP throw then
   PLAN-DATA-END @ CELL 1- + CELL negate and {: aligned:n :}
   aligned ROWS-MAX OWNER-SIZE + PLAN-BASE !
   aligned ROWS-MAX OWNER-SIZE + ARENA-CAP CELL + + PLAN-DATA-END !
   0 PLAN-ROWS ! 0 PLAN-USED ! 0 PLAN-LIVE !
   PLAN-N @ PLAN-FIRST ! ;

public

: PREPARE-BEGIN ( n -- ) {: data-end:n :}
   data-end PLAN-DATA-END !
   STRING-ARENA PTR>N PLAN-BASE !
   ROWS @ PLAN-ROWS ! USED @ PLAN-USED !
   -1 PLAN-LIVE !
   0 PLAN-N ! 0 PLAN-FIRST ! -1 PLAN-ON ! ;

: PREPARE-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   PLAN-ON @ 0= if E-NSTR-BODY throw then
   u 0 < if E-NSTR-BODY throw then
   a u PLAN-SEARCH if exit then drop
   u ARENA-CAP > if E-NSTR-CAP throw then
   PLAN-ROWS @ ROWS-MAX >=
   u ARENA-CAP PLAN-USED @ - > or if PLAN-GROW then
   u 0= if ARENA-CAP else PLAN-USED @ then {: off:n :}
   PLAN-BASE @ off + {: address:n :}
   PLAN-N @ 1+ PLAN-P-RESERVE
   PLAN-N @ 1+ PLAN-U-RESERVE
   PLAN-N @ 1+ PLAN-ADDR-RESERVE
   a PLAN-N @ PLAN-P ! u PLAN-N @ PLAN-U !
   address PLAN-N @ PLAN-ADDR !
   1 PLAN-N +! 1 PLAN-ROWS +! u PLAN-USED +!
   address ;

: PREPARE-DATA-END ( -- n ) PLAN-DATA-END @ ;
: PREPARE-CLOSE ( -- )
   PLAN-P-RELEASE PLAN-U-RELEASE PLAN-ADDR-RELEASE
   0 PLAN-N ! 0 PLAN-FIRST ! 0 PLAN-ROWS ! 0 PLAN-USED !
   0 PLAN-BASE ! 0 PLAN-DATA-END ! 0 PLAN-LIVE ! 0 PLAN-ON ! ;

public

: SEGMENTS ( -- n )
   0 ACTIVE-P @
   begin dup 0= 0= while
      swap 1+ swap MORE@
   repeat
   drop ;

\ The retained owner's literal rows, as the build driver reads them, one segment
\ of the active pool at a time: the arena and its capacity for the span check,
\ then the rows themselves. Published here, while the package is open, because
\ the alternative is the driver finding STRING-ARENA, ARENA-CAP, R-OFF and R-LEN by name
\ in this package's private wordlist after it closes.
: SOURCE-SPAN ( n -- ptr u8 n )
   SEGMENT ARENA@ ARENA-CAP ;

: SOURCE-ROWS ( n -- ptr u8 n ptr n ptr n ) {: k:n :}
   k SEGMENT {: seg:ptr :}
   seg ARENA@ seg ROWS-FIELD @ seg OFFSETS seg LENGTHS ;

\ Every owner header and row table is cell storage. The separate arena holds
\ literal bodies, whose byte extents are reported by OWNER-ROW.
: CELL-SPANS ( [ ptr u8 n -- ] -- ) {: keep :}
   FIRST-P @
   begin dup 0= 0= while
      {: owner:ptr :}
      owner owner CAP-FIELD @ OWNER-SIZE keep execute
      owner NEXT@
   repeat drop
   SLOT SLOTS cells keep execute ;

\ Query only registered row metadata, never memory at the candidate address.
: OWNER-ROW ( n -- ptr u8 n bool ) {: address:n :}
   FIRST-P @
   begin dup 0= 0= while
      dup address swap FIND-OWNER-ROW if
         rot drop true exit
      then
      2drop NEXT@
   repeat
   drop NULL-PTR 0 false ;

: REINTERN-OWNED ( n -- n bool ) {: address:n :}
   address OWNER-ROW 0= if 2drop address false exit then
   {: body:ptr size:n :}
   address body PTR>N - {: offset:n :}
   body size STRING offset + true ;

\ The active pool's rows and bytes, over all its segments.
: COUNT ( -- n )
   0  SEGMENTS 0 ?do  i SEGMENT ROWS-FIELD @ +  loop ;

: BYTES ( -- n )
   0  SEGMENTS 0 ?do  i SEGMENT USED-FIELD @ +  loop ;

seal-package

INSTALL

;package
