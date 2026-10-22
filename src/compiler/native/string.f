\ string.f - where a string literal's bytes live once the chain has compiled one.
\ One concern: turning a literal's body into a permanent address, and turning
\ equal bodies within a segment into the SAME permanent address.
\
\ The bytes live in DATA because a DATA address means the same thing after a
\ snapshot restore and an mmap one does not. A pool is a chain of segments, each
\ an owner record, its row tables and an arena. A capture or a retained compiler
\ opens a pool, and the store opens the pool's next segment at `here` when the
\ last one cannot take a body, so a pool holds as many bodies and bytes as DATA
\ does; one body larger than a segment's arena is refused by name, and so is a
\ body a closed window's pool has no room left for (WINDOW-CLOSE). Fetch
\ descriptors (src/compiler/native/fetch.f KEEP) are bodies under the same rule.
\ Equal bodies share an address within the segment being written: a body
\ interned again after its segment filled is copied into the next one.
\
\ A segment can open inside work that is later undone - a failed evaluation or
\ REPL line, a rolled-back declaration - while the addresses it answered are
\ already compiled into published routines. So every owner the store publishes
\ raises the engine's DATA floor (src/habu/layout.f DATA-FLOOR-CELL) past
\ itself, and every rewind of DATA stops at the floor.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/habu/layout.f

package NSTR
private

\ ---- the store ---------------------------------------------------------------
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

: ARENA ( -- ptr u8 ) WRITE-P @ ARENA@ ;
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
   ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

: OWNER-ROW$ ( ptr u8 n -- ptr u8 n ) {: owner:ptr row:n :}
   owner ARENA@ owner OFFSETS row cells + @ +
   owner LENGTHS row cells + @ ;

: LAST-OWNER ( -- ptr u8 )
   FIRST-P @
   begin dup NEXT@ 0= 0= while NEXT@ repeat ;

\ `here` is past the owner just laid out, its rows and its arena, so no rewind of
\ DATA from now on frees any of them.
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
\ FNV-1a, masked by an `and` against a positive constant so a hash whose top bit
\ ran into the sign still lands inside the table.
: HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   $811C9DC5 HV !
   u 0 ?do
      a i + c@ HV @ xor  $1000193 *  HV !
   loop
   HV @ SLOTS 1- and ;

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
   a  ARENA stored +  u BYTE-COPY
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

\ Segment n of the active pool, counted from its first.
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
: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   a u FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + u SOURCE-RECORD exit then
   a u ADD {: row:n :}
   BASE row ROW-OFF + u SOURCE-RECORD ;

: INTERN-SOURCE ( ptr u8 n -- n n )
   INTERN
   SOURCE-ON @ if SOURCE-N @ negate 1- else -1 then ;

private

\ Only the build driver may import the retained owner's actual row tables; it
\ proves the byte arena belongs to the capture before invoking this private seam,
\ and test/compiler/native-string.f pins the name as unfindable so no source can
\ invoke literal ownership import.
\ THE DRIVER STILL FINDS IT BY NAME, and it is the last name in the build path
\ that reaches a private word. A native build hands the retained host's rows to
\ the FRESHLY LOADED target's pool, which did not exist when the driver was
\ compiled, so the routine has to be found after the load; a public wrapper
\ would be a well-typed way for any source to invoke literal ownership import,
\ which test/compiler/native-string.f exists to forbid, and an engine cell for
\ the token costs a fixed slot and a layout row. So the capture ships this name
\ on purpose: it is a keep-set entry in src/habu/aot-capture.f ACAP-KEEP?.
\ ATTACH-SEGMENTS has the same private build-driver boundary.
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

\ The file-object importer copies complete new segments into its DATA extent.
\ Check their owner metadata and links before joining them to the existing
\ pool. The caller has already checked each owner's extent against that DATA
\ section. Old tail links are supplied from before its prefix DATA updates:
\ those updates contain the source image's relative link bytes.
: ATTACH-OWNER ( ptr u8 n -- ptr u8 ) {: list:ptr row:n :}
   list row cells + CELL-VIEW @ NULL-PTR BYTE-VIEW + ;

: ATTACH-CHECK ( ptr u8 -- ) {: owner:ptr :}
   owner ROLE-FIELD @ ROLE-MORE <> if E-NSTR-BODY throw then
   owner CAP-FIELD @ ROWS-MAX <> if E-NSTR-BODY throw then
   owner ARENA@ owner ROWS-MAX OWNER-SIZE + <> if E-NSTR-BODY throw then
   owner ROWS-FIELD @ owner OFFSETS owner LENGTHS IMPORT-CHECK
   owner USED-FIELD @ {: used:n :}
   used 0 < used ARENA-CAP > or if E-NSTR-BODY throw then
   owner ROWS-FIELD @ 0 ?do
      owner OFFSETS i cells + @ {: off:n :}
      owner LENGTHS i cells + @ {: size:n :}
      size 0<> if off size + used > if E-NSTR-BODY throw then then
   loop ;

: ATTACH-SEGMENTS ( ptr u8 n n n -- )
   {: owners:ptr count:n old-last:n old-write:n :}
   owners 0 ATTACH-OWNER {: first:ptr :}
   owners count 1- ATTACH-OWNER {: last:ptr :}
   first old-last NULL-PTR BYTE-VIEW + NEXT-FIELD LINK!
   first old-write NULL-PTR BYTE-VIEW + MORE-FIELD LINK!
   last WRITE-P !
   SLOT-CLEAR
   ROWS @ 0 ?do
      i 1+ i ROW$ FREE-SLOT cells SLOT + !
   loop
   last ARENA@ ARENA-CAP CELL + + PTR>N data-base PTR>N -
      data-base DATA-FLOOR-CELL + {: floor:ptr :}
   dup floor @ > if floor ! else drop then ;

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
PTR-VARIABLE PLAN-STAGED
variable PLAN-ON
variable PLAN-LAST
variable PLAN-WRITE

: PLAN-SEARCH ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   PLAN-N @ PLAN-FIRST @ ?do
      i PLAN-P @ i PLAN-U @ a u STR= if
         i PLAN-ADDR @ true unloop exit
      then
   loop
   PLAN-LIVE @ if
      a u FIND {: row:n :}
      row 0 >= if PLAN-BASE @ row ROW-OFF + true exit then
   else
      PLAN-STAGED @ NULL-PTR <> if
         PLAN-STAGED @ {: owner:ptr :}
         owner ROWS-FIELD @ 0 ?do
            owner i OWNER-ROW$ a u STR= if
               PLAN-BASE @ owner OFFSETS i cells + @ +
                  true unloop exit
            then
         loop
      then
   then
   0 false ;

: PLAN-GROW ( -- )
   ACTIVE-P @ ROLE-FIELD @ ROLE-CLOSED = if E-NSTR-CAP throw then
   PLAN-DATA-END @ CELL 1- + CELL negate and {: aligned:n :}
   aligned PLAN-LAST ! aligned PLAN-WRITE !
   aligned ROWS-MAX OWNER-SIZE + PLAN-BASE !
   aligned ROWS-MAX OWNER-SIZE + ARENA-CAP CELL + + PLAN-DATA-END !
   0 PLAN-ROWS ! 0 PLAN-USED ! 0 PLAN-LIVE !
   NULL-PTR PLAN-STAGED !
   PLAN-N @ PLAN-FIRST ! ;

public

: PREPARE-BEGIN ( n -- ) {: data-end:n :}
   data-end PLAN-DATA-END !
   ARENA PTR>N PLAN-BASE !
   ROWS @ PLAN-ROWS ! USED @ PLAN-USED !
   -1 PLAN-LIVE ! NULL-PTR PLAN-STAGED !
   LAST-OWNER PTR>N PLAN-LAST ! WRITE-P @ PTR>N PLAN-WRITE !
   0 PLAN-N ! 0 PLAN-FIRST ! -1 PLAN-ON ! ;

: PREPARE-TAILS ( -- n n ) PLAN-LAST @ PLAN-WRITE @ ;

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
: PREPARE-ADVANCE ( n -- ) {: size:n :}
   size 0 < size MEM-MAX-N PLAN-DATA-END @ - > or
      if E-NSTR-BODY throw then
   size PLAN-DATA-END +! ;

: PREPARE-SEGMENTS ( ptr u8 n n n n -- )
   {: owners:ptr count:n old-last:n old-write:n projected-last:n :}
   PLAN-ON @ 0= if E-NSTR-BODY throw then
   count 0 <= old-last 0= or old-write 0= or
   old-last PLAN-LAST @ <> or old-write PLAN-WRITE @ <> or
      if E-NSTR-BODY throw then
   count 0 ?do
      owners i ATTACH-OWNER {: owner:ptr :}
      owner ATTACH-CHECK
      i 1+ count < if owners i 1+ ATTACH-OWNER else NULL-PTR then
         {: next:ptr :}
      owner NEXT@ next <> owner MORE@ next <> or
         if E-NSTR-BODY throw then
   loop
   owners count 1- ATTACH-OWNER {: owner:ptr :}
   projected-last owner ARENA@ PTR>N owner PTR>N - + PLAN-BASE !
   owner ROWS-FIELD @ PLAN-ROWS !
   owner USED-FIELD @ PLAN-USED !
   projected-last PLAN-LAST ! projected-last PLAN-WRITE !
   0 PLAN-LIVE ! owner PLAN-STAGED !
   PLAN-N @ PLAN-FIRST ! ;

: PREPARE-CLOSE ( -- )
   PLAN-P-RELEASE PLAN-U-RELEASE PLAN-ADDR-RELEASE
   0 PLAN-N ! 0 PLAN-FIRST ! 0 PLAN-ROWS ! 0 PLAN-USED !
   0 PLAN-BASE ! 0 PLAN-DATA-END ! 0 PLAN-LIVE ! 0 PLAN-ON !
   0 PLAN-LAST ! 0 PLAN-WRITE !
   NULL-PTR PLAN-STAGED ! ;

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
\ the alternative is the driver finding ARENA, ARENA-CAP, R-OFF and R-LEN by name
\ in this package's private wordlist after it closes.
: SOURCE-SPAN ( n -- ptr u8 n )
   SEGMENT ARENA@ ARENA-CAP ;

: SOURCE-ROWS ( n -- ptr u8 n ptr n ptr n ) {: k:n :}
   k SEGMENT {: seg:ptr :}
   seg ARENA@ seg ROWS-FIELD @ seg OFFSETS seg LENGTHS ;

: SEGMENT-BYTES ( -- n ) ROWS-MAX OWNER-SIZE ARENA-CAP CELL + + ;

: SOURCE-SEGMENT-SPAN ( n -- ptr u8 n )
   SEGMENT {: seg:ptr :}
   seg SEGMENT-BYTES ;

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

\ The importer records these before applying source prefix-byte updates.
: SOURCE-TAILS ( -- n n ) LAST-OWNER PTR>N WRITE-P @ PTR>N ;

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
   body size INTERN offset + true ;

\ The active pool's rows and bytes, over all its segments.
: COUNT ( -- n )
   0  SEGMENTS 0 ?do  i SEGMENT ROWS-FIELD @ +  loop ;

: BYTES ( -- n )
   0  SEGMENTS 0 ?do  i SEGMENT USED-FIELD @ +  loop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
