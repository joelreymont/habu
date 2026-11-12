\ intern.f - the permanent address of a string literal's bytes; equal bodies
\ within a segment share one. Bodies live in DATA, since a DATA address means the
\ same after a snapshot restore and an mmap one does not. Both tiers intern
\ through INTERN-SOURCE, and fetch descriptors are bodies under the same rule. A
\ build's data space keeps its strings at `here`.
\
\ A segment is an owner record, its row tables and an arena. The store opens the
\ next segment at `here` when the last cannot take a body, and a capture opens
\ one before its window's source. A body larger than an arena is refused by
\ name. A body interned again after its segment filled is copied into the next
\ one.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/habu/layout.f
require src/habu/intern-table.f

package INTERN
private

\ ---- the store ---------------------------------------------------------------
$80000 constant ARENA-CAP            \ 512 KB of bodies per segment
8192 constant ROWS-MAX               \ distinct bodies per segment
ROWS-MAX 2 * constant SLOTS          \ a power of two

3 cells constant OWNER-BYTES

\ The elaborator stages the arena address as an ordinary integer literal: its
\ distance from the null pointer.
: PTR>N ( ptr a -- n ) NULL-PTR - ;

\ The arena field holds the arena's distance from the field, not an address: a
\ native build moves the captured window as one block and relocates no raw
\ cell, and a rewind of DATA has no pointer mark to drop.
: ARENA-FIELD ( ptr u8 -- ptr n ) CELL-VIEW ;
: ROWS-FIELD ( ptr u8 -- ptr n ) 1 cells + CELL-VIEW ;
: USED-FIELD ( ptr u8 -- ptr n ) 2 cells + CELL-VIEW ;
: OFFSETS ( ptr u8 -- ptr n ) OWNER-BYTES + CELL-VIEW ;
: LENGTHS ( ptr u8 -- ptr n ) OFFSETS ROWS-MAX cells + ;

: LINK@ ( ptr n -- ptr u8 ) {: cell:ptr :}
   cell BYTE-VIEW cell @ + ;

: LINK! ( ptr u8 ptr n -- ) {: target:ptr cell:ptr :}
   target PTR>N cell PTR>N - cell ! ;

: ARENA@ ( ptr u8 -- ptr u8 ) ARENA-FIELD LINK@ ;

ROWS-MAX cells 2 * OWNER-BYTES + constant OWNER-SIZE

: CLEAR-CELLS ( ptr u8 n -- )
   {: buf:ptr bytes:n :}
   bytes CELL / 0 ?do 0 buf i cells + CELL-VIEW ! loop ;

: OWNER-INIT ( ptr u8 ptr u8 -- ) {: owner:ptr arena:ptr :}
   \ Allot leaves old bytes intact. Unused rows and arena capacity belong to
   \ this new owner too, and must not retain the build host's scratch values.
   owner OWNER-SIZE CLEAR-CELLS
   arena ARENA-CAP CELL + CLEAR-CELLS
   arena owner ARENA-FIELD LINK! ;

create BOOT-OWNER OWNER-SIZE allot
\ The separate empty-row byte gives it an address no nonempty row can own,
\ without counting a byte in USED or reducing the arena's body capacity.
create BOOT-ARENA ARENA-CAP CELL + allot
BOOT-OWNER BOOT-ARENA OWNER-INIT

PERSISTED-PTR-VARIABLE WRITE-P       \ the segment that takes new bodies
BOOT-OWNER WRITE-P !

: STRING-ARENA ( -- ptr u8 ) WRITE-P @ ARENA@ ;
: R-OFF ( -- ptr n ) WRITE-P @ OFFSETS ;
: R-LEN ( -- ptr n ) WRITE-P @ LENGTHS ;
: USED ( -- ptr n ) WRITE-P @ USED-FIELD ;
: ROWS ( -- ptr n ) WRITE-P @ ROWS-FIELD ;

create SLOT SLOTS cells allot       \ the last segment's hash slots: row index plus one
variable PROBE

: BASE ( -- n )
   STRING-ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   STRING-ARENA k ROW-OFF +  k ROW-LEN ;

\ A segment can open inside work later undone (a failed evaluation or REPL
\ line, a rolled-back declaration) while its addresses are already compiled
\ into published routines, so its end becomes the DATA floor and no rewind of
\ DATA frees it.
: FLOOR-RAISE ( -- )
   here PTR>N data-base PTR>N -  data-base DATA-FLOOR-CELL + ! ;

\ ---- finding a body that is already here --------------------------------------
\ Masked by an `and` against a positive constant so a hash whose top bit ran
\ into the sign still lands inside the table.
: HASH ( ptr u8 n -- n ) FNV SLOTS 1- and ;

: PROBE-NEXT ( -- )
   PROBE @ 1+ SLOTS 1- and PROBE ! ;

: SLOT@ ( -- n )
   PROBE @ cells SLOT + @ ;

: ROW-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   begin SLOT@ 0<> while
      SLOT@ 1- ROW$ a u STR= if SLOT@ 1- exit then
      PROBE-NEXT
   repeat
   -1 ;

\ Ends at an empty slot: a body is placed only while the segment holds fewer
\ than ROWS-MAX rows (ROOM?), and SLOTS is twice ROWS-MAX.
: FREE-SLOT ( ptr u8 n -- n )
   HASH PROBE !
   begin SLOT@ 0= 0= while PROBE-NEXT repeat
   PROBE @ ;

: SLOT-CLEAR ( -- )
   SLOTS 0 ?do 0 i cells SLOT + ! loop ;

public
\ A file object's link puts a body the build's data holds into the table.
: ADOPT ( n -- )
   {: body:n :}
   SPACE? 0= if E-COMP-STATE throw then
   data-base DPHW-CELL + @ 0<> if E-INTERN-REWIND throw then
   body SPACE-ADOPT 0<> if E-COMP-STATE throw then ;

private

\ ---- putting one in ------------------------------------------------------------
: NEW-SEGMENT ( -- )
   align here {: owner:ptr :}
   OWNER-SIZE allot
   here {: arena:ptr :}
   ARENA-CAP CELL + allot
   owner arena OWNER-INIT
   FLOOR-RAISE
   owner WRITE-P !
   SLOT-CLEAR ;

: ROOM? ( n -- bool ) {: u:n :}
   ROWS @ ROWS-MAX <  u ARENA-CAP USED @ - <=  and ;

\ A body no segment can hold is refused before anything moves, so the refusal
\ leaves no trace.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   u ARENA-CAP > if E-NSTR-CAP throw then
   u ROOM? 0= if NEW-SEGMENT then
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

public

\ A capture calls this after latching its window's start, before evaluating
\ its source, so the window's bodies land inside it.
: WINDOW-OPEN ( -- ) NEW-SEGMENT ;

\ `s" "` is a body: it gets a row and an address like any other.
: STRING ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   SPACE? if a u SPACE-STRING exit then
   a u ROW-FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + exit then
   a u ADD {: row:n :}
   BASE row ROW-OFF + ;

\ The second result is the origin its callers file beside the address: -1, none.
: INTERN-SOURCE ( ptr u8 n -- n n )
   STRING -1 ;

seal-package

;package
