\ string.f - where a string literal's bytes live once the chain has compiled one.
\ One concern: turning a literal's body into a permanent address, and turning
\ equal bodies into the SAME permanent address.
\
\ The bytes live in DATA because a DATA address means the same thing after a
\ snapshot restore and an mmap one does not. Each pool is reserved before
\ evaluation can rewind DATA; published literal addresses remain valid when an
\ evaluation fails. A retained compiler opens a fresh pool inside each capture.

require lib/prelude.f
require lib/errors.f
require lib/string.f

package NSTR
private

\ ---- the store ---------------------------------------------------------------
$80000 constant ARENA-CAP            \ 512 KB of bodies
8192 constant ROWS-MAX               \ distinct bodies
16384 constant SLOTS                 \ a power of two, twice ROWS-MAX

$28 constant OWNER-BYTES

\ Each pool owns its metadata as well as its bytes. Forward links leave a new
\ capture free of references into the older pools excluded from that capture.
\ The two complete row tables reserve 128 KiB per pool for that lasting authority.
: NEXT-FIELD ( ptr u8 -- ptr ptr u8 ) 0 ptr-field ;
: ARENA-FIELD ( ptr u8 -- ptr ptr u8 ) 1 ptr-field ;
: CAP-FIELD ( ptr u8 -- ptr n ) 2 cells + CELL-VIEW ;
: ROWS-FIELD ( ptr u8 -- ptr n ) 3 cells + CELL-VIEW ;
: USED-FIELD ( ptr u8 -- ptr n ) 4 cells + CELL-VIEW ;
: OFFSETS ( ptr u8 -- ptr n ) OWNER-BYTES + CELL-VIEW ;

: LENGTHS ( ptr u8 -- ptr n ) {: owner:ptr :}
   owner OFFSETS owner CAP-FIELD @ cells + ;

: OWNER-SIZE ( n -- n ) cells 2 * OWNER-BYTES + ;

: OWNER-INIT ( ptr u8 ptr u8 n -- ) {: owner:ptr arena:ptr cap:n :}
   owner ptr-cell-mark
   owner CELL + ptr-cell-mark
   NULL-PTR owner NEXT-FIELD !
   arena owner ARENA-FIELD !
   cap owner CAP-FIELD !
   0 owner ROWS-FIELD !  0 owner USED-FIELD ! ;

create BOOT-OWNER ROWS-MAX OWNER-SIZE allot
\ The separate empty-row byte gives it an address no nonempty row can own,
\ without charging a byte to BYTES or reducing the arena's body capacity.
create BOOT-ARENA ARENA-CAP CELL + allot
BOOT-OWNER BOOT-ARENA ROWS-MAX OWNER-INIT

PERSISTED-PTR-VARIABLE FIRST-P
PERSISTED-PTR-VARIABLE ACTIVE-P
BOOT-OWNER FIRST-P !  BOOT-OWNER ACTIVE-P !

: ARENA ( -- ptr u8 ) ACTIVE-P @ ARENA-FIELD @ ;
: R-OFF ( -- ptr n ) ACTIVE-P @ OFFSETS ;
: R-LEN ( -- ptr n ) ACTIVE-P @ LENGTHS ;
: USED ( -- ptr n ) ACTIVE-P @ USED-FIELD ;
: ROWS ( -- ptr n ) ACTIVE-P @ ROWS-FIELD ;

create SLOT SLOTS cells allot       \ live hash slot: row index plus one
variable PROBE
variable FOUND
variable HV

\ The checker has no term for "the integer this pointer is", and the elaborator
\ stages the arena address as an ordinary integer literal. Retires with
\ habu-guard-an-executed-8a0f2f77.
TRUSTED: PTR>N ( ptr a -- n ) ;

: BASE ( -- n )
   ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

: OWNER-ROW$ ( ptr u8 n -- ptr u8 n ) {: owner:ptr row:n :}
   owner ARENA-FIELD @ owner OFFSETS row cells + @ +
   owner LENGTHS row cells + @ ;

: APPEND-OWNER ( ptr u8 -- ) {: owner:ptr :}
   FIRST-P @
   begin dup NEXT-FIELD @ 0= 0= while NEXT-FIELD @ repeat
   NEXT-FIELD owner swap ! ;

: NEW-POOL ( -- ptr u8 )
   align here {: owner:ptr :}
   ROWS-MAX OWNER-SIZE allot
   here {: arena:ptr :}
   ARENA-CAP CELL + allot
   owner arena ROWS-MAX OWNER-INIT
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

\ Bounded by the table rather than by an empty slot, so a full table cannot spin.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if leave then
      SLOT@ 1- ROW$ a u STR= if SLOT@ 1- FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

: FREE-SLOT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if PROBE @ FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

\ ---- putting one in ------------------------------------------------------------
\ Every ceiling is checked before anything moves, so a refusal leaves no trace.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   ROWS @ ROWS-MAX >= if E-NSTR-CAP throw then
   u ARENA-CAP USED @ - > if E-NSTR-CAP throw then
   a u FREE-SLOT {: s:n :}
   s 0 < if E-NSTR-CAP throw then
   USED @ {: off:n :}
   u 0= if ARENA-CAP else off then {: stored:n :}
   a  ARENA stored +  u BYTE-COPY
   stored ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   ROWS @ 1+ s cells SLOT + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

public

\ The capture driver calls this after latching D0, before evaluating source.
\ Old pools stay allocated because published routines still hold their bytes.
\ Call outside any evaluation that could rewind this reservation on failure.
: WINDOW-OPEN ( -- )
   NEW-POOL dup APPEND-OWNER ACTIVE-P !
   SLOTS 0 ?do 0 i cells SLOT + ! loop ;

\ `s" "` is a body: it gets a row and an address like any other.
: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   a u FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + exit then
   BASE  a u ADD ROW-OFF  + ;

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
\ the token costs a fixed slot and a layout row. Recorded as the one keep-set
\ entry of habu-ship-no-dictionary-2fee2dea until that trade is decided.
: IMPORT-ROWS ( ptr u8 n ptr n ptr n -- )
   {: arena:ptr rows:n source-off:ptr source-len:ptr :}
   rows source-off source-len IMPORT-CHECK
   align here {: owner:ptr :}
   rows OWNER-SIZE allot
   owner arena rows OWNER-INIT
   source-off BYTE-VIEW owner OFFSETS BYTE-VIEW rows cells BYTE-COPY
   source-len BYTE-VIEW owner LENGTHS BYTE-VIEW rows cells BYTE-COPY
   rows owner ROWS-FIELD !
   owner APPEND-OWNER ;

public

\ The retained owner's literal rows, as the build driver reads them: the arena
\ and its capacity for the span check, then the rows themselves. Published here,
\ while the package is open, because the alternative is the driver finding
\ ARENA, ARENA-CAP, R-OFF and R-LEN by name in this package's private wordlist
\ after it closes.
: SOURCE-SPAN ( -- ptr u8 n )
   ARENA ARENA-CAP ;

: SOURCE-ROWS ( -- ptr u8 n ptr n ptr n )
   ARENA ROWS @ R-OFF R-LEN ;

\ Query only registered row metadata, never memory at the candidate address.
: OWNER-ROW ( n -- ptr u8 n bool ) {: address:n :}
   FIRST-P @
   begin dup 0= 0= while
      dup address swap FIND-OWNER-ROW if
         rot drop true exit
      then
      2drop NEXT-FIELD @
   repeat
   drop NULL-PTR 0 false ;

: REINTERN-OWNED ( n -- n bool ) {: address:n :}
   address OWNER-ROW 0= if 2drop address false exit then
   {: body:ptr size:n :}
   address body PTR>N - {: offset:n :}
   body size INTERN offset + true ;

: COUNT ( -- n )
   ROWS @ ;

: BYTES ( -- n )
   USED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
