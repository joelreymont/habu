\ string.f - where a string literal's bytes live once the chain has compiled one.
\ One concern: turning a literal's body into a permanent address, and turning
\ equal bodies into the SAME permanent address.
\
\ WHY THE BYTES NEED A HOME AT ALL. A definition the chain compiles may hand its
\ string out - 186 of the definitions in lib, src/core and src/compiler that hold
\ a literal declare a pointer among their outputs - so the address a routine
\ pushes has to be good after that routine returns, for as long as the image
\ lives. Nothing about a compilation is that long-lived: the tape is per
\ definition, the emitter's buffer is per routine, and the text the reader kept
\ is the migration's own scratch. So the bytes are copied once into storage that
\ outlives all three.
\
\ AND WHY THAT HOME IS DATA SPACE. src/os/linux/layout.f fixes DATA at a constant
\ virtual address, which is exactly why src/habu/habu2.f EMIT-CREATE builds a
\ `create`d word's body with C-DATA-ADDR and no SNAP-RELOC:MARK-SITE while
\ C-CODE-ADDR marks every code address it writes: a DATA address means the same
\ thing after a snapshot restore and a code address does not. A string here is
\ therefore the same kind of value a `create`d buffer already is, and the chain
\ already carries one of those - src/compiler/native/elaborate.f EMIT-FIXED
\ stages a data word's address as an ordinary integer literal. So a literal needs
\ no new memory form, no new relocation kind and no new machine operation; it
\ needs an address and a length, which are two constants.
\
\ AN mmap SPAN WOULD NOT DO, and it is worth saying which one this is not.
\ lib/memory.f hands out process-local mappings that no snapshot carries, so an
\ address into one is good until the image is written and wrong afterwards -
\ silently, because it is an ordinary integer either way. DATA is the only home
\ whose lifetime is the image's.
\
\ INTERNING IS REQUIRED HERE AND IS NOT AN OPTIMISATION. The chain re-elaborates
\ a definition that ran out of registers and compiles it again, so a store that
\ allocated per SITE would leak a copy on every attempt and a definition that was
\ refused would leave bytes behind that nothing points at. Keyed by content, an
\ allocation is idempotent: a second attempt at the same definition allocates
\ nothing, and a refusal costs at most that definition's distinct new bodies.
\ That is what stands in for "a refusal moves nothing" over a resource the
\ publication seam does not own - src/compiler/native/publish.f owns the code
\ arena, the call map, the record and the log, and not this.
\
\ AND THE STORE HAS NO RESET, WHICH IS A DESIGN STATEMENT RATHER THAN AN
\ OMISSION. Addresses out of here are compiled into published routines, so
\ handing the same bytes out twice is the one thing that must never happen. A
\ word that emptied the table would make every address already baked into code
\ point at bytes some later literal now owns.

require lib/prelude.f
require lib/errors.f
require lib/string.f

package NSTR
private

\ ---- the store ---------------------------------------------------------------
\ The arena is dictionary-sized static storage, which is what `create ... allot`
\ is for, and it is sized from a measurement rather than a guess: the string
\ literals of every plain colon definition in lib, src/core and src/compiler come
\ to 100282 payload bytes, so this holds them five times over with the equal
\ bodies among them folded together. The index is bounded too, and both ceilings
\ are one named refusal: a store that cannot take a body must say so rather than
\ hand back an address that means something else.
$80000 constant ARENA-CAP            \ 512 KB of bodies
8192 constant ROWS-MAX               \ distinct bodies
16384 constant SLOTS                 \ a power of two, twice ROWS-MAX

create ARENA ARENA-CAP allot
create R-OFF ROWS-MAX cells allot    \ each row's offset into the arena
create R-LEN ROWS-MAX cells allot    \ and its length
create SLOT SLOTS cells allot        \ hash slot: row index plus one, zero is empty

variable USED
variable ROWS
variable PROBE
variable FOUND
variable HV

\ The one crossing this package needs, and the reason it is a boundary. The
\ arena is a `create`d buffer, so the engine hands it over as a pointer; what a
\ compiled routine needs is the NUMBER that pointer is, because the elaborator
\ stages it as an ordinary integer literal exactly the way it stages a data
\ word's address. The checker has no term for "the integer this pointer is" and
\ this is the identity that says so. It is exercised by every case in
\ test/compiler/native-string.f, since no address this package answers is
\ reachable any other way. Retirement owner: the same typed-address capability
\ that would let src/compiler/native/dict.f stop answering a `create`d word's
\ address as a bare `n` (habu-guard-an-executed-8a0f2f77).
TRUSTED: PTR>N ( ptr a -- n ) ;

: BASE ( -- n )
   ARENA PTR>N ;

: ROW-OFF ( n -- n ) {: k:n :}
   k cells R-OFF + @ ;

: ROW-LEN ( n -- n ) {: k:n :}
   k cells R-LEN + @ ;

: ROW$ ( n -- ptr u8 n ) {: k:n :}
   ARENA k ROW-OFF +  k ROW-LEN ;

\ ---- finding a body that is already here --------------------------------------
\ FNV-1a over the bytes, masked to a slot. The mask is an `and` against a
\ positive constant, so a hash whose top bit ran into the sign still lands inside
\ the table.
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

\ Which row holds these bytes, or -1. The walk is bounded by the table rather
\ than by finding an empty slot, so a full table ends the search instead of
\ spinning in it.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if leave then
      SLOT@ 1- ROW$ a u STR= if SLOT@ 1- FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

\ And where a body that is NOT here would go, asked as its own question rather
\ than read off where the search above happened to stop.
: FREE-SLOT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u HASH PROBE !
   -1 FOUND !
   SLOTS 0 ?do
      SLOT@ 0= if PROBE @ FOUND ! leave then
      PROBE-NEXT
   loop
   FOUND @ ;

\ ---- putting one in ------------------------------------------------------------
\ Every ceiling is checked before anything moves, so a refused body leaves the
\ arena, the index and the row count exactly as it found them.
: ADD ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0 < if E-NSTR-BODY throw then
   ROWS @ ROWS-MAX >= if E-NSTR-CAP throw then
   USED @ u + ARENA-CAP > if E-NSTR-CAP throw then
   a u FREE-SLOT {: s:n :}
   s 0 < if E-NSTR-CAP throw then
   a  ARENA USED @ +  u BYTE-COPY
   USED @ ROWS @ cells R-OFF + !
   u ROWS @ cells R-LEN + !
   ROWS @ 1+ s cells SLOT + !
   USED @ u + USED !
   ROWS @ 1+ ROWS !
   ROWS @ 1- ;

public

\ The address of a permanent copy of these bytes. Equal bodies always answer the
\ same address, which is what makes a second attempt at a refused definition cost
\ nothing and what makes two sites writing the same string share one copy.
\
\ AN EMPTY BODY IS A BODY. `s" "` is the empty string, and it gets a row and an
\ address like any other: a caller that received no address for it would have
\ nothing to push, and the length it pushes beside the address is already zero.
: INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u FIND {: k:n :}
   k 0 >= if BASE k ROW-OFF + exit then
   BASE  a u ADD ROW-OFF  + ;

\ How much is in here. Both are published because the idempotence this package
\ exists for is only assertable from outside by watching them NOT move.
: COUNT ( -- n )
   ROWS @ ;

: BYTES ( -- n )
   USED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
