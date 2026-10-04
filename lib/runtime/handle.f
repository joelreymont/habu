\ handle.f - generation-checked handles and the tables that issue them, for
\ the browser runtime (docs/browser-runtime.md §2.1).
\
\ STORAGE CLASS. CALLER-OWNED: the package keeps no state. A table lives in the
\ caller's cells, HEADER-CELLS of them for its header and one per slot (each
\ `n TYPED-BUFFER S n`), so any number of tasks use their own tables at once.
\ The header is a private record: OPEN writes it into zeroed cells and answers
\ the table, an opaque token for them. A table cell OPEN has not filled holds
\ the zero token, which ISSUE, INDEX and RELEASE refuse before reading a cell.
\
\ A handle is (slot u32, generation u32) in one cell, the slot in the low half,
\ which is the wire's little-endian order. Null is both halves zero; a handle
\ with only one of them zero is refused where one enters, at HANDLE and
\ BYTES>HANDLE.
\
\ BOUNDARY. Outside the package no typed route leads from a table to its
\ header or slots, and none from a number to a table: its converters and the
\ header's constructor are private, and a cast from a number to a table or a
\ handle is E-CAST-OWNER. Numbers become a handle only through HANDLE, and
\ bytes only through BYTES>HANDLE. SLOT and GENERATION read a handle's halves
\ back as numbers and grant no authority: a handle rebuilt from them passes a
\ lookup only if its table issued it. Construction establishes only that a
\ handle is well formed; a table's INDEX, or a pool's lookup of its own
\ handles, establishes that it was issued and is live. Raw storage passes the
\ refusals above: a private pointer mint (docs/forth.md, CAST:), such as any
\ package's own `CAST: ( RT-HANDLE:table -- ptr n )`, reads and writes a
\ header and, through it, the slots, and the public byte and cell views over a
\ cell that holds a handle or a table (`T BYTE-VIEW CELL-VIEW !`) store any
\ number there, a fabricated table included. The header and slot cells also
\ stay the caller's buffers, which the package does not guard: writing them
\ can reopen a header or rewind a slot and revive a released handle. Each
\ table owns its header and slot cells exclusively: no two tables, and no
\ table and counter, share a cell, and OPEN is never handed cells a live table
\ or counter uses. What follows holds while only this package's words write a
\ table's cells and each table's cells are its own.
\
\ A handle names no table, so two tables over overlapping ranges of slots
\ cannot tell their handles apart: the context opens one table per allocator
\ kind and assigns each a range disjoint from every other table's. A handle
\ outside a table's range is foreign to it, null included. Generations of a
\ slot run 1 .. 2^32 - 1; releasing its last one retires the slot, which is
\ never issued again, so a slot's generations never wrap. A header opens once,
\ so a table never issues a handle twice. A restart opens a fresh zeroed
\ header, a second table over its range, so only the runtime's epoch tells the
\ earlier incarnation's handles apart (§2.1).

require lib/errors.f
require lib/type/deftype.f

package RT-HANDLE

public

\ The decade -9510..-9519.
-9510 constant E-RT-HANDLE-FIRST
-9519 constant E-RT-HANDLE-LAST
-9510 constant E-RT-HANDLE-LENGTH     \ a handle span that is not 8 bytes at a non-null address
-9511 constant E-RT-HANDLE-HALF-NULL  \ a handle with exactly one of its slot and generation zero
-9512 constant E-RT-HANDLE-STALE      \ a slot of the table whose live generation is not the handle's
-9513 constant E-RT-HANDLE-FOREIGN    \ a slot outside the table's range, null included
-9514 constant E-RT-HANDLE-FULL       \ no slot to issue: each is live or retired
-9515 constant E-RT-HANDLE-UNOPENED   \ ISSUE, INDEX or RELEASE of the zero table token, a table cell OPEN has not filled
-9516 constant E-RT-HANDLE-RANGE      \ OPEN over null storage, or of a range that is empty, starts at slot 0, passes slot 2^32 - 1 or holds over 2^31 - 1 slots
-9517 constant E-RT-HANDLE-OPEN       \ OPEN over a header that is already open
-9518 constant E-RT-HANDLE-WIDE       \ HANDLE of a slot or a generation outside 0 .. 2^32 - 1

DEFTYPE HANDLE
undefine >HANDLE
undefine HANDLE>N

DEFTYPE TABLE
undefine >TABLE
undefine TABLE>N

private

\ slots: the caller's slot cells; first: the slot number of index 0; cap: the
\ slot count, 0 until OPEN; used: how many indexes have been issued at least
\ once; head: the first free index + 1, 0 when none.
STRUCTURE header 0 DERIVE addr
   FIELD slots ptr n
   FIELD first n
   FIELD cap n
   FIELD used n
   FIELD head n
;STRUCTURE

public

EXPORT HEADER-CELLS

private

CAST: >HANDLE ( n -- handle )
CAST: HANDLE>N ( handle -- n )
CAST: >TABLE ( ptr n -- table )
CAST: TABLE>HEADER ( table -- ptr header )

8 constant HANDLE-BYTES
$FFFFFFFF constant U32-MAX
U32-MAX constant LAST-GEN
$7FFFFFFF constant MAX-SLOTS

\ A slot cell holds the generation last issued in bits 0-31, LIVE while that
\ generation's handle is out and, while the slot is free, the next free index
\ + 1 from bit 33 up (0 at the end): MAX-SLOTS keeps that link in 31 bits.
$100000000 constant LIVE
33 constant LINK-SHIFT

: >SLOT-HANDLE ( n n -- handle )
   {: slot:n gen:n :}
   gen 32 lshift slot or >HANDLE ;

: CELL-GEN ( n -- n )
   U32-MAX and ;

: CELL-AT ( ptr n n -- ptr n )
   cells + ;

\ The table's header, refusing the zero token before anything reads through it.
: HEADER-OF ( table -- ptr header )
   TABLE>HEADER {: hd:ptr :}
   hd 0= if E-RT-HANDLE-UNOPENED throw then
   hd ;

public

: NULL ( -- handle )
   0 >HANDLE ;

: NULL? ( handle -- bool )
   HANDLE>N 0= ;

\ The handle's slot, 0 for null.
: SLOT ( handle -- n )
   HANDLE>N U32-MAX and ;

\ The handle's generation, 0 for null.
: GENERATION ( handle -- n )
   HANDLE>N 32 rshift ;

\ The handle of a slot at a generation, each within 0 .. 2^32 - 1 and both
\ zero, the null handle, or neither.
: HANDLE ( n n -- handle )
   {: slot:n gen:n :}
   slot gen or U32-MAX invert and 0<> if E-RT-HANDLE-WIDE throw then
   slot 0= gen 0= xor if E-RT-HANDLE-HALF-NULL throw then
   slot gen >SLOT-HANDLE ;

\ The handle in eight little-endian bytes: the slot u32, then the generation.
: BYTES>HANDLE ( ptr u8 n -- handle )
   {: a:ptr u:n :}
   u HANDLE-BYTES <> if E-RT-HANDLE-LENGTH throw then
   a 0= if E-RT-HANDLE-LENGTH throw then
   0 HANDLE-BYTES 0 do 8 lshift a HANDLE-BYTES 1 - i - + c@ or loop
   {: v:n :}
   v U32-MAX and v 32 rshift HANDLE ;

\ Open a table over `count` slot cells numbered from `first`, its header the
\ HEADER-CELLS zeroed cells at `hdr`. The last slot, first + count - 1, is
\ bounded as first against 2^32 - count, which cannot wrap.
: OPEN ( ptr n n n ptr n -- table )
   {: slots:ptr count:n first:n hdr:ptr :}
   slots 0= hdr 0= or count 1 < or first 1 < or if E-RT-HANDLE-RANGE throw then
   count MAX-SLOTS > if E-RT-HANDLE-RANGE throw then
   first U32-MAX count - 1 + > if E-RT-HANDLE-RANGE throw then
   hdr >TABLE {: t:table :}
   t TABLE>HEADER {: hd:ptr :}
   hd HEADER-CAP @ 0<> if E-RT-HANDLE-OPEN throw then
   slots first count 0 0 HEADER-MAKE hd !
   t ;

\ The index in the table of the slot a live handle names, 0 for `first`.
: INDEX ( handle table -- n )
   {: h:handle t:table :}
   t HEADER-OF @ HEADER-UNMAKE {: slots:ptr first:n cap:n used:n head:n :}
   h SLOT first - {: i:n :}
   i 0 < i cap >= or if E-RT-HANDLE-FOREIGN throw then
   i used >= if E-RT-HANDLE-STALE throw then
   slots i CELL-AT @ {: s:n :}
   s LIVE and 0= if E-RT-HANDLE-STALE throw then
   s CELL-GEN h GENERATION <> if E-RT-HANDLE-STALE throw then
   i ;

\ A free slot's next generation, else a never-issued slot's first one.
: ISSUE ( table -- handle )
   HEADER-OF {: hd:ptr :}
   hd @ HEADER-UNMAKE {: slots:ptr first:n cap:n used:n head:n :}
   head 0<> if
      head 1 - {: i:n :}
      slots i CELL-AT @ {: s:n :}
      s CELL-GEN 1 + {: g:n :}
      g LIVE or slots i CELL-AT !
      s LINK-SHIFT rshift hd HEADER-HEAD !
      first i + g >SLOT-HANDLE exit
   then
   used cap < if
      1 LIVE or slots used CELL-AT !
      used 1 + hd HEADER-USED !
      first used + 1 >SLOT-HANDLE exit
   then
   E-RT-HANDLE-FULL throw ;

\ Free the handle's slot for its next generation, or retire it after its last.
: RELEASE ( handle table -- )
   {: h:handle t:table :}
   h t INDEX {: i:n :}
   t HEADER-OF {: hd:ptr :}
   hd HEADER-SLOTS @ i CELL-AT {: p:ptr :}
   h GENERATION {: g:n :}
   g LAST-GEN = if g p ! exit then
   hd HEADER-HEAD @ LINK-SHIFT lshift g or p !
   i 1 + hd HEADER-HEAD ! ;

;package
