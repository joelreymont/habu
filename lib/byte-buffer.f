\ byte-buffer.f - one package-owned growable byte buffer.
\
\ A BUF header is three cells the caller embeds ( BUF:HDR-BYTES ): a `ptr u8`
\ storage pointer, an active byte length, and a byte capacity. The buffer owns the
\ ONE OS mapping at the storage pointer; the capacity cell is the ownership token
\ (positive = a live mapping of that many bytes is owned, zero = no mapping owned,
\ fresh or disposed). It mirrors lib/vector.f's storage discipline for bytes:
\ copy-on-grow with the release LAST after a successful install (a failed grow
\ throws before install, leaving the old mapping owned and intact), and
\ capacity-as-ownership so a touch of a disposed buffer throws E-BUF-STATE instead
\ of dereferencing freed storage. Growth doubles with a checked cell-overflow
\ clamp; storage flows through MEM:ALLOC-SPAN / MEM:FREE-SPAN.
\
\ THE HEADER IS THE SPAN, KEPT IN THE THREE CELLS THE CALLER ALREADY ALLOTS.
\ `data` plus `cap` IS a span - a base and a reach in bytes - and `len` is the
\ active prefix, so the record design is not "store a span value plus a length"
\ but "the base lives in the header's POINTER FIELD (DATA-FIELD's `ptr-field`)
\ and the reach lives in the capacity cell". Storing a minted span by writing its
\ two cells into the header instead would put a base address in a raw cell, which
\ docs/effects.md "Raw storage never holds an address" refuses, and it would
\ change HDR-BYTES, a published footprint every caller allots. So the header
\ layout is UNCHANGED and BUF-SPAN projects the pair into a span at each use: the
\ capacity cell is the ownership token, a live mapping of exactly that many bytes,
\ which is the fact SPAN:MAKE needs and the one this module already maintains.
\ Every write into the storage - append, replace, the copy into a fresh mapping -
\ then goes through SPAN:U8! / SPAN:COPY and is bounds-checked against the owned
\ reach, so the module has no unchecked byte copy left. DATA-FIELD's cast is the
\ pointer field's, unchanged and still BUF-private.
\
\ Load after lib/errors.f, lib/memory.f and lib/span.f.

require lib/errors.f
require lib/memory.f
require lib/span.f

package BUF
private

\ ---- header layout: [ data:ptr u8 | len:n | cap:n ] ---------------------------
0 constant DATA-OFF                     \ storage pointer (a `ptr u8` cell)
1 cells constant LEN-OFF                 \ active byte length (raw cell)
2 cells constant CAP-OFF                 \ byte capacity / ownership token (raw cell)
3 cells constant HDR-SIZE                 \ header footprint the caller allots

2 constant GROWTH                         \ capacity doubling factor
MEM-MAX-N constant MAX-BYTES              \ largest byte extent (one cell); the doubling-overflow ceiling

\ ---- representation projection (BUF's only retype, and it is checked) ----------
\ Reads a validated `byte-len`'s raw cell to store the header capacity/length cell,
\ to size the allocation, and to drive the byte-copy/pointer arithmetic - all of
\ which still consume a bare `n`. BUF-private, no public export; there is no public
\ inverse, so a length/offset role cannot round-trip through a raw cell by accident.
\ Deleting the projection outright waits on habu-build-exact-modular-44f4c2dc.
CAST: BLEN>N ( NUM:byte-len -- n )

\ ---- n -> byte-len: the read/derived cell is provably nonnegative (SET-LEN keeps
\ len >= 0, INIT keeps cap > 0), so the refusal arms are unreachable invariants
\ (E-BUF-BOUNDS; mirrors VEC's OK-ITEM-COUNT / MEM's E-MEM-TOTALITY discipline).
: OK-BLEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF          overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF     bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;
: N>BLEN ( n -- NUM:byte-len )  NUM:BYTE-LEN OK-BLEN ;

\ ---- header field access ------------------------------------------------------
: DATA-FIELD ( ptr a -- ptr ptr u8 )  DATA-OFF ptr-field ;
: DATA@ ( ptr a -- ptr u8 )  DATA-FIELD @ ;
: DATA! ( ptr u8 ptr a -- ) {: d:ptr buf:ptr :}  d buf DATA-FIELD ! ;
: LEN-FIELD ( ptr a -- ptr n )  BYTE-VIEW LEN-OFF + CELL-VIEW ;
: CAP-FIELD ( ptr a -- ptr n )  BYTE-VIEW CAP-OFF + CELL-VIEW ;
: LEN-RAW@ ( ptr a -- n )  LEN-FIELD @ ;
: CAP-RAW@ ( ptr a -- n )  CAP-FIELD @ ;
: CAP-RAW! ( n ptr a -- ) {: v:n buf:ptr :}  v buf CAP-FIELD ! ;

\ ---- ownership / liveness (the capacity cell is the token) ---------------------
: CHECK-LIVE ( ptr a -- ) {: buf:ptr :}
   buf CAP-RAW@ 0= if E-BUF-STATE throw then ;
: CHECK-DEAD ( ptr a -- ) {: buf:ptr :}
   buf CAP-RAW@ 0 <> if E-BUF-STATE throw then ;

\ ---- checked length setter: the len <= cap header invariant --------------------
: SET-LEN ( n ptr a -- ) {: len:n buf:ptr :}
   len 0 < if E-BUF-BOUNDS throw then
   len buf CAP-RAW@ > if E-BUF-BOUNDS throw then
   len buf LEN-FIELD ! ;

\ ---- size / capacity checks (raw n) -------------------------------------------
\ A capacity/length arrives as a `byte-len` role, always in [0, MAX-BYTES], so the
\ only reachable magnitude fault is zero capacity. Growth needs are computed as
\ len + delta on raw cells: a cell overflow turns the sum NEGATIVE in two's
\ complement (both operands are < 2^63, so any sum past MAX-BYTES wraps below zero),
\ which CHECK-NEED rejects fail-closed - that is the checked cell-overflow guard.
: CHECK-CAP ( n -- ) {: cap:n :}
   cap 0= if E-BUF-CAPACITY throw then ;
: CHECK-NEED ( n -- ) {: need:n :}
   need 0 < if E-BUF-CAPACITY throw then ;

\ ---- storage: allocate / release exactly `n` bytes through the typed MEM sinks -
: STORAGE-ALLOC ( n -- SPAN:span<u8> )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;
: STORAGE-RELEASE ( SPAN:span<u8> -- )
   MEM:FREE-SPAN ;

\ ---- the header's storage as the span it is ------------------------------------
\ The reach is the capacity cell, which is the ownership token: positive means a
\ live mapping of exactly that many bytes is owned here (INIT-RAW and
\ INSTALL-RESIZE write base and reach together, DISPOSE-RAW clears both), so the
\ extent SPAN:MAKE is told is the extent this module owns.
: BUF-SPAN ( ptr a -- SPAN:span<u8> ) {: buf:ptr :}
   buf DATA@ buf CAP-RAW@ SPAN:MAKE ;

\ ---- copy into the new mapping, install it, then release the prior one. Release
\ is LAST and the caller allocates BEFORE this word runs, so a failed grow (the
\ alloc throws upstream) never reaches here and leaves the old storage owned.
\ The active prefix is taken from the old span and copied into the new one, so
\ both ends of the move are bounds-checked (E-SPAN-RANGE, E-SPAN-CAPACITY).
: INSTALL-RESIZE ( ptr a SPAN:span<u8> -- ) {: buf:ptr d :}
   buf BUF-SPAN {: old :}
   old buf LEN-RAW@ SPAN:TAKE SPAN:$ d SPAN:COPY
   d SPAN:$ {: dp:ptr cap:n :}
   dp buf DATA!
   cap buf CAP-RAW!
   old STORAGE-RELEASE ;

: CHECK-RESIZE-CAP ( ptr a n -- ) {: buf:ptr cap:n :}
   buf CHECK-LIVE
   cap CHECK-CAP
   cap buf LEN-RAW@ < if E-BUF-BOUNDS throw then ;

: RESIZE-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
   buf cap CHECK-RESIZE-CAP
   buf  cap STORAGE-ALLOC  INSTALL-RESIZE ;

\ ---- doubling growth with a checked cell-overflow clamp ------------------------
: GROW-CAP ( ptr a n -- n ) {: buf:ptr need:n :}
   buf CHECK-LIVE
   need CHECK-NEED
   buf CAP-RAW@
   begin dup need < while
      dup MAX-BYTES GROWTH / > if
         drop need
      else
         GROWTH *
      then
   repeat ;

: ENSURE-RAW ( ptr a n -- ) {: buf:ptr need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf  buf need GROW-CAP  RESIZE-RAW ;

: RESERVE-RAW ( ptr a n -- ) {: buf:ptr need:n :}
   buf CHECK-LIVE
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf need RESIZE-RAW ;

: INIT-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
   buf CHECK-DEAD
   cap CHECK-CAP
   cap STORAGE-ALLOC SPAN:$ {: d:ptr got:n :}
   d buf DATA!
   got buf CAP-RAW!
   0 buf SET-LEN ;

\ Clear the entire header before release; dead headers retain no process address.
: DISPOSE-RAW ( ptr a -- ) {: buf:ptr :}
   buf BUF-SPAN {: owned :}
   NULL-PTR buf DATA!
   0 buf CAP-RAW!
   0 buf LEN-FIELD !
   owned SPAN:LEN 0= if exit then
   owned STORAGE-RELEASE ;

: CLEAR-RAW ( ptr a -- ) {: buf:ptr :}
   buf CHECK-LIVE
   0 buf SET-LEN ;

\ The three writers mint the span AFTER the growth step: a grow installs a new
\ mapping, so a span taken before it would name the released one.
: APPEND-BYTE-RAW ( n ptr a -- ) {: v:n buf:ptr :}
   buf  buf LEN-RAW@ 1 +  ENSURE-RAW
   v  buf BUF-SPAN buf LEN-RAW@ SPAN:U8!
   buf LEN-RAW@ 1 + buf SET-LEN ;

: APPEND-SPAN-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf  buf LEN-RAW@ u +  ENSURE-RAW           \ len + u; a cell-overflow need fails closed
   src u  buf BUF-SPAN buf LEN-RAW@ SPAN:SKIP  SPAN:COPY
   buf LEN-RAW@ u + buf SET-LEN ;

: REPLACE-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf CHECK-LIVE
   buf u RESERVE-RAW
   src u buf BUF-SPAN SPAN:COPY
   u buf SET-LEN ;

public

\ ---- header size the caller allots for one buffer ------------------------------
: HDR-BYTES ( -- n )  HDR-SIZE ;

\ ---- lifecycle ----------------------------------------------------------------
: INIT ( ptr a NUM:byte-len -- ) {: buf:ptr cap:NUM:byte-len :}
   buf cap BLEN>N INIT-RAW ;
: DISPOSE ( ptr a -- )  DISPOSE-RAW ;
: CLEAR ( ptr a -- )  CLEAR-RAW ;

\ ---- state readers (fail closed on a disposed / uninitialized buffer) ----------
: LEN@ ( ptr a -- NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf LEN-RAW@ N>BLEN ;
: CAP@ ( ptr a -- NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf CAP-RAW@ N>BLEN ;
: SPAN$ ( ptr a -- ptr u8 NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf DATA@  buf LEN-RAW@ N>BLEN ;

\ ---- capacity management ------------------------------------------------------
: RESERVE ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf n BLEN>N RESERVE-RAW ;

\ ---- append / replace (growth doubles through the checked adapter) ------------
: APPEND-BYTE ( n ptr a -- )  APPEND-BYTE-RAW ;
: APPEND-SPAN ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf APPEND-SPAN-RAW ;
: REPLACE ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf REPLACE-RAW ;

;package
