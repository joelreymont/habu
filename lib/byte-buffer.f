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
\ clamp; storage flows through MEM:ALLOC-BYTES / MEM:RELEASE-BYTES.
\
\ Load after lib/errors.f and lib/memory.f.

require lib/errors.f
require lib/memory.f

package BUF
private

\ ---- header layout: [ data:ptr u8 | len:n | cap:n ] ---------------------------
0 constant DATA-OFF                     \ storage pointer (a `ptr u8` cell)
1 cells constant LEN-OFF                 \ active byte length (raw cell)
2 cells constant CAP-OFF                 \ byte capacity / ownership token (raw cell)
3 cells constant HDR-SIZE                 \ header footprint the caller allots

2 constant GROWTH                         \ capacity doubling factor
MEM-MAX-N constant MAX-BYTES              \ largest byte extent (one cell); the doubling-overflow ceiling

\ ---- audited representation projection (the ONLY unchecked word in BUF) --------
\ Reads a validated `byte-len`'s raw cell to store the header capacity/length cell,
\ to size the allocation, and to drive the byte-copy/pointer arithmetic - all of
\ which still consume a bare `n`. BUF-private, no public export; there is no public
\ inverse, so a length/offset role cannot round-trip through a raw cell by accident.
TRUSTED: BLEN>N ( CAD-NUM:byte-len -- n ) ;

\ ---- n -> byte-len: the read/derived cell is provably nonnegative (SET-LEN keeps
\ len >= 0, INIT keeps cap > 0), so the refusal arms are unreachable invariants
\ (E-BUF-BOUNDS; mirrors VEC's OK-ITEM-COUNT / MEM's E-MEM-TOTALITY discipline).
: OK-BLEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF          overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF     bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;
: N>BLEN ( n -- CAD-NUM:byte-len )  CAD-NUM:BYTE-LEN OK-BLEN ;

\ ---- header field access ------------------------------------------------------
: DATA-FIELD ( ptr a -- ptr ptr u8 )  DATA-OFF ptr-field ;
: DATA@ ( ptr a -- ptr u8 )  DATA-FIELD @ ;
: DATA! ( ptr u8 ptr a -- ) {: d:ptr buf:ptr :}  d buf DATA-FIELD ! ;
: LEN-FIELD ( ptr a -- ptr a )  LEN-OFF + ;
: CAP-FIELD ( ptr a -- ptr a )  CAP-OFF + ;
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
: STORAGE-ALLOC ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;
: STORAGE-RELEASE ( ptr u8 n -- ) {: d:ptr cap:n :}
   d  cap MEM:BYTES-ALLOC-LEN  MEM:RELEASE-BYTES ;

: COPY ( ptr u8 ptr u8 n -- ) {: src:ptr dst:ptr u:n :}
   src dst u >LEN BYTE-COPY-LEN ;

\ ---- copy into the new mapping, install it, then release the prior one. Release
\ is LAST and the caller allocates BEFORE this word runs, so a failed grow (the
\ alloc throws upstream) never reaches here and leaves the old storage owned.
: INSTALL-RESIZE ( ptr a n ptr u8 -- ) {: buf:ptr cap:n d:ptr :}
   buf DATA@ {: old:ptr :}
   buf CAP-RAW@ {: oldcap:n :}
   old d buf LEN-RAW@ COPY
   d buf DATA!
   cap buf CAP-RAW!
   old oldcap STORAGE-RELEASE ;

: CHECK-RESIZE-CAP ( ptr a n -- ) {: buf:ptr cap:n :}
   buf CHECK-LIVE
   cap CHECK-CAP
   cap buf LEN-RAW@ < if E-BUF-BOUNDS throw then ;

: RESIZE-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
   buf cap CHECK-RESIZE-CAP
   buf cap  cap STORAGE-ALLOC  INSTALL-RESIZE ;

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
   cap STORAGE-ALLOC buf DATA!
   cap buf CAP-RAW!
   0 buf SET-LEN ;

\ ---- consume-on-release DISPOSE: void the ownership cell BEFORE the release, so a
\ second dispose observes cap==0 and returns, and a release that throws cannot
\ double-free on retry. A dead header (cap==0) is the proved no-op.
: DISPOSE-RAW ( ptr a -- ) {: buf:ptr :}
   buf CAP-RAW@ {: cap:n :}
   cap 0= if exit then
   buf DATA@ {: d:ptr :}
   0 buf CAP-RAW!
   0 buf LEN-FIELD !
   d cap STORAGE-RELEASE ;

: CLEAR-RAW ( ptr a -- ) {: buf:ptr :}
   buf CHECK-LIVE
   0 buf SET-LEN ;

: APPEND-BYTE-RAW ( n ptr a -- ) {: v:n buf:ptr :}
   buf  buf LEN-RAW@ 1 +  ENSURE-RAW
   v  buf DATA@ buf LEN-RAW@ +  c!
   buf LEN-RAW@ 1 + buf SET-LEN ;

: APPEND-SPAN-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf  buf LEN-RAW@ u +  ENSURE-RAW           \ len + u; a cell-overflow need fails closed
   src  buf DATA@ buf LEN-RAW@ +  u COPY
   buf LEN-RAW@ u + buf SET-LEN ;

: REPLACE-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf CHECK-LIVE
   buf u RESERVE-RAW
   src buf DATA@ u COPY
   u buf SET-LEN ;

public

\ ---- header size the caller allots for one buffer ------------------------------
: HDR-BYTES ( -- n )  HDR-SIZE ;

\ ---- lifecycle ----------------------------------------------------------------
: INIT ( ptr a CAD-NUM:byte-len -- ) {: buf:ptr cap:CAD-NUM:byte-len :}
   buf cap BLEN>N INIT-RAW ;
: DISPOSE ( ptr a -- )  DISPOSE-RAW ;
: CLEAR ( ptr a -- )  CLEAR-RAW ;

\ ---- state readers (fail closed on a disposed / uninitialized buffer) ----------
: LEN@ ( ptr a -- CAD-NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf LEN-RAW@ N>BLEN ;
: CAP@ ( ptr a -- CAD-NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf CAP-RAW@ N>BLEN ;
: SPAN$ ( ptr a -- ptr u8 CAD-NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf DATA@  buf LEN-RAW@ N>BLEN ;

\ ---- capacity management ------------------------------------------------------
: RESERVE ( ptr a CAD-NUM:byte-len -- ) {: buf:ptr n:CAD-NUM:byte-len :}
   buf n BLEN>N RESERVE-RAW ;

\ ---- append / replace (growth doubles through the checked adapter) ------------
: APPEND-BYTE ( n ptr a -- )  APPEND-BYTE-RAW ;
: APPEND-SPAN ( ptr u8 CAD-NUM:byte-len ptr a -- ) {: src:ptr u:CAD-NUM:byte-len buf:ptr :}
   src u BLEN>N buf APPEND-SPAN-RAW ;
: REPLACE ( ptr u8 CAD-NUM:byte-len ptr a -- ) {: src:ptr u:CAD-NUM:byte-len buf:ptr :}
   src u BLEN>N buf REPLACE-RAW ;

;package
