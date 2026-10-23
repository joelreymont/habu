\ byte-buffer.f - one package-owned growable byte buffer.
\
\ A BUF header is three cells the caller embeds ( BUF:HDR-BYTES ): a `ptr u8`
\ storage pointer, an active byte length, and a byte capacity. The buffer owns the
\ ONE OS mapping at the storage pointer; the capacity cell is the ownership token
\ (positive = a live mapping of that many bytes is owned, zero = no mapping owned,
\ fresh or disposed). It mirrors src/lib/vector.f's storage discipline for bytes:
\ copy-on-grow with validation before allocation and release LAST after a
\ successful install (a refused grow leaves the old mapping owned and intact), and
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
\ docs/forth.md "Raw storage never holds an address" refuses, and it would
\ change HDR-BYTES, a published footprint every caller allots. So the header
\ layout is UNCHANGED and BUF-SPAN projects the pair into a span at each use: the
\ capacity cell is the ownership token, a live mapping of exactly that many bytes,
\ which is the fact SPAN:MAKE needs and the one this module already maintains.
\ Every write into the storage - append, replace, the copy into a fresh mapping -
\ then goes through SPAN:U8! / SPAN:COPY and is bounds-checked against the owned
\ reach, so the module has no unchecked byte copy left. DATA-FIELD's cast is the
\ pointer field's, unchanged and still BUF-private.
\
\ N>BLEN throws E-BUF-BOUNDS on a negative count. RESERVE grows to exactly the
\ requested capacity and ENSURE by checked doubling; both keep the active bytes.
\ SPAN, and the pointer SPAN$ answers, borrow exactly the active prefix until
\ the next mutation or disposal: the span enforces neither that lifetime nor
\ read-only use, and the borrower never frees it. Appends keep saved offsets
\ valid; CLEAR and REPLACE do not. APPEND-SPAN accepts input borrowed from the
\ buffer's own bytes, because growth copies the input before releasing the old
\ mapping. APPEND-REPLACED appends the text with each non-overlapping
\ occurrence of old (as COUNT-SUB counts them) replaced by new and answers the
\ count; a dead buffer is E-BUF-STATE, a negative length E-STR-BOUNDS and an
\ operand overlapping the buffer's storage E-BUF-BOUNDS, each before a byte
\ moves.
\
\ STORAGE CLASS. CALLER-OWNED: the caller embeds the header; the buffer owns
\ its mapping.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/adt/option.f

package BUF
private

\ ---- header layout: [ data:ptr u8 | len:n | cap:n ] ---------------------------
0 constant DATA-OFF                     \ storage pointer (a `ptr u8` cell)
1 cells constant LEN-OFF                 \ active byte length (raw cell)
2 cells constant CAP-OFF                 \ byte capacity / ownership token (raw cell)
3 cells constant HDR-SIZE                 \ header footprint the caller allots

2 constant GROWTH                         \ capacity doubling factor
MEM-MAX-N constant MAX-BYTES              \ largest byte extent (one cell); the doubling-overflow ceiling

\ Shared byte-length conversion: callers use the same refusal as the buffer.
: OK-BLEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF          overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF     bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

public
\ Project a validated length for byte-offset arithmetic; the inverse validates
\ the raw count and throws E-BUF-BOUNDS on a negative value. Zero is a length.
CAST: BLEN>N ( NUM:byte-len -- n )
: N>BLEN ( n -- NUM:byte-len )  NUM:BYTE-LEN OK-BLEN ;
private

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

\ Install only after all source bytes have been copied; release is last.
: INSTALL-STORAGE ( ptr a SPAN:span<u8> SPAN:span<u8> -- )
   {: buf:ptr d old :}
   d SPAN:$ {: dp:ptr cap:n :}
   dp buf DATA!
   cap buf CAP-RAW!
   old STORAGE-RELEASE ;

\ The caller validates the old prefix and new capacity before allocating.
: INSTALL-RESIZE ( ptr a SPAN:span<u8> -- )
   {: buf:ptr d :}
   buf BUF-SPAN {: old :}
   old buf LEN-RAW@ SPAN:TAKE SPAN:$ d SPAN:COPY
   buf d old INSTALL-STORAGE ;

: CHECK-RESIZE-CAP ( ptr a n -- ) {: buf:ptr cap:n :}
   buf CHECK-LIVE
   cap CHECK-CAP
   cap buf LEN-RAW@ < if E-BUF-BOUNDS throw then
   buf BUF-SPAN buf LEN-RAW@ SPAN:TAKE SPAN:$ 2drop ;

: RESIZE-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
   buf cap CHECK-RESIZE-CAP
   buf  cap STORAGE-ALLOC  INSTALL-RESIZE ;

\ ---- doubling growth with a checked cell-overflow clamp ------------------------
: GROW-CAP ( ptr a n -- n ) {: buf:ptr need:n :}
   buf CAP-RAW@
   begin dup need < while
      dup MAX-BYTES GROWTH / > if
         drop need
      else
         GROWTH *
      then
   repeat ;

: ENSURE-RAW ( ptr a n -- ) {: buf:ptr need:n :}
   buf CHECK-LIVE
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

\ Writers project their destination from the mapping that receives the bytes.
: APPEND-BYTE-RAW ( n ptr a -- ) {: v:n buf:ptr :}
   buf  buf LEN-RAW@ 1 +  ENSURE-RAW
   v  buf BUF-SPAN buf LEN-RAW@ SPAN:U8!
   buf LEN-RAW@ 1 + buf SET-LEN ;

: APPEND-SPAN-RAW ( ptr u8 n ptr a -- )
   {: src:ptr u:n buf:ptr :}
   buf CHECK-LIVE
   buf LEN-RAW@ u + {: need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ > if
      buf need GROW-CAP {: cap:n :}
      buf cap CHECK-RESIZE-CAP
      buf BUF-SPAN {: old :}
      cap STORAGE-ALLOC {: d :}
      old buf LEN-RAW@ SPAN:TAKE SPAN:$ d SPAN:COPY
      \ src may borrow old storage. Copy it before install releases old.
      src u d buf LEN-RAW@ SPAN:SKIP SPAN:COPY
      buf d old INSTALL-STORAGE
   else
      src u buf BUF-SPAN buf LEN-RAW@ SPAN:SKIP SPAN:COPY
   then
   need buf SET-LEN ;

: REPLACE-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf CHECK-LIVE
   buf u RESERVE-RAW
   src u buf BUF-SPAN SPAN:COPY
   u buf SET-LEN ;

\ ---- append with substitution ------------------------------------------------
\ An operand overlapping the owned mapping would dangle once an append grew it.
: BORROWED? ( ptr u8 n ptr a -- bool )
   {: p:ptr u:n buf:ptr :}
   p buf DATA@ - {: off:n :}
   u 0 > off buf CAP-RAW@ < and off u + 0 > and ;

\ Appends text[at, hit) and new for the first old at or after at and answers
\ the offset past that old and true; with no old left, appends the rest and
\ answers at and false.
: APPEND-UNTIL-OLD ( ptr u8 n ptr u8 n ptr u8 n ptr a n -- n bool )
   {: text:ptr tu:n old:ptr ou:n new:ptr nu:n buf:ptr at:n :}
   text tu old ou at FIND-SUB-FROM MATCH option
      none OF text at + tu at - buf APPEND-SPAN-RAW at false ENDOF
      some OF IDX>N dup at - text at + swap buf APPEND-SPAN-RAW
              new nu buf APPEND-SPAN-RAW ou + true ENDOF
   ;MATCH ;

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
\ Borrow the active prefix, not spare capacity. Reacquire after mutation;
\ BUF retains ownership of the mapping, so the borrower must not free it.
: SPAN ( ptr a -- SPAN:span<u8> )
   {: buf:ptr :}
   buf CHECK-LIVE
   buf BUF-SPAN buf LEN-RAW@ SPAN:TAKE ;
: SPAN$ ( ptr a -- ptr u8 NUM:byte-len ) {: buf:ptr :}
   buf CHECK-LIVE  buf DATA@  buf LEN-RAW@ N>BLEN ;

\ ---- capacity management ------------------------------------------------------
: RESERVE ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf n BLEN>N RESERVE-RAW ;
: ENSURE ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf n BLEN>N ENSURE-RAW ;

\ ---- append / replace (growth doubles through the checked adapter) ------------
: APPEND-BYTE ( n ptr a -- )  APPEND-BYTE-RAW ;
: APPEND-SPAN ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf APPEND-SPAN-RAW ;
: REPLACE ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf REPLACE-RAW ;

\ The text with every old replaced by new, appended; the count COUNT-SUB answers.
\ An empty old is its zero occurrences: FIND-SUB-FROM would answer every offset.
: APPEND-REPLACED ( ptr u8 n ptr u8 n ptr u8 n ptr a -- n )
   {: text:ptr tu:n old:ptr ou:n new:ptr nu:n buf:ptr :}
   buf CHECK-LIVE
   tu ou STR-CHECK-LENS nu STR-CHECK-LEN
   text tu buf BORROWED? old ou buf BORROWED? or new nu buf BORROWED? or if E-BUF-BOUNDS throw then
   ou 0= if text tu buf APPEND-SPAN-RAW 0 exit then
   0 0 begin >r text tu old ou new nu buf r> APPEND-UNTIL-OLD while swap 1+ swap repeat drop ;

;package
