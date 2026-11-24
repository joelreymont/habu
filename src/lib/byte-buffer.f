split-str
\ byte-buffer.f - one package-owned growable byte buffer, package BUF.
\ The caller embeds a three-cell header (BUF:HDR-BYTES); the buffer owns the one
\ block at its storage pointer: a HEAP:ALLOCATOR block below
\ STACK-ABI:PAGE-BYTES, an OS mapping otherwise. The capacity cell is the
\ ownership token: positive, a live block of that many bytes; zero, none (fresh
\ or disposed), and any growth makes one.
\ STORAGE CLASS. CALLER-OWNED: the caller embeds the header; BUF owns the block.

require src/lib/error.f
require src/lib/memory.f
require src/lib/allocator.f
require src/lib/heap.f
require src/habu/stack-abi.f
require src/lib/span.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f

package BUF
private

0 constant DATA-OFF                     \ storage pointer (a `^u8` cell)
1 cells constant LEN-OFF                 \ active byte length (raw cell)
2 cells constant CAP-OFF                 \ byte capacity / ownership token (raw cell)
3 cells constant HDR-SIZE                 \ header footprint the caller allots

2 constant GROWTH                         \ capacity doubling factor
MEM-MAX-N constant MAX-BYTES              \ largest byte extent (one cell); the doubling-overflow ceiling

public
\ Project a length for byte-offset arithmetic, and a raw count back to a length.
CAST: BLEN>N ( NUM:byte-len -- n )
CAST: N>BLEN ( n -- NUM:byte-len )
private

: DATA-FIELD ( ^a -- ^^u8 )  DATA-OFF ptr-field ;
: DATA@ ( ^a -- ^u8 )  DATA-FIELD @ ;
: DATA! ( ^u8 ^a -- ) {: d:^ buf:^ :}  d buf DATA-FIELD ! ;
: LEN-FIELD ( ^a -- ^n )  BYTE-VIEW LEN-OFF + CELL-VIEW ;
: CAP-FIELD ( ^a -- ^n )  BYTE-VIEW CAP-OFF + CELL-VIEW ;
: LEN-RAW@ ( ^a -- n )  LEN-FIELD @ ;
: CAP-RAW@ ( ^a -- n )  CAP-FIELD @ ;
: CAP-RAW! ( n ^a -- ) {: v:n buf:^ :}  v buf CAP-FIELD ! ;

\ len <= cap is the header invariant.
: SET-LEN ( n ^a -- ) {: len:n buf:^ :}
   len buf LEN-FIELD ! ;

\ A capacity or length must lie in [0, MAX-BYTES]; the `byte-len` cast does not
\ check it. A growth need is len + delta on raw cells, so a sum past MAX-BYTES
\ wraps below zero, which CHECK-NEED refuses.
: CHECK-CAP ( n -- ) {: cap:n :}
   cap 0= if E-BUF-CAPACITY throw then ;
: CHECK-NEED ( n -- ) {: need:n :}
   need 0 < if E-BUF-CAPACITY throw then ;

\ A block's capacity names its route, so it is released by the route that made
\ it: ALLOC:FREE takes no size.
: HEAP-BLOCK? ( n -- bool )  STACK-ABI:PAGE-BYTES < ;

: STORAGE-ALLOC ( n -- SPAN:span<u8> ) {: len:n :}
   len HEAP-BLOCK? if HEAP:ALLOCATOR len ALLOC:BYTES SPAN:MAKE exit then
   len MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;
: STORAGE-RELEASE ( SPAN:span<u8> -- ) {: old :}
   old SPAN:$ {: p:^ len:n :}
   len HEAP-BLOCK? if HEAP:ALLOCATOR p ALLOC:FREE exit then
   old MEM:FREE-SPAN ;

\ The header is the span: the base lives in the pointer field and the reach in
\ the capacity cell (INIT-RAW and INSTALL-RESIZE write both, DISPOSE-RAW clears
\ both), so the extent SPAN:MAKE is told is the extent this module owns. Storing
\ a minted span's two cells instead would put a base address in a raw cell
\ (docs/forth.md "Raw storage never holds an address") and change HDR-BYTES, a
\ footprint every caller allots. Every write goes through SPAN:U8! / SPAN:COPY
\ against the owned reach.
: BUF-SPAN ( ^a -- SPAN:span<u8> ) {: buf:^ :}
   buf DATA@ buf CAP-RAW@ SPAN:MAKE ;

\ Install only after all source bytes have been copied; release is last, so a
\ refused grow leaves the old block owned and intact.
: INSTALL-STORAGE ( ^a SPAN:span<u8> SPAN:span<u8> -- )
   {: buf:^ d old :}
   d SPAN:$ {: dp:^ cap:n :}
   dp buf DATA!
   cap buf CAP-RAW!
   old STORAGE-RELEASE ;

\ The caller validates the new capacity before allocating.
: INSTALL-RESIZE ( ^a SPAN:span<u8> -- )
   {: buf:^ d :}
   buf BUF-SPAN {: old :}
   old buf LEN-RAW@ SPAN:TAKE SPAN:$ d SPAN:COPY
   buf d old INSTALL-STORAGE ;

: RESIZE-RAW ( ^a n -- ) {: buf:^ cap:n :}
   cap CHECK-CAP
   buf  cap STORAGE-ALLOC  INSTALL-RESIZE ;

: GROW-CAP ( ^a n -- n ) {: buf:^ need:n :}
   buf CAP-RAW@
   dup 0= if drop need exit then
   begin dup need < while
      dup MAX-BYTES GROWTH / > if
         drop need
      else
         GROWTH *
      then
   repeat ;

: ENSURE-RAW ( ^a n -- ) {: buf:^ need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf  buf need GROW-CAP  RESIZE-RAW ;

: RESERVE-RAW ( ^a n -- ) {: buf:^ need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf need RESIZE-RAW ;

: INIT-RAW ( ^a n -- ) {: buf:^ cap:n :}
   cap CHECK-CAP
   cap STORAGE-ALLOC SPAN:$ {: d:^ got:n :}
   d buf DATA!
   got buf CAP-RAW!
   0 buf SET-LEN ;

\ Clear the entire header before release; dead headers retain no process address.
: DISPOSE-RAW ( ^a -- ) {: buf:^ :}
   buf BUF-SPAN {: owned :}
   NULL-PTR buf DATA!
   0 buf CAP-RAW!
   0 buf LEN-FIELD !
   owned STORAGE-RELEASE ;

: CLEAR-RAW ( ^a -- ) {: buf:^ :}
   0 buf SET-LEN ;

\ Writers project their destination from the block that receives the bytes.
: APPEND-BYTE-RAW ( n ^a -- ) {: v:n buf:^ :}
   buf  buf LEN-RAW@ 1 +  ENSURE-RAW
   v  buf BUF-SPAN buf LEN-RAW@ SPAN:U8!
   buf LEN-RAW@ 1 + buf SET-LEN ;

: APPEND-SPAN-RAW ( str ^a -- )
   {: src:^ u:n buf:^ :}
   buf LEN-RAW@ u + {: need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ > if
      buf need GROW-CAP {: cap:n :}
      cap CHECK-CAP
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

: REPLACE-RAW ( str ^a -- ) {: src:^ u:n buf:^ :}
   buf u RESERVE-RAW
   src u buf BUF-SPAN SPAN:COPY
   u buf SET-LEN ;

\ Appends text[at, hit) and new for the first old at or after at and answers
\ the offset past that old and true; with no old left, appends the rest and
\ answers at and false.
: APPEND-UNTIL-OLD ( str str str ^a n -- n bool )
   {: text:^ tu:n old:^ ou:n new:^ nu:n buf:^ at:n :}
   text tu old ou at STR:FIND-SUB-FROM MATCH option
      none OF text at + tu at - buf APPEND-SPAN-RAW at false ENDOF
      some OF IDX>N dup at - text at + swap buf APPEND-SPAN-RAW
              new nu buf APPEND-SPAN-RAW ou + true ENDOF
   ;MATCH ;

public

: HDR-BYTES ( -- n )  HDR-SIZE ;

: INIT ( ^a NUM:byte-len -- ) {: buf:^ cap:NUM:byte-len :}
   buf cap BLEN>N INIT-RAW ;
: DISPOSE ( ^a -- )  DISPOSE-RAW ;
: CLEAR ( ^a -- )  CLEAR-RAW ;
\ Zeroes the bytes past the length too, for secrets a later grow or dispose would
\ release unzeroed; a freed heap block's bytes stay in the process until reused.
: WIPE ( ^a -- ) {: buf:^ :}
   0 buf BUF-SPAN SPAN:FILL
   buf CLEAR-RAW ;

: LEN@ ( ^a -- NUM:byte-len )  LEN-RAW@ N>BLEN ;
: CAP@ ( ^a -- NUM:byte-len )  CAP-RAW@ N>BLEN ;
\ Any length from zero through the capacity. The capacity stays, and bytes a
\ longer length takes in are unspecified until written.
: LEN! ( NUM:byte-len ^a -- ) {: n:NUM:byte-len buf:^ :}
   n BLEN>N buf SET-LEN ;
: LIVE? ( ^a -- bool )  CAP-RAW@ 0<> ;
\ SPAN and SPAN$ borrow exactly the active prefix until the next mutation or
\ disposal; the span enforces neither that lifetime nor read-only use, and the
\ borrower never frees it.
: SPAN ( ^a -- SPAN:span<u8> )
   {: buf:^ :}
   buf BUF-SPAN buf LEN-RAW@ SPAN:TAKE ;
: SPAN$ ( ^a -- ^u8 NUM:byte-len ) {: buf:^ :}
   buf DATA@  buf LEN-RAW@ N>BLEN ;

\ RESERVE grows to exactly the requested capacity, ENSURE and GROW by checked
\ doubling; all keep the active bytes. GROW answers a writable span over the n
\ new bytes, unspecified until written and valid until the next mutation or
\ disposal; a length past one cell is E-BUF-CAPACITY before the length moves.
: RESERVE ( ^a NUM:byte-len -- ) {: buf:^ n:NUM:byte-len :}
   buf n BLEN>N RESERVE-RAW ;
: ENSURE ( ^a NUM:byte-len -- ) {: buf:^ n:NUM:byte-len :}
   buf n BLEN>N ENSURE-RAW ;
: GROW ( ^a NUM:byte-len -- SPAN:span<u8> ) {: buf:^ n:NUM:byte-len :}
   buf LEN-RAW@ n BLEN>N {: at:n u:n :}
   buf at u + ENSURE-RAW
   at u + buf SET-LEN
   buf BUF-SPAN at u SPAN:SUB ;

\ RESIZE makes the capacity exactly n, at least the length, larger or smaller,
\ and keeps the active bytes; zero is E-BUF-CAPACITY. The new block is made and
\ filled before the old one is released, so a refused allocation throws with the
\ buffer as it was.
: RESIZE ( ^a NUM:byte-len -- ) {: buf:^ n:NUM:byte-len :}
   buf n BLEN>N RESIZE-RAW ;

\ n zero on a dead header is E-BUF-CAPACITY, as INIT; a refused growth leaves a
\ live one empty at its old capacity.
: RESET ( ^a NUM:byte-len -- ) {: buf:^ n:NUM:byte-len :}
   buf LIVE? 0= if buf n INIT exit then
   buf CLEAR  buf n RESERVE ;

\ Appends keep saved offsets valid; CLEAR and REPLACE do not. APPEND-SPAN
\ accepts input borrowed from the buffer's own bytes: growth copies the input
\ before releasing the old block.
: APPEND-BYTE ( n ^a -- )  APPEND-BYTE-RAW ;
: APPEND-SPAN ( ^u8 NUM:byte-len ^a -- ) {: src:^ u:NUM:byte-len buf:^ :}
   src u BLEN>N buf APPEND-SPAN-RAW ;
: REPLACE ( ^u8 NUM:byte-len ^a -- ) {: src:^ u:NUM:byte-len buf:^ :}
   src u BLEN>N buf REPLACE-RAW ;

\ The text with every non-overlapping old replaced by new, appended; answers the
\ count STR:COUNT-SUB answers. No operand may overlap the storage. An empty old is
\ its zero occurrences: STR:FIND-SUB-FROM would answer every offset.
: APPEND-REPLACED ( str str str ^a -- n )
   {: text:^ tu:n old:^ ou:n new:^ nu:n buf:^ :}
   ou 0= if text tu buf APPEND-SPAN-RAW 0 exit then
   0 0 begin >r text tu old ou new nu buf r> APPEND-UNTIL-OLD while swap 1+ swap repeat drop ;

;package
