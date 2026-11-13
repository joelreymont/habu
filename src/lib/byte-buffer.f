\ byte-buffer.f - one package-owned growable byte buffer, package BUF.
\ The caller embeds a three-cell header (BUF:HDR-BYTES); the buffer owns the one
\ OS mapping at its storage pointer. The capacity cell is the ownership token:
\ positive, a live mapping of that many bytes; zero, none (fresh or disposed).
\ STORAGE CLASS. CALLER-OWNED: the caller embeds the header; BUF owns the mapping.

require src/lib/error.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/adt/option.f

package BUF
private

0 constant DATA-OFF                     \ storage pointer (a `ptr u8` cell)
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

: DATA-FIELD ( ptr a -- ptr ptr u8 )  DATA-OFF ptr-field ;
: DATA@ ( ptr a -- ptr u8 )  DATA-FIELD @ ;
: DATA! ( ptr u8 ptr a -- ) {: d:ptr buf:ptr :}  d buf DATA-FIELD ! ;
: LEN-FIELD ( ptr a -- ptr n )  BYTE-VIEW LEN-OFF + CELL-VIEW ;
: CAP-FIELD ( ptr a -- ptr n )  BYTE-VIEW CAP-OFF + CELL-VIEW ;
: LEN-RAW@ ( ptr a -- n )  LEN-FIELD @ ;
: CAP-RAW@ ( ptr a -- n )  CAP-FIELD @ ;
: CAP-RAW! ( n ptr a -- ) {: v:n buf:ptr :}  v buf CAP-FIELD ! ;

\ len <= cap is the header invariant.
: SET-LEN ( n ptr a -- ) {: len:n buf:ptr :}
   len buf LEN-FIELD ! ;

\ A capacity or length must lie in [0, MAX-BYTES]; the `byte-len` cast does not
\ check it. A growth need is len + delta on raw cells, so a sum past MAX-BYTES
\ wraps below zero, which CHECK-NEED refuses.
: CHECK-CAP ( n -- ) {: cap:n :}
   cap 0= if E-BUF-CAPACITY throw then ;
: CHECK-NEED ( n -- ) {: need:n :}
   need 0 < if E-BUF-CAPACITY throw then ;

: STORAGE-ALLOC ( n -- SPAN:span<u8> )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN ;
: STORAGE-RELEASE ( SPAN:span<u8> -- )
   MEM:FREE-SPAN ;

\ The header is the span: the base lives in the pointer field and the reach in
\ the capacity cell (INIT-RAW and INSTALL-RESIZE write both, DISPOSE-RAW clears
\ both), so the extent SPAN:MAKE is told is the extent this module owns. Storing
\ a minted span's two cells instead would put a base address in a raw cell
\ (docs/forth.md "Raw storage never holds an address") and change HDR-BYTES, a
\ footprint every caller allots. Every write goes through SPAN:U8! / SPAN:COPY
\ against the owned reach.
: BUF-SPAN ( ptr a -- SPAN:span<u8> ) {: buf:ptr :}
   buf DATA@ buf CAP-RAW@ SPAN:MAKE ;

\ Install only after all source bytes have been copied; release is last, so a
\ refused grow leaves the old mapping owned and intact.
: INSTALL-STORAGE ( ptr a SPAN:span<u8> SPAN:span<u8> -- )
   {: buf:ptr d old :}
   d SPAN:$ {: dp:ptr cap:n :}
   dp buf DATA!
   cap buf CAP-RAW!
   old STORAGE-RELEASE ;

\ The caller validates the new capacity before allocating.
: INSTALL-RESIZE ( ptr a SPAN:span<u8> -- )
   {: buf:ptr d :}
   buf BUF-SPAN {: old :}
   old buf LEN-RAW@ SPAN:TAKE SPAN:$ d SPAN:COPY
   buf d old INSTALL-STORAGE ;

: RESIZE-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
   cap CHECK-CAP
   buf  cap STORAGE-ALLOC  INSTALL-RESIZE ;

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
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf  buf need GROW-CAP  RESIZE-RAW ;

: RESERVE-RAW ( ptr a n -- ) {: buf:ptr need:n :}
   need CHECK-NEED
   need buf CAP-RAW@ <= if exit then
   buf need RESIZE-RAW ;

: INIT-RAW ( ptr a n -- ) {: buf:ptr cap:n :}
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
   0 buf SET-LEN ;

\ Writers project their destination from the mapping that receives the bytes.
: APPEND-BYTE-RAW ( n ptr a -- ) {: v:n buf:ptr :}
   buf  buf LEN-RAW@ 1 +  ENSURE-RAW
   v  buf BUF-SPAN buf LEN-RAW@ SPAN:U8!
   buf LEN-RAW@ 1 + buf SET-LEN ;

: APPEND-SPAN-RAW ( ptr u8 n ptr a -- )
   {: src:ptr u:n buf:ptr :}
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

: REPLACE-RAW ( ptr u8 n ptr a -- ) {: src:ptr u:n buf:ptr :}
   buf u RESERVE-RAW
   src u buf BUF-SPAN SPAN:COPY
   u buf SET-LEN ;

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

: HDR-BYTES ( -- n )  HDR-SIZE ;

: INIT ( ptr a NUM:byte-len -- ) {: buf:ptr cap:NUM:byte-len :}
   buf cap BLEN>N INIT-RAW ;
: DISPOSE ( ptr a -- )  DISPOSE-RAW ;
: CLEAR ( ptr a -- )  CLEAR-RAW ;

: LEN@ ( ptr a -- NUM:byte-len )  LEN-RAW@ N>BLEN ;
: CAP@ ( ptr a -- NUM:byte-len )  CAP-RAW@ N>BLEN ;
\ Any length from zero through the capacity. The capacity stays, and bytes a
\ longer length takes in are unspecified until written.
: LEN! ( NUM:byte-len ptr a -- ) {: n:NUM:byte-len buf:ptr :}
   n BLEN>N buf SET-LEN ;
\ The one accessor safe on a fresh or disposed header.
: LIVE? ( ptr a -- bool )  CAP-RAW@ 0<> ;
\ SPAN and SPAN$ borrow exactly the active prefix until the next mutation or
\ disposal; the span enforces neither that lifetime nor read-only use, and the
\ borrower never frees it.
: SPAN ( ptr a -- SPAN:span<u8> )
   {: buf:ptr :}
   buf BUF-SPAN buf LEN-RAW@ SPAN:TAKE ;
: SPAN$ ( ptr a -- ptr u8 NUM:byte-len ) {: buf:ptr :}
   buf DATA@  buf LEN-RAW@ N>BLEN ;

\ RESERVE grows to exactly the requested capacity, ENSURE and GROW by checked
\ doubling; all keep the active bytes. GROW answers a writable span over the n
\ new bytes, unspecified until written and valid until the next mutation or
\ disposal; a length past one cell is E-BUF-CAPACITY before the length moves.
: RESERVE ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf n BLEN>N RESERVE-RAW ;
: ENSURE ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf n BLEN>N ENSURE-RAW ;
: GROW ( ptr a NUM:byte-len -- SPAN:span<u8> ) {: buf:ptr n:NUM:byte-len :}
   buf LEN-RAW@ n BLEN>N {: at:n u:n :}
   buf at u + ENSURE-RAW
   at u + buf SET-LEN
   buf BUF-SPAN at u SPAN:SUB ;

\ n zero on a dead header is E-BUF-CAPACITY, as INIT; a growth the OS refuses
\ leaves a live one empty at its old capacity.
: RESET ( ptr a NUM:byte-len -- ) {: buf:ptr n:NUM:byte-len :}
   buf LIVE? 0= if buf n INIT exit then
   buf CLEAR  buf n RESERVE ;

\ Appends keep saved offsets valid; CLEAR and REPLACE do not. APPEND-SPAN
\ accepts input borrowed from the buffer's own bytes: growth copies the input
\ before releasing the old mapping.
: APPEND-BYTE ( n ptr a -- )  APPEND-BYTE-RAW ;
: APPEND-SPAN ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf APPEND-SPAN-RAW ;
: REPLACE ( ptr u8 NUM:byte-len ptr a -- ) {: src:ptr u:NUM:byte-len buf:ptr :}
   src u BLEN>N buf REPLACE-RAW ;

\ The text with every non-overlapping old replaced by new, appended; answers the
\ count COUNT-SUB answers. No operand may overlap the storage. An empty old is
\ its zero occurrences: FIND-SUB-FROM would answer every offset.
: APPEND-REPLACED ( ptr u8 n ptr u8 n ptr u8 n ptr a -- n )
   {: text:ptr tu:n old:ptr ou:n new:ptr nu:n buf:ptr :}
   ou 0= if text tu buf APPEND-SPAN-RAW 0 exit then
   0 0 begin >r text tu old ou new nu buf r> APPEND-UNTIL-OLD while swap 1+ swap repeat drop ;

;package
