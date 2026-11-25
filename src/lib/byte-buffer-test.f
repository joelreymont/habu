\ byte-buffer-test.f - focused tests for the owned growable byte buffer.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/memory.f
require src/habu/stack-abi.f
require src/lib/byte-buffer.f
require src/lib/property.f

create BUFT-BUF BUF:HDR-BYTES allot
create BUFT-REL-BUF BUF:HDR-BYTES allot
create BUFT-BIG-BUF BUF:HDR-BYTES allot
create BUFT-LIVE-BUF BUF:HDR-BYTES allot
create BUFT-NEW-BUF BUF:HDR-BYTES allot
create BUFT-CHUNK 1024 allot
create BUFT-REF 4096 allot

32 constant BUFT-CAP0
2097152 constant BUFT-BIG-N
STACK-ABI:PAGE-BYTES constant BUFT-PAGE      \ the smallest capacity BUF maps
100 constant BUFT-SMALL-N
16 constant BUFT-SMALL-CAP
$1000 constant BUFT-PAGE-MIN                 \ the smallest page any target maps
create BUFT-SMALL BUFT-SMALL-N BUF:HDR-BYTES * allot
create BUFT-BASES BUFT-SMALL-N cells allot

CAST: BUFT-BL>RAW ( NUM:byte-len -- n )

: BUFT-TLEN ( ^a -- n )  BUF:LEN@ BUFT-BL>RAW ;
: BUFT-TCAP ( ^a -- n )  BUF:CAP@ BUFT-BL>RAW ;

: BUFT-BLEN ( str -- ^u8 NUM:byte-len ) {: s:str :}
   s STR:DATA s STR:LEN BUF:N>BLEN ;

: BUFT-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Re-init of a live header would leak its block, so each test opens from a
\ disposed header.
: BUFT-FRESH ( n -- )  BUFT-BUF BUF:DISPOSE  BUFT-BUF swap BUF:N>BLEN BUF:INIT ;
: BUFT-REL-INIT ( n -- )  BUFT-REL-BUF BUF:DISPOSE  BUFT-REL-BUF swap BUF:N>BLEN BUF:INIT ;

: BUFT-GROW ( -- )
   2 BUFT-FRESH
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 2 T=
   65 BUFT-BUF BUF:APPEND-BYTE
   66 BUFT-BUF BUF:APPEND-BYTE                 \ 'B' fills the 2-byte cap
   67 BUFT-BUF BUF:APPEND-BYTE                 \ 'C' grows (2 -> 4)
   BUFT-BUF BUFT-TLEN 3 T=
   BUFT-BUF BUFT-TCAP 4 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" ABC" T$= ;

: BUFT-APPEND-SPAN ( -- )
   4 BUFT-FRESH
   s" ab" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   s" cdef" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN    \ 2+4 = 6 > 4: grows to 8
   BUFT-BUF BUFT-TLEN 6 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abcdef" T$= ;

\ A borrowed source can be this buffer's own active prefix or a substring.
\ Growth must keep that source's block until the append has copied its bytes.
: BUFT-APPEND-SELF ( -- )
   64 BUFT-FRESH
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
   BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
   T$=
   8 BUFT-FRESH
   s" abcd" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abcdabcd" T$= ;

: BUFT-APPEND-PART ( -- )
   8 BUFT-FRESH
   s" abcdefgh" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ drop 2 + 3 BUF:N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abcdefghcde" T$= ;

\ Reacquire the active span after growth; stored row offsets remain valid.
: BUFT-CONTENT ( -- )
   8 BUFT-FRESH
   BUFT-BUF BUF:SPAN SPAN:LEN 0 T=
   s" abc" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN 3 0 SPAN:SUB SPAN:LEN 0 T=
   [: BUFT-BUF BUF:SPAN 3 1 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWSQ
   1 2 {: off:n len:n :}
   s" defghijk" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN off len SPAN:SUB SPAN:$ s" bc" T$= ;

: BUFT-RESERVE ( -- )
   4 BUFT-FRESH
   s" xy" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 100 BUF:N>BLEN BUF:RESERVE            \ exact: cap becomes 100, not doubled
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUFT-TLEN 2 T=
   BUFT-BUF 50 BUF:N>BLEN BUF:RESERVE            \ 50 <= 100: no-op
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" xy" T$= ;

: BUFT-RESET ( -- )
   BUFT-BUF BUF:DISPOSE
   BUFT-BUF 16 BUF:N>BLEN BUF:RESET
   BUFT-BUF BUFT-TCAP 16 T=
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF 100 BUF:N>BLEN BUF:RESERVE
   s" xy" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 16 BUF:N>BLEN BUF:RESET
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 100 T=
   s" xy" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 200 BUF:N>BLEN BUF:RESET
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 200 T= ;

\ RESIZE sets the capacity exactly, either way, and keeps the active bytes.
: BUFT-RESIZE ( -- )
   4 BUFT-FRESH
   s" xy" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 100 BUF:N>BLEN BUF:RESIZE
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" xy" T$=
   BUFT-BUF BUF:CLEAR
   BUFT-BUF 16 BUF:N>BLEN BUF:RESIZE
   BUFT-BUF BUFT-TCAP 16 T=
   BUFT-BUF BUFT-TLEN 0 T=
   s" ab" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" ab" T$= ;

: BUFT-ENSURE ( -- )
   8 BUFT-FRESH
   s" abc" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 9 BUF:N>BLEN BUF:ENSURE
   BUFT-BUF BUFT-TCAP 16 T=
   BUFT-BUF BUFT-TLEN 3 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abc" T$= ;

: BUFT-GROW-TAIL ( -- )
   4 BUFT-FRESH
   BUFT-BUF 3 BUF:N>BLEN BUF:GROW {: s :}
   s SPAN:LEN 3 T=
   s SPAN:$ STR:DATA BUFT-BUF BUF:SPAN$ drop = TTRUE
   s" abc" s SPAN:COPY
   BUFT-BUF 2 BUF:N>BLEN BUF:GROW {: t :}       \ 3+2 = 5 > 4: doubles to 8
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUFT-TLEN 5 T=
   t SPAN:LEN 2 T=
   t SPAN:$ STR:DATA BUFT-BUF BUF:SPAN$ drop 3 + = TTRUE
   s" de" t SPAN:COPY
   BUFT-BUF 0 BUF:N>BLEN BUF:GROW SPAN:LEN 0 T=  \ zero keeps the capacity
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abcde" T$= ;

: BUFT-GROW-TAIL-OVERFLOW ( -- )
   2 BUFT-FRESH
   65 BUFT-BUF BUF:APPEND-BYTE
   BUFT-BUF MEM-MAX-N BUF:N>BLEN BUF:GROW SPAN:LEN drop ;

: BUFT-REPLACE ( -- )
   8 BUFT-FRESH
   s" hi" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   s" replacement" BUFT-BLEN BUFT-BUF BUF:REPLACE
   BUFT-BUF BUFT-TLEN 11 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" replacement" T$= ;

: BUFT-CLEAR ( -- )
   8 BUFT-FRESH
   s" data" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ drop {: before:^ :}
   BUFT-BUF BUF:CLEAR
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ drop before = TTRUE
   s" new" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" new" T$= ;

\ CLEAR leaves the longer text's tail past the length; WIPE zeroes it too.
: BUFT-WIPE ( -- )
   8 BUFT-FRESH
   s" a longer secret" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:CLEAR
   s" short" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUFT-TCAP {: cap:n :}
   BUFT-BUF BUF:WIPE
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP cap T=
   BUFT-BUF BUF:SPAN$ drop {: p:^ :}
   cap 0 ?do p i + c@ 0 T= loop
   BUFT-BUF BUF:DISPOSE
   BUFT-BUF BUF:WIPE
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 0 T= ;

: BUFT-LEN-STORE ( -- )
   8 BUFT-FRESH
   s" data" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ drop {: before:^ :}
   2 BUF:N>BLEN BUFT-BUF BUF:LEN!
   BUFT-BUF BUFT-TLEN 2 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW {: data:^ len:n :}
   data before = TTRUE
   data len >str s" da" T$=
   s" X" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" daX" T$=
   8 BUF:N>BLEN BUFT-BUF BUF:LEN!
   BUFT-BUF BUFT-TLEN 8 T=
   BUFT-BUF BUFT-TCAP 8 T= ;

: BUFT-APPEND-REPLACED ( -- )
   4 BUFT-FRESH
   s" pre" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   s" ab-cd-ef" s" -" s" +++" BUFT-BUF BUF:APPEND-REPLACED 2 T=      \ grows across the pieces
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" preab+++cd+++ef" T$=
   8 BUFT-FRESH
   s" xx-yy-" s" -" s" " BUFT-BUF BUF:APPEND-REPLACED 2 T=          \ an empty new removes every old
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" xxyy" T$=
   BUFT-BUF BUF:CLEAR
   s" --a--" s" --" s" x" BUFT-BUF BUF:APPEND-REPLACED 2 T=         \ old at both ends
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" xax" T$=
   BUFT-BUF BUF:CLEAR
   s" aaa" s" aa" s" b" BUFT-BUF BUF:APPEND-REPLACED 1 T=           \ non-overlapping, as STR:COUNT-SUB counts
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" ba" T$=
   BUFT-BUF BUF:CLEAR
   s" abc" s" z" s" q" BUFT-BUF BUF:APPEND-REPLACED 0 T=            \ absent old: the text as it is
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abc" T$=
   BUFT-BUF BUF:CLEAR
   s" abc" s" " s" q" BUFT-BUF BUF:APPEND-REPLACED 0 T=             \ empty old: STR:COUNT-SUB's zero
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abc" T$=
   s" -" s" -" BUFT-BUF BUF:SPAN$ BUFT-BL>RAW drop 1 + 0 >str
   BUFT-BUF BUF:APPEND-REPLACED 1 T=                                \ an empty new inside overlaps nothing
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abc" T$= ;

\ Address reuse proves a mapping was freed.
: BUFT-GROW-RELEASES ( -- )
   BUFT-PAGE BUFT-REL-INIT
   BUFT-REL-BUF BUF:SPAN$ drop {: old:^ :}
   BUFT-REL-BUF BUFT-PAGE 2 * BUF:N>BLEN BUF:RESERVE
   BUFT-PAGE MEM-ALLOC-BYTES drop {: reuse:^ :}        \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: BUFT-DISPOSE-RELEASES ( -- )
   BUFT-PAGE BUFT-REL-INIT
   BUFT-REL-BUF BUF:SPAN$ drop {: old:^ :}
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-PAGE MEM-ALLOC-BYTES drop {: reuse:^ :}
   reuse old = TTRUE ;

: BUFT-DISPOSE-DOUBLE ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:DISPOSE ;

: BUFT-DISPOSE-CYCLE-BOUNDED ( -- )
   BUFT-PAGE BUFT-REL-INIT
   BUFT-REL-BUF BUF:SPAN$ drop {: base:^ :}
   BUFT-REL-BUF BUF:DISPOSE
   8 0 ?do
      BUFT-PAGE BUFT-REL-INIT
      BUFT-REL-BUF BUF:SPAN$ drop base = TTRUE         \ every cycle refills the same freed hole
      BUFT-REL-BUF BUF:DISPOSE
   loop ;

\ LIVE? answers on a header never initialised, a live one and a disposed one.
: BUFT-LIVE ( -- )
   BUFT-LIVE-BUF BUF:LIVE? TFALSE
   BUFT-LIVE-BUF BUFT-CAP0 BUF:N>BLEN BUF:INIT
   BUFT-LIVE-BUF BUF:LIVE? TTRUE
   BUFT-LIVE-BUF BUF:DISPOSE
   BUFT-LIVE-BUF BUF:LIVE? TFALSE ;

: BUFT-APPEND-NEW ( -- )
   s" abc" BUFT-BLEN BUFT-NEW-BUF BUF:APPEND-SPAN
   BUFT-NEW-BUF BUF:SPAN$ BUFT-BL>RAW >str s" abc" T$=
   BUFT-NEW-BUF BUF:DISPOSE ;

: BUFT-GROW-FAIL-ATOMIC ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   s" keep" BUFT-BLEN BUFT-REL-BUF BUF:APPEND-SPAN
   BUFT-REL-BUF BUF:SPAN$ drop {: before:^ :}
   [: BUFT-REL-BUF MEM-MAX-N BUF:N>BLEN BUF:RESERVE ;] E-MEM-MAP TTHROWSQ   \ mmap of the whole space fails
   BUFT-REL-BUF BUF:SPAN$ drop before = TTRUE
   BUFT-REL-BUF BUFT-TLEN 4 T=
   BUFT-REL-BUF BUFT-TCAP BUFT-CAP0 T=
   BUFT-REL-BUF BUF:SPAN$ BUFT-BL>RAW >str s" keep" T$= ;

: BUFT-CAP-ZERO ( -- )    0 BUFT-FRESH ;
: BUFT-APPEND-OVERFLOW ( -- )
   2 BUFT-FRESH
   65 BUFT-BUF BUF:APPEND-BYTE
   s" x" STR:DATA MEM-MAX-N BUF:N>BLEN BUFT-BUF BUF:APPEND-SPAN ;  \ 1 + MAX-BYTES wraps below zero

: BUFT-CHUNK-FILL ( -- )
   1024 0 ?do  i 255 and  BUFT-CHUNK i +  c!  loop ;
: BUFT-BIG-FILL ( -- )
   BUFT-BIG-BUF 16 BUF:N>BLEN BUF:INIT
   BUFT-CHUNK-FILL
   2048 0 ?do  BUFT-CHUNK 1024 BUF:N>BLEN BUFT-BIG-BUF BUF:APPEND-SPAN  loop
   BUFT-BIG-BUF BUFT-TLEN BUFT-BIG-N T=
   BUFT-BIG-BUF BUFT-TCAP BUFT-PAGE > TTRUE                      \ grew from a heap block to a mapping
   BUFT-BIG-BUF BUF:SPAN$ BUFT-BL>RAW {: data:^ len:n :}
   len BUFT-BIG-N T=
   0 len 0 ?do  data i + c@ i 255 and <> if 1 + then  loop  0 T=
   BUFT-BIG-BUF BUF:DISPOSE ;

: BUFT-SMALL-AT ( n -- ^u8 )  BUF:HDR-BYTES * BUFT-SMALL + ;

: BUFT-SMALL-BASE ( n -- n )  BUFT-SMALL-AT BUF:SPAN$ drop NULL-PTR - ;

: BUFT-SMALL-GAP ( n -- n ) {: k:n :}
   k BUFT-SMALL-BASE  k 1 - BUFT-SMALL-BASE  - abs ;

\ A mapping per block puts each at least a page from the last: none share.
: BUFT-SMALL-SHARE ( -- )
   BUFT-SMALL-N 0 ?do  i BUFT-SMALL-AT BUFT-SMALL-CAP BUF:N>BLEN BUF:INIT  loop
   0 BUFT-SMALL-N 1 ?do  i BUFT-SMALL-GAP BUFT-PAGE-MIN < if 1 + then  loop
   BUFT-SMALL-N 2 / >= TTRUE
   BUFT-SMALL-N 0 ?do  i BUFT-SMALL-AT BUF:DISPOSE  loop ;

: BUFT-BASE-AT ( n -- ^n )  cells BUFT-BASES + CELL-VIEW ;

\ Whether no earlier cycle recorded the base cycle k recorded.
: BUFT-BASE-NEW? ( n -- bool ) {: k:n :}
   true  k 0 ?do  i BUFT-BASE-AT @  k BUFT-BASE-AT @  = if drop false then  loop ;

\ The allocator may hand a freed block back in any order, so this bounds the
\ distinct blocks over many cycles; a skipped free gives one per cycle.
: BUFT-SMALL-REUSE ( -- )
   BUFT-SMALL-N 0 ?do
      0 BUFT-SMALL-AT BUFT-CAP0 BUF:N>BLEN BUF:INIT
      0 BUFT-SMALL-BASE  i BUFT-BASE-AT !
      0 BUFT-SMALL-AT BUF:DISPOSE
   loop
   0 BUFT-SMALL-N 0 ?do  i BUFT-BASE-NEW? if 1 + then  loop
   16 <= TTRUE ;

: BUFT-PROP ( -- )
   16 BUFT-FRESH
   7 256 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do
      256 PROP:RND% {: b:n :}
      b  BUFT-REF i +  c!
      b BUFT-BUF BUF:APPEND-BYTE
      i 1 + BUFT-BUF BUFT-TLEN T=
      BUFT-BUF BUFT-TCAP  BUFT-BUF BUFT-TLEN  >= TTRUE
   loop
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str  BUFT-REF PROP:COUNT@ >str  T$= ;

: BUFT-RUN ( -- )
   T-RESET
   s" shared byte-length conversions admit zero and the full nonnegative range" T-LABEL
   0 BUF:N>BLEN BUF:BLEN>N 0 T=
   MEM-MAX-N BUF:N>BLEN BUF:BLEN>N MEM-MAX-N T=
   s" a byte appended past the capacity doubles it and keeps the bytes" T-LABEL
   BUFT-GROW
   s" a span appended past the capacity grows it to fit" T-LABEL
   BUFT-APPEND-SPAN
   s" the buffer's own content appends to itself across growth" T-LABEL
   BUFT-APPEND-SELF
   s" part of the buffer's own content appends to itself" T-LABEL
   BUFT-APPEND-PART
   s" a range past the length is rejected, and offsets saved before growth stay valid" T-LABEL
   BUFT-CONTENT
   s" reserve sets the capacity exactly and never shrinks it" T-LABEL
   BUFT-RESERVE
   s" reset empties the buffer and only ever raises its capacity" T-LABEL
   BUFT-RESET
   s" resize sets the capacity exactly either way and keeps the bytes" T-LABEL
   BUFT-RESIZE
   s" resize to zero capacity is rejected" T-LABEL
   [: BUFT-BUF 0 BUF:N>BLEN BUF:RESIZE ;] E-BUF-CAPACITY TTHROWSQ
   s" ensure past the capacity doubles it and keeps the bytes" T-LABEL
   BUFT-ENSURE
   s" grow hands back the new tail, doubling the capacity when it must" T-LABEL
   BUFT-GROW-TAIL
   s" replace swaps the whole content for a longer text" T-LABEL
   BUFT-REPLACE
   s" clear empties the buffer and keeps its capacity and block" T-LABEL
   BUFT-CLEAR
   s" wipe zeroes the whole capacity, and a disposed buffer wipes as empty" T-LABEL
   BUFT-WIPE
   s" storing a length keeps the bytes under it, and the full capacity is allowed" T-LABEL
   BUFT-LEN-STORE
   s" append-replaced substitutes each old piece and counts the substitutions" T-LABEL
   BUFT-APPEND-REPLACED
   s" growth frees the old mapping" T-LABEL
   BUFT-GROW-RELEASES
   s" dispose frees the mapping" T-LABEL
   BUFT-DISPOSE-RELEASES
   s" a second dispose is harmless" T-LABEL
   BUFT-DISPOSE-DOUBLE
   s" repeated init and dispose reuse one mapping" T-LABEL
   BUFT-DISPOSE-CYCLE-BOUNDED
   s" small buffers lie within a page of each other" T-LABEL
   BUFT-SMALL-SHARE
   s" a disposed small buffer's block is reused" T-LABEL
   BUFT-SMALL-REUSE
   s" LIVE? answers on a fresh, live and disposed header and never throws" T-LABEL
   [: BUFT-LIVE ;] catch 0 T=
   s" a span appended to a header never initialised grows it to fit" T-LABEL
   BUFT-APPEND-NEW
   s" a failed growth leaves the buffer as it was" T-LABEL
   BUFT-GROW-FAIL-ATOMIC
   s" init with zero capacity is rejected" T-LABEL
   [: BUFT-CAP-ZERO ;] E-BUF-CAPACITY TTHROWSQ
   s" an append whose length would wrap is rejected" T-LABEL
   [: BUFT-APPEND-OVERFLOW ;] E-BUF-CAPACITY TTHROWSQ
   s" a grow whose length would wrap is rejected" T-LABEL
   [: BUFT-GROW-TAIL-OVERFLOW ;] E-BUF-CAPACITY TTHROWSQ
   s" a rejected grow leaves the length where it was" T-LABEL
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" A" T$=
   BUFT-REL-BUF BUF:DISPOSE
   s" two megabytes appended in chunks grow from a heap block to a mapping and keep every byte" T-LABEL
   BUFT-BIG-FILL
   s" random bytes appended one at a time keep their order" T-LABEL
   BUFT-PROP
   s" init and append-span accept a typed byte length" T-LABEL
   s" BOK-INIT ( ^a NUM:byte-len -- ) BUF:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" BOK-SPAN ( ^u8 NUM:byte-len ^a -- ) BUF:APPEND-SPAN" CHECK-QUIET-CANDIDATE! -1 T=
   s" init, append-span, reserve and ensure reject a raw cell for a byte length" T-LABEL
   s" BSWAP-RAW-CAP ( ^a n -- ) BUF:INIT" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-SPAN ( ^u8 n ^a -- ) BUF:APPEND-SPAN" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-RESERVE ( ^a n -- ) BUF:RESERVE" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-ENSURE ( ^a n -- ) BUF:ENSURE" BUFT-CHECK-REJECTS
   s" storing a length accepts a typed byte length" T-LABEL
   s" BOK-LEN ( NUM:byte-len ^a -- ) BUF:LEN!" CHECK-QUIET-CANDIDATE! -1 T=
   s" storing a length rejects a raw cell" T-LABEL
   s" BSWAP-RAW-LEN ( n ^a -- ) BUF:LEN!" BUFT-CHECK-REJECTS
   T-REPORT ;

BUFT-RUN

false [if] \ off until dot habu-add-the-checks-a49d054d
: BUFT-REPLACED-NEG ( n n n -- )
   {: tu:n ou:n nu:n :}
   s" a-b" STR:DATA tu >str  s" -" STR:DATA ou >str  s" +" STR:DATA nu >str
   BUFT-BUF BUF:APPEND-REPLACED drop ;

: BUFT-REPLACED-NEGATIVE ( -- )
   4 BUFT-FRESH
   s" a-b" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   [: -1 1 1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: 3 -1 1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: 3 1 -1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" a-b" T$=
   BUFT-BUF BUF:DISPOSE ;

T-RESET BUFT-REPLACED-NEGATIVE T-REPORT
[then]

false [if] \ off until dot habu-add-the-checks-a49d054d
: BUFT-USE-SPAN ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:SPAN$ 2drop ;
: BUFT-USE-CONTENT ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:SPAN SPAN:LEN drop ;
: BUFT-USE-APPEND ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   9 BUFT-REL-BUF BUF:APPEND-BYTE ;
: BUFT-USE-RESERVE ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 64 BUF:N>BLEN BUF:RESERVE ;
: BUFT-USE-ENSURE-ZERO ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 0 BUF:N>BLEN BUF:ENSURE ;
: BUFT-USE-GROW ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 0 BUF:N>BLEN BUF:GROW SPAN:LEN drop ;
: BUFT-USE-LEN ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:LEN@ drop ;
: BUFT-USE-LEN-STORE ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   0 BUF:N>BLEN BUFT-REL-BUF BUF:LEN! ;
: BUFT-USE-CLEAR ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:CLEAR ;
: BUFT-USE-REPLACED ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   s" a-b" s" -" s" +" BUFT-REL-BUF BUF:APPEND-REPLACED drop ;
: BUFT-INIT-LIVE-REJECTS ( -- )
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUFT-CAP0 BUF:N>BLEN BUF:INIT ;    \ re-init without dispose throws (else old block leaks)

: BUFT-LEN-ABOVE-CAP ( -- )
   8 BUFT-FRESH
   s" data" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   9 BUF:N>BLEN BUFT-BUF BUF:LEN! ;

\ An operand inside the buffer's block would dangle once an append grew it.
: BUFT-REPLACED-SELF ( -- )
   4 BUFT-FRESH
   s" a-b" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" -" s" +" BUFT-BUF BUF:APPEND-REPLACED drop ;

\ So would one that starts below the block and runs into it.
: BUFT-REPLACED-STRADDLE ( -- )
   4 BUFT-FRESH
   s" a-b" BUFT-BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW drop 1 - 2 >str s" -" s" +"
   BUFT-BUF BUF:APPEND-REPLACED drop ;

: BUFT-CHECKS-RUN ( -- )
   T-RESET
   [: -1 BUF:N>BLEN drop ;] E-BUF-BOUNDS TTHROWSQ
   [: BUFT-USE-SPAN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-CONTENT ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-APPEND ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-RESERVE ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-ENSURE-ZERO ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-GROW ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-LEN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-LEN-STORE ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-CLEAR ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-REPLACED ;] E-BUF-STATE TTHROWSQ
   [: BUFT-INIT-LIVE-REJECTS ;] E-BUF-STATE TTHROWSQ
   BUFT-REL-BUF BUF:DISPOSE                                          \ the rejected re-init left the buffer owned
   [: BUFT-REPLACED-SELF ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" a-b" T$=                        \ rejected before a byte moved
   [: BUFT-REPLACED-STRADDLE ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" a-b" T$=
   [: BUFT-LEN-ABOVE-CAP ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW >str s" data" T$=                       \ rejected before the length moved
   T-REPORT ;

BUFT-CHECKS-RUN
[then]
