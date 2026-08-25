\ byte-buffer-test.f - focused tests for the owned growable byte buffer.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/byte-buffer.f lib/byte-buffer-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/byte-buffer.f
require lib/property.f
require test/checker-assert.f

create BUFT-BUF BUF:HDR-BYTES allot
create BUFT-REL-BUF BUF:HDR-BYTES allot     \ dedicated header for release/dispose lifecycle tests
create BUFT-BIG-BUF BUF:HDR-BYTES allot
create BUFT-CHUNK 1024 allot                 \ multi-megabyte fill source
create BUFT-REF 4096 allot                    \ property-test reference bytes

32 constant BUFT-CAP0                          \ shared init capacity (bytes)
2097152 constant BUFT-BIG-N                    \ 2 MiB fill target (2048 * 1024)

\ ---- white-box readers: reopen BUF to project the raw header cells the lifecycle
\ proofs need without the public live checks (mirrors vector-test's CAD-NUM reopen).
package BUF
public
: BUFT-DATA@ ( ptr a -- ptr u8 ) DATA@ ;
: BUFT-CAP-RAW@ ( ptr a -- n ) CAP-RAW@ ;
;package

\ ---- byte-len role builders/readers for scalar assertions ----------------------
package CAD-NUM
public
: BUFT-BL>RAW ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
;package

: BUFT-N>BLEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF          overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF     bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

: BUFT-TLEN ( ptr a -- n )  BUF:LEN@ CAD-NUM:BUFT-BL>RAW ;
: BUFT-TCAP ( ptr a -- n )  BUF:CAP@ CAD-NUM:BUFT-BL>RAW ;

: BUFT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ dispose-then-init the shared header at a raw byte capacity (re-init of a live
\ header rejects, so each test opens from a disposed header).
: BUFT-FRESH ( n -- )  BUFT-BUF BUF:DISPOSE  BUFT-BUF swap BUFT-N>BLEN BUF:INIT ;
: BUFT-REL-INIT ( n -- )  BUFT-REL-BUF BUF:DISPOSE  BUFT-REL-BUF swap BUFT-N>BLEN BUF:INIT ;

\ ---- init / grow: growth preserves the accumulated bytes -----------------------
: BUFT-GROW ( -- )
   2 BUFT-FRESH
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 2 T=
   65 BUFT-BUF BUF:APPEND-BYTE                 \ 'A'
   66 BUFT-BUF BUF:APPEND-BYTE                 \ 'B' fills the 2-byte cap
   67 BUFT-BUF BUF:APPEND-BYTE                 \ 'C' grows (2 -> 4)
   BUFT-BUF BUFT-TLEN 3 T=
   BUFT-BUF BUFT-TCAP 4 T=
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" ABC" T$= ;

\ ---- append span across a growth boundary --------------------------------------
: BUFT-APPEND-SPAN ( -- )
   4 BUFT-FRESH
   s" ab" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   s" cdef" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN    \ 2+4 = 6 > 4: grows to 8
   BUFT-BUF BUFT-TLEN 6 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" abcdef" T$= ;

\ ---- reserve exact grows capacity to exactly the request, preserving bytes ------
: BUFT-RESERVE ( -- )
   4 BUFT-FRESH
   s" xy" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 100 BUFT-N>BLEN BUF:RESERVE            \ exact: cap becomes 100, not doubled
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUFT-TLEN 2 T=
   BUFT-BUF 50 BUFT-N>BLEN BUF:RESERVE            \ 50 <= 100: no-op
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" xy" T$= ;

\ ---- replace swaps the contents, growing capacity if needed --------------------
: BUFT-REPLACE ( -- )
   8 BUFT-FRESH
   s" hi" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   s" replacement" BUFT-N>BLEN BUFT-BUF BUF:REPLACE   \ 11 bytes > 8: grows
   BUFT-BUF BUFT-TLEN 11 T=
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" replacement" T$= ;

\ ---- clear reuses the allocation (length 0, capacity + mapping unchanged) -------
: BUFT-CLEAR ( -- )
   8 BUFT-FRESH
   s" data" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:BUFT-DATA@ {: before:ptr :}
   BUFT-BUF BUF:CLEAR
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:BUFT-DATA@ before = TTRUE            \ same mapping reused
   s" new" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN   \ re-fills from offset 0
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" new" T$= ;

\ ---- release / dispose lifecycle (address reuse proves the mapping was freed) ---
: BUFT-GROW-RELEASES ( -- )                       \ a grow frees the prior mapping
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: old:ptr :}
   BUFT-REL-BUF BUFT-CAP0 2 * BUFT-N>BLEN BUF:RESERVE   \ prior mapping released after copy/install
   BUFT-CAP0 MEM-ALLOC-BYTES drop {: reuse:ptr :}        \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: BUFT-DISPOSE-RELEASES ( -- )                    \ dispose frees the owned mapping
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: old:ptr :}
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-CAP0 MEM-ALLOC-BYTES drop {: reuse:ptr :}
   reuse old = TTRUE ;

: BUFT-DISPOSE-DOUBLE ( -- )                      \ a second dispose is a proved no-op (no throw)
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:BUFT-CAP-RAW@ 0 T= ;              \ stays dead; reaching here proves no throw

: BUFT-DISPOSE-CYCLE-BOUNDED ( -- )               \ init/dispose cycles keep ONE resident mapping
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: base:ptr :}
   BUFT-REL-BUF BUF:DISPOSE
   8 0 ?do
      BUFT-CAP0 BUFT-REL-INIT
      BUFT-REL-BUF BUF:BUFT-DATA@ base = TTRUE         \ every cycle refills the same freed hole
      BUFT-REL-BUF BUF:DISPOSE
   loop ;

\ ---- a failed grow leaves the old buffer valid (no leak, old storage intact) ----
: BUFT-GROW-FAIL-ATOMIC ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   s" keep" BUFT-N>BLEN BUFT-REL-BUF BUF:APPEND-SPAN
   BUFT-REL-BUF BUF:BUFT-DATA@ {: before:ptr :}
   [: BUFT-REL-BUF MEM-MAX-N BUFT-N>BLEN BUF:RESERVE ;] E-MEM-MAP TTHROWSQ   \ mmap of the whole space fails
   BUFT-REL-BUF BUF:BUFT-DATA@ before = TTRUE          \ mapping not swapped out
   BUFT-REL-BUF BUFT-TLEN 4 T=
   BUFT-REL-BUF BUFT-TCAP BUFT-CAP0 T=
   BUFT-REL-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW s" keep" T$= ;

\ ---- use-after-dispose fails closed (E-BUF-STATE) ------------------------------
: BUFT-USE-SPAN ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:SPAN$ 2drop ;
: BUFT-USE-APPEND ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   9 BUFT-REL-BUF BUF:APPEND-BYTE ;
: BUFT-USE-RESERVE ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 64 BUFT-N>BLEN BUF:RESERVE ;
: BUFT-USE-LEN ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:LEN@ drop ;
: BUFT-USE-CLEAR ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:CLEAR ;
: BUFT-INIT-LIVE-REJECTS ( -- )
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-CAP0 BUFT-REL-INIT                          \ header now owns a live mapping
   BUFT-REL-BUF BUFT-CAP0 BUFT-N>BLEN BUF:INIT ;    \ re-init without dispose throws (else old mapping leaks)

\ ---- runtime capacity / cell-overflow refusals ---------------------------------
: BUFT-CAP-ZERO ( -- )    0 BUFT-FRESH ;                  \ zero capacity rejects
: BUFT-APPEND-OVERFLOW ( -- )                              \ len + span overflows one cell
   2 BUFT-FRESH
   65 BUFT-BUF BUF:APPEND-BYTE                             \ len = 1
   s" x" drop MEM-MAX-N BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN ;  \ 1 + MAX-BYTES wraps below zero

\ ---- multi-megabyte fill: 2 MiB via 1024-byte chunked appends -------------------
: BUFT-CHUNK-FILL ( -- )
   1024 0 ?do  i 255 and  BUFT-CHUNK i +  c!  loop ;
: BUFT-BIG-FILL ( -- )
   BUFT-BIG-BUF 16 BUFT-N>BLEN BUF:INIT
   BUFT-CHUNK-FILL
   2048 0 ?do  BUFT-CHUNK 1024 BUFT-N>BLEN BUFT-BIG-BUF BUF:APPEND-SPAN  loop
   BUFT-BIG-BUF BUFT-TLEN BUFT-BIG-N T=
   BUFT-BIG-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW {: data:ptr len:n :}
   len BUFT-BIG-N T=                                                   \ SPAN$ length matches
   data 0 + c@ 0 T=                                                    \ first chunk byte
   data 1000 + c@ 232 T=                                              \ 1000 & 255
   data BUFT-BIG-N 1 - + c@ 255 T=                                    \ last byte = 1023 & 255
   BUFT-BIG-BUF BUF:DISPOSE ;

\ ---- property: random single-byte appends preserve every byte through growth ----
: BUFT-PROP ( -- )
   16 BUFT-FRESH
   7 256 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do                                                  \ 256 random appends
      256 PROP:RND% {: b:n :}
      b  BUFT-REF i +  c!                                             \ shadow into the reference
      b BUFT-BUF BUF:APPEND-BYTE
      i 1 + BUFT-BUF BUFT-TLEN T=                                     \ length tracks appends
      BUFT-BUF BUFT-TCAP  BUFT-BUF BUFT-TLEN  >= TTRUE                \ capacity always covers length
   loop
   BUFT-BUF BUF:SPAN$ CAD-NUM:BUFT-BL>RAW  BUFT-REF PROP:COUNT@  T$= ; \ span equals the reference

: BUFT-RUN ( -- )
   T-RESET
   BUFT-GROW
   BUFT-APPEND-SPAN
   BUFT-RESERVE
   BUFT-REPLACE
   BUFT-CLEAR
   \ ---- release / dispose lifecycle -------------------------------------------
   BUFT-GROW-RELEASES
   BUFT-DISPOSE-RELEASES
   BUFT-DISPOSE-DOUBLE
   BUFT-DISPOSE-CYCLE-BOUNDED
   BUFT-GROW-FAIL-ATOMIC
   [: BUFT-USE-SPAN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-APPEND ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-RESERVE ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-LEN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-CLEAR ;] E-BUF-STATE TTHROWSQ
   [: BUFT-INIT-LIVE-REJECTS ;] E-BUF-STATE TTHROWSQ
   \ ---- capacity / bounds refusals --------------------------------------------
   [: BUFT-CAP-ZERO ;] E-BUF-CAPACITY TTHROWSQ
   [: BUFT-APPEND-OVERFLOW ;] E-BUF-CAPACITY TTHROWSQ
   \ ---- scale + property ------------------------------------------------------
   BUFT-BIG-FILL
   BUFT-PROP
   \ ---- static typed role-swap rejections (0) + resolving positives (-1) -------
   s" BOK-INIT ( ptr a CAD-NUM:byte-len -- ) BUF:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" BOK-SPAN ( ptr u8 CAD-NUM:byte-len ptr a -- ) BUF:APPEND-SPAN" CHECK-QUIET-CANDIDATE! -1 T=
   s" BSWAP-RAW-CAP ( ptr a n -- ) BUF:INIT" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-SPAN ( ptr u8 n ptr a -- ) BUF:APPEND-SPAN" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-RESERVE ( ptr a n -- ) BUF:RESERVE" BUFT-CHECK-REJECTS
   T-REPORT ;

BUFT-RUN
