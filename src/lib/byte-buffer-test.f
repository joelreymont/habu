\ byte-buffer-test.f - focused tests for the owned growable byte buffer.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/byte-buffer.f src/lib/byte-buffer-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/property.f

create BUFT-BUF BUF:HDR-BYTES allot
create BUFT-REL-BUF BUF:HDR-BYTES allot
create BUFT-BIG-BUF BUF:HDR-BYTES allot
create BUFT-CHUNK 1024 allot
create BUFT-REF 4096 allot

32 constant BUFT-CAP0
2097152 constant BUFT-BIG-N

\ White-box readers: BUF reopened for the raw header cells, past the public live
\ checks.
package BUF
public
: BUFT-DATA@ ( ptr a -- ptr u8 ) DATA@ ;
: BUFT-CAP-RAW@ ( ptr a -- n ) CAP-RAW@ ;


: BUFT-STALE! ( ptr a -- ) {: buf:ptr :}
   s" stale" drop buf DATA!
   7 buf LEN-FIELD ! ;


: BUFT-LEN-RAW@ ( ptr a -- n ) LEN-FIELD @ ;

\ Writes the length cell WITHOUT SET-LEN's len <= cap guard, to stage the one
\ header corruption the span refuses: an active length past the owned mapping.
: BUFT-LEN-RAW! ( n ptr a -- ) {: len:n buf:ptr :}  len buf LEN-FIELD ! ;
;package

\ Projection out of a cell family needs no ownership (checker.f CAST-OWNER?), so
\ NUM is not reopened.
CAST: BUFT-BL>RAW ( NUM:byte-len -- n )

: BUFT-N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                              negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF          overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF     bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

: BUFT-TLEN ( ptr a -- n )  BUF:LEN@ BUFT-BL>RAW ;
: BUFT-TCAP ( ptr a -- n )  BUF:CAP@ BUFT-BL>RAW ;

: BUFT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ Re-init of a live header is refused, so each test opens from a disposed header.
: BUFT-FRESH ( n -- )  BUFT-BUF BUF:DISPOSE  BUFT-BUF swap BUFT-N>BLEN BUF:INIT ;
: BUFT-REL-INIT ( n -- )  BUFT-REL-BUF BUF:DISPOSE  BUFT-REL-BUF swap BUFT-N>BLEN BUF:INIT ;

: BUFT-GROW ( -- )
   2 BUFT-FRESH
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 2 T=
   65 BUFT-BUF BUF:APPEND-BYTE
   66 BUFT-BUF BUF:APPEND-BYTE                 \ 'B' fills the 2-byte cap
   67 BUFT-BUF BUF:APPEND-BYTE                 \ 'C' grows (2 -> 4)
   BUFT-BUF BUFT-TLEN 3 T=
   BUFT-BUF BUFT-TCAP 4 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" ABC" T$= ;

: BUFT-APPEND-SPAN ( -- )
   4 BUFT-FRESH
   s" ab" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   s" cdef" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN    \ 2+4 = 6 > 4: grows to 8
   BUFT-BUF BUFT-TLEN 6 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abcdef" T$= ;

\ A borrowed source can be this buffer's own active prefix or a substring.
\ Growth must keep that source mapped until the append has copied its bytes.
: BUFT-APPEND-SELF ( -- )
   64 BUFT-FRESH
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
   BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
   T$=
   8 BUFT-FRESH
   s" abcd" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abcdabcd" T$= ;

: BUFT-APPEND-PART ( -- )
   8 BUFT-FRESH
   s" abcdefgh" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ drop 2 + 3 BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abcdefghcde" T$= ;

\ Reacquire the active span after growth; stored row offsets remain valid.
: BUFT-CONTENT ( -- )
   8 BUFT-FRESH
   BUFT-BUF BUF:SPAN SPAN:LEN 0 T=
   s" abc" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN 3 0 SPAN:SUB SPAN:LEN 0 T=
   [: BUFT-BUF BUF:SPAN 3 1 SPAN:SUB SPAN:LEN drop ;] E-SPAN-RANGE TTHROWSQ
   1 2 {: off:n len:n :}
   s" defghijk" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN off len SPAN:SUB SPAN:$ s" bc" T$= ;

: BUFT-RESERVE ( -- )
   4 BUFT-FRESH
   s" xy" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 100 BUFT-N>BLEN BUF:RESERVE            \ exact: cap becomes 100, not doubled
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUFT-TLEN 2 T=
   BUFT-BUF 50 BUFT-N>BLEN BUF:RESERVE            \ 50 <= 100: no-op
   BUFT-BUF BUFT-TCAP 100 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" xy" T$= ;

: BUFT-ENSURE ( -- )
   8 BUFT-FRESH
   s" abc" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF 9 BUFT-N>BLEN BUF:ENSURE
   BUFT-BUF BUFT-TCAP 16 T=
   BUFT-BUF BUFT-TLEN 3 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abc" T$= ;

: BUFT-GROW-TAIL ( -- )
   4 BUFT-FRESH
   BUFT-BUF 3 BUFT-N>BLEN BUF:GROW {: s :}
   s SPAN:LEN 3 T=
   s SPAN:$ drop BUFT-BUF BUF:BUFT-DATA@ = TTRUE
   s" abc" s SPAN:COPY
   BUFT-BUF 2 BUFT-N>BLEN BUF:GROW {: t :}       \ 3+2 = 5 > 4: doubles to 8
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUFT-TLEN 5 T=
   t SPAN:LEN 2 T=
   t SPAN:$ drop BUFT-BUF BUF:BUFT-DATA@ 3 + = TTRUE
   s" de" t SPAN:COPY
   BUFT-BUF 0 BUFT-N>BLEN BUF:GROW SPAN:LEN 0 T=  \ zero keeps the capacity
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abcde" T$= ;

: BUFT-GROW-TAIL-OVERFLOW ( -- )
   2 BUFT-FRESH
   65 BUFT-BUF BUF:APPEND-BYTE
   BUFT-BUF MEM-MAX-N BUFT-N>BLEN BUF:GROW SPAN:LEN drop ;

: BUFT-REPLACE ( -- )
   8 BUFT-FRESH
   s" hi" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   s" replacement" BUFT-N>BLEN BUFT-BUF BUF:REPLACE
   BUFT-BUF BUFT-TLEN 11 T=
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" replacement" T$= ;

: BUFT-CLEAR ( -- )
   8 BUFT-FRESH
   s" data" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:BUFT-DATA@ {: before:ptr :}
   BUFT-BUF BUF:CLEAR
   BUFT-BUF BUFT-TLEN 0 T=
   BUFT-BUF BUFT-TCAP 8 T=
   BUFT-BUF BUF:BUFT-DATA@ before = TTRUE
   s" new" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" new" T$= ;

: BUFT-APPEND-REPLACED ( -- )
   4 BUFT-FRESH
   s" pre" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   s" ab-cd-ef" s" -" s" +++" BUFT-BUF BUF:APPEND-REPLACED 2 T=      \ grows across the pieces
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" preab+++cd+++ef" T$=
   8 BUFT-FRESH
   s" xx-yy-" s" -" s" " BUFT-BUF BUF:APPEND-REPLACED 2 T=          \ an empty new removes every old
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" xxyy" T$=
   BUFT-BUF BUF:CLEAR
   s" --a--" s" --" s" x" BUFT-BUF BUF:APPEND-REPLACED 2 T=         \ old at both ends
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" xax" T$=
   BUFT-BUF BUF:CLEAR
   s" aaa" s" aa" s" b" BUFT-BUF BUF:APPEND-REPLACED 1 T=           \ non-overlapping, as COUNT-SUB counts
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" ba" T$=
   BUFT-BUF BUF:CLEAR
   s" abc" s" z" s" q" BUFT-BUF BUF:APPEND-REPLACED 0 T=            \ absent old: the text as it is
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abc" T$=
   BUFT-BUF BUF:CLEAR
   s" abc" s" " s" q" BUFT-BUF BUF:APPEND-REPLACED 0 T=             \ empty old: COUNT-SUB's zero
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abc" T$=
   s" -" s" -" BUFT-BUF BUF:SPAN$ BUFT-BL>RAW drop 1 + 0
   BUFT-BUF BUF:APPEND-REPLACED 1 T=                                \ an empty new inside overlaps nothing
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" abc" T$= ;

\ An operand inside the buffer's mapping would dangle once an append grew it.
: BUFT-REPLACED-SELF ( -- )
   4 BUFT-FRESH
   s" a-b" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" -" s" +" BUFT-BUF BUF:APPEND-REPLACED drop ;

\ So would one that starts below the mapping and runs into it.
: BUFT-REPLACED-STRADDLE ( -- )
   4 BUFT-FRESH
   s" a-b" BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW drop 1 - 2 s" -" s" +" BUFT-BUF BUF:APPEND-REPLACED drop ;

: BUFT-REPLACED-NEG ( n n n -- )
   {: tu:n ou:n nu:n :}
   s" a-b" drop tu s" -" drop ou s" +" drop nu BUFT-BUF BUF:APPEND-REPLACED drop ;

\ Address reuse proves a mapping was freed.
: BUFT-GROW-RELEASES ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: old:ptr :}
   BUFT-REL-BUF BUFT-CAP0 2 * BUFT-N>BLEN BUF:RESERVE
   BUFT-CAP0 MEM-ALLOC-BYTES drop {: reuse:ptr :}        \ a fresh same-size mapping refills the freed hole
   reuse old = TTRUE ;

: BUFT-DISPOSE-RELEASES ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: old:ptr :}
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-CAP0 MEM-ALLOC-BYTES drop {: reuse:ptr :}
   reuse old = TTRUE ;

: BUFT-DISPOSE-DOUBLE ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:BUFT-CAP-RAW@ 0 T=
   BUFT-REL-BUF BUF:BUFT-LEN-RAW@ 0 T=
   BUFT-REL-BUF BUF:BUFT-DATA@ 0= TTRUE
   BUFT-REL-BUF BUF:BUFT-STALE!
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:BUFT-DATA@ 0= TTRUE
   BUFT-REL-BUF BUF:BUFT-LEN-RAW@ 0 T= ;

: BUFT-DISPOSE-CYCLE-BOUNDED ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUF:BUFT-DATA@ {: base:ptr :}
   BUFT-REL-BUF BUF:DISPOSE
   8 0 ?do
      BUFT-CAP0 BUFT-REL-INIT
      BUFT-REL-BUF BUF:BUFT-DATA@ base = TTRUE         \ every cycle refills the same freed hole
      BUFT-REL-BUF BUF:DISPOSE
   loop ;

: BUFT-GROW-FAIL-ATOMIC ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   s" keep" BUFT-N>BLEN BUFT-REL-BUF BUF:APPEND-SPAN
   BUFT-REL-BUF BUF:BUFT-DATA@ {: before:ptr :}
   [: BUFT-REL-BUF MEM-MAX-N BUFT-N>BLEN BUF:RESERVE ;] E-MEM-MAP TTHROWSQ   \ mmap of the whole space fails
   BUFT-REL-BUF BUF:BUFT-DATA@ before = TTRUE
   BUFT-REL-BUF BUFT-TLEN 4 T=
   BUFT-REL-BUF BUFT-TCAP BUFT-CAP0 T=
   BUFT-REL-BUF BUF:SPAN$ BUFT-BL>RAW s" keep" T$= ;

\ A length cell past the owned capacity must not turn the grow into an
\ over-read: the source is taken from the owned span, so the take refuses at the
\ reach (E-SPAN-RANGE) before a byte moves or a mapping is allocated.
: BUFT-APPEND-MORE ( -- )
   s" more" BUFT-N>BLEN BUFT-REL-BUF BUF:APPEND-SPAN ;

: BUFT-GROW-OVER-READ ( -- )
   BUFT-CAP0 BUFT-REL-INIT
   s" keep" BUFT-N>BLEN BUFT-REL-BUF BUF:APPEND-SPAN
   BUFT-CAP0 2 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: hole :}
   hole MEM:FREE-SPAN
   BUFT-CAP0 1 + BUFT-REL-BUF BUF:BUFT-LEN-RAW!        \ len = 33 past the 32-byte mapping
   [: BUFT-REL-BUF BUF:SPAN SPAN:LEN drop ;] E-SPAN-RANGE TTHROWSQ
   [: BUFT-APPEND-MORE ;] E-SPAN-RANGE TTHROWSQ
   \ A refused grow must leave the free hole available, not leak a new mapping
   \ into it. This is the same address-reuse proof as the release tests above.
   BUFT-CAP0 2 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: reuse :}
   reuse SPAN:$ drop hole SPAN:$ drop = TTRUE
   reuse MEM:FREE-SPAN
   BUFT-REL-BUF BUF:BUFT-DATA@ 4 s" keep" T$=
   BUFT-REL-BUF BUFT-TCAP BUFT-CAP0 T= ;

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
   BUFT-REL-BUF 64 BUFT-N>BLEN BUF:RESERVE ;
: BUFT-USE-ENSURE-ZERO ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 0 BUFT-N>BLEN BUF:ENSURE ;
: BUFT-USE-GROW ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF 0 BUFT-N>BLEN BUF:GROW SPAN:LEN drop ;
: BUFT-USE-LEN ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:LEN@ drop ;
: BUFT-USE-CLEAR ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   BUFT-REL-BUF BUF:CLEAR ;
: BUFT-USE-REPLACED ( -- )
   BUFT-CAP0 BUFT-REL-INIT  BUFT-REL-BUF BUF:DISPOSE
   s" a-b" s" -" s" +" BUFT-REL-BUF BUF:APPEND-REPLACED drop ;
: BUFT-INIT-LIVE-REJECTS ( -- )
   BUFT-REL-BUF BUF:DISPOSE
   BUFT-CAP0 BUFT-REL-INIT
   BUFT-REL-BUF BUFT-CAP0 BUFT-N>BLEN BUF:INIT ;    \ re-init without dispose throws (else old mapping leaks)

: BUFT-CAP-ZERO ( -- )    0 BUFT-FRESH ;
: BUFT-APPEND-OVERFLOW ( -- )
   2 BUFT-FRESH
   65 BUFT-BUF BUF:APPEND-BYTE
   s" x" drop MEM-MAX-N BUFT-N>BLEN BUFT-BUF BUF:APPEND-SPAN ;  \ 1 + MAX-BYTES wraps below zero

: BUFT-CHUNK-FILL ( -- )
   1024 0 ?do  i 255 and  BUFT-CHUNK i +  c!  loop ;
: BUFT-BIG-FILL ( -- )
   BUFT-BIG-BUF 16 BUFT-N>BLEN BUF:INIT
   BUFT-CHUNK-FILL
   2048 0 ?do  BUFT-CHUNK 1024 BUFT-N>BLEN BUFT-BIG-BUF BUF:APPEND-SPAN  loop
   BUFT-BIG-BUF BUFT-TLEN BUFT-BIG-N T=
   BUFT-BIG-BUF BUF:SPAN$ BUFT-BL>RAW {: data:ptr len:n :}
   len BUFT-BIG-N T=
   data 0 + c@ 0 T=
   data 1000 + c@ 232 T=                                              \ 1000 & 255
   data BUFT-BIG-N 1 - + c@ 255 T=                                    \ last byte = 1023 & 255
   BUFT-BIG-BUF BUF:DISPOSE ;

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
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW  BUFT-REF PROP:COUNT@  T$= ;

: BUFT-RUN ( -- )
   T-RESET
   s" shared byte-length conversions admit zero and the full nonnegative range" T-LABEL
   0 BUF:N>BLEN BUF:BLEN>N 0 T=
   MEM-MAX-N BUF:N>BLEN BUF:BLEN>N MEM-MAX-N T=
   [: -1 BUF:N>BLEN drop ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-GROW
   BUFT-APPEND-SPAN
   BUFT-APPEND-SELF
   BUFT-APPEND-PART
   BUFT-CONTENT
   BUFT-RESERVE
   BUFT-ENSURE
   BUFT-GROW-TAIL
   BUFT-REPLACE
   BUFT-CLEAR
   BUFT-APPEND-REPLACED
   BUFT-GROW-RELEASES
   BUFT-DISPOSE-RELEASES
   BUFT-DISPOSE-DOUBLE
   BUFT-DISPOSE-CYCLE-BOUNDED
   BUFT-GROW-FAIL-ATOMIC
   [: BUFT-USE-SPAN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-CONTENT ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-APPEND ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-RESERVE ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-ENSURE-ZERO ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-GROW ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-LEN ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-CLEAR ;] E-BUF-STATE TTHROWSQ
   [: BUFT-USE-REPLACED ;] E-BUF-STATE TTHROWSQ
   [: BUFT-INIT-LIVE-REJECTS ;] E-BUF-STATE TTHROWSQ
   [: BUFT-CAP-ZERO ;] E-BUF-CAPACITY TTHROWSQ
   [: BUFT-APPEND-OVERFLOW ;] E-BUF-CAPACITY TTHROWSQ
   [: BUFT-GROW-TAIL-OVERFLOW ;] E-BUF-CAPACITY TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" A" T$=                          \ refused before the length moved
   [: BUFT-REPLACED-SELF ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" a-b" T$=                        \ refused before a byte moved
   [: BUFT-REPLACED-STRADDLE ;] E-BUF-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" a-b" T$=
   [: -1 1 1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: 3 -1 1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   [: 3 1 -1 BUFT-REPLACED-NEG ;] E-STR-BOUNDS TTHROWSQ
   BUFT-BUF BUF:SPAN$ BUFT-BL>RAW s" a-b" T$=
   BUFT-GROW-OVER-READ
   BUFT-REL-BUF BUF:DISPOSE                                          \ the refused grow left the buffer owned
   BUFT-BIG-FILL
   BUFT-PROP
   s" BOK-INIT ( ptr a NUM:byte-len -- ) BUF:INIT" CHECK-QUIET-CANDIDATE! -1 T=
   s" BOK-SPAN ( ptr u8 NUM:byte-len ptr a -- ) BUF:APPEND-SPAN" CHECK-QUIET-CANDIDATE! -1 T=
   s" BSWAP-RAW-CAP ( ptr a n -- ) BUF:INIT" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-SPAN ( ptr u8 n ptr a -- ) BUF:APPEND-SPAN" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-RESERVE ( ptr a n -- ) BUF:RESERVE" BUFT-CHECK-REJECTS
   s" BSWAP-RAW-ENSURE ( ptr a n -- ) BUF:ENSURE" BUFT-CHECK-REJECTS
   T-REPORT ;

BUFT-RUN
