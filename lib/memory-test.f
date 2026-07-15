\ memory-test.f - focused tests for OS-backed memory buffers.

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/memory.f

64 constant MEMT-BUFS
16 constant MEMT-SPAN-BUFS
32 constant MEMT-SPANS
65 constant MEMT-MARK-A
90 constant MEMT-MARK-Z

variable MEMT-HERE

: MEMT-TOTAL ( -- n )
   MEMT-BUFS MEM-64K * ;

: MEMT-SPAN-TOTAL ( -- n )
   MEMT-SPAN-BUFS MEM-64K * ;

: MEMT-ZERO-BYTES ( -- )
   0 MEM-ALLOC-BYTES 2drop ;

: MEMT-NEG-BYTES ( -- )
   -1 MEM-ALLOC-BYTES 2drop ;

: MEMT-ZERO-64K ( -- )
   0 MEM-ALLOC-64K-BUFFERS 2drop ;

: MEMT-TOO-MANY-64K ( -- )
   MEM-MAX-64K-BUFFERS 1 + MEM-64K-BYTES drop ;

: MEMT-TOO-LARGE-SPAN ( -- )
   MEM-MAX-N MEM-ALLOC-64K-SPAN 2drop ;

: MEMT-ZERO-CELLS ( -- )
   0 >COUNT MEM-ALLOC-CELLS drop ;

: MEMT-TOO-MANY-CELLS ( -- )
   MEM-MAX-CELLS 1 + >COUNT MEM-CELLS>BYTES drop ;

: MEMT-END ( ptr u8 n -- ptr u8 ) {: a:ptr u :}
   a u 1 - + ;

: MEMT-TOUCH-ENDS ( ptr u8 n -- ) {: a:ptr u :}
   MEMT-MARK-A a c!
   MEMT-MARK-Z a u MEMT-END c!
   a c@ MEMT-MARK-A T=
   a u MEMT-END c@ MEMT-MARK-Z T= ;

: MEMT-TOUCH-SLOTS ( ptr u8 n n -- ) {: a:ptr u count :}
   count MEM-64K * u T=
   count 0 ?do
      MEMT-MARK-A i + a i MEM-64K * + c!
      a i MEM-64K * + c@ MEMT-MARK-A i + T=
   loop ;

: MEMT-TOUCH-64K-SLOTS ( ptr u8 n -- )
   MEMT-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-TOUCH-SPAN-SLOTS ( ptr u8 n -- )
   MEMT-SPAN-BUFS MEMT-TOUCH-SLOTS ;

: MEMT-CELL-SPAN ( -- )
   4 >COUNT MEM-ALLOC-CELLS {: a:ptr :}
   111 a !
   222 a 1 cells + !
   a @ 111 T=
   a 1 cells + @ 222 T= ;

: MEMT-SINGLE-64K ( -- )
   MEM-ALLOC-64K
   dup MEM-64K T=
   MEMT-TOUCH-ENDS ;

: MEMT-MANY-64K ( -- )
   MEMT-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-TOUCH-64K-SLOTS ;

: MEMT-DATA-UNCHANGED ( -- )
   here data-base - MEMT-HERE !
   MEM-ALLOC-64K 2drop
   here data-base - MEMT-HERE @ T= ;

: MEMT-LIVE-SPAN-FRAME ( ptr u8 n n -- ) {: a:ptr u remaining :}
   u MEMT-SPAN-TOTAL T=
   a u MEMT-TOUCH-SPAN-SLOTS
   remaining 0 > if
      MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
      remaining 1 - recurse
   then
   a u MEMT-TOUCH-SPAN-SLOTS ;

: MEMT-MANY-LIVE-SPANS ( -- )
   here data-base - MEMT-HERE !
   MEMT-SPAN-BUFS MEM-ALLOC-64K-BUFFERS
   MEMT-SPANS 1 - MEMT-LIVE-SPAN-FRAME
   here data-base - MEMT-HERE @ T= ;

T-RESET
MEM-64K $10000 T=
MEM-MAP-SHARED 1 T=
1 MEM-64K-BYTES MEM-64K T=
1 MEM-64K-COUNT-FOR 1 T=
MEM-64K MEM-64K-COUNT-FOR 1 T=
MEM-64K 1 + MEM-64K-COUNT-FOR 2 T=
MEM-CELL-BYTES 1 cells T=
1 >COUNT MEM-CELLS>BYTES MEM-CELL-BYTES T=
1 MEM-64K-SPAN-BYTES MEM-64K T=
MEM-64K 1 + MEM-64K-SPAN-BYTES MEM-64K 2 * T=
MEMT-CELL-SPAN
MEMT-SINGLE-64K
MEMT-MANY-64K
MEMT-DATA-UNCHANGED
MEMT-MANY-LIVE-SPANS
' MEMT-ZERO-BYTES E-MEM-SIZE TTHROWS
' MEMT-NEG-BYTES E-MEM-SIZE TTHROWS
' MEMT-ZERO-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-64K E-MEM-SIZE TTHROWS
' MEMT-TOO-LARGE-SPAN E-MEM-SIZE TTHROWS
' MEMT-ZERO-CELLS E-MEM-SIZE TTHROWS
' MEMT-TOO-MANY-CELLS E-MEM-SIZE TTHROWS
T-REPORT

\ ---- B5 package MEM: typed allocation roles (MODEL-CAD-V2-PLAN.md B5.5) --------
\ numeric-result<a> is a layout value with no polymorphic eliminator yet, so each
\ classifier MATCHes the concrete role it holds and maps ok -> 0 / refusal ->
\ E-CADNUM-* (the cad-num-types-test.f idiom). The scalar words admit zero; only
\ the alloc-* sinks reject zero/overflow, and that refusal precedes any mmap.

: MEMT-BL-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-IC-CODE ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-AB-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: MEMT-AC-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

\ scalar sizing: raw n -> zero-admitting role -> MEM word -> class
: MEMT-CELLS>BYTES# ( n -- n ) CAD-NUM:CELL-COUNT
   MATCH CAD-NUM:numeric-result ok OF MEM:CELLS>BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-BYTES# ( n -- n ) CAD-NUM:ITEM-COUNT
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-COUNT-FOR# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-COUNT-FOR MEMT-IC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-64K-SPAN# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-SPAN-BYTES MEMT-BL-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;

\ allocation narrowing: raw n -> role -> AS-ALLOC-* -> class (never allocates on refuse)
: MEMT-BYTES-ALLOC# ( n -- n ) CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result ok OF CAD-NUM:AS-ALLOC-BYTE-LEN MEMT-AB-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;
: MEMT-CELLS-ALLOC# ( n -- n ) CAD-NUM:CELL-COUNT
   MATCH CAD-NUM:numeric-result ok OF CAD-NUM:AS-ALLOC-CELL-COUNT MEMT-AC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF misaligned OF E-CADNUM-MISALIGNED ENDOF ;MATCH ;

\ real typed allocation via the shared MEM narrowing helpers: MEM:BYTES-ALLOC-LEN
\ / MEM:CELLS-ALLOC-COUNT take a raw n straight to the validated alloc role and
\ throw E-MEM-SIZE on any refusal before an mmap (TTHROWSQ cases drive the
\ zero/negative/overflow refusals in RT-MEM).

\ The returned alloc role's raw projection is MEM-private, so the tests touch
\ memory over the KNOWN raw extent and drop the role.
: MEMT-TYPED-BYTES ( -- )                    \ typed byte alloc is writable end to end
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop {: a:ptr :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-64K ( -- )                      \ typed single-64K alloc is writable end to end
   MEM:ALLOC-64K drop {: a:ptr :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-CELLS ( -- )                    \ typed cell alloc stores/fetches two cells
   4 MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS {: a:ptr :}
   111 a !  222 a 1 cells + !
   a @ 111 T=  a 1 cells + @ 222 T= ;

\ ---- purified MEM:64K-COUNT-FOR: byte-identical parity with the legacy raw word -
\ MEM:64K-COUNT-FOR now composes CAD-NUM:DIV-BYTES-CEIL and reads no raw cell.
\ Pin its EXACT item-count at the boundary inputs 1 / 64K / 64K+1 / MAX-N so the
\ purification stays behavior-identical: the ceil counts match the legacy raw
\ MEM-64K-COUNT-FOR at 1 / 64K / 64K+1, and at MAX-N the pure scalar word returns
\ the true ceil (MEM-MAX-64K-BUFFERS+1), where the raw word instead throws its
\ own bundled buffer ceiling (that allocation ceiling now lives at the alloc sink).
package CAD-NUM
public
: IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;   \ test-only white-box reader
;package

: MEMT-64K-COUNT ( n -- n ) CAD-NUM:BYTE-LEN          \ raw bytes -> typed ceil count value
   MATCH CAD-NUM:numeric-result ok OF MEM:64K-COUNT-FOR
      MATCH CAD-NUM:numeric-result ok OF CAD-NUM:IC>RAW ENDOF
         negative OF E-MEM-SIZE throw ENDOF zero OF E-MEM-SIZE throw ENDOF
         overflow OF E-MEM-SIZE throw ENDOF underflow OF E-MEM-SIZE throw ENDOF
         bad-alignment OF E-MEM-SIZE throw ENDOF misaligned OF E-MEM-SIZE throw ENDOF ;MATCH ENDOF
      negative OF E-MEM-SIZE throw ENDOF zero OF E-MEM-SIZE throw ENDOF overflow OF E-MEM-SIZE throw ENDOF
      underflow OF E-MEM-SIZE throw ENDOF bad-alignment OF E-MEM-SIZE throw ENDOF misaligned OF E-MEM-SIZE throw ENDOF ;MATCH ;

: MEMT-COUNT-PARITY ( -- )                            \ exact-count regression at 1/64K/64K+1/MAX-N
   1 MEMT-64K-COUNT 1 T=
   MEM-64K MEMT-64K-COUNT 1 T=
   MEM-64K 1 + MEMT-64K-COUNT 2 T=
   MEM-MAX-N MEMT-64K-COUNT MEM-MAX-64K-BUFFERS 1 + T= ;

: RT-MEM ( -- )
   T-RESET
   MEMT-COUNT-PARITY
   \ scalar sizing admits zero and positive (zero is a valid scalar answer)
   0 MEMT-CELLS>BYTES# 0 T=      1 MEMT-CELLS>BYTES# 0 T=
   0 MEMT-64K-BYTES# 0 T=        3 MEMT-64K-BYTES# 0 T=
   0 MEMT-64K-COUNT-FOR# 0 T=    1 MEMT-64K-COUNT-FOR# 0 T=   MEM-64K 1 + MEMT-64K-COUNT-FOR# 0 T=
   0 MEMT-64K-SPAN# 0 T=         MEM-64K 1 + MEMT-64K-SPAN# 0 T=
   \ allocation sinks reject zero (never allocate) and reject negative
   0 MEMT-BYTES-ALLOC# E-CADNUM-ZERO T=      -1 MEMT-BYTES-ALLOC# E-CADNUM-NEGATIVE T=
   0 MEMT-CELLS-ALLOC# E-CADNUM-ZERO T=      -1 MEMT-CELLS-ALLOC# E-CADNUM-NEGATIVE T=
   1 MEMT-BYTES-ALLOC# 0 T=                  1 MEMT-CELLS-ALLOC# 0 T=
   \ over-allocation fails at validation, BEFORE any mmap primitive is reachable
   MEM-MAX-CELLS 1 + MEMT-CELLS-ALLOC# E-CADNUM-OVERFLOW T=
   \ real typed allocations touch OS-backed memory
   MEMT-TYPED-BYTES
   MEMT-TYPED-64K
   MEMT-TYPED-CELLS
   \ shared narrowing helpers refuse zero/negative/overflow with E-MEM-SIZE
   \ before any mmap primitive is reachable
   [: 0 MEM:BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 MEM:BYTES-ALLOC-LEN drop ;] E-MEM-SIZE TTHROWSQ
   [: 0 MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: -1 MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   [: MEM-MAX-CELLS 1 + MEM:CELLS-ALLOC-COUNT drop ;] E-MEM-SIZE TTHROWSQ
   T-REPORT ;
RT-MEM

\ ---- static rejection matrix: frozen signatures accept; role swaps reject ------
\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
: STAT-MEM ( -- )
   T-RESET
   \ positive signature controls: the exact B5.5-frozen effects resolve.
   s" G-CELLS>BYTES ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:CELLS>BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-BYTES ( CAD-NUM:item-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-COUNT-FOR ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:item-count> ) MEM:64K-COUNT-FOR"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-64K-SPAN ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-SPAN-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC ( CAD-NUM:alloc-byte-len -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC-CELLS ( CAD-NUM:alloc-cell-count -- ptr a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-ALLOC-64K ( -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-64K"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-BYTES-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len ) MEM:BYTES-ALLOC-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" G-CELLS-ALLOC-COUNT ( n -- CAD-NUM:alloc-cell-count ) MEM:CELLS-ALLOC-COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: a zero-admitting role at the sink, byte<->cell role swaps, raw n.
   s" B-ZEROABLE-ALLOC ( CAD-NUM:byte-len -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-CELL-ROLE-BYTES ( CAD-NUM:alloc-cell-count -- ptr u8 CAD-NUM:alloc-byte-len ) MEM:ALLOC-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BYTE-ROLE-CELLS ( CAD-NUM:alloc-byte-len -- ptr a ) MEM:ALLOC-CELLS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RAW-CELLS>BYTES ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:CELLS>BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-BYTE-ROLE-64K-BYTES ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) MEM:64K-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT-MEM
s" memory-test: ok" type cr
