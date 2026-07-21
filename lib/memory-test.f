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
\ EXEMPLAR MIGRATION: a bounded-lifetime scratch mapping that formerly leaked
\ (`MEM:ALLOC-BYTES drop {: a:ptr :}` used the buffer then dropped its length, so it
\ could never be released) now scopes the mapping in MEM:WITH-BYTES - written+read end
\ to end inside the body, released on scope exit. The write/read body is a named word
\ because a quotation cannot hold locals.
: MEMT-TYPED-BYTES-BODY ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: a:ptr len :}
   MEMT-MARK-A a c!  MEMT-MARK-Z a MEM-64K 1 - + c!
   a c@ MEMT-MARK-A T=  a MEM-64K 1 - + c@ MEMT-MARK-Z T= ;
: MEMT-TYPED-BYTES ( -- )                    \ typed byte alloc is writable end to end, scoped (no leak)
   MEM-64K MEM:BYTES-ALLOC-LEN [: MEMT-TYPED-BYTES-BODY ;] MEM:WITH-BYTES ;
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

\ ---- RELEASE-BYTES: the typed munmap inverse of ALLOC-BYTES -------------------
\ Allocate a real mapping, write + read both ends, then release it; a clean return
\ (munmap rc 0, no throw) is the positive proof. A one-byte-misaligned address is a
\ forged pointer the kernel rejects, so RELEASE-BYTES propagates E-MEM-UNMAP. A
\ zero/negative release length never narrows to an alloc role, so it is refused
\ with E-MEM-SIZE at the typed boundary, before any munmap.
: MEMT-ALLOC-WRITE-RELEASE ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES         \ ptr-u8 alloc-byte-len
   over MEMT-MARK-A swap c!                             \ first byte
   over MEM-64K 1 - + MEMT-MARK-Z swap c!              \ last byte
   over c@ MEMT-MARK-A T=
   over MEM-64K 1 - + c@ MEMT-MARK-Z T=
   MEM:RELEASE-BYTES ;
: MEMT-RELEASE-FORGED ( -- )
   MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES         \ ptr-u8 alloc-byte-len
   swap 1 + swap                                        \ misalign the address by one byte
   MEM:RELEASE-BYTES ;
: MEMT-RELEASE-ZERO ( -- )
   MEM:ALLOC-64K drop  0 MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;
: MEMT-RELEASE-NEG ( -- )
   MEM:ALLOC-64K drop -1 MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

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
   \ typed release round-trips a real mapping; a forged address propagates
   \ E-MEM-UNMAP; a zero/negative length refuses at the typed boundary before munmap
   MEMT-ALLOC-WRITE-RELEASE
   [: MEMT-RELEASE-FORGED ;] E-MEM-UNMAP TTHROWSQ
   [: MEMT-RELEASE-ZERO ;] E-MEM-SIZE TTHROWSQ
   [: MEMT-RELEASE-NEG ;] E-MEM-SIZE TTHROWSQ
   T-REPORT ;
RT-MEM

\ ---- MEM:WITH-BYTES: quotation-scoped mapped memory (RAII) --------------------
\ Release is proved by ADDRESS REUSE (the lib/vector-test.f idiom): a mapping freed
\ to the OS leaves a hole the next same-size mmap refills, so a fresh allocation lands
\ on the freed address. RED-FIRST both ways: an UNSCOPED raw alloc that throws leaks
\ (its hole is NOT refilled - fresh alloc lands elsewhere); the SCOPED version releases
\ on the throw so its hole IS refilled. Nested WITH-BYTES releases the inner mapping
\ before the outer (reverse order); the outer address is refilled only after the outer
\ scope exits, and its correct refill proves the per-call frame was not clobbered by the
\ inner call (the double-release / wrong-buffer guard). Distinct sizes keep each probe's
\ freed hole unambiguous. Bodies are named words (a quotation cannot hold locals).
-7777 constant E-PRIMARY                              \ a body error distinct from the E-MEM-* codes
$20000 constant WBT-SZ-A                              \ 128K: throw + nested-outer probe size
$30000 constant WBT-SZ-B                              \ 192K: nested-inner probe size
$40000 constant WBT-SZ-C                              \ 256K: unscoped-leak control size
create WBT-CAP-A  2 cells allot                       \ ptr-field slots capturing a mapping's fat pointer
create WBT-OUT-A  2 cells allot
create WBT-IN-A   2 cells allot
create WBT-LEAK-A 2 cells allot

: WBT-RES ( ptr u8 CAD-NUM:alloc-byte-len -- n ) {: buf:ptr len :}   \ write+read the mapping, return the byte
   MEMT-MARK-A buf c!  buf c@ ;
: WBT-THROW ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: buf:ptr len :}   \ capture the mapping, then throw mid-body
   buf WBT-CAP-A 0 ptr-field !  E-PRIMARY throw ;
: WBT-INNER ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: buf:ptr len :}
   buf WBT-IN-A 0 ptr-field ! ;
: WBT-OUTER ( ptr u8 CAD-NUM:alloc-byte-len -- ) {: buf:ptr len :}
   buf WBT-OUT-A 0 ptr-field !
   WBT-SZ-B MEM:BYTES-ALLOC-LEN [: WBT-INNER ;] MEM:WITH-BYTES         \ inner scope releases on exit
   WBT-SZ-B MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: xb:ptr xl :}        \ fresh inner-size alloc
   xb WBT-IN-A 0 ptr-field @ = TTRUE                                    \ refills the inner hole (inner freed, outer live)
   xb xl MEM:RELEASE-BYTES ;
: WBT-LEAK ( -- ) {: :}                                                 \ UNSCOPED: raw alloc + throw, no release
   WBT-SZ-C MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: lb:ptr ll :}
   lb WBT-LEAK-A 0 ptr-field !  E-PRIMARY throw ;

: WBT-RESULT ( -- n )      MEM-64K MEM:BYTES-ALLOC-LEN [: WBT-RES ;] MEM:WITH-BYTES ;
: WBT-THROW-CALL ( -- )    WBT-SZ-A MEM:BYTES-ALLOC-LEN [: WBT-THROW ;] MEM:WITH-BYTES ;

: RT-WITH-BYTES ( -- )
   T-RESET
   \ 1. result threading: the body's row S flows out through the scope
   WBT-RESULT MEMT-MARK-A T=
   \ 2. RED-FIRST control: an unscoped alloc that throws LEAKS (hole NOT refilled)
   [: WBT-LEAK ;] E-PRIMARY TTHROWSQ
   WBT-SZ-C MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: cb:ptr cl :}
   cb WBT-LEAK-A 0 ptr-field @ = 0= TTRUE                               \ distinct address: the leak is resident
   cb cl MEM:RELEASE-BYTES
   WBT-LEAK-A 0 ptr-field @ WBT-SZ-C MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES   \ reclaim the control leak
   \ 3. SCOPED throw releases exactly once: the freed hole IS refilled + primary error preserved
   [: WBT-THROW-CALL ;] E-PRIMARY TTHROWSQ
   WBT-SZ-A MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: rb:ptr rl :}
   rb WBT-CAP-A 0 ptr-field @ = TTRUE
   rb rl MEM:RELEASE-BYTES
   \ 4. nested two-buffer: inner released before outer (reverse order); outer frame intact
   WBT-SZ-A MEM:BYTES-ALLOC-LEN [: WBT-OUTER ;] MEM:WITH-BYTES
   WBT-SZ-A MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES {: yb:ptr yl :}
   yb WBT-OUT-A 0 ptr-field @ = TTRUE                                   \ outer hole refilled only after outer exit
   yb yl MEM:RELEASE-BYTES
   T-REPORT ;
RT-WITH-BYTES

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
   \ RELEASE-BYTES demands the exact ptr u8 + alloc-byte-len ALLOC-BYTES mints: the
   \ frozen signature resolves, while a raw-integer address, a raw-n length, a
   \ zero-admitting byte-len, or a cell role are checker rejects, so no forged
   \ address or unvalidated size reaches munmap.
   s" G-RELEASE ( ptr u8 CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-RELEASE-RAW-PTR ( n CAD-NUM:alloc-byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-RAW-LEN ( ptr u8 n -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-ZEROABLE-LEN ( ptr u8 CAD-NUM:byte-len -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-RELEASE-CELL-LEN ( ptr u8 CAD-NUM:alloc-cell-count -- ) MEM:RELEASE-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   \ WITH-BYTES types the quotation body row: the frozen signature (row-polymorphic S
   \ threaded from the body) resolves, while a raw-n length or a cell-count role in the
   \ scoped length + body row are checker rejects, so a caller cannot scope a mapping
   \ over an unvalidated size or a byte/cell role swap.
   s" G-WITH-BYTES ( R CAD-NUM:alloc-byte-len [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" B-WB-RAW-LEN ( R n [ R ptr u8 CAD-NUM:alloc-byte-len -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" B-WB-CELL-LEN ( R CAD-NUM:alloc-cell-count [ R ptr u8 CAD-NUM:alloc-cell-count -- S ] -- S ) MEM:WITH-BYTES"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT-MEM
s" memory-test: ok" type cr
