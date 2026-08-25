\ lit-emit-size-test.f - exact compiled-body sizes for scalar literal emission, and
\ the structural proof that relocatable addresses never flow through the scalar path.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f \
\        lib/test/src-shape.f test/lit-emit-size-test.f
\
\ Dot habu-separate-scalar-and: scalar constants and string lengths now emit a MINIMAL
\ MOVZ/MOVN+MOVK chain through the shared synthesizer (LVMOVK, via LVLITPUSH) instead of
\ the fixed four-instruction x9 chain. A word body's byte footprint is the address gap to
\ the next word (code is emitted contiguously), so `['] NEXT ['] W -` is W's exact body
\ length. Red-first: on the pre-fix engine every constant baked the full four-instruction
\ chain, so each footprint below was 28 and each assertion fails; the minimal emitter makes
\ them 16/20/24/28 by needed-chunk count (both-direction proof recorded 2026-07-21: the
\ base fixpoint yields 28/28/28/28/64/68, this branch 16/20/24/28/52/56).

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/test/src-shape.f

package LIT-EMIT-SIZE-TEST

\ --- fixtures: one constant per MOVZ/MOVN+MOVK shape, packed contiguously so each gap is
\ exactly the preceding word's body. Trailing markers bound the last measured word. ---
0 constant Z0                              \ zero               -> 1 chunk  (movz)
42 constant Z1                             \ small positive     -> 1 chunk  (movz, the K=42 case)
-1 constant ZN1                            \ all ones           -> 1 chunk  (movn, MOVN-favorable)
-2 constant ZN2                            \ high chunks $FFFF  -> 1 chunk  (movn, MOVN-favorable)
$12345678 constant Z2                      \ two 16-bit chunks  -> movz+movk
$1234567890AB constant Z3                  \ three chunks
$1122334455667788 constant Z4              \ four chunks (a genuine 64-bit value)
0 constant CEND                            \ marker: bounds Z4

: SEMPTY ( -- ) s" " 2drop ;               \ empty string literal
: SONE ( -- ) s" x" 2drop ;                \ one-byte string literal
: SMARK ( -- ) ;                           \ marker: bounds SONE

: BODY ( n n -- n ) - ;                    \ body length = gap to the next contiguous word

: SIZES ( -- )
   T-RESET
   \ Exact scalar-body sizes: minimal chain (n chunks) + push (2 instr) + ret = (n+3)*4 bytes.
   ['] Z1  ['] Z0  BODY 16 T=              \ zero:            1 chunk  -> 16
   ['] ZN1 ['] Z1  BODY 16 T=              \ 42 (K):          1 chunk  -> 16  (was 28)
   ['] ZN2 ['] ZN1 BODY 16 T=              \ -1  MOVN:        1 chunk  -> 16
   ['] Z2  ['] ZN2 BODY 16 T=              \ -2  MOVN:        1 chunk  -> 16
   ['] Z3  ['] Z2  BODY 20 T=              \ 2 chunks:                -> 20
   ['] Z4  ['] Z3  BODY 24 T=              \ 3 chunks:                -> 24
   ['] CEND ['] Z4 BODY 28 T=             \ 4 chunks (full 64-bit):  -> 28
   \ String-word bodies shrink by the same 12 bytes the minimal length-push saves.
   ['] SONE  ['] SEMPTY BODY 52 T=
   ['] SMARK ['] SONE   BODY 56 T=
   T-REPORT ;

\ --- Structural proof (item: a scalar numerically inside an address range is never
\ relocated). Scalars materialize into x16 via the shared synthesizer; only the dedicated
\ address emitters build the fixed four-instruction x9 chain the AOT relocation recognises,
\ so no scalar can ever present the x9 shape the reloc scan matches on. ---
: SHAPE ( -- )
   s" src/habu/habu2.f" SHAPE:LOAD
   s" : C-LIT ( -- )" SHAPE:MUST-HAVE
   s" LVLITPUSH LABEL@ BL," SHAPE:MUST-HAVE            \ scalar-push -> shared x16 synthesizer
   s" : C-RAW-LIT ( -- )" SHAPE:MUST-HAVE
   s" 14 16 MOVZ,  LVMOVK LABEL@ BL," SHAPE:MUST-HAVE  \ raw scalar -> x16, minimal
   s" : C-DATA-ADDR ( -- )" SHAPE:MUST-HAVE
   s" : C-DATA-ADDR-RAW ( -- )" SHAPE:MUST-HAVE
   s" : C-CODE-ADDR ( -- )" SHAPE:MUST-HAVE
   s" : C-X9-LIT" SHAPE:MUST-LACK ;                    \ the conflated scalar/address emitter is gone

: MAIN ( -- )
   SIZES
   SHAPE
   s" lit-emit-size-test: ok" type cr ;

MAIN

;package
