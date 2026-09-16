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
\
\ Dot habu-use-pre-and-1830972f moved every one of them again, because a data-stack
\ push is now ONE instruction: `str x16,[x19],#8` writes the pointer back as part of
\ the store, where it used to be `str x16,[x19]` followed by `add x19,x19,#8`. The
\ figures below are read off the compiled bodies, not adjusted:
\
\   0 constant Z0        movz x16,#0 ; str x16,[x19],#8 ; ret                    12
\   $12345678 constant   movz + movk ; str x16,[x19],#8 ; ret                    16
\   ...three chunks                                                              20
\   ...four chunks       movz + movk*3 ; str x16,[x19],#8 ; ret                  24
\
\ A constant carries no frame -- a created word never had a prologue -- so its body is
\ exactly the chunk chain, the one push and the return: (chunks + 2) * 4, where it was
\ (chunks + 3) * 4.
\
\ : SEMPTY ( -- ) s" " 2drop ; loses FOUR instructions and lands at 36:
\
\   str x30,[sp,#-16]!     the frame, one instruction now, not sub sp + str x30
\   b over the bytes
\   adr x9                 the literal's address
\   str x9,[x19],#8        ...pushed, one instruction now
\   movz x16,#0            its length
\   str x16,[x19],#8       ...pushed, one instruction now
\   bl 2drop               which is what makes the body keep a frame at all
\   ldr x30,[sp],#16       one instruction now, not ldr x30 + add sp
\   ret
\
\ SONE is the same nine instructions plus the one word its byte sits in: 40.

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
\ Execution tokens as cell values: a constant's ( -- a ) and a string word's ( -- ).
TRUSTED: XT>N ( [ -- a ] -- n ) ;
TRUSTED: XT0>N ( [ -- ] -- n ) ;

: SIZES ( -- )
   T-RESET
   \ Exact scalar-body sizes: minimal chain (n chunks) + push (1 instr) + ret = (n+2)*4 bytes.
   ['] Z1 XT>N  ['] Z0 XT>N  BODY 12 T=              \ zero:            1 chunk  -> 12
   ['] ZN1 XT>N ['] Z1 XT>N  BODY 12 T=              \ 42 (K):          1 chunk  -> 12
   ['] ZN2 XT>N ['] ZN1 XT>N BODY 12 T=              \ -1  MOVN:        1 chunk  -> 12
   ['] Z2 XT>N  ['] ZN2 XT>N BODY 12 T=              \ -2  MOVN:        1 chunk  -> 12
   ['] Z3 XT>N  ['] Z2 XT>N  BODY 16 T=              \ 2 chunks:                -> 16
   ['] Z4 XT>N  ['] Z3 XT>N  BODY 20 T=              \ 3 chunks:                -> 20
   ['] CEND XT>N ['] Z4 XT>N BODY 24 T=              \ 4 chunks (full 64-bit):  -> 24
   \ String-word bodies lose four instructions: both ends of the frame and both pushes.
   ['] SONE XT0>N  ['] SEMPTY XT0>N BODY 36 T=
   ['] SMARK XT0>N ['] SONE XT0>N   BODY 40 T=
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
