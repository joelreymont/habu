\ certify-dynamic-buffer.f - certification knows what DYNAMIC-BUFFER publishes.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f \
\   src/habu/verify-source.f test/checker-assert.f test/certify-dynamic-buffer.f
\
\ One DYNAMIC-BUFFER line publishes three checked words through the generated
\ source in src/core/layout-buffer.f DBUF-SOURCE: the accessor
\ NAME ( n -- ptr <type> ), NAME-RESERVE ( n -- ) and NAME-RELEASE ( -- ).
\ Certification never executes the definer, so the verify-source scanner has to
\ know the triple statically. It did not, and the gap did not surface until a
\ full engine build: `bin/hb --load tools/build-fixpoint.f -- all --force` died
\ at `certify: stage2-src rejected rc 70`, E-UNDEFINED on
\ AOT-NAMES-STORAGE-RESERVE, because src/habu/aot-decl.f declares its names
\ buffer this way. This file is that failure without a build - the scanner and
\ the checker registration in one process, red in about a second.
\
\ Every verdict is measured, and the negatives carry the discrimination: a name
\ the definer does NOT publish stays unresolvable (1) rather than being swept in
\ by a prefix match, the hidden `#base` storage word stays unresolvable too, and
\ each published word is refused (0) when called with the wrong stack, which is
\ what proves the registered effect is the generated one and not a permissive
\ stand-in. The last section runs the same declaration for real, so the
\ certified effects are pinned against what the definer actually does.

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/verify-source.f
require test/checker-assert.f

package CERTIFY-DYNAMIC-BUFFER

-1 constant ACCEPTED
0 constant REFUSED
1 constant UNRESOLVED

: CDB-VERDICT ( ptr u8 n -- n )
   CHECK-QUIET-CANDIDATE! ;

\ ---- 1. the three published words, and the stack each one declares ----------
: CDB-SECTION-PUBLISHED ( -- )
   s" DYNAMIC-BUFFER CDBT n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the accessor certifies as ( n -- ptr n )" T-LABEL
   s" C1 ( n -- ptr n ) CDBT" CDB-VERDICT ACCEPTED T=
   s" the accessor without its index is refused" T-LABEL
   s" C2 ( -- ptr n ) CDBT" CDB-VERDICT REFUSED T=
   s" the accessor pointee is the declared type" T-LABEL
   s" C3 ( n -- ptr u8 ) CDBT" CDB-VERDICT REFUSED T=
   s" -RESERVE certifies as ( n -- )" T-LABEL
   s" C4 ( n -- ) CDBT-RESERVE" CDB-VERDICT ACCEPTED T=
   s" -RESERVE without its count is refused" T-LABEL
   s" C5 ( -- ) CDBT-RESERVE" CDB-VERDICT REFUSED T=
   s" -RELEASE certifies as ( -- )" T-LABEL
   s" C6 ( -- ) CDBT-RELEASE" CDB-VERDICT ACCEPTED T=
   s" -RELEASE taking an argument is refused" T-LABEL
   s" C7 ( n -- ) CDBT-RELEASE" CDB-VERDICT REFUSED T= ;

\ ---- 2. nothing else is published -------------------------------------------
: CDB-SECTION-UNPUBLISHED ( -- )
   s" a suffix the definer never emits is unresolvable" T-LABEL
   s" C8 ( n -- ) CDBT-RESIZE" CDB-VERDICT UNRESOLVED T=
   s" the hidden storage word stays unresolvable" T-LABEL
   s" C9 ( -- ptr a ) CDBT#base" CDB-VERDICT UNRESOLVED T= ;

\ ---- 3. the production shape that broke the build ---------------------------
\ Verbatim src/habu/aot-decl.f: the declaration and the word whose body reaches
\ the generated -RESERVE.
: CDB-SECTION-AOT-SHAPE ( -- )
   s" DYNAMIC-BUFFER CDB-NAMES-STORAGE n" VERIFY:SOURCE-BUF-IN-SCOPE
   s" the aot-decl reserve body certifies" T-LABEL
   s" CDB-NAMES-RESERVE ( n -- ) CELL 1- + CELL / CDB-NAMES-STORAGE-RESERVE"
      CDB-VERDICT ACCEPTED T= ;

\ ---- 4. a typed pointee flows into the accessor -----------------------------
: CDB-SECTION-TYPED-POINTEE ( -- )
   s" DYNAMIC-BUFFER CDBP ptr u8" VERIFY:SOURCE-BUF-IN-SCOPE
   s" a ptr-typed cell certifies as ( n -- ptr ptr u8 )" T-LABEL
   s" C10 ( n -- ptr ptr u8 ) CDBP" CDB-VERDICT ACCEPTED T=
   s" the same accessor over a bare cell is refused" T-LABEL
   s" C11 ( n -- ptr n ) CDBP" CDB-VERDICT REFUSED T= ;

\ ---- 5. the certified effects are the ones the definer really has ------------
DYNAMIC-BUFFER CDB-LIVE n

: CDB-PUT ( n n -- ) {: i:n v:n :} v i CDB-LIVE ! ;
: CDB-GET ( n -- n ) CDB-LIVE @ ;

: CDB-SECTION-LIVE ( -- )
   8 CDB-LIVE-RESERVE
   0 42 CDB-PUT
   7 99 CDB-PUT
   s" the live buffer round-trips its first cell" T-LABEL
   0 CDB-GET 42 T=
   s" the live buffer round-trips its last cell" T-LABEL
   7 CDB-GET 99 T=
   s" a reserve past the end still bounds the accessor" T-LABEL
   16 CDB-LIVE-RESERVE
   0 CDB-GET 42 T=
   CDB-LIVE-RELEASE ;

: MAIN ( -- )
   T-RESET
   CDB-SECTION-PUBLISHED
   CDB-SECTION-UNPUBLISHED
   CDB-SECTION-AOT-SHAPE
   CDB-SECTION-TYPED-POINTEE
   CDB-SECTION-LIVE
   T-REPORT ;

MAIN

;package
