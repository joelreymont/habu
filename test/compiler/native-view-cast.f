\ native-view-cast.f - a read-view unpacked by a CAST: compiles as its two cells.
\
\ A read-view<p,q,T> is one value of two cells, its base under its byte bound.
\ Any package may unpack one in its private section, CAST: ( read-view<p,q,u8>
\ -- ptr u8 n ) (src/core/checker.f VIEW-CAST-CERTIFY): the same two cells as
\ two values. The native compiler keeps the cells where they are and regroups
\ them as the cast's output (src/compiler/native/hir-word.f DECLARE-BOUND-CAST,
\ src/compiler/native/elaborate.f RENAME); while it kept them as the one view
\ value, a body that used the cells apart did not compile at tier 1.
\
\ The words read "habu" through the unpacked base and length and check an
\ index against the unpacked bound. This row asserts them at tier 0 and its
\ twin compiler-native-view-cast-aot at tier 1 (test/compiler/aot-mode.f): the
\ same answers at both tiers. A cast whose sides differ in cells never reaches
\ the compiler: the checker refuses the declaration, at either tier.

require lib/test.f
require lib/errors.f
require lib/memory.f
require lib/c2-memory.f

package VIEW-CAST-TEST
private

CAST: UNPACK ( read-view<p,q,u8> -- ptr u8 n )

\ The byte at idx, read through the unpacked base after the unpacked bound
\ admits idx.
: BYTE-AT ( read-view<p,q,u8> n -- u8 read-view<p,q,u8> )
   {: idx:n :}
   dup UNPACK {: base:ptr bound:n :}
   idx 0 < idx bound >= or if E-SPAN-RANGE throw then
   base idx + c@ swap ;

: LENGTH ( read-view<p,q,u8> -- n read-view<p,q,u8> )
   dup UNPACK nip swap ;

: READS ( read-view<p,q,u8> -- u8 u8 n read-view<p,q,u8> )
   0 BYTE-AT 3 BYTE-AT LENGTH ;

\ "habu", stored by the library's own MUT-BYTE!, so only the reads are under
\ test.
: HABU ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   0 $68 C2-MEM:MUT-BYTE!  1 $61 C2-MEM:MUT-BYTE!
   2 $62 C2-MEM:MUT-BYTE!  3 $75 C2-MEM:MUT-BYTE! ;

: READ-ROOT ( mut-view<p,l,a,u8> -- u8 u8 n mut-view<p,l,a,u8> )
   HABU [: READS ;] C2-MEM:WITH-READ ;

: ANSWERS ( -- u8 u8 n )
   4 MEM:BYTES-ALLOC-LEN [: READ-ROOT ;] C2-MEM:WITH-MUT ;

: OVERRUN-ROOT ( mut-view<p,l,a,u8> -- u8 mut-view<p,l,a,u8> )
   HABU [: 4 BYTE-AT ;] C2-MEM:WITH-READ ;

: OVERRUN ( -- )
   4 MEM:BYTES-ALLOC-LEN [: OVERRUN-ROOT ;] C2-MEM:WITH-MUT drop ;

public

: RUN ( -- )
   T-RESET
   ANSWERS {: first:u8 last:u8 size:n :}
   s" the byte at index 0, through the unpacked base" T-LABEL
   first $68 T=
   s" the byte at index 3, through the unpacked base" T-LABEL
   last $75 T=
   s" the unpacked length" T-LABEL
   size 4 T=
   s" an index at the unpacked bound is E-SPAN-RANGE" T-LABEL
   [: OVERRUN ;] E-SPAN-RANGE TTHROWSQ
   s" a cast from one cell to two is the checker's E-CAST-ARITY" T-LABEL
   s" package VIEW-CAST-TEST private CAST: WIDEN ( n -- n n ) ;package"
   TEST-EVAL:RC E-CAST-ARITY T=
   s" a view cast to one cell is the checker's E-CAST-SCOPE" T-LABEL
   s" package VIEW-CAST-TEST private CAST: NARROW ( read-view<p,q,u8> -- ptr u8 ) ;package"
   TEST-EVAL:RC E-CAST-SCOPE T=
   T-REPORT ;

;package

VIEW-CAST-TEST:RUN
