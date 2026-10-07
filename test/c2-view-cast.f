\ c2-view-cast.f - C2-MEM's view packs compile at tier 1 as a regrouping.
\
\ Run: the WHITEBOX-SUITE row c2-view-cast (test/gate-stdlib-cases.f), which
\ hands it the unsealed engine (test/whitebox-engine.f): a product seals
\ C2-MEM, and only C2-MEM's private section packs ( ptr u8 n -- V ) or unpacks
\ a mutable view (src/core/checker.f VIEW-CAST-CERTIFY).
\
\ A view is one value of two cells. An unpack leaves the same cells as two
\ values and a pack takes two values back as one; the native compiler keeps the
\ cells where they are and regroups them (src/compiler/native/hir-word.f
\ DECLARE-BOUND-CAST, src/compiler/native/elaborate.f RENAME). The words below
\ are C2-MEM's own shapes, compiled at tier 1 in the reopened package: BIND
\ (unpack, a ( ptr u8 n -- ptr u8 n ) callee, pack), MUT-BYTE! (store through
\ the unpacked base, pack the same cells) and PUBLISH (unpack, pack as a
\ read-view, nothing between). The first two run: the view each returns still
\ reads the byte stored through it, at the last index its bound admits.

require lib/test.f
require lib/memory.f
require lib/c2-memory.f

1 set-tier
package C2-MEM
private

CAST: VC-MUT-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n )
CAST: VC-MUT-PACK ( ptr u8 n -- mut-view<p,q,a,u8> )
CAST: VC-READ-PACK ( ptr u8 n -- read-view<p,q,u8> )

: VC-PASS ( ptr u8 n -- ptr u8 n ) ;

: VC-BIND ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   VC-MUT-UNPACK VC-PASS VC-MUT-PACK ;

: VC-MUT-BYTE! ( mut-view<p,q,a,u8> n u8 -- mut-view<p,q,a,u8> )
   rot VC-MUT-UNPACK {: idx:n value:u8 base:ptr bound:n :}
   idx bound CHECK-INDEX
   value base idx + c!
   base bound VC-MUT-PACK ;

: VC-PUBLISH ( mut-view<p,l,a,u8> -- read-view<p,l,u8> )
   VC-MUT-UNPACK VC-READ-PACK ;
0 set-tier

: VC-BIND-ROOT ( mut-view<p,l,a,u8> -- u8 mut-view<p,l,a,u8> )
   3 $5A MUT-BYTE! VC-BIND 3 MUT-BYTE@ ;

: VC-STORE-ROOT ( mut-view<p,l,a,u8> -- u8 mut-view<p,l,a,u8> )
   3 $4B VC-MUT-BYTE! 3 MUT-BYTE@ ;

: VC-BIND-CASE ( -- u8 )
   4 MEM:BYTES-ALLOC-LEN [: VC-BIND-ROOT ;] WITH-MUT ;

: VC-STORE-CASE ( -- u8 )
   4 MEM:BYTES-ALLOC-LEN [: VC-STORE-ROOT ;] WITH-MUT ;

T-RESET
s" a view through BIND's shape reads the byte stored before it" T-LABEL
VC-BIND-CASE $5A T=
s" a byte MUT-BYTE!'s shape stores reads back through its view" T-LABEL
VC-STORE-CASE $4B T=
T-REPORT

;package
