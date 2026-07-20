\ maki/extent-tensor-test.f - checked tests for TENSOR:/ITENSOR: (maki/extent-tensor.f).
\
\ The acceptance vehicle is the gathered GEMM golden of docs/golden-syntax.md /
\ docs/tma-gather.md:  O[m,n] = sum_k A[ix[m],k] * B[n,k]   (A: MxK, B: NxK, ix: M),
\ written in candidate-B form against the extent-typed accessors and verified
\ numerically against a plain-buffer reference. The negatives are the point of the
\ whole design: a flipped index pair, an ix<#N> where ix<#M> is demanded, and a raw
\ n fed to an accessor are AUTHOR-TIME checker rejects (CHECK-QUIET-CANDIDATE! = 0),
\ not runtime errors. Uses #M/#N/#K (own extents; no other suite declares them).

require lib/test.f
require test/checker-assert.f
require maki/extent-tensor.f

T-RESET

package MAKI

4 EXTENT: #M   3 EXTENT: #N   2 EXTENT: #K
TENSOR:  AT  ( #M #K )        \ A : M x K
TENSOR:  BT  ( #N #K )        \ B : N x K
TENSOR:  OT  ( #M #N )        \ O : M x N
ITENSOR: IXT ( #M #M )        \ ix : gather M positions into A's M-row space

create PA #M #K * cells allot
create PB #N #K * cells allot
create PO #M #N * cells allot
create PR #M #N * cells allot          \ plain-buffer reference output
create PI #M cells allot                \ gather index buffer

\ O[m,n] contraction in candidate-B form: every index crosses explicitly and every
\ accessor call is extent-typed, so a wrong extent cannot slip in.
: GG-EL ( ix<extm> ix<extn> -- r )  IX>N {: nn:n :}  IX>N {: mm:n :}
   0.0  #K 0 ?do
      mm >#M IXT@  i >#K  AT@         \ A[ix[m], k]
      nn >#N       i >#K  BT@         \ B[n, k]
      f* f+
   loop ;

: GGEMM ( -- )
   #M 0 ?do  #N 0 ?do
      j >#M i >#N GG-EL
      j >#M i >#N OT!
   loop loop ;

\ A[m,k] = m*10+k ; B[n,k] = n+k ; ix = [3,0,2,1] (a non-identity permutation)
: FILL ( -- )
   #M 0 ?do #K 0 ?do  i j 10 * + s>f  PA j #K * i + T-SET  loop loop
   #N 0 ?do #K 0 ?do  i j + s>f       PB j #K * i + T-SET  loop loop
   3 PI 0 T-AT !  0 PI 1 T-AT !  2 PI 2 T-AT !  1 PI 3 T-AT ! ;

\ plain-buffer reference (raw indexing) for the same math
: REF-EL ( n n -- r ) {: m:n nn:n :}
   0.0 #K 0 ?do  PA PI m T-AT @ #K * i + T-GET  PB nn #K * i + T-GET  f* f+  loop ;
: REF ( -- )
   #M 0 ?do #N 0 ?do  j i REF-EL  PR j #N * i + T-SET  loop loop ;

FILL  PA AT-BIND  PB BT-BIND  PO OT-BIND  PI IXT-BIND
GGEMM  REF

\ --- numeric equivalence: extent-typed GGEMM == plain-buffer reference (exact) ---
PO PR #M #N * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=

\ --- author-time rejects (the design's whole point): -1 accept, 0 reject ---------
s" GP ( -- r ) 1 >#M 1 >#K AT@ "    CHECK-QUIET-CANDIDATE! -1 T=   \ control: correct order
s" GF ( -- r ) 1 >#K 1 >#M AT@ "    CHECK-QUIET-CANDIDATE!  0 T=   \ flipped index pair
s" GW ( -- r ) 1 >#M 1 >#M AT@ "    CHECK-QUIET-CANDIDATE!  0 T=   \ ix<#M> where ix<#K> demanded
s" GR ( -- r ) 1 1 AT@ "            CHECK-QUIET-CANDIDATE!  0 T=   \ raw n into a data accessor
s" GG ( -- ix<extm> ) 1 IXT@ "      CHECK-QUIET-CANDIDATE!  0 T=   \ raw n into the gather accessor

\ --- runtime range guard on every crossing into an index type (E-EXT-RANGE) ------
\ In-range crossings roundtrip; the injector is the single guarded choke point.
0 >#K IX>N 0 T=                     \ low bound in range
#K 1 - >#K IX>N #K 1 - T=          \ high bound (#K-1) in range
: TRY-HI ( -- ) #K >#K drop ;      \ n == extent -> out of [0,#K)
: TRY-LO ( -- ) -1 >#M drop ;      \ n < 0 -> out of range
' TRY-HI E-EXT-RANGE TTHROWS
' TRY-LO E-EXT-RANGE TTHROWS
\ gather guards a corrupt index buffer: an out-of-range stored row index is caught
\ at the codomain injector before it can walk into a data accessor's address math.
: TRY-GATHER ( -- ) #M PI 0 T-AT !  0 >#M IXT@ drop ;   \ PI[0] := #M (>= codomain #M)
' TRY-GATHER E-EXT-RANGE TTHROWS
3 PI 0 T-AT !                       \ restore the original valid permutation entry

\ --- (b) the tensor kind is a real sum type: kind-vs-rank confusion is a type
\ mismatch, and every branch on the kind must be exhaustive. -1 accept, 0 reject.
s" TKR   ( tr-slot -- n ) TR-RANK@ "            CHECK-QUIET-CANDIDATE! -1 T=   \ rank column is a plain n
s" TKK   ( tr-slot -- n ) TR-KIND@ "            CHECK-QUIET-CANDIDATE!  0 T=   \ a kind is not an n
s" TKP   ( tr-slot -- bool ) TR-KIND@ TR-KIND-DATA? "  CHECK-QUIET-CANDIDATE! -1 T=   \ branch on the kind
s" TKEX  ( tensor-kind -- ) MATCH tensor-kind data OF ENDOF gather OF ENDOF ;MATCH "  CHECK-QUIET-CANDIDATE! -1 T=
s" TKNE  ( tensor-kind -- ) MATCH tensor-kind data OF ENDOF ;MATCH "  CHECK-QUIET-CANDIDATE!  0 T=   \ missing gather arm

\ --- (c) the tensor-registry slot index is its own type: a raw n cannot address a
\ registry row without the explicit `>TR-SLOT` crossing.
s" TSOK  ( tr-slot -- n ) TR-RANK@ "            CHECK-QUIET-CANDIDATE! -1 T=
s" TSRAW ( n -- n ) TR-RANK@ "                  CHECK-QUIET-CANDIDATE!  0 T=   \ raw n as a slot: reject
s" TSMK  ( n -- n ) >TR-SLOT TR-RANK@ "         CHECK-QUIET-CANDIDATE! -1 T=   \ explicit crossing restores it

\ --- (d) rank-0 accessors (dot habu-rank-0-tensor): TENSOR: NAME ( ) emits a
\ zero-index read/write pair over a 1-element span. TG-OFFSET seeded the Horner fold
\ with the local x0 that TG-PROJ never binds for rank 0, so TENSOR: died E-UNDEFINED:
\ x0; the offset is now the constant 0 and the single element round-trips at offset 0.
TENSOR: SC0 ( )                    \ SC0@ ( -- r )   SC0! ( r -- )
create PS0 1 cells allot
PS0 SC0-BIND
42.0 SC0!                          \ write through the rank-0 accessor
PS0 0 T-GET f>s 42 T=              \ landed at the 1-element span's offset 0 ...
SC0@ f>s 42 T=                     \ ... and reads back through the accessor
-3.0 SC0!  SC0@ f>s -3 T=          \ overwrite round-trips

;package

T-REPORT
