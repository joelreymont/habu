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

\ --- (b) the tensor kind is a real declared type: kind-vs-rank confusion is a type
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

\ --- (e) tensor-registry capacity (dot habu-raise-or-right-6ee33f69): the registry
\ is a fixed-cap, library-load-time table; a registration past TR-CAP fails closed
\ with the named E-EXT-CAP die. TR-OVERFILL registers TR-CAP+1 rows to cross the
\ boundary, symbolic in TR-CAP so it tracks the value (like model-ir's TRY-MIR-CAP).
\ TR-N is saved/restored around the probe: the registry has no reset and this is the
\ shared cold-gate image, so the rows the probe burns must not starve later suites.
: TR-OVERFILL ( -- )  TR-CAP 1+ 0 ?do  s" TRCAPX" 2 KIND-DATA TR-ADD  loop ;
TR-N @  ' TR-OVERFILL E-EXT-CAP TTHROWS  TR-N !
TR-CAP 256 T=                       \ the raised cap this regression pins

\ --- (f) how the kind is DECLARED (dot habu-migrate-extent-tensor-a1be181d).
\ `tensor-kind` carries no payload in either case, so it is written in the compact
\ enum form (`ENUM tensor-kind data gather ;ENUM`). The type registry therefore
\ records it as an enum family and no longer as a general sum. Both spellings are
\ one cell wide and both give the same MATCH surface, so no consumer can tell them
\ apart by behaviour - which is precisely why the recorded kind is pinned here,
\ read live out of the family registry through the read-only accessors the checker
\ publishes for exactly this kind of tool (src/core/checker.f, "Public-signature
\ metadata"). Writing the declaration back as `SUMTYPE tensor-kind 0 ...`, or as
\ the arity-headed full enum form, changes the recorded kind and turns this suite
\ red. The same probe pins the generated constructor package spelling that
\ KIND-DATA and KIND-GATHER compile against, so a constructor rename cannot pass
\ unnoticed either.
variable KP-FAM                     \ tensor-kind's row in the live family registry
variable KP-VAR                     \ its first case's row in the variant registry
: KP-NAMED? ( n ptr u8 n -- bool ) {: id:n a:ptr u:n :}  id TFAM-NAME$ a u STR= ;
: KP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}   \ family row for tail `a u`, or -1
   TFAM-N@ 0 ?do  i a u KP-NAMED? if i unloop exit then  loop  -1 ;

s" tensor-kind" KP-FIND KP-FAM !
KP-FAM @ 0 < 0 T=                   \ the family is registered ...
KP-FAM @ TFAM-KIND@ TK-ENUM T=      \ ... as an enum family (the pinned ruling) ...
KP-FAM @ TFAM-KIND@ TK-SUM = 0 T=   \ ... and not as a general sum
KP-FAM @ TFAM-ARITY@ 0 T=           \ compact form declares no type parameters
KP-FAM @ TFAM-WIDTH@ 1 T=           \ one cell, the same width the sum form had
KP-FAM @ TFAM-PUBLIC? -1 T=         \ public, so the constructors are generated
KP-FAM @ TFAM-VAR-COUNT@ 2 T=
KP-FAM @ TFAM-VAR-START@ KP-VAR !
KP-VAR @     SUMV-NAME$ s" data" T$=                     \ case order fixes the tags
KP-VAR @ 1 + SUMV-NAME$ s" gather" T$=
KP-VAR @     SUMV-CTOR-PKG$ s" MAKI-TENSOR--KIND" T$=    \ constructor spelling
KP-VAR @ 1 + SUMV-CTOR-PKG$ s" MAKI-TENSOR--KIND" T$=

\ both cases round-trip: constructor -> MATCH projection -> rebuild -> the
\ MATCH-based predicates. No step reads a bare tag off the stack.
KIND-DATA   KIND>TAG 0 T=
KIND-GATHER KIND>TAG 1 T=
KIND-DATA   TR-KIND-DATA?   -1 T=
KIND-DATA   TR-KIND-GATHER?  0 T=
KIND-GATHER TR-KIND-GATHER? -1 T=
KIND-GATHER TR-KIND-DATA?    0 T=
0 TAG>KIND KIND>TAG 0 T=
1 TAG>KIND KIND>TAG 1 T=

\ the generated constructors are nullary and yield the kind, never a bare cell.
s" TKCD ( -- tensor-kind ) MAKI-TENSOR--KIND:DATA "    CHECK-QUIET-CANDIDATE! -1 T=
s" TKCG ( -- tensor-kind ) MAKI-TENSOR--KIND:GATHER "  CHECK-QUIET-CANDIDATE! -1 T=
s" TKCN ( -- n ) MAKI-TENSOR--KIND:DATA "              CHECK-QUIET-CANDIDATE!  0 T=

\ forging a kind out of a bare cell stays a reject at both entry points.
s" TKFP ( tensor-kind -- bool ) TR-KIND-DATA? "  CHECK-QUIET-CANDIDATE! -1 T=
s" TKFN ( n -- bool ) TR-KIND-DATA? "            CHECK-QUIET-CANDIDATE!  0 T=
s" TKFM ( n -- ) MATCH tensor-kind data OF ENDOF gather OF ENDOF ;MATCH "  CHECK-QUIET-CANDIDATE! 0 T=

\ identity is by name, not by shape: a second payloadless family with the same two
\ case names and the same one-cell width is a DIFFERENT type in both directions.
\ Declared private here, so it publishes no constructors of its own.
ENUM tkother data gather ;ENUM
s" TKO  ( tkother -- ) MATCH tkother data OF ENDOF gather OF ENDOF ;MATCH "     CHECK-QUIET-CANDIDATE! -1 T=
s" TKOX ( tkother -- bool ) TR-KIND-DATA? "                                     CHECK-QUIET-CANDIDATE!  0 T=
s" TKOM ( tkother -- ) MATCH tensor-kind data OF ENDOF gather OF ENDOF ;MATCH " CHECK-QUIET-CANDIDATE!  0 T=
s" TKOK ( tensor-kind -- ) MATCH tkother data OF ENDOF gather OF ENDOF ;MATCH " CHECK-QUIET-CANDIDATE!  0 T=

;package

T-REPORT
