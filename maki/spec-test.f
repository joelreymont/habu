\ maki/spec-test.f - checked tests for SPEC: (maki/spec.f).
\
\ The acceptance vehicle is the gathered GEMM golden, now written as ONE SPEC: line:
\   O[p q] = sum_r A[ix[p],r] * B[q,r]   (A: PxR, B: QxR, ix: P)
\ SPEC: derives the checked candidate-B golden (SGEMM-EL + SGEMM), which is run and
\ proven numerically equal to a plain-buffer reference. The other derivations are
\ asserted directly: (2) the dataflow record (free / contraction indices, per-factor
\ index structure incl. gather) via the SPEC-* query words, and (3) the PROMOTE shape
\ obligations (output-shape extents + contraction extents) via SPEC-*-EXTENT@. The
\ negatives are the point: every malformed spec is a NAMED throw (SPEC-CHECK$), and a
\ valid-but-transposed spec is a CHECKER reject of the derived accessor loop
\ (SPEC-CAND$). Uses #P/#Q/#R (own extents; no other suite declares them).

require lib/test.f
require test/checker-assert.f
require maki/spec.f
require maki/gradcheck.f              \ GC-CLOSE?: the central-FD closeness idiom the adjoint gradcheck reuses

T-RESET

package MAKI

4 EXTENT: #P   3 EXTENT: #Q   2 EXTENT: #R
TENSOR:  A ( #P #R )         \ A : P x R
TENSOR:  B ( #Q #R )         \ B : Q x R
TENSOR:  O ( #P #Q )         \ O : P x Q
ITENSOR: IX ( #P #P )        \ ix : gather P positions into A's P-row space

SPEC: SGEMM  O[p q] = A[ IX[p] r ] B[q r] * +SUM r ;

\ --- derivation (2): the dataflow record parsed from the schematic ---------------
SPEC-NAME$ s" SGEMM" STR= -1 T=
SPEC-OUT$  s" O" STR= -1 T=
SPEC-FREE-N 2 T=   0 SPEC-FREE@ s" p" STR= -1 T=   1 SPEC-FREE@ s" q" STR= -1 T=
SPEC-CT-N   1 T=   0 SPEC-CT@   s" r" STR= -1 T=
SPEC-FAC-N  2 T=
0 SPEC-FAC-NAME@ s" A" STR= -1 T=   1 SPEC-FAC-NAME@ s" B" STR= -1 T=
0 SPEC-FAC-RANK@ 2 T=                                 \ A[ix[p] r] has two indices
0 0 SPEC-FAC-IDX@ s" p" STR= -1 T=                    \ A's row index var is p ...
0 0 SPEC-FAC-GATHER@ s" IX" STR= -1 T=                \ ... gathered through IX
0 1 SPEC-FAC-IDX@ s" r" STR= -1 T=                    \ A's col index is a plain r
0 1 SPEC-FAC-GATHER@ nip 0= -1 T=
1 0 SPEC-FAC-IDX@ s" q" STR= -1 T=   1 1 SPEC-FAC-IDX@ s" r" STR= -1 T=

\ --- derivation (3): the PROMOTE shape obligations (extent magnitudes) ------------
0 SPEC-FREE-EXTENT@ 4 T=   1 SPEC-FREE-EXTENT@ 3 T=    \ output shape 4 x 3
0 SPEC-CT-EXTENT@ 2 T=                                 \ contraction span 2

\ --- derivation (1): the generated golden matches the plain-buffer reference -----
create QA #P #R * cells allot
create QB #Q #R * cells allot
create QO #P #Q * cells allot
create QR #P #Q * cells allot
create QI #P cells allot
: SGFILL ( -- )
   #P 0 ?do #R 0 ?do  i j 10 * + s>f  QA j #R * i + T-SET  loop loop
   #Q 0 ?do #R 0 ?do  i j + s>f       QB j #R * i + T-SET  loop loop
   3 QI 0 T-AT !  0 QI 1 T-AT !  2 QI 2 T-AT !  1 QI 3 T-AT ! ;
: SGREFEL ( n n -- r ) {: p:n qq:n :}
   0.0 #R 0 ?do  QA QI p T-AT @ #R * i + T-GET  QB qq #R * i + T-GET  f* f+  loop ;
: SGREF ( -- ) #P 0 ?do #Q 0 ?do  j i SGREFEL  QR j #Q * i + T-SET  loop loop ;
SGFILL  QA A-BIND  QB B-BIND  QO O-BIND  QI IX-BIND
SGEMM  SGREF
QO QR #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=      \ exact numeric equality

\ --- the equation registry executor path (docs/model-unified.md stage 1) ----------
\ A COMPOSABLE (gather-free) einsum joins the registry, so SPEC: also generates its
\ <NAME>-RUN word and captures its xt. Running it through EQ-EXEC (fetch the RUN xt by
\ registry slot and execute it) must reproduce the plain-buffer GEMM. This is the named
\ test for the EQ-EXEC TRUSTED boundary (dot habu-typed-xt-cell-4c8ecc4c) and the only
\ path that exercises EQ-GEN-RUN / EQ-ARG-SET! / EQ-OUT-SET! / EQ-FIND / EQ-EXEC (SGEMM
\ above carries a gather, so it is not composable and never registers). PGEMM reuses the
\ A/B/O tensors and the QA/QB buffers SGFILL already filled; the reference is ungathered.
create QPO #P #Q * cells allot
create QPR #P #Q * cells allot
: PGREFEL ( n n -- r ) {: p:n qq:n :}
   0.0 #R 0 ?do  QA p #R * i + T-GET  QB qq #R * i + T-GET  f* f+  loop ;
: PGREF ( -- ) #P 0 ?do #Q 0 ?do  j i PGREFEL  QPR j #Q * i + T-SET  loop loop ;
: PGEMM-OK? ( -- bool )   \ the equation registered as a composable op
   s" PGEMM" EQ-FIND MATCH option  none OF false ENDOF  some OF drop true ENDOF ;MATCH ;
: PGEMM-EXEC ( -- )       \ look the equation up by name and run it through the registry
   s" PGEMM" EQ-FIND MATCH option  none OF E-SPEC-SYNTAX throw ENDOF  some OF EQ-EXEC ENDOF ;MATCH ;
SPEC: PGEMM  O[p q] = A[p r] B[q r] * +SUM r ;
PGREF
PGEMM-OK? -1 T=                                       \ composable: it joined the registry
QA 0 EQ-ARG-SET!   QB 1 EQ-ARG-SET!   QPO EQ-OUT-SET!  \ stage operand + output buffers
PGEMM-EXEC                                             \ EQ-EXEC runs the generated RUN xt
QPO QPR #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=    \ EQ-EXEC output == plain GEMM

\ --- Unicode math spellings: each confusable pair normalizes to ONE token, so every
\ spelling registers an equation whose EQ-EXEC output equals the ASCII PGEMM golden.
\ Summation Σ (U+03A3) / ∑ (U+2211) both lex to +SUM; product · (U+00B7) / ⋅ (U+22C5)
\ both lex to *. PGU1 is the docs/golden-syntax.md pitch line written gather-free with
\ real Σ and · bytes (prefix Σ + infix ·); PGU3 shows the ASCII prefix +SUM the
\ normalization implies. All must reproduce the plain-buffer GEMM (QPR from PGREF). ----
create QU1 #P #Q * cells allot
create QU2 #P #Q * cells allot
create QU3 #P #Q * cells allot
: UEQ-EXEC ( ptr u8 n -- )   \ find an equation by name and run it through the registry
   EQ-FIND MATCH option  none OF E-SPEC-SYNTAX throw ENDOF  some OF EQ-EXEC ENDOF ;MATCH ;
SPEC: PGU1  O[p q] = Σr A[p r] · B[q r] ;      \ prefix Σ U+03A3, infix · U+00B7 (the pitch line)
SPEC: PGU2  O[p q] = ∑r A[p r] ⋅ B[q r] ;      \ prefix ∑ U+2211, infix ⋅ U+22C5
SPEC: PGU3  O[p q] = +SUM r A[p r] * B[q r] ;  \ ASCII prefix +SUM (Σ normalizes to +SUM)
QA 0 EQ-ARG-SET!  QB 1 EQ-ARG-SET!  QU1 EQ-OUT-SET!  s" PGU1" UEQ-EXEC
QA 0 EQ-ARG-SET!  QB 1 EQ-ARG-SET!  QU2 EQ-OUT-SET!  s" PGU2" UEQ-EXEC
QA 0 EQ-ARG-SET!  QB 1 EQ-ARG-SET!  QU3 EQ-OUT-SET!  s" PGU3" UEQ-EXEC
QU1 QPR #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=    \ U+03A3 / U+00B7 == plain GEMM
QU2 QPR #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=    \ U+2211 / U+22C5 == plain GEMM
QU3 QPR #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=    \ ASCII prefix +SUM == plain GEMM
QU1 QU2 #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=    \ the two spellings of each pair are identical

\ a stray OTHER non-ASCII byte is the named E-SPEC-SYNTAX reject whose diagnostic names
\ the offending codepoint (− is U+2212 MINUS SIGN, a lookalike outside the confusable set).
: T-UNICP ( -- ) s" O[p q] = Σr A[p r] − B[q r]" SPEC-CHECK$ ;
' T-UNICP  E-SPEC-SYNTAX  TTHROWS
s" U+2212" SPEC-REJECT$ 2swap CONTAINS? -1 T=

\ --- negatives: every malformed spec is a named throw (no code generated) ---------
: T-UNKEXT  ( -- ) s" O[z q] = A[ IX[z] r ] B[q r] * +SUM r" SPEC-CHECK$ ;   \ z -> #Z undeclared
: T-TENSOR  ( -- ) s" O[p q] = Z[ IX[p] r ] B[q r] * +SUM r" SPEC-CHECK$ ;   \ tensor Z undeclared
: T-ARITY   ( -- ) s" O[p q] = A[ IX[p] ] B[q r] * +SUM r" SPEC-CHECK$ ;      \ A rank 2, one index
: T-SYNTAX  ( -- ) s" O[p q] A[ IX[p] r ] B[q r] * +SUM r" SPEC-CHECK$ ;      \ missing =
: T-OK      ( -- ) s" O[p q] = A[ IX[p] r ] B[q r] * +SUM r" SPEC-CHECK$ ;     \ valid: no throw
' T-UNKEXT  E-SPEC-EXTENT  TTHROWS
' T-TENSOR  E-SPEC-TENSOR  TTHROWS
' T-ARITY   E-SPEC-ARITY   TTHROWS
' T-SYNTAX  E-SPEC-SYNTAX  TTHROWS
' T-OK      0              TTHROWS

\ unbound index needs a DECLARED extent that is neither free nor contracted, so the
\ extent check passes and the bound check fires. Declare #S and index it in a factor.
2 EXTENT: #S
: T-UNBOUND2 ( -- ) s" O[p q] = A[ IX[p] s ] B[q r] * +SUM r" SPEC-CHECK$ ;   \ s: #S declared, not free/contracted
' T-UNBOUND2 E-SPEC-UNBOUND TTHROWS

\ --- two-index prefix contraction (the surface's max): the prefix Σ index list carries
\ BOTH contraction indices (Σr s). Such an equation must register and EQ-EXEC identically
\ to its ASCII trailing "+SUM r s" spelling. Reuse A (#P #R) for the p-side and add TWC
\ (#Q #S) for the q-side so the two contraction indices r,s are DISTINCT declared extents.
\ The einsum O[p q] = Σr,s A[p r]·TWC[q s] is the outer product of A's and TWC's row-sums;
\ both spellings must reproduce that plain-buffer reference (Q2R) and each other. --------
TENSOR: TWC ( #Q #S )                  \ TWC : Q x S, the q-side operand
create QC  #Q #S * cells allot
create Q2P #P #Q * cells allot         \ prefix-Σ output
create Q2T #P #Q * cells allot         \ trailing-+SUM output
create Q2R #P #Q * cells allot         \ plain-buffer reference
: TW2FILL ( -- ) #Q 0 ?do #S 0 ?do  i j 2 * + 1+ s>f  QC j #S * i + T-SET  loop loop ;
: TW2A ( n -- r ) {: p:n :}   0.0 #R 0 ?do  QA p  #R * i + T-GET  f+  loop ;   \ Σ_r A[p r]
: TW2C ( n -- r ) {: qq:n :}  0.0 #S 0 ?do  QC qq #S * i + T-GET  f+  loop ;   \ Σ_s TWC[q s]
: TW2REF ( -- ) #P 0 ?do #Q 0 ?do  j TW2A  i TW2C  f*  Q2R j #Q * i + T-SET  loop loop ;
TW2FILL  QA A-BIND  QC TWC-BIND  TW2REF
SPEC: TW2P  O[p q] = Σr s A[p r] · TWC[q s] ;      \ prefix Σ carries BOTH contraction indices
SPEC: TW2T  O[p q] = A[p r] TWC[q s] * +SUM r s ;  \ ASCII trailing +SUM r s (same einsum)
QA 0 EQ-ARG-SET!  QC 1 EQ-ARG-SET!  Q2P EQ-OUT-SET!  s" TW2P" UEQ-EXEC
QA 0 EQ-ARG-SET!  QC 1 EQ-ARG-SET!  Q2T EQ-OUT-SET!  s" TW2T" UEQ-EXEC
Q2P Q2R #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=   \ prefix Σr s == outer product of row-sums
Q2T Q2R #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=   \ trailing +SUM r s == same reference
Q2P Q2T #P #Q * T-DIST2  1000.0 f* 0.5 f+ f>s  0 T=   \ the two 2-index spellings are identical

\ mixed reduction with the 2-index form: a prefix Σ AND a trailing +SUM double-specify the
\ reduction, which SP-PARSE-REDUCTION rejects (named E-SPEC-SYNTAX) before any codegen.
: T-MIX2 ( -- ) s" O[p q] = Σr s A[p r] TWC[q s] * +SUM r s" SPEC-CHECK$ ;
' T-MIX2  E-SPEC-SYNTAX  TTHROWS

\ --- a valid-but-transposed spec: the derived accessor loop flips extents, so the
\ checker rejects it (CHECK-QUIET-CANDIDATE! = 0); the correct order is accepted (-1).
SPEC-CAND: GGOK  O[p q] = A[ IX[p] r ] B[q r] * +SUM r ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: GGBAD O[p q] = A[ r IX[p] ] B[q r] * +SUM r ;      \ A[r ix[p]]: ix<extr> in A's row slot
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=

\ --- (a) the index-variable and tensor lookups now return option<>; a MATCH that
\ forgets the none arm fails certification - the exhaustiveness every SP-*-POS /
\ SP-EXT-SLOT / TR-FIND consumer here now relies on. -1 accept, 0 reject.
s" SAOK ( option<n> -- ) MATCH option none OF ENDOF some OF drop ENDOF ;MATCH "  CHECK-QUIET-CANDIDATE! -1 T=
s" SANE ( option<n> -- ) MATCH option some OF drop ENDOF ;MATCH "                CHECK-QUIET-CANDIDATE!  0 T=

\ --- (c) the factor-index (SP-FI-*) table index is its own type: a raw n cannot
\ address the table without the explicit >SP-FI crossing.
s" SFIOK  ( sp-fi -- ptr u8 n ) SP-FI-VAR@ "  CHECK-QUIET-CANDIDATE! -1 T=
s" SFIRAW ( n -- ptr u8 n ) SP-FI-VAR@ "      CHECK-QUIET-CANDIDATE!  0 T=

\ ==================================================================================
\ broadcast / elementwise forms (dot habu-spec-broadcast-forms-ad851424).
\ An output-shaped expression with elementwise + (add) or · (mul) and NO contraction:
\ a factor whose index list is a SUFFIX of the output's broadcasts the missing LEADING
\ axes. The three shape classes maki/cad.f SHP-LEGAL? legalizes map to suffix lengths:
\   same-shape (residual/add/mul) : rank == output  -> BXA[bm bn]
\   row broadcast 1xC (bias add)  : rank == output-1 -> BVEC[bn]   (SHP-ROW-OK?)
\   scalar 1x1 (scale)            : rank 0 (empty)   -> unauthorable (see the wall below)
\ Canonical spelling: infix · (U+00B7) for mul (ASCII * stays legal); + for elementwise
\ add (ASCII only - addition has no blessed confusable, so any non-ASCII + is the named
\ E-SPEC-SYNTAX reject via the existing lexer). Proven like the einsum: (1) generated
\ golden == plain-buffer reference, (2) the dataflow record (free-only, no contraction;
\ a suffix factor), (3) PROMOTE extents, named-throw negatives, a checker reject of a
\ wrong-extent broadcast, and a central-FD gradcheck of the derived adjoints.
3 EXTENT: #BM   2 EXTENT: #BN
TENSOR: BXA   ( #BM #BN )      \ same-shape operand
TENSOR: BXB   ( #BM #BN )      \ same-shape operand
TENSOR: BVEC  ( #BN )          \ row-broadcast bias vector (1xC): indexed by the trailing free var
TENSOR: BXO   ( #BM #BN )      \ output
TENSOR: BCOL  ( #BM )          \ a column vector: [bm] is a PREFIX (Rx1), an illegal broadcast
TENSOR: BWRONG ( #BM )         \ wrong-extent operand used at the bn slot -> checker extent reject
TENSOR: BSCL  ( )              \ rank-0 scalar operand (1x1 scale); dot habu-rank-0-tensor

SPEC: BC-BIAS   BXO[bm bn] = BXA[bm bn] + BVEC[bn] ;        \ row-broadcast bias add (1xC)
\ --- (2) dataflow record: two free indices, NO contraction axis; the bias is a rank-1 suffix
SPEC-NAME$ s" BC-BIAS" STR= -1 T=
SPEC-OUT$  s" BXO" STR= -1 T=
SPEC-FREE-N 2 T=   0 SPEC-FREE@ s" bm" STR= -1 T=   1 SPEC-FREE@ s" bn" STR= -1 T=
SPEC-CT-N   0 T=                                       \ elementwise: no contraction index
SPEC-FAC-N  2 T=
0 SPEC-FAC-NAME@ s" BXA" STR= -1 T=   1 SPEC-FAC-NAME@ s" BVEC" STR= -1 T=
0 SPEC-FAC-RANK@ 2 T=   1 SPEC-FAC-RANK@ 1 T=          \ BVEC is rank 1 (a proper suffix)
1 0 SPEC-FAC-IDX@ s" bn" STR= -1 T=                    \ ...indexed by the trailing free var bn
\ --- (3) PROMOTE shape obligations (extent magnitudes) ---
0 SPEC-FREE-EXTENT@ 3 T=   1 SPEC-FREE-EXTENT@ 2 T=

SPEC: BC-RESID  BXO[bm bn] = BXA[bm bn] + BXB[bm bn] ;      \ same-shape add (residual)
SPEC: BC-HAD    BXO[bm bn] = BXA[bm bn] · BXB[bm bn] ;      \ same-shape mul (hadamard), canonical ·
SPEC-CT-N 0 T=                                         \ the mul form is also contraction-free

\ --- (1) forward parity: each generated golden == its plain-buffer reference -----
create BCA  #BM #BN * cells allot
create BCB  #BM #BN * cells allot
create BCV  #BN cells allot
create BCO  #BM #BN * cells allot
create BCR  #BM #BN * cells allot
create BCDO #BM #BN * cells allot        \ seed cotangent (varied, non-uniform)
create BCDA #BM #BN * cells allot         \ analytic dBXA
create BCDB #BM #BN * cells allot         \ analytic dBXB
create BCDV #BN cells allot               \ analytic dBVEC (column-sum)
: BC-FILL ( -- )
   #BM #BN * 0 ?do  i 1 + s>f 3.0 f/  BCA i T-SET  loop
   #BM #BN * 0 ?do  i 2 * 1 + s>f 5.0 f/  BCB i T-SET  loop
   #BN 0 ?do  i 1 + s>f 7.0 f/  BCV i T-SET  loop
   #BM #BN * 0 ?do  i 7 mod s>f 0.13 f* 0.6 f+  BCDO i T-SET  loop ;
: BC-BIND-BIAS  ( -- )  BCA BXA-BIND  BCV BVEC-BIND  BCO BXO-BIND ;
: BC-BIND-RESID ( -- )  BCA BXA-BIND  BCB BXB-BIND   BCO BXO-BIND ;
: BC-BIND-HAD   ( -- )  BCA BXA-BIND  BCB BXB-BIND   BCO BXO-BIND ;
: BC-REF-BIAS   ( -- )  #BM 0 ?do #BN 0 ?do  BCA j #BN * i + T-GET  BCV i T-GET  f+  BCR j #BN * i + T-SET  loop loop ;
: BC-REF-RESID  ( -- )  #BM #BN * 0 ?do  BCA i T-GET  BCB i T-GET  f+  BCR i T-SET  loop ;
: BC-REF-HAD    ( -- )  #BM #BN * 0 ?do  BCA i T-GET  BCB i T-GET  f*  BCR i T-SET  loop ;
: BC-DIST0? ( -- bool )  BCO BCR #BM #BN * T-DIST2  1000000.0 f* 0.5 f+ f>s 0= ;
BC-FILL
BC-BIND-BIAS   BC-BIAS   BC-REF-BIAS   BC-DIST0? -1 T=      \ bias add == reference
BC-BIND-RESID  BC-RESID  BC-REF-RESID  BC-DIST0? -1 T=      \ residual add == reference
BC-BIND-HAD    BC-HAD    BC-REF-HAD    BC-DIST0? -1 T=      \ hadamard mul == reference

\ --- adjoint gradcheck: each derived <NAME>-ADJj word vs a central finite difference of
\ L = sum(seed * O), compared with maki/gradcheck.f GC-CLOSE?. dBIAS/dRESID copy the
\ cotangent; the bias's dBVEC is the column-sum; the hadamard applies the product rule.
: BC-LOSS ( -- r )  0.0 #BM #BN * 0 ?do  BCDO i T-GET  BCO i T-GET  f*  f+  loop ;
: BC-H ( -- r )  0.001 ;
: BC-FD-BIAS  ( ptr a n -- r ) {: p:ptr e:n :}
   p e T-GET {: base:r :}
   base BC-H f+ p e T-SET  BC-BIND-BIAS BC-BIAS BC-LOSS {: yp:r :}
   base BC-H f- p e T-SET  BC-BIND-BIAS BC-BIAS BC-LOSS {: ym:r :}
   base p e T-SET  yp ym f- BC-H 2.0 f* f/ ;
: BC-FD-RESID ( ptr a n -- r ) {: p:ptr e:n :}
   p e T-GET {: base:r :}
   base BC-H f+ p e T-SET  BC-BIND-RESID BC-RESID BC-LOSS {: yp:r :}
   base BC-H f- p e T-SET  BC-BIND-RESID BC-RESID BC-LOSS {: ym:r :}
   base p e T-SET  yp ym f- BC-H 2.0 f* f/ ;
: BC-FD-HAD   ( ptr a n -- r ) {: p:ptr e:n :}
   p e T-GET {: base:r :}
   base BC-H f+ p e T-SET  BC-BIND-HAD BC-HAD BC-LOSS {: yp:r :}
   base BC-H f- p e T-SET  BC-BIND-HAD BC-HAD BC-LOSS {: ym:r :}
   base p e T-SET  yp ym f- BC-H 2.0 f* f/ ;
: BC-GC-BIAS ( -- bool )
   BC-FILL
   BCDO BXO-BIND BCDA BXA-BIND  BC-BIAS-ADJ0                \ dBXA = dO (copy)
   BCDO BXO-BIND BCDV BVEC-BIND BC-BIAS-ADJ1                \ dBVEC[bn] = sum_bm dO
   BC-BIND-BIAS BC-BIAS  true
   #BM #BN * 0 ?do  BCDA i T-GET  BCA i BC-FD-BIAS  GC-CLOSE? 0= if drop false leave then  loop
   dup if  #BN 0 ?do  BCDV i T-GET  BCV i BC-FD-BIAS  GC-CLOSE? 0= if drop false leave then  loop  then ;
: BC-GC-RESID ( -- bool )
   BC-FILL
   BCDO BXO-BIND BCDA BXA-BIND  BC-RESID-ADJ0               \ dBXA = dO
   BCDO BXO-BIND BCDB BXB-BIND  BC-RESID-ADJ1               \ dBXB = dO
   BC-BIND-RESID BC-RESID  true
   #BM #BN * 0 ?do  BCDA i T-GET  BCA i BC-FD-RESID  GC-CLOSE? 0= if drop false leave then  loop
   dup if  #BM #BN * 0 ?do  BCDB i T-GET  BCB i BC-FD-RESID  GC-CLOSE? 0= if drop false leave then  loop  then ;
: BC-GC-HAD ( -- bool )
   BC-FILL
   BCDO BXO-BIND BCB BXB-BIND BCDA BXA-BIND  BC-HAD-ADJ0    \ dBXA = dO * BXB
   BCDO BXO-BIND BCA BXA-BIND BCDB BXB-BIND  BC-HAD-ADJ1    \ dBXB = dO * BXA
   BC-BIND-HAD BC-HAD  true
   #BM #BN * 0 ?do  BCDA i T-GET  BCA i BC-FD-HAD  GC-CLOSE? 0= if drop false leave then  loop
   dup if  #BM #BN * 0 ?do  BCDB i T-GET  BCB i BC-FD-HAD  GC-CLOSE? 0= if drop false leave then  loop  then ;
BC-GC-BIAS  -1 T=          \ row-broadcast bias adjoints gradcheck
BC-GC-RESID -1 T=          \ same-shape add adjoints gradcheck
BC-GC-HAD   -1 T=          \ same-shape mul (product-rule) adjoints gradcheck

\ --- scalar 1x1 broadcast (dot habu-rank-0-tensor): a rank-0 factor BSCL[] scales (·) or
\ offsets (+) every output element. An empty index list is a suffix of any output, so the
\ forward falls out of the SAME suffix machinery as the bias/residual forms - an ordinary
\ generated golden, matched here vs a plain scale/offset reference (the SHP-SCALE-OK? 1x1
\ class). The full-sum adjoint is a rank-0 OUTPUT (ds = sum dO·A for the scale, ds = sum dO
\ for the add). That rank-0-output einsum is not yet in the SPEC: contraction grammar (the
\ auto-derived adjoint fails closed E-SPEC-ARITY, asserted below), so the analytic adjoint is
\ authored in candidate-B form - writing the scalar through the rank-0 WRITE accessor BSCL! -
\ and central-FD gradchecked, exactly as the composable forms gradcheck their derived words.
create BSC  1 cells allot              \ scalar param buffer (1 element)
create BCDS 1 cells allot              \ analytic dBSCL
: BC-SCL-SET ( -- )  2.5 3.0 f/  BSC 0 T-SET ;         \ a non-trivial scale factor
: BC-BIND-SCALE ( -- )  BCA BXA-BIND  BSC BSCL-BIND  BCO BXO-BIND ;
: BC-BIND-ADDS  ( -- )  BCA BXA-BIND  BSC BSCL-BIND  BCO BXO-BIND ;
SPEC: BC-SCALE  BXO[bm bn] = BXA[bm bn] · BSCL[] ;     \ scalar mul (scale), canonical ·
SPEC: BC-ADDS   BXO[bm bn] = BXA[bm bn] + BSCL[] ;     \ scalar add (offset)
: BC-REF-SCALE ( -- )  #BM #BN * 0 ?do  BCA i T-GET  BSC 0 T-GET  f*  BCR i T-SET  loop ;
: BC-REF-ADDS  ( -- )  #BM #BN * 0 ?do  BCA i T-GET  BSC 0 T-GET  f+  BCR i T-SET  loop ;
BC-FILL BC-SCL-SET  BC-BIND-SCALE  BC-SCALE  BC-REF-SCALE  BC-DIST0? -1 T=   \ scale == reference
BC-FILL BC-SCL-SET  BC-BIND-ADDS   BC-ADDS   BC-REF-ADDS   BC-DIST0? -1 T=   \ offset == reference

\ central-FD references (perturb the single scalar param) - the same L = sum(seed * O) the
\ authored adjoints were checked against; now they check the SPEC:-DERIVED words instead.
: BC-FD-SCALE ( -- r )  \ central FD of L wrt the single scalar param
   BSC 0 T-GET {: base:r :}
   base BC-H f+ BSC 0 T-SET  BC-BIND-SCALE BC-SCALE BC-LOSS {: yp:r :}
   base BC-H f- BSC 0 T-SET  BC-BIND-SCALE BC-SCALE BC-LOSS {: ym:r :}
   base BSC 0 T-SET  yp ym f- BC-H 2.0 f* f/ ;
: BC-FD-ADDS ( -- r )
   BSC 0 T-GET {: base:r :}
   base BC-H f+ BSC 0 T-SET  BC-BIND-ADDS BC-ADDS BC-LOSS {: yp:r :}
   base BC-H f- BSC 0 T-SET  BC-BIND-ADDS BC-ADDS BC-LOSS {: ym:r :}
   base BSC 0 T-SET  yp ym f- BC-H 2.0 f* f/ ;
\ the SPEC: auto-derived adjoints. The scale form's factor adjoints are BC-SCALE-ADJ0
\ (dBXA[bm bn] = dO · BSCL, a rank-0-FACTOR broadcast) and BC-SCALE-ADJ1 (dBSCL[] = Σ dO · BXA,
\ the rank-0-OUTPUT full-sum the dot names). The add form's are the analogous copy / column-sum.
\ Both derive through the SAME parse+emit+register pipeline as every other adjoint now that the
\ equation registry admits rank-0 (dot habu-equation-registry-rank); the authored special case
\ retired. dBSCL is gradchecked against the identical central-FD reference the authored one passed.
: BC-GC-SCALE ( -- bool )
   BC-FILL BC-SCL-SET
   BCDO BXO-BIND  BSC BSCL-BIND  BCDA BXA-BIND   BC-SCALE-ADJ0     \ dBXA = dO * BSCL (rank-0 factor)
   BCDO BXO-BIND  BCA BXA-BIND   BCDS BSCL-BIND  BC-SCALE-ADJ1     \ dBSCL = Σ dO * BXA (rank-0 output)
   BCDS 0 T-GET  BC-FD-SCALE  GC-CLOSE? ;
: BC-GC-ADDS  ( -- bool )
   BC-FILL BC-SCL-SET
   BCDO BXO-BIND  BCDA BXA-BIND   BC-ADDS-ADJ0                     \ dBXA = dO
   BCDO BXO-BIND  BCDS BSCL-BIND  BC-ADDS-ADJ1                     \ dBSCL = Σ dO (rank-0 output)
   BCDS 0 T-GET  BC-FD-ADDS  GC-CLOSE? ;
BC-GC-SCALE -1 T=          \ scalar-scale auto-derived rank-0-output adjoint gradchecks
BC-GC-ADDS  -1 T=          \ scalar-add auto-derived rank-0-output adjoint gradchecks

\ the SPEC: auto-derived adjoint of a scalar form IS that rank-0-output contraction, now admitted
\ through the equation registry, so the training gate ACCEPTS it (was the E-SPEC-ARITY reject the
\ retired authored adjoints stood in for) - proven directly as the string-seam training verdict.
: BC-SCL-ADJ-OK ( -- ) s" BXO[bm bn] = BXA[bm bn] · BSCL[]" SPEC-ADJ-CHECK$ ;
' BC-SCL-ADJ-OK 0 TTHROWS

\ --- full reduction (rank-0 output, dot habu-equation-registry-rank): s[] = Σbm,bn A · B is now
\ authorable - the Frobenius inner product of two same-shape operands. Golden it against a plain
\ summed reference, then gradcheck its DERIVED adjoints dA[bm bn] = ds · B, dB[bm bn] = ds · A
\ (each a rank-0-FACTOR scalar broadcast, the same class the scale forward derives) against a
\ central FD of L = s (seed ds = 1). Reuses BXA/BXB (#BM #BN) and the rank-0 BSCL as the scalar
\ output (no new tensor registry rows); the derived words rebind BSCL per role.
create FRSB 1 cells allot              \ scalar output buffer
create FRSR 1 cells allot              \ plain summed reference
create FRSD 1 cells allot              \ scalar cotangent (seed ds = 1, so L = s)
create FREDA #BM #BN * cells allot     \ derived dA
create FREDB #BM #BN * cells allot     \ derived dB
SPEC: BC-FRED  BSCL[] = BXA[bm bn] · BXB[bm bn] +SUM bm bn ;
: BC-FRED-REF ( -- )  0.0 #BM #BN * 0 ?do  BCA i T-GET  BCB i T-GET  f* f+  loop  FRSR 0 T-SET ;
BC-FILL  BCA BXA-BIND  BCB BXB-BIND  FRSB BSCL-BIND  BC-FRED  BC-FRED-REF
FRSB FRSR 1 T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=      \ full-reduction golden == plain summed reference
: BC-FD-FRED ( ptr a n -- r ) {: p:ptr e:n :}          \ central FD of s wrt element e of buffer p
   p e T-GET {: base:r :}
   base BC-H f+ p e T-SET  BCA BXA-BIND BCB BXB-BIND FRSB BSCL-BIND BC-FRED  FRSB 0 T-GET {: yp:r :}
   base BC-H f- p e T-SET  BCA BXA-BIND BCB BXB-BIND FRSB BSCL-BIND BC-FRED  FRSB 0 T-GET {: ym:r :}
   base p e T-SET  yp ym f- BC-H 2.0 f* f/ ;
: BC-GC-FRED ( -- bool )
   BC-FILL  1.0 FRSD 0 T-SET
   FRSD BSCL-BIND  BCB BXB-BIND  FREDA BXA-BIND  BC-FRED-ADJ0      \ dA = ds * B
   FRSD BSCL-BIND  BCA BXA-BIND  FREDB BXB-BIND  BC-FRED-ADJ1      \ dB = ds * A
   true
   #BM #BN * 0 ?do  FREDA i T-GET  BCA i BC-FD-FRED  GC-CLOSE? 0= if drop false leave then  loop
   dup if  #BM #BN * 0 ?do  FREDB i T-GET  BCB i BC-FD-FRED  GC-CLOSE? 0= if drop false leave then  loop  then ;
BC-GC-FRED -1 T=          \ full-reduction derived adjoints (rank-0-factor scalar broadcasts) gradcheck
: BC-FRED-ADJ-OK ( -- ) s" BSCL[] = BXA[bm bn] · BXB[bm bn] +SUM bm bn" SPEC-ADJ-CHECK$ ;
' BC-FRED-ADJ-OK 0 TTHROWS   \ empty-contraction adjoint (ADJ-CT0-OK?): in-grammar elementwise broadcast

\ --- named-throw negatives (reuse E-SPEC-SYNTAX/ARITY): every malformed form fails closed
: BC-N-MIX     ( -- ) s" BXO[bm bn] = BXA[bm bn] + BXB[bm bn] * BXA[bm bn]" SPEC-CHECK$ ;  \ + and * mixed
: BC-N-PLUSSUM ( -- ) s" BXO[bm bn] = BXA[bm bn] + BXB[bm bn] +SUM bn" SPEC-CHECK$ ;        \ + with a reduction
: BC-N-COL     ( -- ) s" BXO[bm bn] = BXA[bm bn] + BCOL[bm]" SPEC-CHECK$ ;                  \ column (non-suffix) broadcast
: BC-N-NOCOMB  ( -- ) s" BXO[bm bn] = BXA[bm bn] BXB[bm bn]" SPEC-CHECK$ ;                  \ two terms, no combiner
: BC-P-SCLOUT  ( -- ) s" BSCL[] = BXA[bm bn] · BXB[bm bn] +SUM bm bn" SPEC-CHECK$ ;         \ rank-0 (full-reduction) OUTPUT: now legal
: BC-N-OK      ( -- ) s" BXO[bm bn] = BXA[bm bn] + BVEC[bn]" SPEC-CHECK$ ;                  \ valid bias: no throw
' BC-N-MIX     E-SPEC-SYNTAX  TTHROWS
' BC-N-PLUSSUM E-SPEC-SYNTAX  TTHROWS
' BC-N-COL     E-SPEC-ARITY   TTHROWS
' BC-N-NOCOMB  E-SPEC-SYNTAX  TTHROWS
' BC-P-SCLOUT  0              TTHROWS       \ replaces the old "rank-0 output illegal" negative: a full-reduction output (empty free list) is exactly what habu-equation-registry-rank legalizes
' BC-N-OK      0              TTHROWS

\ the narrower reject that survives: a rank-0 GEMM UNDER a batch axis (a per-batch scalar loss,
\ e.g. BFO[bf] = Σbm,bn A[bf bm bn] · B[bf bm bn]) has no GEMM axis for the batched machinery to
\ replicate, so it is stage-2 and fails closed named (E-SPEC-ARITY, never a silent mis-derivation).
\ The reject fires at the batch/GEMM arity check - ahead of any tensor lookup - so it needs only
\ the free extent #BF declared; a rank-0 GEMM with NO batch axis is the legal full reduction above.
2 FREE-EXTENT: #BF
: BC-N-BATCHSCL ( -- ) s" BFO[bf] = BF3[bf bm bn] +SUM bm bn" SPEC-CHECK$ ;
' BC-N-BATCHSCL E-SPEC-ARITY TTHROWS

\ --- a valid-but-wrong-extent broadcast is a CHECKER reject of the generated accessor:
\ BWRONG is declared ( #BM ) but read at the bn slot, so >#BN feeds an ix<extbn> where
\ BWRONG@ demands ix<extbm> - the candidate scores 0; the correct BVEC ( #BN ) scores -1.
SPEC-CAND: BCOK  BXO[bm bn] = BXA[bm bn] + BVEC[bn] ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: BCBAD BXO[bm bn] = BXA[bm bn] + BWRONG[bn] ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=

\ --- the training gate accepts every in-grammar elementwise adjoint (string seam, no code)
: BC-ADJ-OK ( -- ) s" BXO[bm bn] = BXA[bm bn] · BXB[bm bn]" SPEC-ADJ-CHECK$ ;
' BC-ADJ-OK 0 TTHROWS

\ ============================================================================
\ Bounds checks on the public SPEC accessors (dot habu-bounds-check-public).
\ Every public dataflow/shape/registry accessor rejects an out-of-domain index
\ with the named E-SPEC-RANGE BEFORE any addressing or cast. RED-FIRST: on the
\ unfixed base each call reaches raw addressing and RETURNS arbitrary data (the
\ two extent readers instead throw the coincidental E-SPEC-EXTENT off the garbage
\ span) - never E-SPEC-RANGE, which did not exist. So each ' W E-SPEC-RANGE
\ TTHROWS below FAILS on the base and PASSES on the fix.
\ ----------------------------------------------------------------------------
\ Shared setup: re-establish the SGEMM dataflow (SP-FREE-N=2, SP-CT-N=1,
\ SP-FAC-N=2, factor ranks 2/2, SP-FI-N=4) that the later SPEC:/SPEC-CHECK$
\ lines in this file have since clobbered, so the throw under test is the
\ accessor's own, not a stale-state artifact.
: SB-SET ( -- )  s" O[p q] = A[ IX[p] r ] B[q r] * +SUM r" SPEC-CHECK$ ;

\ --- free-index domain (SPEC-FREE@ and its SPEC-FREE-EXTENT@/SPEC-BATCH@ views) ---
: SB-FREE-NEG   ( -- )  SB-SET  -1 SPEC-FREE@ 2drop ;
: SB-FREE-CNT   ( -- )  SB-SET   2 SPEC-FREE@ 2drop ;            \ == live count
: SB-FREE-CNT1  ( -- )  SB-SET   3 SPEC-FREE@ 2drop ;            \ count + 1
: SB-FREE-BIG   ( -- )  SB-SET  99 SPEC-FREE@ 2drop ;
: SB-FREXT-NEG  ( -- )  SB-SET  -1 SPEC-FREE-EXTENT@ drop ;
: SB-FREXT-BIG  ( -- )  SB-SET  99 SPEC-FREE-EXTENT@ drop ;
: SB-BATCH-BIG  ( -- )  SB-SET  99 SPEC-BATCH@ 2drop ;
' SB-FREE-NEG   E-SPEC-RANGE TTHROWS
' SB-FREE-CNT   E-SPEC-RANGE TTHROWS
' SB-FREE-CNT1  E-SPEC-RANGE TTHROWS
' SB-FREE-BIG   E-SPEC-RANGE TTHROWS
' SB-FREXT-NEG  E-SPEC-RANGE TTHROWS
' SB-FREXT-BIG  E-SPEC-RANGE TTHROWS
' SB-BATCH-BIG  E-SPEC-RANGE TTHROWS
SB-SET  1 SPEC-FREE@ s" q" STR= -1 T=                            \ last valid index still returns (guard is >=, not >)

\ --- contraction-index domain (SPEC-CT@ and SPEC-CT-EXTENT@) ---
: SB-CT-NEG     ( -- )  SB-SET  -1 SPEC-CT@ 2drop ;
: SB-CT-CNT     ( -- )  SB-SET   1 SPEC-CT@ 2drop ;              \ == live count
: SB-CT-CNT1    ( -- )  SB-SET   2 SPEC-CT@ 2drop ;              \ count + 1
: SB-CT-BIG     ( -- )  SB-SET  99 SPEC-CT@ 2drop ;
: SB-CTEXT-BIG  ( -- )  SB-SET  99 SPEC-CT-EXTENT@ drop ;
' SB-CT-NEG     E-SPEC-RANGE TTHROWS
' SB-CT-CNT     E-SPEC-RANGE TTHROWS
' SB-CT-CNT1    E-SPEC-RANGE TTHROWS
' SB-CT-BIG     E-SPEC-RANGE TTHROWS
' SB-CTEXT-BIG  E-SPEC-RANGE TTHROWS
SB-SET  0 SPEC-CT@ s" r" STR= -1 T=                              \ last valid index still returns

\ --- factor domain (SPEC-FAC-NAME@ / SPEC-FAC-RANK@ against SP-FAC-N) ---
: SB-FAC-NEG    ( -- )  SB-SET  -1 SPEC-FAC-NAME@ 2drop ;
: SB-FAC-CNT    ( -- )  SB-SET   2 SPEC-FAC-NAME@ 2drop ;        \ == live count
: SB-FAC-CNT1   ( -- )  SB-SET   3 SPEC-FAC-NAME@ 2drop ;
: SB-FAC-BIG    ( -- )  SB-SET  99 SPEC-FAC-NAME@ 2drop ;
: SB-RANK-NEG   ( -- )  SB-SET  -1 SPEC-FAC-RANK@ drop ;
: SB-RANK-CNT   ( -- )  SB-SET   2 SPEC-FAC-RANK@ drop ;
: SB-RANK-BIG   ( -- )  SB-SET  99 SPEC-FAC-RANK@ drop ;
' SB-FAC-NEG    E-SPEC-RANGE TTHROWS
' SB-FAC-CNT    E-SPEC-RANGE TTHROWS
' SB-FAC-CNT1   E-SPEC-RANGE TTHROWS
' SB-FAC-BIG    E-SPEC-RANGE TTHROWS
' SB-RANK-NEG   E-SPEC-RANGE TTHROWS
' SB-RANK-CNT   E-SPEC-RANGE TTHROWS
' SB-RANK-BIG   E-SPEC-RANGE TTHROWS
SB-SET  1 SPEC-FAC-NAME@ s" B" STR= -1 T=                        \ last valid factor still returns
SB-SET  0 SPEC-FAC-RANK@ 2 T=

\ --- factor-member domain (SPEC-FAC-IDX@ / SPEC-FAC-GATHER@ against factor f's rank) ---
\ f=0 has rank 2, so k in {2,3,99} escapes factor 0's SP-FI window. On the base, 0 2
\ SPEC-FAC-IDX@ BLEEDS into factor 1's first index and returns "q"; the guard rejects it.
: SB-FIDX-FNEG  ( -- )  SB-SET  -1 0 SPEC-FAC-IDX@ 2drop ;
: SB-FIDX-FCNT  ( -- )  SB-SET   2 0 SPEC-FAC-IDX@ 2drop ;       \ factor == count
: SB-FIDX-KNEG  ( -- )  SB-SET   0 -1 SPEC-FAC-IDX@ 2drop ;
: SB-FIDX-KRANK ( -- )  SB-SET   0  2 SPEC-FAC-IDX@ 2drop ;      \ k == rank (cross-factor bleed on base)
: SB-FIDX-KRNK1 ( -- )  SB-SET   0  3 SPEC-FAC-IDX@ 2drop ;
: SB-FIDX-KBIG  ( -- )  SB-SET   0 99 SPEC-FAC-IDX@ 2drop ;
: SB-FGAT-FCNT  ( -- )  SB-SET   2  0 SPEC-FAC-GATHER@ 2drop ;
: SB-FGAT-KRANK ( -- )  SB-SET   0  2 SPEC-FAC-GATHER@ 2drop ;
' SB-FIDX-FNEG  E-SPEC-RANGE TTHROWS
' SB-FIDX-FCNT  E-SPEC-RANGE TTHROWS
' SB-FIDX-KNEG  E-SPEC-RANGE TTHROWS
' SB-FIDX-KRANK E-SPEC-RANGE TTHROWS
' SB-FIDX-KRNK1 E-SPEC-RANGE TTHROWS
' SB-FIDX-KBIG  E-SPEC-RANGE TTHROWS
' SB-FGAT-FCNT  E-SPEC-RANGE TTHROWS
' SB-FGAT-KRANK E-SPEC-RANGE TTHROWS
SB-SET  0 1 SPEC-FAC-IDX@ s" r" STR= -1 T=                       \ last valid member of factor 0
SB-SET  1 0 SPEC-FAC-IDX@ s" q" STR= -1 T=                       \ first member of factor 1 (the base bleed target)

\ --- factor-index row (public SP-FI-VAR@ / SP-FI-GATH@ against SP-FI-N): a directly
\ forged >SP-FI must fail closed, not just an accidental raw counter. SGEMM has SP-FI-N=4.
: SB-FI-NEG   ( -- )  SB-SET  -1 >SP-FI SP-FI-VAR@ 2drop ;
: SB-FI-CNT   ( -- )  SB-SET   4 >SP-FI SP-FI-VAR@ 2drop ;       \ row == live SP-FI-N
: SB-FI-BIG   ( -- )  SB-SET  99 >SP-FI SP-FI-GATH@ 2drop ;
' SB-FI-NEG   E-SPEC-RANGE TTHROWS
' SB-FI-CNT   E-SPEC-RANGE TTHROWS
' SB-FI-BIG   E-SPEC-RANGE TTHROWS
SB-SET  3 >SP-FI SP-FI-VAR@ s" r" STR= -1 T=                     \ last valid row (confirms SP-FI-N=4)

\ --- empty-domain (zero free indices): the full-reduction BC-FRED output has SP-FREE-N=0,
\ so EVERY free index - including 0 - is out of domain and rejected.
: SB-FRED-SET   ( -- )  s" BSCL[] = BXA[bm bn] · BXB[bm bn] +SUM bm bn" SPEC-CHECK$ ;
: SB-FRED-FREE0 ( -- )  SB-FRED-SET  0 SPEC-FREE@ 2drop ;
' SB-FRED-FREE0 E-SPEC-RANGE TTHROWS
SB-FRED-SET  SPEC-FREE-N 0 T=                                    \ confirm the empty free domain

\ --- equation-factor domain (EQ-FROW@ / EQ-FCOL@ / EQ-ADJ@ against EQ-K@). PGEMM is the
\ registered composable equation above; EQ-K@ = 2. k in {2,3,99} escapes its live factor
\ window; EQ-ADJ@ with a bad k would otherwise forge an eq-slot from a neighbouring cell.
: PG-SLOT ( -- eq-slot )  s" PGEMM" EQ-FIND MATCH option  none OF E-SPEC-SYNTAX throw ENDOF  some OF ENDOF ;MATCH ;
: SB-FROW-KNEG  ( -- )  PG-SLOT -1 EQ-FROW@ drop ;
: SB-FROW-KCNT  ( -- )  PG-SLOT  2 EQ-FROW@ drop ;              \ == EQ-K@
: SB-FROW-KCNT1 ( -- )  PG-SLOT  3 EQ-FROW@ drop ;
: SB-FROW-KBIG  ( -- )  PG-SLOT 99 EQ-FROW@ drop ;
: SB-FCOL-KCNT  ( -- )  PG-SLOT  2 EQ-FCOL@ drop ;
: SB-FCOL-KBIG  ( -- )  PG-SLOT 99 EQ-FCOL@ drop ;
: SB-ADJ-KNEG   ( -- )  PG-SLOT -1 EQ-ADJ@ drop ;
: SB-ADJ-KCNT   ( -- )  PG-SLOT  2 EQ-ADJ@ drop ;
: SB-ADJ-KBIG   ( -- )  PG-SLOT 99 EQ-ADJ@ drop ;
PG-SLOT EQ-K@ 2 T=                                              \ the live factor window is [0,2)
' SB-FROW-KNEG  E-SPEC-RANGE TTHROWS
' SB-FROW-KCNT  E-SPEC-RANGE TTHROWS
' SB-FROW-KCNT1 E-SPEC-RANGE TTHROWS
' SB-FROW-KBIG  E-SPEC-RANGE TTHROWS
' SB-FCOL-KCNT  E-SPEC-RANGE TTHROWS
' SB-FCOL-KBIG  E-SPEC-RANGE TTHROWS
' SB-ADJ-KNEG   E-SPEC-RANGE TTHROWS
' SB-ADJ-KCNT   E-SPEC-RANGE TTHROWS
' SB-ADJ-KBIG   E-SPEC-RANGE TTHROWS
PG-SLOT 1 EQ-FROW@ 0 > -1 T=                                    \ last valid factor still returns a live dim

\ --- arena invariant: every VALID accessor span has a length within its slot cap, so with
\ the index-in-[0,count) guard the whole [ptr,ptr+len) lies inside the live table arena.
: SPANS-IN-ARENA? ( -- bool )
   SB-SET  true
   SPEC-FREE-N 0 ?do  i SPEC-FREE@ nip SP-IDX-NAME <= and  loop
   SPEC-CT-N   0 ?do  i SPEC-CT@   nip SP-IDX-NAME <= and  loop
   SPEC-FAC-N  0 ?do
      i SPEC-FAC-NAME@ nip SP-TNAME <= and
      i SPEC-FAC-RANK@ 0 ?do  j i SPEC-FAC-IDX@ nip SP-IDX-NAME <= and  loop
   loop ;
SPANS-IN-ARENA? TTRUE

;package

T-REPORT
