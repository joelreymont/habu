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

;package

T-REPORT
