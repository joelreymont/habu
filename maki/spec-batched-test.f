\ maki/spec-batched-test.f - batched free-extent SPEC: equations (BTC-2), the step
\ between the landed extent-role product/factorization checker (BTC-7) and trainable
\ batched attention. Proves a SPEC: line can carry a FREE (non-contracted) batch/head
\ index that rides every factor + the output, so the GGEMM is the SAME contraction
\ REPLICATED over the free extents (docs/batch-sequence-design.md §5 BTC-2, §4.3;
\ docs/golden-syntax.md batched-contraction section; docs/tma-gather.md:29-45,90-92).
\
\   S[b h i j] = Σk Q[b h i k] · K[b h j k]     \ b,h free (batch/head); i,j GEMM; k contracted
\
\ Derivations, each proven the spec-attention-test.f way:
\   (1) checked accessor bodies - the generated <NAME>/<NAME>-GEMM/<NAME>-EL golden runs
\       and equals a plain replicated-loop reference to exact numeric parity. The free
\       extents type as free: FREE-EXTENT: marks #B/#H, so a contraction over one is the
\       BTC-7 checker reject (below), never a runtime error.
\   (2) the dataflow record - SPEC-BATCH-N / SPEC-BATCH@ / SPEC-BATCH-EXTENT@ expose the
\       replication axes (the free extents the contraction is replicated over), alongside
\       the existing SPEC-FREE / SPEC-CT / SPEC-FAC readers.
\   (3) the PROMOTE obligations - SPEC-BATCH-EXTENT@ gives the free-extent magnitudes the
\       planner needs to derive B-outermost strides / TMA box dims under the SMEM budget
\       (docs/tma-gather.md:90-92). Consistent with the shipped SPEC: PROMOTE record: a
\       self-contained magnitude record, not a live planner consumer (BTC-6 is that).
\   Adjoint: the adjoint of a batched contraction is the batched TRANSPOSED contraction
\       with the SAME free extents riding along - central-FD gradchecked, with batch
\       isolation proven (a cross-batch perturbation has zero effect on another batch).
\   Negatives red-first: contracting a declared-free extent is the BTC-7 load reject
\       (exit 70 class, proven by scoring SPEC:'s generated redx witness), a mismatched
\       free extent between factors is a checker reject of the generated accessor, and the
\       batch-not-leading / batch-missing / elementwise-with-a-free-role forms fail closed.
\
\ Own extents #BB/#BH/#BI/#BJ/#BK and SBT- tensors, so it loads cleanly into the single
\ shared maki/test.f image alongside every other suite. i (query) and j (key) are distinct
\ extents (the transposed-operand pattern of spec-attention-test.f), both length 3.

require lib/test.f
require test/checker-assert.f
require maki/spec.f
require maki/gradcheck.f              \ GC-CLOSE?: the central-FD closeness idiom the adjoint gradcheck reuses

T-RESET

package MAKI

2 FREE-EXTENT: #BB   2 FREE-EXTENT: #BH   3 EXTENT: #BI   3 EXTENT: #BJ   2 EXTENT: #BK
TENSOR: SBT-Q ( #BB #BH #BI #BK )    \ Q : B x H x Tq x d
TENSOR: SBT-K ( #BB #BH #BJ #BK )    \ K : B x H x Tk x d  (indexed [key row]; the transposed operand)
TENSOR: SBT-S ( #BB #BH #BI #BJ )    \ S : B x H x Tq x Tk  (per-(b,h) scores)

SPEC: SBT-ATT  SBT-S[bb bh bi bj] = Σbk SBT-Q[bb bh bi bk] · SBT-K[bb bh bj bk] ;

\ --- derivation (2): the dataflow record - two batch (replication) axes, two GEMM output
\ axes, one contraction; the free extents the contraction is replicated over ------------
SPEC-NAME$ s" SBT-ATT" STR= -1 T=
SPEC-OUT$  s" SBT-S" STR= -1 T=
SPEC-BATCH-N 2 T=
0 SPEC-BATCH@ s" bb" STR= -1 T=   1 SPEC-BATCH@ s" bh" STR= -1 T=
SPEC-FREE-N 4 T=   2 SPEC-FREE@ s" bi" STR= -1 T=   3 SPEC-FREE@ s" bj" STR= -1 T=
SPEC-CT-N   1 T=   0 SPEC-CT@ s" bk" STR= -1 T=
SPEC-FAC-N  2 T=
0 SPEC-FAC-NAME@ s" SBT-Q" STR= -1 T=   1 SPEC-FAC-NAME@ s" SBT-K" STR= -1 T=
0 SPEC-FAC-RANK@ 4 T=   1 SPEC-FAC-RANK@ 4 T=                              \ each factor carries every batch axis
0 0 SPEC-FAC-IDX@ s" bb" STR= -1 T=   1 0 SPEC-FAC-IDX@ s" bb" STR= -1 T=  \ batch rides both factors
0 3 SPEC-FAC-IDX@ s" bk" STR= -1 T=   1 3 SPEC-FAC-IDX@ s" bk" STR= -1 T=  \ shared trailing contraction index

\ --- derivation (3): the PROMOTE shape obligations - free (replication) + GEMM + contraction
0 SPEC-BATCH-EXTENT@ 2 T=   1 SPEC-BATCH-EXTENT@ 2 T=   \ replication extents B=2, H=2
2 SPEC-FREE-EXTENT@ 3 T=    3 SPEC-FREE-EXTENT@ 3 T=    \ GEMM (score) shape Tq x Tk = 3 x 3
0 SPEC-CT-EXTENT@ 2 T=                                  \ contraction span d = 2

\ --- derivation (1): the generated batched golden equals a plain replicated-loop reference
create SBT-BQ #BB #BH * #BI * #BK * cells allot
create SBT-BK #BB #BH * #BJ * #BK * cells allot
create SBT-BS #BB #BH * #BI * #BJ * cells allot
create SBT-RS #BB #BH * #BI * #BJ * cells allot        \ reference score
create SBT-DS #BB #BH * #BI * #BJ * cells allot        \ seed cotangent (varied, non-uniform)
create SBT-DQ #BB #BH * #BI * #BK * cells allot        \ analytic dQ
create SBT-DK #BB #BH * #BJ * #BK * cells allot        \ analytic dK
create SBT-DQ2 #BB #BH * #BI * #BK * cells allot       \ dQ after a cross-batch perturbation

: SBT-FILL ( -- )
   #BB #BH * #BI * #BK * 0 ?do  i 1 + s>f 7.0 f/  SBT-BQ i T-SET  loop
   #BB #BH * #BJ * #BK * 0 ?do  i 2 * 1 + s>f 11.0 f/  SBT-BK i T-SET  loop
   #BB #BH * #BI * #BJ * 0 ?do  i 5 mod s>f 0.1 f* 0.3 f+  SBT-DS i T-SET  loop ;
: SBT-BIND ( -- )  SBT-BQ SBT-Q-BIND  SBT-BK SBT-K-BIND  SBT-BS SBT-S-BIND ;

\ plain replicated-loop reference: per (b,h) block, the Q.K^T score; distinct locals so the
\ position args never collide with the loop-index words i/j (a real batched-code footgun).
: SBT-QIDX ( n n n n -- n ) {: b:n h:n p:n k:n :}  b #BH * h + #BI * p + #BK * k + ;
: SBT-REF-EL ( n n n n -- r ) {: b:n h:n p:n q:n :}
   0.0 #BK 0 ?do  SBT-BQ b h p i SBT-QIDX T-GET  SBT-BK b h q i SBT-QIDX T-GET  f* f+  loop ;
: SBT-SIDX ( n n n n -- n ) {: b:n h:n p:n q:n :}  b #BH * h + #BI * p + #BJ * q + ;
: SBT-REF-BLK ( n n -- ) {: b:n h:n :}
   #BI 0 ?do #BJ 0 ?do  b h j i SBT-REF-EL  SBT-RS b h j i SBT-SIDX T-SET  loop loop ;
: SBT-REF ( -- )  #BB 0 ?do #BH 0 ?do  j i SBT-REF-BLK  loop loop ;

SBT-FILL  SBT-BIND  SBT-ATT  SBT-REF
SBT-BS SBT-RS #BB #BH * #BI * #BJ * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=   \ exact numeric equality

\ --- adjoint: dQ = SBT-ATT-ADJ0 (dS,K), dK = SBT-ATT-ADJ1 (dS,Q), each a batched
\ transposed contraction carrying b,h. Central-FD gradchecked against L = sum(seed * S). --
: SBT-LOSS ( -- r )  0.0 #BB #BH * #BI * #BJ * 0 ?do  SBT-DS i T-GET  SBT-BS i T-GET  f*  f+  loop ;
: SBT-FD-Q ( n -- r ) {: e:n :}
   SBT-BQ e T-GET {: base:r :}
   base GC-H f+ SBT-BQ e T-SET  SBT-BIND SBT-ATT SBT-LOSS {: yp:r :}
   base GC-H f- SBT-BQ e T-SET  SBT-BIND SBT-ATT SBT-LOSS {: ym:r :}
   base SBT-BQ e T-SET  yp ym f- GC-H 2.0 f* f/ ;
: SBT-FD-K ( n -- r ) {: e:n :}
   SBT-BK e T-GET {: base:r :}
   base GC-H f+ SBT-BK e T-SET  SBT-BIND SBT-ATT SBT-LOSS {: yp:r :}
   base GC-H f- SBT-BK e T-SET  SBT-BIND SBT-ATT SBT-LOSS {: ym:r :}
   base SBT-BK e T-SET  yp ym f- GC-H 2.0 f* f/ ;
: SBT-RUN-ADJ ( -- )   \ dQ into SBT-DQ, dK into SBT-DK, from seed dS
   SBT-DS SBT-S-BIND  SBT-BK SBT-K-BIND  SBT-DQ SBT-Q-BIND  SBT-ATT-ADJ0
   SBT-DS SBT-S-BIND  SBT-BQ SBT-Q-BIND  SBT-DK SBT-K-BIND  SBT-ATT-ADJ1 ;
: SBT-GC ( -- bool )
   SBT-FILL  SBT-BIND SBT-ATT
   SBT-RUN-ADJ
   SBT-BIND  true
   #BB #BH * #BI * #BK * 0 ?do  SBT-DQ i T-GET  i SBT-FD-Q  GC-CLOSE? 0= if drop false leave then  loop
   dup if  #BB #BH * #BJ * #BK * 0 ?do  SBT-DK i T-GET  i SBT-FD-K  GC-CLOSE? 0= if drop false leave then  loop  then ;
SBT-GC -1 T=          \ batched adjoint gradchecks vs central FD

\ --- batch isolation: dQ for batch b depends ONLY on batch b's data. Perturb K in batch 1;
\ dQ's batch-0 block must be BIT-IDENTICAL (zero cross-batch effect), while batch 1 changes
\ (so the probe is not vacuous). This is the structural cross-sequence-leak guard, numerically.
#BH #BI * #BK * constant SBT-BLK          \ elements of one batch block in Q/dQ
: SBT-ISO ( -- bool )
   SBT-FILL  SBT-BIND SBT-ATT
   SBT-DS SBT-S-BIND  SBT-BK SBT-K-BIND  SBT-DQ SBT-Q-BIND  SBT-ATT-ADJ0     \ baseline dQ
   #BH #BJ * #BK *  {: kb1:n :}  SBT-BK kb1 T-GET  1.0 f+  SBT-BK kb1 T-SET   \ perturb K, batch 1
   SBT-DS SBT-S-BIND  SBT-BK SBT-K-BIND  SBT-DQ2 SBT-Q-BIND  SBT-ATT-ADJ0    \ perturbed dQ
   SBT-DQ SBT-DQ2 SBT-BLK T-DIST2  1000000.0 f* 0.5 f+ f>s 0=               \ batch-0 block unchanged
   SBT-DQ SBT-BLK T-AT  SBT-DQ2 SBT-BLK T-AT  SBT-BLK T-DIST2  0.0 f> and ; \ batch-1 block changed
SBT-ISO -1 T=

\ --- negatives (named throws): every mis-formed batched spec fails closed BEFORE codegen ---
TENSOR: SBT-NOB ( #BI #BK )           \ a factor missing the batch axis (broadcast, not replicate)
: SBT-N-NOLEAD  ( -- ) s" SBT-S[bi bb bj] = SBT-Q[bb bh bi bk] · SBT-K[bb bh bj bk] * +SUM bk" SPEC-CHECK$ ;
: SBT-N-MISSING ( -- ) s" SBT-S[bb bh bi bj] = SBT-NOB[bi bk] · SBT-K[bb bh bj bk] * +SUM bk" SPEC-CHECK$ ;
: SBT-N-EWFREE  ( -- ) s" SBT-S[bb bh bi bj] = SBT-Q[bb bh bi bk] + SBT-K[bb bh bj bk]" SPEC-CHECK$ ;
' SBT-N-NOLEAD  E-SPEC-ARITY  TTHROWS       \ a free role after a plain output index (batch must lead)
' SBT-N-MISSING E-SPEC-ARITY  TTHROWS       \ a batch axis absent from a factor
' SBT-N-EWFREE  E-SPEC-ARITY  TTHROWS       \ a free/batch role in an elementwise form (contraction-only)

\ --- checker rejects (candidate scoring; -1 accept, 0 reject) ---------------------------
\ (a) contracting a declared-free extent is the BTC-7 reject: SPEC: generates a per-equation
\ redx witness `<NAME>-RSUM ( ix<ct-ext> -- redx<ct-ext> )`, and the checker rejects
\ redx<free> at signature parse. An inner (head-dim) contraction extent accepts; the free
\ (batch) extent rejects - the same verdict a real authored line load-rejects with (exit 70).
TENSOR: SBT-A2 ( #BB #BI )   TENSOR: SBT-C2 ( #BB #BJ )   TENSOR: SBT-Y2 ( #BI #BJ )
SPEC-RCAND: SBT-ROK   SBT-S[bb bh bi bj] = SBT-Q[bb bh bi bk] · SBT-K[bb bh bj bk] * +SUM bk ;
SPEC-RCAND$ CHECK-QUIET-CANDIDATE! -1 T=          \ inner contraction #BK: accepted
SPEC-RCAND: SBT-RBAD  SBT-Y2[bi bj] = SBT-A2[bb bi] · SBT-C2[bb bj] * +SUM bb ;
SPEC-RCAND$ CHECK-QUIET-CANDIDATE!  0 T=          \ contracting the FREE #BB: rejected (the leak, closed)

\ (b) a mismatched free extent between factors: writing K's batch/head slots in Q's positions
\ flips ix<extbh> where ix<extbb> is demanded, so the generated accessor loop rejects.
SPEC-CAND: SBT-COK   SBT-S[bb bh bi bj] = SBT-Q[bb bh bi bk] · SBT-K[bb bh bj bk] * +SUM bk ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: SBT-CFLIP SBT-S[bb bh bi bj] = SBT-Q[bh bb bi bk] · SBT-K[bb bh bj bk] * +SUM bk ;   \ bb/bh swapped
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=

;package

T-REPORT
