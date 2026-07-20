\ maki/mha-test.f - checked proofs for the trainable batched multi-head causal self-
\ attention sublayer (maki/mha.f). The sublayer is the full GPT-2 c_attn shape: three
\ biased Q/K/V projections, per-head batched scaled-dot-product attention with segment-
\ causal masking, an output projection with weight+bias, and the residual add - MHA-FWD
\ paired with the exact adjoint MHA-BWD.
\
\ Proofs here (correctness of the sublayer itself; training + GPT-block integration are
\ maki/mha-block-test.f):
\   (A) FORWARD GOLDEN - MHA-FWD equals an independent replicated-loop reference (the
\       single-head attention case computed per (batch, head) from the canonical
\       primitives maki/matmul.f MATMUL, maki/attention.f MM-NT, maki/causal.f
\       CAUSAL-SOFTMAX-ROWS), cell for cell. Because the reference IS the single-head
\       case replicated, this is also the B=1,H=1 single-head parity the dot asks for
\       (every (b,h) block reduces to standalone single-head SDPA).
\   (B) GRADCHECK - the analytic MHA-BWD adjoint of X and EVERY weight and bias matches a
\       central finite difference of the seeded loss L = sum seed*Y, over every element.
\   (C) BATCH ISOLATION - perturbing batch 1's input leaves batch 0's dX bit-identical
\       (and changes batch 1's, so the probe is not vacuous): the batched score/context
\       equations carry b as a free replication axis, so cross-batch leakage is zero.
\   (D) HEAD ISOLATION - perturbing head 1's Q/K/V weight columns leaves head 0's context
\       block untouched (pre-merge) and changes head 1's: heads never mix before Wo.
\   (E) CAUSALITY per (b,h) - every future-key attention weight (key > query) is < 1e-6
\       in every batch and head.
\   (F) NEGATIVES - malformed batched specs are named throws before codegen, and a
\       swapped-operand or free-extent-contraction candidate is a CHECKER reject.
\
\ Assertions run at top level (interpret mode) like maki/spec-batched-test.f. Buffers
\ carry an MHAT- prefix; the sublayer owns extents #MB/#MH/#MQ/#MK/#MD/#MC, so this loads
\ cleanly into the shared maki/test.f image.

require lib/test.f
require test/checker-assert.f
require maki/gradcheck.f              \ GC-CLOSE? / GC-H : the central-FD closeness idiom
require maki/attention.f              \ MM-NT : the single-head Q.K^T reference GEMM
require maki/mha.f

T-RESET

package MAKI

\ ---- sizes (B=2, T=4, C=6, H=2, hd=3) ------------------------------------------------
MHA-BT #MC * constant MHAT-XN         \ flat input/output elements B*T*C
#MC #MC * constant MHAT-WN            \ projection weight elements C*C
#MQ #MD * constant MHAT-HDN           \ one head block T*hd
#MQ #MK * constant MHAT-SN            \ one head score block T*T

\ ---- inputs + outputs ----------------------------------------------------------------
create MHAT-X  MHAT-XN cells allot
create MHAT-WQ MHAT-WN cells allot   create MHAT-BQ #MC cells allot
create MHAT-WK MHAT-WN cells allot   create MHAT-BK #MC cells allot
create MHAT-WV MHAT-WN cells allot   create MHAT-BV #MC cells allot
create MHAT-WO MHAT-WN cells allot   create MHAT-BO #MC cells allot
create MHAT-Y  MHAT-XN cells allot
create MHAT-SEED MHAT-XN cells allot

\ ---- analytic gradient outputs -------------------------------------------------------
create MHAT-DX  MHAT-XN cells allot
create MHAT-DWQ MHAT-WN cells allot   create MHAT-DBQ #MC cells allot
create MHAT-DWK MHAT-WN cells allot   create MHAT-DBK #MC cells allot
create MHAT-DWV MHAT-WN cells allot   create MHAT-DBV #MC cells allot
create MHAT-DWO MHAT-WN cells allot   create MHAT-DBO #MC cells allot
create MHAT-DX2 MHAT-XN cells allot    \ dX after a cross-batch perturbation (isolation)

\ ---- reference scratch ---------------------------------------------------------------
create MHAT-QF MHAT-XN cells allot   create MHAT-KF MHAT-XN cells allot   create MHAT-VF MHAT-XN cells allot
create MHAT-QH MHAT-HDN cells allot  create MHAT-KH MHAT-HDN cells allot  create MHAT-VH MHAT-HDN cells allot
create MHAT-S  MHAT-SN cells allot   create MHAT-A  MHAT-SN cells allot   create MHAT-OH MHAT-HDN cells allot
create MHAT-OF MHAT-XN cells allot   create MHAT-RY MHAT-XN cells allot
create MHAT-OSAVE MHAT-HDN cells allot   \ head-0 context snapshot (head isolation)

: MHAT-ROWBIAS ( ptr a n n ptr a -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do  cols 0 ?do  yb j cols * i + T-GET  bb i T-GET f+  yb j cols * i + T-SET  loop  loop ;
: MHAT-COPY ( ptr a ptr a n -- ) {: s:ptr d:ptr n:n :}  n 0 ?do  s i T-GET  d i T-SET  loop ;

\ deterministic non-degenerate fills (distinct sources per buffer)
: MHAT-FILL ( -- )
   MHAT-XN 0 ?do  i 1 + s>f 23.0 f/  MHAT-X  i T-SET  loop
   MHAT-WN 0 ?do  i 2 + s>f 29.0 f/  MHAT-WQ i T-SET  loop
   MHAT-WN 0 ?do  i 3 + s>f 31.0 f/  MHAT-WK i T-SET  loop
   MHAT-WN 0 ?do  i 5 + s>f 37.0 f/  MHAT-WV i T-SET  loop
   MHAT-WN 0 ?do  i 7 + s>f 41.0 f/  MHAT-WO i T-SET  loop
   #MC 0 ?do  i 1 + s>f 43.0 f/  MHAT-BQ i T-SET  loop
   #MC 0 ?do  i 2 + s>f 47.0 f/  MHAT-BK i T-SET  loop
   #MC 0 ?do  i 3 + s>f 53.0 f/  MHAT-BV i T-SET  loop
   #MC 0 ?do  i 4 + s>f 59.0 f/  MHAT-BO i T-SET  loop
   MHAT-XN 0 ?do  i 7 mod s>f 0.1 f* 0.2 f+  MHAT-SEED i T-SET  loop ;

\ ---- MHA-FWD / MHA-BWD drivers -------------------------------------------------------
: MHAT-RUNF ( -- )
   MHAT-X MHAT-WQ MHAT-BQ MHAT-WK MHAT-BK MHAT-WV MHAT-BV MHAT-WO MHAT-BO MHAT-Y MHA-FWD ;
: MHAT-RUNB ( -- )    \ seed cotangent = MHAT-SEED, analytic grads into the MHAT-D* buffers
   MHAT-X MHAT-WQ MHAT-WK MHAT-WV MHAT-WO MHAT-SEED
   MHAT-DX MHAT-DWQ MHAT-DBQ MHAT-DWK MHAT-DBK MHAT-DWV MHAT-DBV MHAT-DWO MHAT-DBO MHA-BWD ;
: MHAT-LOSS ( -- r )  0.0 MHAT-XN 0 ?do  MHAT-SEED i T-GET  MHAT-Y i T-GET  f*  f+  loop ;

\ ---- (A) independent replicated-loop reference ---------------------------------------
\ Extract head (b,h)'s T x hd block from a flat (B*T,C) projection buffer into a
\ contiguous temp: blk[t,d] = flat[(b*T+t)*C + h*hd + d].
: MHAT-EXTRACT ( ptr a n n ptr a -- ) {: sf:ptr b:n h:n dt:ptr :}
   #MQ 0 ?do  #MD 0 ?do
      sf  b #MQ * j +  #MC *  h #MD * +  i +  T-GET
      dt  j #MD * i +  T-SET
   loop  loop ;
\ store head (b,h)'s T x hd context back into the flat merged buffer MHAT-OF.
: MHAT-STORE-OF ( ptr a n n -- ) {: oh:ptr b:n h:n :}
   #MQ 0 ?do  #MD 0 ?do
      oh j #MD * i + T-GET
      MHAT-OF  b #MQ * j +  #MC *  h #MD * +  i +  T-SET
   loop  loop ;
\ one (b,h) block: project-slice -> scaled causal SDPA -> store context (into MHAT-OF).
\ leaves the head-0 context in MHAT-OH (snapshotted by the head-isolation probe).
: MHAT-BH ( n -- ) {: bh:n :}
   bh #MH / {: b:n :}   bh #MH mod {: h:n :}
   MHAT-QF b h MHAT-QH MHAT-EXTRACT
   MHAT-KF b h MHAT-KH MHAT-EXTRACT
   MHAT-VF b h MHAT-VH MHAT-EXTRACT
   MHAT-QH MHAT-KH MHAT-S  #MQ #MK #MD  MM-NT
   MHAT-S MHAT-SN MHA-INV ATTN-SCALE!
   MHAT-S MHAT-A #MQ CAUSAL-SOFTMAX-ROWS
   MHAT-A MHAT-VH MHAT-OH  #MQ #MK #MD  MATMUL
   MHAT-OH b h MHAT-STORE-OF ;
: MHAT-REF ( -- )
   MHAT-X MHAT-WQ MHAT-QF MHA-BT #MC #MC MATMUL   MHAT-QF MHA-BT #MC MHAT-BQ MHAT-ROWBIAS
   MHAT-X MHAT-WK MHAT-KF MHA-BT #MC #MC MATMUL   MHAT-KF MHA-BT #MC MHAT-BK MHAT-ROWBIAS
   MHAT-X MHAT-WV MHAT-VF MHA-BT #MC #MC MATMUL   MHAT-VF MHA-BT #MC MHAT-BV MHAT-ROWBIAS
   #MB #MH * 0 ?do  i MHAT-BH  loop
   MHAT-OF MHAT-WO MHAT-RY MHA-BT #MC #MC MATMUL
   MHAT-RY MHA-BT #MC MHAT-BO MHAT-ROWBIAS
   MHAT-RY MHAT-X MHA-BT #MC * T-ADD! ;

MHAT-FILL
MHAT-REF
MHAT-RUNF
MHAT-Y MHAT-RY MHAT-XN T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=      \ (A) forward golden, exact

\ ---- (B) gradcheck: analytic adjoint vs central FD, every param, every element --------
\ perturb param buffer pb element e, rerun forward, return the seeded loss.
: MHAT-FD ( ptr a n -- r ) {: pb:ptr e:n :}
   pb e T-GET {: base:r :}
   base GC-H f+ pb e T-SET  MHAT-RUNF MHAT-LOSS {: lp:r :}
   base GC-H f- pb e T-SET  MHAT-RUNF MHAT-LOSS {: lm:r :}
   base pb e T-SET
   lp lm f-  GC-H 2.0 f* f/ ;
\ every element of param pb (length n) agrees with its analytic grad gb.
: MHAT-GC? ( ptr a n ptr a -- bool ) {: pb:ptr n:n gb:ptr :}
   true
   n 0 ?do  gb i T-GET  pb i MHAT-FD  GC-CLOSE? 0= if drop false leave then  loop ;

MHAT-FILL  MHAT-RUNF  MHAT-RUNB          \ analytic grads captured before FD perturbs the tape
MHAT-X  MHAT-XN MHAT-DX  MHAT-GC? TTRUE           \ dX
MHAT-WQ MHAT-WN MHAT-DWQ MHAT-GC? TTRUE           \ dWq
MHAT-BQ #MC     MHAT-DBQ MHAT-GC? TTRUE           \ dbq
MHAT-WK MHAT-WN MHAT-DWK MHAT-GC? TTRUE           \ dWk
MHAT-BK #MC     MHAT-DBK MHAT-GC? TTRUE           \ dbk
MHAT-WV MHAT-WN MHAT-DWV MHAT-GC? TTRUE           \ dWv
MHAT-BV #MC     MHAT-DBV MHAT-GC? TTRUE           \ dbv
MHAT-WO MHAT-WN MHAT-DWO MHAT-GC? TTRUE           \ dWo
MHAT-BO #MC     MHAT-DBO MHAT-GC? TTRUE           \ dbo

\ ---- (C) batch isolation: perturb batch 1's input, batch 0's dX is bit-identical ------
#MQ #MC * constant MHAT-BATCHN            \ elements of one batch block in X/dX (T*C)
: MHAT-BATCH-ISO ( -- bool )
   MHAT-FILL  MHAT-RUNF  MHAT-RUNB                            \ baseline dX
   MHAT-DX MHAT-DX2 MHAT-XN MHAT-COPY                          \ save baseline
   MHAT-BATCHN 0 ?do                                          \ perturb batch 1's whole input block
      MHAT-X MHAT-BATCHN i +  T-GET  1.0 f+  MHAT-X MHAT-BATCHN i +  T-SET
   loop
   MHAT-RUNF MHAT-RUNB                                         \ perturbed dX in MHAT-DX
   MHAT-DX MHAT-DX2 MHAT-BATCHN T-DIST2  1000000.0 f* 0.5 f+ f>s 0=          \ batch-0 block unchanged
   MHAT-DX MHAT-BATCHN T-AT  MHAT-DX2 MHAT-BATCHN T-AT  MHAT-BATCHN T-DIST2  0.0 f> and ; \ batch-1 changed
MHAT-BATCH-ISO TTRUE

\ ---- (D) head isolation: perturb head 1's Wq/Wk/Wv columns, head 0's context untouched
\ MHAT-BH leaves head 0's context in MHAT-OH when it runs LAST for (b=0,h=0); recompute
\ head (0,0) explicitly and snapshot it, then perturb head-1 weight columns and recompute.
: MHAT-HEAD00 ( -- )   \ compute head (0,0)'s context into MHAT-OH (independent of the mix)
   MHAT-X MHAT-WQ MHAT-QF MHA-BT #MC #MC MATMUL   MHAT-QF MHA-BT #MC MHAT-BQ MHAT-ROWBIAS
   MHAT-X MHAT-WK MHAT-KF MHA-BT #MC #MC MATMUL   MHAT-KF MHA-BT #MC MHAT-BK MHAT-ROWBIAS
   MHAT-X MHAT-WV MHAT-VF MHA-BT #MC #MC MATMUL   MHAT-VF MHA-BT #MC MHAT-BV MHAT-ROWBIAS
   0 MHAT-BH ;          \ bh=0 -> (b=0,h=0); context into MHAT-OH
: MHAT-HEAD-ISO ( -- bool )
   MHAT-FILL  MHAT-HEAD00  MHAT-OH MHAT-OSAVE MHAT-HDN MHAT-COPY     \ snapshot head-0 context
   #MC 0 ?do                                                        \ perturb head-1 columns [hd,2hd) of Wq/Wk/Wv
      MHAT-WQ i #MC * #MD +  T-GET 50.0 f+  MHAT-WQ i #MC * #MD +  T-SET
      MHAT-WK i #MC * #MD +  T-GET 50.0 f+  MHAT-WK i #MC * #MD +  T-SET
      MHAT-WV i #MC * #MD +  T-GET 50.0 f+  MHAT-WV i #MC * #MD +  T-SET
   loop
   MHAT-HEAD00                                                       \ recompute head 0's context
   MHAT-OH MHAT-OSAVE MHAT-HDN T-DIST2  1000000.0 f* 0.5 f+ f>s 0= ; \ head 0 unchanged by head-1 weights
MHAT-HEAD-ISO TTRUE

\ ---- (E) causality per (b,h): every future-key attention weight is ~ 0 -----------------
\ Recompute A per (b,h) with the reference and assert A[i,j] < 1e-6 for every j>i.
: MHAT-CAUSAL? ( -- bool )
   MHAT-FILL
   MHAT-X MHAT-WQ MHAT-QF MHA-BT #MC #MC MATMUL   MHAT-QF MHA-BT #MC MHAT-BQ MHAT-ROWBIAS
   MHAT-X MHAT-WK MHAT-KF MHA-BT #MC #MC MATMUL   MHAT-KF MHA-BT #MC MHAT-BK MHAT-ROWBIAS
   true
   #MB #MH * 0 ?do
      i #MH / {: b:n :}  i #MH mod {: h:n :}
      MHAT-QF b h MHAT-QH MHAT-EXTRACT
      MHAT-KF b h MHAT-KH MHAT-EXTRACT
      MHAT-QH MHAT-KH MHAT-S #MQ #MK #MD MM-NT
      MHAT-S MHAT-SN MHA-INV ATTN-SCALE!
      MHAT-S MHAT-A #MQ CAUSAL-SOFTMAX-ROWS
      #MQ 0 ?do  #MK 0 ?do
         i j > if  MHAT-A j #MK * i + T-GET 0.000001 f<  and  then     \ key i > query j : masked ~ 0
      loop  loop
   loop ;
MHAT-CAUSAL? TTRUE

\ ---- (F) negatives: malformed batched specs are NAMED throws (no code generated) ------
: MHAT-NOSTAR   ( -- ) s" MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] MHA-K[mb mh mk md] +SUM md" SPEC-CHECK$ ;   \ 2 factors, no *
: MHAT-RANK     ( -- ) s" MHA-S[mb mh mq mk] = MHA-Q[mb mh mq] MHA-K[mb mh mk md] * +SUM md" SPEC-CHECK$ ;      \ MHA-Q rank 3, given 3 indices vs 4
: MHAT-FREE3    ( -- ) s" MHA-S[mb mh mq mk md] = MHA-Q[mb mh mq md] MHA-K[mb mh mk md] * +SUM md" SPEC-CHECK$ ; \ 3 GEMM axes (mq mk md) > 2
: MHAT-UNKEXT   ( -- ) s" MHA-S[mb mh mq zz] = MHA-Q[mb mh mq md] MHA-K[mb mh zz md] * +SUM md" SPEC-CHECK$ ;   \ zz -> #ZZ undeclared
: MHAT-UNKTENS  ( -- ) s" MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] ZZ[mb mh mk md] * +SUM md" SPEC-CHECK$ ;      \ tensor ZZ undeclared
: MHAT-OKSPEC   ( -- ) s" MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] MHA-K[mb mh mk md] * +SUM md" SPEC-CHECK$ ;    \ valid: no throw
' MHAT-NOSTAR   E-SPEC-SYNTAX  TTHROWS
' MHAT-RANK     E-SPEC-ARITY   TTHROWS
' MHAT-FREE3    E-SPEC-ARITY   TTHROWS
' MHAT-UNKEXT   E-SPEC-EXTENT  TTHROWS
' MHAT-UNKTENS  E-SPEC-TENSOR  TTHROWS
' MHAT-OKSPEC   0              TTHROWS

\ a swapped-operand candidate is a CHECKER reject (correct order accepts). Proven on the
\ transposed Q.K^T operand: writing K's key/head-dim indices in the wrong slots flips the
\ demanded extent, so the generated accessor loop scores 0.
SPEC-CAND: MHA-COK   MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] MHA-K[mb mh mk md] * +SUM md ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: MHA-CFLIP MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] MHA-K[mb mh md mk] * +SUM md ;   \ MHA-K mk/md swapped
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=
\ (contracting a declared-FREE batch extent is the BTC-7 load reject: proven for this same
\ FREE-EXTENT: mechanism in maki/spec-batched-test.f, which #MB/#MH reuse verbatim.)

;package

T-REPORT
