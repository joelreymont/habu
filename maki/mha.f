\ maki/mha.f - trainable batched multi-head causal self-attention sublayer, the full
\ GPT-2 c_attn shape: checked B,T,C,H,hd geometry, ONE fused Q/K/V projection with a
\ combined bias, per-head batched scaled-dot-product attention with segment-causal
\ masking, an output projection with weight+bias, and the residual add. The sublayer is
\ differentiable end to end: MHA-FWD is paired with MHA-BWD, which produces the adjoints
\ of X and EVERY weight and bias.
\
\   Y = residual(X) + ( concat_h softmax_causal( Q_h K_h^T / sqrt(hd) ) V_h ) Wo + bo
\   [Q K V] = X Wqkv + bqkv   (ONE fused GPT-2 c_attn projection; Q|K|V are column blocks
\                              [0,C)|[C,2C)|[2C,3C) of the (B*T,3C) output, split by offset)
\
\ WHAT IS "IN THE GRAPH" (batched SPEC equations) vs HOST GLUE.
\   - The per-head scores and context are BATCHED SPEC: equations carrying b,h as FREE
\     (replication) indices - the landed free-extent + batched-adjoint machinery
\     (maki/spec.f EQ-BATCHED-ADJ-DERIVE, proven in maki/spec-batched-test.f). Each is
\     the SAME contraction replicated over every (batch, head):
\       MHA-SCORE  S[b h i j] = Q[b h i d] K[b h j d] +SUM d       (per-(b,h) Q.K^T)
\       MHA-CTX    O[b h i d] = A[b h i j] V[b h j d] +SUM j       (per-(b,h) A.V)
\     SPEC: derives their transposed adjoints (MHA-SCORE-ADJ0/1, MHA-CTX-ADJ0/1) with
\     the batch extents riding along, so batch AND head isolation are structural: a
\     cross-batch or cross-head perturbation has zero effect on another block's grad.
\   - Everything the multiply-then-sum grammar cannot state stays a named HOST op, as in
\     maki/attention.f / maki/causal.f: the ONE fused QKV projection + combined bias (a
\     shared-weight GEMM = plain 2D matmul over the flattened B*T rows, maki/matmul.f),
\     the head-major layout glue, the 1/sqrt(hd) scale, the causal-masked row softmax, the head merge,
\     the output projection + bias, and the residual add. Each host step has a hand-
\     composed adjoint in MHA-BWD; the whole forward/backward pair is finite-difference
\     gradchecked in maki/mha-test.f.
\
\ SV-6 HEAD HANDLING (this lane OWNS the decision, docs/strided-views.md SV-6). Per-head
\ BATCHED EQUATIONS win over head-split strided VIEWS, on evidence: the batched-equation
\ route needs NO new machinery (it is the landed grammar), while head-split views, RE-
\ EVALUATED 2026-07-20 now that SV-1..4 HAS landed (maki/tensor-value.f TV-HEAD-SPLIT /
\ TV-VIEW / TV-VIEW-ADJOINT+), are STILL insufficient here: that view core is 2D (offset +
\ row/col strides over rows x cols) read ONLY through the TV-AT@ strided seam, but this
\ sublayer's consumers are the 4D batched SPEC equations (MHA-SCORE / MHA-CTX) reading ONE
\ contiguous head-major buffer via extent-tensor.f's baked row-major Horner accessors, plus
\ MATMUL - all contiguous-only, NONE reading TV-AT@ (T-GET is a plain cells+ fetch). A 2D
\ head-split view of the fused buffer holds one head across all (b,t) rows: it cannot feed a
\ single 4D batched bind, and the baked accessor cannot apply its strides. Retiring the copy
\ would need N-D strided views + stride-aware equation/GEMM feeds (unlanded, on other lanes'
\ extent-tensor.f / spec.f / matmul.f surfaces) OR a per-head view-GEMM rewrite that abandons
\ the batched machinery and moves the 1789/532 block-training locks - so the equation route
\ STILL wins. Its only cost is a PHYSICAL head-major materialization: the projection writes
\ fused (B*T,3C) and the batched attention reads head-major (B,H,T,hd), so a fused <-> head-
\ major permutation (MHA-FLATIDX / MHA-QKVIDX, a 4D reindex, NOT a 2D affine stride) sits
\ between them as host layout glue (MHA-QKV>HM / MHA-HM>QKV forward, MHA-FLAT>HM / MHA-HM>FLAT
\ for the O merge) with its inverse-permutation adjoint. That materialization is the HONEST
\ BRIDGE, not a retirable copy; the SV-6 view program (H per-head views of one buffer, no
\ copy) remains the perf-optimal long-term form, sequenced behind this outcome AND those
\ still-unlanded view capabilities.
\
\ FIXED TOY MAGNITUDES ARE AN ORACLE, NOT A COMPLETION PROOF. Every extent magnitude is a
\ compile-time SPEC: constant, so this runs at ONE toy shape (B=2, T=4, C=6, H=2, hd=3),
\ like every SPEC: golden. The five roles (B,T,C,H,hd) are all declared and checked
\ (C = H*hd, query positions = key positions) at load. GPT-2-small magnitudes and the
\ real batch dimension arrive with the device path (habu-gb10-batched-attention-3055d565,
\ SV-7 strided TMA transfers); the fixed-shape forward here is an oracle for that work.
\
\ DEVICE LEG. The batched score/context are NON-composable equations (rank-4 factors do
\ not map to the 2D op registry, maki/spec.f EQ-COMPOSABLE?), so they are not MODEL: nodes
\ and do not enter the device-lowered MODEL graph (the landed lower/red path lowers
\ composable nodes). This sublayer is HOST-ONLY by construction; the device leg for
\ batched attention is the named downstream boundary above, not a silent skip.
\
\ REENTRANCY. Forward saves its activations (Q/K/V/A/O head-major + the merged O) in
\ module-private scratch, which MHA-BWD reads - a single-threaded saved-activation tape,
\ not reentrant. The packaging/owned-workspace hardening is dot habu-own-multi-head-
\ c863298a, serialized behind this lane. MHA-FWD is alias-safe by construction: all
\ intermediate compute lands in private scratch and the output buffer is written by one
\ final copy, so an aliased X/Y output never corrupts the residual. maki -> habu only;
\ mha owns -5151.

require maki/array.f          \ T-GET/T-SET/T-ADD! : buffer read/write + residual add
require maki/matmul.f         \ MATMUL / MATMUL-DX / MATMUL-DW : the projection GEMMs + adjoints
require maki/attention.f      \ ATTN-SCALE! : the 1/sqrt(hd) score scale (named host op)
require maki/causal.f         \ CAUSAL-SOFTMAX-ROWS / CAUSAL-SMBWD-ROWS : masked row softmax + VJP
require maki/spec.f           \ SPEC: / TENSOR: / EXTENT: / FREE-EXTENT: : the batched einsums

-5151 constant E-MHA-SHAPE   \ head geometry inconsistent: C != H*hd, or query/key extents unequal

package MAKI

\ ---- the five extent-role magnitudes (B,T,C,H,hd). Batch and head are FREE roles the
\ per-head contraction is replicated over; query and key positions are DISTINCT nominal
\ roles that share the magnitude T (so Q.K^T is the transposed-operand GEMM the checker
\ accepts and a swapped operand is a reject); channels C = H*hd. ---------------------
2 FREE-EXTENT: #MB    \ batch B
2 FREE-EXTENT: #MH    \ heads H
4 EXTENT: #MQ         \ query positions (sequence length T)
4 EXTENT: #MK         \ key positions (= T; a distinct role from the query axis)
3 EXTENT: #MD         \ head dim hd
6 EXTENT: #MC         \ channels C

\ every extent magnitude is a compile-time constant, so the geometry law is a load-time
\ named throw, never a silent wrong-shape run.
: MHA-CONFIG-CHECK ( -- )
   #MQ #MK <> if E-MHA-SHAPE throw then           \ self-attention: query positions = key positions
   #MC #MH #MD * <> if E-MHA-SHAPE throw then ;    \ channels = heads * head-dim
MHA-CONFIG-CHECK

\ ---- derived magnitudes (flat row count, head-major and score buffer sizes) ----------
: MHA-BT ( -- n )  #MB #MQ * ;                     \ flattened rows B*T
: MHA-CC ( -- n )  #MC ;                            \ channels C
: MHA-3C ( -- n )  #MC 3 * ;                        \ fused QKV width 3C (Q|K|V column blocks)
: MHA-HM ( -- n )  #MB #MH * #MQ * #MD * ;          \ head-major Q/K/V/O elements = B*T*C
: MHA-SS ( -- n )  #MB #MH * #MQ * #MK * ;          \ scores/attn elements B*H*T*T
: MHA-INV ( -- r )  1.0 #MD s>f fsqrt f/ ;          \ 1/sqrt(hd)

\ ---- head-major batched tensors (b,h leading = the replication axes) -----------------
TENSOR: MHA-Q ( #MB #MH #MQ #MD )     \ Q head-major (B,H,T,hd)
TENSOR: MHA-K ( #MB #MH #MK #MD )     \ K head-major
TENSOR: MHA-V ( #MB #MH #MK #MD )     \ V head-major
TENSOR: MHA-S ( #MB #MH #MQ #MK )     \ scores (B,H,T,T)
TENSOR: MHA-A ( #MB #MH #MQ #MK )     \ attention weights (B,H,T,T)
TENSOR: MHA-O ( #MB #MH #MQ #MD )     \ context head-major (B,H,T,hd)

\ ---- the two batched contractions (each a checked SPEC: line; SPEC: also derives the
\ transposed adjoints MHA-SCORE-ADJ0/1 and MHA-CTX-ADJ0/1, batch extents riding along) --
SPEC: MHA-SCORE  MHA-S[mb mh mq mk] = MHA-Q[mb mh mq md] MHA-K[mb mh mk md] * +SUM md ;   \ S = Q.K^T per (b,h)
SPEC: MHA-CTX    MHA-O[mb mh mq md] = MHA-A[mb mh mq mk] MHA-V[mb mh mk md] * +SUM mk ;   \ O = A.V  per (b,h)

private

\ ---- private saved-activation scratch (the backward tape) -----------------------------
create MHA-QKVF MHA-BT MHA-3C * cells allot   \ fused flat projection (B*T,3C): Q|K|V column blocks
create MHA-QM MHA-HM cells allot         \ head-major projections (B,H,T,hd)
create MHA-KM MHA-HM cells allot
create MHA-VM MHA-HM cells allot
create MHA-SM MHA-SS cells allot         \ scaled scores
create MHA-AM MHA-SS cells allot         \ causal-softmax attention (saved for the VJP)
create MHA-OM MHA-HM cells allot         \ head-major context
create MHA-OF MHA-BT #MC * cells allot    \ merged context (B*T,C), saved for dWo
create MHA-YF MHA-BT #MC * cells allot    \ sublayer output staging (alias-safe)

\ ---- backward scratch ----------------------------------------------------------------
create MHA-dOF MHA-BT #MC * cells allot
create MHA-dOM MHA-HM cells allot
create MHA-dAM MHA-SS cells allot
create MHA-dSM MHA-SS cells allot
create MHA-dQM MHA-HM cells allot
create MHA-dKM MHA-HM cells allot
create MHA-dVM MHA-HM cells allot
create MHA-dQKVF MHA-BT MHA-3C * cells allot   \ fused flat projection cotangent (B*T,3C)
create MHA-dXT MHA-BT #MC * cells allot   \ dX contribution accumulator (fused proj.W^T)

\ ---- host glue -----------------------------------------------------------------------
: MHA-COPY ( ptr r ptr r n -- ) {: s:ptr d:ptr n:n :}  n 0 ?do  s i T-GET  d i T-SET  loop ;

\ yb[r,c] += bb[c] : add a length-C bias to every one of the B*T rows (row broadcast).
: MHA-ROWBIAS ( ptr r n n ptr r -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do  cols 0 ?do  yb j cols * i + T-GET  bb i T-GET f+  yb j cols * i + T-SET  loop  loop ;

\ db[c] = sum_r sb[r,c] : the bias adjoint (column sum over the B*T rows).
: MHA-COLSUM ( ptr r n n ptr r -- ) {: sb:ptr rows:n cols:n db:ptr :}
   cols 0 ?do
      i {: c:n :}
      0.0  rows 0 ?do  sb  i cols * c +  T-GET  f+  loop
      db c T-SET
   loop ;

\ head-major linear index e -> the corresponding flat (B*T,C) index. e decomposes as
\ (b,h,t,d) over (B,H,T,hd); the flat position is (b*T+t)*C + h*hd + d. Four nested axes
\ exceed habu's two loop counters, so both directions iterate ONE linear index and map.
: MHA-FLATIDX ( n -- n ) {: e:n :}
   e #MD mod {: d:n :}
   e #MD / #MQ mod {: t:n :}
   e #MD #MQ * / #MH mod {: h:n :}
   e #MD #MQ * #MH * / {: b:n :}
   b #MQ * t +  #MC *  h #MD * +  d + ;
: MHA-FLAT>HM ( ptr r ptr r -- ) {: fb:ptr hb:ptr :}   \ hm[e] = flat[flatidx(e)]  (split)
   MHA-HM 0 ?do  fb i MHA-FLATIDX T-GET  hb i T-SET  loop ;
: MHA-HM>FLAT ( ptr r ptr r -- ) {: hb:ptr fb:ptr :}   \ flat[flatidx(e)] = hm[e]  (merge; the split's inverse)
   MHA-HM 0 ?do  hb i T-GET  fb i MHA-FLATIDX T-SET  loop ;

\ THE ONE QKV LAYOUT CONTRACT. head-major elem e=(b,h,t,d) -> its index in the FUSED
\ (B*T,3C) buffer's column block at `base` (base 0 = Q, #MC = K, 2*#MC = V): position
\ (b*T+t)*3C + base + h*hd + d. Forward split AND adjoint merge both derive from this one
\ map, so Q/K/V slices never disagree - the offset arithmetic that replaces strided views.
: MHA-QKVIDX ( n n -- n ) {: e:n base:n :}
   e #MD mod {: d:n :}
   e #MD / #MQ mod {: t:n :}
   e #MD #MQ * / #MH mod {: h:n :}
   e #MD #MQ * #MH * / {: b:n :}
   b #MQ * t +  MHA-3C *  base +  h #MD * +  d + ;
: MHA-QKV>HM ( ptr r ptr r n -- ) {: fb:ptr hb:ptr base:n :}   \ hm[e] = fused[qkvidx(e,base)] (split)
   MHA-HM 0 ?do  fb i base MHA-QKVIDX T-GET  hb i T-SET  loop ;
: MHA-HM>QKV ( ptr r ptr r n -- ) {: hb:ptr fb:ptr base:n :}   \ fused[qkvidx(e,base)] = hm[e] (merge)
   MHA-HM 0 ?do  hb i T-GET  fb i base MHA-QKVIDX T-SET  loop ;

\ per-(b,h) T x T block base (elements) for the causal row softmax + its VJP.
: MHA-BLK ( n -- n )  #MQ #MK * * ;

\ the fused QKV projection: ONE X.Wqkv GEMM into the (B*T,3C) buffer, ONE combined-bias
\ row broadcast, then split the three column blocks to head-major by offset arithmetic -
\ no materialized Q/K/V copy beyond the single fused buffer. wb: Wqkv (C,3C); bb: bqkv (3C).
: MHA-QKVPROJ ( ptr r ptr r ptr r -- ) {: xb:ptr wb:ptr bb:ptr :}
   xb wb MHA-QKVF  MHA-BT #MC MHA-3C  MATMUL     \ one fused contraction (X read once)
   MHA-QKVF MHA-BT MHA-3C bb MHA-ROWBIAS         \ one combined bias over all 3C columns
   MHA-QKVF MHA-QM 0        MHA-QKV>HM           \ Q = block [0,C)   -> head-major
   MHA-QKVF MHA-KM #MC      MHA-QKV>HM           \ K = block [C,2C)  -> head-major
   MHA-QKVF MHA-VM #MC 2 *  MHA-QKV>HM ;         \ V = block [2C,3C) -> head-major

\ the scaled causal-softmax attention over all (b,h) score blocks: scale S in place,
\ then the segment-causal row softmax per block into A (saved for the VJP).
: MHA-ATTEND ( -- )
   MHA-SM MHA-SS MHA-INV ATTN-SCALE!
   #MB #MH * 0 ?do
      i MHA-BLK {: off:n :}
      MHA-SM off T-AT  MHA-AM off T-AT  #MQ  CAUSAL-SOFTMAX-ROWS
   loop ;

public

\ MHA-FWD - the full trainable sublayer forward. xb: input X (B*T,C row-major; row b*T+t
\ is batch b position t). wqkvb/bqkvb: the fused GPT-2 c_attn projection weight (C,3C) and
\ combined bias (3C) - Q|K|V are its column blocks. wob/bob: output projection weight (C,C)
\ and bias (C). yb: output Y (B*T,C) = residual(X) + attention. Saves the tape for MHA-BWD.
: MHA-FWD ( ptr r ptr r ptr r ptr r ptr r ptr r -- )
   {: xb:ptr wqkvb:ptr bqkvb:ptr wob:ptr bob:ptr yb:ptr :}
   xb wqkvb bqkvb MHA-QKVPROJ                   \ Q|K|V = X.Wqkv + bqkv (one GEMM) -> head-major
   MHA-QM MHA-Q-BIND  MHA-KM MHA-K-BIND  MHA-SM MHA-S-BIND  MHA-SCORE   \ S = Q.K^T per (b,h)
   MHA-ATTEND                                                          \ scale + causal softmax -> A
   MHA-AM MHA-A-BIND  MHA-VM MHA-V-BIND  MHA-OM MHA-O-BIND  MHA-CTX     \ O = A.V per (b,h)
   MHA-OM MHA-OF MHA-HM>FLAT                    \ merge heads -> flat (B*T,C)
   MHA-OF wob MHA-YF  MHA-BT #MC #MC  MATMUL    \ output projection O.Wo
   MHA-YF MHA-BT #MC bob MHA-ROWBIAS            \ + output bias bo (row broadcast)
   MHA-YF xb MHA-BT #MC * T-ADD!                \ + residual X (into private staging)
   MHA-YF yb MHA-BT #MC * MHA-COPY ;            \ single final write (alias-safe)

\ MHA-BWD - the sublayer backward: given the output cotangent dyb, produce the adjoints
\ of X and every weight/bias. Reads the activation tape MHA-FWD saved. xb + the two weights
\ (Wqkv, Wo) are the forward inputs the adjoints reference (biases are not needed - their
\ adjoint is a column sum of the projection-output cotangent). dxb: dX (B*T,C). The grad
\ outputs mirror the fused forward params: dwqkvb (C,3C), dbqkvb (3C), dwob, dbob.
: MHA-BWD ( ptr r ptr r ptr r ptr r ptr r ptr r ptr r ptr r ptr r -- )
   {: xb:ptr wqkvb:ptr wob:ptr dyb:ptr dxb:ptr
      dwqkvb:ptr dbqkvb:ptr dwob:ptr dbob:ptr :}
   \ output projection backward (Y = O.Wo + bo + X): dbo, dWo, dO_flat
   dyb MHA-BT #MC dbob MHA-COLSUM
   MHA-OF dyb dwob  MHA-BT #MC #MC  MATMUL-DW
   dyb wob MHA-dOF  MHA-BT #MC #MC  MATMUL-DX
   MHA-dOF MHA-dOM MHA-FLAT>HM                  \ split dO_flat -> head-major
   \ context backward (O = A.V): dA, dV
   MHA-dOM MHA-O-BIND  MHA-VM MHA-V-BIND  MHA-dAM MHA-A-BIND  MHA-CTX-ADJ0
   MHA-dOM MHA-O-BIND  MHA-AM MHA-A-BIND  MHA-dVM MHA-V-BIND  MHA-CTX-ADJ1
   \ causal-softmax VJP per (b,h) block (reads the saved A): dA -> dS_scaled
   #MB #MH * 0 ?do
      i MHA-BLK {: off:n :}
      MHA-dAM off T-AT  MHA-AM off T-AT  MHA-dSM off T-AT  #MQ  CAUSAL-SMBWD-ROWS
   loop
   MHA-dSM MHA-SS MHA-INV ATTN-SCALE!           \ undo the 1/sqrt(hd) scale
   \ score backward (S = Q.K^T): dQ, dK
   MHA-dSM MHA-S-BIND  MHA-KM MHA-K-BIND  MHA-dQM MHA-Q-BIND  MHA-SCORE-ADJ0
   MHA-dSM MHA-S-BIND  MHA-QM MHA-Q-BIND  MHA-dKM MHA-K-BIND  MHA-SCORE-ADJ1
   MHA-dQM MHA-dQKVF 0        MHA-HM>QKV         \ merge dQ|dK|dV into ONE fused (B*T,3C) buffer
   MHA-dKM MHA-dQKVF #MC      MHA-HM>QKV         \   by the same layout contract - no extra copy
   MHA-dVM MHA-dQKVF #MC 2 *  MHA-HM>QKV
   \ fused projection backward (Q|K|V = X.Wqkv + bqkv): dWqkv = X^T.dQKV, dbqkv = colsum,
   \ dX += dQKV.Wqkv^T - one GEMM each (X read once), same scalars as the three-block form.
   xb MHA-dQKVF dwqkvb  MHA-BT #MC MHA-3C  MATMUL-DW
   MHA-dQKVF MHA-BT MHA-3C dbqkvb MHA-COLSUM
   dyb dxb MHA-BT #MC * MHA-COPY                \ dX starts at the residual passthrough
   MHA-dQKVF wqkvb MHA-dXT  MHA-BT #MC MHA-3C  MATMUL-DX   dxb MHA-dXT MHA-BT #MC * T-ADD! ;

;package
