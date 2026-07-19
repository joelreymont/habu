\ maki/mha.f - multi-head causal self-attention sublayer forward, authored as
\ SPEC: contraction lines composed by plain checked colon words with named row ops.
\
\ Single sequence (no batch): T positions, C channels, H heads, head dim hd = C/H.
\ The sublayer is  Y = out_proj( concat_h softmax_causal( Q_h.K_h^T / sqrt(hd) ) . V_h )
\ where Q_h = X.WQ_h, K_h = X.WK_h, V_h = X.WV_h are the per-head projections. Every
\ CONTRACTION is one SPEC: line (the checked candidate-B golden, certified through
\ XG-EVAL); the non-contraction steps stay named ops: the 1/sqrt(hd) scale
\ (maki/attention.f ATTN-SCALE!) and the causal-masked row softmax (maki/causal.f
\ CAUSAL-SOFTMAX-ROWS). This mirrors maki/spec-attention-test.f's SPAT-FWD, extended
\ from one head to H heads and from scores/A.V to the full projections.
\
\ HEAD SPLIT / MERGE ARE CONTRACTIONS, NOT COPIES:
\   - The split is folded into the QKV projection: each head is projected with its
\     own weight block, so MHA-QPROJ/KPROJ/VPROJ (looped over heads, one weight
\     block bound per head) write the per-head (T,hd) activation directly - no
\     strided column view, no movement op. Weights are head-major (H,C,hd); the
\     activations are head-major (H,T,hd).
\   - The merge is folded into the output projection: MHA-OUTPROJ is ONE contraction
\     over the composite (head, head-dim) index -
\       Y[hq hc] = O[hh hq hd] WO[hh hd hc] * +SUM hh hd
\     summing every head's contribution into Y with no concat buffer and no
\     per-head accumulation. This is exactly  Y = concat_h(O_h) . Wo  with Wo
\     reshaped head-major (H,hd,C).
\
\ POSITION ROLES: query positions #HQ and key positions #HN are DISTINCT nominal
\ extents that share the magnitude T (self-attention), so Q.K^T is the transposed-
\ operand GEMM the checker accepts while a swapped operand is a reject (see the
\ MHA-COK / MHA-CFLIP candidates in maki/mha-test.f). MHA-CONFIG-CHECK enforces
\ #HQ = #HN and C = H*hd at load.
\
\ FIXED EXTENTS: SPEC: bakes the extent magnitudes into the generated accessors, so
\ this golden runs at one toy shape (T=4, C=6, H=2, hd=3), like every SPEC: golden
\ and like the toy-scale segment reference (maki/segment.f). Real GPT-2 shapes and
\ the batch dimension arrive via PROMOTE and the extent-role product capability
\ (habu-extent-role-product-8e364885); bias/residual become SPEC broadcast forms
\ under habu-spec-broadcast-forms-ad851424 and stay named ops here meanwhile.
\ maki -> habu only; mha owns -5151.

require maki/array.f          \ T-AT / T-GET / T-SET / T-ADD! : buffer + residual add
require maki/attention.f      \ ATTN-SCALE! : the 1/sqrt(hd) score scale (named row op)
require maki/causal.f         \ CAUSAL-SOFTMAX-ROWS : the causal-masked row softmax (named row op)
require maki/spec.f           \ SPEC: / TENSOR: / EXTENT: : the checked contraction goldens

-5151 constant E-MHA-SHAPE   \ head geometry inconsistent: C != H*hd, or query/key extents unequal

package MAKI

\ ---- head geometry (single sequence). Query and key positions are distinct roles
\ that share the magnitude T; channels C = H * hd. ----------------------------------
4 EXTENT: #HQ    \ query positions (sequence length T)
4 EXTENT: #HN    \ key positions (= T; a distinct nominal role from the query axis)
6 EXTENT: #HC    \ channels C
3 EXTENT: #HD    \ head dim hd = C / H
2 EXTENT: #HH    \ heads H

\ every extent magnitude is a compile-time SPEC: constant, so the geometry law is a
\ load-time named throw, never a silent wrong-shape run.
: MHA-CONFIG-CHECK ( -- )
   #HQ #HN <> if E-MHA-SHAPE throw then           \ self-attention: query positions = key positions
   #HC #HH #HD * <> if E-MHA-SHAPE throw then ;   \ channels = heads * head-dim
MHA-CONFIG-CHECK

\ ---- extent-typed tensor accessors. X carries two positional views (query rows for
\ Q, key rows for K/V) over ONE buffer; activations/weights are head-major so each
\ head's block is contiguous. --------------------------------------------------------
TENSOR: MHA-X  ( #HQ #HC )       \ input, query-row view
TENSOR: MHA-XK ( #HN #HC )       \ input, key-row view (same buffer, key-position role)
TENSOR: MHA-WQ ( #HC #HD )       \ one head's query weight block (C, hd)
TENSOR: MHA-WK ( #HC #HD )       \ one head's key weight block
TENSOR: MHA-WV ( #HC #HD )       \ one head's value weight block
TENSOR: MHA-QH ( #HQ #HD )       \ one head's Q (T, hd)
TENSOR: MHA-KH ( #HN #HD )       \ one head's K (T, hd)
TENSOR: MHA-VH ( #HN #HD )       \ one head's V (T, hd)
TENSOR: MHA-SH ( #HQ #HN )       \ one head's scores (T, T)
TENSOR: MHA-AH ( #HQ #HN )       \ one head's attention weights (T, T)
TENSOR: MHA-OH ( #HQ #HD )       \ one head's attention output (T, hd)
TENSOR: MHA-O  ( #HH #HQ #HD )   \ head-major attention output (H, T, hd)
TENSOR: MHA-WO ( #HH #HD #HC )   \ head-major output weight (H, hd, C)
TENSOR: MHA-Y  ( #HQ #HC )       \ sublayer output (T, C)

\ ---- the contraction goldens (each a checked SPEC: line) ---------------------------
SPEC: MHA-QPROJ   MHA-QH[hq hd] = MHA-X[hq hc]  MHA-WQ[hc hd] * +SUM hc ;        \ Q_h = X . WQ_h
SPEC: MHA-KPROJ   MHA-KH[hn hd] = MHA-XK[hn hc] MHA-WK[hc hd] * +SUM hc ;        \ K_h = X . WK_h
SPEC: MHA-VPROJ   MHA-VH[hn hd] = MHA-XK[hn hc] MHA-WV[hc hd] * +SUM hc ;        \ V_h = X . WV_h
SPEC: MHA-SCORES  MHA-SH[hq hn] = MHA-QH[hq hd] MHA-KH[hn hd] * +SUM hd ;        \ S_h = Q_h . K_h^T
SPEC: MHA-AV      MHA-OH[hq hd] = MHA-AH[hq hn] MHA-VH[hn hd] * +SUM hn ;        \ O_h = A_h . V_h
SPEC: MHA-OUTPROJ MHA-Y[hq hc]  = MHA-O[hh hq hd] MHA-WO[hh hd hc] * +SUM hh hd ; \ Y = concat_h(O_h) . Wo

\ ---- internal head-major scratch. Query and key positions share magnitude T
\ (MHA-CONFIG-CHECK), so one T*hd block stride serves Q/K/V/O; S/A are reused per head.
create MHA-QSCR #HH #HQ * #HD * cells allot
create MHA-KSCR #HH #HQ * #HD * cells allot
create MHA-VSCR #HH #HQ * #HD * cells allot
create MHA-OSCR #HH #HQ * #HD * cells allot
create MHA-SSCR #HQ #HN * cells allot
create MHA-ASCR #HQ #HN * cells allot

: MHA-INV-SCALE ( -- r )  1.0 #HD s>f fsqrt f/ ;   \ 1/sqrt(hd)

\ the caller's ORIGINAL weight bases, so each head offsets from the base - never from
\ a previous head's already-offset binding.
variable MHA-WQ-ORG  variable MHA-WK-ORG  variable MHA-WV-ORG

\ one head's forward: project Q/K/V from X with this head's weight block, score,
\ scale, causal-softmax, then A.V - all into this head's contiguous blocks.
: MHA-HEAD ( n -- ) {: h:n :}
   h #HC #HD * *  {: woff:n :}                      \ head weight block base (cells)
   h #HQ #HD * *  {: boff:n :}                      \ head activation block base (cells)
   MHA-WQ-ORG @ woff T-AT MHA-WQ-BIND
   MHA-WK-ORG @ woff T-AT MHA-WK-BIND
   MHA-WV-ORG @ woff T-AT MHA-WV-BIND
   MHA-QSCR boff T-AT MHA-QH-BIND
   MHA-KSCR boff T-AT MHA-KH-BIND
   MHA-VSCR boff T-AT MHA-VH-BIND
   MHA-OSCR boff T-AT MHA-OH-BIND
   MHA-QPROJ  MHA-KPROJ  MHA-VPROJ                  \ Q_h, K_h, V_h = X . W_h
   MHA-SCORES                                       \ S_h = Q_h . K_h^T
   MHA-SSCR  #HQ #HN *  MHA-INV-SCALE  ATTN-SCALE!  \ S_h *= 1/sqrt(hd)
   MHA-SSCR MHA-ASCR #HQ  CAUSAL-SOFTMAX-ROWS       \ A_h = softmax_causal(S_h)
   MHA-AV ;                                         \ O_h = A_h . V_h

\ attention over all heads: bind the shared X views and score/attn scratch once, store
\ the weight bases, run each head, leaving every head's output in head-major MHA-OSCR.
: MHA-ATTN ( ptr a ptr a ptr a ptr a -- )
   {: xb:ptr wqb:ptr wkb:ptr wvb:ptr :}
   xb MHA-X-BIND   xb MHA-XK-BIND
   wqb MHA-WQ-ORG !  wkb MHA-WK-ORG !  wvb MHA-WV-ORG !
   MHA-SSCR MHA-SH-BIND   MHA-ASCR MHA-AH-BIND
   #HH 0 ?do  i MHA-HEAD  loop ;

\ the output projection merges the heads: Y = concat_h(O_h) . Wo in one contraction.
: MHA-OUT ( ptr a ptr a -- ) {: wob:ptr yb:ptr :}
   MHA-OSCR MHA-O-BIND   wob MHA-WO-BIND   yb MHA-Y-BIND
   MHA-OUTPROJ ;

public

\ MHA-O-SCRATCH - the head-major (H,T,hd) attention-output buffer MHA-FWD fills
\ before the output projection. Exposed so a test can prove per-head slicing: each
\ head's block must match its independently-computed reference and stay isolated
\ from the other heads' weights.
: MHA-O-SCRATCH ( -- ptr a )  MHA-OSCR ;

\ MHA-FWD - the core attention sublayer forward (no bias, no residual): from the
\ input X and the head-major QKV / output weights, write the attention output Y.
\ xb: X (T,C) ; wqb/wkb/wvb: head-major QKV weights (H,C,hd) ; wob: head-major output
\ weight (H,hd,C) ; yb: output Y (T,C).
: MHA-FWD ( ptr a ptr a ptr a ptr a ptr a ptr a -- )
   {: xb:ptr wqb:ptr wkb:ptr wvb:ptr wob:ptr yb:ptr :}
   xb wqb wkb wvb MHA-ATTN
   wob yb MHA-OUT ;

\ ROW-BIAS-ADD! - add a length-cols bias vector to every row (the named broadcast op
\ standing in until SPEC broadcast forms land): yb[r,c] += bb[c].
: ROW-BIAS-ADD! ( ptr a n n ptr a -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do
      cols 0 ?do
         yb  j cols * i +  T-GET   bb i T-GET  f+   yb  j cols * i +  T-SET
      loop
   loop ;

\ MHA-SUBLAYER-FWD - the full sublayer: core attention, then the output-projection
\ bias add and the residual add of the sublayer input X (both named ops).
\ bob: output bias (C) ; the residual adds xb (T,C) to Y.
: MHA-SUBLAYER-FWD ( ptr a ptr a ptr a ptr a ptr a ptr a ptr a -- )
   {: xb:ptr wqb:ptr wkb:ptr wvb:ptr wob:ptr bob:ptr yb:ptr :}
   xb wqb wkb wvb wob yb MHA-FWD
   yb #HQ #HC bob ROW-BIAS-ADD!            \ + output bias (broadcast over rows)
   yb xb #HQ #HC * T-ADD! ;               \ + residual (sublayer input X)

;package
