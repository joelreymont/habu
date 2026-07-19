\ maki/gptblock-test.f - the GPT-2 transformer-block WHOLE-MODEL composition
\ (dot habu-gpt-2-block): token+pos embed -> pre-LN(affine) -> MLP(LINEAR GELU
\ LINEAR) -> residual -> final affine LN -> LM head, captured as ONE MODEL:,
\ gradchecked, and trained end-to-end with softmax cross-entropy on INTEGER
\ targets. Tiny fixed shapes (T=5, C=4, V=6, H=8): the composition proof, not
\ GPT-2-small at scale.
\
\ TWO STRUCTURAL WALLS shaped this composition (both proven, not worked around):
\
\ 1. THE MHA SUBLAYER IS NOT A DIFFERENTIABLE MODEL: NODE. The single-running-value
\    MODEL: DSL cannot root an internal Q@K^T (each rooted op needs the one running
\    value; closed dot habu-multi-head-self-a1e0692f), so maki/mha.f is a forward-only
\    buffer golden with no CAD adjoint. A block whose attention derives Q/K/V from the
\    running representation therefore cannot live in the differentiable graph and
\    cannot train end-to-end through the DSL. This file delivers the largest HONEST
\    end-to-end-trainable sub-composition: the block with the MHA sublayer omitted
\    (embed -> pre-LN -> MLP -> residual -> final-LN -> LM head). The attention CORE is
\    separately trainable only with PRE-PROJECTED Q/Kt/V as independent inputs
\    (maki/adam-attn-grad-test.f ADAM-ATTN), which is a different object than a block
\    that attends over its own running value.
\
\ 2. THE MODEL: NAMED-REFERENCE QUEUE CAPS AT 4 PER BODY. cad.f CAP-PEND-CAP=4 and the
\    queue's push index is never reset across a body (CAP-PEND-RESET runs only in
\    CAP-BEGIN), so at most four `>V`/by-name references may appear in one MODEL:.
\    Falsifying evidence: a body with a 5th named reference throws E-CAD-REF (-5029).
\    The block needs a pre-LN affine (gamma,beta = 2 refs), a residual skip (1 ref),
\    and a final affine LN (gamma,beta = 2 refs) = 5 named refs > 4. To fit, the FINAL
\    affine LN is composed from primitives whose params AUTO-DRAIN off the declared-
\    input cursor (LAYERNORM -> BCAST-MUL by gamma -> BIAS by beta) instead of the
\    fused arity-3 OP-LAYERNORM; the pre-LN uses the fused affine op (the arity-3
\    exemplar, maki/layernorm-affine-op-test.f). Both LNs are affine and their
\    gamma/beta receive gradients; the composition uses exactly 3 named refs.
\
\ Proofs: (A) forward golden - the executor forward equals an independent layer-by-
\ layer reference (TOKPOS-EMBED, LN-AFFINE-FWD, MATMUL+bias, GELU-F, LN-FWD) cell for
\ cell; (B) GC-RUN V-PASS - every trained input's analytic grad matches central FD;
\ (C) BW-HAS-GRAD? enumerated over all 13 slots (12 params grad, the integer ids do
\ not); (D) SGD training - the mean CE is STRICTLY decreasing and DETERMINISTIC over
\ 60 steps, from 1802 to 397 milli (more than halved); (E) two block-boundary shape
\ rejects fire (the affine-LN 1xC gamma guard and the residual equal-shape guard).
\ maki -> habu only.

require lib/test.f
require lib/float.f
require maki/cad.f
require maki/gradcheck.f
require maki/backward.f
require maki/executor.f
require maki/loss-tensor.f
require maki/array.f
require maki/embedding.f
require maki/layernorm.f
require maki/gelu.f
require maki/matmul.f

package MAKI

\ ---- shapes (positions, channels, vocab, MLP hidden) + logit element count -------
5 constant GT   4 constant GC   6 constant GV   8 constant GH   30 constant GON

\ ---- bound host buffers: 13 model inputs (params + integer ids) + targets + seed --
\ signature order = capture order; the auto-drained params (ids, wpe, w1, b1, w2, b2,
\ gfin, bfin, wlm, blm) come first so the declared-input cursor walks them in order,
\ the named-reference params (ln1g, ln1b) last so the cursor never reaches them.
create G-WTE 24 cells allot   create G-IDS 5 cells allot   create G-WPE 20 cells allot
create G-W1 32 cells allot    create G-B1 8 cells allot
create G-W2 32 cells allot    create G-B2 4 cells allot
create G-GF 4 cells allot     create G-BF 4 cells allot
create G-WLM 24 cells allot   create G-BLM 6 cells allot
create G-LG 4 cells allot     create G-LB 4 cells allot
create G-TGT 5 cells allot    create G-SEED GON cells allot

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -> small param init ---
variable G-RNG
: G-NEXT ( -- r )  G-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup G-RNG !  s>f 4294967296.0 f/ ;
: G-UNIT ( -- r )  G-NEXT 2.0 f* 1.0 f- ;
: G-SMALL ( ptr a n -- ) {: p:ptr n:n :}  n 0 ?do  G-UNIT 0.1 f*  p i T-SET  loop ;

\ committed init: small weights, gamma=1/beta=0 for both affine LNs, and a copy task
\ (token id t = t mod V, target class t = t mod V) the block learns through wte + head.
: G-INIT ( -- )
   $C0FFEE G-RNG !
   G-WTE 24 G-SMALL  G-WPE 20 G-SMALL
   G-W1 32 G-SMALL   G-B1 8 G-SMALL   G-W2 32 G-SMALL   G-B2 4 G-SMALL
   G-WLM 24 G-SMALL  G-BLM 6 G-SMALL
   GC 0 ?do  1.0 G-GF i T-SET  0.0 G-BF i T-SET  1.0 G-LG i T-SET  0.0 G-LB i T-SET  loop
   GT 0 ?do  i GV mod s>f  G-IDS i T-SET   i GV mod s>f  G-TGT i T-SET  loop ;

\ ---- executor forward output (the last node = the 5x6 logits) + per-slot gradient -
: G-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: G-GRAD ( n -- ptr a )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;
: G-INVR ( -- r )  1.0 GT s>f f/ ;

\ write the mean-scaled y-onehot(target) seed cotangent from the logits; return mean CE
: G-LOSS-SEED ( -- r )
   G-OUT {: ob:ptr :}
   ob G-TGT G-SEED GT GV GT LOSS:TT-XENT-SEED
   GON 0 ?do  G-SEED i T-GET G-INVR f*  G-SEED i T-SET  loop
   ob GT GV G-TGT GT LOSS:TT-XENT  G-INVR f* ;

\ ---- SGD trainer: forward slice -> loss + seed -> full IR -> update every param ----
: G-LR ( -- r )  0.05 ;
: G-UPD ( ptr a n n -- ) {: p:ptr slot:n n:n :}  G-LR p slot G-GRAD n T-SGD! ;
: G-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   G-LOSS-SEED {: loss:r :}
   EX-RUN
   G-WTE 0 24 G-UPD   G-WPE 2 20 G-UPD
   G-W1 3 32 G-UPD    G-B1 4 8 G-UPD   G-W2 5 32 G-UPD   G-B2 6 4 G-UPD
   G-GF 7 4 G-UPD     G-BF 8 4 G-UPD
   G-WLM 9 24 G-UPD   G-BLM 10 6 G-UPD
   G-LG 11 4 G-UPD    G-LB 12 4 G-UPD
   loss ;

\ build backward once, bind every host buffer (params + integer ids + seed cotangent)
: G-SETUP ( -- )
   G-INIT  BW-BUILD  EX-RESET
   G-WTE 0 MIR-SLOT-ID EX-BIND   G-IDS 1 MIR-SLOT-ID EX-BIND   G-WPE 2 MIR-SLOT-ID EX-BIND
   G-W1 3 MIR-SLOT-ID EX-BIND    G-B1 4 MIR-SLOT-ID EX-BIND
   G-W2 5 MIR-SLOT-ID EX-BIND    G-B2 6 MIR-SLOT-ID EX-BIND
   G-GF 7 MIR-SLOT-ID EX-BIND    G-BF 8 MIR-SLOT-ID EX-BIND
   G-WLM 9 MIR-SLOT-ID EX-BIND   G-BLM 10 MIR-SLOT-ID EX-BIND
   G-LG 11 MIR-SLOT-ID EX-BIND   G-LB 12 MIR-SLOT-ID EX-BIND
   G-SEED BW-SEED-SLOT@ EX-BIND ;

\ ---- training run: records init/final mean CE and counts non-decreasing steps -----
variable G-IL   variable G-FL   variable G-BAD   variable G-PREV
: G-RUN ( n -- ) {: n:n :}
   0 G-BAD !  1000000.0 G-PREV !
   n 0 ?do
      G-STEP {: l:r :}
      i 0=     if l G-IL ! then
      i n 1- = if l G-FL ! then
      l G-PREV @ f< 0= if G-BAD @ 1+ G-BAD ! then
      l G-PREV !
   loop ;
: G-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;   \ committed loss lock (proves determinism)

\ ---- independent layer-by-layer forward reference (forward golden) ----------------
create R-X0 20 cells allot   create R-LN1 20 cells allot   create R-H 40 cells allot
create R-M 20 cells allot    create R-F 20 cells allot     create R-LOG 30 cells allot
: R-ROWBIAS ( ptr a n n ptr a -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do cols 0 ?do  yb j cols * i + T-GET  bb i T-GET f+  yb j cols * i + T-SET  loop loop ;
: R-ROWMUL ( ptr a n n ptr a -- ) {: yb:ptr rows:n cols:n gb:ptr :}
   rows 0 ?do cols 0 ?do  yb j cols * i + T-GET  gb i T-GET f*  yb j cols * i + T-SET  loop loop ;
: R-FWD ( -- )
   G-WTE G-IDS G-WPE R-X0  GT GT GC  TOKPOS-EMBED                       \ wte[ids] + wpe
   GT 0 ?do  R-X0 i GC * T-AT  R-LN1 i GC * T-AT  G-LG G-LB GC LN-AFFINE-FWD  loop  \ pre-LN affine
   R-LN1 G-W1 R-H  GT GC GH  MATMUL   R-H GT GH G-B1 R-ROWBIAS          \ LINEAR w1,b1
   GT GH * 0 ?do  R-H i T-GET GELU-F  R-H i T-SET  loop                 \ GELU
   R-H G-W2 R-M  GT GH GC  MATMUL   R-M GT GC G-B2 R-ROWBIAS            \ LINEAR w2,b2
   GT GC * 0 ?do  R-M i T-GET  R-X0 i T-GET f+  R-M i T-SET  loop       \ residual + X0
   GT 0 ?do  R-M i GC * T-AT  R-F i GC * T-AT  GC LN-FWD  loop          \ final LN (normalize)
   R-F GT GC G-GF R-ROWMUL   R-F GT GC G-BF R-ROWBIAS                   \ * gamma ; + beta (affine)
   R-F G-WLM R-LOG  GT GC GV  MATMUL   R-LOG GT GV G-BLM R-ROWBIAS ;    \ LM head -> logits

\ ---- block-boundary shape rejects (catchable EW-SHAPE-CHECK probes) ---------------
: G-GAMMA-BAD ( -- )                 \ pre-LN gamma 2xC vs x 5xC: not 1xC row-broadcast -> reject
   TENSOR:TV-RESET
   GT GC SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   2  GC SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:LAYERNORM EW-SHAPE-CHECK ;
: G-RESID-BAD ( -- )                 \ residual skip 4xC vs running value 5xC: unequal -> reject
   TENSOR:TV-RESET
   GT GC SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   4  GC SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC
   MAKI-OPKIND:RESIDUAL-ADD EW-SHAPE-CHECK ;

T-RESET

\ ================= (1) the whole-model composition captures the block ==============
MODEL: GPTBLK ( wte:6x4 ids:5x1 wpe:5x4 w1:4x8 b1:1x8 w2:8x4 b2:1x4 gfin:1x4 bfin:1x4 wlm:4x6 blm:1x6 ln1g:1x4 ln1b:1x4 -- logits )
   GATHER  ADD  >V X0
   ln1g ln1b LAYERNORM
   LINEAR  GELU  LINEAR
   X0 RESIDUAL-ADD
   LAYERNORM  BCAST-MUL  BIAS
   LINEAR ;
MODEL-K 11 T=                                                 \ 11 op nodes
0  MIR-NODE-ID MIR-OP@ OPR-NAME s" gather"       STR= TTRUE   \ wte[ids]
1  MIR-NODE-ID MIR-OP@ OPR-NAME s" add"          STR= TTRUE   \ + wpe
2  MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm"    STR= TTRUE   \ pre-LN
2  MIR-NODE-ID MIR-IN-COUNT@ 3 T=                             \ affine (x, gamma, beta)
3  MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE   \ MLP w1,b1
4  MIR-NODE-ID MIR-OP@ OPR-NAME s" gelu"         STR= TTRUE
5  MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE   \ MLP w2,b2
6  MIR-NODE-ID MIR-OP@ OPR-NAME s" residual-add" STR= TTRUE   \ + X0 skip
7  MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm"    STR= TTRUE   \ final LN
8  MIR-NODE-ID MIR-OP@ OPR-NAME s" bcast-mul"    STR= TTRUE   \ final-LN gamma
9  MIR-NODE-ID MIR-OP@ OPR-NAME s" bias"         STR= TTRUE   \ final-LN beta
10 MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE   \ LM head -> logits

\ ================= (2) gradcheck: every trained input's analytic grad = FD =========
GC-RUN V-PASS T=
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE

\ ================= (3) per-slot gradient enumeration ===============================
MODEL: GPTBLK ( wte:6x4 ids:5x1 wpe:5x4 w1:4x8 b1:1x8 w2:8x4 b2:1x4 gfin:1x4 bfin:1x4 wlm:4x6 blm:1x6 ln1g:1x4 ln1b:1x4 -- logits )
   GATHER  ADD  >V X0
   ln1g ln1b LAYERNORM
   LINEAR  GELU  LINEAR
   X0 RESIDUAL-ADD
   LAYERNORM  BCAST-MUL  BIAS
   LINEAR ;
G-SETUP
0  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wte  (token embedding, via gather scatter-add)
1  MIR-SLOT-ID BW-HAS-GRAD? TFALSE    \ ids  (integer index operand - never a gradient)
2  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wpe  (positional table)
3  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ w1
4  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ b1
5  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ w2
6  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ b2
7  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ gfin (final-LN gamma)
8  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ bfin (final-LN beta)
9  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wlm  (LM head weight)
10 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ blm  (LM head bias)
11 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln1g (pre-LN gamma)
12 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln1b (pre-LN beta)

\ ================= (4) forward golden: executor == layer-by-layer reference ========
BW-FWD-N@ EX-RUN-N
R-FWD
G-OUT R-LOG GON T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=        \ cell-for-cell equal

\ ================= (5) training: strictly decreasing, deterministic mean CE ========
60 G-RUN
G-FL @ G-IL @ f< TTRUE                         \ loss decreased
G-FL @ G-IL @ 0.5 f* f< TTRUE                  \ at least halved
G-BAD @ 0 T=                                   \ strictly decreasing (no non-decreasing step)
G-IL @ G-MILLI 1802 T=                         \ committed initial mean CE (determinism lock)
G-FL @ G-MILLI 397 T=                          \ committed final mean CE (determinism lock)

\ ================= (6) block-boundary shape rejects fire ===========================
' G-GAMMA-BAD E-CAD-PARAM-SHAPE TTHROWS         \ affine-LN gamma must be 1xC
' G-RESID-BAD E-CAD-PARAM-SHAPE TTHROWS         \ residual skip must equal the running value

T-REPORT

;package
