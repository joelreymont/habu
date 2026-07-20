\ maki/examples/nanogpt/gptblock-attn-test.f - the FULL GPT-2 transformer block
\ WHOLE-MODEL composition (dot habu-gpt-2-block): token+pos embed -> pre-LN ->
\ CAUSAL SELF-ATTENTION (real, differentiable, over the running value) -> residual
\ -> pre-LN -> MLP(LINEAR GELU LINEAR) -> residual -> final LN -> LM head, captured
\ as ONE MODEL:, gradchecked, and trained end-to-end with softmax cross-entropy on
\ INTEGER targets. The attention-less block (maki/examples/nanogpt/gptblock-test.f)
\ is kept as history; THIS file adds the two walls that once blocked it:
\
\ 1. ATTENTION IS NOW IN THE DIFFERENTIABLE GRAPH (wall #1, fell a5da3318). The
\    single-running-value DSL cannot root three projections from the running X, so
\    exactly ONE projection is a standalone MATMUL (Q = LN1.WQ, rooting from the
\    pre-LN running value) and the K and V projections FOLD into the score/context
\    einsums (maki/attn-eq.f A-SCORES / A-CTX, whose adjoints derive at declaration
\    time). Causal masking is an ADD of a mask constant before the row softmax - no
\    new op-kind. The context is projected back to C by an output MATMUL (WO) so it
\    can add to the residual.
\
\ 2. THE NAMED-REF QUEUE IS A RING (wall #2, fell 1bb03366). CAP-PEND-CAP now bounds
\    OUTSTANDING refs (= EQ-FCAP-1 = 7), consumed at each op, NOT the body total. The
\    block emits many named refs (X0 skip, LN1 folded into two einsums, X1 skip, three
\    affine LN gamma/beta pairs) but never more than 2 outstanding before one op, so
\    every LN here is the fused arity-3 affine op (the registry's declared form).
\
\ Shapes REUSE maki/attn-eq.f's registered einsums, so they are fixed at T=4, C=6,
\ d=3 (single head; #DAQ=#DAK=4, #DAC=6, #DAD=3). V=6 vocab, H=8 MLP hidden. Tiny
\ fixed extents: the composition proof, not GPT-2-small at scale.
\
\ Proofs: (A) structure - the 18-node capture is exactly the GPT-2 block; (B) GC-RUN
\ V-PASS - every trained input's analytic grad matches central FD, and the per-slot
\ enumeration asserts the two non-gradient/frozen slots (integer ids; the frozen mask
\ constant) alongside the trained ones; (C) forward golden - the executor forward
\ equals an independent layer-by-layer reference (the attention einsums mirror the
\ equation engine's contraction order) cell for cell; (D) training - Adam more than
\ HALVES the mean CE, deterministic (committed milli locks) and run-twice bit-identical;
\ (E) causality in-composition - with the real causal mask bound, every future-key
\ softmax weight decays to ~0 and query 0 attends only to key 0. maki -> habu only.

require lib/test.f
require lib/float.f
require maki/cad.f
require maki/backward.f
require maki/executor.f
require maki/gradcheck.f
require maki/loss-tensor.f
require maki/optim-tensor.f
require maki/array.f
require maki/attn-eq.f
require maki/embedding.f
require maki/layernorm.f
require maki/gelu.f
require maki/matmul.f
require maki/attention.f

package MAKI

\ ---- shapes: T positions, C channels, d head dim, V vocab, H MLP hidden ------------
4 constant GBT   6 constant GBC   3 constant GBD   6 constant GBV   8 constant GBH
24 constant GBON   \ logit elements = GBT*GBV

\ ---- bound host buffers: 21 model inputs. AUTO-DRAINED params (slots 1..13, walked by
\ the declared-input cursor in op order) come first; slot 0 (wte) is the initial running
\ value GATHER roots from; the NAMED-REF-only params (mask, the three affine LN pairs)
\ sit at slots 14..20 where the cursor never reaches - they enter only by name. ---------
create GB-WTE GBV GBC * cells allot   create GB-IDS GBT cells allot     create GB-WPE GBT GBC * cells allot
create GB-WQ GBC GBD * cells allot    create GB-WK GBC GBD * cells allot   create GB-SC 1 cells allot
create GB-WV GBC GBD * cells allot    create GB-WO GBD GBC * cells allot
create GB-W1 GBC GBH * cells allot    create GB-B1 GBH cells allot
create GB-W2 GBH GBC * cells allot    create GB-B2 GBC cells allot
create GB-WLM GBC GBV * cells allot   create GB-BLM GBV cells allot
create GB-MASK GBT GBT * cells allot
create GB-LG1 GBC cells allot        create GB-LB1 GBC cells allot
create GB-LG2 GBC cells allot        create GB-LB2 GBC cells allot
create GB-GF GBC cells allot         create GB-BF GBC cells allot
create GB-TGT GBT cells allot        create GB-SEED GBON cells allot

\ ---- Adam moment buffers (m,v) per trained param (all inputs except ids and mask) -----
create GB-WTEM GBV GBC * cells allot  create GB-WTEV GBV GBC * cells allot
create GB-WPEM GBT GBC * cells allot  create GB-WPEV GBT GBC * cells allot
create GB-WQM GBC GBD * cells allot   create GB-WQV GBC GBD * cells allot
create GB-WKM GBC GBD * cells allot   create GB-WKV GBC GBD * cells allot
create GB-SCM 1 cells allot         create GB-SCV 1 cells allot
create GB-WVM GBC GBD * cells allot   create GB-WVV GBC GBD * cells allot
create GB-WOM GBD GBC * cells allot   create GB-WOV GBD GBC * cells allot
create GB-W1M GBC GBH * cells allot   create GB-W1V GBC GBH * cells allot
create GB-B1M GBH cells allot        create GB-B1V GBH cells allot
create GB-W2M GBH GBC * cells allot   create GB-W2V GBH GBC * cells allot
create GB-B2M GBC cells allot        create GB-B2V GBC cells allot
create GB-WLMM GBC GBV * cells allot  create GB-WLMV GBC GBV * cells allot
create GB-BLMM GBV cells allot       create GB-BLMV GBV cells allot
create GB-LG1M GBC cells allot       create GB-LG1V GBC cells allot
create GB-LB1M GBC cells allot       create GB-LB1V GBC cells allot
create GB-LG2M GBC cells allot       create GB-LG2V GBC cells allot
create GB-LB2M GBC cells allot       create GB-LB2V GBC cells allot
create GB-GFM GBC cells allot        create GB-GFV GBC cells allot
create GB-BFM GBC cells allot        create GB-BFV GBC cells allot

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -> small param init -------
variable GB-RNG
: GB-NEXT ( -- r )  GB-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup GB-RNG !  s>f 4294967296.0 f/ ;
: GB-UNIT ( -- r )  GB-NEXT 2.0 f* 1.0 f- ;
: GB-SMALL ( ptr a n -- ) {: p:ptr n:n :}  n 0 ?do  GB-UNIT 0.1 f*  p i T-SET  loop ;

\ committed init: small weights, sc = 1/sqrt(d), gamma=1/beta=0 for all three affine LNs,
\ a copy task (token id t = t mod V, target class t = t mod V) the block learns end-to-end.
: GB-INIT ( -- )
   $C0FFEE GB-RNG !
   GB-WTE GBV GBC * GB-SMALL   GB-WPE GBT GBC * GB-SMALL
   GB-WQ GBC GBD * GB-SMALL    GB-WK GBC GBD * GB-SMALL   GB-WV GBC GBD * GB-SMALL   GB-WO GBD GBC * GB-SMALL
   GB-W1 GBC GBH * GB-SMALL    GB-B1 GBH GB-SMALL        GB-W2 GBH GBC * GB-SMALL   GB-B2 GBC GB-SMALL
   GB-WLM GBC GBV * GB-SMALL   GB-BLM GBV GB-SMALL
   1.0 GBD s>f fsqrt f/  GB-SC 0 T-SET
   GBC 0 ?do  1.0 GB-LG1 i T-SET  0.0 GB-LB1 i T-SET  1.0 GB-LG2 i T-SET  0.0 GB-LB2 i T-SET
             1.0 GB-GF i T-SET   0.0 GB-BF i T-SET  loop
   GBT 0 ?do  i GBV mod s>f  GB-IDS i T-SET   i GBV mod s>f  GB-TGT i T-SET  loop
   GB-MASK A-MASK-FILL ;

\ ---- Adam state (step count + running decay powers) -----------------------------------
: GB-BET1 ( -- r ) 0.9 ;  : GB-BET2 ( -- r ) 0.999 ;  : GB-EPS ( -- r ) 0.00000001 ;
variable GB-T  variable GB-B1T  variable GB-B2T
: GB-ARESET ( -- )  0 GB-T !  1.0 GB-B1T !  1.0 GB-B2T !
   0.0 GB-WTEM GBV GBC * T-FILL 0.0 GB-WTEV GBV GBC * T-FILL 0.0 GB-WPEM GBT GBC * T-FILL 0.0 GB-WPEV GBT GBC * T-FILL
   0.0 GB-WQM GBC GBD * T-FILL 0.0 GB-WQV GBC GBD * T-FILL 0.0 GB-WKM GBC GBD * T-FILL 0.0 GB-WKV GBC GBD * T-FILL
   0.0 GB-SCM 1 T-FILL 0.0 GB-SCV 1 T-FILL 0.0 GB-WVM GBC GBD * T-FILL 0.0 GB-WVV GBC GBD * T-FILL
   0.0 GB-WOM GBD GBC * T-FILL 0.0 GB-WOV GBD GBC * T-FILL 0.0 GB-W1M GBC GBH * T-FILL 0.0 GB-W1V GBC GBH * T-FILL
   0.0 GB-B1M GBH T-FILL 0.0 GB-B1V GBH T-FILL 0.0 GB-W2M GBH GBC * T-FILL 0.0 GB-W2V GBH GBC * T-FILL
   0.0 GB-B2M GBC T-FILL 0.0 GB-B2V GBC T-FILL 0.0 GB-WLMM GBC GBV * T-FILL 0.0 GB-WLMV GBC GBV * T-FILL
   0.0 GB-BLMM GBV T-FILL 0.0 GB-BLMV GBV T-FILL 0.0 GB-LG1M GBC T-FILL 0.0 GB-LG1V GBC T-FILL
   0.0 GB-LB1M GBC T-FILL 0.0 GB-LB1V GBC T-FILL 0.0 GB-LG2M GBC T-FILL 0.0 GB-LG2V GBC T-FILL
   0.0 GB-LB2M GBC T-FILL 0.0 GB-LB2V GBC T-FILL 0.0 GB-GFM GBC T-FILL 0.0 GB-GFV GBC T-FILL
   0.0 GB-BFM GBC T-FILL 0.0 GB-BFV GBC T-FILL ;
: GB-TICK ( -- )  GB-T @ 1+ GB-T !  GB-B1T @ GB-BET1 f* GB-B1T !  GB-B2T @ GB-BET2 f* GB-B2T ! ;
: GB-C1 ( -- r ) 1.0 GB-B1T @ f- ;  : GB-C2 ( -- r ) 1.0 GB-B2T @ f- ;

\ ---- executor output (last forward node = the logits) + per-slot gradient node --------
: GB-OUT ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: GB-GRAD ( n -- ptr a )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;
: GB-INVR ( -- r )  1.0 GBT s>f f/ ;

\ write the mean-scaled y-onehot(target) seed cotangent from the logits; return mean CE
: GB-LOSS-SEED ( -- r )
   GB-OUT {: ob:ptr :}
   ob GB-TGT GB-SEED GBT GBV GBT LOSS:TT-XENT-SEED
   GBON 0 ?do  GB-SEED i T-GET GB-INVR f*  GB-SEED i T-SET  loop
   ob GBT GBV GB-TGT GBT LOSS:TT-XENT  GB-INVR f* ;

\ ---- Adam update of one bound parameter from its gradient node -------------------------
: GB-LR ( -- r )  0.05 ;
: GB-UPD ( ptr a n ptr a ptr a n -- ) {: p:ptr slot:n mp:ptr vp:ptr n:n :}
   GB-LR GB-BET1 GB-BET2 GB-EPS GB-C1 GB-C2  p  slot GB-GRAD  mp vp n  OPTIM:TT-ADAM! ;

\ bind every host buffer to its input slot (params + integer ids + frozen mask + seed)
: GB-BIND ( -- )
   GB-WTE 0 MIR-SLOT-ID EX-BIND   GB-IDS 1 MIR-SLOT-ID EX-BIND   GB-WPE 2 MIR-SLOT-ID EX-BIND
   GB-WQ 3 MIR-SLOT-ID EX-BIND    GB-WK 4 MIR-SLOT-ID EX-BIND    GB-SC 5 MIR-SLOT-ID EX-BIND
   GB-WV 6 MIR-SLOT-ID EX-BIND    GB-WO 7 MIR-SLOT-ID EX-BIND
   GB-W1 8 MIR-SLOT-ID EX-BIND    GB-B1 9 MIR-SLOT-ID EX-BIND    GB-W2 10 MIR-SLOT-ID EX-BIND
   GB-B2 11 MIR-SLOT-ID EX-BIND   GB-WLM 12 MIR-SLOT-ID EX-BIND  GB-BLM 13 MIR-SLOT-ID EX-BIND
   GB-MASK 14 MIR-SLOT-ID EX-BIND
   GB-LG1 15 MIR-SLOT-ID EX-BIND  GB-LB1 16 MIR-SLOT-ID EX-BIND  GB-LG2 17 MIR-SLOT-ID EX-BIND
   GB-LB2 18 MIR-SLOT-ID EX-BIND  GB-GF 19 MIR-SLOT-ID EX-BIND   GB-BF 20 MIR-SLOT-ID EX-BIND
   GB-SEED BW-SEED-SLOT@ EX-BIND ;

\ build backward once, init params, bind every buffer
: GB-SETUP ( -- )  GB-INIT  GB-ARESET  BW-BUILD  EX-RESET  GB-BIND ;

\ forward slice -> loss + seed -> full IR -> Adam-update every trained param (skip ids,mask)
: GB-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   GB-LOSS-SEED {: loss:r :}
   EX-RUN
   GB-TICK
   GB-WTE 0 GB-WTEM GB-WTEV GBV GBC * GB-UPD    GB-WPE 2 GB-WPEM GB-WPEV GBT GBC * GB-UPD
   GB-WQ 3 GB-WQM GB-WQV GBC GBD * GB-UPD       GB-WK 4 GB-WKM GB-WKV GBC GBD * GB-UPD
   GB-SC 5 GB-SCM GB-SCV 1 GB-UPD             GB-WV 6 GB-WVM GB-WVV GBC GBD * GB-UPD
   GB-WO 7 GB-WOM GB-WOV GBD GBC * GB-UPD
   GB-W1 8 GB-W1M GB-W1V GBC GBH * GB-UPD       GB-B1 9 GB-B1M GB-B1V GBH GB-UPD
   GB-W2 10 GB-W2M GB-W2V GBH GBC * GB-UPD      GB-B2 11 GB-B2M GB-B2V GBC GB-UPD
   GB-WLM 12 GB-WLMM GB-WLMV GBC GBV * GB-UPD   GB-BLM 13 GB-BLMM GB-BLMV GBV GB-UPD
   GB-LG1 15 GB-LG1M GB-LG1V GBC GB-UPD        GB-LB1 16 GB-LB1M GB-LB1V GBC GB-UPD
   GB-LG2 17 GB-LG2M GB-LG2V GBC GB-UPD        GB-LB2 18 GB-LB2M GB-LB2V GBC GB-UPD
   GB-GF 19 GB-GFM GB-GFV GBC GB-UPD           GB-BF 20 GB-BFM GB-BFV GBC GB-UPD
   loss ;

: GB-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;
variable GB-IL   variable GB-FL
: GB-RUN ( n -- ) {: n:n :}
   n 0 ?do
      GB-STEP {: l:r :}
      i 0=     if l GB-IL ! then
      i n 1- = if l GB-FL ! then
   loop ;

\ ---- independent layer-by-layer forward reference (forward golden) --------------------
\ Every buffer below is the executor node's materialized output. The two attention
\ einsums are hand-mirrored to the equation engine's generated kernel: contraction loops
\ nest in DECLARED index order (A-SCORES Sdad,dac -> dad outer, dac inner; A-CTX Sdak,dac
\ -> dak outer, dac inner) and each term is f0*(f1*f2) (factor 0 * (factor 1 * factor 2)),
\ so the float summation order matches CELL FOR CELL.
create GBR-X0 GBT GBC * cells allot    create GBR-LN1 GBT GBC * cells allot   create GBR-Q GBT GBD * cells allot
create GBR-S GBT GBT * cells allot     create GBR-P GBT GBT * cells allot     create GBR-O GBT GBD * cells allot
create GBR-AT GBT GBC * cells allot    create GBR-X1 GBT GBC * cells allot    create GBR-LN2 GBT GBC * cells allot
create GBR-H GBT GBH * cells allot     create GBR-M GBT GBC * cells allot     create GBR-X2 GBT GBC * cells allot
create GBR-F GBT GBC * cells allot     create GBR-LOG GBT GBV * cells allot

: GBR-ROWBIAS ( ptr a n n ptr a -- ) {: yb:ptr rows:n cols:n bb:ptr :}
   rows 0 ?do cols 0 ?do  yb j cols * i + T-GET  bb i T-GET f+  yb j cols * i + T-SET  loop loop ;

\ A-SCORES element: S[q,k] = sum_{dad,dac} Q[q,dad]*(LN1[k,dac]*WK[dac,dad])
: GBR-SCORE-EL ( n n -- r ) {: q:n k:n :}
   0.0
   GBD 0 ?do                                   \ dad = j (outer)
      GBC 0 ?do                                \ dac = i (inner)
         GBR-Q q GBD * j + T-GET   GBR-LN1 k GBC * i + T-GET   GB-WK i GBD * j + T-GET   f* f*  f+
      loop
   loop ;
: GBR-SCORE ( -- )
   GBT 0 ?do  GBT 0 ?do  j i GBR-SCORE-EL   GBR-S j GBT * i + T-SET  loop  loop ;

\ A-CTX element: O[q,dad] = sum_{dak,dac} P[q,dak]*(LN1[dak,dac]*WV[dac,dad])
: GBR-CTX-EL ( n n -- r ) {: q:n dd:n :}
   0.0
   GBT 0 ?do                                   \ dak = j (outer)
      GBC 0 ?do                                \ dac = i (inner)
         GBR-P q GBT * j + T-GET   GBR-LN1 j GBC * i + T-GET   GB-WV i GBD * dd + T-GET   f* f*  f+
      loop
   loop ;
: GBR-CTX ( -- )
   GBT 0 ?do  GBD 0 ?do  j i GBR-CTX-EL   GBR-O j GBD * i + T-SET  loop  loop ;

\ scale (multiply by sc) then add the causal mask - one pass equals the two executor nodes
: GBR-SCALE-MASK ( -- )
   GB-SC 0 T-GET {: s:r :}
   GBT GBT * 0 ?do  GBR-S i T-GET s f*  GB-MASK i T-GET f+  GBR-S i T-SET  loop ;

: GBR-FWD ( -- )
   GB-WTE GB-IDS GB-WPE GBR-X0  GBT GBT GBC  TOKPOS-EMBED                    \ wte[ids] + wpe
   GBT 0 ?do  GBR-X0 i GBC * T-AT  GBR-LN1 i GBC * T-AT  GB-LG1 GB-LB1 GBC LN-AFFINE-FWD  loop  \ pre-attn LN
   GBR-LN1 GB-WQ GBR-Q  GBT GBC GBD  MATMUL                                    \ Q = LN1.WQ
   GBR-SCORE   GBR-SCALE-MASK   GBR-S GBR-P GBT ATTN-SOFTMAX-ROWS                \ S -> scale+mask -> softmax P
   GBR-CTX                                                               \ O = P.(LN1.WV)
   GBR-O GB-WO GBR-AT  GBT GBD GBC  MATMUL                                     \ attn = O.WO
   GBT GBC * 0 ?do  GBR-AT i T-GET  GBR-X0 i T-GET f+  GBR-X1 i T-SET  loop     \ residual + X0
   GBT 0 ?do  GBR-X1 i GBC * T-AT  GBR-LN2 i GBC * T-AT  GB-LG2 GB-LB2 GBC LN-AFFINE-FWD  loop  \ pre-MLP LN
   GBR-LN2 GB-W1 GBR-H  GBT GBC GBH  MATMUL   GBR-H GBT GBH GB-B1 GBR-ROWBIAS        \ LINEAR w1,b1
   GBT GBH * 0 ?do  GBR-H i T-GET GELU-F  GBR-H i T-SET  loop                 \ GELU
   GBR-H GB-W2 GBR-M  GBT GBH GBC  MATMUL   GBR-M GBT GBC GB-B2 GBR-ROWBIAS          \ LINEAR w2,b2
   GBT GBC * 0 ?do  GBR-M i T-GET  GBR-X1 i T-GET f+  GBR-X2 i T-SET  loop      \ residual + X1
   GBT 0 ?do  GBR-X2 i GBC * T-AT  GBR-F i GBC * T-AT  GB-GF GB-BF GBC LN-AFFINE-FWD  loop  \ final LN
   GBR-F GB-WLM GBR-LOG  GBT GBC GBV  MATMUL   GBR-LOG GBT GBV GB-BLM GBR-ROWBIAS ;  \ LM head -> logits

\ ---- causality probe: bind init params + real mask, forward, return the softmax P node --
: GB-FWD-P ( -- ptr a )
   GB-INIT  EX-RESET  GB-BIND
   BW-FWD-N@ EX-RUN-N
   7 MIR-NODE-ID EX-OUT@ ;                     \ node 7 = SOFTMAX-ROW (the attention weights P)
: GB-CAUSAL? ( -- bool )                       \ every future-key weight (key > query) ~ 0
   GB-FWD-P {: pb:ptr :}
   true
   GBT 0 ?do  GBT 0 ?do
      i j > if  pb j GBT * i + T-GET 0.000001 f<  and  then
   loop  loop ;
: GB-Q0-PRESENT? ( -- bool )                   \ query 0 attends only to key 0 (weight ~ 1)
   GB-FWD-P 0 T-GET 0.99 f> ;

T-RESET

\ ================= (A) the whole-model composition captures the GPT-2 block ============
MODEL: GBLK ( wte:6x6 ids:4x1 wpe:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wo:3x6 w1:6x8 b1:1x8 w2:8x6 b2:1x6 wlm:6x6 blm:1x6 mask:4x4 ln1g:1x6 ln1b:1x6 ln2g:1x6 ln2b:1x6 gfin:1x6 bfin:1x6 -- logits )
   GATHER  ADD  >V X0
   ln1g ln1b LAYERNORM  >V LN1
   MATMUL  LN1 A-SCORES  SCALE  mask ADD  SOFTMAX-ROW  LN1 A-CTX  MATMUL
   X0 RESIDUAL-ADD  >V X1
   ln2g ln2b LAYERNORM
   LINEAR  GELU  LINEAR
   X1 RESIDUAL-ADD
   gfin bfin LAYERNORM
   LINEAR ;
MODEL-K 18 T=                                                  \ 18 op nodes
0  MIR-NODE-ID MIR-OP@ OPR-NAME s" gather"       STR= TTRUE    \ wte[ids]
1  MIR-NODE-ID MIR-OP@ OPR-NAME s" add"          STR= TTRUE    \ + wpe
2  MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm"    STR= TTRUE    \ pre-attn LN
2  MIR-NODE-ID MIR-IN-COUNT@ 3 T=                              \ affine (x, gamma, beta)
3  MIR-NODE-ID MIR-OP@ OPR-NAME s" matmul"       STR= TTRUE    \ Q = LN1.WQ (roots from running LN1)
4  MIR-NODE-ID MIR-OP@ OPR-NAME s" equation"     STR= TTRUE    \ scores: K projection folded
4  MIR-NODE-ID MIR-IN-COUNT@ 3 T=                              \ three-factor (Q, LN1, WK)
5  MIR-NODE-ID MIR-OP@ OPR-NAME s" scale"        STR= TTRUE    \ 1/sqrt(d) temperature
6  MIR-NODE-ID MIR-OP@ OPR-NAME s" add"          STR= TTRUE    \ + causal mask
7  MIR-NODE-ID MIR-OP@ OPR-NAME s" softmax-row"  STR= TTRUE    \ attention weights P
8  MIR-NODE-ID MIR-OP@ OPR-NAME s" equation"     STR= TTRUE    \ context: V projection folded
8  MIR-NODE-ID MIR-IN-COUNT@ 3 T=                              \ three-factor (P, LN1, WV)
9  MIR-NODE-ID MIR-OP@ OPR-NAME s" matmul"       STR= TTRUE    \ output projection O.WO -> T x C
10 MIR-NODE-ID MIR-OP@ OPR-NAME s" residual-add" STR= TTRUE    \ + X0 skip
11 MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm"    STR= TTRUE    \ pre-MLP LN
11 MIR-NODE-ID MIR-IN-COUNT@ 3 T=
12 MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE    \ MLP w1,b1
13 MIR-NODE-ID MIR-OP@ OPR-NAME s" gelu"         STR= TTRUE
14 MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE    \ MLP w2,b2
15 MIR-NODE-ID MIR-OP@ OPR-NAME s" residual-add" STR= TTRUE    \ + X1 skip
16 MIR-NODE-ID MIR-OP@ OPR-NAME s" layernorm"    STR= TTRUE    \ final LN
16 MIR-NODE-ID MIR-IN-COUNT@ 3 T=
17 MIR-NODE-ID MIR-OP@ OPR-NAME s" linear"       STR= TTRUE    \ LM head -> logits
17 MIR-NODE-ID MIR-ROWS@ ROWS-RAW GBT T=  17 MIR-NODE-ID MIR-COLS@ COLS-RAW GBV T=   \ logits T x V

\ ================= (B) gradcheck: every trained input's analytic grad = FD =============
GC-RUN V-PASS T=
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE

\ ================= (B') per-slot gradient enumeration ==================================
MODEL: GBLK ( wte:6x6 ids:4x1 wpe:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wo:3x6 w1:6x8 b1:1x8 w2:8x6 b2:1x6 wlm:6x6 blm:1x6 mask:4x4 ln1g:1x6 ln1b:1x6 ln2g:1x6 ln2b:1x6 gfin:1x6 bfin:1x6 -- logits )
   GATHER  ADD  >V X0
   ln1g ln1b LAYERNORM  >V LN1
   MATMUL  LN1 A-SCORES  SCALE  mask ADD  SOFTMAX-ROW  LN1 A-CTX  MATMUL
   X0 RESIDUAL-ADD  >V X1
   ln2g ln2b LAYERNORM
   LINEAR  GELU  LINEAR
   X1 RESIDUAL-ADD
   gfin bfin LAYERNORM
   LINEAR ;
GB-SETUP
0  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wte  (token embedding, via gather scatter-add)
1  MIR-SLOT-ID BW-HAS-GRAD? TFALSE    \ ids  (integer index operand - never a gradient)
2  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wpe
3  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wq  (Q projection, standalone)
4  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wk  (K projection, folded into scores)
5  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ sc  (temperature)
6  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wv  (V projection, folded into context)
7  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wo  (output projection)
8  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ w1
9  MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ b1
10 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ w2
11 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ b2
12 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wlm
13 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ blm
14 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ mask (ADD copy adjoint gives it a grad; FROZEN in training)
15 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln1g
16 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln1b
17 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln2g
18 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ ln2b
19 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ gfin
20 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ bfin

\ ================= (C) forward golden: executor == layer-by-layer reference ============
BW-FWD-N@ EX-RUN-N
GBR-FWD
GB-OUT GBR-LOG GBON T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T=        \ cell-for-cell equal

\ ================= (D) training: mean CE more than halved, deterministic ===============
12 GB-RUN
GB-FL @ GB-IL @ f< TTRUE                        \ loss decreased
GB-FL @ GB-IL @ 0.5 f* f< TTRUE                 \ at least halved
GB-IL @ GB-MILLI 1741 T=                        \ committed initial mean CE (determinism lock)
GB-FL @ GB-MILLI 64 T=                           \ committed final mean CE (determinism lock)

\ run-twice bit-identical: reset params + moments (reuse the built graph), retrain, same milli
GB-INIT GB-ARESET  12 GB-RUN  GB-FL @ GB-MILLI 64 T=

\ ================= (E) causality in-composition ========================================
GB-CAUSAL? TTRUE                               \ every future-key softmax weight ~ 0
GB-Q0-PRESENT? TTRUE                           \ query 0 attends only to key 0 (weight ~ 1)

\ ================= (Nx) two stacked blocks are now DIFFERENTIABLE (MIR-CAP raise) ======
\ The MIR node-table cap raise (maki/model-ir.f: MIR-CAP 128 -> 1024; executor EX-NCAP
\ raised in lockstep) unblocks the differentiable 2-block stack that once died E-MIR-CAP at
\ node 128 during BW-BUILD. The node table holds forward + adjoint; measured accounting:
\   1 block : 74  = 18 fwd + 56  adjoint  (the single-block milestone above)
\   2 blocks: 133 = 32 fwd + 101 adjoint  (pinned below)
\ marginal 59 nodes/block -> 12 GPT-2-small blocks ~ 74 + 59*11 = 723 (1024 = 1.4x headroom).
\ Forward still composes (ref ring <= 2 outstanding of EQ-FCAP-1 = 7); the wall was the core
\ node-table capacity, now raised. gradcheck proves the backward is correct across BOTH
\ attention blocks; a 3-Adam-step run locks run-twice bit-identical training.

\ ---- generic per-slot 2-block train harness: loops over GBLK2's 34 model inputs, reusing
\ the single-block LCG + Adam machinery above (GB-RNG/GB-UNIT/GB-TICK/GB-C*/GB-OUT/GB-GRAD).
\ slot 1 = ids (integer copy task, no gradient); slot 23 = mask (causal, frozen). ----------
48 constant GB2MAX   34 constant GB2NIN   1 constant GB2IDS   23 constant GB2MASK
create GB2-W GB2NIN GB2MAX * cells allot   create GB2-M GB2NIN GB2MAX * cells allot   create GB2-V GB2NIN GB2MAX * cells allot
create GB2-SEED GBON cells allot   create GB2-TGT GBT cells allot
: GB2-WP ( n -- ptr a )  GB2MAX * cells GB2-W + ;
: GB2-MP ( n -- ptr a )  GB2MAX * cells GB2-M + ;
: GB2-VP ( n -- ptr a )  GB2MAX * cells GB2-V + ;
: GB2-SEL ( n -- n )  dup MIR-SLOT-ID MIR-SLOT-ROWS@ ROWS-RAW swap MIR-SLOT-ID MIR-SLOT-COLS@ COLS-RAW * ;
: GB2-PARAM? ( n -- bool )  dup GB2IDS <> swap GB2MASK <> and ;
: GB2-INIT ( -- )
   $C0FFEE GB-RNG !
   GB2NIN 0 ?do
      i GB2-PARAM? if  i GB2-SEL 0 ?do  GB-UNIT 0.1 f*  j GB2-WP i T-SET  loop  then
      0.0 i GB2-MP GB2MAX T-FILL  0.0 i GB2-VP GB2MAX T-FILL
   loop
   GBT 0 ?do  i GBV mod s>f  GB2IDS GB2-WP i T-SET  i GBV mod s>f  GB2-TGT i T-SET  loop
   GB2MASK GB2-WP A-MASK-FILL ;
: GB2-BIND ( -- )  EX-RESET  GB2NIN 0 ?do  i GB2-WP  i MIR-SLOT-ID EX-BIND  loop  GB2-SEED BW-SEED-SLOT@ EX-BIND ;
: GB2-LOSS-SEED ( -- r )
   GB-OUT {: ob:ptr :}
   ob GB2-TGT GB2-SEED GBT GBV GBT LOSS:TT-XENT-SEED
   GBON 0 ?do  GB2-SEED i T-GET GB-INVR f*  GB2-SEED i T-SET  loop
   ob GBT GBV GB2-TGT GBT LOSS:TT-XENT  GB-INVR f* ;
: GB2-ARESET ( -- )  0 GB-T !  1.0 GB-B1T !  1.0 GB-B2T ! ;
: GB2-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N  GB2-LOSS-SEED {: loss:r :}  EX-RUN  GB-TICK
   GB2NIN 0 ?do
      i GB2-PARAM? if
         GB-LR GB-BET1 GB-BET2 GB-EPS GB-C1 GB-C2
         i GB2-WP  i GB-GRAD  i GB2-MP i GB2-VP  i GB2-SEL  OPTIM:TT-ADAM!
      then
   loop  loss ;
variable GB2-IL  variable GB2-FL
: GB2-RUN ( n -- ) {: n:n :}  n 0 ?do  GB2-STEP {: l:r :}  i 0= if l GB2-IL ! then  i n 1- = if l GB2-FL ! then  loop ;
: GB2-SETUP ( -- )  BW-BUILD  GB2-INIT  GB2-ARESET  GB2-BIND ;

MODEL: GBLK2 ( wte:6x6 ids:4x1 wpe:4x6 wqa:6x3 wka:6x3 sca:1x1 wva:6x3 woa:3x6 w1a:6x8 b1a:1x8 w2a:8x6 b2a:1x6 wqb:6x3 wkb:6x3 scb:1x1 wvb:6x3 wob:3x6 w1b:6x8 b1b:1x8 w2b:8x6 b2b:1x6 wlm:6x6 blm:1x6 mask:4x4 ln1ga:1x6 ln1ba:1x6 ln2ga:1x6 ln2ba:1x6 ln1gb:1x6 ln1bb:1x6 ln2gb:1x6 ln2bb:1x6 gfin:1x6 bfin:1x6 -- logits )
   GATHER  ADD  >V X0A
   ln1ga ln1ba LAYERNORM  >V LN1A
   MATMUL  LN1A A-SCORES  SCALE  mask ADD  SOFTMAX-ROW  LN1A A-CTX  MATMUL
   X0A RESIDUAL-ADD  >V X1A
   ln2ga ln2ba LAYERNORM
   LINEAR  GELU  LINEAR
   X1A RESIDUAL-ADD  >V X0B
   ln1gb ln1bb LAYERNORM  >V LN1B
   MATMUL  LN1B A-SCORES  SCALE  mask ADD  SOFTMAX-ROW  LN1B A-CTX  MATMUL
   X0B RESIDUAL-ADD  >V X1B
   ln2gb ln2bb LAYERNORM
   LINEAR  GELU  LINEAR
   X1B RESIDUAL-ADD
   gfin bfin LAYERNORM
   LINEAR ;
MODEL-K 32 T=                                  \ forward composes: 32 nodes, ref ring satisfied

\ (Nx-A) gradcheck across BOTH attention blocks: every trained input's analytic grad = FD
GC-RUN V-PASS T=                               \ self-restores the forward IR (MIR-MARK/RELEASE)
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE

\ (Nx-B) the differentiable backward now BUILDS (was the E-MIR-CAP wall) - node accounting pinned
GB2-SETUP                                      \ BW-BUILD + init + bind
MIR-N@ 133 T=                                  \ fwd+bwd node count (future IR growth stays visible)
BW-FWD-N@ 32 T=                                \ forward slice
MIR-N@ BW-FWD-N@ - 101 T=                       \ adjoint node count

\ (Nx-C) deterministic training lock: 3 Adam steps, loss falls, run-twice bit-identical
3 GB2-RUN
GB2-IL @ GB-MILLI 1783 T=                       \ committed initial mean CE (determinism lock)
GB2-FL @ GB-MILLI 1661 T=                       \ committed final mean CE (determinism lock)
GB2-FL @ GB2-IL @ f< TTRUE                      \ 2-block stack trains: loss decreased
GB2-INIT GB2-ARESET  3 GB2-RUN  GB2-FL @ GB-MILLI 1661 T=   \ run-twice bit-identical

\ ================= (12x) derive-from-model at scale: single-input table-sizing focus ============
\ (dot habu-size-model-proportional 3iv/3v) This 260-op GELU chain drives the DERIVED table sizing
\ on ONE input (PLAN / DESCRIPTOR / SOURCE / node table), isolated from the input dimension: it
\ captures, builds, and gradchecks far past the OLD caps (PLAN-CAP 64, TV-CAP 256, MSRC 2048).
\ Every size derives from the model and grows-to-largest; the forward node table grows past
\ MIR-NODE-SEED (128) and the backward pass copy-on-grows it further. The input/name dimension -
\ once engine-bounded at ~3 blocks because the translator minted one typed LOCAL per input/name
\ against the frozen 64-local cap - is lifted by dot habu-ref-model-inputs (slot-indexed capture);
\ the (Nx-slot) section below captures the literal 4- and 12-block stacks the local cap blocked.
260 constant OPHN                                    \ > TV-CAP old 256, PLAN-CAP old 64, MSRC old 2048
create OPH-BUF 8192 allot   variable OPH-U
: OPH+ ( ptr u8 n -- ) {: a:ptr u:n :}  a OPH-BUF OPH-U @ + u BYTE-COPY  OPH-U @ u + OPH-U ! ;
: OPH-BUILD ( -- )  0 OPH-U !  s" MODEL: OPH ( x:4x8 -- y ) " OPH+  OPHN 0 ?do s" GELU " OPH+ loop  s" ; " OPH+ ;
OPH-BUILD  OPH-BUF OPH-U @ evaluate                  \ evaluate the generated MODEL: line at top level (like the literal ones above)
TENSOR:PLAN-N@  OPHN    T=                            \ 260 plan ops   (old PLAN-CAP 64 would have died -5045)
TENSOR:TV-COUNT OPHN 1+ T=                            \ 261 descriptors (old TV-CAP 256 would have died -5040)
MODEL-K        OPHN    T=                             \ 260 forward IR nodes (> MIR-NODE-SEED 128: grew)
MSRC-N @ 2048 > TTRUE                                 \ translated body past the old MSRC-CAP 2048 (-5024)
GC-RUN V-PASS T=                                     \ gradcheck the grown tables: correct at scale (self-restores IR)
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE
BW-BUILD                                             \ fresh backward over the 260-node forward IR
BW-FWD-N@ OPHN T=                                     \ forward slice exact
MIR-N@ OPHN 2 * T=                                   \ 520 = 260 fwd + 260 adjoint (backward copy-on-grow)

\ ================= (Nx-slot) any-size stacks: slot-indexed capture (dot habu-ref-model-inputs) ===
\ The MODEL: translator used to mint ONE typed local of the compiled capture word per signature
\ input + ">V" name; the frozen engine caps a definition at 64 locals, so a 4-block stack (60
\ inputs + 12 names = 72 locals) died at its 65th local and a 12-block (164 inputs + 36 names =
\ 200) was impossible - the old CAP-CAP=64 was THIS engine wall in a table costume. Now inputs and
\ named values are referenced by SLOT INDEX into a runtime slot table (MAKI:CAP-SLOT@), minting no
\ per-name local, so the capture word stays at zero input locals regardless of size. The captured
\ PLAN is identical to the old per-local form (behavior-neutral, proven by the GBLK/GBLK2 checks
\ above). Generator below emits the same block as GBLK/GBLK2 for any L (fixed tiny einsum shapes):
\ inputs = 8 + 13L, >V names = 3L, forward nodes = 4 + 14L.
create GBN-BUF 16384 allot   variable GBN-U
: GBN+ ( ptr u8 n -- ) {: a:ptr u:n :}  a GBN-BUF GBN-U @ + u BYTE-COPY  GBN-U @ u + GBN-U ! ;
: GBN# ( n -- )  SB-RESET FMT:SB-INT SB$ GBN+ ;
: GBN-PN ( ptr u8 n n -- ) {: a:ptr u:n i:n :}  a u GBN+  i GBN# ;   \ emit "<prefix><i>"
: GBN-SIG-BLK ( n -- ) {: i:n :}                    \ per-block cursor-drained params (consumption order)
   s" wq" i GBN-PN s" :6x3 " GBN+  s" wk" i GBN-PN s" :6x3 " GBN+  s" sc" i GBN-PN s" :1x1 " GBN+
   s" wv" i GBN-PN s" :6x3 " GBN+  s" wo" i GBN-PN s" :3x6 " GBN+
   s" w1" i GBN-PN s" :6x8 " GBN+  s" b1" i GBN-PN s" :1x8 " GBN+
   s" w2" i GBN-PN s" :8x6 " GBN+  s" b2" i GBN-PN s" :1x6 " GBN+ ;
: GBN-SIG-LN ( n -- ) {: i:n :}                     \ per-block named-ref affine-LN gamma/beta pairs
   s" ln1g" i GBN-PN s" :1x6 " GBN+  s" ln1b" i GBN-PN s" :1x6 " GBN+
   s" ln2g" i GBN-PN s" :1x6 " GBN+  s" ln2b" i GBN-PN s" :1x6 " GBN+ ;
: GBN-BLK ( n n -- ) {: i:n L:n :}                  \ one transformer block, block index i of L
   s" ln1g" i GBN-PN  s"  ln1b" i GBN-PN  s"  LAYERNORM  >V LN1" i GBN-PN
   s"  MATMUL LN1" i GBN-PN  s"  A-SCORES SCALE mask ADD SOFTMAX-ROW LN1" i GBN-PN  s"  A-CTX MATMUL X0" i GBN-PN
   s"  RESIDUAL-ADD  >V X1" i GBN-PN
   s"  ln2g" i GBN-PN  s"  ln2b" i GBN-PN  s"  LAYERNORM LINEAR GELU LINEAR X1" i GBN-PN  s"  RESIDUAL-ADD" GBN+
   i 1+ L < if  s"  >V X0" i 1+ GBN-PN  then  s"  " GBN+ ;
: GBN-BUILD ( n -- ) {: L:n :}
   0 GBN-U !
   s" MODEL: GBLKN ( wte:6x6 ids:4x1 wpe:4x6 " GBN+
   L 0 ?do  i GBN-SIG-BLK  loop
   s" wlm:6x6 blm:1x6 mask:4x4 " GBN+
   L 0 ?do  i GBN-SIG-LN  loop
   s" gfin:1x6 bfin:1x6 -- logits ) GATHER ADD  >V X00 " GBN+
   L 0 ?do  i L GBN-BLK  loop
   s" gfin bfin LAYERNORM LINEAR ; " GBN+ ;
: GBN$ ( -- ptr u8 n )  GBN-BUF GBN-U @ ;

\ (Nx-slot-A) the 4-block repro that DIES at the 65th local on base (72 locals) now LOADS and is
\ differentiable - proof the slot-indexed capture mints no per-input local.
4 GBN-BUILD  GBN$ evaluate
MODEL-K 60 T=                                        \ 4 + 14*4 forward nodes
SLOT-COUNT@ CAD-NUM:CAD-IC>N 60 T=                    \ 8 + 13*4 model inputs (past the old block-4 local wall)
GC-RUN V-PASS T=                                     \ every trained input gradchecks (self-restores IR)

\ (Nx-slot-B) THE 12-BLOCK GPT-2-SHAPED ACCEPTANCE (164 inputs): captures, gradchecks a sample of
\ every input slot, backward-builds. Old scheme = 164 inputs + 36 >V names = 200 locals, impossible
\ under the 64-local cap. Pinned MEASURED accounting = the sizing program's prediction, now realized.
12 GBN-BUILD  GBN$ evaluate
MODEL-K 172 T=                                       \ 4 + 14*12 forward nodes (measured; predicted ~172)
SLOT-COUNT@ CAD-NUM:CAD-IC>N 164 T=                   \ 8 + 13*12 model inputs (measured; predicted 164)
GC-RUN V-PASS T=                                     \ gradcheck-sample: each input slot's grad = central FD (self-restores IR)
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE
BW-BUILD                                             \ backward builds over the 172-node forward IR
BW-FWD-N@ 172 T=                                     \ forward slice exact
MIR-N@ 723 T=                                        \ 172 fwd + 551 adjoint (measured; predicted ~723 total)

T-REPORT

;package
