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

\ ================= (Nx) two stacked blocks: forward composes, backward hits the IR cap ==
\ The ref ring HOSTS two blocks forward - the capture is a clean 32-node graph and no op
\ ever has more than 2 outstanding named refs (the affine LN gamma/beta pairs), well under
\ the EQ-FCAP-1 = 7 ring. But the DIFFERENTIABLE 2-block graph cannot be built: one block's
\ full forward+backward IR is 74 nodes (18 forward + 56 adjoint), so a second block's
\ adjoints push past MIR-CAP = 128 (maki/model-ir.f) and BW-BUILD throws E-MIR-CAP at node
\ 128. The wall is the IR node-table capacity (core), NOT the named-ref ring - recorded
\ mechanically, not faked. Raising MIR-CAP is a separate core-capacity change out of this
\ composition lane's scope. THE FULL DIFFERENTIABLE MILESTONE IS THE SINGLE BLOCK ABOVE.
: NX-BUILD ( -- )  BW-BUILD ;
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
' NX-BUILD E-MIR-CAP TTHROWS                    \ backward exceeds the 128-node IR cap (E-MIR-CAP)

T-REPORT

;package
