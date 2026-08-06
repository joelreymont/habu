\ maki/attn-eq-test.f - differentiable self-attention over the running value, built
\ from EQUATION ops (dot habu-differentiable-attention-via-a23b42d4). Proves the wall
\ maki/gptblock-test.f documented (wall #1: the single-running-value DSL cannot root
\ Q@Kᵀ over the running representation) is CLEARED: attention now trains end to end with
\ gradients flowing to the Q/K/V projection weights and to X itself.
\
\ The sublayer (maki/attn-eq.f) is Q = X·WQ (a MATMUL rooting from the running X) then
\ two folded contractions - A-SCORES (K projection folded: S = Q·(X·WK)ᵀ) and A-CTX
\ (V projection folded: O = P·(X·WV)) - around a scaled row softmax. Q is standalone
\ because only the first op can be factor 0 of the single running value; the K and V
\ projections fold into the einsums and reach their weights through the derived adjoints
\ (this is why the equation engine is load-bearing: einsum adjoints derive at declaration
\ time, maki/spec.f). The three-factor equations compose through the PLAN-EQUATION3
\ capture arm this lane added (maki/plan-ops.f / plan-vocab.f / cad.f); the executor and
\ backward already ran K factors generically.
\
\ Proofs: (A) the two einsums register + are differentiable; (B) the sublayer captures as
\ MATMUL,equation,SCALE,SOFTMAX-ROW,equation and gradchecks vs central FD, every trained
\ slot (x, wq, wk, sc, wv) carrying a gradient; (C) the +LM-head model gradchecks; (D) it
\ trains under Adam on integer targets with the mean cross-entropy MORE THAN HALVED and
\ deterministic (committed milli locks); (E) causal masking via an ADD of a mask constant
\ + the existing SOFTMAX-ROW (no new op-kind) gradchecks AND drives future-key attention
\ to ~0; (F) a three-factor equation operand with the wrong contraction extent is the
\ named E-TV-SHAPE reject (the new operand check has teeth). maki -> habu only.

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

package MAKI

\ ---- shapes: T positions, C channels, d head dim, V vocab -------------------------
5 constant MVOC     4 constant MSEQ   3 constant MD   6 constant MC
20 constant MON   \ logit elements = MSEQ*MVOC

\ ---- model buffers (signature/capture order) + moment buffers for Adam --------------
create M-X  MSEQ MC * cells allot   create M-WQ MC MD * cells allot   create M-WK MC MD * cells allot
create M-SC 1 cells allot         create M-WV MC MD * cells allot   create M-WH MD MVOC * cells allot
create M-MASK MSEQ MSEQ * cells allot create M-TGT MSEQ cells allot        create M-SEED MON cells allot

create M-XM MSEQ MC * cells allot   create M-XV MSEQ MC * cells allot
create M-QM MC MD * cells allot   create M-QV MC MD * cells allot
create M-KM MC MD * cells allot   create M-KV MC MD * cells allot
create M-SM 1 cells allot         create M-SV 1 cells allot
create M-VM MC MD * cells allot   create M-VV MC MD * cells allot
create M-HM MD MVOC * cells allot   create M-HV MD MVOC * cells allot

\ ---- deterministic 32-bit LCG (Numerical Recipes constants) -> small param init -----
variable M-RNG
: M-NEXT ( -- r )  M-RNG @ 1664525 * 1013904223 + $FFFFFFFF and dup M-RNG !  s>f 4294967296.0 f/ ;
: M-UNIT ( -- r )  M-NEXT 2.0 f* 1.0 f- ;
: M-SMALL ( ptr r n -- ) {: p:ptr n:n :}  n 0 ?do  M-UNIT 0.2 f*  p i T-SET  loop ;

\ ---- Adam state (step count + running decay powers) ---------------------------------
: M-B1 ( -- r ) 0.9 ;  : M-B2 ( -- r ) 0.999 ;  : M-EPS ( -- r ) 0.00000001 ;
variable M-T  variable M-B1T  variable M-B2T
: M-ARESET ( -- )  0 M-T !  1.0 M-B1T !  1.0 M-B2T !
   0.0 M-XM MSEQ MC * T-FILL 0.0 M-XV MSEQ MC * T-FILL 0.0 M-QM MC MD * T-FILL 0.0 M-QV MC MD * T-FILL
   0.0 M-KM MC MD * T-FILL 0.0 M-KV MC MD * T-FILL 0.0 M-SM 1 T-FILL 0.0 M-SV 1 T-FILL
   0.0 M-VM MC MD * T-FILL 0.0 M-VV MC MD * T-FILL 0.0 M-HM MD MVOC * T-FILL 0.0 M-HV MD MVOC * T-FILL ;
: M-TICK ( -- )  M-T @ 1+ M-T !  M-B1T @ M-B1 f* M-B1T !  M-B2T @ M-B2 f* M-B2T ! ;
: M-C1 ( -- r ) 1.0 M-B1T @ f- ;  : M-C2 ( -- r ) 1.0 M-B2T @ f- ;

\ committed init: small weights, sc = 1/sqrt(d), a copy task (target t = t mod V) the
\ block learns by attending over its own running value.
: M-INIT ( -- )
   $C0FFEE M-RNG !
   M-X MSEQ MC * M-SMALL   M-WQ MC MD * M-SMALL   M-WK MC MD * M-SMALL
   M-WV MC MD * M-SMALL  M-WH MD MVOC * M-SMALL
   1.0 MD s>f fsqrt f/  M-SC 0 T-SET
   MSEQ 0 ?do  i MVOC mod s>f  M-TGT i T-SET  loop
   M-MASK A-MASK-FILL ;

\ ---- executor output (last forward node = the logits) + per-slot gradient node -------
: M-OUT ( -- ptr r )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: M-GRAD ( n -- ptr r )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;
: M-INVR ( -- r )  1.0 MSEQ s>f f/ ;

\ write the mean-scaled y-onehot(target) seed cotangent from the logits; return mean CE
: M-LOSS-SEED ( -- r )
   M-OUT {: ob:ptr :}
   ob M-TGT M-SEED MSEQ MVOC MSEQ LOSS:TT-XENT-SEED
   MON 0 ?do  M-SEED i T-GET M-INVR f*  M-SEED i T-SET  loop
   ob MSEQ MVOC M-TGT MSEQ LOSS:TT-XENT  M-INVR f* ;

\ ---- Adam update of one bound parameter from its gradient node ----------------------
: M-LR ( -- r )  0.05 ;
: M-UPD ( ptr r n ptr r ptr r n -- ) {: p:ptr slot:n mp:ptr vp:ptr n:n :}
   M-LR M-B1 M-B2 M-EPS M-C1 M-C2  p  slot M-GRAD  mp vp n  OPTIM:TT-ADAM! ;

\ build backward once, bind every host buffer (params + seed cotangent)
: M-SETUP ( -- )
   M-INIT  M-ARESET  BW-BUILD  EX-RESET
   M-X 0 MIR-SLOT-ID EX-BIND   M-WQ 1 MIR-SLOT-ID EX-BIND   M-WK 2 MIR-SLOT-ID EX-BIND
   M-SC 3 MIR-SLOT-ID EX-BIND  M-WV 4 MIR-SLOT-ID EX-BIND   M-WH 5 MIR-SLOT-ID EX-BIND
   M-SEED BW-SEED-SLOT@ EX-BIND ;

\ forward slice -> loss + seed -> full IR -> Adam-update every parameter
: M-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   M-LOSS-SEED {: loss:r :}
   EX-RUN
   M-TICK
   M-X 0 M-XM M-XV MSEQ MC * M-UPD   M-WQ 1 M-QM M-QV MC MD * M-UPD   M-WK 2 M-KM M-KV MC MD * M-UPD
   M-SC 3 M-SM M-SV 1 M-UPD        M-WV 4 M-VM M-VV MC MD * M-UPD   M-WH 5 M-HM M-HV MD MVOC * M-UPD
   loss ;

: M-MILLI ( r -- n )  1000.0 f* 0.5 f+ f>s ;
variable M-IL   variable M-FL
: M-RUN ( n -- ) {: n:n :}
   n 0 ?do
      M-STEP {: l:r :}
      i 0=     if l M-IL ! then
      i n 1- = if l M-FL ! then
   loop ;

T-RESET

\ ================= (A) the einsums registered + are differentiable =================
: A-SCORES-DIFF? ( -- bool )  s" A-SCORES" EQ-FIND MATCH option none OF false ENDOF some OF EQ-DIFF? ENDOF ;MATCH ;
: A-CTX-DIFF?    ( -- bool )  s" A-CTX"    EQ-FIND MATCH option none OF false ENDOF some OF EQ-DIFF? ENDOF ;MATCH ;
A-SCORES-DIFF? TTRUE
A-CTX-DIFF? TTRUE

\ ================= (B) the sublayer captures + gradchecks ==========================
MODEL: A-SUB ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 -- ctx )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX ;
MODEL-K 5 T=                                                    \ 5 op nodes
0 MIR-NODE-ID MIR-OP@ OPR-NAME s" matmul"      STR= TTRUE       \ Q = X . WQ (roots from running X)
1 MIR-NODE-ID MIR-OP@ OPR-NAME s" equation"    STR= TTRUE       \ scores: K projection folded
1 MIR-NODE-ID MIR-IN-COUNT@ 3 T=                                \ three-factor equation (Q, X, WK)
2 MIR-NODE-ID MIR-OP@ OPR-NAME s" scale"       STR= TTRUE       \ 1/sqrt(d) temperature
3 MIR-NODE-ID MIR-OP@ OPR-NAME s" softmax-row" STR= TTRUE
4 MIR-NODE-ID MIR-OP@ OPR-NAME s" equation"    STR= TTRUE       \ context: V projection folded
4 MIR-NODE-ID MIR-IN-COUNT@ 3 T=                                \ three-factor equation (P, X, WV)
4 MIR-NODE-ID MIR-ROWS@ ROWS-RAW 4 T=  4 MIR-NODE-ID MIR-COLS@ COLS-RAW 3 T=   \ O = 4 x d

\ per-slot gradient enumeration: every trained projection input carries a gradient
MODEL: A-SUB ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 -- ctx )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX ;
BW-BUILD
BW-BWD-COUNT 0 > TTRUE
0 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ x   (the running representation)
1 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wq  (Q projection)
2 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wk  (K projection, folded into scores)
3 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ sc  (temperature)
4 MIR-SLOT-ID BW-HAS-GRAD? TTRUE     \ wv  (V projection, folded into context)

\ gradcheck the assembled sublayer vs central finite differences
MODEL: A-SUB ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 -- ctx )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX ;
GC-RUN V-PASS T=
GC-RE$ s" input(s) gradchecked" CONTAINS? TTRUE

\ ================= (C) the +LM-head model gradchecks ==============================
MODEL: A-MODEL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 -- logits )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX MATMUL ;
MODEL-K 6 T=
5 MIR-NODE-ID MIR-OP@ OPR-NAME s" matmul" STR= TTRUE            \ LM head -> logits
5 MIR-NODE-ID MIR-ROWS@ ROWS-RAW 4 T=  5 MIR-NODE-ID MIR-COLS@ COLS-RAW 5 T=   \ logits 4 x V
MODEL: A-MODEL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 -- logits )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX MATMUL ;
GC-RUN V-PASS T=

\ ================= (D) training: mean CE more than halved, deterministic ===========
MODEL: A-MODEL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 -- logits )
   MATMUL x A-SCORES SCALE SOFTMAX-ROW x A-CTX MATMUL ;
M-SETUP
80 M-RUN
M-FL @ M-IL @ f< TTRUE                          \ loss decreased
M-FL @ M-IL @ 0.5 f* f< TTRUE                   \ at least halved
M-IL @ M-MILLI 1610 T=                          \ committed initial mean CE (determinism lock)
M-FL @ M-MILLI 700 T=                           \ committed final mean CE (determinism lock)

\ ================= (E) causal masking via ADD mask-constant + SOFTMAX-ROW ==========
\ Uses EXISTING ops - no new op-kind: a causal mask constant added before the softmax.
MODEL: A-CAUSAL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 mask:4x4 -- logits )
   MATMUL x A-SCORES SCALE mask ADD SOFTMAX-ROW x A-CTX MATMUL ;
MODEL-K 7 T=                                                    \ +1 node: the mask ADD
3 MIR-NODE-ID MIR-OP@ OPR-NAME s" add"         STR= TTRUE       \ scaled scores + mask
4 MIR-NODE-ID MIR-OP@ OPR-NAME s" softmax-row" STR= TTRUE
\ gradchecks like the unmasked model (ADD's copy adjoint carries the mask through)
MODEL: A-CAUSAL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 mask:4x4 -- logits )
   MATMUL x A-SCORES SCALE mask ADD SOFTMAX-ROW x A-CTX MATMUL ;
GC-RUN V-PASS T=

\ forward semantic: with the real causal mask bound, query 0 attends ONLY to key 0 -
\ future-key weights decay to ~0 through the stable softmax (node 4 = the softmax P).
: A-CAUSAL-FWD-P ( -- ptr r )   \ bind params + real mask, run forward, return softmax P
   M-INIT  EX-RESET
   M-X 0 MIR-SLOT-ID EX-BIND   M-WQ 1 MIR-SLOT-ID EX-BIND   M-WK 2 MIR-SLOT-ID EX-BIND
   M-SC 3 MIR-SLOT-ID EX-BIND  M-WV 4 MIR-SLOT-ID EX-BIND   M-WH 5 MIR-SLOT-ID EX-BIND
   M-MASK 6 MIR-SLOT-ID EX-BIND
   NODE-COUNT@ CAD-NUM:CAD-IC>N EX-RUN-N
   4 MIR-NODE-ID EX-OUT@ ;
: A-CAUSAL-Q0? ( -- bool )   \ query 0's attention is causal: k1,k3 ~0 and k0 ~1
   A-CAUSAL-FWD-P {: pb:ptr :}
   pb 1 T-GET 0.000001 f<
   pb 3 T-GET 0.000001 f< and
   pb 0 T-GET 0.99 f>     and ;
MODEL: A-CAUSAL ( x:4x6 wq:6x3 wk:6x3 sc:1x1 wv:6x3 wh:3x5 mask:4x4 -- logits )
   MATMUL x A-SCORES SCALE mask ADD SOFTMAX-ROW x A-CTX MATMUL ;
A-CAUSAL-Q0? TTRUE

\ ================= (F) three-factor operand extent check has teeth ================
\ A folded-projection operand whose contraction extent (cols 5) differs from the
\ equation's #DAD = 3 is the named E-TV-SHAPE reject, like the matmul inner-dim check.
: A-BADOP3 ( -- )
   TENSOR:TV-RESET
   #DAQ #DAD SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC   \ Q : 4 x 3 (ok)
   #DAK #DAC SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC   \ X : 4 x 6 (ok)
   #DAC 5   SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW SPACE-HOST TENSOR:TV-DESC   \ WK: 6 x 5 (bad: want 6x3)
   s" A-SCORES" EQ-FIND MATCH option
      none OF E-SPEC-SYNTAX throw ENDOF
      some OF PLAN-EQUATION3 ENDOF
   ;MATCH drop ;
' A-BADOP3  E-TV-SHAPE  TTHROWS

;package

T-REPORT
