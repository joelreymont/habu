\ maki/pos-embed-test.f - the token+pos MODEL: composition: gradcheck + Adam training
\ (dot habu-learned-positional-embedding).
\
\ GPT-2's block input is wte[idx] + wpe[pos]. The single-running-value MODEL: DSL
\ cannot root two independent lookups (a GATHER of wte AND a SLICE/GATHER of wpe) in
\ one body - each rooted op needs its root as the running value, and there is only one
\ (proven: "GATHER wpe SLICE:0..T ADD" and "SLICE >V P GATHER P ADD" both reject,
\ E-CAD-PARAM-SHAPE / E-CAD-REF). The composable prototype is therefore GATHER ADD with
\ wpe pre-shaped to the gathered rows (positions 0..B*T-1 = the whole table, B=1): the
\ wte gather plus the positional table added elementwise. The wpe SLICE of a larger
\ table + its pad-scatter adjoint is the buffer golden (maki/embedding.f WPE-SLICE);
\ here the ADD's copy adjoint routes dY to wpe while the gather's scatter-add routes it
\ to wte, so BOTH tables receive gradients (the load-bearing acceptance). Then an Adam
\ trainer drives the mean-MSE loss down, updating wte + wpe on fixed integer token ids
\ (the index operand is never a trained parameter). maki -> habu only.

require lib/test.f
require lib/float.f
require maki/cad.f
require maki/gradcheck.f
require maki/from-scratch-train.f       \ SC-SLOT / SC-GRAD-AT
require maki/optim-tensor.f             \ OPTIM:TT-ADAM!
require maki/loss-tensor.f              \ LOSS:TT-MSE / LOSS:TT-MSE-DY

package MAKI

T-RESET

\ ================= (1) the composition gradchecks - wte AND wpe ================
MODEL: TOK-POS ( wte:4x3 ids:6x1 wpe:6x3 -- y ) GATHER ADD ;
GC-RUN V-PASS T=                        \ every trained input's analytic grad = finite diff
BW-BUILD
0 MIR-SLOT-ID BW-HAS-GRAD? TTRUE        \ wte (gather table) receives a gradient
2 MIR-SLOT-ID BW-HAS-GRAD? TTRUE        \ wpe (added positional table) receives a gradient
1 MIR-SLOT-ID BW-HAS-GRAD? TFALSE       \ ids (integer index operand) never does

\ ================= (2) Adam training: mean-MSE loss falls =====================
4  constant TP-V     3 constant TP-C     6 constant TP-T
12 constant TP-WN    18 constant TP-PN                 \ wte elems ; wpe/out/target elems
0 constant TP-WTE-SLOT   1 constant TP-IDS-SLOT   2 constant TP-WPE-SLOT

create TP-WTE  TP-WN cells allot   create TP-IDS  TP-T cells allot
create TP-WPE  TP-PN cells allot   create TP-TGT  TP-PN cells allot   create TP-SEED TP-PN cells allot
create TP-WTEM TP-WN cells allot   create TP-WTEV TP-WN cells allot
create TP-WPEM TP-PN cells allot   create TP-WPEV TP-PN cells allot

\ ---- Adam bias-correction bookkeeping (b1=0.9, b2=0.999, eps=1e-8) -----------
variable TP-T@   variable TP-B1T   variable TP-B2T
: TP-ARESET ( -- )  0 TP-T@ !  1.0 TP-B1T !  1.0 TP-B2T ! ;
: TP-TICK   ( -- )  TP-T@ @ 1+ TP-T@ !  TP-B1T @ 0.9 f* TP-B1T !  TP-B2T @ 0.999 f* TP-B2T ! ;
: TP-C1 ( -- r )  1.0 TP-B1T @ f- ;
: TP-C2 ( -- r )  1.0 TP-B2T @ f- ;
: TP-LR ( -- r )  0.05 ;

\ Adam-update one parameter buffer in place from its gradient buffer
: TP-ADAM ( r ptr a ptr a ptr a ptr a n -- ) {: lr:r wp:ptr gp:ptr mp:ptr vp:ptr len:n :}
   lr 0.9 0.999 0.00000001 TP-C1 TP-C2  wp gp mp vp len  OPTIM:TT-ADAM! ;

\ deterministic init: small wte/wpe, fixed integer token ids, a committed target
: TP-INIT ( -- )
   TP-WN 0 ?do  i 5 mod s>f 0.1 f*  TP-WTE i T-SET  loop
   TP-PN 0 ?do  i 3 mod s>f 0.1 f* 0.1 f-  TP-WPE i T-SET  loop
   1.0 TP-IDS 0 T-SET  3.0 TP-IDS 1 T-SET  0.0 TP-IDS 2 T-SET
   2.0 TP-IDS 3 T-SET  3.0 TP-IDS 4 T-SET  1.0 TP-IDS 5 T-SET
   TP-PN 0 ?do  i 7 mod s>f 0.2 f* 0.5 f-  TP-TGT i T-SET  loop
   0.0 TP-WTEM TP-WN T-FILL  0.0 TP-WTEV TP-WN T-FILL
   0.0 TP-WPEM TP-PN T-FILL  0.0 TP-WPEV TP-PN T-FILL
   TP-ARESET ;

\ build backward once, bind every host buffer (parameters + index + seed cotangent)
: TP-SETUP ( -- )
   TP-INIT  BW-BUILD  EX-RESET
   TP-WTE  TP-WTE-SLOT SC-SLOT EX-BIND
   TP-IDS  TP-IDS-SLOT SC-SLOT EX-BIND
   TP-WPE  TP-WPE-SLOT SC-SLOT EX-BIND
   TP-SEED BW-SEED-SLOT@ EX-BIND ;

: TP-OUT  ( -- ptr a )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;   \ last forward node = the 6x3 output
: TP-INVN ( -- r )  1.0 TP-PN s>f f/ ;

\ write the mean-scaled seed cotangent (2*(o-t)/N) from the current output; return mean MSE
: TP-LOSS-SEED ( -- r )
   TP-OUT {: ob:ptr :}
   ob TP-TGT TP-SEED TP-PN LOSS:TT-MSE-DY
   TP-PN 0 ?do  TP-SEED i T-GET TP-INVN f*  TP-SEED i T-SET  loop
   ob TP-TGT TP-PN LOSS:TT-MSE  TP-INVN f* ;

\ one Adam step: forward -> loss + seed -> full IR -> update wte (scatter-add grad node)
\ and wpe (its grad is the ADD-copied seed cotangent, the bound TP-SEED buffer)
: TP-STEP ( -- r )
   BW-FWD-N@ EX-RUN-N
   TP-LOSS-SEED {: loss:r :}
   EX-RUN
   TP-TICK
   TP-LR TP-WTE  TP-WTE-SLOT SC-GRAD-AT  TP-WTEM TP-WTEV TP-WN  TP-ADAM
   TP-LR TP-WPE  TP-SEED                 TP-WPEM TP-WPEV TP-PN  TP-ADAM
   loss ;

variable TP-INIT-L   variable TP-FINAL-L
: TP-RUN ( n -- ) {: n:n :}
   TP-SETUP
   n 0 ?do
      TP-STEP {: l:r :}
      i 0=     if l TP-INIT-L  ! then
      i n 1- = if l TP-FINAL-L ! then
   loop ;

MODEL: TOK-POS ( wte:4x3 ids:6x1 wpe:6x3 -- y ) GATHER ADD ;
30 TP-RUN
TP-FINAL-L @ TP-INIT-L @ f< TTRUE                 \ loss strictly decreased
TP-FINAL-L @ TP-INIT-L @ 0.5 f* f< TTRUE          \ at least halved
TP-FINAL-L @ 0.05 f< TTRUE                         \ converged below a committed floor

T-REPORT

;package
