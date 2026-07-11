\ maki/adam-train-test.f - Adam training steps, attention E2E convergence, and
\ per-step gradient parity vs the element-level reference.
\
\ Proves, deterministically: the Adam bias-correction chain; the exact
\ attention IR that trains also gradchecks (GC-RUN V-PASS); per-step parity of
\ the BW-BUILD/executor gradients against an INDEPENDENT element-level reverse
\ pass hand-composed from the maki/autograd.f scalar rules (MUL-BWD for every
\ product, the ADD-BWD cotangent copy for every bias/accumulate) for 3 Adam
\ steps - dX, dW1, dB1, dW2, dB2 and the loss each within a tight relative-L2
\ tolerance; Adam-MLP and attention end-to-end training with committed
\ initial/final losses and loss strictly decreasing; attention determinism
\ from a fresh capture; and the fail-closed accessor guard (E-ADAM-RUN).
\
\ Reference scope: GELU has no maki/autograd.f element rule (its scalar rule
\ lives in maki/gelu.f), so the reference reuses GELU-F/GELU-BWD for that one
\ op. The gelu rule itself is independently verified against central finite
\ differences by the gradcheck this same IR passes (GC-RUN V-PASS); every
\ linear/bias/product step here is derived only from the autograd.f rules.
\
\ This file never calls SC-RUN, so from-scratch accessor-guard tests stay
\ valid regardless of manifest order; it is wired AFTER maki/from-scratch-test.f
\ in maki/test.f.

require lib/test.f
require lib/float.f
require maki/adam-train.f
require maki/gradcheck.f

package MAKI

\ ---- committed run lengths ---------------------------------------------------
60 constant RGT-MLP-N        \ Adam MLP training steps
40 constant RGT-ATTN-N       \ attention training steps

\ ---- throw-path wrappers (accessors used before any training run) ------------
: RGT-AMT-THROW ( -- )  AMT-INITIAL@ drop ;
: RGT-ATN-THROW ( -- )  ATN-FINAL@ drop ;

\ ---- element-level reference: buffers ----------------------------------------
create RG-H1   SC-BATCH SC-HID * cells allot   \ pre-activation x@w1+b1 (8x16)
create RG-A1   SC-BATCH SC-HID * cells allot   \ gelu(h1)
create RG-OUT  SC-BATCH SC-OUT * cells allot   \ a1@w2+b2 (8x2)
create RG-SEED SC-BATCH SC-OUT * cells allot   \ dL/dout (mean-scaled)
create RG-DA1  SC-BATCH SC-HID * cells allot   \ dL/da1
create RG-DH1  SC-BATCH SC-HID * cells allot   \ dL/dh1
create RG-DX   SC-XN  cells allot
create RG-DW1  SC-W1N cells allot
create RG-DB1  SC-B1N cells allot
create RG-DW2  SC-W2N cells allot
create RG-DB2  SC-B2N cells allot

: RG-ACC ( r ptr a n -- ) {: d:r p:ptr k:n :}
   p k T-GET d f+  p k T-SET ;

\ ---- reference forward (MUL-F/ADD-F chains + the gelu scalar rule) -----------
: RG-DOT1 ( n n -- r ) {: r:n j:n :}      \ sum_k x[r,k]*w1[k,j]
   0.0
   SC-FEAT 0 ?do
      SC-X r SC-FEAT * i + T-GET  SC-W1 i SC-HID * j + T-GET  MUL-F  ADD-F
   loop ;

: RG-H1-CELL ( n n -- ) {: r:n j:n :}
   r j RG-DOT1  SC-B1 j T-GET  ADD-F
   RG-H1 r SC-HID * j + T-SET ;

: RG-H1! ( -- )
   SC-BATCH 0 ?do  SC-HID 0 ?do  j i RG-H1-CELL  loop loop ;

: RG-A1! ( -- )
   SC-BATCH SC-HID * 0 ?do  RG-H1 i T-GET GELU-F  RG-A1 i T-SET  loop ;

: RG-DOT2 ( n n -- r ) {: r:n o:n :}      \ sum_j a1[r,j]*w2[j,o]
   0.0
   SC-HID 0 ?do
      RG-A1 r SC-HID * i + T-GET  SC-W2 i SC-OUT * o + T-GET  MUL-F  ADD-F
   loop ;

: RG-OUT-CELL ( n n -- ) {: r:n o:n :}
   r o RG-DOT2  SC-B2 o T-GET  ADD-F
   RG-OUT r SC-OUT * o + T-SET ;

: RG-OUT! ( -- )
   SC-BATCH 0 ?do  SC-OUT 0 ?do  j i RG-OUT-CELL  loop loop ;

\ ---- reference loss + seed (same mean-NLL layout as the trainer) -------------
: RG-ROW-SEED ( n -- r ) {: r:n :}
   RG-OUT r SC-OUT * T-GET {: mu:r :}
   RG-OUT r SC-OUT * 1+ T-GET {: lv:r :}
   SC-Y r T-GET {: y:r :}
   y mu lv LOSS:NLL-MU-GRAD     SC-INV-BATCH f*  RG-SEED r SC-OUT *    T-SET
   y mu lv LOSS:NLL-LOGVAR-GRAD SC-INV-BATCH f*  RG-SEED r SC-OUT * 1+ T-SET
   y mu lv LOSS:NLL ;

: RG-SEED! ( -- r )
   0.0  SC-BATCH 0 ?do  i RG-ROW-SEED f+  loop  SC-INV-BATCH f* ;

\ ---- reference backward (reverse pass over the element rules) ----------------
: RG-ZERO-GRADS ( -- )
   0.0 RG-DX  SC-XN  T-FILL
   0.0 RG-DW1 SC-W1N T-FILL
   0.0 RG-DB1 SC-B1N T-FILL
   0.0 RG-DW2 SC-W2N T-FILL
   0.0 RG-DB2 SC-B2N T-FILL
   0.0 RG-DA1 SC-BATCH SC-HID * T-FILL ;

\ product rule at (r,o,j): out[r,o] += a1[r,j]*w2[j,o]
: RG-L2-CELL ( n n n -- ) {: r:n o:n j:n :}
   RG-SEED r SC-OUT * o + T-GET
   RG-A1   r SC-HID * j + T-GET
   SC-W2   j SC-OUT * o + T-GET
   MUL-BWD {: da:r dw:r :}
   da RG-DA1 r SC-HID * j + RG-ACC
   dw RG-DW2 j SC-OUT * o + RG-ACC ;

\ bias add backward is the ADD-BWD cotangent copy, accumulated over the batch
: RG-DB2-CELL ( n n -- ) {: r:n o:n :}
   RG-SEED r SC-OUT * o + T-GET  RG-DB2 o RG-ACC ;

: RG-L2-ROW ( n n -- ) {: r:n o:n :}
   r o RG-DB2-CELL
   SC-HID 0 ?do  r o i RG-L2-CELL  loop ;

: RG-L2! ( -- )
   SC-BATCH 0 ?do  SC-OUT 0 ?do  j i RG-L2-ROW  loop loop ;

: RG-DH1! ( -- )
   SC-BATCH SC-HID * 0 ?do
      RG-DA1 i T-GET  RG-H1 i T-GET  GELU-BWD  RG-DH1 i T-SET
   loop ;

\ product rule at (r,j,k): h1[r,j] += x[r,k]*w1[k,j]
: RG-L1-CELL ( n n n -- ) {: r:n j:n k:n :}
   RG-DH1 r SC-HID * j + T-GET
   SC-X   r SC-FEAT * k + T-GET
   SC-W1  k SC-HID * j + T-GET
   MUL-BWD {: dx:r dw:r :}
   dx RG-DX  r SC-FEAT * k + RG-ACC
   dw RG-DW1 k SC-HID * j + RG-ACC ;

: RG-DB1-CELL ( n n -- ) {: r:n j:n :}
   RG-DH1 r SC-HID * j + T-GET  RG-DB1 j RG-ACC ;

: RG-L1-ROW ( n n -- ) {: r:n j:n :}
   r j RG-DB1-CELL
   SC-FEAT 0 ?do  r j i RG-L1-CELL  loop ;

: RG-L1! ( -- )
   SC-BATCH 0 ?do  SC-HID 0 ?do  j i RG-L1-ROW  loop loop ;

\ full element-level forward + backward over the LIVE model/parameter buffers;
\ returns the batch mean NLL at the current parameters
: RG-GRADS ( -- r )
   RG-H1!  RG-A1!  RG-OUT!
   RG-SEED! {: loss:r :}
   RG-ZERO-GRADS
   RG-L2!  RG-DH1!  RG-L1!
   loss ;

\ ---- relative-L2 comparison --------------------------------------------------
: RG-DIFF2 ( ptr a ptr a n -- r ) {: ap:ptr bp:ptr len:n :}
   0.0  len 0 ?do  ap i T-GET  bp i T-GET  LOSS:RES2  f+  loop ;

: RG-NORM2 ( ptr a n -- r ) {: bp:ptr len:n :}
   0.0  len 0 ?do  bp i T-GET  0.0  LOSS:RES2  f+  loop ;

\ ||a-b|| / ||b|| : relative L2 distance to the reference b
: RG-REL ( ptr a ptr a n -- r ) {: ap:ptr bp:ptr len:n :}
   ap bp len RG-DIFF2 fsqrt
   bp len RG-NORM2 fsqrt
   f/ ;

: RG-REL1 ( r r -- r ) {: a:r b:r :}      \ |a-b|/|b| scalar relative error
   a b f- fabs  b fabs  f/ ;

\ analytic-vs-analytic in f64: only summation-order noise is tolerated
: RG-TOL ( -- r )  0.00000001 ;

\ one parity step: executor/BW-BUILD gradients vs the element-level reference,
\ then apply Adam so the NEXT step checks freshly-updated parameters
: RGT-STEP ( -- )
   AMT-GRADS {: l:r :}
   RG-GRADS  {: rl:r :}
   l rl RG-REL1  RG-TOL f<  TTRUE
   SC-X-SLOT  SC-GRAD-AT  RG-DX   SC-XN  RG-REL  RG-TOL f<  TTRUE
   SC-W1-SLOT SC-GRAD-AT  RG-DW1  SC-W1N RG-REL  RG-TOL f<  TTRUE
   SC-B1-SLOT SC-GRAD-AT  RG-DB1  SC-B1N RG-REL  RG-TOL f<  TTRUE
   SC-W2-SLOT SC-GRAD-AT  RG-DW2  SC-W2N RG-REL  RG-TOL f<  TTRUE
   SC-B2-SLOT SC-GRAD-AT  RG-DB2  SC-B2N RG-REL  RG-TOL f<  TTRUE
   AMT-APPLY ;

variable RGT-L1               \ stashed final loss (determinism check)

T-RESET

\ ---- fail closed: accessors before any run throw E-ADAM-RUN ------------------
' RGT-AMT-THROW E-ADAM-RUN TTHROWS
' RGT-ATN-THROW E-ADAM-RUN TTHROWS

\ ---- Adam bias-correction chain (1-b^t after one and two ticks) --------------
ADAM-RESET ADAM-TICK
ADAM-C1 1000.0    f* 0.5 f+ f>s   100 T=      \ 1 - 0.9
ADAM-C2 1000000.0 f* 0.5 f+ f>s  1000 T=      \ 1 - 0.999
ADAM-TICK
ADAM-C1 1000.0    f* 0.5 f+ f>s   190 T=      \ 1 - 0.81
ADAM-C2 1000000.0 f* 0.5 f+ f>s  1999 T=      \ 1 - 0.998001

\ ---- per-step gradient parity over 3 Adam steps ------------------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP
RGT-STEP
RGT-STEP
RGT-STEP

\ ---- Adam MLP end-to-end: loss decreases + converges --------------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGT-MLP-N AMT-RUN
AMT-STEPS@ RGT-MLP-N T=
AMT-INITIAL@ SC-MILLI 130 T=            \ same committed init as the SGD trainer
AMT-FINAL@ AMT-INITIAL@ f< TTRUE
AMT-CONVERGED? TTRUE
AMT-FINAL@ SC-MILLI -2749 T=            \ committed final NLL (regression lock)

\ ---- the exact attention IR that trains also gradchecks ----------------------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
GC-RUN V-PASS T=

\ ---- attention end-to-end: loss decreases + converges -------------------------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
RGT-ATTN-N ATN-RUN
ATN-STEPS@ RGT-ATTN-N T=
ATN-INITIAL@ SC-MILLI 155 T=            \ committed initial mean MSE
ATN-FINAL@ ATN-INITIAL@ f< TTRUE
ATN-FINAL@  ATN-INITIAL@ ATN-CONV-RATIO f*  f< TTRUE
ATN-FINAL@ ATN-CONV-THRESH f< TTRUE
ATN-CONVERGED? TTRUE
ATN-FINAL@ SC-MILLI 9 T=                \ committed final mean MSE (regression lock)

\ ---- determinism: same committed seed -> identical final loss (exact) --------
ATN-FINAL@ RGT-L1 !
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
RGT-ATTN-N ATN-RUN
ATN-FINAL@ RGT-L1 @ f= TTRUE

T-REPORT

end-package
