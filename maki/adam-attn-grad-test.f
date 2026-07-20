\ maki/adam-attn-grad-test.f - per-step gradient parity for the 1-layer attention
\ block against an INDEPENDENT central-finite-difference reference (dot
\ habu-autograd-end-to residue: the attention half of per-step grad parity).
\
\ The Adam-MLP already has per-step gradient parity against two independent
\ references - the element-level reverse pass (maki/adam-train-test.f RGT-STEP)
\ and committed PyTorch (maki/adam-torch-ref-test.f). The attention block had only
\ a ONE-TIME host gradcheck at synthetic inputs (maki/adam-train-test.f GC-RUN
\ V-PASS) plus end-to-end convergence; it had no per-step gradient compare on the
\ actual Adam training trajectory. This file closes that: for the committed
\ ADAM-ATTN trainer (maki/adam-train.f: O = softmax(Q@Kt * s) @ V under mean-MSE,
\ all four operands Q/Kt/s/V trained by Adam) it checks, per step, the emitted
\ BW-BUILD/executor VJP (dQ, dKt, ds, dV via SC-GRAD-AT) against a central finite
\ difference of a hand-composed attention forward.
\
\ Independent reference: AG-FWD recomposes the forward in the test (Q@Kt via
\ maki/matmul.f, in-place scale, per-row maki/softmax.f, attn@V, mean MSE) and its
\ loss is central-differenced (h=1e-3, the gradcheck GC-H) w.r.t. every parameter
\ element. The finite difference of the forward is the standard independent oracle
\ for the emitted analytic backward - the thing under test - so the FD-vs-SC-GRAD-AT
\ comparison is independent of the reverse pass exactly as a gradcheck is, but pinned
\ to the LIVE trained parameters at each Adam step instead of one synthetic point.
\
\ Parts A/B pin the matmul op-kind fixture; part C re-runs the SAME checks after swapping the
\ first matmul for a SPEC: equation GEMM (docs/model-unified.md stage 2), proving the derived
\ einsum adjoints finite-difference-check AND train bit-identically to the matmul op-kind.
\
\  A. 3 Adam steps with full FD gradient parity per parameter (dQ/dKt/ds/dV) and
\     per-step forward-loss parity (independent AG-LOSS vs the executor's ATN-GRADS
\     loss), then ATN-APPLY so the next step checks freshly-updated parameters.
\  B. the full committed 40-step run driven with per-step forward-loss parity; its
\     independent-forward initial/final mean-MSE match the committed 155/9 milli
\     locks of maki/adam-train-test.f and converge (final < ratio*initial) - the
\     independent-reference final-loss check the dot asks for.
\ Plus a DETECTION fixture: a deliberately corrupted analytic gradient must exceed
\ the tolerance, proving the parity has teeth (never a vacuous pass).
\
\ Tolerances (both sides f64): AG-GRAD-TOL = 1e-4 relative-L2. Measured worst over
\ all steps/parameters is 1.04e-6 - the O(h^2) central-difference truncation floor
\ of this smooth block - so 1e-4 keeps ~97x margin while a wrong adjoint shows >=1e-2
\ (the detection fixture measures ~1e1). AG-LOSS-TOL = 1e-9: the independent forward
\ reuses the same reference host kernels the ADAM-ATTN ops lower to, so the loss is
\ bit-identical (measured rel err 0 across all 43 steps); 1e-9 only guards summation
\ order across hosts. CPU-side proof: the small-model device region classes are
\ forward-only, so a device-grad leg is out of scope here. The attention PyTorch
\ reference run stays USER-GATED (external tool; Habu-Only bars a local script), so
\ the committed independent reference is central finite differences - the dot's
\ stated minimum. Load via maki/test.f.

require lib/test.f
require lib/float.f
require maki/array.f
require maki/matmul.f
require maki/softmax.f
require maki/loss-tensor.f
require maki/adam-train.f

package MAKI

\ shapes pinned to the committed ADAM-ATTN signature (q:4x3 kt:3x4 s:1x1 v:4x3)
4  constant AG-L               \ sequence length L
16 constant AG-SN              \ score / attention elements (L*L = 4x4)

create AG-SCORES AG-SN cells allot    \ Q@Kt (4x4), scaled in place
create AG-ATTN   AG-SN cells allot     \ softmax rows (4x4)
create AG-OUT    ATN-EN cells allot    \ attn@V (4x3)

create AG-QFD ATN-EN cells allot       \ per-parameter finite-difference gradients
create AG-KFD ATN-EN cells allot
create AG-SFD ATN-SN cells allot
create AG-VFD ATN-EN cells allot
create AG-BAD ATN-EN cells allot        \ corrupted-analytic scratch (detection)

\ ---- independent forward: scores = Q@Kt ; *s ; softmax rows ; out = attn@V ----
: AG-SCORES! ( -- )  ATN-Q ATN-KT AG-SCORES  AG-L ATN-D AG-L  MATMUL ;

: AG-SCALE! ( -- ) ATN-S 0 T-GET {: sv:r :}
   AG-SN 0 ?do  AG-SCORES i T-GET sv f*  AG-SCORES i T-SET  loop ;

: AG-SOFTMAX! ( -- )
   AG-L 0 ?do  AG-SCORES i AG-L * cells +  AG-ATTN i AG-L * cells +  AG-L  SM-FWD  loop ;

: AG-MATV! ( -- )  AG-ATTN ATN-V AG-OUT  AG-L AG-L ATN-D  MATMUL ;

: AG-FWD ( -- )  AG-SCORES!  AG-SCALE!  AG-SOFTMAX!  AG-MATV! ;

\ ---- independent mean-MSE loss at the current live parameters -----------------
: AG-INV-N ( -- r )  1.0 ATN-EN s>f f/ ;
: AG-LOSS  ( -- r )  AG-FWD  AG-OUT ATN-TGT ATN-EN LOSS:TT-MSE  AG-INV-N f* ;

\ ---- central finite difference of AG-LOSS over every element of a buffer ------
: AG-FD-STEP ( -- r )  0.001 ;   \ matches gradcheck GC-H

: AG-FD! ( ptr a n ptr a -- ) {: pb:ptr len:n fdb:ptr :}
   len 0 ?do
      pb i T-GET {: base:r :}
      base AG-FD-STEP f+  pb i T-SET  AG-LOSS {: yp:r :}
      base AG-FD-STEP f-  pb i T-SET  AG-LOSS {: ym:r :}
      base pb i T-SET
      yp ym f-  AG-FD-STEP 2.0 f* f/  fdb i T-SET
   loop ;

\ ---- tolerances (justified in the header) ------------------------------------
: AG-GRAD-TOL ( -- r )  0.0001 ;         \ 1e-4 FD-vs-analytic rel-L2 (~97x the FD floor)
: AG-LOSS-TOL ( -- r )  0.000000001 ;    \ 1e-9 forward-loss parity (summation noise)

\ one parameter: full central-FD gradient vs the executor VJP node, rel-L2 < tol
: AG-CHECK ( n ptr a n ptr a -- ) {: slot:n pb:ptr len:n fdb:ptr :}
   pb len fdb AG-FD!
   fdb  slot SC-GRAD-AT  len  T-REL-L2  AG-GRAD-TOL f<  TTRUE ;

\ one FD-parity step: executor forward+backward, forward-loss parity, then FD every
\ parameter's gradient, then apply Adam so the next step sees updated parameters
: AG-STEP-FD ( -- )
   ATN-GRADS {: el:r :}
   AG-LOSS   {: il:r :}
   el il T-REL1  AG-LOSS-TOL f<  TTRUE
   ATN-Q-SLOT  ATN-Q  ATN-EN AG-QFD  AG-CHECK
   ATN-KT-SLOT ATN-KT ATN-EN AG-KFD  AG-CHECK
   ATN-S-SLOT  ATN-S  ATN-SN AG-SFD  AG-CHECK
   ATN-V-SLOT  ATN-V  ATN-EN AG-VFD  AG-CHECK
   ATN-APPLY ;

\ ---- part B: independent-forward loss parity across the committed trajectory ---
40 constant AG-CONV-N          \ committed attention run length (maki/adam-train-test.f)
variable AG-CONV-INIT          \ independent initial mean MSE
variable AG-CONV-FINAL         \ independent final mean MSE (pre-update, last step)

: AG-STEP-LOSS ( n -- ) {: k:n :}
   ATN-GRADS {: el:r :}
   AG-LOSS   {: il:r :}
   el il T-REL1  AG-LOSS-TOL f<  TTRUE
   k 0=             if il AG-CONV-INIT  ! then
   k AG-CONV-N 1- = if il AG-CONV-FINAL ! then
   ATN-APPLY ;

: AG-CONV-RUN ( -- )  AG-CONV-N 0 ?do  i AG-STEP-LOSS  loop ;

\ ---- detection: a corrupted analytic gradient must exceed the tolerance --------
: AG-DETECT ( -- r )   \ rel-L2 of FD vs the analytic dQ with element 0 corrupted
   ATN-Q ATN-EN AG-QFD AG-FD!
   ATN-Q-SLOT SC-GRAD-AT {: an:ptr :}
   ATN-EN 0 ?do  an i T-GET  AG-BAD i T-SET  loop
   AG-BAD 0 T-GET 0.5 f+  AG-BAD 0 T-SET
   AG-QFD AG-BAD ATN-EN T-REL-L2 ;

T-RESET

\ ---- A: 3 Adam steps, full FD gradient parity per parameter -------------------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
ATN-SETUP
AG-STEP-FD
AG-STEP-FD
AG-STEP-FD

\ ---- B: full committed run, independent-forward loss parity + convergence ------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
ATN-SETUP
AG-CONV-RUN
AG-CONV-INIT  @ SC-MILLI 155 T=                 \ independent forward reproduces the
AG-CONV-FINAL @ SC-MILLI 9   T=                 \ committed executor milli-MSE locks
AG-CONV-FINAL @ AG-CONV-INIT @ f< TTRUE
AG-CONV-FINAL @  AG-CONV-INIT @ ATN-CONV-RATIO f*  f< TTRUE

\ ---- detection: corrupted analytic gradient is caught (parity has teeth) --------
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
ATN-SETUP
ATN-GRADS drop
AG-DETECT AG-GRAD-TOL f< TFALSE

\ ---- C: derived equation-op adjoints (docs/model-unified.md stage 2) -----------------
\ Replace the fixture's first MATMUL (S = Q@Kt) with a SPEC: equation GEMM. Its adjoints
\ dEAQ/dEAKT are DERIVED at declaration time (maki/spec.f EQ-ADJ-DERIVE) and run by the ONE
\ equation arm of the reverse transform (maki/backward.f BW-STEP-EQUATION). The extents and
\ signature match the committed ADAM-ATTN fixture, so the AG central-FD oracle above (which
\ reads ATN-Q/ATN-KT, model-agnostic) validates the derived adjoints unchanged.
4 EXTENT: #EAM   3 EXTENT: #EAK   4 EXTENT: #EAN
TENSOR: EAQ ( #EAM #EAK )   TENSOR: EAKT ( #EAK #EAN )   TENSOR: EAS ( #EAM #EAN )
SPEC: EAGEMM  EAS[eam ean] = Σeak EAQ[eam eak] · EAKT[eak ean] ;   \ S = Q@Kt

\ C1: the derived adjoints match central finite differences (3 Adam steps, live params) -
MODEL: EQ-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) EAGEMM SCALE SOFTMAX-ROW MATMUL ;
ATN-SETUP
AG-STEP-FD
AG-STEP-FD
AG-STEP-FD

\ C2: acceptance - the equation-op GEMM trains IDENTICALLY to the matmul op-kind on the
\ maki/adam-train.f:224 fixture: the forward score and both derived adjoints are bit-identical
\ to matmul's (IEEE product commutes exactly; same contraction order), so the per-step loss
\ trajectory is EQUAL, not merely close. Asserted element-wise over the committed 40 steps.
40 constant EQX-N
create EQX-EQ EQX-N cells allot   create EQX-MM EQX-N cells allot
variable EQX-BAD
: EQX-RUN ( ptr a -- ) {: dst:ptr :}  ATN-SETUP  EQX-N 0 ?do  ATN-STEP  dst i T-SET  loop ;
: EQX-CMP ( -- n )
   0 EQX-BAD !
   EQX-N 0 ?do  EQX-EQ i T-GET  EQX-MM i T-GET  f= 0= if EQX-BAD @ 1+ EQX-BAD ! then  loop
   EQX-BAD @ ;
MODEL: EQ-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) EAGEMM SCALE SOFTMAX-ROW MATMUL ;
EQX-EQ EQX-RUN
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
EQX-MM EQX-RUN
EQX-CMP 0 T=                                     \ every step's loss bit-identical to matmul
EQX-EQ 0      T-GET SC-MILLI 155 T=              \ same committed initial mean-MSE
EQX-EQ EQX-N 1- T-GET SC-MILLI 9 T=              \ same committed final mean-MSE

T-REPORT

;package
