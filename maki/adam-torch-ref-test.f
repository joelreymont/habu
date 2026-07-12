\ maki/adam-torch-ref-test.f - PyTorch grad + Adam parity for the Adam-MLP
\ trainer (dot habu-autograd-end-to residue: external-reference parity).
\
\ Replays the exact committed AMT trainer (maki/adam-train.f over
\ maki/from-scratch-model.f: same LCG-seeded data + parameters, same
\ LINEAR GELU LINEAR forward, same mean Gaussian NLL, same Adam
\ hyperparameters) for 3 Adam steps and asserts, per step, the pre-update
\ loss and the BW-BUILD/executor gradients (dX, dW1, dB1, dW2, dB2) against
\ torch.autograd's numbers computed on the identical seeded floats
\ (maki/adam-torch-ref-data.f), then the Adam-updated parameters after the
\ third step - grad parity proves the backward chain against an external
\ autograd, parameter parity proves the optimizer chain on top of it.
\
\ Tolerance: both sides run f64, so the only expected gap is Habu's committed
\ FEXP polynomial core (lib/fmath.f, rel err ~1e-7 per exp) inside GELU's
\ tanh and the NLL's exp(-logvar), against torch's true libm transcendentals.
\ Measured worst relative-L2 over all steps/tensors is 1.4e-8 (loss rel err
\ 7e-10); TTR-TOL = 2e-6 keeps two orders of margin above that floor while
\ staying orders below any real defect (a wrong rule or constant shows >=1e-3).
\
\ Like maki/adam-train-test.f this file never calls AMT-RUN/SC-RUN, so the
\ accessor-guard tests of sibling suites stay valid regardless of manifest
\ order. Load via maki/test.f.

require lib/test.f
require lib/float.f
require maki/adam-train.f
require maki/adam-torch-ref-data.f

package MAKI

: TTR-TOL ( -- r )  0.000002 ;   \ see header: ~140x the measured FEXP-gap floor

\ tensor closeness against a committed torch reference buffer
: TTR-NEAR ( ptr a ptr a n -- )  T-REL-L2 TTR-TOL f<  TTRUE ;

\ one parity step: loss + all five gradients vs torch, then apply Adam so the
\ NEXT step checks freshly-updated parameters (the optimizer chain compounds)
: TTR-STEP ( n -- ) {: s:n :}
   AMT-GRADS {: l:r :}
   l  s TRD-LOSS@  T-REL1  TTR-TOL f<  TTRUE
   SC-X-SLOT  SC-GRAD-AT  s TRD-GX@   SC-XN  TTR-NEAR
   SC-W1-SLOT SC-GRAD-AT  s TRD-GW1@  SC-W1N TTR-NEAR
   SC-B1-SLOT SC-GRAD-AT  s TRD-GB1@  SC-B1N TTR-NEAR
   SC-W2-SLOT SC-GRAD-AT  s TRD-GW2@  SC-W2N TTR-NEAR
   SC-B2-SLOT SC-GRAD-AT  s TRD-GB2@  SC-B2N TTR-NEAR
   AMT-APPLY ;

\ fail-closed data accessors: only the 3 committed steps exist
: TTR-BAD-STEP ( -- )  TRD-STEPS TRD-GX@ drop ;
: TTR-BAD-LOSS ( -- )  -1 TRD-LOSS@ drop ;

T-RESET

' TTR-BAD-STEP E-TREF-STEP TTHROWS
' TTR-BAD-LOSS E-TREF-STEP TTHROWS

\ ---- per-step grad + loss parity vs torch over 3 Adam steps ------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP
0 TTR-STEP
1 TTR-STEP
2 TTR-STEP

\ ---- Adam side: parameters after the third step match torch.optim.Adam -------
SC-W1 TRD-FW1 SC-W1N TTR-NEAR
SC-B1 TRD-FB1 SC-B1N TTR-NEAR
SC-W2 TRD-FW2 SC-W2N TTR-NEAR
SC-B2 TRD-FB2 SC-B2N TTR-NEAR

T-REPORT

;package
