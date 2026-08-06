\ maki/examples/nanogpt/xent-train-test.f - the cross-entropy classifier trainer
\ end-to-end (maki/examples/nanogpt/xent-train.f).
\
\ This suite lives in its own test-owned package (XENT-TRAIN-TEST) and imports the
\ MAKI and LOSS public surfaces with `using`, so no test helper leaks into a shared
\ namespace. Each scenario brackets the trainer's BW-BUILD with MIR-MARK/MIR-RELEASE
\ (the gradcheck discipline) instead of re-capturing the model, so the shared forward
\ IR is restored between scenarios.
\
\ What each part proves:
\  1. GC-RUN V-PASS checks the executor's LINEAR adjoint by finite-differencing the
\     GENERIC objective L = sum_k seed_k * output_k with an arbitrary cotangent seed.
\     It never calls TT-XENT/TT-XENT-SEED, so it proves the op's backward - NOT the
\     composed cross-entropy gradient that training actually uses.
\  2. The COMPOSED parameter-gradient check (the property training depends on): for
\     EVERY weight and bias element, the analytic gradient the training loop reads -
\     seeded by TT-XENT-SEED (the mean-scaled y-t cotangent) and propagated by the
\     backward IR - is compared against a central finite difference of the MEAN
\     cross-entropy TT-XENT recomputed through the forward slice. This ties the loss
\     (TT-XENT), its seed (TT-XENT-SEED), and the executor's LINEAR backward together.
\     A DETECTION fixture corrupts one analytic element and shows the check has teeth.
\  3. Seeded end-to-end convergence, the committed initial/final mean CE, and exact
\     determinism from a restored IR.
\
\ Tolerances: the composed check uses h=1e-3 central differences (the gradcheck GC-H).
\ Measured worst analytic-vs-FD residual over all 30 parameters is 6.1e-8 absolute /
\ 4e-6 relative - the O(h^2) central-difference floor of this smooth softmax-CE-linear
\ composition. GTOL = 1e-6 absolute + 1e-4 relative keeps >16x margin while a wrong
\ adjoint is off by O(1); the DETECTION fixture (+0.5 on one analytic element) measures
\ ~0.5 and is caught. Data are the committed one-hot classes (class c = r mod V) with
\ integer targets read via f>s.

require lib/test.f
require maki/gradcheck.f
require maki/examples/nanogpt/xent-train.f

package XENT-TRAIN-TEST
using MAKI
using LOSS

60 constant TRAIN-STEPS                         \ committed training steps
1 LAYOUT-BUFFER MK-BUF MIR:mark                 \ stash the forward-IR restore mark
variable LAST-LOSS                              \ stashed final loss (determinism check)

\ ---- read the analytic gradient node for a model input slot -----------------
: AN-GRAD ( n -- ptr r )  MIR-SLOT-ID BW-SLOT-GRAD@ MIR-REF-NODE EX-OUT@ ;
\ ---- forward logits node + a forward-slice-only run -------------------------
: LOGITS  ( -- ptr r )  BW-FWD-N@ 1- MIR-NODE-ID EX-OUT@ ;
: RUN-FWD ( -- )  BW-FWD-N@ EX-RUN-N ;
\ ---- the batch MEAN cross-entropy at the current logits (what training reports)
: MEAN-CE ( -- r )  LOGITS CET-R CET-V CET-T CET-TN TT-XENT  CET-INV-R f* ;
\ ---- write the mean-scaled y-t cotangent seed (exactly as the trainer does) --
: WRITE-SEED ( -- )
   LOGITS CET-T CET-SEED CET-R CET-V CET-TN TT-XENT-SEED
   CET-ON 0 ?do  CET-SEED i T-GET CET-INV-R f*  CET-SEED i T-SET  loop ;

\ ---- central finite difference of MEAN-CE over one parameter element ---------
: FD-AT ( ptr r n -- r ) {: pb:ptr e:n :}
   pb e T-GET {: base:r :}
   base 0.001 f+ pb e T-SET  RUN-FWD MEAN-CE {: yp:r :}
   base 0.001 f- pb e T-SET  RUN-FWD MEAN-CE {: ym:r :}
   base pb e T-SET
   yp ym f-  0.002 f/ ;

\ ---- analytic-vs-FD agreement: |a-fd| < abs + rel*|a| (justified in the header)
: GTOL-ABS ( -- r )  0.000001 ;
: GTOL-REL ( -- r )  0.0001 ;
: GCLOSE? ( r r -- bool ) {: a:r fd:r :}
   a fd f- fabs   GTOL-ABS  GTOL-REL a fabs f* f+   f< ;

\ ---- every element of one parameter tensor: analytic node vs central FD -------
: CHECK-PARAM ( n ptr r n -- ) {: slot:n pb:ptr len:n :}
   len 0 ?do  slot AN-GRAD i T-GET  pb i FD-AT  GCLOSE? TTRUE  loop ;

\ ---- fresh forward + mean-scaled seed + full backward (grads at current params)
: SEED-BACKWARD ( -- )  CET-SETUP RUN-FWD WRITE-SEED EX-RUN ;

\ ---- the composed check: analytic seeded by TT-XENT-SEED vs central FD of TT-XENT
: COMPOSED ( -- )
   MIR-MARK 0 MK-BUF !
   SEED-BACKWARD
   CET-W-SLOT CET-W CET-WN CHECK-PARAM
   CET-B-SLOT CET-B CET-BN CHECK-PARAM
   0 MK-BUF @ MIR-RELEASE ;

\ ---- detection: a corrupted analytic gradient must exceed the tolerance --------
: DETECT ( -- bool )
   MIR-MARK 0 MK-BUF !
   SEED-BACKWARD
   CET-W-SLOT AN-GRAD 0 T-GET 0.5 f+  {: bad:r :}   \ corrupt analytic W[0] by +0.5
   CET-W 0 FD-AT {: fd:r :}
   0 MK-BUF @ MIR-RELEASE
   bad fd GCLOSE? ;

\ ---- one committed convergence run, IR restored afterward --------------------
: CONV ( -- )  MIR-MARK 0 MK-BUF !  TRAIN-STEPS CET-RUN  0 MK-BUF @ MIR-RELEASE ;

T-RESET

\ ---- 1. LINEAR adjoint via the generic sum(seed*output) objective (NOT the CE grad)
GC-RUN V-PASS T=

\ ---- 2. the composed cross-entropy parameter gradient training actually uses --
COMPOSED                                        \ every W and b element: analytic vs FD of TT-XENT
DETECT TFALSE                                   \ a corrupted analytic element is caught

\ ---- 3. seeded end-to-end convergence ---------------------------------------
CONV
CET-STEPS@ TRAIN-STEPS T=
CET-INITIAL@ 1000.0 f* 0.5 f+ f>s 1606 T=       \ committed initial batch mean CE
CET-FINAL@   1000.0 f* 0.5 f+ f>s  188 T=       \ committed final mean CE (regression lock)
CET-FINAL@ CET-INITIAL@ f< TTRUE                \ loss strictly decreased
CET-FINAL@ CET-INITIAL@ 0.5 f* f< TTRUE         \ below initial * 0.5
CET-FINAL@ 0.5 f< TTRUE                         \ below the committed CE floor

\ ---- determinism: same seed -> identical final loss (exact) -----------------
CET-FINAL@ LAST-LOSS !
CONV
CET-FINAL@ LAST-LOSS @ f= TTRUE

T-REPORT

;using
;using
;package
