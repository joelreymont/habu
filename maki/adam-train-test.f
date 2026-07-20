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
\ ---- relative-L2 comparison: maki/array.f T-REL-L2 / T-REL1 -------------------
\ analytic-vs-analytic in f64: only summation-order noise is tolerated
: RG-TOL ( -- r )  0.00000001 ;

\ one parity step: executor/BW-BUILD gradients vs the element-level reference,
\ then apply Adam so the NEXT step checks freshly-updated parameters
: RGT-STEP ( -- )
   AMT-GRADS {: l:r :}
   RG-GRADS  {: rl:r :}
   l rl T-REL1  RG-TOL f<  TTRUE
   SC-X-SLOT  SC-GRAD-AT  RG-DX   SC-XN  T-REL-L2  RG-TOL f<  TTRUE
   SC-W1-SLOT SC-GRAD-AT  RG-DW1  SC-W1N T-REL-L2  RG-TOL f<  TTRUE
   SC-B1-SLOT SC-GRAD-AT  RG-DB1  SC-B1N T-REL-L2  RG-TOL f<  TTRUE
   SC-W2-SLOT SC-GRAD-AT  RG-DW2  SC-W2N T-REL-L2  RG-TOL f<  TTRUE
   SC-B2-SLOT SC-GRAD-AT  RG-DB2  SC-B2N T-REL-L2  RG-TOL f<  TTRUE
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

\ ---- scheduled Adam MLP: opt-in LR schedule (linear warmup + cosine decay) ------
\ Same SCRATCH-MLP fixture and RGT-MLP-N steps as the fixed-lr run above, but the
\ step size follows LR-SCHED(warmup=10, decay=50, min=0.002, max=0.02): linear
\ warmup to 0.02 over steps 0..9, cosine decay to 0.002 over 10..50, then flat
\ 0.002. It has its own deterministic final-loss lock; the fixed-lr -2749 lock
\ above is untouched (AMT-RUN-SCHED disarms the schedule on exit).
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
10 50 0.002 0.02 RGT-MLP-N AMT-RUN-SCHED
AMT-STEPS@ RGT-MLP-N T=
AMT-INITIAL@ SC-MILLI 130 T=            \ identical init (params equal at step 0)
AMT-FINAL@ AMT-INITIAL@ f< TTRUE        \ loss decreased
AMT-CONVERGED? TTRUE                    \ halved and below the NLL floor
AMT-FINAL@ SC-MILLI -2599 T=           \ scheduled final NLL (new regression lock)

\ determinism: same schedule params + seed -> identical final loss (exact)
AMT-FINAL@ RGT-L1 !
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
10 50 0.002 0.02 RGT-MLP-N AMT-RUN-SCHED
AMT-FINAL@ RGT-L1 @ f= TTRUE

\ zero regression: after the scheduled run disarms, the fixed-lr path locks -2749
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGT-MLP-N AMT-RUN
AMT-FINAL@ SC-MILLI -2749 T=

\ ---- global-norm gradient clipping: armed run with a small clip (real clipping) ----
\ Same SCRATCH-MLP fixture and RGT-MLP-N steps, but each step clips the global grad
\ norm to 0.1 (well below the step grad norms) before Adam. Deterministic final-loss
\ lock, distinct from the unclipped -2749, so clipping demonstrably changed the
\ trajectory; the fixed-lr -2749 lock is untouched (AMT-RUN-CLIP disarms on exit).
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
0.1 RGT-MLP-N AMT-RUN-CLIP
AMT-STEPS@ RGT-MLP-N T=
AMT-INITIAL@ SC-MILLI 130 T=            \ identical init (clip acts on grads, not the step-0 loss)
AMT-FINAL@ AMT-INITIAL@ f< TTRUE        \ loss decreased
AMT-FINAL@ SC-MILLI -2505 T=           \ clipped final NLL (new regression lock)

\ determinism: same clip + seed -> identical final loss (exact)
AMT-FINAL@ RGT-L1 !
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
0.1 RGT-MLP-N AMT-RUN-CLIP
AMT-FINAL@ RGT-L1 @ f= TTRUE

\ a clip above every step's grad norm never rescales -> bit-identical to the unclipped
\ -2749 run (integration proof that not clipping applies no eps perturbation)
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
1000000.0 RGT-MLP-N AMT-RUN-CLIP
AMT-FINAL@ SC-MILLI -2749 T=

\ ============================================================================
\ Host batch-loop + gradient-accumulation trainer (INTERIM; BTC-3)
\ docs/batch-sequence-design.md section 5 BTC-3. Proves: the per-slot running-sum
\ (SC-GRAD-ACCUM!) equals the SUM of the independent element-level reference
\ gradients over the batch; a B=1 batch is bit-identical to the single-sequence
\ AMT path; the running buffers are zeroed at step start so a stale grad cannot
\ leak across steps; and deterministic B=4 training composes with clip (on the
\ accumulated norm) and the LR schedule under committed locks.
\ ============================================================================
4 constant RGB-B                     \ sequences in the batch fixtures
variable RGB-L1                      \ determinism stash for the batch final loss

\ reference running-sum buffers (element-level grads summed over the batch)
create RGB-W1S SC-W1N cells allot
create RGB-B1S SC-B1N cells allot
create RGB-W2S SC-W2N cells allot
create RGB-B2S SC-B2N cells allot

\ post-step parameter snapshots (grad-leak bit-identity comparison)
create RGB-W1C SC-W1N cells allot
create RGB-B1C SC-B1N cells allot
create RGB-W2C SC-W2N cells allot
create RGB-B2C SC-B2N cells allot

: RGB-COPY ( ptr a ptr a n -- ) {: s:ptr d:ptr n:n :}
   n 0 ?do  s i T-GET  d i T-SET  loop ;

: RGB-ZERO-REF ( -- )
   0.0 RGB-W1S SC-W1N T-FILL   0.0 RGB-B1S SC-B1N T-FILL
   0.0 RGB-W2S SC-W2N T-FILL   0.0 RGB-B2S SC-B2N T-FILL ;

\ mirror sequence b's stored X slice into SC-X so the SC-X-reading reference sees
\ the same input the executor was bound to (AMT-BIND-SEQ binds the slice + targets)
: RGB-SEQ-X! ( n -- ) {: b:n :}
   SC-XN 0 ?do  AMT-BX b SC-XN * i +  T-GET  SC-X i  T-SET  loop ;

: RGB-ACC-REF ( -- )                 \ running-sum the element-level reference grads
   RGB-W1S RG-DW1 SC-W1N T-ADD!   RGB-B1S RG-DB1 SC-B1N T-ADD!
   RGB-W2S RG-DW2 SC-W2N T-ADD!   RGB-B2S RG-DB2 SC-B2N T-ADD! ;

\ one accumulation pass over the batch at the (unchanged) initial params: the
\ trainer running-sum (device under test) alongside the element-level reference
: RGB-ACCUM! ( -- )
   AMT-BATCH-ZERO-GRADS
   RGB-ZERO-REF
   RGB-B 0 ?do
      i AMT-BIND-SEQ                 \ bind seq i slice + copy its targets to SC-Y
      i RGB-SEQ-X!                   \ mirror seq i's X into SC-X for the reference
      AMT-GRADS drop                 \ executor forward+backward -> BW grad nodes
      AMT-ACCUM-GRADS                \ running-sum the executor grads
      RG-GRADS drop                  \ element-level reference at the same params
      RGB-ACC-REF                    \ running-sum the reference grads
   loop ;

\ snapshot the live parameters after a step (grad-leak comparison)
: RGB-SNAP-PARAMS ( -- )
   SC-W1 RGB-W1C SC-W1N RGB-COPY   SC-B1 RGB-B1C SC-B1N RGB-COPY
   SC-W2 RGB-W2C SC-W2N RGB-COPY   SC-B2 RGB-B2C SC-B2N RGB-COPY ;

\ ---- accumulation correctness: running-sum == summed element-level reference ----
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGB-B AMT-BATCH-SETUP
RGB-ACCUM!
AMT-W1G RGB-W1S SC-W1N T-REL-L2  RG-TOL f< TTRUE
AMT-B1G RGB-B1S SC-B1N T-REL-L2  RG-TOL f< TTRUE
AMT-W2G RGB-W2S SC-W2N T-REL-L2  RG-TOL f< TTRUE
AMT-B2G RGB-B2S SC-B2N T-REL-L2  RG-TOL f< TTRUE

\ ---- B=1 identity: a one-sequence batch equals the single AMT path (exact) ------
\ Strongest no-regression proof: sequence 0 of the loaded batch is the committed
\ SC-GEN-DATA, so a 60-step B=1 batch run and AMT-RUN share the trajectory bit-for-
\ bit (1/B = 1 leaves the summed grad and the loss unchanged).
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGT-MLP-N AMT-RUN
AMT-FINAL@ RGB-L1 !
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
1 RGT-MLP-N AMT-BATCH-RUN
AMT-BATCH-INITIAL@ SC-MILLI 130 T=          \ same committed init as the single path
AMT-BATCH-FINAL@ RGB-L1 @ f= TTRUE          \ bit-identical final loss

\ ---- fail-closed: the running buffers are zeroed at step start (no grad leak) ----
\ A clean one-step update, then an identical step whose accumulators are POISONED
\ before it: step-start zeroing neutralizes the poison, so the params are bit-
\ identical. Removing AMT-BATCH-ZERO-GRADS lets the poison leak in and the params
\ diverge (red-first).
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
2 AMT-BATCH-SETUP
AMT-BATCH-STEP drop
RGB-SNAP-PARAMS
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
2 AMT-BATCH-SETUP
7.0 AMT-W1G SC-W1N T-FILL   7.0 AMT-B1G SC-B1N T-FILL
7.0 AMT-W2G SC-W2N T-FILL   7.0 AMT-B2G SC-B2N T-FILL
AMT-BATCH-STEP drop
SC-W1 RGB-W1C SC-W1N T-DIST2  0.0 f= TTRUE
SC-B1 RGB-B1C SC-B1N T-DIST2  0.0 f= TTRUE
SC-W2 RGB-W2C SC-W2N T-DIST2  0.0 f= TTRUE
SC-B2 RGB-B2C SC-B2N T-DIST2  0.0 f= TTRUE

\ ---- B=4 deterministic training + committed locks -------------------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGB-B RGT-MLP-N AMT-BATCH-RUN
AMT-BATCH-STEPS@ RGT-MLP-N T=
AMT-BATCH-INITIAL@ SC-MILLI 188 T=          \ mean initial NLL over the 4 sequences
AMT-BATCH-FINAL@ AMT-BATCH-INITIAL@ f< TTRUE
AMT-BATCH-FINAL@ SC-MILLI -2453 T=          \ committed batch final NLL (new lock)
AMT-BATCH-FINAL@ RGB-L1 !
\ determinism: same seed -> identical final loss (exact)
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGB-B RGT-MLP-N AMT-BATCH-RUN
AMT-BATCH-FINAL@ RGB-L1 @ f= TTRUE

\ ---- clip on the ACCUMULATED global norm ----------------------------------------
\ A small clip (0.1, below the meaned batch-grad norms) rescales the accumulated
\ gradient, giving a deterministic final distinct from the unclipped -2453 lock;
\ AMT-BATCH-RUN-CLIP disarms on exit so the unclipped trajectory is untouched.
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
0.1 RGB-B RGT-MLP-N AMT-BATCH-RUN-CLIP
AMT-BATCH-FINAL@ SC-MILLI -2423 T=          \ clipped batch final NLL (new lock)
AMT-BATCH-FINAL@ RGB-L1 @ f= 0= TTRUE       \ distinct from the unclipped trajectory
\ a clip above every meaned batch-grad norm never rescales -> bit-identical to the
\ unclipped -2453 run (no eps perturbation on the accumulated norm)
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
1000000.0 RGB-B RGT-MLP-N AMT-BATCH-RUN-CLIP
AMT-BATCH-FINAL@ RGB-L1 @ f= TTRUE

\ ---- LR schedule composes with the batch loop -----------------------------------
\ LR-SCHED(warmup=10, decay=50, min=0.002, max=0.02) over the batch step count;
\ deterministic final distinct from the fixed-lr -2453 lock; disarms on exit.
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
10 50 0.002 0.02 RGB-B RGT-MLP-N AMT-BATCH-RUN-SCHED
AMT-BATCH-FINAL@ SC-MILLI -1912 T=          \ scheduled batch final NLL (new lock)

\ ---- zero regression: the single-path -2749 lock survives the batch runs --------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
RGT-MLP-N AMT-RUN
AMT-FINAL@ SC-MILLI -2749 T=

\ ============================================================================
\ Training-state checkpoint: resume equivalence + t-matters divergence (BTC-3+)
\ Proves a run trained N+M steps is BIT-IDENTICAL to one trained N, checkpointed,
\ then RESUMED into a FRESH trainer and trained M more - under a fixed LR AND with
\ the LR schedule + global-norm clip armed (the schedule reads ADAM-T, which the
\ checkpoint persists, so a resume continues it at the right step). A resume that
\ RESETS the step counter (ADAM-STEP-RESET) diverges, proving ADAM-T is
\ load-bearing. Equivalence is witnessed by byte-identical state checkpoints
\ (maki/train-state.f digests every captured cell), which also re-exercises
\ save->load->save determinism on the real f64 parameter + moment buffers.
\ ============================================================================
20 constant TSR-N                    \ steps trained before the checkpoint
40 constant TSR-M                    \ steps resumed after it

$4000 constant TSR-FCAP
create TSR-REF TSR-FCAP allot  variable TSR-REFU
create TSR-CMP TSR-FCAP allot  variable TSR-CMPU

: TSR-STEPS ( n -- )  0 ?do  AMT-STEP drop  loop ;
: TSR-READ-REF ( ptr u8 n -- ) {: na:ptr nu:n :}  na nu TSC-PATH$ TSR-REF TSR-FCAP READ-ALL TSR-REFU ! ;
: TSR-READ-CMP ( ptr u8 n -- ) {: na:ptr nu:n :}  na nu TSC-PATH$ TSR-CMP TSR-FCAP READ-ALL TSR-CMPU ! ;
: TSR-SAME? ( -- bool )              \ ref vs cmp checkpoint byte-identical
   TSR-REFU @ TSR-CMPU @ <> if false exit then
   TSR-REFU @ 0 ?do  TSR-REF i + c@ TSR-CMP i + c@ <> if false unloop exit then  loop true ;
: TSR-ARM ( -- )  10 50 0.002 0.02 AMT-SCHED!  0.1 AMT-CLIP! ;   \ LR schedule + real global-norm clip
: TSR-DISARM ( -- )  AMT-SCHED-OFF AMT-CLIP-OFF ;

\ ---- fixed-LR resume equivalence --------------------------------------------
STORE-RESET
\ uninterrupted N+M -> snapshot the final state to "ref"
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  TSR-N TSR-M + TSR-STEPS
ADAM-T@ TSR-N TSR-M + T=
AMT-CKPT-SEGS s" ref" TSC-SAVE
\ producer: fresh trainer, N steps, checkpoint at step N
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  TSR-N TSR-STEPS  AMT-CKPT-SAVE
\ resume: FRESH trainer, load step-N state, train M -> snapshot to "cmp"
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  AMT-CKPT-LOAD
ADAM-T@ TSR-N T=
TSR-M TSR-STEPS
AMT-CKPT-SEGS s" cmp" TSC-SAVE
s" ref" TSR-READ-REF  s" cmp" TSR-READ-CMP
TSR-SAME? TTRUE                      \ resumed final state == uninterrupted, bit-for-bit

\ ---- t matters: a resume that drops the step counter diverges ----------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  AMT-CKPT-LOAD
ADAM-STEP-RESET                      \ forget t (keep params + moments)
ADAM-T@ 0 T=
TSR-M TSR-STEPS
AMT-CKPT-SEGS s" cmp" TSC-SAVE
s" cmp" TSR-READ-CMP
TSR-SAME? 0= TTRUE                   \ diverges from the uninterrupted run

\ ---- resume equivalence with the LR schedule AND clip armed ------------------
STORE-RESET
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  TSR-ARM  TSR-N TSR-M + TSR-STEPS  TSR-DISARM
AMT-CKPT-SEGS s" ref" TSC-SAVE
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  TSR-ARM  TSR-N TSR-STEPS  TSR-DISARM  AMT-CKPT-SAVE
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
AMT-SETUP  AMT-CKPT-LOAD
ADAM-T@ TSR-N T=
TSR-ARM  TSR-M TSR-STEPS  TSR-DISARM      \ re-arm the SAME schedule+clip; it continues at step N
AMT-CKPT-SEGS s" cmp" TSC-SAVE
s" ref" TSR-READ-REF  s" cmp" TSR-READ-CMP
TSR-SAME? TTRUE

T-REPORT

;package
