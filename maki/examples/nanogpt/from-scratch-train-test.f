\ maki/from-scratch-train-test.f - one training step over the executor.
\
\ Drives the landed machinery (BW-BUILD / EX-BIND / EX-RUN / T-SGD!) through
\ SC-SETUP + SC-STEP and proves: the exact IR that trains also gradchecks
\ (analytic backward == finite diff, GC-RUN V-PASS); the committed initial loss;
\ one SGD step reduces the batch NLL; the update actually writes the parameter
\ buffers; a step is bit-deterministic from a fresh capture; and the accessor
\ fail-closed guard before any run (E-SC-RUN). This file NEVER calls SC-RUN, so
\ the run flag stays clear for the throw tests - it must be wired BEFORE
\ maki/from-scratch-test.f (the first file that runs SC-RUN) in maki/test.f.

require lib/test.f
require lib/float.f
require maki/examples/nanogpt/from-scratch-train.f
require maki/gradcheck.f

package MAKI

\ throw-path wrappers (accessors used before any training run)
: REP-THROW  ( -- )  SCRATCH-REPORT 2drop ;
: INIT-THROW ( -- )  SC-INITIAL@ drop ;

variable ST-A     \ stashed step loss
variable ST-B
variable ST-W     \ stashed parameter value

T-RESET

\ ---- fail closed: accessors before any run throw E-SC-RUN --------------------
' REP-THROW  E-SC-RUN TTHROWS
' INIT-THROW E-SC-RUN TTHROWS

\ ---- the exact IR that trains also gradchecks on host -----------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GC-RUN V-PASS T=

\ ---- one SGD step reduces the loss; initial loss is the committed 130 mNLL ----
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SC-SETUP
SC-STEP ST-A !
ST-A @ SC-MILLI 130 T=                    \ committed initial batch NLL
SC-STEP ST-B !
ST-B @ ST-A @ f< TTRUE                     \ second step's pre-update loss is lower

\ ---- the update writes the parameter buffers (gradient applied) --------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SC-SETUP
SC-W1 0 T-GET ST-W !
SC-STEP drop
SC-W1 0 T-GET ST-W @ f= TFALSE            \ w1[0] moved after a step

\ ---- a step is bit-deterministic from a fresh capture -----------------------
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SC-SETUP  SC-STEP ST-A !
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
SC-SETUP  SC-STEP ST-B !
ST-A @ ST-B @ f= TTRUE

\ ---- LR-COS: cosine on [0,pi] vs exact closed forms (x1e7, half-away rounding) --
\ Series is alternating with decreasing terms on [0,pi/2], so its truncation error
\ is bounded by the first omitted term (pi/2)^14/14! ~ 6.4e-9 < 1e-8; [pi/2,pi] is
\ the exact reflection cos(x)=-cos(pi-x). Every angle below is exact at unit 1e-7.
: COS7 ( r -- n )  10000000.0 f* dup f0< if 0.5 f- else 0.5 f+ then f>s ;
0.0                  LR-COS COS7   10000000 T=   \ cos 0     = 1
LR-PI 6.0 f/         LR-COS COS7    8660254 T=   \ cos pi/6  = sqrt3/2
LR-PI 4.0 f/         LR-COS COS7    7071068 T=   \ cos pi/4  = sqrt2/2
LR-PI 3.0 f/         LR-COS COS7    5000000 T=   \ cos pi/3  = 1/2
LR-PI 2.0 f/         LR-COS COS7          0 T=   \ cos pi/2  = 0
LR-PI 2.0 f* 3.0 f/  LR-COS COS7   -5000000 T=   \ cos 2pi/3 = -1/2
LR-PI                LR-COS COS7  -10000000 T=   \ cos pi    = -1
\ proven max-error bound: worst realized error is at pi/2 (~6.3e-9), asserted < 1e-8
LR-PI 2.0 f/  LR-COS  fabs  0.00000001 f<  TTRUE

\ ---- LR-SCHED: nanoGPT lr(t) across all three phases + both boundaries ---------
\ warmup=4, decay=8, min=0.1, max=1.0 (lr x1e7). Warmup uses lmax*(t+1)/(warm+1);
\ t=warm and t=decay hit the exact cosine boundaries max/min.
4 8 0.1 1.0 0  LR-SCHED COS7    2000000 T=   \ warmup    t=0: 1.0*1/5
4 8 0.1 1.0 3  LR-SCHED COS7    8000000 T=   \ warmup    t=3: 1.0*4/5 (last)
4 8 0.1 1.0 4  LR-SCHED COS7   10000000 T=   \ boundary  t=warm : ratio 0 -> max
4 8 0.1 1.0 5  LR-SCHED COS7    8681981 T=   \ cosine    t=5: cos(pi/4)
4 8 0.1 1.0 6  LR-SCHED COS7    5500000 T=   \ cosine    t=6: cos(pi/2)
4 8 0.1 1.0 8  LR-SCHED COS7    1000000 T=   \ boundary  t=decay: ratio 1 -> min
4 8 0.1 1.0 9  LR-SCHED COS7    1000000 T=   \ flat      t>decay: min

\ ---- red-first domain guards: every violation throws E-LR-SCHED -----------------
: LRS-NEG-T   ( -- )  4 8 0.1 1.0 -1 LR-SCHED drop ;          \ t < 0
: LRS-WARM-GE ( -- )  8 8 0.1 1.0 0  LR-SCHED drop ;          \ warmup >= decay
: LRS-MIN-GT  ( -- )  4 8 1.0 0.1 0  LR-SCHED drop ;          \ min > max
: LRS-INF-MAX ( -- )  4 8 0.1 1.0 0.0 f/ 0 LR-SCHED drop ;    \ max = +inf
: LRS-NAN-MIN ( -- )  4 8 0.0 0.0 f/ 1.0 5 LR-SCHED drop ;    \ min = nan
' LRS-NEG-T   E-LR-SCHED TTHROWS
' LRS-WARM-GE E-LR-SCHED TTHROWS
' LRS-MIN-GT  E-LR-SCHED TTHROWS
' LRS-INF-MAX E-LR-SCHED TTHROWS
' LRS-NAN-MIN E-LR-SCHED TTHROWS

\ ---- global-norm gradient clip: exact scaled values + post-clip norm ============
\ Synthetic grad buffers GA=[3,4], GB=[12]: global L2 norm = sqrt(25+144)=13. clip=1
\ < 13 so coef = 1/(13+1e-6) < 1; every grad scales by coef (torch clip_grad_norm_).
\ Goldens are the exact scaled values at unit 1e7 (half-away, COS7); the eps shifts
\ them below the 1e7 rounding boundary, so the integers equal g*clip/norm.
create GCA 2 cells allot   create GCB 1 cells allot
variable GCV                               \ stashed clip coefficient
3.0 GCA 0 T-SET   4.0 GCA 1 T-SET   12.0 GCB 0 T-SET
GCA 2 T-NORM2  GCB 1 T-NORM2 f+  fsqrt  COS7  130000000 T=      \ global norm = 13.0
GCA 2 T-NORM2  GCB 1 T-NORM2 f+  fsqrt  1.0 GRAD-CLIP-COEF  GCV !
GCV @ 1.0 f< TTRUE                          \ coef < 1: clipping engaged
GCV @ GCA 2 GCLIP-SCALE!   GCV @ GCB 1 GCLIP-SCALE!
GCA 0 T-GET COS7  2307692 T=               \ 3  / (13+eps)   (x1e7, half-away)
GCA 1 T-GET COS7  3076923 T=               \ 4  / (13+eps)
GCB 0 T-GET COS7  9230769 T=               \ 12 / (13+eps)
\ post-clip global norm equals clip within a tight fp tolerance (deviation ~eps/norm)
GCA 2 T-NORM2  GCB 1 T-NORM2 f+  fsqrt  1.0 f-  fabs  0.000001 f<  TTRUE

\ ---- below-clip: norm < clip -> coef>=1 -> grads bit-unchanged (no eps perturbation) --
create GCU 2 cells allot
0.3 GCU 0 T-SET   0.4 GCU 1 T-SET          \ global norm = 0.5 < clip 1.0
GCU 2 T-NORM2 fsqrt  1.0 GRAD-CLIP-COEF  GCV !
GCV @ 1.0 f< TFALSE                         \ coef >= 1: not clipping
GCV @ GCU 2 GCLIP-SCALE!                    \ no-op when coef >= 1
GCU 0 T-GET  0.3 f=  TTRUE                  \ bit-identical: no rescale, no eps applied
GCU 1 T-GET  0.4 f=  TTRUE

\ ---- zero-grad edge: norm 0 -> coef = clip/eps (finite, >=1), never divides by 0 -----
create GCZ 3 cells allot
0.0 GCZ 0 T-SET  0.0 GCZ 1 T-SET  0.0 GCZ 2 T-SET
1.0  GCZ 3 T-NORM2 fsqrt 1.0 GRAD-CLIP-COEF  f<  TTRUE       \ 1.0 < coef: not clipping AND not NaN
GCZ 3 T-NORM2 fsqrt 1.0 GRAD-CLIP-COEF  2000000.0 f<  TTRUE  \ coef < 2e6: finite (no 0-divide to +inf)

\ ---- red-first domain guards: clip <= 0 or non-finite throw E-GRAD-CLIP ----------
: GC-CLIP-ZERO ( -- )  1.0 0.0 GRAD-CLIP-COEF drop ;             \ clip = 0
: GC-CLIP-NEG  ( -- )  1.0 0.5 fnegate GRAD-CLIP-COEF drop ;     \ clip < 0
: GC-CLIP-INF  ( -- )  1.0 1.0 0.0 f/ GRAD-CLIP-COEF drop ;      \ clip = +inf
: GC-CLIP-NAN  ( -- )  1.0 0.0 0.0 f/ GRAD-CLIP-COEF drop ;      \ clip = nan
' GC-CLIP-ZERO E-GRAD-CLIP TTHROWS
' GC-CLIP-NEG  E-GRAD-CLIP TTHROWS
' GC-CLIP-INF  E-GRAD-CLIP TTHROWS
' GC-CLIP-NAN  E-GRAD-CLIP TTHROWS

T-REPORT

;package
