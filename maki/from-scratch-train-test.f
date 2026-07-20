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
require maki/from-scratch-train.f
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

T-REPORT

;package
