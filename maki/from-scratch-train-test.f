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

T-REPORT

;package
