\ maki/eval-train.f - the maki model train/eval leg (docs/maki/eval.md (ii)).
\
\ Runs the LANDED host trainers end-to-end from their committed seeds - the
\ Adam windowed MLP (AMT-*) and the 1-layer attention block (ATN-*, both
\ maki/adam-train.f) - and reports the train/eval verdict as a deterministic
\ table: steps, initial/final loss (milli fixed point, SC-MILLI), and the
\ committed convergence gate. The eval FAILS if either trainer stops
\ converging; the exact per-step loss regression locks live in
\ maki/adam-train-test.f. Host-only; the device training loop is the
\ gpu-train/M-series work, not this eval.

require lib/test.f
require lib/float.f
require maki/executor.f          \ flatten the deep chain (engine include depth)
require maki/backward.f
require maki/from-scratch-model.f
require maki/from-scratch-train.f
require maki/adam-train.f

package MAKI

\ committed run lengths of this eval (same lengths adam-train-test locks)
60 constant ET-MLP-N
40 constant ET-ATTN-N

: ET-YESNO ( bool -- )
   if s" yes" type exit then
   s" no" type ;

: ET-INT ( n -- )  SB-RESET SB-INT SB$ type ;

: ET-CELL ( n -- )  s"  | " type ET-INT ;

: ET-ROW ( ptr u8 n n n n bool -- ) {: steps:n init:n final:n ok:bool :}
   s" | " type type
   steps ET-CELL  init ET-CELL  final ET-CELL
   s"  | " type ok ET-YESNO s"  |" type cr ;

: ET-HEADER ( -- )
   s" | trainer | steps | loss-initial-milli | loss-final-milli | converged |" type cr ;

: ET-MLP-ROW ( -- )
   s" mlp-adam" ET-MLP-N
   AMT-INITIAL@ SC-MILLI  AMT-FINAL@ SC-MILLI  AMT-CONVERGED?  ET-ROW ;

: ET-ATTN-ROW ( -- )
   s" attn-adam" ET-ATTN-N
   ATN-INITIAL@ SC-MILLI  ATN-FINAL@ SC-MILLI  ATN-CONVERGED?  ET-ROW ;

T-RESET

\ --- MLP under Adam: fresh capture, committed steps, convergence is the verdict ---
MODEL: SCRATCH-MLP ( x:8x6 w1:6x16 b1:1x16 w2:16x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
ET-MLP-N AMT-RUN
AMT-FINAL@ AMT-INITIAL@ f< TTRUE
AMT-CONVERGED? TTRUE

\ --- attention under Adam: fresh capture, committed steps ---
MODEL: ADAM-ATTN ( q:4x3 kt:3x4 s:1x1 v:4x3 -- o ) MATMUL SCALE SOFTMAX-ROW MATMUL ;
ET-ATTN-N ATN-RUN
ATN-FINAL@ ATN-INITIAL@ f< TTRUE
ATN-CONVERGED? TTRUE

\ --- the train/eval report ---
ET-HEADER
ET-MLP-ROW
ET-ATTN-ROW

T-REPORT

;package
