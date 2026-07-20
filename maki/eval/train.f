\ maki/eval/train.f - the maki model train/eval leg (docs/maki/eval.md (ii)).
\
\ Drives maki/train.f's library training loop (forward -> MSE -> SGD) to
\ convergence on two library-owned reference regressions - a scalar y = w*x and a
\ per-element tensor y[i] = w[i]*x[i] - from committed deterministic seeds, and
\ reports the train/eval verdict as a deterministic table: steps, initial/final
\ loss (milli fixed point, ET-MILLI), and the convergence gate (loss at least
\ halved). The eval FAILS if either regression stops converging. Host-only; the
\ concrete Adam trainers + their exact per-step loss regression locks live in the
\ nanoGPT example's Adam trainer tests. maki -> habu only.

require lib/test.f
require lib/float.f
require maki/train.f
require lib/fmt.f

package MAKI

\ committed run lengths of this eval (each converges well within them)
40 constant ET-SCALAR-N
60 constant ET-TENSOR-N

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
   s" | regression | steps | loss-initial-milli | loss-final-milli | converged |" type cr ;

: ET-MILLI ( r -- n )                       \ float -> signed milli-units (round half away)
   1000.0 f*  dup f0< if 0.5 f- else 0.5 f+ then  f>s ;

\ ---- scalar regression y = w*x: w0=0, x=2, t=6 (optimum w=3), SGD lr=0.05 ------
: ET-SCALAR-INIT  ( -- r )  0.0 2.0 6.0 LOSS-AT ;
: ET-SCALAR-FINAL ( -- r )
   0.0 2.0 6.0 0.05 ET-SCALAR-N TRAIN-N  2.0 6.0 LOSS-AT ;

\ ---- tensor regression y[i] = w[i]*x[i]: w0=0, x=[1..4], t=2*x (optimum w=2) ----
4 constant ET-TLEN
create ET-TW ET-TLEN cells allot
create ET-TX ET-TLEN cells allot
create ET-TT ET-TLEN cells allot

: ET-TENSOR-DATA ( -- )                     \ fresh deterministic init/data
   0.0 ET-TW ET-TLEN T-FILL
   ET-TLEN 0 ?do  i 1+ s>f  ET-TX i T-SET  loop         \ x = 1,2,3,4
   ET-TLEN 0 ?do  i 1+ s>f 2.0 f*  ET-TT i T-SET  loop ; \ t = 2,4,6,8

: ET-TENSOR-INIT ( -- r )
   ET-TENSOR-DATA  ET-TW ET-TX ET-TT ET-TLEN T-LOSS ;

: ET-TENSOR-FINAL ( -- r )
   ET-TENSOR-DATA
   0.02 ET-TW ET-TX ET-TT ET-TLEN ET-TENSOR-N T-TRAIN-N!
   ET-TW ET-TX ET-TT ET-TLEN T-LOSS ;

variable ET-SI   variable ET-SF   variable ET-TI   variable ET-TF

T-RESET

\ --- scalar SGD: loss strictly falls and is at least halved -------------------
ET-SCALAR-INIT  ET-SI !
ET-SCALAR-FINAL ET-SF !
ET-SF @ ET-SI @ f< TTRUE
ET-SF @ ET-SI @ 0.5 f* f< TTRUE

\ --- tensor SGD: loss strictly falls and is at least halved ------------------
ET-TENSOR-INIT  ET-TI !
ET-TENSOR-FINAL ET-TF !
ET-TF @ ET-TI @ f< TTRUE
ET-TF @ ET-TI @ 0.5 f* f< TTRUE

\ --- the train/eval report ---------------------------------------------------
ET-HEADER
s" linear-sgd"    ET-SCALAR-N  ET-SI @ ET-MILLI  ET-SF @ ET-MILLI  ET-SF @ ET-SI @ 0.5 f* f<  ET-ROW
s" linear-tensor" ET-TENSOR-N  ET-TI @ ET-MILLI  ET-TF @ ET-MILLI  ET-TF @ ET-TI @ 0.5 f* f<  ET-ROW

T-REPORT

;package
