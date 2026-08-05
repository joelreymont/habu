\ maki/loss-tensor.f - tensor-level loss family + gradients (lifts maki/loss.f).
\
\ Each loss is the maki/loss.f per-element rule reduced over a whole tensor (the
\ BLOCK-SUM the device kernel mirrors); each gradient maps the per-element analytic
\ gradient into a cotangent buffer. Covers MSE plus the Gaussian / covariance family
\ (Gaussian NLL over predicted log-variance, Mahalanobis diagonal covariance, Huber).
\ The Mahalanobis reduction inherits the fail-closed variance guard: a non-positive
\ variance element throws E-MK-VAR mid-reduction, never clamps. Needs maki/array.f +
\ maki/loss.f. maki -> habu only.

require maki/array.f
require maki/loss.f
require maki/softmax.f                 \ SM-FWD (seed softmax) + MAX-F/FLN/FEXP (stable logsumexp)
require maki/tensor.f                  \ E-MK-DIM: the shared negative/overflowing-dimension law

package LOSS
public

\ Cross-entropy's fail-closed error contract belongs to the module (no global aliases).
-5009 constant E-MK-TGT     \ target not a finite, exactly-integral class id in [0, V)
-5017 constant E-MK-SHAPE   \ target count does not match the logit row count

: TT-MSE ( ptr r ptr r n -- r ) {: pb:ptr tb:ptr len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  MSE  f+  loop ;

: TT-MSE-DY ( ptr r ptr r ptr r n -- ) {: pb:ptr tb:ptr dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  MSE-GRAD  dyb i T-SET  loop ;

\ --- Gaussian NLL over (target y, predicted mean mu, predicted log-variance lv) ---
: TT-NLL ( ptr r ptr r ptr r n -- r ) {: yb:ptr mb:ptr lvb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL  f+  loop ;

\ gradient of the summed NLL w.r.t. the mean buffer -> dmb
: TT-NLL-DMU ( ptr r ptr r ptr r ptr r n -- ) {: yb:ptr mb:ptr lvb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-MU-GRAD  dmb i T-SET  loop ;

\ gradient of the summed NLL w.r.t. the log-variance buffer -> dlvb
: TT-NLL-DLV ( ptr r ptr r ptr r ptr r n -- ) {: yb:ptr mb:ptr lvb:ptr dlvb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-LOGVAR-GRAD  dlvb i T-SET  loop ;

\ --- Mahalanobis (diagonal covariance) over (y, mu, var); var>0 or throw E-MK-VAR ---
: TT-MAHALANOBIS ( ptr r ptr r ptr r n -- r ) {: yb:ptr mb:ptr vb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS  f+  loop ;

: TT-MAHALANOBIS-DMU ( ptr r ptr r ptr r ptr r n -- ) {: yb:ptr mb:ptr vb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS-GRAD  dmb i T-SET  loop ;

\ --- Huber over (pred, tgt) with a shared transition delta ---
: TT-HUBER ( ptr r ptr r r n -- r ) {: pb:ptr tb:ptr delta:r len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER  f+  loop ;

: TT-HUBER-DY ( ptr r ptr r r ptr r n -- ) {: pb:ptr tb:ptr delta:r dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER-GRAD  dyb i T-SET  loop ;

\ --- stable softmax cross-entropy over logits (RxV row-major) + INTEGER targets ---
\ GPT-2's classifier loss: per row r a length-V logit vector and one integer class
\ target t_r (an integer-valued float id, one per cell, read via f>s - the same
\ contract EMB-GATHER's index operand uses). L = sum_r ( logsumexp(logits_r) -
\ logits_r[t_r] ) is the log-softmax cross-entropy. Numerically STABLE: each row's
\ logsumexp subtracts the row max before exp (never exp-then-log), so extreme
\ logits that overflow a naive exp stay finite. Fail-closed guards, ALL checked before
\ any output write: bad dimensions (R<0, V<1, or an overflowing R*V) throw E-MK-DIM; a
\ target count != row count throws E-MK-SHAPE; a target that is not a finite,
\ exactly-integral id in [0,V) throws E-MK-TGT. R=0 is the deliberate empty batch (zero
\ loss, no seed written). TT-XENT-SEED preflights every target before it writes any
\ gradient row, so a rejected target leaves the output buffer byte-identical. Typed
\ logit/target spans and a checker-enforced nominal class-id type stay follow-up
\ capability work: both change this public signature and every caller.

private

$7FFFFFFFFFFFFFFF constant DIM-CEIL     \ signed-cell max: R*V may not overflow past this

\ finite float (not NaN, not +/-Inf): x-x is exactly 0 only for a finite x (IEEE)
: FINITE? ( r -- bool )  dup f- 0.0 f= ;

\ logsumexp of one V-length logit row: m + ln( sum_i exp(x_i - m) ), m = row max
: XENT-LSE ( ptr r n -- r ) {: xb:ptr v:n :}
   xb 0 T-GET  v 1 ?do  xb i T-GET  MAKI:MAX-F  loop  {: m:r :}
   0.0  v 0 ?do  xb i T-GET  m f-  FMATH:FEXP  f+  loop  MAKI:FLN  m f+ ;

\ dimension law: R rows >= 0, V classes >= 1, and R*V does not overflow a cell
: XENT-CK-DIMS ( n n -- ) {: r:n v:n :}
   r 0 <  v 1 <  or if E-MK-DIM throw then
   r DIM-CEIL v / > if E-MK-DIM throw then ;

\ read + validate row r's class target: finite, exactly integral, in [0,V) (E-MK-TGT).
\ Every bound is tested in float space, so a huge or non-finite value never reaches f>s.
: XENT-TGT ( ptr r n n -- n ) {: tb:ptr r:n v:n :}
   tb r T-GET {: x:r :}
   x FINITE? 0= if E-MK-TGT throw then                 \ NaN / +Inf / -Inf
   x 0.0 f<  x v s>f f< 0=  or if E-MK-TGT throw then   \ x < 0 or x >= V
   x f>s {: t:n :}                                      \ safe: x is finite and in [0,V)
   t s>f x f= 0= if E-MK-TGT throw then                 \ not exactly integral (fractional)
   t ;

\ shape guard: the targets vector holds exactly one class id per logit row
: XENT-CK-SHAPE ( n n -- ) {: r:n tn:n :}  r tn <> if E-MK-SHAPE throw then ;

public

\ summed stable cross-entropy over R rows of V logits with R integer targets.
\ Validates dimensions, shape, and every target; R=0 is the empty batch (returns 0.0).
: TT-XENT ( ptr r n n ptr r n -- r ) {: lb:ptr r:n v:n tb:ptr tn:n :}
   r v XENT-CK-DIMS
   r tn XENT-CK-SHAPE
   0.0  r 0 ?do
      lb i v * T-AT {: row:ptr :}
      tb i v XENT-TGT {: t:n :}
      row v XENT-LSE  row t T-GET f-  f+
   loop ;

\ seed cotangent dL/dlogits = softmax(logits) - onehot(target), row by row, into db
\ (RxV) - the exact y-t the fused softmax+CE backward seeds into the training loop.
\ Preflights dimensions, shape, and EVERY target before the first write, so a rejected
\ target leaves db byte-identical (no partially-written gradient rows).
: TT-XENT-SEED ( ptr r ptr r ptr r n n n -- ) {: lb:ptr tb:ptr db:ptr r:n v:n tn:n :}
   r v XENT-CK-DIMS
   r tn XENT-CK-SHAPE
   r 0 ?do  tb i v XENT-TGT drop  loop     \ preflight every target; throw leaves db intact
   r 0 ?do
      lb i v * T-AT {: row:ptr :}
      db i v * T-AT {: dr:ptr :}
      tb i v XENT-TGT {: t:n :}
      row dr v MAKI:SM-FWD               \ dr = softmax(row)
      dr t T-GET 1.0 f-  dr t T-SET      \ dr[t] -= 1  ->  y - onehot
   loop ;

;package
