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

-5009 constant E-MK-TGT     \ integer class target outside [0, V)
-5017 constant E-MK-SHAPE   \ target count does not match the logit row count

package LOSS
public

: TT-MSE ( ptr a ptr a n -- r ) {: pb:ptr tb:ptr len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  MSE  f+  loop ;

: TT-MSE-DY ( ptr a ptr a ptr a n -- ) {: pb:ptr tb:ptr dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  MSE-GRAD  dyb i T-SET  loop ;

\ --- Gaussian NLL over (target y, predicted mean mu, predicted log-variance lv) ---
: TT-NLL ( ptr a ptr a ptr a n -- r ) {: yb:ptr mb:ptr lvb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL  f+  loop ;

\ gradient of the summed NLL w.r.t. the mean buffer -> dmb
: TT-NLL-DMU ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr lvb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-MU-GRAD  dmb i T-SET  loop ;

\ gradient of the summed NLL w.r.t. the log-variance buffer -> dlvb
: TT-NLL-DLV ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr lvb:ptr dlvb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-LOGVAR-GRAD  dlvb i T-SET  loop ;

\ --- Mahalanobis (diagonal covariance) over (y, mu, var); var>0 or throw E-MK-VAR ---
: TT-MAHALANOBIS ( ptr a ptr a ptr a n -- r ) {: yb:ptr mb:ptr vb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS  f+  loop ;

: TT-MAHALANOBIS-DMU ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr vb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS-GRAD  dmb i T-SET  loop ;

\ --- Huber over (pred, tgt) with a shared transition delta ---
: TT-HUBER ( ptr a ptr a r n -- r ) {: pb:ptr tb:ptr delta:r len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER  f+  loop ;

: TT-HUBER-DY ( ptr a ptr a r ptr a n -- ) {: pb:ptr tb:ptr delta:r dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER-GRAD  dyb i T-SET  loop ;

\ --- stable softmax cross-entropy over logits (RxV row-major) + INTEGER targets ---
\ GPT-2's classifier loss: per row r a length-V logit vector and one integer class
\ target t_r (an integer-valued float id, one per cell, read via f>s - the same
\ contract EMB-GATHER's index operand uses). L = sum_r ( logsumexp(logits_r) -
\ logits_r[t_r] ) is the log-softmax cross-entropy. Numerically STABLE: each row's
\ logsumexp subtracts the row max before exp (never exp-then-log), so extreme
\ logits that overflow a naive exp stay finite. Two fail-closed guards: a target id
\ outside [0,V) throws E-MK-TGT; a target count != row count throws E-MK-SHAPE.

private

\ logsumexp of one V-length logit row: m + ln( sum_i exp(x_i - m) ), m = row max
: XENT-LSE ( ptr a n -- r ) {: xb:ptr v:n :}
   xb 0 T-GET  v 1 ?do  xb i T-GET  MAKI:MAX-F  loop  {: m:r :}
   0.0  v 0 ?do  xb i T-GET  m f-  FMATH:FEXP  f+  loop  MAKI:FLN  m f+ ;

\ read + range-check row r's integer class target (fail closed E-MK-TGT)
: XENT-TGT ( ptr a n n -- n ) {: tb:ptr r:n v:n :}
   tb r T-GET f>s  {: t:n :}
   t 0 <  t v >=  or if E-MK-TGT throw then  t ;

\ shape guard: the targets vector holds exactly one class id per logit row
: XENT-CK-SHAPE ( n n -- ) {: r:n tn:n :}  r tn <> if E-MK-SHAPE throw then ;

public

\ summed stable cross-entropy over R rows of V logits with R integer targets
: TT-XENT ( ptr a n n ptr a n -- r ) {: lb:ptr r:n v:n tb:ptr tn:n :}
   r tn XENT-CK-SHAPE
   0.0  r 0 ?do
      lb i v * T-AT {: row:ptr :}
      tb i v XENT-TGT {: t:n :}
      row v XENT-LSE  row t T-GET f-  f+
   loop ;

\ seed cotangent dL/dlogits = softmax(logits) - onehot(target), row by row, into db
\ (RxV) - the exact y-t the fused softmax+CE backward seeds into the training loop.
: TT-XENT-SEED ( ptr a ptr a ptr a n n n -- ) {: lb:ptr tb:ptr db:ptr r:n v:n tn:n :}
   r tn XENT-CK-SHAPE
   r 0 ?do
      lb i v * T-AT {: row:ptr :}
      db i v * T-AT {: dr:ptr :}
      tb i v XENT-TGT {: t:n :}
      row dr v MAKI:SM-FWD               \ dr = softmax(row)
      dr t T-GET 1.0 f-  dr t T-SET      \ dr[t] -= 1  ->  y - onehot
   loop ;

;package
