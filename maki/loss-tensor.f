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

package MAKI
public

: TT-MSE-LOSS ( ptr a ptr a n -- r ) {: pb:ptr tb:ptr len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  MSE  f+  loop ;

: TT-MSE-DY ( ptr a ptr a ptr a n -- ) {: pb:ptr tb:ptr dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  MSE-GRAD  dyb i T-SET  loop ;

\ --- Gaussian NLL over (target y, predicted mean mu, predicted log-variance lv) ---
: TT-NLL-LOSS ( ptr a ptr a ptr a n -- r ) {: yb:ptr mb:ptr lvb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL  f+  loop ;

\ gradient of the summed NLL w.r.t. the mean buffer -> dmb
: TT-NLL-DMU ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr lvb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-MU-GRAD  dmb i T-SET  loop ;

\ gradient of the summed NLL w.r.t. the log-variance buffer -> dlvb
: TT-NLL-DLV ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr lvb:ptr dlvb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  lvb i T-GET  NLL-LOGVAR-GRAD  dlvb i T-SET  loop ;

\ --- Mahalanobis (diagonal covariance) over (y, mu, var); var>0 or throw E-MK-VAR ---
: TT-MAHALANOBIS-LOSS ( ptr a ptr a ptr a n -- r ) {: yb:ptr mb:ptr vb:ptr len:n :}
   0.0  len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS  f+  loop ;

: TT-MAHALANOBIS-DMU ( ptr a ptr a ptr a ptr a n -- ) {: yb:ptr mb:ptr vb:ptr dmb:ptr len:n :}
   len 0 ?do  yb i T-GET  mb i T-GET  vb i T-GET  MAHALANOBIS-GRAD  dmb i T-SET  loop ;

\ --- Huber over (pred, tgt) with a shared transition delta ---
: TT-HUBER-LOSS ( ptr a ptr a r n -- r ) {: pb:ptr tb:ptr delta:r len:n :}
   0.0  len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER  f+  loop ;

: TT-HUBER-DY ( ptr a ptr a r ptr a n -- ) {: pb:ptr tb:ptr delta:r dyb:ptr len:n :}
   len 0 ?do  pb i T-GET  tb i T-GET  delta  HUBER-GRAD  dyb i T-SET  loop ;

end-package
