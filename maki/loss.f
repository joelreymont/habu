\ maki/loss.f - loss functions for maki (scalar/element-wise).
\
\ Per-element loss + its gradient w.r.t. the prediction. The tensor-level loss is
\ the element loss reduced over a tensor (a BLOCK-SUM kernel), later; this is the
\ element rule, runnable checked Habu. Needs maki/fmath.f for FEXP (the Gaussian
\ NLL precision term exp(-logvar)). Maki owns its own error range (-5000..-5099);
\ it never extends lib/errors.f (the one-way fence). maki -> habu only.

require maki/fmath.f

-5030 constant E-MK-VAR   \ non-positive variance into a variance-consuming loss

package LOSS
public

\ Squared error:        L = (pred - tgt)^2
: MSE ( r r -- r ) {: pred tgt :}
   pred tgt f-  {: d :}  d d f* ;

\ ... and its gradient:  dL/dpred = 2*(pred - tgt)
: MSE-GRAD ( r r -- r ) {: pred tgt :}
   pred tgt f-  2.0 f* ;

\ Absolute error:       L = |pred - tgt|
: L1 ( r r -- r ) {: pred tgt :}
   pred tgt f-  fabs ;

\ Squared residual (y-mu)^2, shared by the Gaussian / covariance family. Symmetric,
\ so predicted-vs-target argument order does not change the value.
: RES2 ( r r -- r ) {: y:r mu:r :}
   y mu f-  {: d:r :}  d d f* ;

\ --- Gaussian negative log likelihood (predicted log-variance) ---
\ Predicting logvar (not variance) keeps the precision exp(-logvar) > 0 for ANY real
\ logvar, so it is unconditionally well defined - no clamp, no fail path here.
\ Per-element rule (the additive constant 0.5*ln(2*pi) is omitted; it shifts every
\ NLL by the same constant and drops out of every gradient):
\   NLL(y, mu, logvar) = 0.5 * ( logvar + (y-mu)^2 * exp(-logvar) )

\ precision = exp(-logvar)  (the inverse variance implied by the predicted logvar)
: NLL-PREC ( r -- r ) {: lv:r :}  lv fnegate FEXP ;

: NLL ( r r r -- r ) {: y:r mu:r lv:r :}
   y mu RES2  lv NLL-PREC  f*  lv f+  0.5 f* ;

\ dNLL/dmu = (mu - y) * exp(-logvar)
: NLL-MU-GRAD ( r r r -- r ) {: y:r mu:r lv:r :}
   mu y f-  lv NLL-PREC  f* ;

\ dNLL/dlogvar = 0.5 * ( 1 - (y-mu)^2 * exp(-logvar) )
: NLL-LOGVAR-GRAD ( r r r -- r ) {: y:r mu:r lv:r :}
   1.0  y mu RES2  lv NLL-PREC  f*  f-  0.5 f* ;

\ --- Mahalanobis distance (diagonal covariance v1) ---
\ Per-element (y-mu)^2 / var with a REAL variance (not logvar). A non-positive var is
\ a degenerate covariance: rejected fail-closed (E-MK-VAR), never silently clamped.

\ fail-closed variance guard: pass a valid variance through, else throw E-MK-VAR
: VAR-OK ( r -- r ) {: var:r :}
   var f0<  var f0=  or if E-MK-VAR throw then  var ;

: MAHALANOBIS ( r r r -- r ) {: y:r mu:r var:r :}
   var VAR-OK  {: v:r :}  y mu RES2  v f/ ;

\ dMahalanobis/dmu = 2*(mu - y)/var  (variance guarded fail-closed here too)
: MAHALANOBIS-GRAD ( r r r -- r ) {: y:r mu:r var:r :}
   var VAR-OK  {: v:r :}  mu y f-  2.0 f*  v f/ ;

\ --- Huber loss (named transition constant delta) ---
\ Quadratic near 0 (like MSE), linear in the tails (like L1) -> robust to outliers.
\ delta is the residual magnitude where the loss switches quadratic -> linear:
\   |r| <= delta : 0.5*r^2                 ; grad = r
\   |r| >  delta : delta*(|r| - 0.5*delta) ; grad = delta*sign(r)

: HUBER-DELTA ( -- r )  1.0 ;   \ default transition point (residual units)

: HUBER ( r r r -- r ) {: pred:r tgt:r delta:r :}
   pred tgt f-  fabs  {: a:r :}
   a delta f> if
      delta  a  delta 0.5 f* f-  f*
   else
      a a f*  0.5 f*
   then ;

: HUBER-GRAD ( r r r -- r ) {: pred:r tgt:r delta:r :}
   pred tgt f-  {: d:r :}
   d fabs delta f> if
      d f0< if delta fnegate else delta then
   else
      d
   then ;

end-package
