\ maki/rmsnorm.f - RMSNorm over a feature vector (no affine), gradcheckable with
\ only + - * / sqrt (no transcendentals). y_i = x_i / sqrt(mean(x^2) + eps).
\
\ The LLaMA-family normalization: cheaper than LayerNorm (no mean subtraction).
\ Backward (no affine): dx_j = (dy_j - x_j*S/(n*r^2)) / r, with r = sqrt(ms+eps),
\ ms = mean(x^2), S = sum_i dy_i*x_i. Stats are recomputed rather than juggled (a
\ CPU reference; correctness over speed). Needs maki/array.f. maki -> habu only.

require maki/array.f

package MAKI

: RMS-EPS ( -- r )  0.00001 ;

\ mean square = mean( x_i^2 )
: RMS-MS ( ptr a n -- r ) {: xb:ptr n:n :}
   0.0  n 0 ?do  xb i T-GET  dup f*  f+  loop  n s>f f/ ;

\ root-mean-square scale r = sqrt(mean(x^2) + eps)
: RMS-RMS ( ptr a n -- r ) {: xb:ptr n:n :}
   xb n RMS-MS  RMS-EPS f+  fsqrt ;

\ write y_i = x_i / r into yb
: RMS-NORM! ( ptr a ptr a n r -- ) {: xb:ptr yb:ptr n:n r:r :}
   n 0 ?do  xb i T-GET  r f/  yb i T-SET  loop ;

public

: RMS-FWD ( ptr a ptr a n -- ) {: xb:ptr yb:ptr n:n :}
   xb yb n  xb n RMS-RMS  RMS-NORM! ;

private

\ S = sum_i dy_i * x_i  (the row coupling term)
: RMS-DOT ( ptr a ptr a n -- r ) {: dyb:ptr xb:ptr n:n :}
   0.0  n 0 ?do  dyb i T-GET  xb i T-GET  f*  f+  loop ;

\ dx_j = (dy_j - x_j*coef)/r , coef = S/(n*r^2)
: RMS-DX! ( ptr a ptr a ptr a n r r -- ) {: dyb:ptr xb:ptr dxb:ptr n:n r:r coef:r :}
   n 0 ?do
      dyb i T-GET  xb i T-GET coef f*  f-  r f/  dxb i T-SET
   loop ;

public

: RMS-BWD ( ptr a ptr a ptr a n -- ) {: dyb:ptr xb:ptr dxb:ptr n:n :}
   xb n RMS-RMS {: r:r :}
   dyb xb n RMS-DOT  n s>f f/  r f/  r f/  {: coef:r :}
   dyb xb dxb n  r  coef  RMS-DX! ;

;package
