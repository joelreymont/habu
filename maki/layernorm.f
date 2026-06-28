\ maki/layernorm.f - LayerNorm over a feature vector (no affine), gradcheckable
\ with only + - * / sqrt (no transcendentals). y_i = (x_i - mu)/sqrt(var+eps).
\
\ A transformer-block normalization (habu-autograd-transformer-block). The backward
\ is the standard simplified form dx_i = (1/std)(dy_i - mean(dy) - xhat_i*mean(dy*xhat)).
\ Stats are recomputed rather than juggled on the stack (a CPU reference; correctness
\ over speed). Needs maki/array.f. maki -> habu only.

: LN-EPS ( -- r )  0.00001 ;

: LN-MEAN ( ptr a n -- r ) {: xb:ptr n:n :}  xb n T-SUM  n s>f f/ ;

\ variance = mean( (x-mu)^2 ) given mu
: LN-VAR ( ptr a n r -- r ) {: xb:ptr n:n mu:r :}
   0.0  n 0 ?do  xb i T-GET  mu f-  dup f*  f+  loop  n s>f f/ ;

\ standard deviation sqrt(var+eps)
: LN-STD ( ptr a n r -- r ) {: xb:ptr n:n mu:r :}
   xb n mu LN-VAR  LN-EPS f+  fsqrt ;

\ write normalized xhat_i = (x_i-mu)/std into yb
: LN-NORM! ( ptr a ptr a n r r -- ) {: xb:ptr yb:ptr n:n mu:r std:r :}
   n 0 ?do  xb i T-GET  mu f-  std f/  yb i T-SET  loop ;

\ forward (recomputes mu twice / std once; clean over fast)
: LN-FWD ( ptr a ptr a n -- ) {: xb:ptr yb:ptr n:n :}
   xb yb n  xb n LN-MEAN  xb n  xb n LN-MEAN  LN-STD  LN-NORM! ;

\ mean( dy_i * (x_i - mu) ) given mu
: LN-MEAN-DYC ( ptr a ptr a n r -- r ) {: dyb:ptr xb:ptr n:n mu:r :}
   0.0  n 0 ?do  dyb i T-GET  xb i T-GET mu f-  f*  f+  loop  n s>f f/ ;

\ dx_i = (dy_i - mdy - xhat_i*mdyx)/std   (mdyx = mean(dy*xhat), xhat=(x-mu)/std)
: LN-DX! ( ptr a ptr a ptr a n r r r r -- )
   {: dyb:ptr xb:ptr dxb:ptr n:n mu:r std:r mdy:r mdyx:r :}
   n 0 ?do
      dyb i T-GET  mdy f-
      xb i T-GET mu f- std f/  mdyx f*  f-
      std f/
      dxb i T-SET
   loop ;

\ backward (recomputes the stats; mdyx = mean(dy*(x-mu)) / std = mean(dy*xhat))
: LN-BWD ( ptr a ptr a ptr a n -- ) {: dyb:ptr xb:ptr dxb:ptr n:n :}
   dyb xb dxb n
   xb n LN-MEAN
   xb n  xb n LN-MEAN  LN-STD
   dyb n T-SUM  n s>f f/
   dyb xb n  xb n LN-MEAN  LN-MEAN-DYC   xb n  xb n LN-MEAN  LN-STD  f/
   LN-DX! ;
