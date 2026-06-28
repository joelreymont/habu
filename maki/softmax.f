\ maki/softmax.f - numerically-stable softmax over a vector + its VJP (the
\ attention core). y_i = exp(x_i - max)/sum. Backward is the softmax Jacobian-
\ vector product dx_i = y_i*(dy_i - sum_j dy_j*y_j). Needs maki/array.f (T-GET/SET),
\ maki/fmath.f (FEXP), maki/autograd.f (MAX-F). maki -> habu only.

: SM-MAX ( ptr a n -- r ) {: xb:ptr n:n :}
   xb 0 T-GET  n 1 ?do  xb i T-GET  MAX-F  loop ;

\ write e_i = exp(x_i - m) into yb, return sum
: SM-EXP! ( ptr a ptr a n r -- r ) {: xb:ptr yb:ptr n:n m:r :}
   0.0  n 0 ?do  xb i T-GET  m f-  FEXP  dup  yb i T-SET  f+  loop ;

\ in-place divide yb by the scalar sum
: SM-DIV! ( ptr a n r -- ) {: yb:ptr n:n s:r :}
   n 0 ?do  yb i T-GET  s f/  yb i T-SET  loop ;

\ y = softmax(x)  (stable: subtract the max before exp)
: SM-FWD ( ptr a ptr a n -- ) {: xb:ptr yb:ptr n:n :}
   yb n  xb yb n  xb n SM-MAX  SM-EXP!  SM-DIV! ;

\ sum_j dy_j * y_j  (the row coupling term)
: SM-DOT ( ptr a ptr a n -- r ) {: dyb:ptr yb:ptr n:n :}
   0.0  n 0 ?do  dyb i T-GET  yb i T-GET  f*  f+  loop ;

\ dx_i = y_i * (dy_i - d)
: SM-DX! ( ptr a ptr a ptr a n r -- ) {: dyb:ptr yb:ptr dxb:ptr n:n d:r :}
   n 0 ?do  yb i T-GET  dyb i T-GET d f-  f*  dxb i T-SET  loop ;

\ dx = J_softmax^T dy   (needs the saved output y, not x)
: SM-BWD ( ptr a ptr a ptr a n -- ) {: dyb:ptr yb:ptr dxb:ptr n:n :}
   dyb yb dxb n  dyb yb n SM-DOT  SM-DX! ;
