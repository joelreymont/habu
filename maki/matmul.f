\ maki/matmul.f - CPU reference matmul + its VJP (the linear-layer pair).
\
\ Y = X.W with X rows x inner (row-major), W inner x cols, Y rows x cols. This is
\ the host reference the device GEMM kernel must match; its VJP gives the linear
\ layer's gradients (dX = dY.W^T, dW = X^T.dY). Floats are cells (T-GET/T-SET from
\ maki/array.f). Indices are explicit so the same loops map onto the checked PTX
\ GEMM later. maki -> habu only.

\ dot of an X-row (inner contiguous floats) with a W-column (strided by cols)
: MM-DOT ( ptr a ptr a n n n -- r ) {: xrow:ptr wb:ptr col:n inner:n cols:n :}
   0.0  inner 0 ?do
      xrow i T-GET   wb  i cols *  col +  T-GET   f* f+
   loop ;

\ Y[r,c] = sum_p X[r,p]*W[p,c]
: MATMUL ( ptr a ptr a ptr a n n n -- ) {: xb:ptr wb:ptr yb:ptr rows:n inner:n cols:n :}
   rows 0 ?do
      cols 0 ?do                              \ r = j (outer), c = i (inner)
         xb j inner * cells +  wb  i  inner cols  MM-DOT
         yb  j cols *  i +  T-SET
      loop
   loop ;

\ dX[r,p] = sum_c dY[r,c]*W[p,c]   (dY-row and W-row both cols-contiguous)
: MM-DOT-DX ( ptr a ptr a n -- r ) {: dyrow:ptr wprow:ptr cols:n :}
   0.0  cols 0 ?do  dyrow i T-GET  wprow i T-GET  f* f+  loop ;
: MATMUL-DX ( ptr a ptr a ptr a n n n -- ) {: dyb:ptr wb:ptr dxb:ptr rows:n inner:n cols:n :}
   rows 0 ?do
      inner 0 ?do                             \ r = j, p = i
         dyb j cols * cells +  wb i cols * cells +  cols  MM-DOT-DX
         dxb  j inner *  i +  T-SET
      loop
   loop ;

\ dW[p,c] = sum_r X[r,p]*dY[r,c]   (X strided by inner, dY strided by cols)
: MM-DW-EL ( ptr a ptr a n n n n n -- r )
   {: xb:ptr dyb:ptr p:n col:n rows:n inner:n cols:n :}
   0.0  rows 0 ?do
      xb  i inner *  p +  T-GET   dyb  i cols *  col +  T-GET   f* f+
   loop ;
: MATMUL-DW ( ptr a ptr a ptr a n n n -- ) {: xb:ptr dyb:ptr dwb:ptr rows:n inner:n cols:n :}
   inner 0 ?do
      cols 0 ?do                              \ p = j, c = i
         xb dyb  j i  rows inner cols  MM-DW-EL
         dwb  j cols *  i +  T-SET
      loop
   loop ;
