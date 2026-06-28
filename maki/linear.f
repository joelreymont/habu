\ maki/linear.f - the nn.Linear layer: Y = X.W + b, with its full VJP.
\
\ X rows x inner, W inner x cols, b cols (broadcast over rows), Y rows x cols.
\ Forward is MATMUL then a row-broadcast bias add; backward reuses MATMUL-DX/-DW
\ for dX/dW and sums dY down the rows for db. The host reference for an MLP layer
\ the device path must match. Needs maki/array.f + maki/matmul.f. maki -> habu only.

require maki/array.f
require maki/matmul.f

package MAKI

\ add bias b[c] to every row of Y (in place)
: ADD-BIAS! ( ptr a ptr a n n -- ) {: yb:ptr bb:ptr rows:n cols:n :}
   rows 0 ?do
      cols 0 ?do                          \ r = j, c = i
         yb  j cols * i +  T-GET   bb i T-GET  f+   yb  j cols * i +  T-SET
      loop
   loop ;

\ Y = X.W + b
public

: LINEAR ( ptr a ptr a ptr a ptr a n n n -- )
   {: xb:ptr wb:ptr bb:ptr yb:ptr rows:n inner:n cols:n :}
   xb wb yb  rows inner cols  MATMUL
   yb bb  rows cols  ADD-BIAS! ;

private

\ db[c] = sum_r dY[r,c]  (the bias gradient is the column sum of the cotangent)
: LINEAR-DB ( ptr a ptr a n n -- ) {: dyb:ptr dbb:ptr rows:n cols:n :}
   cols 0 ?do                             \ c = i (outer)
      0.0
      rows 0 ?do  dyb  i cols *  j +  T-GET  f+  loop   \ r = i, c = j
      dbb i T-SET
   loop ;

\ dX = dY.W^T, dW = X^T.dY, db = colsum(dY)
public

: LINEAR-BWD ( ptr a ptr a ptr a ptr a ptr a ptr a n n n -- )
   {: dyb:ptr xb:ptr wb:ptr dxb:ptr dwb:ptr dbb:ptr rows:n inner:n cols:n :}
   dyb wb dxb  rows inner cols  MATMUL-DX
   xb dyb dwb  rows inner cols  MATMUL-DW
   dyb dbb  rows cols  LINEAR-DB ;

end-package
