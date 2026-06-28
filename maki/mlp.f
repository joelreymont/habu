\ maki/mlp.f - a 2-layer MLP (the small real model), composed from the checked
\ tensor-autograd pieces. This is the CPU host reference the GPU end-to-end must
\ match (habu-small-model-end): forward Z1=X.W1+b1, H=relu(Z1), Y=H.W2+b2; backward
\ threads the cotangent back through both LINEAR layers and the RELU gate; one SGD
\ step updates all four parameter tensors. Needs maki/{array,matmul,linear,
\ autograd,autograd-tensor}.f. maki -> habu only.

\ forward: Z1 = X.W1+b1 ; H = relu(Z1) ; Y = H.W2+b2   (Z1,H saved for backward)
: MLP-FWD ( ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a n n n n -- )
   {: xb:ptr w1:ptr b1:ptr w2:ptr b2:ptr z1:ptr hb:ptr yb:ptr
      rows:n in:n hid:n out:n :}
   xb w1 b1 z1  rows in hid  LINEAR
   z1 hb  rows hid *  TT-RELU-F
   hb w2 b2 yb  rows hid out  LINEAR ;

\ backward from dY: dH,dW2,dB2 (layer 2) -> dZ1 (relu gate on Z1) -> dX,dW1,dB1
: MLP-BWD ( ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a n n n n -- )
   {: dyb:ptr xb:ptr w1:ptr z1:ptr hb:ptr w2:ptr
      dhb:ptr dz1:ptr dxb:ptr dw1:ptr db1:ptr dw2:ptr db2:ptr
      rows:n in:n hid:n out:n :}
   dyb hb w2  dhb dw2 db2  rows hid out  LINEAR-BWD
   dhb z1 dz1  rows hid *  TT-RELU-BWD
   dz1 xb w1  dxb dw1 db1  rows in hid  LINEAR-BWD ;

\ one SGD step over all four parameter tensors: p -= lr * dp
: MLP-SGD ( r ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a n n n -- )
   {: lr:r w1:ptr b1:ptr w2:ptr b2:ptr dw1:ptr db1:ptr dw2:ptr db2:ptr
      in:n hid:n out:n :}
   lr w1 dw1  in hid *   T-SGD!
   lr b1 db1  hid        T-SGD!
   lr w2 dw2  hid out *  T-SGD!
   lr b2 db2  out        T-SGD! ;
