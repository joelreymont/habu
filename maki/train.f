\ maki/train.f - a runnable training loop (the forward -> loss -> backward ->
\ optimizer cycle), demonstrated on a 1-weight linear model y = w*x with MSE/SGD.
\
\ This is the orchestration that ties maki/autograd (VJP), maki/loss (MSE), and
\ maki/optim (SGD) into a training step that PROVABLY reduces the loss. The
\ tensor/batched version maps each op onto a Habu-PTX kernel; here it runs on
\ Habu floats so convergence is testable now. maki -> habu only.
\ Load after maki/autograd.f, maki/loss.f, maki/optim.f.
\
\ Wrapped in `package MAKI`: the training API exports as `MAKI:TRAIN-N` etc. The body
\ consumes maki words still defined globally (MUL-F / MSE / MSE-GRAD / SGD / T-GET / T-SET)
\ via the package's global-fallback lookup; once those modules join `package MAKI` the
\ same bare references resolve inside the package (docs/forth.md "Packages").

package MAKI
public

\ current loss of the linear model: L = MSE(w*x, t)
: LOSS-AT ( r r r -- r ) {: w x t :}
   w x MUL-F  t MSE ;

\ one training step on y = w*x:
\   pred = w*x ; dpred = dL/dpred ; dw = dpred*x ; w' = w - lr*dw
: TRAIN-STEP ( r r r r -- r ) {: w x t lr :}
   w x MUL-F            {: pred :}
   pred t MSE-GRAD      {: dpred :}
   dpred x f*           {: dw :}     \ chain rule: d(w*x)/dw = x
   w dw lr SGD ;

\ run n steps, threading the weight; returns the trained weight
: TRAIN-N ( r r r r n -- r ) {: x t lr n :}
   n 0 ?do
      x t lr TRAIN-STEP
   loop ;

\ --- tensor-scale training (over a whole weight tensor; needs maki/array.f) ---
\ model y[i] = w[i]*x[i] ; MSE over the tensor ; element gradient 2*(w*x - t)*x

: T-LOSS ( ptr a ptr a ptr a n -- r ) {: wb xb tb len :}
   0.0  len 0 ?do
      wb i T-GET  xb i T-GET f*  tb i T-GET f-  dup f*  f+
   loop ;

\ in-place tensor training step: w[i] -= lr * 2*(w[i]*x[i] - t[i])*x[i]
: T-TRAIN-STEP! ( r ptr a ptr a ptr a n -- ) {: lr wb xb tb len :}
   len 0 ?do
      wb i T-GET                                  \ w[i]
      wb i T-GET  xb i T-GET f*  tb i T-GET f-     \ (w*x - t)
      2.0 f*  xb i T-GET f*                        \ grad = 2*(w*x-t)*x
      lr f*  f-                                    \ w - lr*grad
      wb i T-SET
   loop ;

: T-TRAIN-N! ( r ptr a ptr a ptr a n n -- ) {: lr wb xb tb len epochs :}
   epochs 0 ?do
      lr wb xb tb len T-TRAIN-STEP!
   loop ;

end-package
