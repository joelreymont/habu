\ maki/train.f - a runnable training loop (the forward -> loss -> backward ->
\ optimizer cycle), demonstrated on a 1-weight linear model y = w*x with MSE/SGD.
\
\ This is the orchestration that ties maki/autograd (VJP), maki/loss (MSE), and
\ maki/optim (SGD) into a training step that PROVABLY reduces the loss. The
\ tensor/batched version maps each op onto a Habu-PTX kernel; here it runs on
\ Habu floats so convergence is testable now. maki -> habu only.
\ Load after maki/autograd.f, maki/loss.f, maki/optim.f.

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
