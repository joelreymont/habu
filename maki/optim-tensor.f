\ maki/optim-tensor.f - tensor-level optimizer updates (lifts maki/optim.f).
\
\ Tensor optimizers apply the scalar/per-weight rules across contiguous float
\ buffers. This host path is the CPU golden for the device lowering.

require maki/array.f
require maki/optim.f

package MAKI

: TT-ADAM-AT! ( r r r r r r ptr a ptr a ptr a ptr a n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr idx:n :}
   wp idx T-GET  gp idx T-GET  mp idx T-GET  vp idx T-GET
   lr b1 b2 eps c1 c2 ADAM
   vp idx T-SET
   mp idx T-SET
   wp idx T-SET ;

public

: TT-ADAM! ( r r r r r r ptr a ptr a ptr a ptr a n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr len:n :}
   len 0 ?do
      lr b1 b2 eps c1 c2 wp gp mp vp i TT-ADAM-AT!
   loop ;

end-package
