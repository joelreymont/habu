\ maki/optim-tensor.f - tensor-level optimizer updates (lifts maki/optim.f).
\
\ Tensor optimizers apply the scalar/per-weight rules across contiguous float
\ buffers. This host path is the CPU golden for the device lowering.

require maki/array.f
require maki/optim.f

package MAKI

: TT-ADAM-AT! ( r r r r r r ptr a ptr a ptr a ptr a n -- ) \ ( in -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr idx:n :} \ ( in -- )
   wp idx T-GET  gp idx T-GET  mp idx T-GET  vp idx T-GET \ ( -- w g m v )
   lr b1 b2 eps c1 c2 ADAM \ ( w g m v -- w' m' v' )
   vp idx T-SET \ ( w' m' v' -- w' m' )
   mp idx T-SET \ ( w' m' -- w' )
   wp idx T-SET ; \ ( w' -- )

public

: TT-ADAM! ( r r r r r r ptr a ptr a ptr a ptr a n -- ) \ ( in -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr len:n :} \ ( in -- )
   len 0 ?do \ ( -- )
      lr b1 b2 eps c1 c2 wp gp mp vp i TT-ADAM-AT! \ ( -- )
   loop ; \ ( -- )

end-package
