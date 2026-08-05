\ maki/optim-tensor.f - tensor-level optimizer updates (lifts maki/optim.f).
\
\ Tensor optimizers apply the scalar/per-weight rules across contiguous float
\ buffers. This host path is the CPU golden for the device lowering.

require maki/array.f
require maki/optim.f

-5156 constant E-ADAMW-WD    \ decoupled weight-decay coefficient must be >= 0

package OPTIM

: TT-ADAM-AT! ( r r r r r r ptr r ptr r ptr r ptr r n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr idx:n :}
   wp idx T-GET  gp idx T-GET  mp idx T-GET  vp idx T-GET
   lr b1 b2 eps c1 c2 ADAM
   vp idx T-SET
   mp idx T-SET
   wp idx T-SET ;

\ one AdamW element: decoupled decay wd is applied to the weight alongside the
\ raw-gradient Adam step (never through m, v) - see OPTIM:ADAMW.
: TT-ADAMW-AT! ( r r r r r r r ptr r ptr r ptr r ptr r n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wd:r wp:ptr gp:ptr mp:ptr vp:ptr idx:n :}
   wp idx T-GET  gp idx T-GET  mp idx T-GET  vp idx T-GET
   lr b1 b2 eps c1 c2 wd ADAMW
   vp idx T-SET
   mp idx T-SET
   wp idx T-SET ;

public

: TT-ADAM! ( r r r r r r ptr r ptr r ptr r ptr r n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wp:ptr gp:ptr mp:ptr vp:ptr len:n :}
   len 0 ?do
      lr b1 b2 eps c1 c2 wp gp mp vp i TT-ADAM-AT!
   loop ;

\ AdamW over a whole parameter tensor: like TT-ADAM! but each weight also takes
\ the decoupled decay -lr*wd*w. wd=0 is bit-identical to TT-ADAM!. The wd domain
\ is guarded once (a negative decay coefficient is meaningless -> throw).
: TT-ADAMW! ( r r r r r r r ptr r ptr r ptr r ptr r n -- )
   {: lr:r b1:r b2:r eps:r c1:r c2:r wd:r wp:ptr gp:ptr mp:ptr vp:ptr len:n :}
   wd 0.0 f< if E-ADAMW-WD throw then
   len 0 ?do
      lr b1 b2 eps c1 c2 wd wp gp mp vp i TT-ADAMW-AT!
   loop ;

;package
