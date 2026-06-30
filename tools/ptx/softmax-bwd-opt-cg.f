\ softmax-bwd-opt-cg.f - emit optimized SOFTMAX-ROWS backward from ADIR.
\
\ This emits the closed-form backward over saved softmax output y and incoming
\ cotangent dy: dx = y * (dy - BLOCK-SUM(dy * y)). The AD op list is still
\ checked before lowering, so an unsupported forward sequence fails closed.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/collective.f
require lib/ptx/ir.f
require lib/ptx/ad-dag.f
require lib/ptx/ad-ir.f

create SBO-OPS 7 cells allot

: SBO-INIT ( -- )
   OP-DUP  SBO-OPS 0 cells + !  OP-BMAX SBO-OPS 1 cells + !  OP-BSUB SBO-OPS 2 cells + !
   OP-EXP  SBO-OPS 3 cells + !  OP-DUP  SBO-OPS 4 cells + !  OP-BSUM SBO-OPS 5 cells + !
   OP-BDIV SBO-OPS 6 cells + ! ;

: CG-BWO-ENTRY ( -- )
   s" .visible .entry SOFTMAX_BWD_OPT(.param .u64 p_y, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

256 %BLOCK

: EMIT-SOFTMAX-BWD-OPT ( -- )
   SBO-INIT
   CG-BW-RESET  CG-HEADER CG-BWO-ENTRY CG-SM-OPEN CG-BW-PARAMS
   EMIT-ROW              {: r:n :}
   1 r EMIT-ROW-SPAN     {: ysp:n :}
   ysp EMIT-ROW-CTX      {: c:n :}
   ysp c EMIT-ROW-LOAD   {: y:n :}
   2 r EMIT-ROW-SPAN     {: dysp:n :}
   dysp c EMIT-ROW-LOAD  {: dy:n :}
   SBO-OPS 7 y dy ADIR-EMIT-SOFTMAX-BWD-FROM {: dx:n :}
   3 r EMIT-ROW-SPAN     {: osp:n :}
   dx osp c EMIT-ROW-STORE
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SOFTMAX-BWD-OPT
