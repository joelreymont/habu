\ sum-device-cg.f - emit ONLY the SUM_ROWS kernel to valid single-header PTX
\ for the Orin device golden.
\
\ tools/ptx/sum-cg.f (canonical) emits SUM_ROWS + SCATTER_ROWS. On the
\ maki-type-families branch that file predates master's PTX-MODULE{ } single-
\ header wrapper, so it emits two `.version` blocks in one stream and ptxas
\ rejects it. The SUM_ROWS body here is the same checked kernel as sum-cg.f's -
\ direct row sum, exercising BLOCK-SUM's reducer-local inactive-lane zero - but
\ emitted alone under one module header so ptxas -arch=sm_87 assembles it. Load
\ after lib/ptx/cg.f, lib/ptx/header.f, lib/ptx/cg-collective.f,
\ lib/ptx/collective.f. Emits to stdout.

256 %BLOCK
KERNEL: SUM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in:a out:b :}
   ROW            {: r:c :}
   in r ROW-SPAN  {: xs:d :}
   xs ROW-CTX     {: ctx:e :}
   xs ctx ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN ctx ROW-STORE ;

: CG-SUM-ENTRY ( -- )
   s" .visible .entry SUM_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: EMIT-SUM ( -- )
   CG-SM-RESET  CG-HEADER CG-SUM-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SUM-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SUM
