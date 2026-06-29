\ sum1024-cg.f - emit the checked 1024-thread SUM-ROWS kernel.
\
\ Regression for collective codegen honoring %BLOCK in shared-memory sizing,
\ fold bounds, and WHERE block validation. Load after lib/ptx/cg.f,
\ lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/collective.f.

1024 %BLOCK
KERNEL: SUM1024-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-1024
   {: in:a out:b :}
   ROW            {: r:c :}
   in r ROW-SPAN  {: xs:d :}
   xs ROW-CTX     {: ctx:e :}
   xs ctx ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN ctx ROW-STORE ;

: CG-SUM1024-ENTRY ( -- )
   s" .visible .entry SUM_ROWS_1024(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: EMIT-SUM1024 ( -- )
   CG-SM-RESET  CG-HEADER CG-SUM1024-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SUM1024-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SUM1024
