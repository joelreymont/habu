\ sum-cg.f - emit the checked SUM-ROWS kernel to PTX via the codegen.
\
\ Direct row sum is the regression for BLOCK-SUM's inactive-lane identity:
\ ROW-LOAD still seeds inactive lanes for max, while BLOCK-SUM must contribute
\ zero for inactive lanes itself. Load after lib/ptx/cg.f, lib/ptx/header.f,
\ lib/ptx/cg-collective.f, lib/ptx/collective.f. Emits to stdout.

256 %BLOCK
KERNEL: SUM-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in:a out:b :}
   ROW            {: r:c :}
   in r ROW-SPAN  {: xs:d :}
   xs ROW-CTX     {: ctx:e :}
   xs ctx ROW-LOAD BLOCK-SUM BROADCAST
   out r ROW-SPAN ctx ROW-STORE ;

KERNEL: SCATTER-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in:a out:b :}
   ROW            {: r:c :}
   in r ROW-SPAN  {: xs:d :}
   xs ROW-CTX     {: ctx:e :}
   xs ctx ROW-LOAD
   out r ROW-SPAN ctx ROW-SCATTER-ADD ;

: CG-SUM-ENTRY ( -- )
   s" .visible .entry SUM_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-SCATTER-ENTRY ( -- )
   s" .visible .entry SCATTER_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: EMIT-SUM ( -- )
   CG-SM-RESET  CG-HEADER CG-SUM-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SUM-ROWS
   CG-SM-RET CG-SM-CLOSE ;

: EMIT-SCATTER ( -- )
   CG-SM-RESET  CG-HEADER CG-SCATTER-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SCATTER-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SUM
EMIT-SCATTER
