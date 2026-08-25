\ sum-cg.f - emit the checked SUM-ROWS + SCATTER-ROWS kernels to PTX.
\
\ Direct row sum is the regression for BLOCK-SUM's inactive-lane identity:
\ ROW-LOAD still seeds inactive lanes for max, while BLOCK-SUM must contribute
\ zero for inactive lanes itself. Emits ONE PTX module (PTX-MODULE{) whose single
\ header is followed by BOTH the SUM_ROWS and SCATTER_ROWS entries, so ptxas
\ assembles the whole stream. Load after lib/ptx/cg.f, lib/ptx/header.f,
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

: EMIT-SUM ( -- )                    \ SUM_ROWS entry+body (module header emitted by caller)
   CG-SM-RESET  CG-SUM-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SUM-ROWS
   CG-SM-RET CG-SM-CLOSE ;

: EMIT-SCATTER ( -- )                \ SCATTER_ROWS entry+body (module header emitted by caller)
   CG-SM-RESET  CG-SCATTER-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  SCATTER-ROWS
   CG-SM-RET CG-SM-CLOSE ;

PTX-MODULE{
   EMIT-SUM
   EMIT-SCATTER
}PTX-MODULE
