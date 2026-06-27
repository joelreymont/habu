\ maxselect-cg.f - emit the checked MAX-SELECT-ROWS kernel (BLOCK-MAX-SELECT proof).
\
\ MAX-SELECT-ROWS loads a row, takes its block max, then scatters that max back to
\ the arg-max lane (0 elsewhere) via BLOCK-MAX-SELECT - the BLOCK-MAX adjoint the
\ AD pass needs. The same checked body lib/ptx/collective-test.f certifies, run in
\ emit mode to produce its PTX. Load after lib/ptx/cg.f, lib/ptx/cg-collective.f,
\ lib/ptx/header.f, lib/ptx/collective.f. Emits to stdout; ptxas -arch=sm_87.

256 %BLOCK
KERNEL: MAX-SELECT-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-MAX    {: mx :}
   mx x mx BLOCK-MAX-SELECT          \ ( ds=mx, x, mx -- dx ): max scattered to arg-max lane
   out r ROW-SPAN c ROW-STORE ;

: EMIT-MAXSELECT ( -- )
   CG-SM-RESET  CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  MAX-SELECT-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-MAXSELECT
