\ ptx-collective-test.f - checked numerically-stable SOFTMAX-ROWS (M6).
\
\ The KERNEL: definition IS the positive proof: the body certifies against its
\ declared parametric effect. in/out share extent-r/extent-c by token, so one
\ row context `c` is valid for both spans; the mask token threads from ROW-LOAD
\ through B-/EXP./BLOCK-SUM/B/ to ROW-STORE by unification. A reject would emit a
\ diagnostic and fail the load.

T-RESET

256 %BLOCK

KERNEL: SOFTMAX-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-MAX    {: mx :}
   x mx B- EXP.   {: e :}
   e BLOCK-SUM    {: s :}
   e s B/  out r ROW-SPAN c ROW-STORE ;

\ Exercises BLOCK-MAX-SELECT (the BLOCK-MAX adjoint): load a row, take its max,
\ then scatter a cotangent back to the arg-max lane.
KERNEL: MAX-SELECT-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: in out :}
   ROW            {: r :}
   in r ROW-SPAN  {: xs :}
   xs ROW-CTX     {: c :}
   xs c ROW-LOAD  {: x :}
   x BLOCK-MAX    {: mx :}
   mx x mx BLOCK-MAX-SELECT       \ ( ds=mx, x, mx -- dx tile )
   out r ROW-SPAN c ROW-STORE ;

\ Clean load past this point is the positive proof: SOFTMAX-ROWS certified.

T-REPORT
