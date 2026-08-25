\ relu-cg.f - emit a CHECKED RELU kernel to PTX via the codegen.
\
\ RELUK computes y = max(x, 0) elementwise: load x, RELU, store to y. The KERNEL: body
\ type-checks against its declared parametric effect AND lowers to PTX in emit mode (RELU
\ -> EMIT-RELU = max.f32 with 0). Used by tools/ptx/gradcheck.f to device-gradcheck a
\ NONLINEAR op's gradient (dRELU/dx = step) vs central differences. The unused uniform `a`
\ keeps the SAXPY param layout (x=%rd1 y=%rd2 a=%f1 n=%r1) so the launcher is shared. Load
\ after lib/ptx/cg.f and lib/ptx/tile.f; emits to stdout.

256 %BLOCK

KERNEL: RELUK ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  RELU
   y g STORE ;

: EMIT-RELUK ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  RELUK
   CG-RET CG-CLOSE ;

EMIT-RELUK
