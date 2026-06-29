\ exp-cg.f - emit a CHECKED EXP kernel to PTX via the codegen.
\
\ EXPK computes y = exp(x) elementwise (EXP. -> ex2.approx(x*log2e)). The KERNEL: body
\ type-checks AND lowers to PTX. Used by tools/ptx/gradcheck.f to device-gradcheck a
\ TRANSCENDENTAL op whose gradient is NON-CONSTANT and equals the forward value
\ (d exp/dx = exp(x)). The unused uniform `a` keeps the shared SAXPY param layout. Load
\ after lib/ptx/cg.f, lib/ptx/header.f, lib/ptx/cg-collective.f, lib/ptx/tile.f,
\ lib/ptx/collective.f; emits to stdout.

256 %BLOCK

KERNEL: EXPK ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  EXP.
   y g STORE ;

: EMIT-EXPK ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  EXPK
   CG-RET CG-CLOSE ;

EMIT-EXPK
