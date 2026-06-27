\ acc-cg.f - emit a CHECKED accumulator kernel to PTX via the codegen.
\
\ AXPY-ACC computes y = x*y elementwise through the register-accumulator vocabulary:
\ zero the accumulator, FMA x*y into it, finalize, store. The KERNEL: body both
\ TYPE-CHECKS (acc<> distinct from tile<>, the completion gate enforced) AND, run in emit
\ mode, lowers to PTX - each ACC-* op calls its EMIT-ACC-* helper (lib/ptx/cg.f). This is
\ the first device-correct codegen rung for the new tile-DSL capabilities (the (c)
\ accumulator path). Load after lib/ptx/cg.f, lib/ptx/tile.f, and lib/ptx/tile-acc.f;
\ emits to stdout.

256 %BLOCK

KERNEL: AXPY-ACC ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: x y :}
   x GRID-CTX {: g :}
   g ACC-ZERO                       \ acc = 0
   x g LOAD  y g LOAD  ACC-FMA      \ acc = x*y + 0
   ACC-TILE                          \ finalize (completion gate)
   y g STORE ;                       \ store x*y to y

: EMIT-AXPY-ACC ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  AXPY-ACC
   CG-RET CG-CLOSE ;

EMIT-AXPY-ACC
