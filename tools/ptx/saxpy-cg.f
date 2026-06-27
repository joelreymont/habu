\ saxpy-cg.f - emit the CHECKED SAXPY kernel to PTX via the codegen.
\
\ The KERNEL: SAXPY body both TYPE-CHECKS (against its declared parametric effect)
\ AND, run in emit mode, lowers to PTX: each tile op (lib/ptx-tile.f) calls its
\ EMIT-* helper (lib/ptx-cg.f). So this is the same checked kernel that the
\ ptx-stdlib gate certifies - now it produces its own PTX. Load after lib/ptx-cg.f
\ and lib/ptx-tile.f; emits to stdout.

256 %BLOCK
KERNEL: SAXPY ( span<space-global,f32,extent-n>  span<space-global,f32,extent-n>  uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :}
   x GRID-CTX {: g :}
   x g LOAD  a SCALE
   y g LOAD  +.
   y g STORE ;

\ scaffold the entry, run the checked body in emit mode (x=%rd1 y=%rd2 a=%f1), close
: EMIT-SAXPY ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  SAXPY
   CG-RET CG-CLOSE ;

EMIT-SAXPY
