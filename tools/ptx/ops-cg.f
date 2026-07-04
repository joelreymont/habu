\ ops-cg.f - emit checked scalar and v4 elementwise PTX op coverage.
\
\ Emits ONE PTX module (PTX-MODULE{) with a single header and two distinctly
\ named entries (SAXPY scalar + SAXPY_V4 vector), so the stream is a legal module.
\ Load after lib/ptx/cg.f, lib/ptx/cg-vec.f, lib/ptx/header.f, lib/ptx/tile.f,
\ and lib/ptx/tile-v4.f; emits to stdout.

256 %BLOCK

KERNEL: TILE-OPS ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- )  GRID: ceil-n-256
   {: x y a :} \ typed-local-lint: allow-bare-local
   x GRID-CTX {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD  y g LOAD  -.
   y g LOAD  /.
   y g STORE
   a x g LOAD  y g LOAD  FMA.
   y g STORE
   x g LOAD
   y g SCATTER-ADD ;

KERNEL: TILE-OPS-V4 ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-1024
   {: x y :} \ typed-local-lint: allow-bare-local
   x GRID-CTX-V4 {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD-V4  y g LOAD-V4  SUB-V4
   y g LOAD-V4  DIV-V4
   y g STORE-V4 ;

: CG-ENTRY-V4 ( -- )                 \ distinct entry so both kernels share one module
   s" .visible .entry SAXPY_V4(.param .u64 p_x, .param .u64 p_y, .param .f32 p_a, .param .u32 p_n)" PTX-L ;

: EMIT-TILE-OPS ( -- )               \ SAXPY entry+body (module header emitted by caller)
   CG-RESET  CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  1 UNIFORM-REG  TILE-OPS
   CG-RET CG-CLOSE ;

: EMIT-TILE-OPS-V4 ( -- )            \ SAXPY_V4 entry+body (module header emitted by caller)
   CG-RESET  CG-ENTRY-V4 CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  TILE-OPS-V4
   CG-RET CG-CLOSE ;

PTX-MODULE{
   EMIT-TILE-OPS
   EMIT-TILE-OPS-V4
}PTX-MODULE
