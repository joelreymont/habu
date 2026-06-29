\ ops-cg.f - emit checked scalar and v4 elementwise PTX op coverage.
\
\ Load after lib/ptx/cg.f, lib/ptx/cg-vec.f, lib/ptx/header.f, lib/ptx/tile.f,
\ and lib/ptx/tile-v4.f; emits to stdout.

256 %BLOCK

KERNEL: TILE-OPS ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: x y :} \ typed-local-lint: allow-bare-local
   x GRID-CTX {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD  y g LOAD  -.
   y g LOAD  /.
   y g STORE ;

KERNEL: TILE-OPS-V4 ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-1024
   {: x y :} \ typed-local-lint: allow-bare-local
   x GRID-CTX-V4 {: g :} \ typed-local-lint: allow-bare-local
   x g LOAD-V4  y g LOAD-V4  SUB-V4
   y g LOAD-V4  DIV-V4
   y g STORE-V4 ;

: EMIT-TILE-OPS ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  TILE-OPS
   CG-RET CG-CLOSE ;

: EMIT-TILE-OPS-V4 ( -- )
   CG-RESET  CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS
   1 SPAN-REG  2 SPAN-REG  TILE-OPS-V4
   CG-RET CG-CLOSE ;

EMIT-TILE-OPS
EMIT-TILE-OPS-V4
