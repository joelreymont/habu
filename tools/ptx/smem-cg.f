\ smem-cg.f - emit a checked shared-memory tile kernel through codegen.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/tile.f
require lib/ptx/tile-smem.f

256 %BLOCK

KERNEL: SMEM-CHECKED ( span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: s :} \ typed-local-lint: allow-bare-local - generic PTX param types contain commas.
   s COOP-CTX {: g :} \ typed-local-lint: allow-bare-local - fresh coopctx mask is inferred.
   s g STAGE {: sh :} \ typed-local-lint: allow-bare-local - shared span type is inferred.
   sh g SLOAD
   sh g SSTORE ;

: SMEM-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L ;

: EMIT-SMEM ( -- )
   CG-RESET  CG-HEADER
   s" .visible .entry SMEM_CHECKED(.param .u64 p_x)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L
   s" .reg .f32 %f<8>;" PTX-L
   s" .reg .b32 %r<16>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L
   s" .shared .align 4 .b8 SMEM[1024];" PTX-L
   SMEM-PARAMS
   1 SPAN-REG SMEM-CHECKED
   s" ret;" PTX-L
   s" }" PTX-L ;

EMIT-SMEM
