\ scatter-add-grad-cg.f - checked PTX fan-in forward/backward kernels.
\
\ FANIN_FWD computes out[0] = sum_i x[0] across n threads.
\ FANIN_BWD computes dx[0] = sum_i dz[0], the accumulated VJP of that fan-in.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/tile.f

package PTXSCATTERGRADCG

256 %BLOCK

KERNEL: FANIN-FWD ( ptr<space-global,f32> ptr<space-global,f32> -- )  GRID: ceil-n-256
   {: x:a out:b :}
   x FANIN-CTX {: g:c :}
   x g FANIN-LOAD
   out g FANIN-SCATTER-ADD ;

KERNEL: FANIN-BWD ( ptr<space-global,f32> ptr<space-global,f32> -- )  GRID: ceil-n-256
   {: dx:a dz:b :}
   dz FANIN-CTX {: g:c :}
   dz g FANIN-LOAD
   dx g FANIN-SCATTER-ADD ;

: ENTRY-FWD ( -- )
   s" .visible .entry FANIN_FWD(.param .u64 p_x, .param .u64 p_out, .param .u32 p_n)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<8>;" PTX-L
   s" .reg .f32 %f<8>;" PTX-L
   s" .reg .b32 %r<16>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L ;

: ENTRY-BWD ( -- )
   s" .visible .entry FANIN_BWD(.param .u64 p_dx, .param .u64 p_dz, .param .u32 p_n)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<8>;" PTX-L
   s" .reg .f32 %f<8>;" PTX-L
   s" .reg .b32 %r<16>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L
   s" ld.param.u64 %rd1, [p_dx];" PTX-L
   s" ld.param.u64 %rd2, [p_dz];" PTX-L
   s" ld.param.u32 %r1, [p_n];" PTX-L ;

: EMIT-FWD ( -- )
   CG-RESET
   CG-HEADER
   ENTRY-FWD
   1 PTR-REG 2 PTR-REG FANIN-FWD
   CG-RET
   CG-CLOSE ;

: EMIT-BWD ( -- )
   CG-RESET
   ENTRY-BWD
   1 PTR-REG 2 PTR-REG FANIN-BWD
   CG-RET
   CG-CLOSE ;

: MAIN ( -- )
   EMIT-FWD
   EMIT-BWD ;

MAIN

end-package
