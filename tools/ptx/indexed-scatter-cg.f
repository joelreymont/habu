\ indexed-scatter-cg.f - checked PTX indexed gather/scatter kernels.
\
\ INDEXED_FWD computes out[idx[i]] += x[idx[i]].
\ INDEXED_BWD computes dx[idx[i]] += dz[idx[i]].
\ INDEXED_STORE copies vals[i] to out[idx[i]] through the unique-index boundary.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/tile.f

package PTXINDEXCG

256 %BLOCK

: INDEXED-RESET ( -- )
   2 CG-NF !
   4 CG-NRD !
   3 CG-NR !
   1 CG-NP !
   0 CG-NL ! ;

KERNEL: INDEXED-FWD ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-d> -- )  GRID: ceil-n-256
   {: idx:a x:b out:c :}
   idx x INDEX-CTX {: g:d :}
   idx x g INDEX-LOAD
   idx out g INDEX-SCATTER-ADD ;

KERNEL: INDEXED-BWD ( span<space-global,u32,extent-i> span<space-global,f32,extent-d> span<space-global,f32,extent-d> -- )  GRID: ceil-n-256
   {: idx:a dx:b dz:c :}
   idx dz INDEX-CTX {: g:d :}
   idx dz g INDEX-LOAD
   idx dx g INDEX-SCATTER-ADD ;

KERNEL: INDEXED-STORE ( span<space-global,u32,extent-i> span<space-global,f32,extent-i> span<space-global,f32,extent-d> -- )  GRID: ceil-n-256
   {: idx:a vals:b out:c :}
   idx out UNIQUE-INDEX-CTX {: g:d :}
   vals g UNIQUE-INDEX-DENSE-LOAD
   idx out g INDEX-STORE ;

: INDEXED-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<16>;" PTX-L
   s" .reg .f32 %f<16>;" PTX-L
   s" .reg .b32 %r<32>;" PTX-L
   s" .reg .b64 %rd<48>;" PTX-L ;

: ENTRY-FWD ( -- )
   s" .visible .entry INDEXED_FWD(.param .u64 p_idx, .param .u64 p_x, .param .u64 p_out, .param .u32 p_nidx, .param .u32 p_ndata)" PTX-L
   INDEXED-OPEN
   s" ld.param.u64 %rd1, [p_idx];" PTX-L
   s" ld.param.u64 %rd2, [p_x];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_nidx];" PTX-L
   s" ld.param.u32 %r2, [p_ndata];" PTX-L ;

: ENTRY-BWD ( -- )
   s" .visible .entry INDEXED_BWD(.param .u64 p_idx, .param .u64 p_dx, .param .u64 p_dz, .param .u32 p_nidx, .param .u32 p_ndata)" PTX-L
   INDEXED-OPEN
   s" ld.param.u64 %rd1, [p_idx];" PTX-L
   s" ld.param.u64 %rd2, [p_dx];" PTX-L
   s" ld.param.u64 %rd3, [p_dz];" PTX-L
   s" ld.param.u32 %r1, [p_nidx];" PTX-L
   s" ld.param.u32 %r2, [p_ndata];" PTX-L ;

: ENTRY-STORE ( -- )
   s" .visible .entry INDEXED_STORE(.param .u64 p_idx, .param .u64 p_vals, .param .u64 p_out, .param .u32 p_nidx, .param .u32 p_ndata)" PTX-L
   INDEXED-OPEN
   s" ld.param.u64 %rd1, [p_idx];" PTX-L
   s" ld.param.u64 %rd2, [p_vals];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_nidx];" PTX-L
   s" ld.param.u32 %r2, [p_ndata];" PTX-L ;

: EMIT-FWD ( -- )
   INDEXED-RESET
   CG-HEADER
   ENTRY-FWD
   1 INDEX-SPAN-REG 2 DATA-SPAN-REG 3 DATA-SPAN-REG INDEXED-FWD
   CG-RET
   CG-CLOSE ;

: EMIT-BWD ( -- )
   INDEXED-RESET
   ENTRY-BWD
   1 INDEX-SPAN-REG 2 DATA-SPAN-REG 3 DATA-SPAN-REG INDEXED-BWD
   CG-RET
   CG-CLOSE ;

: EMIT-INDEXED-STORE ( -- )
   INDEXED-RESET
   ENTRY-STORE
   1 INDEX-SPAN-REG 2 INDEX-VALUE-SPAN-REG 3 DATA-SPAN-REG INDEXED-STORE
   CG-RET
   CG-CLOSE ;

: MAIN ( -- )
   EMIT-FWD
   EMIT-BWD
   EMIT-INDEXED-STORE ;

MAIN

end-package
