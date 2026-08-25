\ swiglu-cg.f - emit the checked SWIGLU_ROWS device kernel: y = silu(gate) * up,
\ the LLaMA-family gated-MLP activation, elementwise over a row (dot habu-infer-swiglu-op).
\
\ The body is a CERTIFIED KERNEL: (the tile DSL, lib/ptx/collective-test.f precedent):
\ per row, load the gate and up tiles, apply silu to the gate tile, multiply by the up
\ tile, and masked-store the row. silu(gate) reuses the register-level PTX-ACT:EMIT-SILU
\ (lib/ptx/cg-activation.f: x*sigmoid(x), the same f32 formula OP-SILU lowers) wrapped as
\ a phantom-preserving pointwise tile op (SILU., the RELU / ROPE-ROT precedent); the
\ multiply is the base tile *. . A PTX MODULE is one header then the `.visible .entry`, so
\ CG-HEADER is emitted once. Emits to stdout; ptxas assembles for the probed device arch
\ (sm_121a GB10). gate=%rd1 up=%rd2 out=%rd3 k=%r1 (the 3-pointer layout = CG-BW-RESET).
\ Load after lib/ptx/cg.f, header.f, cg-collective.f, cg-activation.f, tile.f, collective.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/cg-activation.f       \ PTX-ACT:EMIT-SILU register emitter
require lib/ptx/tile.f
require lib/ptx/collective.f

package PTX-SWIGLU-CG

private

256 %BLOCK

\ silu as a phantom-preserving pointwise tile op: PTX-ACT:EMIT-SILU wrapped by PTXREP:REP1, exactly
\ as tile.f's RELU wraps EMIT-RELU. Local to this producer (the ROPE-ROT precedent keeps a
\ domain-specific tile op next to its one use rather than in the base tile vocabulary).
: SILU. ( tile<f32,b,m> -- tile<f32,b,m> )  [: PTX-ACT:EMIT-SILU ;] PTXREP:REP1 ;

KERNEL: SWIGLU-ROWS ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: gate up out :}
   ROW             {: r :}
   gate r ROW-SPAN {: gs :}
   up   r ROW-SPAN {: us :}
   gs ROW-CTX      {: c :}
   gs c ROW-LOAD   {: gt :}
   us c ROW-LOAD   {: ut :}
   gt SILU. ut *.  out r ROW-SPAN c ROW-STORE ;

\ gate=%rd1 up=%rd2 out=%rd3 k=%r1 -> register seeds start past %rd3 / %r1 (CG-BW-RESET).
: CG-SWIGLU-ENTRY ( -- )
   s" .visible .entry SWIGLU_ROWS(.param .u64 p_gate, .param .u64 p_up, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-SWIGLU-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_gate];" PTX-L
   s" ld.param.u64 %rd2, [p_up];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: EMIT-SWIGLU ( -- )
   CG-BW-RESET  CG-HEADER CG-SWIGLU-ENTRY CG-SM-OPEN CG-SWIGLU-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  SWIGLU-ROWS
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SWIGLU

;package
