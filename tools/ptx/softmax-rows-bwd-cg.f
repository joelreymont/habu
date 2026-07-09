\ softmax-rows-bwd-cg.f - the CHECKED closed-form softmax backward, self-emitting.
\
\ Capstone of ad-reverse (habu-ad-softmax-rows): SOFTMAX-ROWS-BWD implements
\ dx = y * (dy - Sum(dy * y)), the closed form the reverse pass + simplifier
\ derive from the softmax forward (lib/ptx/ad-ir.f; lib/ptx/ir-test.f asserts
\ the derived text "y dy y dy *. BLOCK-SUM PTX:B- *."). The kernel is CHECKED:
\ y, dy, dx share extent-r/extent-c BY TOKEN, so len(dx) = len(y) is proven
\ statically - the extent-mismatch variant is REJECTED by the checker
\ (lib/ptx/autograd-neg-test.f). Running the certified body in emit mode
\ produces SOFTMAX_BWD_ROWS(p_y, p_dy, p_out, p_k), device-gradchecked against
\ the CPU golden by the zed gradcheck suite. Emits to stdout.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/tile.f
require lib/ptx/collective.f

256 %BLOCK

\ KERNEL: locals hold tile/span/rowctx types the cell annotations cannot
\ express; the signature carries the detail (docs/forth.md bare-local exception).
KERNEL: SOFTMAX-ROWS-BWD ( matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c>  matrix<space-global,f32,extent-r,extent-c> -- )  GRID: extent-r  WHERE extent-c <= block-256
   {: y dy dx :}       \ typed-local-lint: allow-bare-local matrices
   ROW            {: r :}    \ typed-local-lint: allow-bare-local rowidx
   y  r ROW-SPAN  {: ys :}   \ typed-local-lint: allow-bare-local span
   dy r ROW-SPAN  {: dys :}  \ typed-local-lint: allow-bare-local span
   ys ROW-CTX     {: c :}    \ typed-local-lint: allow-bare-local rowctx
   ys  c ROW-LOAD {: yt :}   \ typed-local-lint: allow-bare-local tile
   dys c ROW-LOAD {: dyt :}  \ typed-local-lint: allow-bare-local tile
   dyt yt *. BLOCK-SUM {: s :}  \ typed-local-lint: allow-bare-local uniform
   dyt s PTX:B-  yt *.
   dx r ROW-SPAN c ROW-STORE ;

: CG-SMR-ENTRY ( -- )
   s" .visible .entry SOFTMAX_BWD_ROWS(.param .u64 p_y, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;

: CG-SMR-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_y];" PTX-L
   s" ld.param.u64 %rd2, [p_dy];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

: EMIT-SOFTMAX-BWD-ROWS ( -- )
   CG-BW-RESET  CG-HEADER CG-SMR-ENTRY CG-SM-OPEN CG-SMR-PARAMS
   1 MATRIX-REG  2 MATRIX-REG  3 MATRIX-REG  SOFTMAX-ROWS-BWD
   CG-SM-RET CG-SM-CLOSE ;

EMIT-SOFTMAX-BWD-ROWS
