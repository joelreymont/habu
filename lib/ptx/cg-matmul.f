\ cg-matmul.f - the CHECKED register-blocked tiled SGEMM kernel MM (production).
\
\ ============================ DESIGN NOTES (read me) ============================
\
\ CHECKED SURFACE (stage 3, dot habu-re-express-tiled-9cc4a73a): the production
\ EMIT-MATMUL body IS a certified `KERNEL:` composed from the typed pipelined
\ register-blocked vocabulary (lib/ptx/tile-pipe.f). The kernel carries the real
\ matrix relation A[M,K] * B[K,N] -> C[M,N]; the accumulator is the nominal
\ mmracc micro-tile (not a tile, not an acc - naive stores reject); the staged
\ operands ride mmstage/mmaslice/mmbslice/mmafrag/mmbfrag with the geometry and
\ buffer-parity obligations checked fail-closed (lib/ptx/tile-pipe-neg-test.f).
\ The ONLY trusted word left in this file is MM-ABI, the launch-ABI mint that
\ asserts the MM entry's three cvta'd params carry that matrix relation (the
\ MK-SPAN analogue for the fixed GEMM ABI). The old MM-STATE/MM-A/B/C-REG
\ phase-token boundary is DELETED; the typed kernel emits BYTE-IDENTICAL PTX to
\ the device-proven register-blocked kernel (~283 GFLOP/s Orin NX; anatomy,
\ tiling, and perf notes live with the emitters in lib/ptx/cg-matmul-emit.f).
\
\ MM-BEGIN / MM-K-LOOP / MM-STORE are the checked GEMM AUTHORING VOCABULARY:
\ maki's eval lane (maki/eval/emit.f, eval-matrix, live-author) grades LLM
\ candidates that compose exactly these three words between MM-AUTHOR-OPEN and
\ MM-AUTHOR-CLOSE - the graded candidate text and this file's production body
\ are the SAME program. Their contracts: skipping MM-K-LOOP still certifies
\ (the pipeline is phase-neutral on ctx+acc) but emits no fma/cp.async;
\ dropping MM-STORE leaves mmctx+mmracc on the stack and REJECTS.
\ Load after lib/ptx/cg-matmul-emit.f and lib/ptx/tile-pipe.f; emits to stdout.
\ ==============================================================================

require lib/ptx/cg-matmul-emit.f
require lib/ptx/tile-pipe.f

\ launch-ABI mint: MM's entry params (cvta'd to %rd1/%rd2/%rd3, dims %r1..%r3 by
\ MM-PARAMS) are asserted as the related A[M,K], B[K,N], C[M,N] operands. The
\ caller's launch contract guarantees it, exactly as MK-SPAN's caller guarantees
\ the extent; token values are the param register numbers (phantom).
TRUSTED: MM-ABI ( -- matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> )
   1 2 3 ;

\ one k step of the register-blocked compute: strided A fragment, vectorized
\ 16B-proven B fragment, 16-FMA micro-tile accumulate - same-stage operands only
: RB-KSTEP ( mmracc<f32,block-256,geom-mt4x4,w> n mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p> -- mmracc<f32,block-256,geom-mt4x4,w> )
   {: acc k:n as bs :} \ typed-local-lint: allow-bare-local - parametric tile-pipe types contain commas.
   acc  as k A-FRAG  bs k B-FRAG.V4  RB-FMA ;

\ one READY staged K-tile: derive the layout slices, unroll the BK k-steps
: RB-TILE ( mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> mmracc<f32,block-256,geom-mt4x4,w> -- mmracc<f32,block-256,geom-mt4x4,w> )
   {: st acc :} \ typed-local-lint: allow-bare-local - parametric tile-pipe types contain commas.
   st STAGE-SLICES {: as bs :} \ typed-local-lint: allow-bare-local - slice types are inferred.
   MM-BK as bs acc [: RB-KSTEP ;] K-UNROLL ;

\ ---- the checked GEMM authoring vocabulary (see design note) ----------------
: MM-BEGIN ( matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> )
   PIPE-SETUP
   PIPE-ACC-ZERO ;

: MM-K-LOOP ( mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> -- mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> )
   [: RB-TILE ;] PIPE-LOOP ;

: MM-STORE ( mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> -- )
   PIPE-STORE ;

KERNEL: MM-CHECKED ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- )  GRID: tile-mn-64
   MM-BEGIN
   MM-K-LOOP
   MM-STORE ;

\ authoring scaffold: header/entry/params up to the three ABI matrices, so a
\ checked kernel body (MM-CHECKED, or a graded candidate K - maki/eval/emit.f)
\ can sit between MM-AUTHOR-OPEN and MM-AUTHOR-CLOSE.
: MM-AUTHOR-OPEN ( -- matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> )
   PTX-HEADER  PTX-NL
   s" .visible .entry MM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<48>;" PTX-L  s" .reg .b32 %r<64>;" PTX-L  s" .reg .b64 %rd<48>;" PTX-L
   SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MM-SMEM FMT:SB-U s" ];" SB-APPEND SB$ PTX-L
   MM-PARAMS
   MM-ABI ;

: MM-AUTHOR-CLOSE ( -- )
   s" ret;" PTX-L  s" }" PTX-L ;

: EMIT-MATMUL ( -- )
   MM-AUTHOR-OPEN  MM-CHECKED  MM-AUTHOR-CLOSE ;
