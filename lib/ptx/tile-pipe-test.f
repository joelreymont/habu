\ tile-pipe-test.f - positive + byte-identity proof for the pipelined vocabulary.
\
\ Three parts (the tile-v4a proof method):
\  1. The KERNEL: definition IS the positive proof: MM-PIPED is a test-local
\     RECOMPOSITION of the typed pipelined register-blocked GEMM - PIPE-SETUP /
\     PIPE-ACC-ZERO / PIPE-LOOP (cp.async double-buffered staging) /
\     STAGE-SLICES / A-FRAG / B-FRAG.V4 / RB-FMA under the checked K-UNROLL /
\     PIPE-STORE - and certifies end to end.
\  2. PTX emit regression: the typed emit carries the pipeline shape - cp.async.cg
\     staging, commit/wait groups, the $KLOOP labels, the strided scalar A loads,
\     the vectorized ld.shared.v4 B load, and the 16-FMA micro-tile.
\  3. Byte-identity: EMIT-PIPED (the recomposition) emits PTX byte-for-byte
\     identical to the PRODUCTION EMIT-MATMUL (itself the typed checked kernel
\     since stage 3, lib/ptx/cg-matmul.f), guarding production-body drift against
\     the vocabulary (~283 GFLOP/s device-proven kernel, Orin NX).
\
\ The negatives (parity mix, strided/plain v4 base, cross-layout mix, naive paths
\ on pipelined types, acc-dropping body) are gated by lib/ptx/tile-pipe-neg-test.f.
\ Load after lib/ptx/cg-matmul.f + lib/ptx/tile-pipe.f.

require lib/ptx/test-prelude.f
require lib/ptx/cg-matmul.f
require lib/ptx/tile-pipe.f

T-RESET

256 %BLOCK

\ ---- part 1: the typed pipelined kernel certifies (positive proof) --------------
\ test-local recomposition (TPT- prefix: cg-matmul.f owns the production RB- words)
: TPT-KSTEP ( mmracc<f32,block-256,geom-mt4x4,w> n mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p> -- mmracc<f32,block-256,geom-mt4x4,w> )
   {: acc k:n as bs :} \ typed-local-lint: allow-bare-local - parametric tile-pipe types contain commas.
   acc  as k A-FRAG  bs k B-FRAG.V4  RB-FMA ;

: TPT-TILE ( mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> mmracc<f32,block-256,geom-mt4x4,w> -- mmracc<f32,block-256,geom-mt4x4,w> )
   {: st acc :} \ typed-local-lint: allow-bare-local - parametric tile-pipe types contain commas.
   st STAGE-SLICES {: as bs :} \ typed-local-lint: allow-bare-local - slice types are inferred.
   MM-BK as bs acc [: TPT-KSTEP ;] K-UNROLL ;

KERNEL: MM-PIPED ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- )  GRID: tile-mn-64
   PIPE-SETUP
   PIPE-ACC-ZERO
   [: TPT-TILE ;] PIPE-LOOP
   PIPE-STORE ;

: EMIT-PIPED ( -- )
   MM-AUTHOR-OPEN  MM-PIPED  MM-AUTHOR-CLOSE ;

$10000 constant TP-CAP                 \ MM PTX is ~30 KB; one capture fits
create TP-GOLD TP-CAP allot            \ typed capture, survives the next capture
variable TP-GOLD-U

: TP-GOLD! ( ptr u8 n -- ) {: a:ptr u:n :}
   u TP-CAP > if s" tile-pipe-test: capture overflow" 76 die then
   a TP-GOLD u BYTE-COPY  u TP-GOLD-U ! ;

: TP-GOLD$ ( -- ptr u8 n )  TP-GOLD TP-GOLD-U @ ;

: TP-HAS ( ptr u8 n -- )   \ assert the typed capture contains a line
   TP-GOLD$ 2swap CONTAINS? TTRUE ;

: TP-MAIN ( -- )
   \ part 2: capture the TYPED emit and pin the pipelined register-blocked shape
   PTX-CAPTURE-ON EMIT-PIPED PTX-CAPTURE-OFF  PTX-CAPTURE$ TP-GOLD!
   s" typed emit is non-empty" T-LABEL  TP-GOLD-U @ 0 > TTRUE
   s" cp.async staging"     T-LABEL  s" cp.async.cg.shared.global" TP-HAS
   s" pipeline commit"      T-LABEL  s" cp.async.commit_group;" TP-HAS
   s" pipeline overlap"     T-LABEL  s" cp.async.wait_group 1;" TP-HAS
   s" runtime K-loop"       T-LABEL  s" $KLOOP:" TP-HAS
   s" strided scalar A"     T-LABEL  s" ld.shared.f32 %f26,[%r12+0];" TP-HAS
   s" vectorized B load"    T-LABEL  s" ld.shared.v4.f32 {%f30,%f31,%f32,%f33},[%r13+0];" TP-HAS
   s" micro-tile FMA 0"     T-LABEL  s" fma.rn.f32 %f10,%f26,%f30,%f10;" TP-HAS
   s" micro-tile FMA 15"    T-LABEL  s" fma.rn.f32 %f25,%f29,%f33,%f25;" TP-HAS
   s" micro-tile store"     T-LABEL  s" st.global.f32 [%rd12],%f25;" TP-HAS
   \ part 3: the recomposition is byte-identical to the production typed emit
   PTX-CAPTURE-ON EMIT-MATMUL PTX-CAPTURE-OFF  PTX-CAPTURE$ {: la:ptr lu:n :}
   s" recomposition PTX == production EMIT-MATMUL PTX" T-LABEL
   la lu TP-GOLD$ T-STR= TTRUE ;

TP-MAIN

\ Clean load past this point is the positive proof: the typed pipelined vocabulary
\ certifies, emits the cp.async register-blocked pipeline, and lowers byte-identically
\ to the kernel the device goldens already validate.

T-REPORT
