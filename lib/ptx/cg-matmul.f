\ cg-matmul.f - PTX codegen: a REGISTER-BLOCKED tiled SGEMM (the compute path).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
\
\ ============================ DESIGN NOTES (read me) ============================
\
\ WHAT: C[M,N] = A[M,K] * B[K,N], row-major fp32. The first GEMM in the tiling chain
\ toward flash-attention. EMIT-MATMUL emits a self-contained kernel `MM`.
\
\ "WHY CAN'T WE DO THE SAME AS TRITON?" - we can; this file is the proof. A NAIVE
\ tiled SGEMM (1 output element/thread, shared staging) ran ~77 GFLOP/s, ~19x slower
\ than Triton (~1474). Adding REGISTER BLOCKING - the single biggest lever - took it
\ to ~283 GFLOP/s (3.6x), closing the gap to ~5x. Every remaining Triton technique is
\ just more PTX we can emit (see "path to parity"). There is no fundamental barrier.
\
\ TILING (register-blocked SGEMM):
\   - One 64x64 OUTPUT tile of C per block; block = 16x16 = 256 threads; each thread
\     owns a 4x4 MICRO-TILE of C (16 accumulators in registers).
\       rowBase = ctaid.y*64,  colBase = ctaid.x*64
\       this thread writes C[rowBase + ty*4 + i][colBase + tx*4 + j], i,j in 0..3.
\   - K swept in BK=32 tiles (the runtime K-LOOP), the gemm-tf32-v1 family floor. Per
\     K-tile the 256 threads COOPERATIVELY stage As[64][32] and Bs[32][64] into shared
\     (8 elements each), bar.sync, then each thread does its 4x4 outer-product accumulate
\     over the 32 shared columns (32 unrolled k * 16 FMAs), bar.sync.
\   - REGISTER BLOCKING is the win: each value loaded from shared (4 A + 4 B per k) is
\     reused across the 4x4 = 16 FMAs. The 4 B operands are 4 CONTIGUOUS columns, loaded
\     with one ld.shared.v4.f32; the 4 A operands are column-strided (As row-major) and
\     stay 4 scalar ld.shared - so 5 shared-load instructions per k drive 16 FMAs.
\   - Shared layout: SH[16384]: As[64][32] at byte 0 (As[r][k] @ (r*32+k)*4);
\     Bs[32][64] at byte 8192 (Bs[k][c] @ 8192 + (k*64+c)*4). SH is .align 16 for v4.
\
\ PERFORMANCE (measured, Orin NX, fp32, 15W):
\   naive (1 elem/thread)     ~77  GFLOP/s
\   THIS (4x4 register block)  ~283 GFLOP/s   (256^3 232, 512^3 273, 1024^3 283)
\   Triton matmul              ~1474 GFLOP/s
\ PATH TO PARITY (all emittable PTX, dotted habu-tiled-gemm-codegen):
\   - vectorized shared loads (ld.shared.v4.f32) - DONE for B (contiguous cols); A stays
\     scalar (column-strided) so cp.async keeps a contiguous global->shared copy;
\   - cp.async multi-stage shared (software pipeline: prefetch next K-tile while computing)
\     - CAD-PLAN 8.1 step 2B, family `stages` parameter;
\   - bigger micro-tiles (8x8) + bigger block tiles (128x128);
\   - the last stretch to Triton may need its MMA/dot path (tensor-core-adjacent).
\
\ CHECKED SURFACE: MM-CHECKED is a matrix-shaped `KERNEL:` body. The PTX instruction
\ sequences remain trusted target primitives, like LOAD/STORE/FMA, but the public
\ GEMM surface now carries the real matrix relation A[M,K] * B[K,N] -> C[M,N],
\ a distinct mmctx phase token, and a distinct mmacc token that must pass through
\ the K-loop before MM-STORE. This removes the old fake 1-D GEMM proof and gives
\ zed a real checked entry that emits the existing device-proven register-blocked
\ kernel. Load after src/arch/ptx/emit.f and lib/ptx/cg.f; emits to stdout.
\ ==============================================================================

64 constant MM-BM   64 constant MM-BN   32 constant MM-BK   4 constant MM-TM
MM-BM MM-BK * 4 * constant MM-ASB        \ bytes in the As shared tile = 8192 (As[64][32])
MM-BK MM-BN * 4 * constant MM-BSB        \ bytes in the Bs shared tile = 8192 (Bs[32][64])
MM-BM MM-BK * 256 / constant MM-NSTG     \ cooperative stage iters/thread (As=Bs=2048 floats / 256 = 8)

TRUSTED: MM-A-REG ( n -- matrix<space-global,f32,extent-m,extent-k> ) ;
TRUSTED: MM-B-REG ( n -- matrix<space-global,f32,extent-k,extent-n> ) ;
TRUSTED: MM-C-REG ( n -- matrix<space-global,f32,extent-m,extent-n> ) ;
TRUSTED: MM-STATE ( matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live> )
   drop drop drop 0 0 ;

: MM-PARAMS ( -- )
   s" ld.param.u64 %rd1,[pA];" PTX-L  s" ld.param.u64 %rd2,[pB];" PTX-L  s" ld.param.u64 %rd3,[pC];" PTX-L
   s" ld.param.u32 %r1,[pM];" PTX-L   s" ld.param.u32 %r2,[pN];" PTX-L   s" ld.param.u32 %r3,[pK];" PTX-L
   s" cvta.to.global.u64 %rd1,%rd1;" PTX-L  s" cvta.to.global.u64 %rd2,%rd2;" PTX-L  s" cvta.to.global.u64 %rd3,%rd3;" PTX-L ;

: MM-THREAD-SETUP ( -- )
   s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,%tid.y;" PTX-L
   s" mov.u32 %r6,%ctaid.x;" PTX-L  s" mov.u32 %r7,%ctaid.y;" PTX-L
   s" mad.lo.u32 %r8,%r5,16,%r4;" PTX-L
   s" mul.lo.u32 %r9,%r7,64;" PTX-L
   s" mul.lo.u32 %r10,%r6,64;" PTX-L
   s" mov.u32 %r11,SH;" PTX-L ;

: MM-ACC-ZERO-EMIT ( -- )
   16 0 do
      SB-RESET s" mov.f32 %f" SB-APPEND 10 i + SB-U s" ,0f00000000;" SB-APPEND SB$ PTX-L
   loop ;

: MM-SMEM-BASES ( -- )
   s" shl.b32 %r12,%r5,9;" PTX-L  s" add.u32 %r12,%r11,%r12;" PTX-L   \ As[ty*4][0]: row stride 32f=128B, *4 rows
   s" shl.b32 %r13,%r4,4;" PTX-L  s" add.u32 %r13,%r11,%r13;" PTX-L
   SB-RESET s" add.u32 %r13,%r13," SB-APPEND MM-ASB SB-U s" ;" SB-APPEND SB$ PTX-L ;

\ cooperative stage of the n-th (of 4) As+Bs element for this thread
: MM-STAGE ( n -- ) {: n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND n 256 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ li = tid_lin + n*256
   \ As[li/32][li%32] = A[rowBase+li/32][kt+li%32]   (bk=32: row=li>>5, k=li&31)
   s" shr.u32 %r30,%r20,5;" PTX-L  s" and.b32 %r31,%r20,31;" PTX-L
   s" add.u32 %r32,%r9,%r30;" PTX-L  s" mad.lo.u32 %r32,%r32,%r3,%r14;" PTX-L  s" add.u32 %r32,%r32,%r31;" PTX-L
   s" mul.wide.u32 %rd10,%r32,4;" PTX-L  s" add.u64 %rd10,%rd1,%rd10;" PTX-L  s" ld.global.f32 %f2,[%rd10];" PTX-L
   s" shl.b32 %r33,%r20,2;" PTX-L  s" add.u32 %r33,%r11,%r33;" PTX-L  s" st.shared.f32 [%r33],%f2;" PTX-L
   \ Bs[li/64][li%64] = B[kt+li/64][colBase+li%64]
   s" shr.u32 %r34,%r20,6;" PTX-L  s" and.b32 %r35,%r20,63;" PTX-L
   s" add.u32 %r36,%r14,%r34;" PTX-L  s" mad.lo.u32 %r36,%r36,%r2,%r10;" PTX-L  s" add.u32 %r36,%r36,%r35;" PTX-L
   s" mul.wide.u32 %rd11,%r36,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd11;" PTX-L  s" ld.global.f32 %f3,[%rd11];" PTX-L
   s" shl.b32 %r37,%r20,2;" PTX-L  s" add.u32 %r37,%r11,%r37;" PTX-L
   SB-RESET s" add.u32 %r37,%r37," SB-APPEND MM-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" st.shared.f32 [%r37],%f3;" PTX-L ;

\ one k step: load 4 A-regs (%f26..29) + 4 B-regs (%f30..33), then the 4x4 FMAs.
\ A (As[k][ty*4+j], row-major As[BM][BK]) is column-strided (row stride 32f=128B) -> 4 scalar
\ ld.shared. B (Bs[k][tx*4+i], row-major Bs[BK][BN]) is 4 CONTIGUOUS cols -> one ld.shared.v4.f32
\ (16B-aligned: SH .align 16 + MM-ASB + tx*16 + k*256). A cannot v4 without transposing As, which
\ would break the contiguous cp.async global->shared staging in the multi-stage step.
: MM-KSTEP ( k -- ) {: k :}
   MM-TM 0 do
      SB-RESET s" ld.shared.f32 %f" SB-APPEND 26 i + SB-U s" ,[%r12+" SB-APPEND  i 128 * k 4 * +  SB-U s" ];" SB-APPEND SB$ PTX-L
   loop
   SB-RESET s" ld.shared.v4.f32 {%f30,%f31,%f32,%f33},[%r13+" SB-APPEND  k 256 *  SB-U s" ];" SB-APPEND SB$ PTX-L
   MM-TM 0 do  MM-TM 0 do                     \ j=outer(tile-row), i=inner(tile-col)
      SB-RESET s" fma.rn.f32 %f" SB-APPEND  10 j 4 * + i +  SB-U
               s" ,%f" SB-APPEND 26 j + SB-U  s" ,%f" SB-APPEND 30 i + SB-U
               s" ,%f" SB-APPEND 10 j 4 * + i + SB-U  s" ;" SB-APPEND SB$ PTX-L
   loop loop ;

\ write the 4x4 micro-tile of C
: MM-WRITE ( -- )
   s" shl.b32 %r40,%r5,2;" PTX-L  s" add.u32 %r40,%r9,%r40;" PTX-L     \ cRow0 = rowBase+ty*4
   s" shl.b32 %r41,%r4,2;" PTX-L  s" add.u32 %r41,%r10,%r41;" PTX-L    \ cCol0 = colBase+tx*4
   MM-TM 0 do
      SB-RESET s" add.u32 %r42,%r40," SB-APPEND i SB-U s" ;" SB-APPEND SB$ PTX-L
      s" mad.lo.u32 %r43,%r42,%r2,%r41;" PTX-L
      MM-TM 0 do
         SB-RESET s" add.u32 %r44,%r43," SB-APPEND i SB-U s" ;" SB-APPEND SB$ PTX-L
         s" mul.wide.u32 %rd12,%r44,4;" PTX-L  s" add.u64 %rd12,%rd3,%rd12;" PTX-L
         SB-RESET s" st.global.f32 [%rd12],%f" SB-APPEND 10 j 4 * + i + SB-U s" ;" SB-APPEND SB$ PTX-L
      loop
   loop ;

: MM-BEGIN ( matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live> )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MM-SMEM-BASES
   MM-STATE ;

: MM-K-LOOP ( mmctx<m,k,q> mmacc<f32,block-256,mask-live> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live> )
   s" mov.u32 %r14,0;" PTX-L  s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   MM-NSTG 0 do  i MM-STAGE  loop
   s" bar.sync 0;" PTX-L
   MM-BK 0 do  i MM-KSTEP  loop
   s" bar.sync 0;" PTX-L  s" add.u32 %r14,%r14,32;" PTX-L  s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L ;

: MM-STORE ( mmctx<m,k,q> mmacc<f32,block-256,mask-live> -- )
   MM-WRITE
   2drop ;

KERNEL: MM-CHECKED ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- )  GRID: tile-mn-64
   MM-BEGIN
   MM-K-LOOP
   MM-STORE ;

: EMIT-MATMUL ( -- )
   PTX-HEADER-SM87  PTX-NL
   s" .visible .entry MM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<48>;" PTX-L  s" .reg .b32 %r<64>;" PTX-L  s" .reg .b64 %rd<48>;" PTX-L
   SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MM-ASB MM-BSB + SB-U s" ];" SB-APPEND SB$ PTX-L
   MM-PARAMS
   1 MM-A-REG  2 MM-B-REG  3 MM-C-REG  MM-CHECKED
   s" ret;" PTX-L  s" }" PTX-L ;
