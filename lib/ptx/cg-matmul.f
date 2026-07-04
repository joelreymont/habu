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
\   - K swept in BK=32 tiles (the runtime K-LOOP), the gemm-tf32-v1 family floor. Staging is
\     a stages=2 cp.async SOFTWARE PIPELINE (double buffer): tile t+1 is cp.async.cg copied
\     global->shared into the OTHER buffer while tile t computes from the current one, so the
\     load latency overlaps the 4x4 accumulate. commit_group/wait_group order it; the only
\     bar.sync's are (a) after wait_group so all threads see tile t, and (b) after compute
\     before the buffer is reused. Each thread does 32 unrolled k * 16 FMAs per tile.
\   - REGISTER BLOCKING is the win: each value loaded from shared (4 A + 4 B per k) is
\     reused across the 4x4 = 16 FMAs. The 4 B operands are 4 CONTIGUOUS columns, loaded
\     with one ld.shared.v4.f32; the 4 A operands are column-strided (As row-major) and
\     stay 4 scalar ld.shared - so 5 shared-load instructions per k drive 16 FMAs.
\   - Shared layout: SH[32768] = 2 buffers of 16384. Buffer b at SH + b*16384; within it
\     As[64][32] at +0 (As[r][k] @ (r*32+k)*4), Bs[32][64] at +8192 (Bs[k][c] @ 8192 +
\     (k*64+c)*4). SH is .align 16 for the v4 load and the 16B cp.async chunks.
\
\ PERFORMANCE (measured, Orin NX, fp32, 15W):
\   naive (1 elem/thread)     ~77  GFLOP/s
\   THIS (4x4 register block)  ~283 GFLOP/s   (256^3 232, 512^3 273, 1024^3 283)
\   Triton matmul              ~1474 GFLOP/s
\ PATH TO PARITY (all emittable PTX, dotted habu-tiled-gemm-codegen):
\   - vectorized shared loads (ld.shared.v4.f32) - DONE for B (contiguous cols); A stays
\     scalar (column-strided) so cp.async keeps a contiguous global->shared copy;
\   - cp.async multi-stage shared - DONE at stages=2 (double buffer, MM-PIPE-KLOOP); the
\     emitter is shaped for the family `stages` param, so 3-4 buffers is more parity slots
\     + a deeper wait_group N;
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
MM-ASB MM-BSB + constant MM-BUFB         \ one double-buffer slot (As+Bs) = 16384 bytes
MM-BUFB 2 * constant MM-SMEM             \ 2 slots (stages=2 cp.async pipeline) = 32768 bytes
MM-ASB 16 / 256 / constant MM-CPN        \ cp.async 16B chunks/thread per array (2048f/4/256 = 2)

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

\ As/Bs micro-tile bases WITHIN a given buffer base register (%r16 = SH + parity*MM-BUFB):
\ As[ty*4][0] = base + ty*512 (row stride 32f=128B, 4 rows); Bs[0][tx*4] = base + MM-ASB + tx*16.
: MM-CUR-BASES ( -- )
   s" shl.b32 %r12,%r5,9;" PTX-L  s" add.u32 %r12,%r16,%r12;" PTX-L
   s" shl.b32 %r13,%r4,4;" PTX-L  s" add.u32 %r13,%r16,%r13;" PTX-L
   SB-RESET s" add.u32 %r13,%r13," SB-APPEND MM-ASB SB-U s" ;" SB-APPEND SB$ PTX-L ;

\ cp.async staging (step 2B): one 16B (4-float) chunk-pair (As+Bs) for chunk-set m,
\ copied DIRECTLY global->shared with cp.async.cg (no register round-trip, no st.shared).
\ chunk c = tid_lin + m*256; As chunk = As[c>>3][(c&7)*4], Bs chunk = Bs[c>>4][(c&15)*4].
\ `bufr`/`ktr` are the dst-buffer-base and kt register numbers (prologue: 11/14; prefetch: 18/17).
: MM-CP-CHUNK ( n n n -- ) {: m:n bufr:n ktr:n :}   \ m=chunk-set, bufr=dst-buffer reg#, ktr=kt reg#
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m 256 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ c = tid_lin + m*256
   \ --- As: row=c>>3, k=(c&7)*4 ; src A[rowBase+row][kt+k] ; dst buf + c*16 ---
   s" shr.u32 %r21,%r20,3;" PTX-L
   s" and.b32 %r22,%r20,7;" PTX-L  s" shl.b32 %r22,%r22,2;" PTX-L
   s" add.u32 %r23,%r9,%r21;" PTX-L
   SB-RESET s" mad.lo.u32 %r23,%r23,%r3,%r" SB-APPEND ktr SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd1,%rd10;" PTX-L
   s" shl.b32 %r23,%r20,4;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L
   \ --- Bs: k=c>>4, col=(c&15)*4 ; src B[kt+k][colBase+col] ; dst buf + MM-ASB + c*16 ---
   s" shr.u32 %r21,%r20,4;" PTX-L
   s" and.b32 %r22,%r20,15;" PTX-L  s" shl.b32 %r22,%r22,2;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r21;" SB-APPEND SB$ PTX-L
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L
   s" shl.b32 %r23,%r20,4;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MM-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;

\ stage one whole K-tile (all As+Bs chunks) into buffer `bufr` from column `ktr`
: MM-CP-STAGE ( n n -- ) {: bufr:n ktr:n :}         \ bufr=dst-buffer reg#, ktr=kt reg#
   MM-CPN 0 do  i bufr ktr MM-CP-CHUNK  loop ;

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

\ Double-buffered (stages=2) cp.async software pipeline. Two SH buffers (parity in %r15,
\ bases SH + parity*MM-BUFB); tile t+1 is cp.async-prefetched into the OTHER buffer while
\ tile t computes from the current one, so the global->shared latency overlaps compute.
\ wait_group 1 keeps the newest (prefetch) group in flight; the tail iteration drains with
\ wait_group 0. The emitter is structured for the family `stages` parameter (2 now); adding
\ 3-4 buffers is more parity slots + a deeper wait_group N, same loop shape.
: MM-PIPE-KLOOP ( -- )
   s" mov.u32 %r14,0;" PTX-L                                       \ kt = 0
   s" mov.u32 %r15,0;" PTX-L                                       \ buffer parity = 0
   11 14 MM-CP-STAGE                                              \ prologue: tile 0 -> buffer 0 (%r11=SH)
   s" cp.async.commit_group;" PTX-L
   s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   SB-RESET s" mul.lo.u32 %r16,%r15," SB-APPEND MM-BUFB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r16,%r11,%r16;" PTX-L                               \ %r16 = cur buffer base
   s" add.u32 %r17,%r14,32;" PTX-L                                 \ next_kt
   s" setp.lt.u32 %p2,%r17,%r3;" PTX-L                             \ has a next tile?
   s" @!%p2 bra $NOPF;" PTX-L
   s" xor.b32 %r18,%r15,1;" PTX-L
   SB-RESET s" mul.lo.u32 %r18,%r18," SB-APPEND MM-BUFB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r18,%r11,%r18;" PTX-L                               \ %r18 = other buffer base
   18 17 MM-CP-STAGE                                              \ prefetch next tile -> other buffer
   s" cp.async.commit_group;" PTX-L
   s" cp.async.wait_group 1;" PTX-L                               \ tile t done, prefetch t+1 still in flight
   s" bra $PFDONE;" PTX-L
   s" $NOPF:" PTX-L
   s" cp.async.wait_group 0;" PTX-L                               \ tail: drain the last tile
   s" $PFDONE:" PTX-L
   s" bar.sync 0;" PTX-L                                          \ all threads' cp.async for tile t visible
   MM-CUR-BASES
   MM-BK 0 do  i MM-KSTEP  loop
   s" bar.sync 0;" PTX-L                                          \ compute done before buffer reuse
   s" add.u32 %r14,%r14,32;" PTX-L                                 \ kt += bk
   s" xor.b32 %r15,%r15,1;" PTX-L                                 \ flip buffer
   s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L ;

: MM-BEGIN ( matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live> )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MM-STATE ;

: MM-K-LOOP ( mmctx<m,k,q> mmacc<f32,block-256,mask-live> -- mmctx<m,k,q> mmacc<f32,block-256,mask-live> )
   MM-PIPE-KLOOP ;

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
   SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MM-SMEM SB-U s" ];" SB-APPEND SB$ PTX-L
   MM-PARAMS
   1 MM-A-REG  2 MM-B-REG  3 MM-C-REG  MM-CHECKED
   s" ret;" PTX-L  s" }" PTX-L ;
