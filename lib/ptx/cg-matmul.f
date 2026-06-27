\ cg-matmul.f - PTX codegen: a REGISTER-BLOCKED tiled SGEMM (the compute path).
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
\   - K swept in BK=16 tiles (the runtime K-LOOP). Per K-tile the 256 threads
\     COOPERATIVELY stage As[64][16] and Bs[16][64] into shared (4 elements each),
\     bar.sync, then each thread does its 4x4 outer-product accumulate over the 16
\     shared columns (16 unrolled k * 16 FMAs), bar.sync.
\   - REGISTER BLOCKING is the win: each value loaded from shared (4 A + 4 B per k) is
\     reused across the 4x4 = 16 FMAs, so arithmetic intensity is 16 FMAs / 8 shared
\     loads instead of 1 FMA / 2 loads - that is what makes it compute-bound.
\   - Shared layout: SH[8192]: As[64][16] at byte 0 (As[r][k] @ (r*16+k)*4);
\     Bs[16][64] at byte 4096 (Bs[k][c] @ 4096 + (k*64+c)*4).
\
\ PERFORMANCE (measured, Orin NX, fp32, 15W):
\   naive (1 elem/thread)     ~77  GFLOP/s
\   THIS (4x4 register block)  ~283 GFLOP/s   (256^3 232, 512^3 273, 1024^3 283)
\   Triton matmul              ~1474 GFLOP/s
\ PATH TO PARITY (all emittable PTX, dotted habu-tiled-gemm-codegen):
\   - vectorized shared/global loads (ld.shared.v4.f32) - fewer instructions;
\   - double-buffered shared (software pipeline: prefetch next K-tile while computing);
\   - bigger micro-tiles (8x8) + bigger block tiles (128x128);
\   - the last stretch to Triton may need its MMA/dot path (tensor-core-adjacent).
\
\ CHECKER BOUNDARY (honest): this is an UNCHECKED emit boundary. Two of the three missing
\ typed capabilities now exist as checked tile-DSL vocabulary: the CHECKED COUNTED LOOP
\ (lib/ptx/tile-loop.f TILE-LOOP, capability (a)) and the SHARED-MEMORY tile type
\ (lib/ptx/tile-smem.f STAGE/SLOAD/SSTORE, capability (b) - space-shared distinct from
\ space-global). Still missing: a REGISTER ACCUMULATOR type (c), and the codegen that
\ lowers a checked KERNEL: MM body to this same PTX (the STAGE/SLOAD bodies throw
\ E-PTX-NOIMPL today). So MM is still emitted as raw PTX. It is a named, tested boundary
\ (device-golden correct vs CPU A*B) per CLAUDE.md; the remaining work is dotted
\ (habu-tiled-gemm-codegen + habu-checker-capability-typed sub-dots c/re-express). When
\ those land, MM becomes a checked KERNEL: body like SAXPY/softmax. Load after
\ src/arch/ptx/emit.f and lib/ptx/cg.f; emits to stdout.
\ ==============================================================================

64 constant MM-BM   64 constant MM-BN   16 constant MM-BK   4 constant MM-TM
MM-BM MM-BK * 4 * constant MM-ASB        \ bytes in the As shared tile = 4096
MM-BK MM-BN * 4 * constant MM-BSB        \ bytes in the Bs shared tile = 4096

\ cooperative stage of the n-th (of 4) As+Bs element for this thread
: MM-STAGE ( n -- ) {: n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND n 256 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ li = tid_lin + n*256
   \ As[li/16][li%16] = A[rowBase+li/16][kt+li%16]
   s" shr.u32 %r30,%r20,4;" PTX-L  s" and.b32 %r31,%r20,15;" PTX-L
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

\ one k step: load 4 A-regs (%f26..29) + 4 B-regs (%f30..33), then the 4x4 FMAs
: MM-KSTEP ( k -- ) {: k :}
   MM-TM 0 do
      SB-RESET s" ld.shared.f32 %f" SB-APPEND 26 i + SB-U s" ,[%r12+" SB-APPEND  i 64 * k 4 * +  SB-U s" ];" SB-APPEND SB$ PTX-L
   loop
   MM-TM 0 do
      SB-RESET s" ld.shared.f32 %f" SB-APPEND 30 i + SB-U s" ,[%r13+" SB-APPEND  k 256 * i 4 * +  SB-U s" ];" SB-APPEND SB$ PTX-L  loop
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

: EMIT-MATMUL ( -- )
   PTX-HEADER-SM87  PTX-NL
   s" .visible .entry MM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<48>;" PTX-L  s" .reg .b32 %r<64>;" PTX-L  s" .reg .b64 %rd<48>;" PTX-L
   SB-RESET s" .shared .align 4 .b8 SH[" SB-APPEND MM-ASB MM-BSB + SB-U s" ];" SB-APPEND SB$ PTX-L
   s" ld.param.u64 %rd1,[pA];" PTX-L  s" ld.param.u64 %rd2,[pB];" PTX-L  s" ld.param.u64 %rd3,[pC];" PTX-L
   s" ld.param.u32 %r1,[pM];" PTX-L   s" ld.param.u32 %r2,[pN];" PTX-L   s" ld.param.u32 %r3,[pK];" PTX-L
   s" cvta.to.global.u64 %rd1,%rd1;" PTX-L  s" cvta.to.global.u64 %rd2,%rd2;" PTX-L  s" cvta.to.global.u64 %rd3,%rd3;" PTX-L
   s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,%tid.y;" PTX-L
   s" mov.u32 %r6,%ctaid.x;" PTX-L  s" mov.u32 %r7,%ctaid.y;" PTX-L
   s" mad.lo.u32 %r8,%r5,16,%r4;" PTX-L              \ tid_lin = ty*16+tx
   s" mul.lo.u32 %r9,%r7,64;" PTX-L                  \ rowBase = by*64
   s" mul.lo.u32 %r10,%r6,64;" PTX-L                 \ colBase = bx*64
   s" mov.u32 %r11,SH;" PTX-L
   16 0 do  SB-RESET s" mov.f32 %f" SB-APPEND 10 i + SB-U s" ,0f00000000;" SB-APPEND SB$ PTX-L loop   \ acc=0
   s" shl.b32 %r12,%r5,8;" PTX-L  s" add.u32 %r12,%r11,%r12;" PTX-L     \ asBase = SH + ty*256
   s" shl.b32 %r13,%r4,4;" PTX-L  s" add.u32 %r13,%r11,%r13;" PTX-L
   SB-RESET s" add.u32 %r13,%r13," SB-APPEND MM-ASB SB-U s" ;" SB-APPEND SB$ PTX-L   \ bsBase = SH+4096+tx*16
   s" mov.u32 %r14,0;" PTX-L  s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   4 0 do  i MM-STAGE  loop
   s" bar.sync 0;" PTX-L
   MM-BK 0 do  i MM-KSTEP  loop
   s" bar.sync 0;" PTX-L  s" add.u32 %r14,%r14,16;" PTX-L  s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L
   MM-WRITE
   s" ret;" PTX-L  s" }" PTX-L ;
