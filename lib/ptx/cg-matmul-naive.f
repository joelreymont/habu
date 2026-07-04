\ cg-matmul-naive.f - the NAIVE one-element-per-thread SGEMM baseline (kernel MMN).
\
\ The pre-blocking baseline for the GEMM-vs-Triton comparison (docs/eval-triton.md, CAD-PLAN
\ 8.1). Each thread computes one C[row][col] with a global-memory K-loop (fma.rn.f32) - no
\ shared staging, no register blocking - so it is DRAM/instruction bound and GFLOP/s does NOT
\ climb with problem size. The register-blocked lib/ptx/cg-matmul.f MM is the improvement over
\ this; tools/ptx/gemm-bench.f times BOTH at the same shapes so the tile-size lift is measured,
\ not asserted. Same ABI as MM (pA,pB,pC,pM,pN,pK), 16x16 block, 2D grid ceil(N/16) x ceil(M/16),
\ bounds-masked so any M/N/K is valid. Emits a self-contained module to stdout / the PTX capture.
\ Load after src/arch/ptx/emit.f and lib/ptx/cg.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f

: MMN-ENTRY ( -- )
   s" .visible .entry MMN(.param .u64 pA, .param .u64 pB, .param .u64 pC, .param .u32 pM, .param .u32 pN, .param .u32 pK)" PTX-L ;

: MMN-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L
   s" .reg .f32 %f<8>;" PTX-L
   s" .reg .b32 %r<16>;" PTX-L
   s" .reg .b64 %rd<16>;" PTX-L ;

: MMN-PARAMS ( -- )
   s" ld.param.u64 %rd1, [pA];" PTX-L  s" ld.param.u64 %rd2, [pB];" PTX-L  s" ld.param.u64 %rd3, [pC];" PTX-L
   s" ld.param.u32 %r1, [pM];" PTX-L   s" ld.param.u32 %r2, [pN];" PTX-L   s" ld.param.u32 %r3, [pK];" PTX-L
   s" cvta.to.global.u64 %rd1, %rd1;" PTX-L  s" cvta.to.global.u64 %rd2, %rd2;" PTX-L  s" cvta.to.global.u64 %rd3, %rd3;" PTX-L ;

: MMN-COORDS ( -- )                                \ row = ctaid.y*ntid.y+tid.y ; col likewise
   s" mov.u32 %r4, %ctaid.y;" PTX-L  s" mov.u32 %r5, %ntid.y;" PTX-L  s" mov.u32 %r6, %tid.y;" PTX-L
   s" mad.lo.u32 %r7, %r4, %r5, %r6;" PTX-L
   s" mov.u32 %r8, %ctaid.x;" PTX-L  s" mov.u32 %r9, %ntid.x;" PTX-L  s" mov.u32 %r10, %tid.x;" PTX-L
   s" mad.lo.u32 %r11, %r8, %r9, %r10;" PTX-L
   s" setp.ge.u32 %p1, %r7, %r1;" PTX-L  s" @%p1 bra DONE;" PTX-L
   s" setp.ge.u32 %p2, %r11, %r2;" PTX-L  s" @%p2 bra DONE;" PTX-L ;

: MMN-KLOOP ( -- )                                 \ acc += A[row*K+kk] * B[kk*N+col]
   s" mov.f32 %f1, 0f00000000;" PTX-L
   s" mov.u32 %r12, 0;" PTX-L
   s" $KL:" PTX-L
   s" setp.ge.u32 %p3, %r12, %r3;" PTX-L  s" @%p3 bra $KE;" PTX-L
   s" mad.lo.u32 %r13, %r7, %r3, %r12;" PTX-L  s" mul.wide.u32 %rd10, %r13, 4;" PTX-L
   s" add.u64 %rd11, %rd1, %rd10;" PTX-L  s" ld.global.f32 %f2, [%rd11];" PTX-L
   s" mad.lo.u32 %r14, %r12, %r2, %r11;" PTX-L  s" mul.wide.u32 %rd12, %r14, 4;" PTX-L
   s" add.u64 %rd13, %rd2, %rd12;" PTX-L  s" ld.global.f32 %f3, [%rd13];" PTX-L
   s" fma.rn.f32 %f1, %f2, %f3, %f1;" PTX-L
   s" add.u32 %r12, %r12, 1;" PTX-L  s" bra $KL;" PTX-L
   s" $KE:" PTX-L ;

: MMN-STORE ( -- )                                 \ C[row*N+col] = acc
   s" mad.lo.u32 %r13, %r7, %r2, %r11;" PTX-L  s" mul.wide.u32 %rd10, %r13, 4;" PTX-L
   s" add.u64 %rd11, %rd3, %rd10;" PTX-L  s" st.global.f32 [%rd11], %f1;" PTX-L ;

: EMIT-MATMUL-NAIVE ( -- )
   PTX-HEADER-SM87  PTX-NL
   MMN-ENTRY  MMN-OPEN  MMN-PARAMS  MMN-COORDS  MMN-KLOOP  MMN-STORE
   s" DONE:" PTX-L  s" ret;" PTX-L  s" }" PTX-L ;
