\ cg-mma.f - PTX codegen: a TF32 TENSOR-CORE (mma.sync) tiled GEMM (the compute-roof lever).
\
\ dot habu-tensor-core-mma. Same 64x64 block, cp.async double-buffered As/Bs staging, and
\ 256-thread (8-warp) layout as the FP32 register-blocked kernel (lib/ptx/cg-matmul.f) - the
\ ONLY swap is the compute inner: the 4x4 fma.rn.f32 micro-tile becomes a warp-level
\ `mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32` tensor-core tile. The FP32 CUDA-core
\ roof (~940 GFLOP/s) caps the fma path; TF32 tensor cores sit on a HIGHER roof (Triton
\ measured 1474), so matching/beating Triton on compute needs MMA, not just better tiling
\ (docs/kernel-principles.md roofline).
\
\ WARP TILING (8 warps = 256 threads over a 64x64 output tile):
\   warp w (= tid_lin>>5) owns warp_row=w>>1 (0..3), warp_col=w&1 (0..1):
\   rows [warp_row*16 .. +15] x cols [warp_col*32 .. +31] = 4 MMA n-tiles (8 cols each).
\   Per K-substep (MMA-K=8) a warp loads ONE 16x8 A fragment (4 tf32/lane) and REUSES it
\   across the 4 n-tiles (each with its own 8x8 B fragment, 2 tf32/lane) -> 16 f32
\   accumulators/lane (%f10..%f25), the register-reuse rung that feeds the tensor cores.
\   K is swept in BK=32 staged tiles (cg-matmul MM-CP-STAGE), 4 MMA-K substeps (0,8,16,24)
\   per tile, fragments read straight from the cp.async-staged As[64][32]/Bs[32][64] .shared.
\
\ FRAGMENT LAYOUT (device-validated by tools/ptx/mma-probe.f, gid=lane>>2 t=lane&3):
\   A(16x8) a0=A[gid][t] a1=A[gid+8][t] a2=A[gid][t+4] a3=A[gid+8][t+4]
\   B(8x8)  b0=B[t][gid] b1=B[t+4][gid]                     (cvt.rna.tf32.f32 each operand)
\   D(16x8) d0=D[gid][2t] d1=D[gid][2t+1] d2=D[gid+8][2t] d3=D[gid+8][2t+1]  (f32 accumulate)
\ Getting this wrong is the course's #1 "correct in NumPy, garbage on device", so it was
\ proven element-exact in isolation before this K-looping kernel was built.
\
\ Reuses cg-matmul.f verbatim: MM-THREAD-SETUP (r8=tid_lin r9=rowBase r10=colBase r11=SH),
\ MM-PARAMS, MM-ACC-ZERO-EMIT (%f10..%f25=0), MM-PIPE-KLOOP-WITH (cp.async double buffer;
\ the compute quotation runs from cur buffer base %r16). Load after cg-matmul.f.
\
\ Register map (beyond cg-matmul's r1..r18/rd1..rd3): invariants r24..r34 (avoid r20..r23
\ which MM-CP-CHUNK scratches every prefetch); compute scratch r40..r44, tf32 regs r50..r55,
\ load temps %f26..%f31. EMIT-MATMUL-MMA emits kernel `MMM`, ABI (pA,pB,pC,pM,pN,pK) = MM's.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-matmul.f

8192 constant MMA-BSOFF        \ Bs tile byte offset within a buffer (= MM-ASB, As[64][32] bytes)

\ loop-invariant lane geometry + the A/B shared byte bases and global C row/col bases.
: MMA-SETUP ( -- )
   s" shr.u32 %r24,%r8,5;" PTX-L         \ warpid  = tid_lin>>5
   s" and.b32 %r25,%r8,31;" PTX-L        \ lane    = tid_lin&31
   s" shr.u32 %r26,%r24,1;" PTX-L        \ warp_row = warpid>>1  (0..3)
   s" and.b32 %r27,%r24,1;" PTX-L        \ warp_col = warpid&1   (0..1)
   s" shr.u32 %r28,%r25,2;" PTX-L        \ gid = lane>>2
   s" and.b32 %r29,%r25,3;" PTX-L        \ t   = lane&3
   s" shl.b32 %r30,%r26,4;" PTX-L        \ A shared row byte base = ((warp_row*16)+gid)*128
   s" add.u32 %r30,%r30,%r28;" PTX-L
   s" shl.b32 %r30,%r30,7;" PTX-L
   s" shl.b32 %r31,%r27,5;" PTX-L        \ B shared col byte base = ((warp_col*32)+gid)*4
   s" add.u32 %r31,%r31,%r28;" PTX-L
   s" shl.b32 %r31,%r31,2;" PTX-L
   s" shl.b32 %r32,%r26,4;" PTX-L        \ gRow0 = rowBase + warp_row*16 + gid
   s" add.u32 %r32,%r9,%r32;" PTX-L
   s" add.u32 %r32,%r32,%r28;" PTX-L
   s" add.u32 %r33,%r32,8;" PTX-L        \ gRow1 = gRow0 + 8
   s" shl.b32 %r34,%r27,5;" PTX-L        \ gCol0 = colBase + warp_col*32 + 2t
   s" add.u32 %r34,%r10,%r34;" PTX-L
   s" shl.b32 %r40,%r29,1;" PTX-L
   s" add.u32 %r34,%r34,%r40;" PTX-L ;

\ one n-tile j (0..3): load its 8x8 B fragment from Bs at col warp_col*32 + j*8, then MMA it
\ into the 4-f32 accumulator %f(10+4j)..%f(13+4j) (D = A.B + D, A fragment reused from MMA-KSTEP).
: MMA-NTILE ( n -- ) {: j:n :}
   SB-RESET s" ld.shared.f32 %f30,[%r44+" SB-APPEND j 32 * SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f31,[%r44+" SB-APPEND j 32 * 1024 + SB-U s" ];" SB-APPEND SB$ PTX-L
   s" cvt.rna.tf32.f32 %r54,%f30;" PTX-L
   s" cvt.rna.tf32.f32 %r55,%f31;" PTX-L
   10 j 4 * + {: a0:n :}
   SB-RESET s" mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32 {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" }, {%r50,%r51,%r52,%r53}, {%r54,%r55}, {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" };" SB-APPEND SB$ PTX-L ;

\ one MMA-K substep ks (0/8/16/24): load the warp's 16x8 A fragment (reused across n-tiles),
\ set the Bs base for j=0 into %r44, then MMA the 4 n-tiles. A/B read from cur buffer base %r16.
: MMA-KSTEP ( n -- ) {: ks:n :}
   SB-RESET s" add.u32 %r40,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L   \ (ks+t)
   s" shl.b32 %r40,%r40,2;" PTX-L                                                \ *4
   s" add.u32 %r41,%r16,%r30;" PTX-L
   s" add.u32 %r40,%r41,%r40;" PTX-L                                             \ A base_lo
   s" ld.shared.f32 %f26,[%r40];" PTX-L         \ a0 = A[gid][ks+t]
   s" ld.shared.f32 %f27,[%r40+1024];" PTX-L    \ a1 = A[gid+8][ks+t]  (+8 rows *128)
   s" ld.shared.f32 %f28,[%r40+16];" PTX-L      \ a2 = A[gid][ks+t+4]  (+4 cols *4)
   s" ld.shared.f32 %f29,[%r40+1040];" PTX-L    \ a3 = A[gid+8][ks+t+4]
   s" cvt.rna.tf32.f32 %r50,%f26;" PTX-L
   s" cvt.rna.tf32.f32 %r51,%f27;" PTX-L
   s" cvt.rna.tf32.f32 %r52,%f28;" PTX-L
   s" cvt.rna.tf32.f32 %r53,%f29;" PTX-L
   SB-RESET s" add.u32 %r42,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L   \ (ks+t)
   s" shl.b32 %r42,%r42,8;" PTX-L                                                \ *256 (Bs row stride 64f)
   SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-BSOFF SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r44,%r44,%r42;" PTX-L
   s" add.u32 %r44,%r44,%r31;" PTX-L                                             \ Bs base, n-tile 0
   4 0 do  i MMA-NTILE  loop ;

: MMA-KTILE ( -- )  4 0 do  i 8 * MMA-KSTEP  loop ;   \ 4 MMA-K substeps over the BK=32 staged tile

\ store one n-tile j's 4 accumulators to global C with the D-fragment (row,col) mapping
: MMA-STORE-TILE ( n -- ) {: j:n :}
   SB-RESET s" add.u32 %r40,%r34," SB-APPEND j 8 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ col0 = gCol0 + j*8
   10 j 4 * + {: a0:n :}
   s" mad.lo.u32 %r41,%r32,%r2,%r40;" PTX-L                                         \ gRow0*N + col0
   s" mul.wide.u32 %rd10,%r41,4;" PTX-L  s" add.u64 %rd12,%rd3,%rd10;" PTX-L
   SB-RESET s" st.global.f32 [%rd12],%f" SB-APPEND a0 SB-U s" ;" SB-APPEND SB$ PTX-L        \ d0
   SB-RESET s" st.global.f32 [%rd12+4],%f" SB-APPEND a0 1+ SB-U s" ;" SB-APPEND SB$ PTX-L   \ d1
   s" mad.lo.u32 %r43,%r33,%r2,%r40;" PTX-L                                         \ gRow1*N + col0
   s" mul.wide.u32 %rd11,%r43,4;" PTX-L  s" add.u64 %rd13,%rd3,%rd11;" PTX-L
   SB-RESET s" st.global.f32 [%rd13],%f" SB-APPEND a0 2 + SB-U s" ;" SB-APPEND SB$ PTX-L    \ d2
   SB-RESET s" st.global.f32 [%rd13+4],%f" SB-APPEND a0 3 + SB-U s" ;" SB-APPEND SB$ PTX-L ;   \ d3

: MMA-STORE ( -- )  4 0 do  i MMA-STORE-TILE  loop ;

: MMA-BODY ( -- )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MMA-SETUP
   [: MMA-KTILE ;] MM-PIPE-KLOOP-WITH
   MMA-STORE ;

: EMIT-MATMUL-MMA ( -- )
   PTX-HEADER-SM87  PTX-NL
   s" .visible .entry MMM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<48>;" PTX-L  s" .reg .b32 %r<64>;" PTX-L  s" .reg .b64 %rd<48>;" PTX-L
   SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MM-SMEM SB-U s" ];" SB-APPEND SB$ PTX-L
   MM-PARAMS
   MMA-BODY
   s" ret;" PTX-L  s" }" PTX-L ;
