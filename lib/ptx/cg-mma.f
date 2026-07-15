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

\ ============ TILE CONFIGURATION (dot habu-mma-larger-bk) =====================
\ Emit-time knobs. Their DEFAULT values reproduce the BK=32 scalar+cvt kernel
\ BYTE-FOR-BYTE, so lib/ptx/cg-matmul.f (MM), maki/lower-mm.f (LMM-MMA-BODY reuses
\ MMA-SETUP / MMA-KTILE / MM-PIPE-KLOOP-WITH), lib/ptx/opt-test.f, and the committed
\ TF32 golden are unchanged. Only cg-mma.f's own EMIT-MATMUL-MMA (and the bench/check
\ harnesses) raise them; a non-default config uses the MMA-owned staging/pipeline below
\ instead of the shared MM-PIPE scaffold. Restore the defaults after emitting.
\   MMA-BK    - staged K-tile depth (fewer bar.sync / K); multiple of MMA-MK, BK/4 a power of two.
\   MMA-PAD   - As row pad floats (0 or a positive multiple of 4 to keep 16B cp.async alignment)
\               so the ldmatrix fragment rows stop colliding on one shared-memory bank.
\   MMA-STAGES- cp.async pipeline buffers (2 = double-buffered overlap; 1 = single-buffer).
\   MMA-DYNSMEM - 1 emits .extern dynamic .shared for a tile past the 48 KiB static cap.
64 constant MMA-BM                              \ output tile rows (fixed by the 8-warp layout, = MM-BM)
64 constant MMA-BN                              \ output tile cols (= MM-BN)
8  constant MMA-MK                              \ mma.sync K per substep (m16n8k8)
49152 constant MMA-SMEM-STATIC-CAP              \ sm_87 static .shared per-block ceiling (48 KiB)
-6100 constant E-MMA-SMEM                        \ derived shared tile exceeds the legal budget

variable MMA-BK      32 MMA-BK !               \ staged K-tile depth
variable MMA-PAD      0 MMA-PAD !              \ As row pad floats
variable MMA-STAGES   2 MMA-STAGES !          \ cp.async pipeline buffers
variable MMA-DYNSMEM  0 MMA-DYNSMEM !         \ 1 = .extern dynamic .shared

: MMA-AROW-F ( -- n )  MMA-BK @ MMA-PAD @ + ;         \ As row stride, floats
: MMA-AROW-B ( -- n )  MMA-AROW-F 4 * ;               \ As row stride, bytes (default 128)
: MMA-ASB    ( -- n )  MMA-BM MMA-AROW-B * ;          \ As tile bytes / Bs byte offset (default 8192)
: MMA-BSB    ( -- n )  MMA-BK @ MMA-BN * 4 * ;        \ Bs tile bytes (default 8192)
: MMA-BUFB   ( -- n )  MMA-ASB MMA-BSB + ;            \ one cp.async buffer (default 16384)
: MMA-SMEM   ( -- n )  MMA-BUFB MMA-STAGES @ * ;      \ total shared bytes (default 32768)
: MMA-KSUBS  ( -- n )  MMA-BK @ MMA-MK / ;            \ mma.sync K substeps per tile (default 4)
: MMA-ACPR   ( -- n )  MMA-BK @ 4 / ;                 \ As cp.async chunks per row (default 8)
: MMA-CPN    ( -- n )  MMA-BM MMA-BK @ * 4 / 256 / ;  \ cp.async chunk-sets/thread per array (default 2)
: MMA-DEFAULT? ( -- bool )                             \ the byte-identical baseline config
   MMA-BK @ 32 =  MMA-PAD @ 0=  and  MMA-STAGES @ 2 =  and  MMA-DYNSMEM @ 0=  and ;

: MMA-LOG2 ( n -- n )                                  \ floor log2 (n a power of two, > 0)
   0 swap  begin dup 1 > while  2 /  swap 1+ swap  repeat  drop ;
: MMA-POW2? ( n -- bool ) {: v:n :}  v 0 >  v v 1- and 0=  and ;
\ emit "%rDST = %rSRC * m": shl.b32 (power of two, byte-identical at m=128 -> shl 7) else mul.lo.u32
: MMA-SCALE ( n n n -- ) {: dst:n src:n m:n :}
   m MMA-POW2? if
      SB-RESET s" shl.b32 %r" SB-APPEND dst SB-U s" ,%r" SB-APPEND src SB-U
         s" ," SB-APPEND m MMA-LOG2 SB-U s" ;" SB-APPEND SB$ PTX-L
   else
      SB-RESET s" mul.lo.u32 %r" SB-APPEND dst SB-U s" ,%r" SB-APPEND src SB-U
         s" ," SB-APPEND m SB-U s" ;" SB-APPEND SB$ PTX-L
   then ;
: MMA-CHECK-SMEM ( -- )                                \ fail closed on an illegal static tile
   MMA-DYNSMEM @ if exit then
   MMA-SMEM MMA-SMEM-STATIC-CAP > if E-MMA-SMEM throw then ;

\ FRAGMENT-LOAD MODE (dot habu-mma-ldmatrix-fragment). The 16x8 A fragment and 8x8 B fragment
\ can be fed to the tensor cores three ways; mode is fixed at emit time:
\   0 = scalar ld.shared.f32 + cvt.rna.tf32.f32 (rung-1 baseline, 4+8 loads + 48 cvt/tile) - DEFAULT
\   1 = scalar ld.shared.b32, NO cvt (mma.sync truncates the raw f32 to tf32) - cvt-drop ablation
\   2 = ldmatrix.sync.aligned.m8n8.x4.shared.b16 for A + raw ld.shared.b32 for B, NO cvt:
\       ONE ldmatrix replaces the 4 scalar A loads and packs the 4 tf32/lane; dropping cvt kills
\       all 48 cvt/tile. A tf32 value is 2 adjacent b16 halves, so the 16x8 A fragment = 4
\       congruous 8x8 b16 tiles = one ldmatrix.x4 (mapping proven element-exact by
\       tools/ptx/mma-probe.f MP-LDM-ALL, and the full K-loop by mma-gemm-check all 3 modes).
\
\ MEASURED (docs/eval-triton.md step 3c, Orin sm_87): dropping cvt (mode 1) is FLAT vs baseline
\ and ldmatrix (mode 2) is ~1% SLOWER (370/389/394 vs 376/394/399 GFLOP/s at 512/1024/2048),
\ ptxas 38 -> 43 reg (ldmatrix needs more, not fewer), 0 spill both. So at THIS rung (16x32 warp
\ tile, 4x A-reuse) the tensor cores are NOT fragment-feed-bound: the scalar-load bank conflicts
\ and the 48 cvt/tile are already hidden, so removing them does not move the MMA-issue bottleneck.
\ The default therefore stays mode 0 (measured-best-tied AND exact-RNE, so the licensed tf32
\ golden is unchanged). The ldmatrix mechanism is kept proven + selectable for the higher-reuse
\ 16x64 warp-tile / swizzled-Bs rung (habu-mma-16x64-warp, habu-mma-larger-bk), where the loaded
\ A fragment is reused 8x so ldmatrix's warp-level cost amortizes and B-ldmatrix becomes clean.
variable MMA-LMODE   0 MMA-LMODE !

\ mode-2 loop-invariant ldmatrix.x4 A geometry: rt=lane&7, tsel=lane>>3 select the 4 8x8 b16
\ tiles; %r47 = A row byte base (row = (tsel&1)*8 + rt + warp_row*16), %r49 = kcol hi bytes.
: MMA-SETUP-LDM ( -- )
   s" and.b32 %r45,%r25,7;" PTX-L        \ rt   = lane&7
   s" shr.u32 %r46,%r25,3;" PTX-L        \ tsel = lane>>3
   s" and.b32 %r40,%r46,1;" PTX-L  s" shl.b32 %r40,%r40,3;" PTX-L        \ (tsel&1)*8  (tile1/3 = +8 rows)
   s" add.u32 %r47,%r40,%r45;" PTX-L
   s" shl.b32 %r40,%r26,4;" PTX-L  s" add.u32 %r47,%r47,%r40;" PTX-L     \ + warp_row*16 = ldm A row
   47 47 MMA-AROW-B MMA-SCALE            \ * As row byte stride = A row byte base (invariant)
   s" shr.u32 %r49,%r46,1;" PTX-L  s" shl.b32 %r49,%r49,4;" PTX-L ;      \ (tsel>>1)*16 = kcol hi bytes (tile2/3 = +4 K)

\ loop-invariant lane geometry + the A/B shared byte bases and global C row/col bases.
: MMA-SETUP ( -- )
   s" shr.u32 %r24,%r8,5;" PTX-L         \ warpid  = tid_lin>>5
   s" and.b32 %r25,%r8,31;" PTX-L        \ lane    = tid_lin&31
   s" shr.u32 %r26,%r24,1;" PTX-L        \ warp_row = warpid>>1  (0..3)
   s" and.b32 %r27,%r24,1;" PTX-L        \ warp_col = warpid&1   (0..1)
   s" shr.u32 %r28,%r25,2;" PTX-L        \ gid = lane>>2
   s" and.b32 %r29,%r25,3;" PTX-L        \ t   = lane&3
   s" shl.b32 %r30,%r26,4;" PTX-L        \ A shared row byte base = ((warp_row*16)+gid)*<As row stride>
   s" add.u32 %r30,%r30,%r28;" PTX-L
   30 30 MMA-AROW-B MMA-SCALE
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
   s" add.u32 %r34,%r34,%r40;" PTX-L
   MMA-LMODE @ 2 = if MMA-SETUP-LDM then ;   \ mode-2-only geometry; modes 0/1 stay byte-identical to rung 1

\ --- A fragment (16x8, reused across the 4 n-tiles) -> tf32 regs %r50..%r53, mode-switched ---
: MMA-A-BASE ( n -- ) {: ks:n :}                \ %r40 = As base_lo = %r16 + %r30 + (ks+t)*4 (scalar A)
   SB-RESET s" add.u32 %r40,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L
   s" shl.b32 %r40,%r40,2;" PTX-L
   s" add.u32 %r41,%r16,%r30;" PTX-L
   s" add.u32 %r40,%r41,%r40;" PTX-L ;
: MMA-A-CVT ( n -- )                            \ mode 0: 4 scalar ld.shared.f32 + cvt.rna
   MMA-A-BASE
   8 MMA-AROW-B * {: a1o:n :}                    \ +8 As rows = a1/a3 byte offset (default 1024)
   s" ld.shared.f32 %f26,[%r40];" PTX-L         \ a0 = A[gid][ks+t]
   SB-RESET s" ld.shared.f32 %f27,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L    \ a1 = A[gid+8][ks+t]
   s" ld.shared.f32 %f28,[%r40+16];" PTX-L      \ a2 = A[gid][ks+t+4]
   SB-RESET s" ld.shared.f32 %f29,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L  \ a3 = A[gid+8][ks+t+4]
   s" cvt.rna.tf32.f32 %r50,%f26;" PTX-L  s" cvt.rna.tf32.f32 %r51,%f27;" PTX-L
   s" cvt.rna.tf32.f32 %r52,%f28;" PTX-L  s" cvt.rna.tf32.f32 %r53,%f29;" PTX-L ;
: MMA-A-RAW ( n -- )                            \ mode 1: 4 scalar ld.shared.b32 (mma truncates f32->tf32)
   MMA-A-BASE
   8 MMA-AROW-B * {: a1o:n :}
   s" ld.shared.b32 %r50,[%r40];" PTX-L
   SB-RESET s" ld.shared.b32 %r51,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L
   s" ld.shared.b32 %r52,[%r40+16];" PTX-L
   SB-RESET s" ld.shared.b32 %r53,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L ;
: MMA-A-LDM ( n -- ) {: ks:n :}                 \ mode 2: ONE ldmatrix.x4 (row base %r47, kcol-hi %r49 from MMA-SETUP)
   SB-RESET s" add.u32 %r48,%r49," SB-APPEND ks 4 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ kcol bytes = (tsel>>1)*16 + ks*4
   s" add.u32 %r48,%r48,%r47;" PTX-L                                                 \ + A row byte base
   s" add.u32 %r48,%r16,%r48;" PTX-L                                                 \ + buffer base = shared addr
   s" ldmatrix.sync.aligned.m8n8.x4.shared.b16 {%r50,%r51,%r52,%r53},[%r48];" PTX-L ;
: MMA-LOAD-A ( n -- )                           \ ks on stack; dispatch by MMA-LMODE
   MMA-LMODE @ 2 = if MMA-A-LDM exit then
   MMA-LMODE @ 0= if MMA-A-CVT else MMA-A-RAW then ;

\ --- B fragment (8x8) -> tf32 regs %r54,%r55, mode-switched (Bs base for j=0 in %r44) ---
: MMA-B-CVT ( n -- ) {: j:n :}                  \ mode 0: f32 load + cvt.rna
   SB-RESET s" ld.shared.f32 %f30,[%r44+" SB-APPEND j 32 * SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f31,[%r44+" SB-APPEND j 32 * 1024 + SB-U s" ];" SB-APPEND SB$ PTX-L
   s" cvt.rna.tf32.f32 %r54,%f30;" PTX-L  s" cvt.rna.tf32.f32 %r55,%f31;" PTX-L ;
: MMA-B-RAW ( n -- ) {: j:n :}                  \ mode 1/2: raw ld.shared.b32 (mma truncates)
   SB-RESET s" ld.shared.b32 %r54,[%r44+" SB-APPEND j 32 * SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r55,[%r44+" SB-APPEND j 32 * 1024 + SB-U s" ];" SB-APPEND SB$ PTX-L ;
: MMA-B-LOAD ( n -- )  MMA-LMODE @ 0= if MMA-B-CVT else MMA-B-RAW then ;

\ one n-tile j (0..3): load its 8x8 B fragment from Bs at col warp_col*32 + j*8, then MMA it
\ into the 4-f32 accumulator %f(10+4j)..%f(13+4j) (D = A.B + D, A fragment reused from MMA-KSTEP).
: MMA-NTILE ( n -- ) {: j:n :}
   j MMA-B-LOAD
   10 j 4 * + {: a0:n :}
   SB-RESET s" mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32 {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" }, {%r50,%r51,%r52,%r53}, {%r54,%r55}, {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" };" SB-APPEND SB$ PTX-L ;

\ one MMA-K substep ks (0/8/16/24): load the warp's 16x8 A fragment (reused across n-tiles),
\ set the Bs base for j=0 into %r44, then MMA the 4 n-tiles. A/B read from cur buffer base %r16.
: MMA-KSTEP ( n -- ) {: ks:n :}
   ks MMA-LOAD-A                                                                \ A fragment -> %r50..53
   SB-RESET s" add.u32 %r42,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L   \ (ks+t)
   s" shl.b32 %r42,%r42,8;" PTX-L                                                \ *256 (Bs row stride 64f)
   SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L   \ + Bs byte offset
   s" add.u32 %r44,%r44,%r42;" PTX-L
   s" add.u32 %r44,%r44,%r31;" PTX-L                                             \ Bs base, n-tile 0
   4 0 do  i MMA-NTILE  loop ;

: MMA-KTILE ( -- )  MMA-KSUBS 0 do  i MMA-MK * MMA-KSTEP  loop ;   \ BK/MMA-K substeps over the staged tile

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

\ ============ MMA-owned cp.async staging + K-loop (NON-default BK/pad/stages) =========
\ Generalizes cg-matmul MM-CP-CHUNK/MM-PIPE-KLOOP-WITH to As[64][BK] with a padded row
\ stride (MMA-AROW-B) and Bs[BK][64], parameterized by MMA-BK/MMA-PAD/MMA-STAGES. The
\ DEFAULT config keeps using the shared MM-PIPE scaffold (byte-identical); these words run
\ only for a raised BK / padded / single-buffer tile. chunk c = tid_lin + m*256 (16B each).
\   As chunk: row = c/ACPR, kchunk = c%ACPR, k = kchunk*4; dst = buf + row*AROW-B + kchunk*16.
\   Bs chunk: k = c/16, col = (c&15)*4; dst = buf + ASB + c*16 (Bs is unpadded/flat).
\ Prefetch scratch %r20..23 / %rd10..11 (invariants live in %r24..34, so they survive).
: MMA-CP-CHUNK ( n n n -- ) {: m:n bufr:n ktr:n :}
   MMA-ACPR MMA-LOG2 {: acl:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m 256 * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*256
   \ --- As: row=c>>acl, k=(c&(ACPR-1))*4 ; src A[rowBase+row][kt+k] ; dst buf + row*AROW-B + kchunk*16 ---
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND acl SB-U s" ;" SB-APPEND SB$ PTX-L         \ row
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-ACPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L \ kchunk
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ k = kchunk*4
   s" add.u32 %r23,%r9,%r21;" PTX-L
   SB-RESET s" mad.lo.u32 %r23,%r23,%r3,%r" SB-APPEND ktr SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd1,%rd10;" PTX-L
   23 21 MMA-AROW-B MMA-SCALE                                                           \ %r23 = row*AROW-B
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ kchunk*16 = k*4
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L
   \ --- Bs: k=c>>4, col=(c&15)*4 ; src B[kt+k][colBase+col] ; dst buf + ASB + c*16 ---
   s" shr.u32 %r21,%r20,4;" PTX-L
   s" and.b32 %r22,%r20,15;" PTX-L  s" shl.b32 %r22,%r22,2;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r21;" SB-APPEND SB$ PTX-L
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L
   s" shl.b32 %r23,%r20,4;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;

: MMA-CP-STAGE ( n n -- ) {: bufr:n ktr:n :}   \ stage one K-tile (As+Bs) into buffer bufr from column ktr
   MMA-CPN 0 do  i bufr ktr MMA-CP-CHUNK  loop ;

\ double-buffered (MMA-STAGES=2) cp.async pipeline, BK-parameterized (mirror of MM-PIPE-KLOOP-WITH).
\ The compute quotation sits on the data stack from entry to its `execute` slot (as in MM-PIPE).
: MMA-PIPE-KLOOP-WITH ( [ -- ] -- )
   s" mov.u32 %r14,0;" PTX-L  s" mov.u32 %r15,0;" PTX-L
   11 14 MMA-CP-STAGE
   s" cp.async.commit_group;" PTX-L
   s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   SB-RESET s" mul.lo.u32 %r16,%r15," SB-APPEND MMA-BUFB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r16,%r11,%r16;" PTX-L
   SB-RESET s" add.u32 %r17,%r14," SB-APPEND MMA-BK @ SB-U s" ;" SB-APPEND SB$ PTX-L
   s" setp.lt.u32 %p2,%r17,%r3;" PTX-L
   s" @!%p2 bra $NOPF;" PTX-L
   s" xor.b32 %r18,%r15,1;" PTX-L
   SB-RESET s" mul.lo.u32 %r18,%r18," SB-APPEND MMA-BUFB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r18,%r11,%r18;" PTX-L
   18 17 MMA-CP-STAGE
   s" cp.async.commit_group;" PTX-L
   s" cp.async.wait_group 1;" PTX-L
   s" bra $PFDONE;" PTX-L
   s" $NOPF:" PTX-L
   s" cp.async.wait_group 0;" PTX-L
   s" $PFDONE:" PTX-L
   s" bar.sync 0;" PTX-L
   execute
   s" bar.sync 0;" PTX-L
   SB-RESET s" add.u32 %r14,%r14," SB-APPEND MMA-BK @ SB-U s" ;" SB-APPEND SB$ PTX-L
   s" xor.b32 %r15,%r15,1;" PTX-L
   s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L ;

\ single-buffer (MMA-STAGES=1) K-loop: stage, drain, compute, reuse. Fewer bar.sync per K than
\ BK=32 (bigger tile) but no cp.async/compute overlap; fits the 48 KiB static cap at BK=64.
: MMA-PIPE-KLOOP-SINGLE ( [ -- ] -- )
   s" mov.u32 %r14,0;" PTX-L
   s" mov.u32 %r16,%r11;" PTX-L                                    \ single buffer base = SH
   s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   11 14 MMA-CP-STAGE
   s" cp.async.commit_group;" PTX-L
   s" cp.async.wait_group 0;" PTX-L
   s" bar.sync 0;" PTX-L
   execute
   s" bar.sync 0;" PTX-L
   SB-RESET s" add.u32 %r14,%r14," SB-APPEND MMA-BK @ SB-U s" ;" SB-APPEND SB$ PTX-L
   s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L ;

: MMA-KLOOP ( [ -- ] -- )                                          \ pick the pipeline for the active config
   MMA-DEFAULT? if MM-PIPE-KLOOP-WITH exit then
   MMA-STAGES @ 1 = if MMA-PIPE-KLOOP-SINGLE else MMA-PIPE-KLOOP-WITH then ;

: MMA-BODY ( -- )
   MMA-CHECK-SMEM
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MMA-SETUP
   [: MMA-KTILE ;] MMA-KLOOP
   MMA-STORE ;

: EMIT-MATMUL-MMA ( -- )
   PTX-HEADER-SM87  PTX-NL
   MMA-DYNSMEM @ if
      s" .extern .shared .align 16 .b8 SH[];" PTX-L        \ module-scope dynamic .shared (sized at launch)
   then
   s" .visible .entry MMM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<48>;" PTX-L  s" .reg .b32 %r<64>;" PTX-L  s" .reg .b64 %rd<48>;" PTX-L
   MMA-DYNSMEM @ 0= if
      SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MMA-SMEM SB-U s" ];" SB-APPEND SB$ PTX-L
   then
   MM-PARAMS
   MMA-BODY
   s" ret;" PTX-L  s" }" PTX-L ;
