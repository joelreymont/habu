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
\
\ The above describes the DEFAULT single-M-frag 64x64 tile. The MMA-MFRAGS knob (see the TILE
\ CONFIGURATION section) grows each warp to MFRAGS stacked 16-row M-fragments (64*MFRAGS x 64
\ block) to AMORTIZE the B-side fragment feed - the parity lever (dot habu-mma-amortize-the):
\ MFRAGS=2 (128x64) measured 2133.9 GFLOP/s = 1.13x Triton at 2048^3 (918 MHz). Every MFRAGS>1
\ path is gated so MFRAGS=1 stays byte-identical to the pinned SWZ / SWZ-BK64 / lower-mm goldens.
\ MMA-ABLATE (DCE-safe wide-kernel cost decomposition) is driven by tools/ptx/mma-ablate.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-matmul.f
require lib/ptx/cpp-slot.f

\ ============ TILE CONFIGURATION (dot habu-mma-larger-bk) =====================
\ Emit-time knobs. Their DEFAULT values reproduce the BK=32 scalar+cvt kernel
\ BYTE-FOR-BYTE, so lib/ptx/cg-matmul.f (MM), maki/lower/mm.f (LMM-MMA-BODY reuses
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
-6102 constant E-MMA-BLDM                        \ B-ldmatrix config illegal (non-16B BT row, or MFRAGS=1)
-6103 constant E-MMA-WARPS                        \ illegal warp grid (WARPS not 4/8, or WARPS=4 without the wide MFRAGS>1 staging)

variable MMA-BK      32 MMA-BK !               \ staged K-tile depth
variable MMA-PAD      0 MMA-PAD !              \ As row pad floats
variable MMA-STAGES   2 MMA-STAGES !          \ cp.async pipeline buffers
variable MMA-DYNSMEM  0 MMA-DYNSMEM !         \ 1 = .extern dynamic .shared

\ WIDER-M REGISTER TILE (dot habu-mma-amortize-the, B-feed amortization). Each warp owns
\ MMA-MFRAGS stacked 16-row M-fragments (default 1). At MFRAGS>1 the 8-warp grid keeps its
\ 4x2 warp layout and BN=64, so the OUTPUT BLOCK grows in M to 64*MFRAGS x 64 and each warp
\ owns (16*MFRAGS)x32 = MFRAGS M-frags x 4 n-tiles. Per K-substep a warp loads MFRAGS A
\ fragments (each ldmatrix.x4 / scalar, one per M-frag) and then, per n-tile, loads its 8x8 B
\ fragment ONCE and issues MFRAGS mma against it -> each B fragment is REUSED across MFRAGS
\ M-frags (the attribution's THE lever: B-side scalar feed was ~40% of iteration time with
\ zero reuse). A-side ldmatrix is ~free (reused 4x per n-tile), so paying MFRAGS A loads to
\ halve+ the B feed is the trade. MFRAGS>1 also HALVES global B staging (Bs reused across
\ 64*MFRAGS rows), lowering the cp.async floor too. Accumulators = 16*MFRAGS f32/lane
\ (%f10.. ; M-frag f n-tile j -> %f(10+16f+4j)); tf32 A group for M-frag f = %r(50+6f)
\ ({%r50..53},{%r56..59}), B in %r54,%r55, mode-0 f-temps relocated to %f42..47 (past the
\ wide accumulators) so the .reg .f32 %f<48> / .b32 %r<64> header is UNCHANGED. MFRAGS=1
\ takes the legacy path verbatim (byte-identical: pinned SWZ / SWZ-BK64 / lower-mm goldens).
\ Occupancy math (target MFRAGS=2 BK=32 pad=8 stages=2 dyn mode-2, 128x64 block, 256 thr):
\   smem = As[128][40]*4 (20480) + Bs[32][64]*4 (8192) = 28672/buf x2 = 57344 B -> dynamic
\   (>48KiB static cap); 164KiB/SM -> 2 blocks/SM = 16 warps (same rung as SWZ-BK64's 66048 B
\   2-block occupancy). ~60 regs/thread (32 acc + 8 A-tf32 + scratch) -> 4 blocks by regs, so
\   smem binds at 2. stages=1 static (28672 B) frees 5 blocks/SM = 40 warps if the floor is
\   already hidden. B fragment feed HALVED, global B staging HALVED.
variable MMA-MFRAGS   1 MMA-MFRAGS !          \ M-fragments (16-row units) per warp

\ WARP-GRID SHAPE (dot habu-4-warp-mma). The block's warps tile the output as WROWS x WCOLS, with
\ WCOLS FIXED at 2 (warp_col = warpid&1 selects one of the two 32-col halves of BN=64) and
\ WROWS = MMA-WARPS/2 (warp_row = warpid>>1 selects one of the WROWS row-blocks). So MMA-WARPS=8 is
\ the legacy 4x2 grid (256 threads, WROWS=4) and MMA-WARPS=4 is the 2x2 grid (128 threads, WROWS=2).
\ The per-warp geometry is IDENTICAL for any WARPS: gid=lane>>2, t=lane&3, one 16-row M-frag per
\ (warp_row, f) at row-block base warp_row*(16*MFRAGS)+f*16 - only the NUMBER of warp-rows and the
\ thread count change, so the fragment->lane map, the 16*MFRAGS accumulator layout, and the
\ D-fragment store map are shared verbatim with the 8-warp family (Triton's per-shape tf32 winners
\ run this narrower 4-warp / BM128xBN64 blocking, docs/eval-triton.md GB10). The 4-warp grid needs
\ the WIDE (MFRAGS>1) staging: its per-block M is WROWS*16*MFRAGS = 32*MFRAGS, and the cp.async
\ chunk partition divides by MMA-NTHREADS = WARPS*32 (128 here, not 256). At WARPS=8 every derived
\ count is unchanged, so all pinned 8-warp configs stay byte-identical.
variable MMA-WARPS    8 MMA-WARPS !           \ warps/block: 8 = 4x2 grid (WROWS=4), 4 = 2x2 grid (WROWS=2)

\ ABLATION knob (dot habu-mma-amortize-the; productizes the attribution-lane timing decomposition).
\ DCE-SAFE variants of the WIDE kernel that keep every mma + store live (so ptxas cannot delete the
\ ablated work) but drop part of the FEED, isolating each cost by same-session timing delta. Wide-path
\ only; MMA-ABLATE=0 keeps the wide kernel BYTE-IDENTICAL to the measured tile (and MFRAGS=1 is
\ untouched regardless). Results are numerically WRONG on purpose - run under tools/ptx/mma-ablate.f
\ (never mma-gemm-check). 0=full; 1=quarter-B (load B only at n-tile 0, reuse stale across 4 n-tiles
\ = 1/4 the B loads: the CEILING proxy); 2=half-B (load at n-tiles 0,2); 3=single-mma (issue only
\ M-frag 0 per n-tile: isolates the 2nd M-frag mma-issue cost).
variable MMA-ABLATE   0 MMA-ABLATE !

\ B-SIDE ldmatrix over a TRANSPOSED Bs (dot habu-mma-wave-3). The wide path's per-n-tile scalar B
\ feed (MMA-B-LOAD-WIDE, 2 ld.shared + 2 cvt / fragment, un-amortized on the residual 27% B-feed) is
\ replaced by ONE ldmatrix.sync.aligned.m8n8.x2 per 8x8 B fragment. The device-proven law (element-
\ exact, tools/ptx/mma-probe.f MP-BLDM-ALL): a NON-trans ldmatrix over a TRANSPOSED staging
\ SHM_BT[n][k]=B[k][n] returns exactly {b0,b1}={B[ks+t][gid],B[ks+4+t][gid]} (ldmatrix.trans is
\ unusable for tf32 - it splits every tf32 into its two b16 halves, so the transpose MUST live in the
\ staging). WIDE PATH ONLY (MFRAGS>1); MMA-BLDM=0 keeps every pinned config BYTE-IDENTICAL. The
\ n-major BT row stride is BK+MMA-BPAD floats: a NEW bank geometry (the transpose scatters the shared
\ write and the ldmatrix read), so MMA-BPAD is a measured knob (BPAD=4 -> BTROW=36 words, an ldmatrix
\ read start-bank stride of 4 -> conflict-free 8-row tiles; BPAD=0 fits the 48 KiB static cap but the
\ 8 tile rows alias one 4-bank window). The staging is a scalar TRANSPOSED copy (coalesced global
\ read B[k][n], strided shared write BT[n][k]) since cp.async cannot scatter a contiguous chunk.
variable MMA-BLDM   0 MMA-BLDM !              \ 1 = B-fragment ldmatrix over transposed Bs (wide path)
variable MMA-BPAD   0 MMA-BPAD !              \ BT row pad floats (n-major row stride = BK+BPAD)

: MMA-WROWS  ( -- n )  MMA-WARPS @ 2 / ;              \ warp-rows (WCOLS fixed 2); 4 at WARPS=8, 2 at WARPS=4
: MMA-NTHREADS ( -- n )  MMA-WARPS @ 32 * ;           \ threads/block; 256 at WARPS=8, 128 at WARPS=4
: MMA-BROWS  ( -- n )  MMA-WROWS 16 * MMA-MFRAGS @ * ;  \ output block rows = WROWS*16*MFRAGS (64*MFRAGS at WARPS=8)
: MMA-AROW-F ( -- n )  MMA-BK @ MMA-PAD @ + ;         \ As row stride, floats
: MMA-AROW-B ( -- n )  MMA-AROW-F 4 * ;               \ As row stride, bytes (default 128)
: MMA-ASB    ( -- n )  MMA-BROWS MMA-AROW-B * ;       \ As tile bytes / Bs byte offset (default 8192)
: MMA-BTROW-F ( -- n )  MMA-BK @ MMA-BPAD @ + ;       \ transposed-Bs (BT) row stride, floats (n-major over k)
: MMA-BTROW-B ( -- n )  MMA-BTROW-F 4 * ;             \ BT row stride, bytes (multiple of 16 for ldmatrix rows)
: MMA-BSB    ( -- n )  MMA-BLDM @ if MMA-BN MMA-BTROW-F * 4 * else MMA-BK @ MMA-BN * 4 * then ;  \ B tile bytes (BT if BLDM)
: MMA-BTCPN  ( -- n )  MMA-BN MMA-BK @ * MMA-NTHREADS / ;   \ transposed-B scalar chunk-sets/thread (64*BK/NTHREADS)
: MMA-BUFB   ( -- n )  MMA-ASB MMA-BSB + ;            \ one cp.async buffer (default 16384)
: MMA-SMEM   ( -- n )  MMA-BUFB MMA-STAGES @ * ;      \ total shared bytes (default 32768)
: MMA-KSUBS  ( -- n )  MMA-BK @ MMA-MK / ;            \ mma.sync K substeps per tile (default 4)
: MMA-ACPR   ( -- n )  MMA-BK @ 4 / ;                 \ As cp.async chunks per row (default 8)
: MMA-CPN    ( -- n )  MMA-BM MMA-BK @ * 4 / MMA-NTHREADS / ;  \ MFRAGS=1 cp.async chunk-sets/thread per array (default 2)
: MMA-ACPN   ( -- n )  MMA-BROWS MMA-BK @ * 4 / MMA-NTHREADS / ; \ wide As cp.async chunk-sets/thread (BROWS!=BN)
: MMA-BCPN   ( -- n )  MMA-BK @ MMA-BN * 4 / MMA-NTHREADS / ;  \ wide Bs cp.async chunk-sets/thread
: MMA-AREG   ( n -- n )  6 * 50 + ;                   \ tf32 A-fragment reg group base for M-frag f
\ Register-pool sizing (dot habu-mma-wave-2). The mode-0 wide cvt temps and the header
\ .reg .f32/.b32 counts must grow past the 16*MFRAGS accumulators for a wider M tile, but MUST
\ stay BYTE-IDENTICAL at MFRAGS<=2 (all shipped/pinned configs). MMA-FTEMP=42 at MFRAGS=2 (= the
\ current hardcoded %f42..47), MMA-FREGS/MMA-RREGS = 48/64 at MFRAGS<=2 (= the current header),
\ and only MFRAGS>2 (the new 256-row MFRAGS=4 tile) enlarges them.
: MMA-FTEMP  ( -- n )  16 MMA-MFRAGS @ * 10 + ;       \ wide cvt temp base (just past the accumulators)
: MMA-FREGS  ( -- n )  MMA-MFRAGS @ 2 > if 16 MMA-MFRAGS @ * 16 + else 48 then ;   \ .reg .f32 pool
: MMA-RREGS  ( -- n )  MMA-MFRAGS @ 2 > if 6 MMA-MFRAGS @ * 48 + else 64 then ;    \ .reg .b32 pool
: MMA-DEFAULT? ( -- bool )                             \ the byte-identical baseline config (8-warp only)
   MMA-BK @ 32 =  MMA-PAD @ 0=  and  MMA-STAGES @ 2 =  and  MMA-DYNSMEM @ 0=  and
   MMA-MFRAGS @ 1 =  and  MMA-WARPS @ 8 =  and ;

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

\ fail closed on an illegal B-ldmatrix config (dot habu-mma-wave-3). ldmatrix.sync.aligned.m8n8.b16
\ addresses each 8x8 tile ROW (16 B) and requires a 16 B aligned row address, so the transposed BT row
\ stride MMA-BTROW-B = (BK+BPAD)*4 MUST be a multiple of 16 (else the per-lane ldmatrix addresses are
\ misaligned and the launch faults - a device sm machine-check, NOT a wrong result). B-ldmatrix is also
\ wide-path only (MFRAGS>1); at MFRAGS=1 the non-wide path would silently ignore BLDM. Reject both here
\ so a bad knob combination throws at EMIT time instead of faulting the GPU.
: MMA-CHECK-BLDM ( -- )
   MMA-BLDM @ 0= if exit then
   MMA-MFRAGS @ 2 < if E-MMA-BLDM throw then            \ B-ldmatrix is defined only on the wide (MFRAGS>1) path
   MMA-BTROW-B 15 and 0= 0= if E-MMA-BLDM throw then ;  \ BT row stride not a multiple of 16 B -> misaligned ldmatrix rows

\ fail closed on an illegal warp grid (dot habu-4-warp-mma). Only the 4x2 (WARPS=8) and 2x2 (WARPS=4)
\ grids are implemented (WCOLS fixed 2). The narrower 4-warp grid stages its As over MMA-BROWS rows
\ (WROWS*16*MFRAGS), so it needs the WIDE (MFRAGS>1) cp.async path; the non-wide MFRAGS=1 staging is
\ hardwired to the 64-row 8-warp tile (MMA-CPN uses MMA-BM), so WARPS=4 + MFRAGS=1 would emit a kernel
\ whose 128 threads stage a 64-row As but compute only 32 rows. Reject both at emit time.
: MMA-CHECK-WARPS ( -- )
   MMA-WARPS @ 8 =  MMA-WARPS @ 4 =  or  0= if E-MMA-WARPS throw then   \ only 4x2 / 2x2 grids
   MMA-WARPS @ 4 =  MMA-MFRAGS @ 1 =  and  if E-MMA-WARPS throw then ;  \ 4-warp needs the wide staging

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

\ ============ WIDER-M compute path (MMA-MFRAGS>1; MFRAGS=1 uses the words above verbatim) ====
\ Same device-proven m16n8k8 tf32 fragment layout as MMA-SETUP/MMA-KSTEP/MMA-STORE, applied
\ once per stacked 16-row M-fragment at a +f*16-row base offset. No new lane->element mapping
\ (mma-probe covers it); the MGC 128^3/256^3 golden proves the per-frag base arithmetic. The
\ warp's row-block base is warp_row*(16*MFRAGS) (vs warp_row*16 at MFRAGS=1). rowBase is
\ ctaid.y*BROWS, so MM-THREAD-SETUP (fixed *64) is replaced by MMA-THREAD-SETUP-WIDE.

: MMA-THREAD-SETUP-WIDE ( -- )                 \ like MM-THREAD-SETUP but rowBase = ctaid.y*BROWS
   s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,%tid.y;" PTX-L
   s" mov.u32 %r6,%ctaid.x;" PTX-L  s" mov.u32 %r7,%ctaid.y;" PTX-L
   s" mad.lo.u32 %r8,%r5,16,%r4;" PTX-L
   9 7 MMA-BROWS MMA-SCALE                      \ rowBase = ctaid.y * BROWS
   s" mul.lo.u32 %r10,%r6,64;" PTX-L            \ colBase = ctaid.x * 64
   s" mov.u32 %r11,SH;" PTX-L ;

: MMA-ACC-ZERO-WIDE ( -- )                      \ zero 16*MFRAGS accumulators %f10..
   16 MMA-MFRAGS @ *  0 do
      SB-RESET s" mov.f32 %f" SB-APPEND 10 i + SB-U s" ,0f00000000;" SB-APPEND SB$ PTX-L
   loop ;

: MMA-SETUP-LDM-WIDE ( -- )                     \ mode-2 ldmatrix geometry, M-frag-0 row base (invariant)
   s" and.b32 %r45,%r25,7;" PTX-L               \ rt   = lane&7
   s" shr.u32 %r46,%r25,3;" PTX-L               \ tsel = lane>>3
   s" and.b32 %r40,%r46,1;" PTX-L  s" shl.b32 %r40,%r40,3;" PTX-L   \ (tsel&1)*8
   s" add.u32 %r47,%r40,%r45;" PTX-L
   40 26 16 MMA-MFRAGS @ * MMA-SCALE            \ %r40 = warp_row*(16*MFRAGS)
   s" add.u32 %r47,%r47,%r40;" PTX-L            \ ldm A row (M-frag 0)
   47 47 MMA-AROW-B MMA-SCALE                   \ * As row byte stride
   s" shr.u32 %r49,%r46,1;" PTX-L  s" shl.b32 %r49,%r49,4;" PTX-L ; \ (tsel>>1)*16 = kcol hi bytes

\ B-ldmatrix loop-invariant lane base (dot habu-mma-wave-3). %r35 = ASB + (warp_col*32 + r)*BTROW-B
\ + p*16, where r=lane&7 (n-row of the 8x8 tile), p=(lane>>3)&1 (k-hi half). Per (ks,j) the ldmatrix
\ address is then %r16 + %r35 + j*8*BTROW-B + ks*4. Self-contained (reads only invariants %r25 lane,
\ %r27 warp_col; scratch %r36,%r37), so it is independent of the A-ldmatrix (LMODE) geometry.
: MMA-SETUP-BLDM-WIDE ( -- )
   s" and.b32 %r36,%r25,7;" PTX-L               \ r = lane&7
   s" shl.b32 %r37,%r27,5;" PTX-L               \ warp_col*32
   s" add.u32 %r36,%r37,%r36;" PTX-L            \ warp_col*32 + r
   36 36 MMA-BTROW-B MMA-SCALE                  \ * BT row byte stride
   s" shr.u32 %r37,%r25,3;" PTX-L               \ tsel = lane>>3
   s" and.b32 %r37,%r37,1;" PTX-L               \ p = tsel&1
   s" shl.b32 %r37,%r37,4;" PTX-L               \ p*16 (k-hi half byte offset)
   s" add.u32 %r36,%r36,%r37;" PTX-L
   SB-RESET s" add.u32 %r35,%r36," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L ;   \ + Bs byte offset (BT base)

: MMA-SETUP-WIDE ( -- )                         \ loop-invariant lane geometry + M-frag-0 shared/global bases
   s" shr.u32 %r24,%r8,5;" PTX-L                \ warpid
   s" and.b32 %r25,%r8,31;" PTX-L               \ lane
   s" shr.u32 %r26,%r24,1;" PTX-L               \ warp_row (0..3)
   s" and.b32 %r27,%r24,1;" PTX-L               \ warp_col (0..1)
   s" shr.u32 %r28,%r25,2;" PTX-L               \ gid = lane>>2
   s" and.b32 %r29,%r25,3;" PTX-L               \ t   = lane&3
   30 26 16 MMA-MFRAGS @ * MMA-SCALE            \ A shared row byte base (M-frag 0) = (warp_row*16*MFRAGS + gid)*AROW-B
   s" add.u32 %r30,%r30,%r28;" PTX-L
   30 30 MMA-AROW-B MMA-SCALE
   s" shl.b32 %r31,%r27,5;" PTX-L               \ B shared col byte base = ((warp_col*32)+gid)*4
   s" add.u32 %r31,%r31,%r28;" PTX-L
   s" shl.b32 %r31,%r31,2;" PTX-L
   32 26 16 MMA-MFRAGS @ * MMA-SCALE            \ gRow0 (M-frag 0) = rowBase + warp_row*16*MFRAGS + gid
   s" add.u32 %r32,%r9,%r32;" PTX-L
   s" add.u32 %r32,%r32,%r28;" PTX-L
   s" add.u32 %r33,%r32,8;" PTX-L               \ gRow1 = gRow0 + 8
   s" shl.b32 %r34,%r27,5;" PTX-L               \ gCol0 = colBase + warp_col*32 + 2t
   s" add.u32 %r34,%r10,%r34;" PTX-L
   s" shl.b32 %r40,%r29,1;" PTX-L
   s" add.u32 %r34,%r34,%r40;" PTX-L
   MMA-LMODE @ 2 = if MMA-SETUP-LDM-WIDE then
   MMA-BLDM @ if MMA-SETUP-BLDM-WIDE then ;

\ --- wide A fragment for M-frag f -> tf32 group %r(50+6f), +f*16 rows past the M-frag-0 base ---
: MMA-A-BASE-WIDE ( n n -- ) {: ks:n f:n :}     \ %r40 = As base = %r16 + %r30 + f*16*AROW-B + (ks+t)*4
   SB-RESET s" add.u32 %r40,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L
   s" shl.b32 %r40,%r40,2;" PTX-L
   s" add.u32 %r41,%r16,%r30;" PTX-L
   f 0 > if SB-RESET s" add.u32 %r41,%r41," SB-APPEND f 16 * MMA-AROW-B * SB-U s" ;" SB-APPEND SB$ PTX-L then
   s" add.u32 %r40,%r41,%r40;" PTX-L ;
: MMA-A-CVT-WIDE ( n n -- ) {: ks:n f:n :}      \ mode 0: 4 scalar ld.shared.f32 + cvt.rna -> group f
   ks f MMA-A-BASE-WIDE
   8 MMA-AROW-B * {: a1o:n :}
   f MMA-AREG {: g:n :}
   MMA-FTEMP {: ft:n :}                          \ = 42 at MFRAGS=2 (byte-identical); higher for MFRAGS=4
   SB-RESET s" ld.shared.f32 %f" SB-APPEND ft SB-U s" ,[%r40];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f" SB-APPEND ft 1+ SB-U s" ,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f" SB-APPEND ft 2 + SB-U s" ,[%r40+16];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f" SB-APPEND ft 3 + SB-U s" ,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r" SB-APPEND g SB-U s" ,%f" SB-APPEND ft SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r" SB-APPEND g 1+ SB-U s" ,%f" SB-APPEND ft 1+ SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r" SB-APPEND g 2 + SB-U s" ,%f" SB-APPEND ft 2 + SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r" SB-APPEND g 3 + SB-U s" ,%f" SB-APPEND ft 3 + SB-U s" ;" SB-APPEND SB$ PTX-L ;
: MMA-A-RAW-WIDE ( n n -- ) {: ks:n f:n :}      \ mode 1: 4 scalar ld.shared.b32 -> group f
   ks f MMA-A-BASE-WIDE
   8 MMA-AROW-B * {: a1o:n :}
   f MMA-AREG {: g:n :}
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g SB-U s" ,[%r40];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 1+ SB-U s" ,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 2 + SB-U s" ,[%r40+16];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 3 + SB-U s" ,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L ;
: MMA-A-LDM-WIDE ( n n -- ) {: ks:n f:n :}      \ mode 2: ONE ldmatrix.x4 -> group f (row base %r47 + f*16 rows)
   f MMA-AREG {: g:n :}
   SB-RESET s" add.u32 %r48,%r49," SB-APPEND ks 4 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ kcol bytes
   s" add.u32 %r48,%r48,%r47;" PTX-L                                                 \ + A row byte base (M-frag 0)
   f 0 > if SB-RESET s" add.u32 %r48,%r48," SB-APPEND f 16 * MMA-AROW-B * SB-U s" ;" SB-APPEND SB$ PTX-L then
   s" add.u32 %r48,%r16,%r48;" PTX-L                                                 \ + buffer base
   SB-RESET s" ldmatrix.sync.aligned.m8n8.x4.shared.b16 {%r" SB-APPEND g SB-U s" ,%r" SB-APPEND g 1+ SB-U
      s" ,%r" SB-APPEND g 2 + SB-U s" ,%r" SB-APPEND g 3 + SB-U s" },[%r48];" SB-APPEND SB$ PTX-L ;
: MMA-LOAD-A-WIDE ( n n -- )                    \ ks f ; dispatch by MMA-LMODE
   MMA-LMODE @ 2 = if MMA-A-LDM-WIDE exit then
   MMA-LMODE @ 0= if MMA-A-CVT-WIDE else MMA-A-RAW-WIDE then ;

: MMA-B-CVT-WIDE ( n -- ) {: j:n :}             \ mode 0: f32 load + cvt.rna, temps past the wide accumulators
   MMA-FTEMP 4 + {: bt:n :}                      \ = 46 at MFRAGS=2 (byte-identical); higher for MFRAGS=4
   SB-RESET s" ld.shared.f32 %f" SB-APPEND bt SB-U s" ,[%r44+" SB-APPEND j 32 * SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.f32 %f" SB-APPEND bt 1+ SB-U s" ,[%r44+" SB-APPEND j 32 * 1024 + SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r54,%f" SB-APPEND bt SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" cvt.rna.tf32.f32 %r55,%f" SB-APPEND bt 1+ SB-U s" ;" SB-APPEND SB$ PTX-L ;
: MMA-B-LOAD-WIDE ( n -- )  MMA-LMODE @ 0= if MMA-B-CVT-WIDE else MMA-B-RAW then ;

\ B-side ldmatrix (dot habu-mma-wave-3): ONE ldmatrix.x2 loads the 8x8 B fragment for n-tile j at
\ K-substep ks from the transposed BT staging -> {%r54,%r55} = {b0,b1} (the mma B operand). Address =
\ %r16 + %r35 (invariant lane base) + j*8*BTROW-B + ks*4, aligned to 16 B per lane (BTROW-B mult 16,
\ ks in {0,8,16,24}). Replaces the 2 ld.shared + 2 cvt (mode 0) / 2 raw loads (mode 1/2) per fragment.
: MMA-B-LDM-WIDE ( n n -- ) {: ks:n j:n :}
   j 8 * MMA-BTROW-B *  ks 4 * +  {: off:n :}   \ (ks,j) byte offset from the lane base
   SB-RESET s" add.u32 %r48,%r35," SB-APPEND off SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r48,%r16,%r48;" PTX-L
   s" ldmatrix.sync.aligned.m8n8.x2.shared.b16 {%r54,%r55},[%r48];" PTX-L ;

: MMA-MMA-WIDE ( n n -- ) {: f:n j:n :}         \ mma for M-frag f, n-tile j: D(=%f(10+16f+4j)..) = A(group f).B(%r54,55) + D
   10 f 16 * + j 4 * + {: d:n :}
   f MMA-AREG {: g:n :}
   SB-RESET s" mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32 {%f" SB-APPEND d SB-U
      s" ,%f" SB-APPEND d 1+ SB-U s" ,%f" SB-APPEND d 2 + SB-U s" ,%f" SB-APPEND d 3 + SB-U
      s" }, {%r" SB-APPEND g SB-U s" ,%r" SB-APPEND g 1+ SB-U s" ,%r" SB-APPEND g 2 + SB-U s" ,%r" SB-APPEND g 3 + SB-U
      s" }, {%r54,%r55}, {%f" SB-APPEND d SB-U
      s" ,%f" SB-APPEND d 1+ SB-U s" ,%f" SB-APPEND d 2 + SB-U s" ,%f" SB-APPEND d 3 + SB-U s" };" SB-APPEND SB$ PTX-L ;

\ ablation predicates (emit-time; all true/full at MMA-ABLATE=0 -> byte-identical)
: MMA-ABL-LOADB? ( n -- bool ) {: j:n :}        \ load B for n-tile j? (pure bool expr, no early exit)
   MMA-ABLATE @ 1 <>  MMA-ABLATE @ 2 <>  and    \ neither quarter- nor half-B -> always load
   MMA-ABLATE @ 1 =   j 0=      and  or          \ quarter-B: only n-tile 0
   MMA-ABLATE @ 2 =   j 1 and 0=  and  or ;      \ half-B: n-tiles 0,2
: MMA-ABL-MFRAGS ( -- n )                        \ how many M-frags to mma per n-tile
   MMA-ABLATE @ 3 = if 1 else MMA-MFRAGS @ then ;

\ one n-tile j: load its 8x8 B fragment ONCE, then mma it against every M-frag (B REUSED MFRAGS times).
\ MMA-BLDM=0 keeps the scalar path BYTE-IDENTICAL (else branch = the legacy body); MMA-BLDM=1 issues one
\ ldmatrix.x2 (ks needed for the BT address, hence the (ks j) signature - unused by the scalar path).
: MMA-NTILE-WIDE ( n n -- ) {: ks:n j:n :}
   MMA-BLDM @ if
      j MMA-ABL-LOADB? if ks j MMA-B-LDM-WIDE then
   else
      j MMA-ABL-LOADB? if j MMA-B-LOAD-WIDE then
   then
   MMA-ABL-MFRAGS 0 do  i j MMA-MMA-WIDE  loop ;

\ one MMA-K substep: load MFRAGS A fragments (persist across n-tiles), set Bs base %r44 (scalar path
\ only; the BLDM path addresses BT directly from %r35), do 4 n-tiles.
: MMA-KSTEP-WIDE ( n -- ) {: ks:n :}
   MMA-MFRAGS @ 0 do  ks i MMA-LOAD-A-WIDE  loop
   MMA-BLDM @ 0= if
      SB-RESET s" add.u32 %r42,%r29," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L
      s" shl.b32 %r42,%r42,8;" PTX-L
      SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
      s" add.u32 %r44,%r44,%r42;" PTX-L
      s" add.u32 %r44,%r44,%r31;" PTX-L
   then
   4 0 do  ks i MMA-NTILE-WIDE  loop ;

: MMA-KTILE-WIDE ( -- )  MMA-KSUBS 0 do  i MMA-MK * MMA-KSTEP-WIDE  loop ;

\ store M-frag f, n-tile j: global rows gRow{0,1}+f*16, col gCol0+j*8 (D-fragment mapping)
: MMA-STORE-TILE-WIDE ( n n -- ) {: f:n j:n :}
   SB-RESET s" add.u32 %r40,%r34," SB-APPEND j 8 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ %r40 = col0 = gCol0 + j*8
   10 f 16 * + j 4 * + {: a0:n :}
   f 0 > if SB-RESET s" add.u32 %r41,%r32," SB-APPEND f 16 * SB-U s" ;" SB-APPEND SB$ PTX-L
   else s" mov.u32 %r41,%r32;" PTX-L then                                          \ %r41 = gRow0 + f*16
   s" mad.lo.u32 %r41,%r41,%r2,%r40;" PTX-L
   s" mul.wide.u32 %rd10,%r41,4;" PTX-L  s" add.u64 %rd12,%rd3,%rd10;" PTX-L
   SB-RESET s" st.global.f32 [%rd12],%f" SB-APPEND a0 SB-U s" ;" SB-APPEND SB$ PTX-L        \ d0
   SB-RESET s" st.global.f32 [%rd12+4],%f" SB-APPEND a0 1+ SB-U s" ;" SB-APPEND SB$ PTX-L   \ d1
   f 0 > if SB-RESET s" add.u32 %r43,%r33," SB-APPEND f 16 * SB-U s" ;" SB-APPEND SB$ PTX-L
   else s" mov.u32 %r43,%r33;" PTX-L then                                          \ %r43 = gRow1 + f*16
   s" mad.lo.u32 %r43,%r43,%r2,%r40;" PTX-L
   s" mul.wide.u32 %rd11,%r43,4;" PTX-L  s" add.u64 %rd13,%rd3,%rd11;" PTX-L
   SB-RESET s" st.global.f32 [%rd13],%f" SB-APPEND a0 2 + SB-U s" ;" SB-APPEND SB$ PTX-L    \ d2
   SB-RESET s" st.global.f32 [%rd13+4],%f" SB-APPEND a0 3 + SB-U s" ;" SB-APPEND SB$ PTX-L ;   \ d3
: MMA-STORE-WIDE ( -- )
   MMA-MFRAGS @ 0 do  i {: f:n :}
      4 0 do  f i MMA-STORE-TILE-WIDE  loop
   loop ;

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
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
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

\ WIDER-M staging (BROWS != BN): As and Bs have DIFFERENT chunk counts, so stage them in two
\ independent loops (the MFRAGS=1 interleaved MMA-CP-CHUNK assumes As-chunks == Bs-chunks and
\ stays byte-identical for the pinned configs). Same chunk geometry as MMA-CP-CHUNK, split.
: MMA-CPW-CHUNK-A ( n n n -- ) {: m:n bufr:n ktr:n :}   \ one As 16B chunk, chunk-set m
   MMA-ACPR MMA-LOG2 {: acl:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND acl SB-U s" ;" SB-APPEND SB$ PTX-L         \ row = c>>acl
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-ACPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L \ kchunk = c & (ACPR-1)
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ k = kchunk*4
   s" add.u32 %r23,%r9,%r21;" PTX-L
   SB-RESET s" mad.lo.u32 %r23,%r23,%r3,%r" SB-APPEND ktr SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd1,%rd10;" PTX-L
   23 21 MMA-AROW-B MMA-SCALE                                                           \ %r23 = row*AROW-B
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ kchunk*16
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;
: MMA-CPW-CHUNK-B ( n n n -- ) {: m:n bufr:n ktr:n :}   \ one Bs 16B chunk, chunk-set m
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   s" shr.u32 %r21,%r20,4;" PTX-L                                                       \ k = c>>4
   s" and.b32 %r22,%r20,15;" PTX-L  s" shl.b32 %r22,%r22,2;" PTX-L                      \ col = (c&15)*4
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r21;" SB-APPEND SB$ PTX-L
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L
   s" shl.b32 %r23,%r20,4;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;

\ TRANSPOSED-Bs staging (dot habu-mma-wave-3): one scalar element BT[n][k] = B[ktr+k][colBase+n], with
\ c = tid_lin + m*256, n = c&63, k = c>>6. Global read is coalesced (a warp's 32 lanes -> 32 contiguous
\ n = 128 B), the shared write is strided (BT n-major, row stride BTROW-B). cp.async CANNOT do the
\ transpose (a contiguous 16 B chunk would scatter across BT rows), so this is a scalar copy; the B
\ tile is tiny (64*BK) and reused across all MFRAGS M-frags, so the extra stores are amortized. Uses
\ only prefetch scratch %r20..23 / %rd10..11 (invariants %r24..34 survive; the loaded value rides %r20
\ after c is dead).
: MMA-CPW-CHUNK-BT ( n n n -- ) {: m:n bufr:n ktr:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   s" and.b32 %r21,%r20,63;" PTX-L                                                      \ n = c & 63
   s" shr.u32 %r22,%r20,6;" PTX-L                                                       \ k = c >> 6
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r22;" SB-APPEND SB$ PTX-L        \ ktr + k
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L                                             \ (ktr+k)*N + colBase
   s" add.u32 %r23,%r23,%r21;" PTX-L                                                    \ + n
   s" mul.wide.u32 %rd10,%r23,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L
   s" ld.global.b32 %r20,[%rd11];" PTX-L                                                \ B[ktr+k][colBase+n] (c dead)
   23 21 MMA-BTROW-B MMA-SCALE                                                          \ %r23 = n * BTROW-B
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ k*4
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L      \ + Bs byte offset
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L       \ + buffer base
   s" st.shared.b32 [%r23],%r20;" PTX-L ;

: MMA-CPW-STAGE ( n n -- ) {: bufr:n ktr:n :}
   MMA-ACPN 0 do  i bufr ktr MMA-CPW-CHUNK-A  loop
   MMA-BLDM @ if
      MMA-BTCPN 0 do  i bufr ktr MMA-CPW-CHUNK-BT  loop
   else
      MMA-BCPN 0 do  i bufr ktr MMA-CPW-CHUNK-B  loop
   then ;

: MMA-CP-STAGE ( n n -- ) {: bufr:n ktr:n :}   \ stage one K-tile (As+Bs) into buffer bufr from column ktr
   MMA-MFRAGS @ 1 > if bufr ktr MMA-CPW-STAGE exit then
   MMA-CPN 0 do  i bufr ktr MMA-CP-CHUNK  loop ;

\ single-buffer cp.async ISSUE, the audited mint core (dot habu-wire-cppslot-typestate-
\ ce2463df): emits the As/Bs stage for this iteration's ONE slot (MMA-CP-STAGE verbatim)
\ and mints that slot's cpp-pending witness - the CPPSLOT protocol entry. Trusted only
\ for the phantom mint: a checked word cannot fabricate the nominal family cell
\ (`( n -- cpp-pending<p> ) 0` rejects), the same audited-mint-core class as the
\ NP-MINT-CHECK-sealed tile mints and the CPPSLOT COMMIT/WAIT transitions. The
\ commit->wait->read ORDERING this slot enters is checked at the caller
\ (MMA-PIPE-KLOOP-SINGLE); misorderings reject (lib/ptx/cg-mma-slot-neg-test.f).
TRUSTED: MMA-STAGE-ISSUE ( n n -- cpp-pending<p> )   MMA-CP-STAGE 0 ;

\ double-buffered (MMA-STAGES=2) cp.async pipeline, BK-parameterized (mirror of MM-PIPE-KLOOP-WITH).
\ The compute quotation sits on the data stack from entry to its `execute` slot (as in MM-PIPE).
\ Composed from the shared CPP-* protocol steps (cg-matmul-emit.f); MMA-CP-STAGE is the As/Bs
\ (or transposed-Bs) stage-issue, MMA-BUFB / MMA-BK @ carry the BK/pad/stage-parameterized bytes.
: MMA-PIPE-KLOOP-WITH ( [ -- ] -- )
   CPP-KT-INIT  CPP-PARITY-INIT
   11 14 MMA-CP-STAGE  CPP-COMMIT
   CPP-KGUARD
   MMA-BUFB CPP-CUR-WINDOW
   MMA-BK @ CPP-KT-NEXT  CPP-PF-TEST
   MMA-BUFB CPP-NEXT-WINDOW  18 17 MMA-CP-STAGE
   CPP-COMMIT  1 CPP-WAIT
   CPP-PF-ELSE
   0 CPP-WAIT
   CPP-PF-END
   CPP-SYNC  execute  CPP-SYNC
   MMA-BK @ CPP-KT-ADVANCE  CPP-FLIP
   CPP-KTAIL ;

\ single-buffer (MMA-STAGES=1) K-loop: stage, drain, compute, reuse. Fewer bar.sync per K than
\ BK=32 (bigger tile) but no cp.async/compute overlap; fits the 48 KiB static cap at BK=64.
\ Single read-window = SH, no parity/prefetch. The per-iteration protocol is CHECKED
\ (dot habu-wire-cppslot-typestate-ce2463df): MMA-STAGE-ISSUE mints this iteration's
\ cpp-pending slot at the cp.async issue, and CPPSLOT COMMIT -> WAIT -> READ thread the
\ slot typestate so a misordered emission (wait-before-commit, dropped wait/sync fence,
\ read-before-wait) is a checker reject, not silent bad PTX. Byte-identical to the
\ former fused steps: WAIT emits `wait_group 0` + `bar.sync` (the pre-compute fence);
\ the trailing CPP-SYNC is the buffer-reuse fence after compute.
: MMA-PIPE-KLOOP-SINGLE ( [ -- ] -- )
   CPP-KT-INIT
   CPP-SINGLE-WINDOW                                              \ single buffer base = SH
   CPP-KGUARD
   11 14 MMA-STAGE-ISSUE  CPPSLOT:COMMIT  CPPSLOT:WAIT  CPPSLOT:READ
   execute  CPP-SYNC
   MMA-BK @ CPP-KT-ADVANCE
   CPP-KTAIL ;

\ ============ N-stage cp.async software pipeline (MMA-STAGES=N, N>=3) =================
\ dot habu-4-warp-mma step 3. The double-buffer above overlaps ONE prefetch with compute; deeper
\ staging (Triton's per-shape winners run 3-5) hides more of the cp.async latency, and the narrower
\ 4-warp tile's halved smem footprint is what makes 3+ full buffers fit under the GB10 99 KB cap while
\ keeping >=2 blocks/SM. This is the standard multistage GEMM pipeline over a RING of N smem buffers
\ (base = SH + stage*BUFB, stage cycled as a byte pointer that wraps at SH+N*BUFB back to SH):
\   prologue : issue tiles 0..N-2 (N-1 groups), one commit_group each (guarded by kt<K for a short K).
\   steady   : while a tile remains to PREFETCH (kt_pf<K), prefetch tile kt_pf into the write buffer,
\              commit, then wait_group(N-1) - this keeps the N-1 most-recent groups in flight and so
\              GUARANTEES the oldest (the tile about to be computed) has landed - then bar.sync, compute
\              from the read buffer, bar.sync (buffer-reuse fence: the read buffer is overwritten N-1
\              iterations later), advance both ring bases and kt.
\   epilogue : the last N-1 tiles have no more prefetch, so the in-flight group count must be drained
\              one at a time: compute tile j with wait_group(N-2-j) for j=0..N-2 (so N-2,N-3,...,0 - the
\              last tile drains fully). The compute is guarded by kt_cmp<K so a K with fewer than N-1
\              tiles (T<N-1) simply computes the tiles that exist. Requires the CHECKED/timed K to give
\              T=ceil(K/BK) >= N-1 for the wait_group literals to be exact (the deep-stage harness rows
\              check at K big enough); a too-small K is proven wrong by mma-gemm-check, never reported.
\ At N=2 these literals reduce to the double-buffer's wait_group(1)/(0), so MMA-KLOOP keeps the pinned
\ stages=1/2 configs on the byte-identical SINGLE/WITH scaffolds and routes only N>=3 here.
\ cyclically advance ring base %rR by one buffer (BUFB bytes), wrapping at %r15 (=SH+N*BUFB) back to SH.
: MMA-RING-ADV ( n -- ) {: r:n :}
   SB-RESET s" add.u32 %r" SB-APPEND r SB-U s" ,%r" SB-APPEND r SB-U s" ," SB-APPEND MMA-BUFB SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" setp.eq.u32 %p3,%r" SB-APPEND r SB-U s" ,%r15;" SB-APPEND SB$ PTX-L
   SB-RESET s" @%p3 mov.u32 %r" SB-APPEND r SB-U s" ,%r11;" SB-APPEND SB$ PTX-L ;
: MMA-KT-ADD ( n -- )                                            \ %rR += BK  (R on stack as reg number)
   {: r:n :}  SB-RESET s" add.u32 %r" SB-APPEND r SB-U s" ,%r" SB-APPEND r SB-U s" ," SB-APPEND MMA-BK @ SB-U s" ;" SB-APPEND SB$ PTX-L ;

\ Compute for one staged K-tile from read-buffer base %r16 (same as the quotation the double-buffer runs):
\ the wide (MFRAGS>1) or non-wide K-tile. Called by name (not a quotation) so the multistage can emit it
\ N times - once in the runtime steady body and once per unrolled epilogue tile.
: MMA-KTILE-DISPATCH ( -- )  MMA-MFRAGS @ 1 > if MMA-KTILE-WIDE else MMA-KTILE then ;

: MMA-PIPE-KLOOP-MULTI ( -- )
   s" mov.u32 %r14,0;" PTX-L  s" mov.u32 %r17,0;" PTX-L           \ kt_pf = kt_cmp = 0
   s" mov.u32 %r16,%r11;" PTX-L  s" mov.u32 %r18,%r11;" PTX-L     \ read base = write base = SH
   s" mov.u32 %r15,%r11;" PTX-L                                   \ ring wrap bound = SH + N*BUFB
   SB-RESET s" add.u32 %r15,%r15," SB-APPEND MMA-SMEM SB-U s" ;" SB-APPEND SB$ PTX-L
   MMA-STAGES @ 1- 0 do                                          \ prologue: issue tiles 0..N-2 into buffers 0..N-2
      s" setp.ge.u32 %p1,%r14,%r3;" PTX-L
      SB-RESET s" @%p1 bra $PLSKIP" SB-APPEND i SB-U s" ;" SB-APPEND SB$ PTX-L
      18 14 MMA-CP-STAGE
      SB-RESET s" $PLSKIP" SB-APPEND i SB-U s" :" SB-APPEND SB$ PTX-L
      CPP-COMMIT
      18 MMA-RING-ADV                                            \ next write buffer
      14 MMA-KT-ADD                                              \ kt_pf += BK
   loop
   s" $MSTEADY:" PTX-L                                           \ steady loop: while kt_pf < K
   s" setp.ge.u32 %p1,%r14,%r3;" PTX-L  s" @%p1 bra $MSEND;" PTX-L
   18 14 MMA-CP-STAGE  CPP-COMMIT                                \ prefetch tile kt_pf -> write buffer
   MMA-STAGES @ 1- CPP-WAIT                                      \ wait_group(N-1): oldest (compute tile) has landed
   CPP-SYNC  MMA-KTILE-DISPATCH  CPP-SYNC                        \ compute from read buffer, then reuse fence
   16 MMA-RING-ADV  18 MMA-RING-ADV                             \ advance read + write bases
   14 MMA-KT-ADD  17 MMA-KT-ADD                                 \ kt_pf += BK ; kt_cmp += BK
   s" bra $MSTEADY;" PTX-L  s" $MSEND:" PTX-L
   MMA-STAGES @ 1- 0 do                                          \ epilogue: last N-1 tiles, draining wait_group(N-2..0)
      MMA-STAGES @ 2 - i - CPP-WAIT                              \ wait_group(N-2-i)
      CPP-SYNC
      s" setp.ge.u32 %p1,%r17,%r3;" PTX-L                        \ guard: compute only tiles that exist (T<N-1 short K)
      SB-RESET s" @%p1 bra $EPSKIP" SB-APPEND i SB-U s" ;" SB-APPEND SB$ PTX-L
      MMA-KTILE-DISPATCH
      SB-RESET s" $EPSKIP" SB-APPEND i SB-U s" :" SB-APPEND SB$ PTX-L
      CPP-SYNC
      16 MMA-RING-ADV  17 MMA-KT-ADD
   loop ;

: MMA-KLOOP ( [ -- ] -- )                                          \ stages 1/2 pipeline (quotation-based); N>=3 via MMA-BODY
   MMA-DEFAULT? if MM-PIPE-KLOOP-WITH exit then
   MMA-STAGES @ 1 = if MMA-PIPE-KLOOP-SINGLE else MMA-PIPE-KLOOP-WITH then ;

: MMA-BODY ( -- )
   MMA-CHECK-SMEM
   MMA-CHECK-BLDM
   MMA-CHECK-WARPS
   MMA-MFRAGS @ 1 > if
      MMA-THREAD-SETUP-WIDE  MMA-ACC-ZERO-WIDE  MMA-SETUP-WIDE
      MMA-STAGES @ 2 > if MMA-PIPE-KLOOP-MULTI else [: MMA-KTILE-WIDE ;] MMA-KLOOP then
      MMA-STORE-WIDE  exit
   then
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MMA-SETUP
   MMA-STAGES @ 2 > if MMA-PIPE-KLOOP-MULTI else [: MMA-KTILE ;] MMA-KLOOP then
   MMA-STORE ;

: EMIT-MATMUL-MMA ( -- )
   PTX-HEADER  PTX-NL
   MMA-DYNSMEM @ if
      s" .extern .shared .align 16 .b8 SH[];" PTX-L        \ module-scope dynamic .shared (sized at launch)
   then
   s" .visible .entry MMM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L
   SB-RESET s" .reg .f32 %f<" SB-APPEND MMA-FREGS SB-U s" >;" SB-APPEND SB$ PTX-L   \ 48 at MFRAGS<=2 (byte-identical)
   SB-RESET s" .reg .b32 %r<" SB-APPEND MMA-RREGS SB-U s" >;" SB-APPEND SB$ PTX-L   \ 64 at MFRAGS<=2 (byte-identical)
   s" .reg .b64 %rd<48>;" PTX-L
   MMA-DYNSMEM @ 0= if
      SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MMA-SMEM SB-U s" ];" SB-APPEND SB$ PTX-L
   then
   MM-PARAMS
   MMA-BODY
   s" ret;" PTX-L  s" }" PTX-L ;
