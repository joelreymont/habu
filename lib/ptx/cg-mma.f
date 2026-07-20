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
2  constant MMA-WCOLS                           \ warp columns per block (FIXED: warp_col selects one of two BN/WCOLS-col halves)
8  constant MMA-MK                              \ mma.sync K per substep (m16n8k8)
49152 constant MMA-SMEM-STATIC-CAP              \ sm_87 static .shared per-block ceiling (48 KiB)
101376 constant MMA-SMEM-DYN-CAP                \ GB10 opt-in dynamic .shared per-block ceiling (99 KB)
-6100 constant E-MMA-SMEM                        \ derived shared tile exceeds the legal budget
-6102 constant E-MMA-BLDM                        \ B-ldmatrix config illegal (non-16B BT row, or MFRAGS=1)
-6103 constant E-MMA-WARPS                        \ illegal warp grid (WARPS not 4/8, or WARPS=4 without the wide MFRAGS>1 staging)
-6104 constant E-MMA-EPI                          \ smem epilogue staging tile exceeds the per-block .shared budget
-6105 constant E-MMA-DTYPE                         \ half dtype (fp16/bf16) with an un-wired feed knob (LMODE=1 / tf32 B-ldmatrix / ablate / LMODE=2+BTF16 / LMODE=2 BN>64)
-6106 constant E-MMA-BTF16                         \ transposed-Bs feed illegal (requested on a tf32 tile, or a non-4B BT row)
-6107 constant E-MMA-BN                            \ output-tile width BN illegal (not a power of two, or below the legacy 64)
-6108 constant E-MMA-REGS                          \ per-lane accumulators bust the 255-register file ceiling for this (BN,MFRAGS)
-6109 constant E-MMA-GROUP                          \ grouped-raster group height illegal (negative M-block group height)
-6111 constant E-MMA-XSWIZ                          \ XOR-swizzle config illegal (non-zero pad, non-ldmatrix A feed, half dtype, BK out of [32,64], or ablate)

255 constant MMA-REG-CEIL                       \ usable per-thread hardware register file (sm_121 255-register ceiling)
32  constant MMA-REG-WORKSET                    \ non-accumulator live working set (measured 30 at BN256/MFRAGS2/128 accs -> 158 regs, rounded up)

\ OUTPUT-TILE WIDTH BN (dot habu-widen-bn-past). The tile's N span. Legacy 64; widened to 128/256 so a warp
\ owns MMA-NTILES = BN/(WCOLS*8) 8-column n-tiles per warp-col half (BN/2 cols), giving B-fragment reuse per
\ load. A power of two >= 64 (the epilogue-drain row/col and the Bs cp.async chunk partition use shift/mask,
\ so BN must be a power of two; below 64 the legacy non-wide path is BN=64-hardwired). BN>64 routes through
\ the WIDE compute + split-staging path (MMA-WIDE?); BN=64 stays byte-identical. Every non-BN knob (MFRAGS,
\ WARPS, BK, PAD, stages, dyn, epilogue, dtype) works across BN; the transposed-Bs feeds (BLDM/BTF16) stage
\ n = c&63 (BN=64-hardwired) so they fail closed above 64.
\
\ REGISTER BUDGET (per-lane f32 accumulators = MFRAGS*NTILES*4; est regs = accs + MMA-REG-WORKSET; the
\ feasibility gate MMA-CHECK-REGS throws E-MMA-REGS when est > MMA-REG-CEIL). WARPS-independent: each lane
\ owns MFRAGS M-frags x NTILES n-tiles regardless of the warp count (WARPS tiles M, not per-lane work).
\   BN   NTILES | MFRAGS=1  MFRAGS=2  MFRAGS=4
\   ---- ------ | -------   --------  --------------
\    64    4    |  16 acc   32 acc    64 acc          all feasible (legacy family)
\   128    8    |  32 acc   64 acc    128 acc (~160)  all feasible
\   256   16    |  64 acc   128 acc   256 acc (~288)  MFRAGS=4 INFEASIBLE -> E-MMA-REGS
\ Triton's BN256 winner is 8-warp MFRAGS=2 NTILES=16 = 128 acc ~158 regs, 0 spills (docs/eval-triton.md GB10
\ sweep: 2048 winner BM64xBN128, 4096 winner BM128xBN256); the MFRAGS=4 BN=256 corner (256 acc) cannot even
\ hold its accumulators under the 255 ceiling, so it fails closed at emit time (MGC-REGS-NEG).
variable MMA-BN      64 MMA-BN !               \ output tile cols KNOB (64 default / 128 / 256; power of two, >= 64)

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

\ SHARED-MEMORY C EPILOGUE (dot habu-shared-mem-epilogue). Default OFF keeps the scattered
\ st.global.f32 store (MMA-STORE / MMA-STORE-WIDE) BYTE-IDENTICAL. When ON, after the K-loop each
\ lane writes its 16*MFRAGS D-fragment accumulators into a block-local [BROWS][BN] staging tile in
\ shared memory (the SAME SH region the cp.async pipeline used - dead after the last compute), then
\ the whole block re-reads the tile and writes C in coalesced 128-byte lines (element e = tid_lin +
\ m*NTHREADS -> row e/BN col e%BN, so a warp's 32 lanes hit 32 contiguous C columns). This trades the
\ store's uncoalesced 4-byte global writes for one strided smem write + one coalesced global write,
\ paying two block barriers. The staging tile is BROWS*BN*4 bytes; SH is sized to the LARGER of the
\ pipeline and the staging tile (MMA-SH-BYTES), so a tile whose staging busts the .shared budget
\ throws E-MMA-EPI at emit time (MMA-CHECK-EPI). The lane->element map is the D-fragment map already
\ proven element-exact by mma-gemm-check, so no new mapping is introduced.
variable MMA-EPILOG   0 MMA-EPILOG !          \ 1 = shared-memory coalesced C epilogue (off by default)

\ XOR-SWIZZLE As shared layout (dot habu-xor-swizzle-mma). A TRUE address-bit swizzle that replaces the
\ MMA-PAD row-stride padding as the ldmatrix-A bank-conflict remedy at ZERO extra shared bytes. MMA-PAD=8
\ pads each As row BK->BK+8 words so the 8 ldmatrix.x4 fragment rows land on 8 distinct 4-bank windows (160B
\ stride, +8 banks/row) - conflict-free, but 8 words/row * BROWS rows * 4 B of dead shared. MMA-XSWIZ keeps
\ the row stride at BK (pad-free) and instead permutes the 16-byte K-chunk position WITHIN each row by
\ chunk' = chunk XOR (row & (ACPR-1)), i.e. address ^= (row & (ACPR-1))<<4 (ACPR = BK/EPC chunks/row). The 8
\ rows of an ldmatrix matrix (same K-column, consecutive As rows) then map to 8 distinct chunks -> 8 distinct
\ 4-bank windows -> conflict-free, same as the pad but with NO padding. Applied IDENTICALLY on the cp.async
\ store (MMA-CP-CHUNK / MMA-CPW-CHUNK-A) and the ldmatrix-A load (MMA-A-LDM / -WIDE), so it is a pure
\ permutation of As storage: correctness is a relabeling of where each element sits and is proven element-
\ exact by mma-gemm-check. Bank-freedom needs the full row available (ACPR>=8, i.e. BK>=32) and the m-frag
\ stride not to disturb the mask (ACPR|16, i.e. BK<=64); MMA-CHECK-XSWIZ fails closed outside [32,64] and on
\ every combo whose store side is not swizzled (non-zero pad, non-ldmatrix A feed LMODE!=2, half dtype whose
\ As store is the F16 word, and wide ablation). Composes with
\ MFRAGS / WARPS / BN / stages / dyn / B-ldmatrix / grouped-raster / epilogue (all leave the As feed alone).
variable MMA-XSWIZ    0 MMA-XSWIZ !           \ 1 = pad-free XOR-swizzled As shared layout (off by default)

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

\ TENSOR DTYPE (dot habu-fp16-mma-tile / habu-bf16-m16n8k16-tile). 0 = TF32 (m16n8k8 f32.tf32.tf32.f32;
\ f32-in, tf32-mma, f32-acc) - the DEFAULT, BYTE-IDENTICAL to every pinned config. 1 = FP16 (m16n8k16
\ f32.f16.f16.f32): A/B are f16 halves in BOTH global and shared (host packs f32->f16 on the fill
\ path), accumulate stays f32, C stays f32. The m16n8k16 shape retires TWICE the K per mma - each
\ lane holds 8 A-halves (4 packed .f16x2 b32 regs) and 4 B-halves (2 b32 regs), the SAME 4+2 b32
\ register budget as tf32's 4+2 - so MMA-KSUBS halves (MMA-MKD=16). 2 = BF16 (m16n8k16 f32.bf16.bf16.f32):
\ the SAME m16n8k16 shape, fragment maps, staging and half-precision (2-byte element) geometry as fp16 -
\ a bf16 half is 2 bytes exactly like an f16 half, so every load/stage is a pure bit-move and only the mma
\ dtype token (MMA-ABT) and the host pack differ (F64>BF16 round-to-nearest-even, lib/ptx/cg.f). fp16 AND
\ bf16 (jointly MMA-HALF?) feed A/B two ways (dot habu-half-precision-ldmatrix): LMODE=0 scalar packed-b32
\ (default) - A four ld.shared.b32, B either k-major (two ld.shared.u16 + shift/or per register) or transposed
\ n-major MMA-BTF16 (one ld.shared.b32); or LMODE=2 LDMATRIX - ONE ldmatrix.sync.aligned.m8n8.x4.b16 fills the
\ four A registers and ONE ldmatrix.x2.trans.b16 fills the two B registers straight from the k-major As/Bs in
\ the mma-native layout (.trans is legal for a half because the element IS a b16, unlike tf32). The half
\ ldmatrix is wired at BN=64 (the mid/large MFRAGS tiles); LMODE=1 (tf32 cvt-drop), tf32 B-ldmatrix (BLDM),
\ ablation, LMODE=2+MMA-BTF16, and LMODE=2 at BN>64 are NOT half paths and MMA-CHECK-DTYPE fails closed on
\ them. Every non-dtype knob (MFRAGS, WARPS, BK, PAD, stages, dyn, epilogue) works for all three dtypes.
variable MMA-DTYPE   0 MMA-DTYPE !            \ 0 = tf32 (default), 1 = fp16, 2 = bf16
: MMA-F16? ( -- bool )  MMA-DTYPE @ 1 = ;
: MMA-BF16? ( -- bool )  MMA-DTYPE @ 2 = ;
: MMA-HALF? ( -- bool )  MMA-F16? MMA-BF16? or ;      \ 16-bit half dtype (fp16 or bf16): shared geometry/staging/feed
: MMA-ABT  ( -- ptr u8 n )  MMA-BF16? if s" bf16" else s" f16" then ;   \ mma A/B operand dtype token

\ TRANSPOSED-Bs fp16 B feed (dot habu-fp16-transposed-bs). The default fp16 B fragment builds each of
\ its two b32 registers from two ld.shared.u16 + shift/or, because the register's two K-adjacent halves
\ are one BN-row apart in the k-major Bs - so the per-K B-feed instruction count stays at tf32 levels even
\ though the mma count halved. Storing Bs TRANSPOSED (n-major BT[n][k], K contiguous) makes each register's
\ K-adjacent pair CONTIGUOUS, so the whole register loads as ONE ld.shared.b32 - dropping the shift/or pair,
\ mirroring the tf32 wave-3 transposed-Bs feed (MMA-BLDM). cp.async CANNOT do the transpose (a contiguous
\ chunk would scatter across BT rows), so the BT tile is a scalar TRANSPOSED copy (coalesced global read
\ B[k][n], strided shared write BT[n][k]), the As tile staying a cp.async copy - exactly the split staging
\ the tf32 BLDM path uses. The n-major BT row stride is BK+MMA-BPAD halves (MMA-BTROW-B), a NEW bank
\ geometry, so MMA-BPAD is a measured knob (BPAD=8 -> stride 40 halves = 80 B -> b32-load start-bank stride
\ 20, conflict-free 8-gid tiles; BPAD=0 fits tightest but aliases banks). Half-dtype-only (fp16 or bf16;
\ MMA-CHECK-BTF16 fails closed on a tf32 tile or a non-4B BT row) - the 2-byte half is transposed the same
\ way for either; works on both warp grids, all MFRAGS, with/without the epilogue.
variable MMA-BTF16   0 MMA-BTF16 !            \ 1 = fp16/bf16 transposed-Bs B feed (one b32 load per B register)

\ GROUPED-RASTER CTA ORDERING (dot habu-grouped-raster-cta). Emit-time knob: 0 = OFF, byte-identical
\ (naive row-major launch, no remap emitted); a POSITIVE value = the group height in M-blocks. When on,
\ the prologue remaps the natural launch ids (ctaid.x = tile_n, ctaid.y = tile_m) Triton GROUP_M-style so
\ CONCURRENTLY RESIDENT CTAs share A-row / B-col tiles in L2 - only the two id registers change (rowBase /
\ colBase derive from them), no smem / register / schedule cost. linear = ctaid.y*gridN + ctaid.x; group =
\ linear/(GROUP*gridN); within a group COLUMN-MAJOR (tile_m varies fastest); the last group clamps its
\ height (MMA-GRID-REMAP, whose constraint comment proves the clamp). The remap arithmetic is general
\ div.u32 / rem.u32 (NOT shift/mask), so GROUP is unrestricted to any positive integer - a power of two is
\ not required. Only a NEGATIVE height is illegal (MMA-CHECK-GROUP throws E-MMA-GROUP); 0 is the OFF sentinel.
variable MMA-GROUP   0 MMA-GROUP !            \ 0 = OFF (byte-identical); positive = grouped-raster group height in M-blocks

: MMA-ESZ  ( -- n )  MMA-HALF? if 2 else 4 then ;    \ A/B element bytes (fp16/bf16 half vs tf32 f32 word)
: MMA-EPC  ( -- n )  16 MMA-ESZ / ;                  \ A/B elements per 16-byte cp.async chunk (8 half / 4 f32)
: MMA-MKD  ( -- n )  MMA-HALF? if 16 else MMA-MK then ;  \ mma.sync K/substep (m16n8k16 half / m16n8k8 tf32)

: MMA-WROWS  ( -- n )  MMA-WARPS @ 2 / ;              \ warp-rows (WCOLS fixed 2); 4 at WARPS=8, 2 at WARPS=4
: MMA-NTHREADS ( -- n )  MMA-WARPS @ 32 * ;           \ threads/block; 256 at WARPS=8, 128 at WARPS=4
: MMA-BROWS  ( -- n )  MMA-WROWS 16 * MMA-MFRAGS @ * ;  \ output block rows = WROWS*16*MFRAGS (64*MFRAGS at WARPS=8)
: MMA-AROW-F ( -- n )  MMA-BK @ MMA-PAD @ + ;         \ As row stride, elements
: MMA-AROW-B ( -- n )  MMA-AROW-F MMA-ESZ * ;         \ As row stride, bytes (tf32 default 128; fp16 halves)
: MMA-ASB    ( -- n )  MMA-BROWS MMA-AROW-B * ;       \ As tile bytes / Bs byte offset (default 8192)
: MMA-BTROW-F ( -- n )  MMA-BK @ MMA-BPAD @ + ;       \ transposed-Bs (BT) row stride, elements (n-major over k)
: MMA-BTROW-B ( -- n )  MMA-BTROW-F MMA-ESZ * ;       \ BT row stride, bytes (tf32 *4: mult of 16 for ldmatrix rows; fp16 *2)
: MMA-BT?    ( -- bool )  MMA-BLDM @ 0= 0=  MMA-BTF16 @ 0= 0=  or ; \ Bs stored TRANSPOSED (tf32 B-ldmatrix or fp16 transposed feed)
: MMA-BSB    ( -- n )  MMA-BT? if MMA-BN @ MMA-BTROW-F * MMA-ESZ * else MMA-BK @ MMA-BN @ * MMA-ESZ * then ;  \ B tile bytes (BT if transposed; else dtype-sized)
: MMA-BTCPN  ( -- n )  MMA-BN @ MMA-BK @ * MMA-NTHREADS / ;   \ transposed-B scalar chunk-sets/thread (64*BK/NTHREADS; BLDM is BN=64)
: MMA-BUFB   ( -- n )  MMA-ASB MMA-BSB + ;            \ one cp.async buffer (default 16384)
: MMA-SMEM   ( -- n )  MMA-BUFB MMA-STAGES @ * ;      \ total pipeline shared bytes (default 32768)
: MMA-EPI-BYTES ( -- n )  MMA-EPILOG @ if MMA-BROWS MMA-BN @ 4 * * else 0 then ;  \ epilogue staging tile bytes (BROWS*BN*4), 0 when off
: MMA-SH-BYTES  ( -- n )  MMA-SMEM MMA-EPI-BYTES max ; \ actual SH allocation = larger of pipeline / staging (= MMA-SMEM when epilogue off)
: MMA-KSUBS  ( -- n )  MMA-BK @ MMA-MKD / ;           \ mma.sync K substeps per tile (tf32 4, fp16 2 at BK=32)
: MMA-ACPR   ( -- n )  MMA-BK @ MMA-EPC / ;           \ As cp.async chunks per row (tf32 8, fp16 4 at BK=32)
: MMA-XMASK  ( -- n )  MMA-ACPR 1- ;                  \ XOR-swizzle chunk mask: row & (ACPR-1) permutes chunks within a row (7 at BK=32)
: MMA-CPN    ( -- n )  MMA-BM MMA-BK @ * MMA-EPC / MMA-NTHREADS / ;  \ MFRAGS=1 cp.async chunk-sets/thread per array (default 2)
: MMA-ACPN   ( -- n )  MMA-BROWS MMA-BK @ * MMA-EPC / MMA-NTHREADS / ; \ wide As cp.async chunk-sets/thread (BROWS!=BN)
: MMA-BCPN   ( -- n )  MMA-BK @ MMA-BN @ * MMA-EPC / MMA-NTHREADS / ;  \ wide Bs cp.async chunk-sets/thread
: MMA-BCPR   ( -- n )  MMA-BN @ MMA-EPC / ;           \ Bs cp.async 16B chunks per Bs row (BN/EPC: tf32 16, fp16 8 at BN=64)
: MMA-NTILES ( -- n )  MMA-BN @ MMA-WCOLS 8 * / ;     \ 8-col n-tiles per warp-col half = BN/(WCOLS*8); 4 at BN=64
: MMA-ACCS   ( -- n )  MMA-MFRAGS @ MMA-NTILES 4 * * ; \ per-lane f32 accumulators = MFRAGS*NTILES*4; 16*MFRAGS at BN=64
: MMA-AREG   ( n -- n )  6 * 50 + ;                   \ tf32 A-fragment reg group base for M-frag f
\ Register-pool sizing (dot habu-mma-wave-2, generalized for wide BN by dot habu-widen-bn-past). The mode-0
\ wide cvt temps and the header .reg .f32 count must grow past the MMA-ACCS accumulators (MFRAGS*NTILES*4)
\ for a wider M OR N tile, but MUST stay BYTE-IDENTICAL at the legacy BN=64 family. At BN=64 MMA-ACCS =
\ 16*MFRAGS, so MMA-FTEMP = 16*MFRAGS+10 (=42 at MFRAGS=2, the former %f42..47) and MMA-FREGS = max(48,
\ 16*MFRAGS+16) reproduces the former MFRAGS<=2 -> 48, MFRAGS>2 -> 16*MFRAGS+16 exactly. The 6 f-temps
\ (4 A-cvt + 2 B-cvt) live at FTEMP..FTEMP+5 = ACCS+10..ACCS+15, so FREGS = ACCS+16 always covers them.
\ MMA-RREGS is BN-INDEPENDENT: the A groups (6*MFRAGS b32) reuse across n-tiles and the B fragment reloads
\ into the same %r54,55 per n-tile, so no b32 register scales with NTILES; the former MFRAGS-only form stands.
: MMA-FTEMP  ( -- n )  MMA-ACCS 10 + ;                \ wide cvt temp base (just past the accumulators)
: MMA-FREGS  ( -- n )  MMA-ACCS 16 + 48 max ;         \ .reg .f32 pool = accumulators + 6 temps, clamped to the legacy 48
: MMA-RREGS  ( -- n )  MMA-MFRAGS @ 2 > if 6 MMA-MFRAGS @ * 48 + else 64 then ;    \ .reg .b32 pool (BN-independent)
: MMA-DEFAULT? ( -- bool )                             \ the byte-identical baseline config (8-warp tf32 BN=64 only)
   MMA-BK @ 32 =  MMA-PAD @ 0=  and  MMA-STAGES @ 2 =  and  MMA-DYNSMEM @ 0=  and
   MMA-MFRAGS @ 1 =  and  MMA-WARPS @ 8 =  and  MMA-DTYPE @ 0=  and  MMA-BN @ 64 =  and
   MMA-XSWIZ @ 0=  and ;   \ BN>64 uses the MMA-owned pipe, not the shared MM-PIPE scaffold; XSWIZ needs the swizzle-aware MMA-owned staging (MM-PIPE has no swizzle)
: MMA-WIDE?  ( -- bool )  MMA-MFRAGS @ 1 >  MMA-BN @ 64 >  or ;   \ WIDE compute + split-staging path (MFRAGS>1 or BN>64); BN=64 MFRAGS=1 stays the byte-identical non-wide path

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
   MMA-BN @ 64 > if E-MMA-BLDM throw then               \ transposed-Bs staging stages n = c&63 (BN=64-hardwired); not wired for wide BN
   MMA-BTROW-B 15 and 0= 0= if E-MMA-BLDM throw then ;  \ BT row stride not a multiple of 16 B -> misaligned ldmatrix rows

\ fail closed on an illegal transposed-Bs half-dtype feed (dot habu-fp16-transposed-bs / habu-bf16-m16n8k16-
\ tile). The n-major BT feed loads each B register with ONE ld.shared.b32 over two K-adjacent halves, so it
\ is defined ONLY for a half dtype (fp16 or bf16; the tf32 element is a whole f32 word, not a K-pair) and the
\ BT row stride MMA-BTROW-B = (BK+BPAD)*2 MUST be a multiple of 4 B (else the b32 load start is mis-aligned).
\ Reject both at EMIT time so a bad knob combination throws instead of silently emitting the wrong kernel or
\ faulting the GPU.
: MMA-CHECK-BTF16 ( -- )
   MMA-BTF16 @ 0= if exit then
   MMA-HALF? 0= if E-MMA-BTF16 throw then              \ transposed-Bs feed is the fp16/bf16 half B path only
   MMA-BN @ 64 > if E-MMA-BTF16 throw then             \ transposed-Bs staging stages n = c&63 (BN=64-hardwired); not wired for wide BN
   MMA-BTROW-B 3 and 0= 0= if E-MMA-BTF16 throw then ; \ BT row stride not a multiple of 4 B -> misaligned b32 B load

\ fail closed on an illegal warp grid (dot habu-4-warp-mma). Only the 4x2 (WARPS=8) and 2x2 (WARPS=4)
\ grids are implemented (WCOLS fixed 2). The narrower 4-warp grid stages its As over MMA-BROWS rows
\ (WROWS*16*MFRAGS), so it needs the WIDE (MFRAGS>1) cp.async path; the non-wide MFRAGS=1 staging is
\ hardwired to the 64-row 8-warp tile (MMA-CPN uses MMA-BM), so WARPS=4 + MFRAGS=1 would emit a kernel
\ whose 128 threads stage a 64-row As but compute only 32 rows. Reject both at emit time.
: MMA-CHECK-WARPS ( -- )
   MMA-WARPS @ 8 =  MMA-WARPS @ 4 =  or  0= if E-MMA-WARPS throw then   \ only 4x2 / 2x2 grids
   MMA-WARPS @ 4 =  MMA-MFRAGS @ 1 =  and  if E-MMA-WARPS throw then ;  \ 4-warp needs the wide staging

\ fail closed on an epilogue whose staging tile busts the .shared budget (dot habu-shared-mem-epilogue).
\ The epilogue sizes SH to the larger of the pipeline and the BROWS*BN*4 staging tile (MMA-SH-BYTES); if
\ that exceeds the static 48 KiB cap (static tile) or the GB10 99 KB opt-in cap (dynamic tile) the launch
\ would be illegal, so reject at EMIT time instead of faulting the GPU. Off when the epilogue is disabled.
: MMA-CHECK-EPI ( -- )
   MMA-EPILOG @ 0= if exit then
   MMA-DYNSMEM @ if
      MMA-SH-BYTES MMA-SMEM-DYN-CAP > if E-MMA-EPI throw then
   else
      MMA-SH-BYTES MMA-SMEM-STATIC-CAP > if E-MMA-EPI throw then
   then ;

\ fail closed on an illegal output-tile width BN (dot habu-widen-bn-past). BN must be a power of two (the
\ epilogue-drain row=e>>log2(BN) / col=e&(BN-1) and the Bs cp.async chunk partition c/(BN/EPC) all use
\ shift/mask, exact only for a power-of-two BN) and >= 64 (below 64 the legacy non-wide path is BN=64-
\ hardwired; only BN=64 and the wide 128/256 are supported). Reject at EMIT time so a bad BN throws instead
\ of emitting a kernel whose shift/mask arithmetic disagrees with the tile geometry.
: MMA-CHECK-BN ( -- )
   MMA-BN @ 64 < if E-MMA-BN throw then                         \ below the legacy width -> non-wide path is BN=64-hardwired
   MMA-BN @ MMA-POW2? 0= if E-MMA-BN throw then ;               \ not a power of two -> drain/chunk shift+mask are wrong

\ fail closed on a (BN,MFRAGS) whose per-lane accumulators bust the register file (dot habu-widen-bn-past).
\ The MMA-ACCS f32 accumulators (MFRAGS*NTILES*4) are ALL live across the K-loop, so they set a hard floor on
\ physical registers; with the fixed non-accumulator working set (MMA-REG-WORKSET, measured 30 at the BN256
\ MFRAGS2 128-acc point -> 158 regs) the kernel must fit MMA-REG-CEIL (255). The BN=256 MFRAGS=4 corner is
\ 256 accumulators - it cannot even hold them under 255 - so it fails closed here instead of spilling under
\ ptxas at launch. WARPS-independent (accs do not depend on the warp count). No legacy config is rejected
\ (BN=64 max is MFRAGS=4 -> 64 accs -> 96 est).
: MMA-CHECK-REGS ( -- )
   MMA-ACCS MMA-REG-WORKSET +  MMA-REG-CEIL >  if E-MMA-REGS throw then ;

\ fail closed on an illegal grouped-raster group height (dot habu-grouped-raster-cta). A group height is a
\ COUNT of M-blocks, so it must be a positive integer; 0 is the OFF sentinel (no remap, byte-identical). A
\ NEGATIVE height is meaningless and, since MMA-GRID-REMAP feeds GROUP into u32 device arithmetic
\ (GROUP*gridN, min(.,GROUP)), a negative value reinterprets as a huge u32 that silently remaps every CTA to
\ garbage tiles - so reject it at EMIT time. No power-of-two / divisibility constraint: the remap uses general
\ div.u32 / rem.u32 (runtime divisors GROUP*gridN and the clamped group size, both proven >= 1), never
\ shift/mask, so any positive GROUP is legal arithmetic.
: MMA-CHECK-GROUP ( -- )
   MMA-GROUP @ 0 < if E-MMA-GROUP throw then ;

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

\ XOR-swizzle emit helpers (dot habu-xor-swizzle-mma). Guarded by MMA-XSWIZ; emit ZERO bytes when off, so
\ every OFF config stays byte-identical. See the MMA-XSWIZ knob header for the layout law.
\ SETUP: capture the loop-invariant swizzle term (ldm_row & (ACPR-1))<<4 into %r38, from the pre-scale ldm A
\ row in %r47. For 32<=BK<=64 the m-frag stride (16 rows) and the warp_row block are multiples of ACPR, so
\ row&(ACPR-1) is unchanged by them -> the term is invariant across the K-loop and every M-frag. %r38 has no
\ other user in the emitted kernel.
: MMA-XSWIZ-SETUP ( -- )   MMA-XSWIZ @ 0= if exit then
   SB-RESET s" and.b32 %r38,%r47," SB-APPEND MMA-XMASK SB-U s" ;" SB-APPEND SB$ PTX-L
   s" shl.b32 %r38,%r38,4;" PTX-L ;
\ LOAD: XOR the swizzle term into the ldmatrix-A chunk field while %r48 still holds the 16B-aligned kcol byte
\ offset (before the row base / buffer base add); the row base is a multiple of the swizzle span so the XOR
\ stays within the row's chunk field.
: MMA-XSWIZ-LOAD ( -- )    MMA-XSWIZ @ 0= if exit then
   s" xor.b32 %r48,%r48,%r38;" PTX-L ;
\ STORE: permute the cp.async destination chunk. In: %r21 = As row (dead after row*AROW-B is captured in
\ %r23), %r22 = chunk*16 byte offset. Out: %r22 ^= (row & (ACPR-1))<<4. Reuses %r21 as the term scratch.
: MMA-XSWIZ-STORE ( -- )   MMA-XSWIZ @ 0= if exit then
   SB-RESET s" and.b32 %r21,%r21," SB-APPEND MMA-XMASK SB-U s" ;" SB-APPEND SB$ PTX-L
   s" shl.b32 %r21,%r21,4;" PTX-L
   s" xor.b32 %r22,%r22,%r21;" PTX-L ;

\ mode-2 loop-invariant ldmatrix.x4 A geometry: rt=lane&7, tsel=lane>>3 select the 4 8x8 b16
\ tiles; %r47 = A row byte base (row = (tsel&1)*8 + rt + warp_row*16), %r49 = kcol hi bytes.
: MMA-SETUP-LDM ( -- )
   s" and.b32 %r45,%r25,7;" PTX-L        \ rt   = lane&7
   s" shr.u32 %r46,%r25,3;" PTX-L        \ tsel = lane>>3
   s" and.b32 %r40,%r46,1;" PTX-L  s" shl.b32 %r40,%r40,3;" PTX-L        \ (tsel&1)*8  (tile1/3 = +8 rows)
   s" add.u32 %r47,%r40,%r45;" PTX-L
   s" shl.b32 %r40,%r26,4;" PTX-L  s" add.u32 %r47,%r47,%r40;" PTX-L     \ + warp_row*16 = ldm A row
   MMA-XSWIZ-SETUP                       \ capture (ldm_row & (ACPR-1))<<4 into %r38 from the pre-scale row
   47 47 MMA-AROW-B MMA-SCALE            \ * As row byte stride = A row byte base (invariant)
   s" shr.u32 %r49,%r46,1;" PTX-L  s" shl.b32 %r49,%r49,4;" PTX-L ;      \ (tsel>>1)*16 = kcol hi bytes (tile2/3 = +4 K)

\ half (fp16/bf16) B-ldmatrix.trans loop-invariant lane base %r35 (dot habu-half-precision-ldmatrix). ONE
\ ldmatrix.sync.aligned.m8n8.x2.trans loads the 8x8 B fragment straight from the DEFAULT k-major Bs (no
\ transposed staging): with .trans the loaded k-major tile B[k][n] is returned transposed, so lane (gid,t)
\ receives {b0,b1}={Bs[ks+2t][col+gid],Bs[ks+2t+1][col+gid]} - EXACTLY the MMA-B-F16 operand (element-exact
\ by mma-gemm-check; .trans is legal here because the half element IS a b16, unlike tf32 where .trans splits a
\ tf32). Each lane addresses source K-row k=ks+(lane&15) at N-col base warp_col*(BN/WCOLS)+j*8; the invariant
\ part is %r35 = ASB + (lane&15)*(BN*2) + warp_col*(BN/WCOLS)*2, then per (ks,j) add ks*(BN*2)+j*16. Works for
\ both warp grids and all MFRAGS (B does not stack over M-frags). Scratch %r36,%r37; %r35 invariant.
: MMA-SETUP-BLDM-F16 ( -- )
   s" and.b32 %r36,%r25,15;" PTX-L              \ lane & 15 = source K-row within the substep
   36 36 MMA-BN @ 2 * MMA-SCALE                 \ * Bs f16 row byte stride (BN*2)
   37 27 MMA-BN @ MMA-WCOLS / 2 * MMA-SCALE     \ warp_col * (BN/WCOLS) * 2 = N-col byte base
   s" add.u32 %r36,%r36,%r37;" PTX-L
   SB-RESET s" add.u32 %r35,%r36," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L ;   \ + Bs byte offset (k-major base)

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
   MMA-LMODE @ 2 = if MMA-SETUP-LDM then   \ mode-2-only A geometry; modes 0/1 stay byte-identical to rung 1
   MMA-HALF? MMA-LMODE @ 2 = and if MMA-SETUP-BLDM-F16 then ;   \ half ldmatrix B (trans, k-major) invariant base

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
   SB-RESET s" add.u32 %r48,%r49," SB-APPEND ks MMA-ESZ * SB-U s" ;" SB-APPEND SB$ PTX-L   \ kcol bytes = (tsel>>1)*16 + ks*ESZ (tf32 *4 / half *2)
   MMA-XSWIZ-LOAD                                                                    \ chunk ^= (ldm_row & mask)<<4 (pad-free bank swizzle)
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
: MMA-B-RAW ( n -- ) {: j:n :}                  \ mode 1/2: raw ld.shared.b32 (mma truncates); b1 is +4 K rows = 16*BN B (1024 at BN=64)
   SB-RESET s" ld.shared.b32 %r54,[%r44+" SB-APPEND j 32 * SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r55,[%r44+" SB-APPEND j 32 * 16 MMA-BN @ * + SB-U s" ];" SB-APPEND SB$ PTX-L ;
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

\ GROUPED-RASTER CTA remap (dot habu-grouped-raster-cta). Triton GROUP_M-style launch swizzle so
\ CONCURRENTLY RESIDENT CTAs share A-row / B-col tiles in L2. In: %r6 = ctaid.x (natural tile_n),
\ %r7 = ctaid.y (natural tile_m). Out: %r6 = tile_n, %r7 = tile_m - ONLY the two id regs change
\ (rowBase/colBase derive from them downstream). Scratch %r12,%r13,%r20,%r21,%r22, all dead here
\ (MMA-SETUP(-WIDE) re-loads %r24..r34 / %r40.. and the cp.async pipeline re-derives %r20..r23 each
\ iteration, so nothing live crosses this remap). linear = ctaid.y*gridN + ctaid.x; group =
\ linear/(GROUP*gridN); within a group COLUMN-MAJOR: tile_m varies fastest (down a column), tile_n slowest.
\ CLAMP (constraint - an off-by-one here computes the WRONG TILE silently): the last group may be PARTIAL.
\ first_m = group*GROUP can leave fewer than GROUP M-blocks in the group (gridM - first_m < GROUP). The
\ within-group divisor/modulus MUST be that clamped height gsize = min(gridM - first_m, GROUP), NOT the raw
\ GROUP: with the un-clamped GROUP the column index tile_n = local/GROUP folds SHORT of gridN and the row
\ index tile_m = first_m + local%GROUP runs PAST gridM-1, so some (tile_m,tile_n) are computed twice and
\ others never - a silent wrong/zero C the element-exact runs at a PARTIAL-group shape (mma-gemm-check)
\ catch. gsize is proven >= 1: a launched CTA has linear < gridM*gridN, so group < gridM/GROUP, so
\ first_m = group*GROUP < gridM, so gridM - first_m >= 1 - hence the runtime div.u32/rem.u32 by gsize (and
\ by GROUP*gridN >= 1) never divide by zero. General div/rem, so GROUP need not be a power of two.
: MMA-GRID-REMAP ( -- )
   s" mov.u32 %r12,%nctaid.x;" PTX-L             \ gridN = gridDim.x (count of N-block columns)
   s" mov.u32 %r13,%nctaid.y;" PTX-L             \ gridM = gridDim.y (count of M-block rows)
   s" mad.lo.u32 %r20,%r7,%r12,%r6;" PTX-L       \ linear = ctaid.y*gridN + ctaid.x (row-major CTA index)
   21 12 MMA-GROUP @ MMA-SCALE                    \ ipg = gridN*GROUP (CTAs per full group)
   s" div.u32 %r22,%r20,%r21;" PTX-L             \ group = linear / ipg
   22 22 MMA-GROUP @ MMA-SCALE                    \ first_m = group*GROUP (group's first M-block)
   s" sub.u32 %r13,%r13,%r22;" PTX-L             \ gridM - first_m (M-blocks remaining in this group)
   SB-RESET s" min.u32 %r13,%r13," SB-APPEND MMA-GROUP @ SB-U s" ;" SB-APPEND SB$ PTX-L   \ gsize = min(that, GROUP): the partial-group clamp
   s" rem.u32 %r20,%r20,%r21;" PTX-L             \ local = linear % ipg (index within the group)
   s" div.u32 %r6,%r20,%r13;" PTX-L              \ tile_n = local / gsize (column-major: n slowest)
   s" rem.u32 %r21,%r20,%r13;" PTX-L             \ local % gsize (M-block within the group)
   s" add.u32 %r7,%r22,%r21;" PTX-L ;            \ tile_m = first_m + (local % gsize)

\ non-wide (BN=64 MFRAGS=1) prologue WITH the grouped-raster remap (dot habu-grouped-raster-cta). Mirrors
\ MM-THREAD-SETUP but inserts MMA-GRID-REMAP between reading the natural ids and deriving the *64 bases, so
\ the block computes the REMAPPED tile. The shared MM-THREAD-SETUP is monolithic (ids + bases in one word)
\ and frozen (it feeds the FP32 MM / lower-mm goldens), so the non-wide grouped path emits its own prologue;
\ MMA-BODY only reaches it when MMA-GROUP is on, and MM-THREAD-SETUP stays the byte-identical GROUP=0 path.
: MMA-THREAD-SETUP-GROUP ( -- )
   s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,%tid.y;" PTX-L
   s" mov.u32 %r6,%ctaid.x;" PTX-L  s" mov.u32 %r7,%ctaid.y;" PTX-L
   s" mad.lo.u32 %r8,%r5,16,%r4;" PTX-L
   MMA-GRID-REMAP                               \ %r6 <- tile_n, %r7 <- tile_m
   s" mul.lo.u32 %r9,%r7,64;" PTX-L             \ rowBase = tile_m * 64 (BROWS at MFRAGS=1)
   s" mul.lo.u32 %r10,%r6,64;" PTX-L            \ colBase = tile_n * 64 (BN in the non-wide path)
   s" mov.u32 %r11,SH;" PTX-L ;

: MMA-THREAD-SETUP-WIDE ( -- )                 \ like MM-THREAD-SETUP but rowBase = ctaid.y*BROWS
   s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,%tid.y;" PTX-L
   s" mov.u32 %r6,%ctaid.x;" PTX-L  s" mov.u32 %r7,%ctaid.y;" PTX-L
   s" mad.lo.u32 %r8,%r5,16,%r4;" PTX-L
   MMA-GROUP @ if MMA-GRID-REMAP then           \ grouped-raster: %r6 <- tile_n, %r7 <- tile_m (OFF = byte-identical)
   9 7 MMA-BROWS MMA-SCALE                      \ rowBase = ctaid.y * BROWS
   SB-RESET s" mul.lo.u32 %r10,%r6," SB-APPEND MMA-BN @ SB-U s" ;" SB-APPEND SB$ PTX-L   \ colBase = ctaid.x * BN (64 default)
   s" mov.u32 %r11,SH;" PTX-L ;

: MMA-ACC-ZERO-WIDE ( -- )                      \ zero MMA-ACCS (= MFRAGS*NTILES*4) accumulators %f10..
   MMA-ACCS  0 do
      SB-RESET s" mov.f32 %f" SB-APPEND 10 i + SB-U s" ,0f00000000;" SB-APPEND SB$ PTX-L
   loop ;

: MMA-SETUP-LDM-WIDE ( -- )                     \ mode-2 ldmatrix geometry, M-frag-0 row base (invariant)
   s" and.b32 %r45,%r25,7;" PTX-L               \ rt   = lane&7
   s" shr.u32 %r46,%r25,3;" PTX-L               \ tsel = lane>>3
   s" and.b32 %r40,%r46,1;" PTX-L  s" shl.b32 %r40,%r40,3;" PTX-L   \ (tsel&1)*8
   s" add.u32 %r47,%r40,%r45;" PTX-L
   40 26 16 MMA-MFRAGS @ * MMA-SCALE            \ %r40 = warp_row*(16*MFRAGS)
   s" add.u32 %r47,%r47,%r40;" PTX-L            \ ldm A row (M-frag 0)
   MMA-XSWIZ-SETUP                              \ capture (ldm_row & (ACPR-1))<<4 into %r38 (m-frag-invariant for 32<=BK<=64)
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
   31 27 MMA-BN @ MMA-WCOLS / MMA-SCALE         \ B shared col byte base = ((warp_col*(BN/WCOLS))+gid)*4; warp_col*32 at BN=64
   s" add.u32 %r31,%r31,%r28;" PTX-L
   s" shl.b32 %r31,%r31,2;" PTX-L
   32 26 16 MMA-MFRAGS @ * MMA-SCALE            \ gRow0 (M-frag 0) = rowBase + warp_row*16*MFRAGS + gid
   s" add.u32 %r32,%r9,%r32;" PTX-L
   s" add.u32 %r32,%r32,%r28;" PTX-L
   s" add.u32 %r33,%r32,8;" PTX-L               \ gRow1 = gRow0 + 8
   34 27 MMA-BN @ MMA-WCOLS / MMA-SCALE         \ gCol0 = colBase + warp_col*(BN/WCOLS) + 2t; warp_col*32 at BN=64
   s" add.u32 %r34,%r10,%r34;" PTX-L
   s" shl.b32 %r40,%r29,1;" PTX-L
   s" add.u32 %r34,%r34,%r40;" PTX-L
   MMA-LMODE @ 2 = if MMA-SETUP-LDM-WIDE then
   MMA-BLDM @ if MMA-SETUP-BLDM-WIDE then
   MMA-HALF? MMA-LMODE @ 2 = and if MMA-SETUP-BLDM-F16 then ;   \ half ldmatrix B (trans, k-major) invariant base

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
   SB-RESET s" add.u32 %r48,%r49," SB-APPEND ks MMA-ESZ * SB-U s" ;" SB-APPEND SB$ PTX-L   \ kcol bytes = (tsel>>1)*16 + ks*ESZ (tf32 *4 / half *2)
   MMA-XSWIZ-LOAD                                                                    \ chunk ^= (ldm_row & mask)<<4 (m-frag-invariant term %r38)
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
   SB-RESET s" ld.shared.f32 %f" SB-APPEND bt 1+ SB-U s" ,[%r44+" SB-APPEND j 32 * 16 MMA-BN @ * + SB-U s" ];" SB-APPEND SB$ PTX-L   \ b1 = +4 K rows = 16*BN B (1024 at BN=64)
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

: MMA-MMA-WIDE ( n n -- ) {: f:n j:n :}         \ mma for M-frag f, n-tile j: D(=%f(10+f*NTILES*4+4j)..) = A(group f).B(%r54,55) + D
   10 f MMA-NTILES 4 * * + j 4 * + {: d:n :}     \ accs/M-frag = NTILES*4 (16 at BN=64)
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
      42 42 MMA-BN @ 4 * MMA-SCALE                                                 \ (ks+t) * Bs row stride BN*4 (shl 8 at BN=64)
      SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
      s" add.u32 %r44,%r44,%r42;" PTX-L
      s" add.u32 %r44,%r44,%r31;" PTX-L
   then
   MMA-NTILES 0 do  ks i MMA-NTILE-WIDE  loop ;

: MMA-KTILE-WIDE ( -- )  MMA-KSUBS 0 do  i MMA-MK * MMA-KSTEP-WIDE  loop ;

\ store M-frag f, n-tile j: global rows gRow{0,1}+f*16, col gCol0+j*8 (D-fragment mapping)
: MMA-STORE-TILE-WIDE ( n n -- ) {: f:n j:n :}
   SB-RESET s" add.u32 %r40,%r34," SB-APPEND j 8 * SB-U s" ;" SB-APPEND SB$ PTX-L   \ %r40 = col0 = gCol0 + j*8
   10 f MMA-NTILES 4 * * + j 4 * + {: a0:n :}                                      \ accs/M-frag = NTILES*4 (16 at BN=64)
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
      MMA-NTILES 0 do  f i MMA-STORE-TILE-WIDE  loop
   loop ;

\ ============ HALF (fp16/bf16) compute path (MMA-DTYPE=1/2; dot habu-fp16-mma-tile + habu-bf16-m16n8k16-tile) ==
\ mma.sync.aligned.m16n8k16.row.col.f32.<t>.<t>.f32 over half As/Bs (host-packed), f32 accumulate, where the
\ operand dtype token <t> = MMA-ABT (f16 at MMA-DTYPE=1, bf16 at 2). fp16 and bf16 share this path VERBATIM:
\ a half is 2 bytes either way so every fragment load/store below is a pure bit-move (ld.shared.b32 /
\ ld.shared.u16 - format-agnostic), and ONLY the mma dtype token differs. These words carry the -F16 name
\ from the fp16 lane; they are the shared half path. (The word names are kept to preserve byte-identity.)
\ The C/D fragment map is IDENTICAL to the tf32 m16n8k8 tile (d0=D[gid][2t] d1=D[gid][2t+1]
\ d2=D[gid+8][2t] d3=D[gid+8][2t+1]), so the scattered store (MMA-STORE/-WIDE) and the smem epilogue
\ (MMA-EPI-*, which stage f32 accumulators) are reused VERBATIM. The A/B fragment maps are the PTX ISA
\ m16n8k16 f16 layout (gid=lane>>2, t=lane&3):
\   A(16x16) 4 packed .f16x2 regs: r0={As[gid][2t],As[gid][2t+1]} r1={As[gid+8][2t..]}
\            r2={As[gid][2t+8..]} r3={As[gid+8][2t+8..]}  (a b32 load packs the adjacent-K pair)
\   B(16x8)  2 packed .f16x2 regs: r0={Bs[2t][col],Bs[2t+1][col]} r1={Bs[2t+8][col],Bs[2t+9][col]}
\            (the pair is one BN-row apart in k-major Bs, so it is built from two u16 loads + shift/or)
\ where col = warp_col*32 + j*8 + gid. As row byte base %r30 and gRow/gCol come from MMA-SETUP(-WIDE)
\ (dtype-independent: %r30 uses the fp16 MMA-AROW-B stride). Same %r50..53 (A) / %r54,55 (B) / %r43
\ (B-pack scratch) budget as tf32, so the .reg header is unchanged.

\ non-wide A fragment (16x16) -> %r50..53. ks = K-substep base (0,16 at BK=32).
: MMA-A-F16 ( n -- ) {: ks:n :}
   s" shl.b32 %r40,%r29,1;" PTX-L                                                    \ 2t
   SB-RESET s" add.u32 %r40,%r40," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L        \ + ks (K col)
   s" shl.b32 %r40,%r40,1;" PTX-L                                                    \ *2 -> K byte offset within row
   s" add.u32 %r41,%r16,%r30;" PTX-L                                                 \ buffer base + A row byte base
   s" add.u32 %r40,%r41,%r40;" PTX-L
   8 MMA-AROW-B * {: a1o:n :}                                                         \ +8 As rows = r1/r3 offset
   s" ld.shared.b32 %r50,[%r40];" PTX-L                                              \ r0 = As[gid][ks+2t : +1]
   SB-RESET s" ld.shared.b32 %r51,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L   \ r1 = As[gid+8][ks+2t : +1]
   s" ld.shared.b32 %r52,[%r40+16];" PTX-L                                          \ r2 = As[gid][ks+2t+8 : +1]
   SB-RESET s" ld.shared.b32 %r53,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L ;  \ r3 = As[gid+8][ks+2t+8 : +1]

\ B fragment (16x8) for K-substep ks, n-tile j -> %r54,%r55. Bs k-major flat [BK][BN] (row stride
\ BN*2 = 128 B): the two halves of each register are adjacent in K (one 128 B row apart), so build
\ each b32 from two u16 loads + shift/or. col = warp_col*32 + j*8 + gid. Scratch %r40,%r42,%r43,%r44.
: MMA-B-F16 ( n n -- ) {: ks:n j:n :}
   40 27 MMA-BN @ MMA-WCOLS / MMA-SCALE                                              \ warp_col*(BN/WCOLS); warp_col*32 at BN=64
   s" add.u32 %r40,%r40,%r28;" PTX-L                                                 \ + gid = col
   j 0 > if SB-RESET s" add.u32 %r40,%r40," SB-APPEND j 8 * SB-U s" ;" SB-APPEND SB$ PTX-L then  \ + j*8
   s" shl.b32 %r40,%r40,1;" PTX-L                                                    \ col*2 (byte)
   s" shl.b32 %r42,%r29,1;" PTX-L                                                    \ 2t
   SB-RESET s" add.u32 %r42,%r42," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L        \ + ks (K row)
   42 42 MMA-BN @ 2 * MMA-SCALE                                                      \ * Bs f16 row stride BN*2 (shl 7 at BN=64)
   SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L   \ + Bs byte offset
   s" add.u32 %r44,%r44,%r42;" PTX-L
   s" add.u32 %r44,%r44,%r40;" PTX-L                                                 \ &Bs[ks+2t][col] (b0)
   s" ld.shared.u16 %r54,[%r44];" PTX-L                                             \ b0
   SB-RESET s" ld.shared.u16 %r43,[%r44+" SB-APPEND MMA-BN @ 2 * SB-U s" ];" SB-APPEND SB$ PTX-L   \ b1 (+1 K = BN*2 B; 128 at BN=64)
   s" shl.b32 %r43,%r43,16;" PTX-L  s" or.b32 %r54,%r54,%r43;" PTX-L
   SB-RESET s" ld.shared.u16 %r55,[%r44+" SB-APPEND 16 MMA-BN @ * SB-U s" ];" SB-APPEND SB$ PTX-L  \ b2 (+8 K = 16*BN B; 1024 at BN=64)
   SB-RESET s" ld.shared.u16 %r43,[%r44+" SB-APPEND 18 MMA-BN @ * SB-U s" ];" SB-APPEND SB$ PTX-L  \ b3 (+9 K = 18*BN B; 1152 at BN=64)
   s" shl.b32 %r43,%r43,16;" PTX-L  s" or.b32 %r55,%r55,%r43;" PTX-L ;

\ TRANSPOSED B fragment (dot habu-fp16-transposed-bs): the SAME {%r54,%r55}={b0,b1},{b2,b3} operand as
\ MMA-B-F16, read from the n-major BT[n][k] staging where each register's K-adjacent halves are contiguous
\ -> ONE ld.shared.b32 per register, no shift/or. BT[col][k] at %r16 + ASB + col*BTROW-B + k*2; b0/b1 at
\ k=ks+2t, b2/b3 at k=ks+2t+8 (+16 B). col = warp_col*32 + gid + j*8. Scratch %r40,%r42,%r44.
: MMA-B-F16-T ( n n -- ) {: ks:n j:n :}
   s" shl.b32 %r40,%r27,5;" PTX-L                                                    \ warp_col*32
   s" add.u32 %r40,%r40,%r28;" PTX-L                                                 \ + gid = col
   j 0 > if SB-RESET s" add.u32 %r40,%r40," SB-APPEND j 8 * SB-U s" ;" SB-APPEND SB$ PTX-L then  \ + j*8
   40 40 MMA-BTROW-B MMA-SCALE                                                       \ col * BT row byte stride
   s" shl.b32 %r42,%r29,1;" PTX-L                                                    \ 2t
   SB-RESET s" add.u32 %r42,%r42," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L        \ + ks (K)
   s" shl.b32 %r42,%r42,1;" PTX-L                                                    \ (ks+2t)*2 = K byte offset within the BT row
   s" add.u32 %r40,%r40,%r42;" PTX-L
   SB-RESET s" add.u32 %r44,%r16," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L   \ + Bs byte offset (BT base)
   s" add.u32 %r44,%r44,%r40;" PTX-L                                                 \ &BT[col][ks+2t] (b0/b1)
   s" ld.shared.b32 %r54,[%r44];" PTX-L                                             \ {b0,b1} (K-adjacent pair, contiguous)
   s" ld.shared.b32 %r55,[%r44+16];" PTX-L ;                                        \ {b2,b3} (+8 K halves = +16 B)
\ half B fragment via ONE ldmatrix.x2.trans over the DEFAULT k-major Bs (dot habu-half-precision-ldmatrix):
\ {%r54,%r55}={b0,b1},{b2,b3}. Address = %r16 + %r35 (invariant lane base) + ks*(BN*2) + j*16, where each
\ lane's source K-row = ks + lane&15 and the tile's 8 N-cols are contiguous in the k-major row - so .trans
\ returns the mma B operand directly (no shift/or, no transposed staging). Replaces the two ld.shared.u16 +
\ shift/or (MMA-B-F16) per register. Scratch %r48.
: MMA-B-LDM-F16 ( n n -- ) {: ks:n j:n :}
   ks MMA-BN @ 2 * *  j 16 * +  {: off:n :}     \ ks*(BN*2) + j*16 byte offset from the lane base
   SB-RESET s" add.u32 %r48,%r35," SB-APPEND off SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r48,%r16,%r48;" PTX-L
   s" ldmatrix.sync.aligned.m8n8.x2.trans.shared.b16 {%r54,%r55},[%r48];" PTX-L ;
: MMA-B-F16-LOAD ( n n -- )                                                           \ ks j ; dispatch: ldmatrix / transposed BT / k-major scalar
   MMA-LMODE @ 2 = if MMA-B-LDM-F16 exit then
   MMA-BTF16 @ if MMA-B-F16-T else MMA-B-F16 then ;
: MMA-LOAD-A-F16 ( n -- )  MMA-LMODE @ 2 = if MMA-A-LDM else MMA-A-F16 then ;         \ ks ; ldmatrix.x4 A or scalar packed-b32

\ non-wide n-tile j (0..3): load its 8x8 B fragment, mma into %f(10+4j)..%f(13+4j) (A reused from MMA-KSTEP-F16).
: MMA-NTILE-F16 ( n n -- ) {: ks:n j:n :}
   ks j MMA-B-F16-LOAD
   10 j 4 * + {: a0:n :}
   SB-RESET s" mma.sync.aligned.m16n8k16.row.col.f32." SB-APPEND MMA-ABT SB-APPEND s" ." SB-APPEND MMA-ABT SB-APPEND s" .f32 {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" }, {%r50,%r51,%r52,%r53}, {%r54,%r55}, {%f" SB-APPEND a0 SB-U
      s" ,%f" SB-APPEND a0 1+ SB-U s" ,%f" SB-APPEND a0 2 + SB-U s" ,%f" SB-APPEND a0 3 + SB-U
      s" };" SB-APPEND SB$ PTX-L ;

\ one MMA-K substep ks: load the 16x16 A fragment (reused across 4 n-tiles), then mma the 4 n-tiles.
: MMA-KSTEP-F16 ( n -- ) {: ks:n :}
   ks MMA-LOAD-A-F16
   4 0 do  ks i MMA-NTILE-F16  loop ;
: MMA-KTILE-F16 ( -- )  MMA-KSUBS 0 do  i MMA-MKD * MMA-KSTEP-F16  loop ;   \ BK/16 substeps over the staged tile

\ --- wide fp16 (MMA-MFRAGS>1): one 16x16 A fragment per stacked M-frag -> group %r(50+6f), +f*16 rows ---
: MMA-A-F16-WIDE ( n n -- ) {: ks:n f:n :}
   f MMA-AREG {: g:n :}
   s" shl.b32 %r40,%r29,1;" PTX-L                                                    \ 2t
   SB-RESET s" add.u32 %r40,%r40," SB-APPEND ks SB-U s" ;" SB-APPEND SB$ PTX-L        \ + ks
   s" shl.b32 %r40,%r40,1;" PTX-L                                                    \ *2 -> K byte offset
   s" add.u32 %r41,%r16,%r30;" PTX-L
   f 0 > if SB-RESET s" add.u32 %r41,%r41," SB-APPEND f 16 * MMA-AROW-B * SB-U s" ;" SB-APPEND SB$ PTX-L then  \ + f*16 rows
   s" add.u32 %r40,%r41,%r40;" PTX-L
   8 MMA-AROW-B * {: a1o:n :}
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g SB-U s" ,[%r40];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 1+ SB-U s" ,[%r40+" SB-APPEND a1o SB-U s" ];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 2 + SB-U s" ,[%r40+16];" SB-APPEND SB$ PTX-L
   SB-RESET s" ld.shared.b32 %r" SB-APPEND g 3 + SB-U s" ,[%r40+" SB-APPEND a1o 16 + SB-U s" ];" SB-APPEND SB$ PTX-L ;
: MMA-LOAD-A-F16-WIDE ( n n -- )  MMA-LMODE @ 2 = if MMA-A-LDM-WIDE else MMA-A-F16-WIDE then ;   \ ks f ; ldmatrix.x4 A or scalar
: MMA-MMA-F16-WIDE ( n n -- ) {: f:n j:n :}    \ D(=%f(10+f*NTILES*4+4j)..) = A(group f).B(%r54,55) + D
   10 f MMA-NTILES 4 * * + j 4 * + {: d:n :}    \ accs/M-frag = NTILES*4 (16 at BN=64)
   f MMA-AREG {: g:n :}
   SB-RESET s" mma.sync.aligned.m16n8k16.row.col.f32." SB-APPEND MMA-ABT SB-APPEND s" ." SB-APPEND MMA-ABT SB-APPEND s" .f32 {%f" SB-APPEND d SB-U
      s" ,%f" SB-APPEND d 1+ SB-U s" ,%f" SB-APPEND d 2 + SB-U s" ,%f" SB-APPEND d 3 + SB-U
      s" }, {%r" SB-APPEND g SB-U s" ,%r" SB-APPEND g 1+ SB-U s" ,%r" SB-APPEND g 2 + SB-U s" ,%r" SB-APPEND g 3 + SB-U
      s" }, {%r54,%r55}, {%f" SB-APPEND d SB-U
      s" ,%f" SB-APPEND d 1+ SB-U s" ,%f" SB-APPEND d 2 + SB-U s" ,%f" SB-APPEND d 3 + SB-U s" };" SB-APPEND SB$ PTX-L ;
: MMA-NTILE-F16-WIDE ( n n -- ) {: ks:n j:n :}   \ B loaded ONCE, mma'd against every M-frag (B reused MFRAGS times)
   ks j MMA-B-F16-LOAD
   MMA-MFRAGS @ 0 do  i j MMA-MMA-F16-WIDE  loop ;
: MMA-KSTEP-F16-WIDE ( n -- ) {: ks:n :}
   MMA-MFRAGS @ 0 do  ks i MMA-LOAD-A-F16-WIDE  loop
   MMA-NTILES 0 do  ks i MMA-NTILE-F16-WIDE  loop ;
: MMA-KTILE-F16-WIDE ( -- )  MMA-KSUBS 0 do  i MMA-MKD * MMA-KSTEP-F16-WIDE  loop ;

\ ============ SHARED-MEMORY C EPILOGUE (MMA-EPILOG=1; unified over the MFRAGS/WARPS family) ====
\ Replaces the scattered per-lane global store with: (1) each lane writes its D-fragment accumulators
\ into a block-local [BROWS][BN] tile at SH (byte offset (r*BN + c)*4, matching C's row-major layout),
\ (2) a block barrier, (3) the block re-reads the tile and writes C coalesced. Uses only scratch regs
\ %r40..46 / %f26 / %rd10,%rd12 and the invariants set by MMA-SETUP(-WIDE) + the thread setup: %r8
\ tid_lin, %r9 rowBase, %r10 colBase, %r11 SH, %r2 N, %rd3 C, %r26 warp_row, %r27 warp_col, %r28 gid,
\ %r29 t. Works for MFRAGS=1 (f=0) and the wide path identically (the row base carries the +f*16 offset).

\ %r45 = this lane's staging base = SH + (warp_row*16*MFRAGS + gid)*(BN*4) + warp_col*(BN/WCOLS)*4 + t*2*4
\   (the M-frag-0, n-tile-0, d0 element; at BN=64: BN*4 = 256, warp_col*(BN/WCOLS)*4 = warp_col*128, t*2*4 = t*8).
: MMA-EPI-SETUP ( -- )
   40 26 16 MMA-MFRAGS @ * MMA-SCALE            \ %r40 = warp_row * (16*MFRAGS)
   s" add.u32 %r40,%r40,%r28;" PTX-L            \ + gid = local row0
   40 40 MMA-BN @ 4 * MMA-SCALE                 \ * staging row byte stride BN*4 (shl 8 at BN=64)
   41 27 MMA-BN @ MMA-WCOLS / 4 * MMA-SCALE     \ warp_col * (BN/WCOLS)*4 (col byte base of the BN/WCOLS-col half; shl 7 at BN=64)
   s" shl.b32 %r42,%r29,3;" PTX-L               \ t * 8 (2 cols/lane * 4 B)
   s" add.u32 %r41,%r41,%r42;" PTX-L
   s" add.u32 %r45,%r11,%r40;" PTX-L            \ SH + row0*(BN*4)
   s" add.u32 %r45,%r45,%r41;" PTX-L ;          \ + col bytes -> lane staging base

\ write M-frag f, n-tile j: d0->[row0][c0] d1->[row0][c0+1] d2->[row0+8][c0] d3->[row0+8][c0+1]
\ (the D-fragment map). Row offset f*16 rows = f*16*(BN*4) B; n-tile col offset j*8 cols = j*32 B; the
\ +8-row (d2/d3) offset is 8*(BN*4) B (2048 at BN=64). All three are emit-time constants.
: MMA-EPI-STORE-TILE ( n n -- ) {: f:n j:n :}
   f 16 * MMA-BN @ 4 * *  j 8 * 4 * +  {: off:n :}   \ (f*16 rows)*(BN*4) + (j*8 cols)*4
   10 f MMA-NTILES 4 * * + j 4 * + {: a0:n :}     \ accumulator base %f(10+f*NTILES*4+4j)
   8 MMA-BN @ 4 * *  {: r8:n :}                   \ +8-row (d2/d3) byte offset = 8*(BN*4) (2048 at BN=64)
   SB-RESET s" add.u32 %r46,%r45," SB-APPEND off SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" st.shared.f32 [%r46],%f" SB-APPEND a0 SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" st.shared.f32 [%r46+4],%f" SB-APPEND a0 1+ SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" st.shared.f32 [%r46+" SB-APPEND r8 SB-U s" ],%f" SB-APPEND a0 2 + SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" st.shared.f32 [%r46+" SB-APPEND r8 4 + SB-U s" ],%f" SB-APPEND a0 3 + SB-U s" ;" SB-APPEND SB$ PTX-L ;

: MMA-EPI-FILL ( -- )                            \ every lane writes its MMA-ACCS accumulators into the tile
   MMA-MFRAGS @ 0 do  i {: f:n :}
      MMA-NTILES 0 do  f i MMA-EPI-STORE-TILE  loop
   loop ;

\ coalesced drain: element e = tid_lin + m*NTHREADS -> tile row e/BN, col e%BN; the block sweeps all
\ BROWS*BN elements in MMA-ACCS rounds (BROWS*BN / NTHREADS = MFRAGS*NTILES*4 = the per-lane accumulator
\ count). Consecutive lanes read consecutive tile elements (SH + e*4) and write consecutive C columns.
: MMA-EPI-DRAIN ( -- )
   MMA-ACCS  0 do  i {: m:n :}
      SB-RESET s" add.u32 %r40,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L   \ e = tid_lin + m*NTHREADS
      SB-RESET s" shr.u32 %r41,%r40," SB-APPEND MMA-BN @ MMA-LOG2 SB-U s" ;" SB-APPEND SB$ PTX-L  \ row = e >> log2(BN) (6 at BN=64)
      SB-RESET s" and.b32 %r42,%r40," SB-APPEND MMA-BN @ 1- SB-U s" ;" SB-APPEND SB$ PTX-L          \ col = e & (BN-1) (63 at BN=64)
      s" add.u32 %r41,%r9,%r41;" PTX-L             \ gRow = rowBase + row
      s" add.u32 %r42,%r10,%r42;" PTX-L            \ gCol = colBase + col
      s" mad.lo.u32 %r43,%r41,%r2,%r42;" PTX-L     \ gRow*N + gCol
      s" mul.wide.u32 %rd10,%r43,4;" PTX-L  s" add.u64 %rd12,%rd3,%rd10;" PTX-L   \ &C[gRow][gCol]
      s" shl.b32 %r44,%r40,2;" PTX-L  s" add.u32 %r44,%r11,%r44;" PTX-L           \ SH + e*4
      s" ld.shared.f32 %f26,[%r44];" PTX-L
      s" st.global.f32 [%rd12],%f26;" PTX-L
   loop ;

\ the full epilogue. The leading barrier is the WAR fence: the staging tile aliases the pipeline's SH
\ buffers, so every warp must finish reading its A/B fragments before any thread overwrites SH. The
\ middle barrier is the RAW fence: the tile must be fully written before any thread reads a foreign lane's
\ element back for the coalesced global store.
: MMA-EPI-STORE ( -- )
   CPP-SYNC  MMA-EPI-SETUP  MMA-EPI-FILL  CPP-SYNC  MMA-EPI-DRAIN ;

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
   MMA-XSWIZ-STORE                                                                      \ chunk ^= (row & mask)<<4 (pad-free bank swizzle; clobbers %r21=row, re-derived for Bs)
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
   MMA-XSWIZ-STORE                                                                      \ chunk ^= (row & mask)<<4 (pad-free bank swizzle; %r21=row dead after this)
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;
: MMA-CPW-CHUNK-B ( n n n -- ) {: m:n bufr:n ktr:n :}   \ one Bs 16B chunk, chunk-set m (BCPR = BN/4 chunks/Bs row)
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND MMA-BCPR MMA-LOG2 SB-U s" ;" SB-APPEND SB$ PTX-L     \ k = c / (BN/4) (c>>4 at BN=64)
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-BCPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L            \ chunk = c & (BN/4-1) (c&15 at BN=64)
   s" shl.b32 %r22,%r22,2;" PTX-L                                                       \ col = chunk*4 (EPC=4 tf32)
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

\ ---- FP16 cp.async staging (dot habu-fp16-mma-tile). Globals pA/pB are f16 (2 B/elem); As k-major
\ (row stride MMA-AROW-B), Bs k-major flat (row stride BN*2). A 16-byte cp.async chunk carries EPC=8
\ halves, so As chunks/row = BK/8, Bs chunks/row = BN/8 = 8. Same chunk->thread partition as the tf32
\ words with the element size halved and the chunk width doubled; MFRAGS=1 interleaves As+Bs (BM=BN),
\ the wide path stages them in independent loops (BROWS != BN). ----
: MMA-CP-CHUNK-F16 ( n n n -- ) {: m:n bufr:n ktr:n :}   \ interleaved As+Bs (MFRAGS=1); 8-half chunks
   MMA-ACPR MMA-LOG2 {: acl:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   \ --- As: row=c>>acl, k=(c&(ACPR-1))*8 ; src A[rowBase+row][kt+k] f16 ; dst buf + row*AROW-B + kchunk*16 ---
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND acl SB-U s" ;" SB-APPEND SB$ PTX-L         \ row
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-ACPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L \ kchunk
   s" shl.b32 %r22,%r22,3;" PTX-L                                                       \ k = kchunk*8
   s" add.u32 %r23,%r9,%r21;" PTX-L
   SB-RESET s" mad.lo.u32 %r23,%r23,%r3,%r" SB-APPEND ktr SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,2;" PTX-L  s" add.u64 %rd11,%rd1,%rd10;" PTX-L            \ *2 (f16 byte)
   23 21 MMA-AROW-B MMA-SCALE                                                           \ %r23 = row*AROW-B
   s" shl.b32 %r22,%r22,1;" PTX-L                                                       \ kchunk*16 = k*2
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L
   \ --- Bs: k=c>>3, col=(c&7)*8 ; src B[kt+k][colBase+col] f16 ; dst buf + ASB + c*16 ---
   s" shr.u32 %r21,%r20,3;" PTX-L
   s" and.b32 %r22,%r20,7;" PTX-L  s" shl.b32 %r22,%r22,3;" PTX-L                       \ col = (c&7)*8
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r21;" SB-APPEND SB$ PTX-L
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,2;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L            \ *2 (f16 byte)
   s" shl.b32 %r23,%r20,4;" PTX-L                                                       \ c*16 (dst chunk stride)
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;
: MMA-CPW-CHUNK-A-F16 ( n n n -- ) {: m:n bufr:n ktr:n :}   \ one wide As 8-half chunk, chunk-set m
   MMA-ACPR MMA-LOG2 {: acl:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND acl SB-U s" ;" SB-APPEND SB$ PTX-L         \ row = c>>acl
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-ACPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L \ kchunk
   s" shl.b32 %r22,%r22,3;" PTX-L                                                       \ k = kchunk*8
   s" add.u32 %r23,%r9,%r21;" PTX-L
   SB-RESET s" mad.lo.u32 %r23,%r23,%r3,%r" SB-APPEND ktr SB-U s" ;" SB-APPEND SB$ PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,2;" PTX-L  s" add.u64 %rd11,%rd1,%rd10;" PTX-L
   23 21 MMA-AROW-B MMA-SCALE
   s" shl.b32 %r22,%r22,1;" PTX-L                                                       \ kchunk*16
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;
: MMA-CPW-CHUNK-B-F16 ( n n n -- ) {: m:n bufr:n ktr:n :}   \ one wide Bs 8-half chunk, chunk-set m (BCPR = BN/8 chunks/Bs row)
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L
   SB-RESET s" shr.u32 %r21,%r20," SB-APPEND MMA-BCPR MMA-LOG2 SB-U s" ;" SB-APPEND SB$ PTX-L      \ k = c / (BN/8) (c>>3 at BN=64)
   SB-RESET s" and.b32 %r22,%r20," SB-APPEND MMA-BCPR 1- SB-U s" ;" SB-APPEND SB$ PTX-L             \ chunk = c & (BN/8-1) (c&7 at BN=64)
   s" shl.b32 %r22,%r22,3;" PTX-L                                                       \ col = chunk*8 (EPC=8 fp16)
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r21;" SB-APPEND SB$ PTX-L
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L
   s" add.u32 %r23,%r23,%r22;" PTX-L
   s" mul.wide.u32 %rd10,%r23,2;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L
   s" shl.b32 %r23,%r20,4;" PTX-L
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L
   s" cp.async.cg.shared.global [%r23],[%rd11],16;" PTX-L ;
: MMA-CPW-STAGE-F16 ( n n -- ) {: bufr:n ktr:n :}
   MMA-ACPN 0 do  i bufr ktr MMA-CPW-CHUNK-A-F16  loop
   MMA-BCPN 0 do  i bufr ktr MMA-CPW-CHUNK-B-F16  loop ;

\ TRANSPOSED fp16 Bs staging (dot habu-fp16-transposed-bs): one scalar half BT[n][k] = B[ktr+k][colBase+n],
\ c = tid_lin + m*NTHREADS, n = c&63, k = c>>6. Global read is coalesced (a warp's 32 lanes -> 32 contiguous
\ n = 64 B), the shared write is strided (BT n-major, row stride BTROW-B). cp.async CANNOT do the transpose
\ (a contiguous chunk scatters across BT rows), so this is a scalar u16 copy - the fp16 mirror of the tf32
\ MMA-CPW-CHUNK-BT (b32). The BT tile is tiny (64*BK halves) and staged once per K-tile, fed KSUBS*4 times.
\ Uses only prefetch scratch %r20..23 / %rd10..11 (invariants %r24..34 survive; the loaded half rides %r20).
: MMA-CPW-CHUNK-BTF16 ( n n n -- ) {: m:n bufr:n ktr:n :}
   SB-RESET s" add.u32 %r20,%r8," SB-APPEND m MMA-NTHREADS * SB-U s" ;" SB-APPEND SB$ PTX-L      \ c = tid_lin + m*NTHREADS
   s" and.b32 %r21,%r20,63;" PTX-L                                                      \ n = c & 63
   s" shr.u32 %r22,%r20,6;" PTX-L                                                       \ k = c >> 6
   SB-RESET s" add.u32 %r23,%r" SB-APPEND ktr SB-U s" ,%r22;" SB-APPEND SB$ PTX-L        \ ktr + k
   s" mad.lo.u32 %r23,%r23,%r2,%r10;" PTX-L                                             \ (ktr+k)*N + colBase
   s" add.u32 %r23,%r23,%r21;" PTX-L                                                    \ + n
   s" mul.wide.u32 %rd10,%r23,2;" PTX-L  s" add.u64 %rd11,%rd2,%rd10;" PTX-L            \ *2 (f16 byte); &B[ktr+k][colBase+n]
   s" ld.global.u16 %r20,[%rd11];" PTX-L                                                \ B[ktr+k][colBase+n] (c dead)
   23 21 MMA-BTROW-B MMA-SCALE                                                          \ %r23 = n * BT row byte stride
   s" shl.b32 %r22,%r22,1;" PTX-L                                                       \ k*2 (f16 byte)
   s" add.u32 %r23,%r23,%r22;" PTX-L
   SB-RESET s" add.u32 %r23,%r23," SB-APPEND MMA-ASB SB-U s" ;" SB-APPEND SB$ PTX-L      \ + Bs byte offset
   SB-RESET s" add.u32 %r23,%r" SB-APPEND bufr SB-U s" ,%r23;" SB-APPEND SB$ PTX-L       \ + buffer base
   s" st.shared.u16 [%r23],%r20;" PTX-L ;
: MMA-CPW-STAGE-BTF16 ( n n -- ) {: bufr:n ktr:n :}   \ As via cp.async 8-half chunks, BT via scalar transposed u16 copy
   MMA-ACPN 0 do  i bufr ktr MMA-CPW-CHUNK-A-F16  loop
   MMA-BTCPN 0 do  i bufr ktr MMA-CPW-CHUNK-BTF16  loop ;

: MMA-CP-STAGE ( n n -- ) {: bufr:n ktr:n :}   \ stage one K-tile (As+Bs) into buffer bufr from column ktr
   MMA-HALF? if                                             \ fp16/bf16 share the half (2-byte element) staging
      MMA-BTF16 @ if bufr ktr MMA-CPW-STAGE-BTF16 exit then   \ transposed BT: split As cp.async + BT scalar copy (any MFRAGS)
      MMA-WIDE? if bufr ktr MMA-CPW-STAGE-F16 exit then       \ split As/Bs (BROWS!=BN): MFRAGS>1 or wide BN
      MMA-CPN 0 do  i bufr ktr MMA-CP-CHUNK-F16  loop  exit   \ interleaved (BM=BN=64, MFRAGS=1)
   then
   MMA-WIDE? if bufr ktr MMA-CPW-STAGE exit then              \ split As/Bs: MFRAGS>1 or wide BN
   MMA-CPN 0 do  i bufr ktr MMA-CP-CHUNK  loop ;              \ interleaved (BM=BN=64, MFRAGS=1)

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
\
\ ISSUE-BEFORE-COMPUTE is REQUIRED here, NOT the deferred cp.async issue of the N>=3 ring (dot
\ habu-reorder-cp-async, MMA-PIPE-KLOOP-MULTI). The double buffer has only ONE buffer of slack: the
\ next-tile prefetch MUST be in flight DURING the compute it overlaps, so it is issued at the loop top
\ and kept in flight by wait_group(1). Deferring the issue to after the compute burst would force
\ wait_group(0) at the next loop top (at N=2 only one group can be in flight), fully EXPOSING the
\ cp.async load latency and collapsing the overlap - measured a -12%..-34% regression across every
\ stages=2 tf32 config (docs/eval-triton.md Round 9, in-session base-vs-branch best-of-3). The deferral
\ only pays when N-2>=1 groups stay in flight (N>=3), so it lives in MMA-PIPE-KLOOP-MULTI alone.
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
\   steady   : while a tile remains to PREFETCH (kt_pf<K): wait_group(N-2), bar.sync, compute from the
\              read buffer, bar.sync (buffer-reuse fence), THEN issue the prefetch of tile kt_pf into the
\              write buffer + commit, then advance both ring bases and kt. DEFERRED cp.async issue (dot
\              habu-reorder-cp-async): the prefetch fires AFTER the mma burst, not before, so ptxas
\              hoists the LDGSTS into the tensor-core stall shadow (the scout's 3.44 -> 1.28 cyc/HMMA
\              exposed-head verdict; docs/eval-triton.md Round 9). Accounting for the deferred issue:
\              at the loop top the prefetch of THIS iteration's tile has not fired yet, so only N-2
\              groups are in flight (one fewer than the issue-first form's N-1); the sum of committed
\              groups at the wait is C=(N-1)+kt_cmp_index, and cp.async.wait_group(n) guarantees the
\              oldest C-n complete, so requiring the compute tile (index kt_cmp_index) landed needs
\              n <= N-2 - hence wait_group(N-2), the largest n that still guarantees it.
\   epilogue : the last N-1 tiles have no more prefetch, so the in-flight group count must be drained
\              one at a time. DERIVATION for the deferred issue (re-derived, literals UNCHANGED vs the
\              issue-first form): the final steady iteration issues+commits the last tile BEFORE the
\              ring exits, so ALL T tiles are committed at epilogue entry (C=T) in either ordering; for
\              epilogue index j (j=0..N-2) the tile computed is (T-N+1)+j, and wait_group(n) guarantees
\              the oldest C-n=T-n complete, so tile (T-N+1)+j landed needs T-n >= (T-N+1)+j+1, i.e.
\              n <= N-2-j; the largest such n is wait_group(N-2-j) (N-2,N-3,...,0 - the last tile drains
\              fully). Because C=T is fixed by epilogue entry, the drain literals do not depend on where
\              the steady issue sits. The compute is guarded by kt_cmp<K so a K with fewer than N-1
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
: MMA-KTILE-DISPATCH ( -- )
   MMA-HALF? if MMA-WIDE? if MMA-KTILE-F16-WIDE else MMA-KTILE-F16 then exit then   \ half path; mma token = MMA-ABT
   MMA-WIDE? if MMA-KTILE-WIDE else MMA-KTILE then ;

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
   MMA-STAGES @ 2 - CPP-WAIT                                     \ wait_group(N-2): deferred issue -> one fewer in flight at loop top
   CPP-SYNC  MMA-KTILE-DISPATCH  CPP-SYNC                        \ compute from read buffer, then reuse fence
   18 14 MMA-CP-STAGE  CPP-COMMIT                                \ deferred: prefetch tile kt_pf AFTER the compute burst
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

\ fail closed on a half (fp16/bf16) tile combined with a feed knob NOT wired for the m16n8k16 half fragment
\ (dot habu-fp16-mma-tile / habu-bf16-m16n8k16-tile / habu-half-precision-ldmatrix). A half tile feeds A/B two
\ ways: LMODE=0 scalar packed-b32 (default), or LMODE=2 ldmatrix (A ldmatrix.x4.b16 + B ldmatrix.x2.trans.b16
\ over the k-major Bs). LMODE=1 (the tf32 cvt-DROP ablation) has no meaning for a half (already raw, no cvt),
\ so it is rejected. The half ldmatrix B is k-major, so it CONFLICTS with the transposed-BT feed (MMA-BTF16) -
\ reject the combination. The half ldmatrix is wired at BN=64 (the mid/large MFRAGS tiles the parity plan
\ targets); BN>64 half stays on the scalar feed, so LMODE=2 + BN>64 is rejected. The tf32 transposed-Bs
\ B-ldmatrix (BLDM) and the DCE-safe wide ablation (ABLATE, tf32 wide-path only) are NOT half paths. Reject at
\ EMIT time so a bad knob throws instead of emitting a kernel whose fragment loads disagree with the mma
\ operand layout. Dtype-token-independent: fp16 and bf16 share the same fragment layout, so both gate identically.
: MMA-CHECK-DTYPE ( -- )
   MMA-HALF? 0= if exit then
   MMA-LMODE @ 1 = if E-MMA-DTYPE throw then            \ no cvt-drop (raw) variant for a half (only 0 scalar / 2 ldmatrix)
   MMA-LMODE @ 2 = if
      MMA-BTF16 @ if E-MMA-DTYPE throw then             \ half ldmatrix B is k-major -> conflicts with the transposed BT feed
      MMA-BN @ 64 > if E-MMA-DTYPE throw then           \ half A/B ldmatrix wired at BN=64 only (BN>64 half uses the scalar feed)
   then
   MMA-BLDM @ if E-MMA-DTYPE throw then                 \ tf32 transposed-Bs B-ldmatrix is not a half path (use MMA-BTF16)
   MMA-ABLATE @ if E-MMA-DTYPE throw then ;             \ ablation variants are the tf32 wide path only

\ fail closed on an illegal XOR-swizzle config (dot habu-xor-swizzle-mma). The swizzle permutes As 16-byte
\ K-chunks by chunk ^= (row & (ACPR-1)) on BOTH the cp.async store and the ldmatrix-A load, so it is legal
\ ONLY where every store site is swizzled and the mask math holds: (1) MMA-PAD must be 0 - the swizzle IS the
\ pad-free remedy, combining the two is contradictory and untested; (2) MMA-LMODE must be 2 - the swizzle is
\ wired on the ldmatrix-A read (%r48 chunk field), not the scalar reads whose +8-row/+4-K offsets are a
\ different address form; (3) tf32 only - a half (fp16/bf16) tile stores As through the F16 word (MMA-CP-CHUNK
\ -F16), NOT swizzled, so the half load would read a mis-permuted As; (4) 32<=BK<=64 - BK<32 (ACPR<8) cannot
\ separate all 8 ldmatrix rows, BK>64 (ACPR>16) breaks the "m-frag stride 16 leaves row&mask unchanged"
\ invariant the loop-invariant swizzle term relies on; (5) no wide ablation (numerically-wrong feed). Reject at
\ EMIT time so a bad knob throws instead of emitting a kernel whose swizzled load disagrees with an un-swizzled store.
: MMA-CHECK-XSWIZ ( -- )
   MMA-XSWIZ @ 0= if exit then
   MMA-PAD @ 0= 0= if E-MMA-XSWIZ throw then                    \ swizzle is pad-free: MMA-PAD must be 0
   MMA-LMODE @ 2 = 0= if E-MMA-XSWIZ throw then                 \ ldmatrix-A read only (scalar reads are not swizzled)
   MMA-HALF? if E-MMA-XSWIZ throw then                          \ tf32 only (half As store is the un-swizzled F16 word)
   MMA-BK @ 32 <  MMA-BK @ 64 >  or if E-MMA-XSWIZ throw then   \ 32<=BK<=64: ACPR in [8,16] for full 8-row separation + mask invariance
   MMA-ABLATE @ if E-MMA-XSWIZ throw then ;                     \ ablation is the numerically-wrong tf32 feed

: MMA-BODY ( -- )
   MMA-CHECK-BN                                                     \ BN geometry gate first: NTILES-derived checks below assume a legal BN
   MMA-CHECK-DTYPE                                                  \ half semantic gate: reject tf32-only feed knobs (BLDM/LMODE/ablate)
   MMA-CHECK-BTF16                                                  \ before MMA-CHECK-BLDM, whose dtype-aware BTROW-B assumes a legal dtype
   MMA-CHECK-SMEM
   MMA-CHECK-BLDM
   MMA-CHECK-WARPS
   MMA-CHECK-EPI
   MMA-CHECK-REGS                                                   \ per-lane accumulators under the 255-register ceiling
   MMA-CHECK-GROUP                                                  \ grouped-raster group height positive (0 = off)
   MMA-CHECK-XSWIZ                                                  \ XOR-swizzle legality: pad=0 + ldmatrix-A + tf32 + 32<=BK<=64, no ablate
   MMA-WIDE? if                                                     \ WIDE path (MFRAGS>1 or BN>64); MMA-KTILE-DISPATCH picks wide + dtype
      MMA-THREAD-SETUP-WIDE  MMA-ACC-ZERO-WIDE  MMA-SETUP-WIDE
      MMA-STAGES @ 2 > if MMA-PIPE-KLOOP-MULTI else [: MMA-KTILE-DISPATCH ;] MMA-KLOOP then
      MMA-EPILOG @ if MMA-EPI-STORE else MMA-STORE-WIDE then  exit
   then
   MMA-GROUP @ if MMA-THREAD-SETUP-GROUP else MM-THREAD-SETUP then  \ grouped-raster non-wide prologue (OFF = byte-identical MM-THREAD-SETUP)
   MM-ACC-ZERO-EMIT
   MMA-SETUP
   MMA-STAGES @ 2 > if MMA-PIPE-KLOOP-MULTI else [: MMA-KTILE-DISPATCH ;] MMA-KLOOP then
   MMA-EPILOG @ if MMA-EPI-STORE else MMA-STORE then ;

: EMIT-MATMUL-MMA ( -- )
   PTX-HEADER  PTX-NL
   MMA-DYNSMEM @ if
      s" .extern .shared .align 16 .b8 SH[];" PTX-L        \ module-scope dynamic .shared (sized at launch)
   then
   SB-RESET s" .visible .entry MMM(.param .u64 pA,.param .u64 pB,.param .u64 pC,.param .u32 pM,.param .u32 pN,.param .u32 pK" SB-APPEND
   s" )" SB-APPEND SB$ PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L
   SB-RESET s" .reg .f32 %f<" SB-APPEND MMA-FREGS SB-U s" >;" SB-APPEND SB$ PTX-L   \ 48 at MFRAGS<=2 (byte-identical)
   SB-RESET s" .reg .b32 %r<" SB-APPEND MMA-RREGS SB-U s" >;" SB-APPEND SB$ PTX-L   \ 64 at MFRAGS<=2 (byte-identical)
   s" .reg .b64 %rd<48>;" PTX-L
   MMA-DYNSMEM @ 0= if
      SB-RESET s" .shared .align 16 .b8 SH[" SB-APPEND MMA-SH-BYTES SB-U s" ];" SB-APPEND SB$ PTX-L
   then
   MM-PARAMS
   MMA-BODY
   s" ret;" PTX-L  s" }" PTX-L ;
