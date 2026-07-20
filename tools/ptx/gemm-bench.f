\ gemm-bench.f - measure NAIVE vs REGISTER-BLOCKED SGEMM on square GEMMs (CUDA events).
\
\ docs/compute-campaign.md step 1: the FIRST measured GEMM baseline. Two kernels, same ABI
\ (pA,pB,pC,pM,pN,pK), each emitted once (M/N/K are runtime params), ptxas-assembled,
\ then timed per shape with CUDA events (tools/ptx/bench.f PTXBENCH, extended with the
\ 2D block/grid the tiles need) and reported as GFLOP/s (2*M*N*K flops per launch):
\   MMN (lib/ptx/cg-matmul-naive.f) - one element/thread, global K-loop; the pre-blocking
\       baseline and the same algorithm the lower-mm.f naive fallback tile emits.
\   MM  (lib/ptx/cg-matmul.f)       - register-blocked 64x64 tile, shared As/Bs staging,
\       4x4 accumulators/thread; the perf tile the lower-mm.f blocked path emits.
\ A=B=1.0, C=0 (values are immaterial to timing). Shapes 512/1024/2048 are multiples of
\ 64/16, valid for both tiles. Device-only: off the Orin (no libcuda) GB-ALL SKIPS so the
\ file still check-loads. Run on the Orin: scp to zed:Work/habu then
\ `bin/hb --load tools/ptx/gemm-bench.f`. The Triton column is docs/eval-triton.md (external).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-matmul-naive.f
require lib/ptx/cg-mma.f
require lib/ptx/toolchain.f
require maki/eval/active-target.f
require tools/ptx/profile.f
require tools/ptx/bench.f

package GEMMBENCH

16 constant GB-BLK                     \ block = 16x16 = 256 threads (both kernels)
$3F800000 constant GB-ONE             \ 1.0f bit pattern (A/B fill)
$3C003C00 constant GB-HALF-ONE        \ two packed 1.0h (f16) bit patterns (fp16 A/B fill; values immaterial to timing)
$3F803F80 constant GB-BF16-ONE        \ two packed 1.0 bf16 bit patterns (bf16 A/B fill; values immaterial to timing)

create GB-QO $1000 allot  create GB-QE $1000 allot
variable GB-DA  variable GB-DB  variable GB-DC
variable GB-NV                         \ M=N=K (square) as the u32 kernel param
variable GB-OT                         \ output-tile edge (grid X / N cols): 64 (MM/MMM blocked) / 16 (MMN naive)
variable GB-OTY                        \ output-tile M edge (grid Y): = GB-OT, except 64*MFRAGS for the wider-M MMM tile
variable GB-SMEM-DYN                    \ dynamic .shared bytes for the launch (0 = static)
variable GB-BLKY                       \ block Y dim: 16 (256 thr, 8-warp) / 8 (128 thr, 4-warp MMM tile)
variable GB-DWS                        \ split-K partials workspace (M*N*S f32)
variable GB-SV                         \ split-K split count S
64 GB-OTY !  16 GB-BLKY !

: GB-INT. ( n -- )  SB-RESET SB-INT SB$ type ;
: GB-HALF-PAT ( -- n )  MMA-BF16? if GB-BF16-ONE else GB-HALF-ONE then ;   \ packed 1.0 half pair for the dtype (fp16/bf16)

: GB-ASSEMBLE ( -- )                   \ captured PTX -> kernel.ptx -> ptxas cubin (die on rc<>0)
   ATGT:LABEL$ PTXTC:TC-ARCH!          \ assembler arch from the probed active target (sm_87 Orin / sm_121a GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   GB-QO $1000 >LEN GB-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" gemm-bench: ptxas failed" 1 die then ;

: GB-ALLOC ( n -- ) {: s:n :}          \ alloc + fill A=B=1.0, C=0 for an s x s GEMM
   s s * {: e:n :}
   MMA-ESZ {: esz:n :}                  \ A/B element bytes: 4 tf32 / 2 fp16 (C stays f32)
   e esz * GB-DA PTXBENCH:DEVICE-ALLOC
   e esz * GB-DB PTXBENCH:DEVICE-ALLOC
   e 4 * GB-DC PTXBENCH:DEVICE-ALLOC
   MMA-HALF? if                         \ fp16/bf16: fill halves via a packed-pair 32-bit memset (e/2 words = e halves)
      GB-DA @ GB-HALF-PAT e 2 / PTXBENCH:DEVICE-MEMSET32
      GB-DB @ GB-HALF-PAT e 2 / PTXBENCH:DEVICE-MEMSET32
   else
      GB-DA @ GB-ONE e PTXBENCH:DEVICE-MEMSET32
      GB-DB @ GB-ONE e PTXBENCH:DEVICE-MEMSET32
   then
   GB-DC @ 0     e PTXBENCH:DEVICE-MEMSET32 ;

: GB-PARAMS ( n -- ) {: s:n :}         \ 2D grid = (s/tile)^2 output tiles, 16x16 block
   s GB-NV !
   GB-BLK PTXBENCH:BLOCK!        GB-BLKY @ PTXBENCH:BLOCKY!   \ blockY = 16 (256 thr) / 8 (128 thr, 4-warp)
   s GB-OT @ / PTXBENCH:GRID!    s GB-OTY @ / PTXBENCH:GRIDY!   \ gridY = M/block-rows (GB-OTY = WROWS*16*MFRAGS for wide)
   36 PTXBENCH:PARAM-BYTES!
   GB-SMEM-DYN @ PTXBENCH:SHARED!         \ dynamic .shared tile (0 = static)
   PTXBENCH:PREPARE-LAUNCH
   0  GB-DA PTXBENCH:PARAM-PTR!
   8  GB-DB PTXBENCH:PARAM-PTR!
   16 GB-DC PTXBENCH:PARAM-PTR!
   24 GB-NV PTXBENCH:PARAM-U32!         \ M
   28 GB-NV PTXBENCH:PARAM-U32!         \ N
   32 GB-NV PTXBENCH:PARAM-U32! ;       \ K

: GB-FREE ( -- )
   GB-DA @ 0 <> if GB-DA @ PTXBENCH:DEVICE-FREE then
   GB-DB @ 0 <> if GB-DB @ PTXBENCH:DEVICE-FREE then
   GB-DC @ 0 <> if GB-DC @ PTXBENCH:DEVICE-FREE then
   0 GB-DA !  0 GB-DB !  0 GB-DC ! ;

: GB-FLOPS ( n n -- n ) {: s:n it:n :}  s s * s * 2 * it * ;   \ 2 s^3 per matmul
: GB-BYTES ( n n -- n ) {: s:n it:n :}  s s * MMA-ESZ 2 * 4 + * it * ;   \ A+B read (esz B each) + C write (4 B); 12 tf32 / 8 fp16

: GB-SHAPE ( n n -- ) {: s:n it:n :}
   it PTXBENCH:ITERS!
   s GB-ALLOC  s GB-PARAMS
   PTXBENCH:BENCH-GPU-NS {: ns:n :}
   s" GEMM " type s GB-INT. s" x" type s GB-INT. s" x" type s GB-INT.
   s"  iters=" type it GB-INT. cr
   s s * PTXBENCH:WORK!
   s it GB-BYTES  s it GB-FLOPS  ns PTXBENCH:REPORT-GPU
   GB-FREE ;

: GB-SHAPES ( -- )
   512  400 GB-SHAPE
   1024 200 GB-SHAPE
   2048 80  GB-SHAPE
   4096 40  GB-SHAPE ;

\ ---- SPLIT-K timing (dot habu-split-k-for-8c3ee0fb): the full split GEMM cost = MMM (partials) + MMR
\ (reduce), both timed per iter via PTXBENCH:BENCH-GPU-NS-2. GFLOP/s = 2*S^3 / (MMM+MMR time) - the useful
\ matmul flops over the honest wall time INCLUDING the reduce overhead. A=B=1 (values immaterial to timing).
\ Workspace M*N*S*4 is fail-closed sized via CUDA:SPLIT-WS-ALLOC. Both funcs' params are pre-set on their
\ handles so the timed loop only issues the two cuLaunchGrid calls. ----
: GB-SHAPE-SPLIT ( n n -- ) {: n:n it:n :}
   it PTXBENCH:ITERS!
   n GB-ALLOC                                     \ DA,DB,DC + fill A=B=1 (f32/half per dtype)
   n n GB-SV @ CUDA:SPLIT-WS-ALLOC CUDA-DEVPTR>N GB-DWS !
   n GB-NV !
   \ --- pre-set MMR (reduce) params on the MMR handle ---
   s" MMR" PTXBENCH:FUNC-BY-NAME!
   256 PTXBENCH:BLOCK!  1 PTXBENCH:BLOCKY!
   n n * 255 + 256 / PTXBENCH:GRID!  1 PTXBENCH:GRIDY!  28 PTXBENCH:PARAM-BYTES!  0 PTXBENCH:SHARED!
   PTXBENCH:PREPARE-LAUNCH
   0 GB-DC PTXBENCH:PARAM-PTR!  8 GB-DWS PTXBENCH:PARAM-PTR!
   16 GB-NV PTXBENCH:PARAM-U32!  20 GB-NV PTXBENCH:PARAM-U32!  24 GB-SV PTXBENCH:PARAM-U32!
   s" MMR" PTXBENCH:FUNC2-BY-NAME!                \ FUNC2 = MMR (params now set)
   n n * 255 + 256 / PTXBENCH:GRID2!  1 PTXBENCH:GRIDY2!
   \ --- pre-set MMM (partials) params on the MMM handle ---
   s" MMM" PTXBENCH:FUNC-BY-NAME!
   GB-BLK PTXBENCH:BLOCK!  GB-BLKY @ PTXBENCH:BLOCKY!
   n MMA-BN @ / PTXBENCH:GRID!  n MMA-BROWS / GB-SV @ * PTXBENCH:GRIDY!   \ gridY = (M/BROWS)*S
   48 PTXBENCH:PARAM-BYTES!  GB-SMEM-DYN @ PTXBENCH:SHARED!
   PTXBENCH:PREPARE-LAUNCH
   0 GB-DA PTXBENCH:PARAM-PTR!  8 GB-DB PTXBENCH:PARAM-PTR!  16 GB-DC PTXBENCH:PARAM-PTR!
   24 GB-NV PTXBENCH:PARAM-U32!  28 GB-NV PTXBENCH:PARAM-U32!  32 GB-NV PTXBENCH:PARAM-U32!
   36 GB-SV PTXBENCH:PARAM-U32!  40 GB-DWS PTXBENCH:PARAM-PTR!
   PTXBENCH:BENCH-GPU-NS-2 {: ns:n :}
   s" GEMM(split=" type GB-SV @ GB-INT. s" ) " type n GB-INT. s" x" type n GB-INT. s" x" type n GB-INT.
   s"  iters=" type it GB-INT. cr
   n n * PTXBENCH:WORK!
   n it GB-BYTES  n it GB-FLOPS  ns PTXBENCH:REPORT-GPU
   GB-DWS @ PTXBENCH:DEVICE-FREE  GB-FREE ;

: GB-SPLIT-SHAPES ( -- )                          \ the occupancy-hole shapes the split targets
   512  400 GB-SHAPE-SPLIT
   1024 200 GB-SHAPE-SPLIT ;

: GB-KERNEL ( ptr u8 n -- ) {: ka:ptr ku:n :}   \ load the named kernel from PTXTC:CUBIN$, bench
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   ka ku PTXBENCH:KERNEL!  ka ku PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   GB-SHAPES
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE ;

: GB-KERNEL-SPLIT ( -- )               \ load MMM (+ MMR in the same module), time the split shapes
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   s" MMM" PTXBENCH:KERNEL!  s" MMM" PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   GB-SPLIT-SHAPES
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE ;

\ SPLIT-K bench config (dot habu-split-k-for-8c3ee0fb): emit the split MMM+MMR module, time MMM+MMR per iter
\ at 512/1024. Element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-SPLIT). Args: bk pad stages dyn mode
\ mfrags warps bn split. Restores the tf32 8-warp BN=64 S=1 default.
: GB-MMM-CFG-SPLIT ( n n n n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n mfrags:n warps:n bn:n split:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mode MMA-LMODE !  mfrags MMA-MFRAGS !  warps MMA-WARPS !  bn MMA-BN !  split MMA-SPLITK !
   split GB-SV !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == SPLIT-K S=" type split GB-INT.  s"  BN=" type bn GB-INT.  s"  MFRAGS=" type mfrags GB-INT.  s"  warps=" type warps GB-INT.
   s"  BK=" type bk GB-INT.  s"  stages=" type stages GB-INT.  s"  dyn=" type dyn GB-INT.  s"  mode=" type mode GB-INT.
   s"  block=" type MMA-BROWS GB-INT. s" x" type bn GB-INT.  s"  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   bn GB-OT !  MMA-BROWS GB-OTY !
   GB-KERNEL-SPLIT
   PTXTC:CLEAN
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  1 MMA-MFRAGS !  8 MMA-WARPS !  64 MMA-BN !  1 MMA-SPLITK !
   0 GB-SMEM-DYN !  64 GB-OT !  64 GB-OTY !  16 GB-BLKY !  1 GB-SV ! ;

: GB-MMN ( -- )                        \ naive baseline column
   s" == MMN naive (1 elem/thread, global K-loop) ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-NAIVE  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   16 GB-OT !  16 GB-OTY !
   s" MMN" GB-KERNEL
   PTXTC:CLEAN ;

: GB-MM ( -- )                         \ register-blocked column (FP32 CUDA-core roof)
   s" == MM register-blocked 64x64 (4x4 micro-tile/thread, shared staging) ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  64 GB-OTY !
   s" MM" GB-KERNEL
   PTXTC:CLEAN ;

: GB-MMM-MODE ( n -- ) {: mode:n :}    \ TF32 tensor-core column for one fragment-load mode
   mode MMA-LMODE !
   s" == MMM tensor-core TF32 mma.sync 64x64, fragment mode " type mode .
   mode 0= if s" (scalar+cvt baseline) ==" type then
   mode 1 = if s" (scalar raw, no cvt) ==" type then
   mode 2 = if s" (ldmatrix.x4 A + raw B, no cvt) ==" type then
   cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  64 GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN ;

: GB-MMM ( -- )                        \ sweep the 3 fragment-load modes (ablation); restore baseline default
   0 GB-MMM-MODE  1 GB-MMM-MODE  2 GB-MMM-MODE
   0 MMA-LMODE ! ;

\ one larger-BK / swizzled tile config (dot habu-mma-larger-bk). Sets the cg-mma.f tile knobs and,
\ for a dynamic-smem tile, the launch .shared size; emits, assembles, benches all shapes; restores.
: GB-MMM-CFG ( n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mode MMA-LMODE !
   MMA-DYNSMEM @ if MMA-SMEM else 0 then GB-SMEM-DYN !
   s" == MMM BK=" type bk GB-INT. s"  pad=" type pad GB-INT. s"  stages=" type stages GB-INT.
   s"  dyn=" type dyn GB-INT. s"  frag=" type mode GB-INT. s"  ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  64 GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  0 GB-SMEM-DYN ! ;

\ WIDER-M register-tile config (dot habu-mma-amortize-the): each warp owns MFRAGS stacked
\ 16-row M-frags so the block is 64*MFRAGS x 64 and grid Y = s / (64*MFRAGS). Amortizes the
\ B-side fragment feed (each B fragment reused across MFRAGS M-frags). Restores MFRAGS=1.
: GB-MMM-CFGW ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n mfrags:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mode MMA-LMODE !  mfrags MMA-MFRAGS !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !   \ MMA-SH-BYTES = MMA-SMEM off, epilogue-grown on
   s" == MMM-WIDE MFRAGS=" type mfrags GB-INT. s"  BK=" type bk GB-INT. s"  pad=" type pad GB-INT.
   s"  stages=" type stages GB-INT. s"  dyn=" type dyn GB-INT. s"  frag=" type mode GB-INT.
   s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B  ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  1 MMA-MFRAGS !
   0 GB-SMEM-DYN !  64 GB-OTY ! ;

\ B-SIDE ldmatrix wide config (dot habu-mma-wave-3): the transposed-Bs staging + ldmatrix.x2 B fragments
\ replace the per-n-tile scalar B feed. bpad is the BT n-major row pad (measured bank-geometry knob).
\ Restores MFRAGS=1 / BLDM=0 / BPAD=0.
: GB-MMM-CFGW-B ( n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mfrags:n bpad:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  2 MMA-LMODE !  mfrags MMA-MFRAGS !
   1 MMA-BLDM !  bpad MMA-BPAD !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !   \ MMA-SH-BYTES = MMA-SMEM off, epilogue-grown on
   s" == MMM-WIDE-B MFRAGS=" type mfrags GB-INT. s"  BK=" type bk GB-INT. s"  pad=" type pad GB-INT.
   s"  bpad=" type bpad GB-INT. s"  stages=" type stages GB-INT. s"  dyn=" type dyn GB-INT.
   s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B  ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  1 MMA-MFRAGS !
   0 MMA-BLDM !  0 MMA-BPAD !  0 GB-SMEM-DYN !  64 GB-OTY ! ;

\ 4-WARP (2x2 warp grid, 128-thread) bench configs (dot habu-4-warp-mma). The narrower grid halves the
\ per-block threads and smem at the SAME per-warp fragment/accumulator/store maps, so more blocks co-reside
\ per SM under the 99 KB cap (Triton's per-shape tf32 winners run this BM128xBN64 4-warp blocking,
\ docs/eval-triton.md GB10). Set MMA-WARPS=4 + block Y = 8 (128 threads), delegate to the shared wide-config
\ machinery, then restore the 8-warp default. Every config is element-exact first (tools/ptx/mma-gemm-check.f).
: GB-MMM-CFGW4 ( n n n n n n -- )                      \ bk pad stages dyn mode mfrags, 4-warp A-ldmatrix/scalar
   4 MMA-WARPS !  8 GB-BLKY !  s" -- 4-WARP: " type
   GB-MMM-CFGW  8 MMA-WARPS !  16 GB-BLKY ! ;
: GB-MMM-CFGW4-B ( n n n n n n -- )                    \ bk pad stages dyn mfrags bpad, 4-warp B-ldmatrix
   4 MMA-WARPS !  8 GB-BLKY !  s" -- 4-WARP: " type
   GB-MMM-CFGW-B  8 MMA-WARPS !  16 GB-BLKY ! ;

\ SHARED-MEMORY EPILOGUE bench wrappers (dot habu-shared-mem-epilogue): the SAME tile with MMA-EPILOG=1,
\ so the coalesced smem C store replaces the scattered per-lane global store. SH is sized to MMA-SH-BYTES
\ (staging may exceed the pipeline buffer, e.g. the 4-warp M4 static tile grows 28672->32768 B). Every
\ config is element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-*-EPI). Restore MMA-EPILOG=0.
: GB-MMM-CFGW4-EPI ( n n n n n n -- )                  \ bk pad stages dyn mode mfrags, 4-warp + epilogue
   1 MMA-EPILOG !  4 MMA-WARPS !  8 GB-BLKY !  s" -- 4-WARP EPILOGUE: " type
   GB-MMM-CFGW  8 MMA-WARPS !  16 GB-BLKY !  0 MMA-EPILOG ! ;
: GB-MMM-CFGW-B-EPI ( n n n n n n -- )                 \ bk pad stages dyn mfrags bpad, 8-warp B-ldmatrix + epilogue
   1 MMA-EPILOG !  s" -- EPILOGUE: " type
   GB-MMM-CFGW-B  0 MMA-EPILOG ! ;

\ XOR-SWIZZLE bench wrappers (dot habu-xor-swizzle-mma): the pad-free XSWIZ variant of each committed
\ ldmatrix-A tile - pad forced 0, MMA-XSWIZ=1, mode=2 (the swizzle's wired A feed). GB-SMEM-DYN follows
\ MMA-SH-BYTES, which shrinks (As row 160B->128B, no pad) so a dynamic tile launches with LESS shared - the
\ whole point. Delegates to the existing wide machinery, then restores XSWIZ=0. Element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-XSWIZ*, 0 mismatches).
: GB-MMM-CFGW-X ( n n n -- ) {: stages:n dyn:n mfrags:n :}         \ 8-warp XSWIZ ldmatrix-A (pad-free)
   1 MMA-XSWIZ !  s" -- XSWIZ: " type  32 0 stages dyn 2 mfrags GB-MMM-CFGW  0 MMA-XSWIZ ! ;
: GB-MMM-CFGW4-X ( n n n -- ) {: stages:n dyn:n mfrags:n :}        \ 4-warp XSWIZ ldmatrix-A
   1 MMA-XSWIZ !  s" -- XSWIZ " type  32 0 stages dyn 2 mfrags GB-MMM-CFGW4  0 MMA-XSWIZ ! ;
: GB-MMM-CFGW4-EPI-X ( n n n -- ) {: stages:n dyn:n mfrags:n :}    \ 4-warp XSWIZ + epilogue
   1 MMA-XSWIZ !  s" -- XSWIZ " type  32 0 stages dyn 2 mfrags GB-MMM-CFGW4-EPI  0 MMA-XSWIZ ! ;
: GB-MMM-CFGW-B-X ( n n n n -- ) {: stages:n dyn:n mfrags:n bpad:n :}      \ 8-warp XSWIZ + B-ldmatrix
   1 MMA-XSWIZ !  s" -- XSWIZ: " type  32 0 stages dyn mfrags bpad GB-MMM-CFGW-B  0 MMA-XSWIZ ! ;
: GB-MMM-CFGW-B-EPI-X ( n n n n -- ) {: stages:n dyn:n mfrags:n bpad:n :}  \ 8-warp XSWIZ + B-ldmatrix + epilogue
   1 MMA-XSWIZ !  s" -- XSWIZ: " type  32 0 stages dyn mfrags bpad GB-MMM-CFGW-B-EPI  0 MMA-XSWIZ ! ;

\ WIDE-BN bench config (dot habu-widen-bn-past): the BN=128/256 tile - the 4096-class geometry Triton's
\ per-shape winners use (2048 = BM64xBN128, 4096 = BM128xBN256, docs/eval-triton.md GB10 sweep). Sets BN +
\ warp grid + wide knobs and the N-tile grid edge (GB-OT = BN, so gridX = s/BN). Element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-BN, 0 mismatches). Restores the tf32 8-warp BN=64 default.
: GB-MMM-CFG-BN ( n n n n n n n -- ) {: bk:n pad:n stages:n dyn:n mode:n mfrags:n bn:n :}
   bk MMA-BK !  pad MMA-PAD !  stages MMA-STAGES !  dyn MMA-DYNSMEM !  mode MMA-LMODE !  mfrags MMA-MFRAGS !  bn MMA-BN !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == WIDE-BN MMM BN=" type bn GB-INT.  s"  MFRAGS=" type mfrags GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type bk GB-INT.  s"  pad=" type pad GB-INT.  s"  stages=" type stages GB-INT.  s"  dyn=" type dyn GB-INT.
   s"  frag=" type mode GB-INT.  s"  block=" type MMA-BROWS GB-INT. s" x" type bn GB-INT.  s"  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   bn GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !  1 MMA-MFRAGS !  8 MMA-WARPS !  64 MMA-BN !
   0 GB-SMEM-DYN !  64 GB-OT !  64 GB-OTY !  16 GB-BLKY ! ;
: GB-MMM-CFG-BN4 ( n n n n n n n -- )                  \ 4-warp (2x2 grid) wide-BN
   4 MMA-WARPS !  8 GB-BLKY !  s" -- 4-WARP: " type
   GB-MMM-CFG-BN  8 MMA-WARPS !  16 GB-BLKY ! ;

\ GROUPED-RASTER bench wrappers (dot habu-grouped-raster-cta): set MMA-GROUP (the CTA-order swizzle),
\ delegate to the wide-BN / 4-warp bench config, restore GROUP=0. GROUP=0 emits byte-identically (no remap),
\ so the OFF row of each crossing is the committed tile; a positive height re-emits with the prologue remap.
: GB-MMM-CFG-BN-G ( n n n n n n n n -- ) {: bk pad stages dyn mode mfrags bn group :}    \ 8-warp wide-BN + grouped-raster
   group MMA-GROUP !  s" -- GROUP=" type group GB-INT. s" : " type
   bk pad stages dyn mode mfrags bn GB-MMM-CFG-BN
   0 MMA-GROUP ! ;
: GB-MMM-CFG-BN4-G ( n n n n n n n n -- ) {: bk pad stages dyn mode mfrags bn group :}   \ 4-warp wide-BN + grouped-raster
   group MMA-GROUP !  s" -- GROUP=" type group GB-INT. s" : " type
   bk pad stages dyn mode mfrags bn GB-MMM-CFG-BN4
   0 MMA-GROUP ! ;
: GB-MMM-CFGW4-G ( n n n n n n n -- ) {: bk pad stages dyn mode mfrags group :}          \ 4-warp BN=64 wide + grouped-raster (512^3 retime)
   group MMA-GROUP !  s" -- GROUP=" type group GB-INT. s" : " type
   bk pad stages dyn mode mfrags GB-MMM-CFGW4
   0 MMA-GROUP ! ;

\ the raised-BK / bank-swizzled configuration space (all element-exact per tools/ptx/mma-gemm-check.f)
: GB-MMM-SWEEP ( -- )
   32 0 2 0 0 GB-MMM-CFG               \ committed default baseline (BK=32, stages=2, scalar+cvt) - A/B reference
   64 0 1 0 0 GB-MMM-CFG               \ BK=64 single-buffer static, scalar+cvt
   64 0 2 1 0 GB-MMM-CFG               \ BK=64 double-buffer dynamic .shared, scalar+cvt
   64 0 2 1 2 GB-MMM-CFG               \ BK=64 double-buffer dynamic, ldmatrix
   32 8 2 0 2 GB-MMM-CFG               \ BK=32 padded (bank-swizzled As), ldmatrix
   64 8 2 1 2 GB-MMM-CFG ;             \ BK=64 padded double-buffer dynamic, ldmatrix

\ wider-M B-feed-amortization sweep (dot habu-mma-amortize-the): same-session A/B against the
\ pinned swizzled baselines, then the 128x64 wide tiles. All element-exact per mma-gemm-check.f.
: GB-MMM-WIDE-SWEEP ( -- )
   32 0 2 0 0 GB-MMM-CFG               \ BK=32 baseline (same-session A/B reference)
   32 8 2 0 2 GB-MMM-CFG               \ MMM-SWZ 64x64 ldmatrix (static, prior best-fits-static)
   64 8 2 1 2 GB-MMM-CFG               \ MMM-SWZ-BK64 (shipped best, 1369.6)
   32 8 2 1 2 2 GB-MMM-CFGW            \ WIDE MFRAGS=2 BK=32 pad=8 stages=2 DYNAMIC ldmatrix (128x64, 57344 B) - parity 2133.9
   32 8 1 0 2 2 GB-MMM-CFGW            \ WIDE MFRAGS=2 BK=32 pad=8 stages=1 STATIC ldmatrix (128x64, 28672 B)
   32 8 2 1 2 4 GB-MMM-CFGW            \ WIDE MFRAGS=4 BK=32 pad=8 stages=2 DYNAMIC ldmatrix (256x64, 98304 B) - dot habu-mma-wave-2
   32 8 1 0 2 4 GB-MMM-CFGW ;          \ WIDE MFRAGS=4 BK=32 pad=8 stages=1 STATIC ldmatrix (256x64, 49152 B) - single-buffer occupancy variant

\ B-side-ldmatrix sweep (dot habu-mma-wave-3): same-session A/B against the MFRAGS=4 scalar-B winner
\ (2707.3), then the transposed-Bs B-ldmatrix at bpad {0,4} single-buffer and bpad=4 double-buffer.
: GB-MMM-WIDE-B-SWEEP ( -- )
   32 0 2 0 0 GB-MMM-CFG               \ BK=32 baseline (same-session A/B reference)
   32 8 1 0 2 4 GB-MMM-CFGW            \ MFRAGS=4 stages=1 STATIC scalar-B ldmatrix-A (the 2707.3 winner reference)
   32 8 1 1 4 0 GB-MMM-CFGW-B          \ MFRAGS=4 bpad=0 stages=1 DYN B-ldmatrix (256x64; bank-aliased read, budget test)
   32 8 1 1 4 4 GB-MMM-CFGW-B          \ MFRAGS=4 bpad=4 stages=1 DYN B-ldmatrix (256x64; conflict-free read stride 36)
   32 8 2 1 4 4 GB-MMM-CFGW-B          \ MFRAGS=4 bpad=4 stages=2 DYN double-buffer B-ldmatrix (256x64)
   32 8 2 1 2 4 GB-MMM-CFGW-B          \ MFRAGS=2 bpad=4 stages=2 DYN B-ldmatrix (128x64)
   32 8 1 1 2 4 GB-MMM-CFGW-B ;        \ MFRAGS=2 bpad=4 stages=1 DYN single-buffer B-ldmatrix (128x64; GB10 1024^3 winner)

\ FP16 tile bench config (dot habu-fp16-mma-tile): MMA-DTYPE=1, m16n8k16 f16.f16.f32, scalar packed-b32
\ feed (mode 0). A/B fill as f16 (GB-ALLOC), C f32; GFLOP/s = 2 s^3 as for tf32. Every config is
\ element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-F16). Args: bk pad stages dyn mfrags warps
\ epilog. Sets block Y from the warp count and the dynamic .shared size, then restores the tf32 default.
: GB-MMM-CFG-F16 ( n n n n n n n -- )
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   1 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-BLDM !  0 MMA-BPAD !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !          \ 16 (256 thr, 8-warp) / 8 (128 thr, 4-warp)
   s" == FP16 MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.  s"  dyn=" type MMA-DYNSMEM @ GB-INT.
   s"  epi=" type MMA-EPILOG @ GB-INT.  s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;

\ FP16 TRANSPOSED-Bs feed bench config (dot habu-fp16-transposed-bs): MMA-DTYPE=1 + MMA-BTF16=1 - the
\ n-major BT staging + one-b32-per-register B feed. Same fill/timing as GB-MMM-CFG-F16 (A=B=1.0, values
\ immaterial). Element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-F16-T). Args: bk pad stages dyn
\ mfrags warps epilog bpad. Restores the tf32 8-warp default.
: GB-MMM-CFG-F16-T ( n n n n n n n n -- )
   MMA-BPAD !  MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   1 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-BLDM !  1 MMA-BTF16 !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == FP16-T MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  bpad=" type MMA-BPAD @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.
   s"  dyn=" type MMA-DYNSMEM @ GB-INT.  s"  epi=" type MMA-EPILOG @ GB-INT.
   s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-BTF16 !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 MMA-BPAD !  0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;

\ BF16 tile bench config (dot habu-bf16-m16n8k16-tile): MMA-DTYPE=2, m16n8k16 f32.bf16.bf16.f32, scalar
\ packed-b32 feed. The bf16 mirror of GB-MMM-CFG-F16 - identical geometry/timing, only the mma dtype token
\ and the A/B fill (bf16 1.0 pair, values immaterial to timing) differ. Element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-BF16). Args: bk pad stages dyn mfrags warps epilog. Restores the tf32 default.
: GB-MMM-CFG-BF16 ( n n n n n n n -- )
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   2 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-BLDM !  0 MMA-BPAD !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !          \ 16 (256 thr, 8-warp) / 8 (128 thr, 4-warp)
   s" == BF16 MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.  s"  dyn=" type MMA-DYNSMEM @ GB-INT.
   s"  epi=" type MMA-EPILOG @ GB-INT.  s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;

\ BF16 TRANSPOSED-Bs feed bench config (dot habu-bf16-m16n8k16-tile): MMA-DTYPE=2 + MMA-BTF16=1 - the bf16
\ mirror of GB-MMM-CFG-F16-T. Same fill/timing (A=B=1.0, values immaterial). Element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-BF16-T). Args: bk pad stages dyn mfrags warps epilog bpad. Restores default.
: GB-MMM-CFG-BF16-T ( n n n n n n n n -- )
   MMA-BPAD !  MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   2 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-BLDM !  1 MMA-BTF16 !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == BF16-T MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  bpad=" type MMA-BPAD @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.
   s"  dyn=" type MMA-DYNSMEM @ GB-INT.  s"  epi=" type MMA-EPILOG @ GB-INT.
   s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-BTF16 !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 MMA-BPAD !  0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;

\ HALF ldmatrix feed bench config (dot habu-half-precision-ldmatrix): MMA-DTYPE=1/2 + MMA-LMODE=2 - ONE
\ ldmatrix.m8n8.x4.b16 fills A and ONE ldmatrix.x2.trans.b16 fills B from the k-major As/Bs. Mirror of
\ GB-MMM-CFG-F16/BF16 (same fill/timing, A=B=1.0), only LMODE=2. Element-exact first (MGC-CFG-F16-LDM /
\ MGC-CFG-BF16-LDM). Args: bk pad stages dyn mfrags warps epilog. Restores the tf32 8-warp default.
: GB-MMM-CFG-F16-LDM ( n n n n n n n -- )
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   1 MMA-DTYPE !  2 MMA-LMODE !  0 MMA-BLDM !  0 MMA-BTF16 !  0 MMA-BPAD !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == FP16-LDM MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.  s"  dyn=" type MMA-DYNSMEM @ GB-INT.
   s"  epi=" type MMA-EPILOG @ GB-INT.  s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;
: GB-MMM-CFG-BF16-LDM ( n n n n n n n -- )   \ bf16 mirror (MMA-DTYPE=2)
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   2 MMA-DTYPE !  2 MMA-LMODE !  0 MMA-BLDM !  0 MMA-BTF16 !  0 MMA-BPAD !
   MMA-DYNSMEM @ if MMA-SH-BYTES else 0 then GB-SMEM-DYN !
   MMA-NTHREADS 16 / GB-BLKY !
   s" == BF16-LDM MMM MFRAGS=" type MMA-MFRAGS @ GB-INT.  s"  warps=" type MMA-WARPS @ GB-INT.
   s"  BK=" type MMA-BK @ GB-INT.  s"  stages=" type MMA-STAGES @ GB-INT.  s"  dyn=" type MMA-DYNSMEM @ GB-INT.
   s"  epi=" type MMA-EPILOG @ GB-INT.  s"  block=" type MMA-BROWS GB-INT. s" x64  smem=" type MMA-SMEM GB-INT. s" B ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !  MMA-BROWS GB-OTY !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN
   0 MMA-DTYPE !  0 MMA-LMODE !  0 MMA-EPILOG !  32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  1 MMA-MFRAGS !  8 MMA-WARPS !
   0 GB-SMEM-DYN !  64 GB-OTY !  16 GB-BLKY ! ;

public

: GB-MMM-BENCH ( -- )                  \ FP32 roof reference + the larger-BK/swizzle sweep (focused)
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM
   GB-MMM-SWEEP ;

: GB-WIDE-BENCH ( -- )                 \ FP32 roof reference + swizzled baselines + wider-M B-feed sweep
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM
   GB-MMM-WIDE-SWEEP ;

: GB-WIDE-B-BENCH ( -- )               \ FP32 roof + MFRAGS=4 scalar-B winner + B-side ldmatrix sweep (dot habu-mma-wave-3)
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM
   GB-MMM-WIDE-B-SWEEP ;

\ SPLIT-K schedule sweep (dot habu-split-k-for-8c3ee0fb, phase 3a): the FP32 CUDA-core roof anchor, the
\ committed 512-class S=1 tiles as same-session baselines (GB-SHAPES 512/1024/2048/4096), then split S=2/4 on
\ those tiles at 512/1024 - where only 32/128 output blocks launch on 48 SMs and split multiplies the resident
\ block count. GFLOP/s = 2*S^3 / (MMM+MMR wall time), the honest throughput including the reduce pass. Every
\ split tile is element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-SPLIT). Best-of-3, run SOLO under the
\ pinned 13.3 ptxas (PTXAS-STALE-SM121 absent). Referee = Triton 3.8 tf32 tl.dot (docs/eval-triton.md GB10:
\ 21.7 / 33.5 at 512 / 1024).
: GB-SPLIT-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same-session clock anchor)
   s" == S=1 baselines (committed 512-class winners, same session) ==" type cr
   32 8 2 1 2 4 GB-MMM-CFGW4            \ 4-warp M4 s2 dyn ldmatrix-A (128x64) - committed 512 winner S=1
   s" == split-K S=2/4 on the 4-warp M4 (128x64) and 8-warp M2 (128x64) tiles ==" type cr
   32 8 2 1 2 4 4 64 2 GB-MMM-CFG-SPLIT   \ 4-warp M4 s2 dyn ldmatrix split=2
   32 8 2 1 2 4 4 64 4 GB-MMM-CFG-SPLIT   \ 4-warp M4 s2 dyn ldmatrix split=4
   32 8 2 1 2 2 8 64 2 GB-MMM-CFG-SPLIT   \ 8-warp M2 s2 dyn ldmatrix split=2
   32 8 2 1 2 2 8 64 4 GB-MMM-CFG-SPLIT ; \ 8-warp M2 s2 dyn ldmatrix split=4

\ GB10 head-to-head campaign (dot habu-gb10-gemm-head): FP32 CUDA-core roof reference, then the full
\ wider-M B-feed-amortization schedule sweep (scalar-B MFRAGS=2/4 and the B-ldmatrix transposed-Bs
\ configs incl. the mmm-wide-b-m4-s1 flagship) across 512/1024/2048/4096. tf32 tensor-core throughput
\ vs the source-built Triton 3.8 referee (docs/eval-triton.md GB10 section).
: GB-GB10 ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM
   GB-MMM-WIDE-SWEEP
   GB-MMM-WIDE-B-SWEEP
   32 8 1 0 2 4 GB-MMM-CFGW4            \ 4-warp MFRAGS=4 stages=1 static ldmatrix-A (128x64) - 1024^3/2048^3 winner
   32 8 2 1 2 4 GB-MMM-CFGW4 ;          \ 4-warp MFRAGS=4 stages=2 dyn ldmatrix-A (128x64) - 512^3 winner

\ WIDE-BN schedule sweep (dot habu-widen-bn-past): the FP32 CUDA-core roof reference, the committed BN=64
\ 2048/4096 tf32 winners as same-session anchors, then the BN=128/256 tiles - the wide-N geometry Triton's
\ per-shape winners use (2048 = BM64xBN128, 4096 = BM128xBN256, docs/eval-triton.md GB10 sweep). Every config
\ is element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-BN, 0 mismatches). Best-of-3 at the sustained
\ GEMM clock, run SOLO under the pinned 13.3 ptxas. Read the per-shape winner; the wide-N movers are expected
\ on the compute-bound 2048/4096 columns (512/1024 stay occupancy/launch-gated). Referee = Triton 3.8 tf32
\ tl.dot (docs/eval-triton.md GB10: 21.7 / 33.5 / 37.8 / 45.3 TFLOP/s).
: GB-BN-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same-session clock anchor)
   32 8 2 1 2 4 GB-MMM-CFGW             \ BN=64 8-warp MFRAGS=4 stages=2 dyn (256x64) - committed 2048 winner anchor
   32 8 1 1 4 4 GB-MMM-CFGW-B           \ BN=64 8-warp MFRAGS=4 bpad=4 stages=1 dyn B-ldmatrix (256x64) - committed 4096 winner anchor
   32 0 1 0 2 2 128 GB-MMM-CFG-BN4      \ BN=128 MFRAGS=2 4-warp stages=1 static ldmA (BM64xBN128; Triton 2048-winner geometry, 24 KB)
   32 0 1 0 2 2 128 GB-MMM-CFG-BN       \ BN=128 MFRAGS=2 8-warp stages=1 static ldmA (BM128xBN128, 32 KB)
   32 0 2 1 2 2 128 GB-MMM-CFG-BN       \ BN=128 MFRAGS=2 8-warp stages=2 dyn ldmA (BM128xBN128, 64 KB)
   32 0 1 1 2 4 128 GB-MMM-CFG-BN       \ BN=128 MFRAGS=4 8-warp stages=1 dyn ldmA (BM256xBN128, 128 accs)
   32 0 2 1 2 1 256 GB-MMM-CFG-BN       \ BN=256 MFRAGS=1 8-warp stages=2 dyn ldmA (BM64xBN256, 64 accs)
   32 0 2 1 2 2 256 GB-MMM-CFG-BN ;     \ BN=256 MFRAGS=2 8-warp stages=2 dyn ldmA (BM128xBN256; Triton 4096-winner geometry, 128 accs, 96 KB)

\ GROUPED-RASTER schedule sweep (dot habu-grouped-raster-cta, Round 10): the FP32 CUDA-core roof reference,
\ then swizzle OFF (GROUP=0, byte-identical committed tile) vs GROUP {4,8} crossed with (a) the BN=256 M2
\ 8-warp stages=2 tile (the current 4096 winner), (b) the BN=128 M2 4-warp DEEP stages 3/4 tile (24 KB/stage,
\ the N-stage ring makes depth pay), (c) the BK=16 BN=256 M2 8-warp stages-3 tile (24 KB/stage), and the
\ 512^3-winner 4-warp M4 stages=2 tile (expected neutral - occupancy-bound). Every tile is element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-GROUP, incl. a non-square grid + partial group). Best-of-3 at the
\ sustained clock, run SOLO under the pinned 13.3 ptxas; MEASUREMENT decides the 4096/2048 winner. All four
\ shapes run per config (so the 512^3 neutrality is read directly). Referee = Triton 3.8 tf32 45.3 at 4096^3.
: GB-GROUP-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same-session clock anchor)
   s" == (a) BN=256 M2 8-warp stages=2 dyn (128x256, current 4096 winner): swizzle OFF vs GROUP {4,8} ==" type cr
   32 0 2 1 2 2 256 0 GB-MMM-CFG-BN-G   \ GROUP=0 OFF (byte-identical committed tile)
   32 0 2 1 2 2 256 4 GB-MMM-CFG-BN-G   \ GROUP=4
   32 0 2 1 2 2 256 8 GB-MMM-CFG-BN-G   \ GROUP=8
   s" == (b) BN=128 M2 4-warp DEEP stages 3 & 4 (64x128, 24KB/stage): swizzle OFF vs GROUP {4,8} ==" type cr
   32 0 3 1 2 2 128 0 GB-MMM-CFG-BN4-G  \ stages=3 OFF
   32 0 3 1 2 2 128 4 GB-MMM-CFG-BN4-G  \ stages=3 GROUP=4
   32 0 3 1 2 2 128 8 GB-MMM-CFG-BN4-G  \ stages=3 GROUP=8
   32 0 4 1 2 2 128 0 GB-MMM-CFG-BN4-G  \ stages=4 OFF
   32 0 4 1 2 2 128 4 GB-MMM-CFG-BN4-G  \ stages=4 GROUP=4
   32 0 4 1 2 2 128 8 GB-MMM-CFG-BN4-G  \ stages=4 GROUP=8
   s" == (c) BK=16 BN=256 M2 8-warp stages=3 dyn (128x256, 24KB/stage): swizzle OFF vs GROUP {4,8} ==" type cr
   16 0 3 1 2 2 256 0 GB-MMM-CFG-BN-G   \ BK=16 stages=3 OFF
   16 0 3 1 2 2 256 4 GB-MMM-CFG-BN-G   \ BK=16 stages=3 GROUP=4
   16 0 3 1 2 2 256 8 GB-MMM-CFG-BN-G   \ BK=16 stages=3 GROUP=8
   s" == 512^3-winner re-time: 4-warp M4 stages=2 dyn (128x64): swizzle OFF vs GROUP {4,8} (expected neutral) ==" type cr
   32 8 2 1 2 4 0 GB-MMM-CFGW4-G        \ OFF
   32 8 2 1 2 4 4 GB-MMM-CFGW4-G        \ GROUP=4
   32 8 2 1 2 4 8 GB-MMM-CFGW4-G ;      \ GROUP=8


: GB-ALL ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MMN
   GB-MM
   GB-MMM
   GB-MMM-SWEEP ;

\ 4-WARP tile schedule sweep (dot habu-4-warp-mma): the FP32 roof reference, the two best 8-warp tf32
\ tiles as same-session A/B anchors, then the 4-warp (2x2 grid, 128-thread) candidates at MFRAGS 2/4,
\ stages 1/2, A-ldmatrix and transposed-Bs B-ldmatrix. Every config is element-exact first
\ (tools/ptx/mma-gemm-check.f). Compared vs Triton 3.8's per-shape tf32 winners (docs/eval-triton.md GB10).
: GB-W4-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 8 2 1 2 4 GB-MMM-CFGW             \ 8-warp MFRAGS=4 stages=2 dyn ldmatrix-A (256x64) - 2048 anchor
   32 8 1 1 4 4 GB-MMM-CFGW-B           \ 8-warp mmm-wide-b-m4-s1 flagship (256x64) - 4096 anchor
   32 8 1 0 2 4 GB-MMM-CFGW4            \ 4-warp MFRAGS=4 stages=1 STATIC ldmatrix-A (128x64, 28672 B)
   32 8 2 1 2 4 GB-MMM-CFGW4            \ 4-warp MFRAGS=4 stages=2 DYN ldmatrix-A (128x64, 57344 B)
   32 8 1 1 4 4 GB-MMM-CFGW4-B          \ 4-warp MFRAGS=4 bpad=4 stages=1 DYN B-ldmatrix (128x64, 29696 B)
   32 8 2 1 4 4 GB-MMM-CFGW4-B          \ 4-warp MFRAGS=4 bpad=4 stages=2 DYN B-ldmatrix (128x64, 59392 B)
   32 8 1 0 2 2 GB-MMM-CFGW4            \ 4-warp MFRAGS=2 stages=1 STATIC ldmatrix-A (64x64, 18432 B)
   32 8 2 1 2 2 GB-MMM-CFGW4            \ 4-warp MFRAGS=2 stages=2 DYN ldmatrix-A (64x64, 36864 B)
   32 8 1 1 2 4 GB-MMM-CFGW4-B          \ 4-warp MFRAGS=2 bpad=4 stages=1 DYN B-ldmatrix (64x64)
   32 8 2 1 2 4 GB-MMM-CFGW4-B          \ 4-warp MFRAGS=2 bpad=4 stages=2 DYN B-ldmatrix (64x64)
   32 8 3 1 2 4 GB-MMM-CFGW4            \ 4-warp MFRAGS=4 stages=3 DYN ldmatrix-A (128x64, 86016 B) - N-stage ring pipeline
   32 8 3 1 2 2 GB-MMM-CFGW4            \ 4-warp MFRAGS=2 stages=3 DYN ldmatrix-A (64x64, 55296 B)
   32 8 4 1 2 2 GB-MMM-CFGW4            \ 4-warp MFRAGS=2 stages=4 DYN ldmatrix-A (64x64, 73728 B)
   32 8 5 1 2 2 GB-MMM-CFGW4            \ 4-warp MFRAGS=2 stages=5 DYN ldmatrix-A (64x64, 92160 B)
   32 8 3 1 4 4 GB-MMM-CFGW4-B ;        \ 4-warp MFRAGS=4 bpad=4 stages=3 DYN B-ldmatrix (128x64, 89088 B)

\ SHARED-MEMORY EPILOGUE sweep (dot habu-shared-mem-epilogue): each round-2 per-shape winning tile, epilogue
\ OFF then ON, same session so the A/B pair isolates the store change. Expect the largest lift at 512^3 (the
\ compute-light launch where the store fraction is largest). Winners: 512^3 = 4-warp M4 stages=2 dyn; 1024^3/
\ 2048^3 = 4-warp M4 stages=1 static; 4096^3 = 8-warp M4 B-ldmatrix stages=1 dyn. Element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-*-EPI). Runs all four shapes per tile; read the winner row per shape.
: GB-EPI-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 8 2 1 2 4 GB-MMM-CFGW4            \ 512^3 winner 4-warp M4 stages=2 dyn ldmatrix-A (128x64) - epilogue OFF
   32 8 2 1 2 4 GB-MMM-CFGW4-EPI        \ same tile + EPILOGUE
   32 8 1 0 2 4 GB-MMM-CFGW4            \ 1024^3/2048^3 winner 4-warp M4 stages=1 static ldmatrix-A - epilogue OFF
   32 8 1 0 2 4 GB-MMM-CFGW4-EPI        \ same tile + EPILOGUE
   32 8 1 1 4 4 GB-MMM-CFGW-B           \ 4096^3 winner 8-warp M4 bpad=4 stages=1 dyn B-ldmatrix (256x64) - epilogue OFF
   32 8 1 1 4 4 GB-MMM-CFGW-B-EPI ;     \ same tile + EPILOGUE

\ XOR-SWIZZLE A/B sweep (dot habu-xor-swizzle-mma): each committed pad winner tile, PAD (pad=8) then the
\ pad-free XSWIZ variant, SAME session so the pair isolates the layout change; plus the two levers the pad
\ smem cost foreclosed - a stages=3 deep ring (round-2 negative) and epilogue-on the 256-row B-ldmatrix tile
\ (round-3 eviction negative) - re-tested with the freed budget. Every config is element-exact first
\ (tools/ptx/mma-gemm-check.f MGC-CFG-XSWIZ*). Best-of-3, run SOLO, pinned 13.3 ptxas. Read the PAD vs XSWIZ
\ pair per shape: a tie means the swizzle frees 4 KB/tile at no throughput cost; a win means the freed budget
\ took a lever the pad could not afford.
: GB-XSWIZ-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 8 2 1 2 4 GB-MMM-CFGW4-EPI        \ 512^3 winner 4-warp M4 stages=2 dyn +epi - PAD baseline
   2 1 4 GB-MMM-CFGW4-EPI-X             \ 512^3 winner - XSWIZ (pad-free)
   32 8 1 0 2 4 GB-MMM-CFGW4-EPI        \ 1024^3/2048^3 winner 4-warp M4 stages=1 static +epi - PAD baseline
   1 0 4 GB-MMM-CFGW4-EPI-X             \ 1024^3/2048^3 winner - XSWIZ
   32 8 1 1 4 4 GB-MMM-CFGW-B           \ 4096^3 winner 8-warp M4 bpad=4 stages=1 dyn B-ldmatrix - PAD baseline
   1 1 4 4 GB-MMM-CFGW-B-X              \ 4096^3 winner - XSWIZ
   32 8 1 0 2 4 GB-MMM-CFGW4            \ bare ldmatrix-A 4-warp M4 s1 static (direct As-conflict A/B) - PAD baseline
   1 0 4 GB-MMM-CFGW4-X                 \ bare ldmatrix-A 4-warp M4 s1 static - XSWIZ
   32 8 2 1 2 4 GB-MMM-CFGW             \ bare ldmatrix-A 8-warp M4 s2 dyn (2048 anchor) - PAD baseline
   2 1 4 GB-MMM-CFGW-X                  \ bare ldmatrix-A 8-warp M4 s2 dyn - XSWIZ
   32 8 3 1 2 4 GB-MMM-CFGW4            \ RE-OPENED lever 1a: 4-warp M4 stages=3 deep ring (round-2 negative) - PAD (SH 86016, 1 blk/SM)
   3 1 4 GB-MMM-CFGW4-X                 \ lever 1a - XSWIZ (SH 73728, STILL 1 blk/SM - the freed 12 KB does not cross the line)
   32 8 3 1 2 2 GB-MMM-CFGW4            \ RE-OPENED lever 1b: 4-warp M2 stages=3 deep ring - PAD (SH 55296, 1 blk/SM)
   3 1 2 GB-MMM-CFGW4-X                 \ lever 1b - XSWIZ (SH 49152 <= 50 KB: CROSSES to 2 blk/SM - the sharpest re-open test)
   32 8 1 1 4 4 GB-MMM-CFGW-B-EPI       \ RE-OPENED lever 2: 8-warp M4 B-ldmatrix + epilogue (round-3 256-row eviction) - PAD
   1 1 4 4 GB-MMM-CFGW-B-EPI-X ;        \ lever 2 - XSWIZ (does the freed budget stop the SH-grow eviction?)

\ FP16 tile schedule sweep (dot habu-fp16-mma-tile): the FP32 CUDA-core roof reference, then the
\ m16n8k16 f16.f16.f32 tile across warp grids (8/4), MFRAGS (1/2/4), stages (1/2), and the smem C
\ epilogue. fp16 HALVES the per-tile smem vs tf32, so the wide MFRAGS=4 8-warp tile now fits the 48 KB
\ static cap. Every config is element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-F16, 0 mismatches);
\ best-of-3 at the sustained GEMM clock, run SOLO. Compared vs Triton 3.8's fp16 tl.dot per-shape
\ numbers (docs/eval-triton.md GB10 fp16 table: 27.4 / 73.8 / 85.8 / 89.1 TFLOP/s). Read the winner row
\ per shape (the per-shape optimum moves with occupancy, as for tf32).
: GB-F16-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 0 2 0 1 8 0 GB-MMM-CFG-F16        \ non-wide MFRAGS=1 8-warp static (64x64)
   32 0 2 1 2 8 0 GB-MMM-CFG-F16        \ MFRAGS=2 8-warp stages=2 dyn (128x64)
   32 0 1 0 2 8 0 GB-MMM-CFG-F16        \ MFRAGS=2 8-warp stages=1 static (128x64)
   32 0 1 0 4 8 0 GB-MMM-CFG-F16        \ MFRAGS=4 8-warp stages=1 static (256x64; fp16 fits the 48 KB cap)
   32 0 2 1 4 8 0 GB-MMM-CFG-F16        \ MFRAGS=4 8-warp stages=2 dyn (256x64)
   32 0 1 0 4 4 0 GB-MMM-CFG-F16        \ 4-warp MFRAGS=4 stages=1 static (128x64)
   32 0 2 1 4 4 0 GB-MMM-CFG-F16        \ 4-warp MFRAGS=4 stages=2 dyn (128x64)
   32 0 1 0 2 4 0 GB-MMM-CFG-F16        \ 4-warp MFRAGS=2 stages=1 static (64x64)
   32 0 1 0 4 4 1 GB-MMM-CFG-F16        \ 4-warp MFRAGS=4 stages=1 static + EPILOGUE (128x64)
   32 0 2 1 2 8 1 GB-MMM-CFG-F16        \ MFRAGS=2 8-warp stages=2 dyn + EPILOGUE (128x64)
   \ transposed-Bs feed (dot habu-fp16-transposed-bs): one b32 load per B register vs 2 u16 + shift/or.
   \ Same winning tiles as above (4-warp MFRAGS=4 sweeps every shape), bpad {0,8} (8 = conflict-free b32 read).
   32 0 2 1 4 4 0 0 GB-MMM-CFG-F16-T    \ 4-warp MFRAGS=4 stages=2 dyn bpad=0 (128x64; 512-2048 baseline winner shape)
   32 0 2 1 4 4 0 8 GB-MMM-CFG-F16-T    \ 4-warp MFRAGS=4 stages=2 dyn bpad=8 (conflict-free BT read)
   32 0 1 0 4 4 0 0 GB-MMM-CFG-F16-T    \ 4-warp MFRAGS=4 stages=1 static bpad=0 (128x64; 4096 baseline winner shape)
   32 0 1 0 4 4 0 8 GB-MMM-CFG-F16-T    \ 4-warp MFRAGS=4 stages=1 static bpad=8
   32 0 2 1 2 8 0 8 GB-MMM-CFG-F16-T    \ 8-warp MFRAGS=2 stages=2 dyn bpad=8 (128x64)
   32 0 2 0 1 8 0 8 GB-MMM-CFG-F16-T ;  \ non-wide MFRAGS=1 8-warp static bpad=8 (64x64; small-shape check)

\ BF16 tile schedule sweep (dot habu-bf16-m16n8k16-tile): the bf16 mirror of GB-F16-SWEEP - the FP32
\ CUDA-core roof reference (same session), then the m16n8k16 f32.bf16.bf16.f32 tile across warp grids (8/4),
\ MFRAGS (1/2/4), stages (1/2), the smem C epilogue, and the transposed-Bs B feed at bpad {0,8}. Same tile
\ geometry and smem as fp16 (a bf16 half is 2 bytes too), so the same tiles are swept. Every config is
\ element-exact first (tools/ptx/mma-gemm-check.f MGC-CFG-BF16 / MGC-CFG-BF16-T, 0 mismatches); best-of-3 at
\ the sustained GEMM clock, run SOLO. Referee = Triton 3.8 bf16 tl.dot (torch.bfloat16) if the venv exposes
\ it, else the same-session Habu fp16 tile (docs/eval-triton.md bf16 round). Read the winner row per shape.
: GB-BF16-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 0 2 0 1 8 0 GB-MMM-CFG-BF16       \ non-wide MFRAGS=1 8-warp static (64x64)
   32 0 2 1 2 8 0 GB-MMM-CFG-BF16       \ MFRAGS=2 8-warp stages=2 dyn (128x64)
   32 0 1 0 2 8 0 GB-MMM-CFG-BF16       \ MFRAGS=2 8-warp stages=1 static (128x64)
   32 0 1 0 4 8 0 GB-MMM-CFG-BF16       \ MFRAGS=4 8-warp stages=1 static (256x64; bf16 fits the 48 KB cap)
   32 0 2 1 4 8 0 GB-MMM-CFG-BF16       \ MFRAGS=4 8-warp stages=2 dyn (256x64)
   32 0 1 0 4 4 0 GB-MMM-CFG-BF16       \ 4-warp MFRAGS=4 stages=1 static (128x64)
   32 0 2 1 4 4 0 GB-MMM-CFG-BF16       \ 4-warp MFRAGS=4 stages=2 dyn (128x64)
   32 0 1 0 2 4 0 GB-MMM-CFG-BF16       \ 4-warp MFRAGS=2 stages=1 static (64x64)
   32 0 1 0 4 4 1 GB-MMM-CFG-BF16       \ 4-warp MFRAGS=4 stages=1 static + EPILOGUE (128x64)
   32 0 2 1 2 8 1 GB-MMM-CFG-BF16       \ MFRAGS=2 8-warp stages=2 dyn + EPILOGUE (128x64)
   \ transposed-Bs feed: one b32 load per B register vs 2 u16 + shift/or. Same winning tiles, bpad {0,8}.
   32 0 2 1 4 4 0 0 GB-MMM-CFG-BF16-T   \ 4-warp MFRAGS=4 stages=2 dyn bpad=0 (128x64)
   32 0 2 1 4 4 0 8 GB-MMM-CFG-BF16-T   \ 4-warp MFRAGS=4 stages=2 dyn bpad=8 (conflict-free BT read)
   32 0 1 0 4 4 0 0 GB-MMM-CFG-BF16-T   \ 4-warp MFRAGS=4 stages=1 static bpad=0 (128x64)
   32 0 1 0 4 4 0 8 GB-MMM-CFG-BF16-T   \ 4-warp MFRAGS=4 stages=1 static bpad=8
   32 0 2 1 2 8 0 8 GB-MMM-CFG-BF16-T   \ 8-warp MFRAGS=2 stages=2 dyn bpad=8 (128x64)
   32 0 2 0 1 8 0 8 GB-MMM-CFG-BF16-T ; \ non-wide MFRAGS=1 8-warp static bpad=8 (64x64; small-shape check)

\ HALF ldmatrix feed schedule sweep (dot habu-half-precision-ldmatrix): the FP32 CUDA-core roof reference
\ (same-session clock anchor), then a HEAD-TO-HEAD of the three B feeds on the winning 4-warp MFRAGS=4 128x64
\ tile - k-major scalar (GB-MMM-CFG-F16, the 512-2048 baseline winner), transposed-Bs (GB-MMM-CFG-F16-T bpad=8),
\ and the new ldmatrix (GB-MMM-CFG-F16-LDM) - at stages=2 dyn and stages=1 static, so the feed delta is read
\ directly per shape. Then the BK=64 "more K per stage" lever on the ldmatrix feed (8-warp MFRAGS=2 static, BK=32
\ vs BK=64; the half element size fits BK=64 stages=2 under the 48 KB static cap). Every config is element-exact
\ first (tools/ptx/mma-gemm-check.f MGC-CFG-F16-LDM). Best-of-3 at the sustained GEMM clock, run SOLO under the
\ pinned 13.3 ptxas. Referee = Triton 3.8 fp16 tl.dot (docs/eval-triton.md GB10: 27.4/73.8/85.8/89.1 TFLOP/s).
: GB-F16-LDM-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 0 2 1 4 4 0 GB-MMM-CFG-F16        \ k-major scalar s2 dyn (512-2048 baseline winner)
   32 0 2 1 4 4 0 8 GB-MMM-CFG-F16-T    \ transposed-Bs s2 dyn bpad=8
   32 0 2 1 4 4 0 GB-MMM-CFG-F16-LDM    \ ldmatrix s2 dyn
   32 0 1 0 4 4 0 GB-MMM-CFG-F16        \ k-major scalar s1 static (4096 baseline)
   32 0 1 0 4 4 0 GB-MMM-CFG-F16-LDM    \ ldmatrix s1 static
   32 0 2 0 2 8 0 GB-MMM-CFG-F16-LDM    \ BK=32 8-warp M2 static ldmatrix (BK sweep base)
   64 0 2 0 2 8 0 GB-MMM-CFG-F16-LDM ;  \ BK=64 8-warp M2 static ldmatrix (more K per stage, half-size smem fits)

\ BF16 ldmatrix feed schedule sweep (dot habu-half-precision-ldmatrix): the bf16 mirror of GB-F16-LDM-SWEEP.
\ bf16 tracks fp16 throughput bit-for-bit (same HMMA ladder), so a lighter head-to-head - k-major vs ldmatrix on
\ the 4-warp MFRAGS=4 s2 dyn tile + the BK=64 ldmatrix lever - confirms the feed delta carries. Element-exact
\ first (MGC-CFG-BF16-LDM). Referee = Triton 3.8 bf16 tl.dot (docs/eval-triton.md GB10: 27.4/67.3/77.6/80.8).
: GB-BF16-LDM-SWEEP ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM                                \ FP32 CUDA-core roof reference (same session)
   32 0 2 1 4 4 0 GB-MMM-CFG-BF16       \ k-major scalar s2 dyn
   32 0 2 1 4 4 0 GB-MMM-CFG-BF16-LDM   \ ldmatrix s2 dyn
   64 0 2 0 2 8 0 GB-MMM-CFG-BF16-LDM ; \ BK=64 8-warp M2 static ldmatrix (more K per stage)

;package

GEMMBENCH:GB-GB10
