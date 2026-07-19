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

create GB-QO $1000 allot  create GB-QE $1000 allot
variable GB-DA  variable GB-DB  variable GB-DC
variable GB-NV                         \ M=N=K (square) as the u32 kernel param
variable GB-OT                         \ output-tile edge (grid X / N cols): 64 (MM/MMM blocked) / 16 (MMN naive)
variable GB-OTY                        \ output-tile M edge (grid Y): = GB-OT, except 64*MFRAGS for the wider-M MMM tile
variable GB-SMEM-DYN                    \ dynamic .shared bytes for the launch (0 = static)
64 GB-OTY !

: GB-INT. ( n -- )  SB-RESET SB-INT SB$ type ;

: GB-ASSEMBLE ( -- )                   \ captured PTX -> kernel.ptx -> ptxas cubin (die on rc<>0)
   ATGT:LABEL$ PTXTC:TC-ARCH!          \ assembler arch from the probed active target (sm_87 Orin / sm_121a GB10)
   PTXTC:PTX$ PTX-CAPTURE$ WRITE-ALL
   GB-QO $1000 >LEN GB-QE $1000 >LEN PTXTC:ASSEMBLE PTXTC:ASM-REPORT {: rc:n :}
   rc 0= 0= if s" gemm-bench: ptxas failed" 1 die then ;

: GB-ALLOC ( n -- ) {: s:n :}          \ alloc + fill A=B=1.0, C=0 for an s x s GEMM
   s s * {: e:n :}
   e 4 * GB-DA PTXBENCH:DEVICE-ALLOC
   e 4 * GB-DB PTXBENCH:DEVICE-ALLOC
   e 4 * GB-DC PTXBENCH:DEVICE-ALLOC
   GB-DA @ GB-ONE e PTXBENCH:DEVICE-MEMSET32
   GB-DB @ GB-ONE e PTXBENCH:DEVICE-MEMSET32
   GB-DC @ 0     e PTXBENCH:DEVICE-MEMSET32 ;

: GB-PARAMS ( n -- ) {: s:n :}         \ 2D grid = (s/tile)^2 output tiles, 16x16 block
   s GB-NV !
   GB-BLK PTXBENCH:BLOCK!        GB-BLK PTXBENCH:BLOCKY!
   s GB-OT @ / PTXBENCH:GRID!    s GB-OTY @ / PTXBENCH:GRIDY!   \ gridY = M/block-rows (GB-OTY = 64*MFRAGS for wide)
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
: GB-BYTES ( n n -- n ) {: s:n it:n :}  s s * 12 * it * ;      \ A+B read + C write, 4 B each

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

: GB-KERNEL ( ptr u8 n -- ) {: ka:ptr ku:n :}   \ load the named kernel from PTXTC:CUBIN$, bench
   PTXBENCH:RESET
   PTXTC:CUBIN$ PTXBENCH:CUBIN!
   ka ku PTXBENCH:KERNEL!  ka ku PTXBENCH:LABEL!
   PTXBENCH:OPEN  PTXBENCH:LOAD
   GB-SHAPES
   PTXBENCH:UNLOAD  PTXBENCH:CLOSE ;

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
   MMA-DYNSMEM @ if MMA-SMEM else 0 then GB-SMEM-DYN !
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
   MMA-DYNSMEM @ if MMA-SMEM else 0 then GB-SMEM-DYN !
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

\ GB10 head-to-head campaign (dot habu-gb10-gemm-head): FP32 CUDA-core roof reference, then the full
\ wider-M B-feed-amortization schedule sweep (scalar-B MFRAGS=2/4 and the B-ldmatrix transposed-Bs
\ configs incl. the mmm-wide-b-m4-s1 flagship) across 512/1024/2048/4096. tf32 tensor-core throughput
\ vs the source-built Triton 3.8 referee (docs/eval-triton.md GB10 section).
: GB-GB10 ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MM
   GB-MMM-WIDE-SWEEP
   GB-MMM-WIDE-B-SWEEP ;


: GB-ALL ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MMN
   GB-MM
   GB-MMM
   GB-MMM-SWEEP ;

;package

GEMMBENCH:GB-GB10
