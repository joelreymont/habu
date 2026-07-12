\ gemm-bench.f - measure NAIVE vs REGISTER-BLOCKED SGEMM on square GEMMs (CUDA events).
\
\ CAD-PLAN 8.1 step 1: the FIRST measured GEMM baseline. Two kernels, same ABI
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
require tools/ptx/profile.f
require tools/ptx/bench.f

package GEMMBENCH

16 constant GB-BLK                     \ block = 16x16 = 256 threads (both kernels)
$3F800000 constant GB-ONE             \ 1.0f bit pattern (A/B fill)

create GB-QO $1000 allot  create GB-QE $1000 allot
variable GB-DA  variable GB-DB  variable GB-DC
variable GB-NV                         \ M=N=K (square) as the u32 kernel param
variable GB-OT                         \ output-tile edge: 64 (MM blocked) / 16 (MMN naive)

: GB-INT. ( n -- )  SB-RESET SB-INT SB$ type ;

: GB-ASSEMBLE ( -- )                   \ captured PTX -> kernel.ptx -> ptxas cubin (die on rc<>0)
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
   s GB-OT @ / PTXBENCH:GRID!    s GB-OT @ / PTXBENCH:GRIDY!
   36 PTXBENCH:PARAM-BYTES!
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
   512  200 GB-SHAPE
   1024 80  GB-SHAPE
   2048 30  GB-SHAPE ;

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
   16 GB-OT !
   s" MMN" GB-KERNEL
   PTXTC:CLEAN ;

: GB-MM ( -- )                         \ register-blocked column (FP32 CUDA-core roof)
   s" == MM register-blocked 64x64 (4x4 micro-tile/thread, shared staging) ==" type cr
   s" habu-gemm-bench" PTXTC:PREPARE
   PTX-CAPTURE-ON  EMIT-MATMUL  PTX-CAPTURE-OFF
   GB-ASSEMBLE
   64 GB-OT !
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
   64 GB-OT !
   s" MMM" GB-KERNEL
   PTXTC:CLEAN ;

: GB-MMM ( -- )                        \ sweep the 3 fragment-load modes (ablation); restore baseline default
   0 GB-MMM-MODE  1 GB-MMM-MODE  2 GB-MMM-MODE
   0 MMA-LMODE ! ;

public

: GB-ALL ( -- )
   CUDA:OPEN? 0= if s" gemm-bench: libcuda unavailable -> SKIPPED (off-device)" type cr exit then
   GB-MMN
   GB-MM
   GB-MMM ;

;package

GEMMBENCH:GB-ALL
