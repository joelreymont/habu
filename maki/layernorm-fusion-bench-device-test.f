\ maki/layernorm-fusion-bench-device-test.f - the LAYERNORM one-kernel fusion proof through the
\ AUTOMATIC lowering pipeline (dot habu-automatic-aggressive-fusion, leg a).
\
\ LayerNorm-with-affine is the maki op chain `LAYERNORM SCALE BIAS` = ROW-REDUCE -> EW -> EW:
\ OP-LAYERNORM (maki/op-kind.f, class ROW-REDUCE) already does mean+variance+normalize INTERNALLY
\ as one block-per-row reduction (maki/lower/red.f LRED-EMIT-LN: two BLOCK-SUMs = mu then var), and
\ the affine epilogue is a scalar gamma (OP-SCALE, 1x1) then a per-feature beta (OP-BIAS, 1xC). The
\ maki op set has no separate MODEL:-parseable mean/variance reduce op, so the fusable seam is the
\ reduction's EW EPILOGUE, not a ROW-REDUCE->ROW-REDUCE decomposition: FP-BUILD's capability table
\ (fusion-plan.f: ROW-REDUCE->EW emittable, EW->EW emittable) fuses LAYERNORM + SCALE + BIAS into
\ ONE region whose single kernel is the LRED block-per-row layernorm with the scale+bias folded as
\ per-lane epilogue ops (LRED-EMIT-NODE scale->EMIT-MUL, bias->EMIT-ADD). This file proves it e2e:
\   1. PLAN-SHAPE (everywhere, no CUDA): fusion ON plans ONE row-reduce region (one fused kernel),
\      ablated (FP-FUSE-OFF!) plans THREE regions (layernorm | scale | bias, each its own kernel with
\      global round-trips). FP-REGION-COUNT 1 vs 3 is free off-device coverage.
\   2. CORRECTNESS (Orin only): each mode is run on device by the SAME whole-model golden
\      (maki/lower/model-device.f LMDM via LOWER-MODEL-GOLDEN, region-by-region f32 with cross-region
\      device buffers) and its final output matched against the SAME host executor reference (EX-RUN:
\      LN-FWD then scalar SCALE then 1xC BIAS, f64) under the composed reduction tolerance. BOTH modes
\      V-PASS - run on device twice, both device-correct vs the same golden.
\   3. BANDWIDTH (Orin only): each mode's AUTOMATICALLY-EMITTED region kernels are benchmarked at scale
\      (LFB-N elems = LFB-ROWS x LFB-COLS, LFB-ITERS iters) with CUDA-event device timing
\      (tools/ptx/bench.f). The bencher dispatches on region class (LLA-REGION-REDUCE?): the fused /
\      layernorm region is the LRED block-per-row kernel (grid = ROWS, block = 256, p_k = COLS), the
\      ablated scale/bias regions are LEW flat kernels (grid = ceil(N/block), p_k = N). Fused reads x
\      once + writes y once (~8 B/elem); ablated round-trips x->t1->t2->y through global (~24 B/elem =
\      the round-trip tax). The measured latency ratio (ablated/fused) is the fusion win; all kernels
\      saturate the same memory roof, so the win is fewer global round-trips (~3x for the 3-op chain),
\      not a higher peak GB/s - the fusion-bench-device-test.f shape. Two perf rows (LN-FUSE-ON /
\      LN-FUSE-OFF, orin-nx-25w) are printed for tools/ptx/perf-rows.tsv.
\
\ CHILD EMIT DRIVER. The device cubin build spawns a fresh bin/hb per region (LMDM-BUILD-CUBINS) that
\ re-imports the model source and calls the region-class emit word (LMDM-EMIT$: LRED-EMIT for the
\ reduce region, LEW-EMIT for the scale/bias regions). The child re-plans the SAME mode, so the model
\ source strings passed here carry an explicit FP-FUSE-ON! / FP-FUSE-OFF! suffix (child runs MODEL:
\ then the toggle then FP-BUILD): parent and child agree on region ids, classes, and kernels.
\
\ SHAPE-GENERIC KERNELS. The golden fixture is 4x8 (small, so LMDM-EVIDENCE stays readable); the bench
\ re-uses those SAME cubins at scale (ROWS x COLS). The LRED kernel takes its column count as the
\ runtime p_k param and its row count as the launch grid, so the fused layernorm kernel benched at
\ COLS=256 is a genuine 256-wide layernorm; the flat LEW scale/bias kernels are generic over p_n. The
\ tiny broadcast params (s 1x1, b 1xC) contribute O(1)/O(C) traffic, negligible vs the O(N) data+output
\ roofline the ratio measures - exactly the fusion-bench-device-test.f precedent (4x8 model, N-scale bench).
\
\ CORRUPTION SENSITIVITY (magnitude-independent). That a WRONG kernel is REJECTED (not silently passed)
\ by the shared whole-model comparator is proven for the LEW/LRED comparators by
\ maki/ablate-golden-device-test.f (seeded wrong-index / transposed-operand / dropped-mask / stale-cubin
\ faults, each caught as V-FAIL or a fail-closed throw). For THIS fixture a magnitude-independent PTX
\ perturbation of the fused REGION_0 layernorm kernel (e.g. dropping the reduction inactive-lane
\ predicate @%p2, or a store perturbation) likewise drives LOWER-GOLDEN to V-FAIL - demonstrated in a
\ temp copy on the Orin; the committed proof stays the clean PASS (the deploy-device-test.f discipline).
\
\ Off the Orin (no libcuda) the correctness + bandwidth legs are SKIPPED (CUDA:OPEN? probe) and the
\ plan-shape assertions still run, so the file check-loads everywhere. Not part of the maki gate
\ (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to zed then
\ `bin/hb --load maki/layernorm-fusion-bench-device-test.f`. Fully checked Habu; maki -> habu only.

require lib/test.f
require lib/fmt.f
require tools/ptx/bench.f
require maki/cad.f
require maki/lower/ew.f
require maki/lower/red.f
require maki/lower/model-device.f

package MAKI

$1000   constant LFB-ROWS      \ 4096 rows (tokens); ROWS x COLS = LFB-N matches the SAXPY-family perf rows
256     constant LFB-COLS      \ row width = block = the LRED column cap; a real layernorm feature dim
200     constant LFB-ITERS     \ CUDA-event iterations per kernel
256     constant LFB-BLOCK     \ launch block (LRED-BLOCK; also the flat LEW block)
8       constant LFB-DEV-CAP   \ K inputs + output devptr cells (K <= LFB-DEV-CAP-1)

: LFB-N ( -- n )  LFB-ROWS LFB-COLS * ;                     \ total work items (1048576)

create LFB-DEV LFB-DEV-CAP cells allot     \ per-buffer device pointer cells (inputs then output)
variable LFB-NV                             \ the u32 kernel param cell (p_k = COLS for reduce, p_n = N for EW)

: LFB-U ( n -- )  SB-RESET FMT:SB-INT SB$ type ;
: LFB-TAB ( -- )  9 emit ;

: LFB-GRID ( -- n )  LFB-N LFB-BLOCK + 1 - LFB-BLOCK / ;    \ ceil(N/block), 1 elem/thread (flat LEW)

\ ---- device buffers for one region kernel (K inputs + 1 output, each LFB-N f32) --------
: LFB-DEV-CELL ( n -- ptr a )  cells LFB-DEV + ;
: LFB-ALLOC-BUFS ( n -- ) {: k:n :}            \ alloc + zero K inputs and the output buffer
   k 1+ 0 ?do
      LFB-N 4 * i LFB-DEV-CELL PTXBENCH:DEVICE-ALLOC
      i LFB-DEV-CELL @ 0 LFB-N PTXBENCH:DEVICE-MEMSET32
   loop ;
: LFB-FREE-BUFS ( n -- ) {: k:n :}
   k 1+ 0 ?do  i LFB-DEV-CELL @ PTXBENCH:DEVICE-FREE  loop ;

\ Kernel ABI (LEW p_n and LRED p_k are both the trailing .u32): K input ptrs (off i*8), output ptr
\ (off K*8), the u32 param (off K*8+8). Over-allocated broadcast inputs stay in-bounds of the N buffer.
: LFB-BIND-PARAMS ( n -- ) {: k:n :}
   k 0 ?do  i 8 *  i LFB-DEV-CELL  PTXBENCH:PARAM-PTR!  loop
   k 8 *      k LFB-DEV-CELL  PTXBENCH:PARAM-PTR!
   k 8 * 8 +  LFB-NV          PTXBENCH:PARAM-U32! ;

\ ---- one region kernel: load its cubin, run LFB-ITERS launches under CUDA events -------
\ ( cubin$ kernel$ K grid kparam -- gpu-ns ). Strings stay on the stack until consumed (roles preserved).
: LFB-RUN ( ptr u8 n ptr u8 n n n n -- n ) {: k:n grid:n kp:n :}
   PTXBENCH:RESET
   PTXBENCH:KERNEL!
   PTXBENCH:CUBIN!
   kp LFB-NV !
   PTXBENCH:OPEN PTXBENCH:LOAD
   k LFB-ALLOC-BUFS
   LFB-BLOCK PTXBENCH:BLOCK!
   grid PTXBENCH:GRID!
   k 8 * 12 + PTXBENCH:PARAM-BYTES!
   PTXBENCH:PREPARE-LAUNCH
   k LFB-BIND-PARAMS
   LFB-ITERS PTXBENCH:ITERS!
   PTXBENCH:BENCH-GPU-NS {: ns:n :}
   k LFB-FREE-BUFS
   PTXBENCH:UNLOAD PTXBENCH:CLOSE
   ns ;

\ ---- per region: the REGION_<rid> kernel name + its LMDM cubin path ---------------------
: LFB-KNAME$ ( CAD-KIND:region -- ptr u8 n )     \ REGION_<raw> (SB span, valid until next SB op)
   RGN>RAW {: raw:n :}
   SB-RESET s" REGION_" SB-APPEND raw FMT:SB-INT SB$ ;

\ bench a ROW-REDUCE region (LRED block-per-row kernel: grid = ROWS, p_k = COLS).
: LFB-BENCH-RED ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   rid LRED-ANALYZE LRED-NIN@ {: k:n :}          \ external input count (also validates FP-BUILD)
   rid LMDM-RP$                                  \ cubin path (stable LMDM-RP buffer)
   rid LFB-KNAME$                                \ REGION_<raw> (SB; consumed before the next SB op)
   k LFB-ROWS LFB-COLS LFB-RUN {: ns:n :}
   s"   REGION_" type rid RGN>RAW LFB-U s" (row-reduce block/row)  inputs=" type k LFB-U
   s"  gpu_ns=" type ns LFB-U cr
   ns ;

\ bench an ELEMENTWISE region (LEW flat kernel: grid = ceil(N/block), p_n = N).
: LFB-BENCH-EW ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   rid LEW-ANALYZE LEW-NIN@ {: k:n :}
   rid LMDM-RP$
   rid LFB-KNAME$
   k LFB-GRID LFB-N LFB-RUN {: ns:n :}
   s"   REGION_" type rid RGN>RAW LFB-U s" (elementwise flat)   inputs=" type k LFB-U
   s"  gpu_ns=" type ns LFB-U cr
   ns ;

\ dispatch on the planned region class (the automatic pipeline chose the kernel schedule).
: LFB-BENCH-REGION ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   rid LLA-REGION-REDUCE? if rid LFB-BENCH-RED else rid LFB-BENCH-EW then ;

: LFB-SUM-NS ( -- n )                            \ sum region-kernel gpu ns over the current plan
   0 FP-REGION-COUNT 0 ?do  i FP-REGION-ID LFB-BENCH-REGION +  loop ;

\ dominant global traffic per region: 1 read of the full RxC data operand + 1 write of the RxC output
\ (the broadcast affine params s 1x1 / b 1xC are O(1)/O(C), negligible vs O(N)); f32 * N * iters.
: LFB-REGION-BYTES ( -- n )  2 LFB-N * 4 * LFB-ITERS * ;
: LFB-MODE-BYTES ( -- n )  FP-REGION-COUNT LFB-REGION-BYTES * ;

\ ---- one mode: assert region count, build cubins, run whole-model golden, bench kernels --
\ ( ma mu expected-regions -- total-gpu-ns total-bytes ). Requires the caller set the toggle + FP-BUILD
\ (in process) and LMDM-BEGIN prepared the toolchain; the source carries the matching FP-FUSE-*! suffix
\ so the emit children re-plan the same mode.
: LFB-MODE ( ptr u8 n n -- n n ) {: exp:n :}
   FP-REGION-COUNT exp T=
   LMDM-BUILD-CUBINS
   LOWER-MODEL-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   v V-PASS T=
   LFB-SUM-NS LFB-MODE-BYTES ;

\ ---- reporting -------------------------------------------------------------------------
: LFB-GBS ( n n -- n )  {: by:n ns:n :}  by 1000 * ns / ;   \ GB/s x1000 = bytes*1000/ns
: LFB-RATIO ( n n -- n ) {: a:n f:n :}  a 1000 * f / ;      \ ablated/fused latency x1000
: LFB-MS. ( n -- ) {: ns:n :}                               \ milliseconds with 3 decimals
   ns 1000000 / LFB-U $2E emit
   ns 1000000 mod 1000 / {: fr:n :}
   fr 100 < if $30 emit then  fr 10 < if $30 emit then  fr LFB-U ;

\ perf-rows.tsv line prefix: kernel grid gridy block blocky iters work metric value device date,
\ through the trailing tab before the note. Both modes launch grid = ROWS = ceil(N/block) = 4096.
: LFB-ROW-HEAD ( ptr u8 n n -- ) {: val:n :}                \ kernel$ VAL
   type LFB-TAB
   LFB-ROWS LFB-U LFB-TAB  1 LFB-U LFB-TAB  LFB-BLOCK LFB-U LFB-TAB  1 LFB-U LFB-TAB
   LFB-ITERS LFB-U LFB-TAB  LFB-N LFB-U LFB-TAB
   s" GBS" type LFB-TAB  val LFB-U LFB-TAB
   s" orin-nx-25w" type LFB-TAB  s" 2026-07-15" type LFB-TAB ;

: LFB-ROW-ON ( n n -- ) {: val:n ratio:n :}                 \ value ratio  (fused row + note)
   s" LN-FUSE-ON" val LFB-ROW-HEAD
   s" automatic-pipeline fused LAYERNORM SCALE BIAS, 1 region/1 block-per-row kernel, ~8 B/elem (x read + y write); 25W mode-3 (8 SMs) @ 918 MHz; fusion_elapsed_ratio_x1000=" type ratio LFB-U
   s"  (maki/layernorm-fusion-bench-device-test.f)" type cr ;
: LFB-ROW-OFF ( n -- ) {: val:n :}                          \ value  (ablated row + note)
   s" LN-FUSE-OFF" val LFB-ROW-HEAD
   s" ablated per-op regions, 3 kernels (layernorm block/row + scale/bias flat) + global round-trips, ~24 B/elem; 25W mode-3 (8 SMs) @ 918 MHz (maki/layernorm-fusion-bench-device-test.f)" type cr ;

: LFB-REPORT ( n n n n -- ) {: fns:n fby:n ans:n aby:n :}   \ fused-ns fused-by ablated-ns ablated-by
   ans fns LFB-RATIO {: ratio:n :}
   cr s" == layernorm-fusion-bench: automatic-pipeline fused vs ablated (orin-nx-25w) ==" type cr
   s" fused   1 kernel   " type fns LFB-MS. s"  ms/200 iters  gpu_ns=" type fns LFB-U
      s"  GBS_x1000=" type fby fns LFB-GBS LFB-U cr
   s" ablated 3 kernels  " type ans LFB-MS. s"  ms/200 iters  gpu_ns=" type ans LFB-U
      s"  GBS_x1000=" type aby ans LFB-GBS LFB-U cr
   s" fusion_elapsed_ratio_x1000=" type ratio LFB-U
      s"  (ablated/fused latency; >1000 => fused wins on effective work/round-trips)" type cr
   ans fns > TTRUE                                          \ fused finishes the chain faster
   cr s" == perf rows (paste into tools/ptx/perf-rows.tsv) ==" type cr
   fby fns LFB-GBS ratio LFB-ROW-ON
   aby ans LFB-GBS LFB-ROW-OFF ;

\ ---- the guarded device leg (both modes) -----------------------------------------------
: LFB-DEVICE-BENCH ( ptr u8 n ptr u8 n -- ) {: onp:ptr onu:n offp:ptr offu:n :}
   CUDA:OPEN? 0= if
      s" layernorm-fusion-bench: off-device -> golden + bandwidth SKIPPED (plan-shape ran everywhere)" type cr
      exit then
   s" == fusion ON (automatic pipeline: layernorm+affine -> 1 block-per-row kernel) ==" type cr
   FP-FUSE-ON! FP-BUILD
   onp onu 1 LFB-MODE {: fns:n fby:n :}
   s" == fusion OFF (ablated: layernorm | scale | bias -> 3 kernels + global round-trips) ==" type cr
   FP-FUSE-OFF! FP-BUILD
   offp offu 3 LFB-MODE {: ans:n aby:n :}
   FP-FUSE-ON!
   fns fby ans aby LFB-REPORT ;

\ ============ the fixture: LAYERNORM SCALE BIAS (row-reduce + affine epilogue) ============
\ fusion ON -> one row-reduce region (layernorm reduction + scalar scale + 1xC bias fused, all in a
\ block-per-row kernel; 3 external inputs x,s,b -> 1 output); ablated -> three regions (layernorm |
\ scale | bias), each output round-tripped through global. The child emit driver re-plans from the
\ source string, so each carries the matching FP-FUSE-*! suffix (child order: MODEL: then the toggle
\ then FP-BUILD).
LMDM-BEGIN                                       \ T-RESET; off-device records the SKIP

s" == layernorm-fusion-bench: LAYERNORM SCALE BIAS 4x8, automatic pipeline (fusion ON vs ablated) ==" type cr
MODEL: LNAFF ( x:4x8 s:1x1 b:1x8 -- y ) LAYERNORM SCALE BIAS ;

\ ---- plan-shape (everywhere, no CUDA): 1 fused region vs 3 ablated regions ---------------
FP-FUSE-ON!  FP-BUILD  FP-REGION-COUNT 1 T=
FP-FUSE-OFF! FP-BUILD  FP-REGION-COUNT 3 T=
FP-FUSE-ON!

\ ---- device legs (Orin only): both modes device-correct vs the same golden + bandwidth ---
s" MODEL: LNAFF ( x:4x8 s:1x1 b:1x8 -- y ) LAYERNORM SCALE BIAS ; FP-FUSE-ON!"
s" MODEL: LNAFF ( x:4x8 s:1x1 b:1x8 -- y ) LAYERNORM SCALE BIAS ; FP-FUSE-OFF!"
LFB-DEVICE-BENCH

LMDM-END                                         \ T-REPORT
;package
