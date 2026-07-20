\ maki/fusion-bench-device-test.f - the end-to-end fusion device bench through the AUTOMATIC
\ lowering pipeline (dot habu-automatic-op-fusion, remaining device leg).
\
\ Takes a multi-op same-shape elementwise chain model (Add->Mul->Relu, 4x8) and lowers it
\ twice through the REAL maki pipeline (FP-BUILD -> per-region cubins -> whole-model device
\ run), once with fusion ON and once ablated (FP-FUSE-OFF!), then:
\   1. PLAN-SHAPE (everywhere, no CUDA): fusion ON plans ONE elementwise region (one fused
\      kernel), ablated plans THREE regions (one kernel per op, with global round-trips
\      between them). FP-REGION-COUNT 1 vs 3 is free off-device coverage.
\   2. CORRECTNESS (Orin only): each mode is run on device by the SAME whole-model golden
\      (maki/lower/golden.f LOWER-MODEL-GOLDEN via LOWER-MODEL-RUN, region-by-region f32 with
\      cross-region device buffers) and its final output matched against the SAME host
\      executor reference (EX-RUN, f64) under the composed tolerance. BOTH modes V-PASS -
\      "run on device twice, both device-correct vs the same golden".
\   3. BANDWIDTH (Orin only): each mode's AUTOMATICALLY-EMITTED region kernels (LEW-EMIT per
\      region, one per FP region) are benchmarked at scale (FBD-N elems, FBD-ITERS iters) with
\      CUDA-event device timing (tools/ptx/bench.f), fused (one kernel, 16 B/elem) vs ablated
\      (three kernels summed, 32 B/elem = the round-trip tax). The measured latency ratio
\      (ablated/fused) is the fusion win; both kernels saturate the same memory roof, so the
\      win is fewer global round-trips (~2x for a 3-op chain), not a higher peak GB/s - the
\      same shape as the kernel-level tools/ptx/fusion-compare.f precedent. Two perf rows
\      (FUSE-CHAIN-ON / FUSE-CHAIN-OFF, orin-nx-25w) are printed for tools/ptx/perf-rows.tsv.
\
\ CHILD EMIT DRIVER. The device cubin build spawns a fresh bin/hb per region (LMDM-BUILD-CUBINS)
\ that re-imports the model source and calls the region-class emit word. The child re-plans the
\ SAME mode as the parent, so the model source strings passed here carry an explicit FP-FUSE-ON!
\ / FP-FUSE-OFF! suffix (the child runs MODEL: then the toggle then FP-BUILD): parent and child
\ plans agree on region ids and kernels for both modes.
\
\ CORRUPTION SENSITIVITY (magnitude-independent). That a WRONG kernel is REJECTED (not silently
\ passed) by the shared whole-model comparator (LG-COMPARE-LIN) is proven by
\ maki/ablate-golden-device-test.f (seeded wrong-index / transposed-operand / stale-cubin /
\ dropped-copy-back faults, each caught as V-FAIL or a fail-closed throw). For THIS fixture a
\ magnitude-independent PTX perturbation of the fused region-0 kernel (e.g. add.rn.f32 ->
\ sub.rn.f32, a constant-offset store, or registering the ablated add-only kernel in place of
\ the fused one) likewise drives LOWER-MODEL-GOLDEN to V-FAIL - demonstrated in a temp copy on
\ the Orin; the committed proof stays the clean PASS (the deploy-device-test.f discipline).
\
\ Off the Orin (no libcuda) the correctness + bandwidth legs are SKIPPED (CUDA:OPEN? probe) and
\ the plan-shape assertions still run, so the file check-loads everywhere. Not part of the maki
\ gate (maki/test.f) - it needs the CUDA toolkit + a device. Run on the Orin: scp to zed:Work/habu
\ then `bin/hb --load maki/fusion-bench-device-test.f`. Fully checked Habu; maki -> habu only.

require lib/test.f
require lib/fmt.f
require tools/ptx/bench.f
require maki/cad.f
require maki/lower/ew.f
require maki/lower/model-device.f

package MAKI

$100000 constant FBD-N        \ bench work items (1048576, matches the SAXPY-family perf rows)
200     constant FBD-ITERS    \ CUDA-event iterations per kernel
256     constant FBD-BLOCK    \ launch block (the flat LEW kernel is 1 elem/thread)
8       constant FBD-DEV-CAP  \ K inputs + output devptr cells (K <= FBD-DEV-CAP-1)

create FBD-DEV FBD-DEV-CAP cells allot    \ per-buffer device pointer cells (inputs then output)
variable FBD-NV                            \ the u32 kernel n-param cell (= FBD-N)

: FBD-U ( n -- )  SB-RESET FMT:SB-INT SB$ type ;
: FBD-TAB ( -- )  9 emit ;

: FBD-GRID ( -- n )  FBD-N FBD-BLOCK + 1 - FBD-BLOCK / ;   \ ceil(N/block), 1 elem/thread

\ ---- device buffers for one region kernel (K inputs + 1 output, each FBD-N f32) ----
: FBD-DEV-CELL ( n -- ptr a )  cells FBD-DEV + ;
: FBD-ALLOC-BUFS ( n -- ) {: k:n :}          \ alloc + zero K inputs and the output buffer
   k 1+ 0 ?do
      FBD-N 4 * i FBD-DEV-CELL PTXBENCH:DEVICE-ALLOC
      i FBD-DEV-CELL @ 0 FBD-N PTXBENCH:DEVICE-MEMSET32
   loop ;
: FBD-FREE-BUFS ( n -- ) {: k:n :}
   k 1+ 0 ?do  i FBD-DEV-CELL @ PTXBENCH:DEVICE-FREE  loop ;

\ LEW kernel ABI: K input ptrs (off i*8), output ptr (off K*8), u32 n (off K*8+8).
: FBD-BIND-PARAMS ( n -- ) {: k:n :}
   k 0 ?do  i 8 *  i FBD-DEV-CELL  PTXBENCH:PARAM-PTR!  loop
   k 8 *      k FBD-DEV-CELL  PTXBENCH:PARAM-PTR!
   k 8 * 8 +  FBD-NV          PTXBENCH:PARAM-U32! ;

\ ---- one region kernel: load its cubin, run FBD-ITERS launches under CUDA events -----
\ ( cubin$ kernel$ K -- gpu-ns ). Strings stay on the stack (roles preserved) until consumed.
: FBD-BENCH-KERNEL ( ptr u8 n ptr u8 n n -- n ) {: k:n :}
   PTXBENCH:RESET
   PTXBENCH:KERNEL!
   PTXBENCH:CUBIN!
   FBD-N FBD-NV !
   PTXBENCH:OPEN PTXBENCH:LOAD
   k FBD-ALLOC-BUFS
   FBD-BLOCK PTXBENCH:BLOCK!
   FBD-GRID PTXBENCH:GRID!
   k 8 * 12 + PTXBENCH:PARAM-BYTES!
   PTXBENCH:PREPARE-LAUNCH
   k FBD-BIND-PARAMS
   FBD-ITERS PTXBENCH:ITERS!
   PTXBENCH:BENCH-GPU-NS {: ns:n :}
   k FBD-FREE-BUFS
   PTXBENCH:UNLOAD PTXBENCH:CLOSE
   ns ;

\ ---- per region: the REGION_<rid> kernel name + its LMDM cubin path -------------------
: FBD-KNAME$ ( CAD-KIND:region -- ptr u8 n )     \ REGION_<raw> (SB span, valid until next SB op)
   RGN>RAW {: raw:n :}
   SB-RESET s" REGION_" SB-APPEND raw FMT:SB-INT SB$ ;

\ bench region rid's kernel (built by LMDM-BUILD-CUBINS at rid LMDM-RP$), print + return ns.
: FBD-BENCH-REGION ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   rid LEW-ANALYZE LEW-NIN@ {: k:n :}            \ external input count (also validates FP-BUILD)
   rid LMDM-RP$                                  \ cubin path (stable LMDM-RP buffer)
   rid FBD-KNAME$                                \ REGION_<raw> (SB; consumed before the next SB op)
   k FBD-BENCH-KERNEL {: ns:n :}
   s"   REGION_" type rid RGN>RAW FBD-U
   s"  inputs=" type k FBD-U s"  gpu_ns=" type ns FBD-U cr
   ns ;

: FBD-SUM-NS ( -- n )                            \ sum region-kernel gpu ns over the current plan
   0 FP-REGION-COUNT 0 ?do  i FP-REGION-ID FBD-BENCH-REGION +  loop ;

\ actual global bytes moved by region rid over the run: (K inputs + 1 output) f32 * N * iters.
: FBD-REGION-BYTES ( CAD-KIND:region -- n )
   LEW-ANALYZE LEW-NIN@ 1+ FBD-N * 4 * FBD-ITERS * ;
: FBD-MODE-BYTES ( -- n )
   0 FP-REGION-COUNT 0 ?do  i FP-REGION-ID FBD-REGION-BYTES +  loop ;

\ ---- one mode: assert region count, build cubins, run whole-model golden, bench kernels ----
\ ( ma mu expected-regions -- total-gpu-ns total-bytes ). Requires the caller set the toggle +
\ FP-BUILD (in process) and LMDM-BEGIN prepared the toolchain; the source carries the matching
\ FP-FUSE-*! suffix so the emit children re-plan the same mode.
: FBD-MODE ( ptr u8 n n -- n n ) {: exp:n :}
   FP-REGION-COUNT exp T=
   LMDM-BUILD-CUBINS
   LOWER-MODEL-GOLDEN {: v:n :}
   LOWER-GOLDEN-REASON$ type cr
   v V-PASS T=
   FBD-SUM-NS FBD-MODE-BYTES ;

\ ---- reporting -----------------------------------------------------------------------
: FBD-GBS ( n n -- n )  {: by:n ns:n :}  by 1000 * ns / ;   \ GB/s x1000 = bytes*1000/ns
: FBD-RATIO ( n n -- n ) {: a:n f:n :}  a 1000 * f / ;      \ ablated/fused latency x1000
: FBD-MS. ( n -- ) {: ns:n :}                               \ milliseconds with 3 decimals
   ns 1000000 / FBD-U $2E emit
   ns 1000000 mod 1000 / {: fr:n :}
   fr 100 < if $30 emit then  fr 10 < if $30 emit then  fr FBD-U ;

\ perf-rows.tsv line prefix: kernel grid gridy block blocky iters work GBS value device date,
\ through the trailing tab before the note (each caller then prints its own note + cr).
: FBD-ROW-HEAD ( ptr u8 n n -- ) {: val:n :}                \ kernel$ VAL
   type FBD-TAB
   FBD-GRID FBD-U FBD-TAB  1 FBD-U FBD-TAB  FBD-BLOCK FBD-U FBD-TAB  1 FBD-U FBD-TAB
   FBD-ITERS FBD-U FBD-TAB  FBD-N FBD-U FBD-TAB
   s" GBS" type FBD-TAB  val FBD-U FBD-TAB
   s" orin-nx-25w" type FBD-TAB  s" 2026-07-15" type FBD-TAB ;

: FBD-ROW-ON ( n n -- ) {: val:n ratio:n :}                 \ value ratio  (fused row + note)
   s" FUSE-CHAIN-ON" val FBD-ROW-HEAD
   s" automatic-pipeline fused Add->Mul->Relu, 1 region/1 kernel, 16 B/elem, 1 elem/thread; 25W mode-3 (8 SMs) @ 918 MHz; fusion_elapsed_ratio_x1000=" type ratio FBD-U
   s"  (maki/fusion-bench-device-test.f)" type cr ;
: FBD-ROW-OFF ( n -- ) {: val:n :}                          \ value  (ablated row + note)
   s" FUSE-CHAIN-OFF" val FBD-ROW-HEAD
   s" ablated per-op regions, 3 kernels + global round-trips, 32 B/elem, 1 elem/thread; 25W mode-3 (8 SMs) @ 918 MHz (maki/fusion-bench-device-test.f)" type cr ;

: FBD-REPORT ( n n n n -- ) {: fns:n fby:n ans:n aby:n :}   \ fused-ns fused-by ablated-ns ablated-by
   ans fns FBD-RATIO {: ratio:n :}
   cr s" == fusion-bench: automatic-pipeline fused vs ablated (orin-nx-25w) ==" type cr
   s" fused   1 kernel   " type fns FBD-MS. s"  ms/200 iters  gpu_ns=" type fns FBD-U
      s"  GBS_x1000=" type fby fns FBD-GBS FBD-U cr
   s" ablated 3 kernels  " type ans FBD-MS. s"  ms/200 iters  gpu_ns=" type ans FBD-U
      s"  GBS_x1000=" type aby ans FBD-GBS FBD-U cr
   s" fusion_elapsed_ratio_x1000=" type ratio FBD-U
      s"  (ablated/fused latency; >1000 => fused wins on effective work/round-trips)" type cr
   ans fns > TTRUE                                          \ fused finishes the chain faster
   cr s" == perf rows (paste into tools/ptx/perf-rows.tsv) ==" type cr
   fby fns FBD-GBS ratio FBD-ROW-ON
   aby ans FBD-GBS FBD-ROW-OFF ;

\ ---- the guarded device leg (both modes) ---------------------------------------------
: FBD-DEVICE-BENCH ( ptr u8 n ptr u8 n -- ) {: onp:ptr onu:n offp:ptr offu:n :}
   CUDA:OPEN? 0= if
      s" fusion-bench: off-device -> golden + bandwidth SKIPPED (plan-shape ran everywhere)" type cr
      exit then
   s" == fusion ON (automatic pipeline: 1 elementwise region -> 1 kernel) ==" type cr
   FP-FUSE-ON! FP-BUILD
   onp onu 1 FBD-MODE {: fns:n fby:n :}
   s" == fusion OFF (ablated: 3 per-op regions -> 3 kernels + global round-trips) ==" type cr
   FP-FUSE-OFF! FP-BUILD
   offp offu 3 FBD-MODE {: ans:n aby:n :}
   FP-FUSE-ON!
   fns fby ans aby FBD-REPORT ;

\ ============ the fixture: Add->Mul->Relu same-shape elementwise chain ==================
\ fusion ON -> one region (add,mul,relu fused, intermediates in registers, 3 external inputs
\ a,b,c -> 1 output); ablated -> three regions (add | mul | relu), each output round-tripped
\ through global. The child emit driver re-plans from the source string, so each carries the
\ matching FP-FUSE-*! suffix (child order: MODEL: then the toggle then FP-BUILD).
LMDM-BEGIN                                       \ T-RESET; off-device records the SKIP

s" == fusion-bench: Add->Mul->Relu 4x8, automatic pipeline (fusion ON vs ablated) ==" type cr
MODEL: FUSECHAIN ( a:4x8 b:4x8 c:4x8 -- y ) ADD MUL RELU ;

\ ---- plan-shape (everywhere, no CUDA): 1 fused region vs 3 ablated regions -------------
FP-FUSE-ON!  FP-BUILD  FP-REGION-COUNT 1 T=
FP-FUSE-OFF! FP-BUILD  FP-REGION-COUNT 3 T=
FP-FUSE-ON!

\ ---- device legs (Orin only): both modes device-correct vs the same golden + bandwidth --
s" MODEL: FUSECHAIN ( a:4x8 b:4x8 c:4x8 -- y ) ADD MUL RELU ; FP-FUSE-ON!"
s" MODEL: FUSECHAIN ( a:4x8 b:4x8 c:4x8 -- y ) ADD MUL RELU ; FP-FUSE-OFF!"
FBD-DEVICE-BENCH

LMDM-END                                         \ T-REPORT
;package
