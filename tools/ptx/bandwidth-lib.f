\ bandwidth-lib.f - reusable Orin SAXPY-family kernel bandwidth runner.

require lib/errors.f
require lib/string.f
require lib/ptx/cuda-driver.f
require tools/ptx/profile.f
require tools/ptx/bench.f

package PTXBW

variable BW-N                       \ DRAM working-set size in f32 elements; derived from the device L2, never fixed
$4000000 constant BW-N-FLOOR        \ 64 Mi elements: floor that must exceed any GB10-class L2 working set
38 constant CU-ATTR-L2-BYTES        \ CU_DEVICE_ATTRIBUTE_L2_CACHE_SIZE
-7303 constant E-BW-L2              \ device L2 cache-size read failed; the DRAM roof cannot be sized off-device
200 constant BW-ITERS
256 constant BW-BLOCK

variable BW-DX
variable BW-DY
variable BW-ABITS
variable BW-NV
variable BW-EPT
variable BW-BPE
variable BW-FPE
variable BW-DEV                     \ CUDA device handle, used only to read the L2 attribute
variable BW-L2                      \ 8-byte scratch cell the 4-byte L2 attribute read writes into

public

\ DEFAULTS sets kernel/label/block/iters only; WORK (the report's item count) is
\ set from the device-derived BW-N inside MEASURE, so DEFAULTS stays host-only.
\ The CUBIN path is NOT defaulted to a shared /tmp artifact - callers self-emit
\ and set CUBIN! to a private per-run PTXTC:CUBIN$, so a missing cubin fails
\ closed in PTXBENCH:LOAD ("cubin path not set"), not on a stale /tmp/saxpy.cubin.
: DEFAULTS ( -- )
   PTXBENCH:RESET
   s" SAXPY" PTXBENCH:KERNEL!
   s" SAXPY" PTXBENCH:LABEL!
   BW-BLOCK PTXBENCH:BLOCK!
   BW-ITERS PTXBENCH:ITERS!
   12 BW-BPE !
   2 BW-FPE !
   1 BW-EPT ! ;

: CUBIN! ( ptr u8 n -- )
   PTXBENCH:CUBIN! ;

: KERNEL! ( ptr u8 n -- )
   PTXBENCH:KERNEL! ;

: LABEL! ( ptr u8 n -- )
   PTXBENCH:LABEL! ;

: ELEMS-PER-THREAD! ( n -- )
   BW-EPT ! ;

: BYTES-PER-ELEM! ( n -- )
   BW-BPE ! ;

: FLOPS-PER-ELEM! ( n -- )
   BW-FPE ! ;

private

\ N = max(BW-N-FLOOR, 8*L2/4): an 8x-L2 working set in f32 elements, floored so a
\ small-L2 device still probes DRAM, not cache. Input is L2 bytes; pure math,
\ unit-tested off-device.
: BW-N-FROM-L2 ( n -- n )
   8 * 4 / BW-N-FLOOR max ;

\ Live device L2 cache size in bytes; a nonzero attribute rc is a named throw, not
\ a fallback - an off-device probe measures nothing and must fail closed.
: BW-READ-L2 ( -- n )
   0 BW-L2 !
   BW-L2 CU-ATTR-L2-BYTES BW-DEV @ >CUDA-DEV CUDA:CU-DEVICE-GET-ATTRIBUTE
   RC>N 0 <> if E-BW-L2 throw then
   BW-L2 @ ;

\ Size BW-N past the live device's L2, fresh before each probe so the value always
\ reflects the current device (no cached constant that could go stale across runs).
: BW-DERIVE-N ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   BW-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   BW-READ-L2 BW-N-FROM-L2 BW-N ! ;

: BW-SETUP ( -- )
   PTXBENCH:OPEN
   PTXBENCH:LOAD ;

: BW-ALLOC ( -- )
   BW-N @ 4 * BW-DX PTXBENCH:DEVICE-ALLOC
   BW-N @ 4 * BW-DY PTXBENCH:DEVICE-ALLOC
   BW-DX @ 0 BW-N @ PTXBENCH:DEVICE-MEMSET32
   BW-DY @ 0 BW-N @ PTXBENCH:DEVICE-MEMSET32 ;

: BW-TILE-ELEMS ( -- n )
   BW-BLOCK BW-EPT @ * ;

: BW-GRID ( -- n )
   BW-N @ BW-TILE-ELEMS 1- + BW-TILE-ELEMS / ;

: BW-PARAMS ( -- )
   $40000000 BW-ABITS !
   BW-N @ BW-NV !
   BW-GRID PTXBENCH:GRID!
   24 PTXBENCH:PARAM-BYTES!
   PTXBENCH:PREPARE-LAUNCH
   0 BW-DX PTXBENCH:PARAM-PTR!
   8 BW-DY PTXBENCH:PARAM-PTR!
   16 BW-ABITS PTXBENCH:PARAM-U32!
   20 BW-NV PTXBENCH:PARAM-U32! ;

: BW-RUN ( -- n )
   PTXBENCH:BENCH-GPU-NS ;

: BW-FREE ( -- )
   BW-DX @ 0 <> if BW-DX @ PTXBENCH:DEVICE-FREE then
   BW-DY @ 0 <> if BW-DY @ PTXBENCH:DEVICE-FREE then
   0 BW-DX ! 0 BW-DY ! ;

: BW-RELEASE ( -- )
   BW-FREE
   PTXBENCH:UNLOAD
   PTXBENCH:CLOSE ;

public

: MEASURE ( -- n )
   BW-DERIVE-N BW-N @ PTXBENCH:WORK!
   BW-SETUP BW-ALLOC BW-PARAMS
   BW-RUN {: ns:n :}
   BW-RELEASE
   ns ;

: REPORT-NS ( n -- )
   {: ns:n :}
   BW-N @ BW-ITERS * BW-BPE @ * {: by:n :}
   BW-N @ BW-ITERS * BW-FPE @ * {: fl:n :}
   s" elems_per_thread=" type BW-EPT @ .U cr
   by fl ns PTXBENCH:REPORT-GPU ;

: REPORT ( -- )
   MEASURE REPORT-NS ;

;package
