\ bandwidth-lib.f - reusable Orin SAXPY-family kernel bandwidth runner.

require lib/errors.f
require lib/string.f
require tools/ptx/profile.f
require tools/ptx/bench.f

package PTXBW

$100000 constant BW-N
200 constant BW-ITERS
256 constant BW-BLOCK

variable BW-DX
variable BW-DY
variable BW-ABITS
variable BW-NV
variable BW-EPT
variable BW-BPE
variable BW-FPE

public

: DEFAULTS ( -- )
   PTX:BENCH-RESET
   s" /tmp/saxpy.cubin" PTX:BENCH-CUBIN!
   s" SAXPY" PTX:BENCH-KERNEL!
   s" SAXPY" PTX:BENCH-LABEL!
   BW-BLOCK PTX:BENCH-BLOCK!
   BW-ITERS PTX:BENCH-ITERS!
   BW-N PTX:BENCH-WORK!
   12 BW-BPE !
   2 BW-FPE !
   1 BW-EPT ! ;

: CUBIN! ( ptr u8 n -- )
   PTX:BENCH-CUBIN! ;

: KERNEL! ( ptr u8 n -- )
   PTX:BENCH-KERNEL! ;

: LABEL! ( ptr u8 n -- )
   PTX:BENCH-LABEL! ;

: ELEMS-PER-THREAD! ( n -- )
   BW-EPT ! ;

: BYTES-PER-ELEM! ( n -- )
   BW-BPE ! ;

: FLOPS-PER-ELEM! ( n -- )
   BW-FPE ! ;

private

: BW-SETUP ( -- )
   PTX:DEVICE-OPEN
   PTX:MODULE-LOAD ;

: BW-ALLOC ( -- )
   BW-N 4 * BW-DX PTX:DEVICE-ALLOC
   BW-N 4 * BW-DY PTX:DEVICE-ALLOC
   BW-DX @ 0 BW-N PTX:DEVICE-MEMSET32
   BW-DY @ 0 BW-N PTX:DEVICE-MEMSET32 ;

: BW-TILE-ELEMS ( -- n )
   BW-BLOCK BW-EPT @ * ;

: BW-GRID ( -- n )
   BW-N BW-TILE-ELEMS 1- + BW-TILE-ELEMS / ;

: BW-PARAMS ( -- )
   $40000000 BW-ABITS !
   BW-N BW-NV !
   BW-GRID PTX:BENCH-GRID!
   24 PTX:KERNEL-PARAM-BYTES!
   PTX:KERNEL-PREPARE-LAUNCH
   0 BW-DX PTX:KERNEL-PARAM-PTR!
   8 BW-DY PTX:KERNEL-PARAM-PTR!
   16 BW-ABITS PTX:KERNEL-PARAM-U32!
   20 BW-NV PTX:KERNEL-PARAM-U32! ;

: BW-RUN ( -- n )
   PTX:BENCH-GPU-NS ;

: BW-FREE ( -- )
   BW-DX @ 0 <> if BW-DX @ PTX:DEVICE-FREE then
   BW-DY @ 0 <> if BW-DY @ PTX:DEVICE-FREE then
   0 BW-DX ! 0 BW-DY ! ;

: BW-RELEASE ( -- )
   BW-FREE
   PTX:MODULE-UNLOAD
   PTX:DEVICE-CLOSE ;

public

: MEASURE ( -- n )
   BW-SETUP BW-ALLOC BW-PARAMS
   BW-RUN {: ns:n :}
   BW-RELEASE
   ns ;

: REPORT-NS ( n -- )
   {: ns:n :}
   BW-N BW-ITERS * BW-BPE @ * {: by:n :}
   BW-N BW-ITERS * BW-FPE @ * {: fl:n :}
   s" elems_per_thread=" type BW-EPT @ U.0 cr
   by fl ns PTX:BENCH-REPORT-GPU ;

: REPORT ( -- )
   MEASURE REPORT-NS ;

end-package
