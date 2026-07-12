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
   PTXBENCH:RESET
   s" /tmp/saxpy.cubin" PTXBENCH:CUBIN!
   s" SAXPY" PTXBENCH:KERNEL!
   s" SAXPY" PTXBENCH:LABEL!
   BW-BLOCK PTXBENCH:BLOCK!
   BW-ITERS PTXBENCH:ITERS!
   BW-N PTXBENCH:WORK!
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

: BW-SETUP ( -- )
   PTXBENCH:OPEN
   PTXBENCH:LOAD ;

: BW-ALLOC ( -- )
   BW-N 4 * BW-DX PTXBENCH:DEVICE-ALLOC
   BW-N 4 * BW-DY PTXBENCH:DEVICE-ALLOC
   BW-DX @ 0 BW-N PTXBENCH:DEVICE-MEMSET32
   BW-DY @ 0 BW-N PTXBENCH:DEVICE-MEMSET32 ;

: BW-TILE-ELEMS ( -- n )
   BW-BLOCK BW-EPT @ * ;

: BW-GRID ( -- n )
   BW-N BW-TILE-ELEMS 1- + BW-TILE-ELEMS / ;

: BW-PARAMS ( -- )
   $40000000 BW-ABITS !
   BW-N BW-NV !
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
   BW-SETUP BW-ALLOC BW-PARAMS
   BW-RUN {: ns:n :}
   BW-RELEASE
   ns ;

: REPORT-NS ( n -- )
   {: ns:n :}
   BW-N BW-ITERS * BW-BPE @ * {: by:n :}
   BW-N BW-ITERS * BW-FPE @ * {: fl:n :}
   s" elems_per_thread=" type BW-EPT @ .U cr
   by fl ns PTXBENCH:REPORT-GPU ;

: REPORT ( -- )
   MEASURE REPORT-NS ;

;package
