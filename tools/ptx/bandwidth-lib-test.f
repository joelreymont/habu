\ bandwidth-lib-test.f - host-side coverage for the SAXPY bandwidth runner
\ configuration math; the device leg is a recorded SKIP off-device.

require lib/test.f
require tools/ptx/bandwidth-lib.f

package PTXBW

T-RESET

DEFAULTS
PTXBENCH:WORK@ BW-N T=
PTXBENCH:ITERS@ BW-ITERS T=
PTXBENCH:BLOCK@ BW-BLOCK T=
PTXBENCH:LABEL$ s" SAXPY" T$=
BW-EPT @ 1 T=
BW-BPE @ 12 T=
BW-FPE @ 2 T=
BW-TILE-ELEMS 256 T=
BW-GRID 4096 T=

4 ELEMS-PER-THREAD!
BW-TILE-ELEMS 1024 T=
BW-GRID 1024 T=

20 BYTES-PER-ELEM!
BW-BPE @ 20 T=
3 FLOPS-PER-ELEM!
BW-FPE @ 3 T=

s" TEST-K" KERNEL!
s" TEST-K" LABEL!
PTXBENCH:LABEL$ s" TEST-K" T$=

: BWT-DEVICE-LEG ( -- )
   CUDA:OPEN? 0= if
      s" bandwidth-lib: libcuda.so.1 unavailable -> device leg SKIPPED (host config math verified)" type cr
   then ;

BWT-DEVICE-LEG

T-REPORT

;package
