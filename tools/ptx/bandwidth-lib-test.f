\ bandwidth-lib-test.f - host-side coverage for the SAXPY bandwidth runner
\ configuration math; the device leg is a recorded SKIP off-device.

require lib/test.f
require tools/ptx/bandwidth-lib.f

package PTXBW

T-RESET

DEFAULTS
PTXBENCH:ITERS@ BW-ITERS T=
PTXBENCH:BLOCK@ BW-BLOCK T=
PTXBENCH:LABEL$ s" SAXPY" T$=
BW-EPT @ 1 T=
BW-BPE @ 12 T=
BW-FPE @ 2 T=

\ derivation: N = max(BW-N-FLOOR, 8*L2/4) from a synthetic L2, device-independent.
\ 4 MiB L2 -> 8*L2/4 = 8 Mi elems, below the floor -> floored at BW-N-FLOOR (64 Mi).
$400000 BW-N-FROM-L2 BW-N-FLOOR T=
\ 128 MiB L2 -> 8*L2/4 = 256 Mi elems, above the floor.
$8000000 BW-N-FROM-L2 $10000000 T=

\ grid = ceil(BW-N/tile); pin the tile math against a synthetic BW-N, not a device value.
$100000 BW-N !
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
