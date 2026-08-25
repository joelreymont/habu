\ profile-test.f - focused tests for PTX benchmark/profile math.

require lib/test.f
require tools/ptx/profile.f

package PTXPROF

T-RESET

100 2 TRIAD-BYTES 2400 T=
100 2 SAXPY-FLOPS 400 T=
63000 1000 GBS-X1000 63000 T=
940000 1000 GFLOPS-X1000 940000 T=
63000 MEM-ROOF-GBS-X1000 UTIL-X1000 1000 T=
470000 FP32-ROOF-GFLOPS-X1000 UTIL-X1000 500 T=

T-REPORT

;package
