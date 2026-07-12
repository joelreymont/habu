\ bench-test.f - focused tests for generic PTX benchmark configuration.

require lib/test.f
require lib/string.f
require tools/ptx/bench.f

package PTXBENCH

T-RESET

RESET
7 GRID!
128 BLOCK!
11 ITERS!
42 WORK!
24 PARAM-BYTES!
s" TEST-KERNEL" LABEL!

GRID@ 7 T=
BLOCK@ 128 T=
ITERS@ 11 T=
WORK@ 42 T=
PARAM-BYTES@ 24 T=
LABEL$ s" TEST-KERNEL" STR= TTRUE

T-REPORT

$3F000000 F32-MS>US 500 T=
$3F800000 F32-MS>US 1000 T=
$40000000 F32-MS>US 2000 T=
$3F000000 F32-MS>NS 500000 T=
$3F800000 F32-MS>NS 1000000 T=
$40000000 F32-MS>NS 2000000 T=

;package
