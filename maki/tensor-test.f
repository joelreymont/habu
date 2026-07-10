\ maki/tensor-test.f - runnable tests for the maki tensor shape + dtype system.

require lib/test.f
require test/checker-assert.f
require maki/tensor.f

package MAKI

T-RESET

\ dtype byte sizes (constructed family values)
MAKI-DTYPE:DF32  DT-SIZE  4 T=
MAKI-DTYPE:DF16  DT-SIZE  2 T=
MAKI-DTYPE:DBF16 DT-SIZE  2 T=
MAKI-DTYPE:DU32  DT-SIZE  4 T=
MAKI-DTYPE:DI32  DT-SIZE  4 T=

\ wire boundaries round-trip: family -> DT-* code / wire text
MAKI-DTYPE:DF32  DTYPE>N DT-F32  T=
MAKI-DTYPE:DI32  DTYPE>N DT-I32  T=
MAKI-DTYPE:DBF16 DT-KEY s" bf16" T$=

\ shape element count
3 4 SHAPE-ELEMS 12 T=
1 1 SHAPE-ELEMS  1 T=

\ broadcast compatibility (NumPy rule)
3 1 3 4 SHAPE-BCAST? TTRUE      \ (3,1) broadcasts with (3,4)
1 4 3 4 SHAPE-BCAST? TTRUE      \ (1,4) broadcasts with (3,4)
3 4 3 4 SHAPE-BCAST? TTRUE      \ identical shapes
3 4 2 4 SHAPE-BCAST? TFALSE     \ rows 3 vs 2, neither 1
3 4 3 5 SHAPE-BCAST? TFALSE     \ cols 4 vs 5, neither 1

\ total bytes = elems * dtype size
3 4 MAKI-DTYPE:DF32  TENSOR-BYTES 48 T=
2 8 MAKI-DTYPE:DF16  TENSOR-BYTES 32 T=

\ exact shape equality
3 4 3 4 SHAPE-EQUAL? TTRUE
3 4 3 5 SHAPE-EQUAL? TFALSE

\ broadcast result shape (non-1 dim wins)
3 1 3 4 BCAST-SHAPE  4 T=  3 T=     \ -> (3,4): cols then rows off the stack
1 1 6 8 BCAST-SHAPE  8 T=  6 T=     \ -> (6,8)

\ invalid dtype is a CHECKER reject, not a runtime throw (the family cannot
\ hold an out-of-range tag; swapped-role negatives pin the boundary)
s" TT-DT-OK   ( dtype -- n ) DT-SIZE"      CHECK-QUIET-CANDIDATE! -1 T=
s" TT-DT-NIN  ( n -- n ) DT-SIZE"          CHECK-QUIET-CANDIDATE! 0 T=
s" TT-DT-NOUT ( dtype -- n ) DTYPE>N 1 +"  CHECK-QUIET-CANDIDATE! -1 T=

T-REPORT

end-package
