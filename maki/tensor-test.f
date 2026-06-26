\ maki/tensor-test.f - runnable tests for the maki tensor shape + dtype system.

T-RESET

\ dtype byte sizes
DT-F32  DT-SIZE  4 T=
DT-F16  DT-SIZE  2 T=
DT-BF16 DT-SIZE  2 T=
DT-U32  DT-SIZE  4 T=
DT-I32  DT-SIZE  4 T=

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
3 4 DT-F32  TENSOR-BYTES 48 T=
2 8 DT-F16  TENSOR-BYTES 32 T=

\ exact shape equality
3 4 3 4 SHAPE-EQUAL? TTRUE
3 4 3 5 SHAPE-EQUAL? TFALSE

\ broadcast result shape (non-1 dim wins)
3 1 3 4 BCAST-SHAPE  4 T=  3 T=     \ -> (3,4): cols then rows off the stack
1 1 6 8 BCAST-SHAPE  8 T=  6 T=     \ -> (6,8)

\ invalid dtype fails closed
: BAD-DT ( -- )  99 DT-SIZE drop ;
' BAD-DT E-MK-DTYPE TTHROWS

T-REPORT
