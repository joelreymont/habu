\ maki/tensor-test.f - nominal tensor metadata tests (typed shape + dtype enum).

require lib/test.f
require test/checker-assert.f
require maki/tensor.f

package MAKI

T-RESET

\ dtype byte sizes (constructed family values -> CAD-KIND:dim)
MAKI-DTYPE:DF32  DT-SIZE 4 DIM-IS? TTRUE
MAKI-DTYPE:DF16  DT-SIZE 2 DIM-IS? TTRUE
MAKI-DTYPE:DBF16 DT-SIZE 2 DIM-IS? TTRUE
MAKI-DTYPE:DU32  DT-SIZE 4 DIM-IS? TTRUE
MAKI-DTYPE:DI32  DT-SIZE 4 DIM-IS? TTRUE

\ wire boundaries round-trip: family -> DT-* code / wire text
MAKI-DTYPE:DF32  DTYPE>N DT-F32  T=
MAKI-DTYPE:DI32  DTYPE>N DT-I32  T=
MAKI-DTYPE:DBF16 DT-KEY s" bf16" T$=

\ typed shape element count
3 4 SHAPE SHAPE-ELEMS 12 DIM-IS? TTRUE
1 1 SHAPE SHAPE-ELEMS 1 DIM-IS? TTRUE

\ broadcast compatibility (NumPy rule)
3 1 SHAPE 3 4 SHAPE SHAPE-BCAST? TTRUE      \ (3,1) broadcasts with (3,4)
1 4 SHAPE 3 4 SHAPE SHAPE-BCAST? TTRUE      \ (1,4) broadcasts with (3,4)
3 4 SHAPE 3 4 SHAPE SHAPE-BCAST? TTRUE      \ identical shapes
3 4 SHAPE 2 4 SHAPE SHAPE-BCAST? TFALSE     \ rows 3 vs 2, neither 1
3 4 SHAPE 3 5 SHAPE SHAPE-BCAST? TFALSE     \ cols 4 vs 5, neither 1

\ total bytes = elems * dtype size
3 4 SHAPE MAKI-DTYPE:DF32 TENSOR-BYTES 48 DIM-IS? TTRUE
2 8 SHAPE MAKI-DTYPE:DF16 TENSOR-BYTES 32 DIM-IS? TTRUE

\ exact shape equality + literal-shape assert
3 4 SHAPE 3 4 SHAPE SHAPE-EQUAL? TTRUE
3 4 SHAPE 3 5 SHAPE SHAPE-EQUAL? TFALSE
3 4 SHAPE 3 4 SHAPE-IS? TTRUE

\ broadcast result shape (non-1 dim wins) + transpose algebra
3 1 SHAPE 3 4 SHAPE BCAST-SHAPE 3 4 SHAPE SHAPE-EQUAL? TTRUE
1 1 SHAPE 6 8 SHAPE BCAST-SHAPE 6 8 SHAPE SHAPE-EQUAL? TTRUE
3 4 SHAPE TRANSPOSE-SHAPE 4 3 SHAPE SHAPE-EQUAL? TTRUE

\ literal row constructors + the owner role conversion (dim -> rows)
ZERO-ROWS 0 1 SHAPE drop ROWS-EQUAL? TTRUE
ZERO-ROWS UNIT-ROWS ROWS-EQUAL? TFALSE
3 4 SHAPE SHAPE-ELEMS DIM>ROWS 12 1 SHAPE drop ROWS-EQUAL? TTRUE

\ dim algebra over the nominal families
3 1 SHAPE SHAPE-ELEMS 1 1 SHAPE SHAPE-ELEMS DIM-BCAST? TTRUE
3 1 SHAPE SHAPE-ELEMS MAKI-DTYPE:DF32 DT-SIZE DIM-BCAST? TFALSE
3 1 SHAPE SHAPE-ELEMS MAKI-DTYPE:DF32 DT-SIZE DIM-MAX 4 DIM-IS? TTRUE
3 1 SHAPE SHAPE-ELEMS 1 3 SHAPE SHAPE-ELEMS DIM-EQUAL? TTRUE
3 1 SHAPE drop 4 1 SHAPE drop ROWS-EQUAL? TFALSE
1 5 SHAPE nip 2 5 SHAPE nip COLS-EQUAL? TTRUE
2 1 SHAPE drop 3 1 SHAPE drop ROWS+ 5 1 SHAPE drop ROWS-EQUAL? TTRUE
8 1 SHAPE drop 3 1 SHAPE drop ROWS- 5 1 SHAPE drop ROWS-EQUAL? TTRUE
1 2 SHAPE nip 1 3 SHAPE nip COLS+ 1 5 SHAPE nip COLS-EQUAL? TTRUE
1 8 SHAPE nip 1 3 SHAPE nip COLS- 1 5 SHAPE nip COLS-EQUAL? TTRUE
MAKI-DTYPE:DF16 DT-SIZE MAKI-DTYPE:DBF16 DT-SIZE DIM* 4 DIM-IS? TTRUE

\ address-space decode + identity
2 ADDRESS-SPACE-DECODE SPACE-SHARED ADDRESS-SPACE-EQUAL? TTRUE
SPACE-HOST SPACE-GLOBAL ADDRESS-SPACE-EQUAL? TFALSE
0 ADDRESS-SPACE-VALID? TTRUE 4 ADDRESS-SPACE-VALID? TFALSE

\ dtype identity via the derived enum compare
MAKI-DTYPE:DF32 MAKI-DTYPE:DF16 MAKI-DTYPE:EQ TFALSE
MAKI-DTYPE:DF32 MAKI-DTYPE:DF32 MAKI-DTYPE:EQ TTRUE

\ inner-dim agreement (X cols vs W rows)
1 2 SHAPE nip 2 1 SHAPE drop INNER-EQUAL? TTRUE

\ fail-closed dimension / address-space domains (runtime throws survive R3)
: TN-BAD-DIM ( -- )
   -1 1 SHAPE 2drop ;

: TN-BAD-SPACE ( -- )
   99 ADDRESS-SPACE-DECODE drop ;

: TN-BAD-BCAST ( -- )
   2 3 SHAPE 4 5 SHAPE BCAST-SHAPE 2drop ;

' TN-BAD-DIM E-MK-DIM TTHROWS
' TN-BAD-SPACE E-MK-SPACE TTHROWS
' TN-BAD-BCAST E-MK-DIM TTHROWS

\ invalid dtype is a CHECKER reject, not a runtime throw (the enum family cannot
\ hold an out-of-range tag; swapped-role negatives pin the boundary). Master's
\ `99 DTYPE-DECODE` runtime negative is re-pointed here: no n->dtype conversion
\ exists on this tree, so the raw-n launder itself is the checker negative.
s" TT-DT-OK   ( dtype -- CAD-KIND:dim ) DT-SIZE"  CHECK-QUIET-CANDIDATE! -1 T=
s" TT-DT-NIN  ( n -- CAD-KIND:dim ) DT-SIZE"      CHECK-QUIET-CANDIDATE! 0 T=
s" TT-DT-NOUT ( dtype -- n ) DTYPE>N 1 +"         CHECK-QUIET-CANDIDATE! -1 T=

\ Same-cell semantic swaps reject before runtime (Model-CAD V2 R3 acceptance).
s" TENSOR-NEG-DIM-DTYPE ( CAD-KIND:dim dtype -- CAD-KIND:dim ) MAKI:DIM*" CHECK-QUIET-CANDIDATE! 0 T=
s" TENSOR-NEG-DIM-FOR-DTYPE ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:dim -- CAD-KIND:dim ) MAKI:TENSOR-BYTES" CHECK-QUIET-CANDIDATE! 0 T=
s" TENSOR-NEG-ROWS-COLS ( CAD-KIND:cols CAD-KIND:rows -- CAD-KIND:dim ) MAKI:SHAPE-ELEMS" CHECK-QUIET-CANDIDATE! 0 T=
s" TENSOR-DIM-ROWS-OK ( CAD-KIND:dim -- CAD-KIND:rows ) MAKI:DIM>ROWS" CHECK-QUIET-CANDIDATE! -1 T=
s" TENSOR-NEG-DIM-ROWS-AS-COLS ( CAD-KIND:dim -- CAD-KIND:cols ) MAKI:DIM>ROWS" CHECK-QUIET-CANDIDATE! 0 T=

T-REPORT

end-package
