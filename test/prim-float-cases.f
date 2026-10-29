\ The float primitive case sets as data, one per float row of src/habu/prims.f
\ in table order, as test/prim-cases.f holds the integer rows; the file defines
\ no word. Columns are integers: a runner passes each input through s>f and each
\ real answer through f>s, except s>f's input and f>s's answer. A flag is 0 or
\ 1. Fractional semantics belong to src/lib/float-test.f and f64 text.

\ A runner includes the file (never require) with CASES, ;CASES and the shapes
\ NN-N NN-F RR-R ( n n n -- ) and N-N N-F R-R ( n n -- ) in scope; the first
\ four mean what they mean in prim-cases.f.

\ RR-R and R-R cases are a double's bits in and out: a NaN made from numbers is
\ $7FF8000000000000 on every target, and a NaN operand passes through, the left
\ one of two (src/compiler/native/hir-word.f DEF-FLOAT).

0 CASES f+
           3           4                   7  NN-N
          -3           4                   1  NN-N
           0           0                   0  NN-N
  $7FF0000000000000  $FFF0000000000000  $7FF8000000000000  RR-R
                  0  $FFF8000000000000  $FFF8000000000000  RR-R
;CASES

0 CASES f-
           7           4                   3  NN-N
           4           7                  -3  NN-N
  $7FF0000000000000  $7FF0000000000000  $7FF8000000000000  RR-R
  $FFF8000000000000                  0  $FFF8000000000000  RR-R
;CASES

0 CASES f*
           6           7                  42  NN-N
          -6           7                 -42  NN-N
                  0  $7FF0000000000000  $7FF8000000000000  RR-R
  $7FF8000000000001  $FFF8000000000002  $7FF8000000000001  RR-R
;CASES

0 CASES f/
          12           3                   4  NN-N
         -12           3                  -4  NN-N
                  0                  0  $7FF8000000000000  RR-R
;CASES

0 CASES fnegate
           5                              -5  N-N
          -5                               5  N-N
;CASES

0 CASES fabs
          -5                               5  N-N
           5                               5  N-N
;CASES

0 CASES fsqrt
          16                               4  N-N
           0                               0  N-N
  $BFF0000000000000                     $7FF8000000000000  R-R
  $FFF8000000000000                     $FFF8000000000000  R-R
;CASES

0 CASES f<
           3           4                   1  NN-F
           4           3                   0  NN-F
;CASES

0 CASES f>
           4           3                   1  NN-F
           3           4                   0  NN-F
;CASES

0 CASES f=
           3           3                   1  NN-F
           3           4                   0  NN-F
;CASES

0 CASES f0<
          -1                               1  N-F
           1                               0  N-F
           0                               0  N-F
;CASES

0 CASES f0=
           0                               1  N-F
           1                               0  N-F
;CASES

0 CASES s>f
           7                               7  N-N
          -7                              -7  N-N
;CASES

0 CASES f>s
           7                               7  N-N
          -7                              -7  N-N
;CASES
