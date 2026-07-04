\ maki/move-test.f - checked tests for the buffer-level movement references.
\ Exact element rewrites (reshape/transpose/slice/concat/gather) over row-major
\ float-cell buffers, plus every fail-closed extent / range / index path.

require lib/test.f
require maki/move.f

package MAKI

\ ---- scratch buffers + helpers ---------------------------------------------
create MV-SRC 8 cells allot
create MV-DST 8 cells allot
create MV-B   4 cells allot
create MV-IX  4 cells allot

: MV-CELL ( ptr a n -- n )  T-GET 0.5 f+ f>s ;     \ read a float cell as an int

: MV-SEQ ( r ptr a n -- ) {: v0:r base:ptr n:n :}  \ fill base[i] = v0 + i
   n 0 ?do  v0 i s>f f+  base i T-SET  loop ;

\ ---- fail-closed probes (top level cannot push quotations) -----------------
: TRY-MV-EXTENT   ( -- )  MV-SRC 2 3 MV-DST 2 2 MOVE-RESHAPE ;      \ 6 != 4
: TRY-MV-TR-DIM   ( -- )  MV-SRC 2 3 MV-DST 2 3 MOVE-TRANSPOSE ;    \ dst must be 3x2
: TRY-MV-CC-COLS  ( -- )  MV-SRC 2 2 MV-B 1 3 MV-DST MOVE-CONCAT ;  \ 2 != 3
: TRY-MV-RANGE-HI ( -- )  MV-SRC 4 2 2 5 MV-DST MOVE-SLICE ;        \ r1 > rows
: TRY-MV-RANGE-INV ( -- ) MV-SRC 4 2 3 1 MV-DST MOVE-SLICE ;        \ r0 > r1
: TRY-MV-INDEX    ( -- )
   3 MV-IX 0 cells + !  0 MV-IX 1 cells + !                          \ idx {3,0}, 3 >= rows
   MV-SRC 3 2 MV-IX 2 MV-DST MOVE-GATHER ;

T-RESET

\ ---- reshape: 2x3 -> 3x2 is a straight copy (order preserved) ---------------
1.0 MV-SRC 6 MV-SEQ                                 \ src = 1..6 row-major
MV-SRC 2 3 MV-DST 3 2 MOVE-RESHAPE
MV-DST 0 MV-CELL 1 T=
MV-DST 5 MV-CELL 6 T=
MV-DST 6 T-SUM 0.5 f+ f>s 21 T=                     \ 1+..+6

\ ---- transpose: 2x3 -> 3x2, dst[k,i] = src[i,k] ----------------------------
MV-SRC 2 3 MV-DST 3 2 MOVE-TRANSPOSE
MV-DST 0 MV-CELL 1 T=                               \ src[0,0]
MV-DST 1 MV-CELL 4 T=                               \ src[1,0]
MV-DST 2 MV-CELL 2 T=                               \ src[0,1]
MV-DST 3 MV-CELL 5 T=                               \ src[1,1]

\ ---- slice: rows [1,3) of a 4x2 buffer -------------------------------------
10.0 MV-SRC 8 MV-SEQ                                \ src = 10..17 row-major (4x2)
MV-SRC 4 2 1 3 MV-DST MOVE-SLICE
MV-DST 0 MV-CELL 12 T=                              \ row 1 = {12,13}
MV-DST 3 MV-CELL 15 T=                              \ row 2 = {14,15}

\ ---- concat: A(2x2) row-append B(1x2) -> 3x2 -------------------------------
1.0 MV-SRC 4 MV-SEQ                                 \ A = {1,2,3,4}
5.0 MV-B   2 MV-SEQ                                 \ B = {5,6}
MV-SRC 2 2 MV-B 1 2 MV-DST MOVE-CONCAT
MV-DST 0 MV-CELL 1 T=
MV-DST 4 MV-CELL 5 T=
MV-DST 5 MV-CELL 6 T=

\ ---- gather: pick rows {2,0} of a 3x2 buffer -------------------------------
7.0 MV-SRC 6 MV-SEQ                                 \ src = {7,8,9,10,11,12}
2 MV-IX 0 cells + !  0 MV-IX 1 cells + !
MV-SRC 3 2 MV-IX 2 MV-DST MOVE-GATHER
MV-DST 0 MV-CELL 11 T=                              \ row 2 = {11,12}
MV-DST 2 MV-CELL 7 T=                               \ row 0 = {7,8}

\ ---- fail closed -----------------------------------------------------------
' TRY-MV-EXTENT    E-MV-EXTENT TTHROWS
' TRY-MV-TR-DIM    E-MV-EXTENT TTHROWS
' TRY-MV-CC-COLS   E-MV-EXTENT TTHROWS
' TRY-MV-RANGE-HI  E-MV-RANGE  TTHROWS
' TRY-MV-RANGE-INV E-MV-RANGE  TTHROWS
' TRY-MV-INDEX     E-MV-INDEX  TTHROWS

T-REPORT

end-package
