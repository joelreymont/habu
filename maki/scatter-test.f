\ maki/scatter-test.f - checked tests for the scatter-backward references (cad-9e).
\ Hand-computed pad placement (PAD-SCATTER) and index accumulation with a duplicate
\ index (SCATTER-ADD) over row-major float-cell buffers, plus the fail-closed
\ out-of-range offset / index paths.

require lib/test.f
require maki/scatter.f

package MAKI

create SC-CT  8 cells allot
create SC-DST 8 cells allot
create SC-IX  4 cells allot

: SC-CELL ( ptr a n -- n )  T-GET 0.5 f+ f>s ;               \ read a float cell as an int
: SC-SET ( n ptr a n -- ) {: v:n base:ptr k:n :}  v s>f base k T-SET ;
: SC-IX! ( n n -- ) {: v:n k:n :}  v SC-IX k cells + ! ;

: TRY-SC-RANGE ( -- )  SC-CT 2 2 3 4 SC-DST PAD-SCATTER ;    \ r0+cr = 5 > dst rows 4
: TRY-SC-INDEX ( -- )
   0 0 SC-IX!  4 1 SC-IX!  0 2 SC-IX!                         \ idx {0,4,0}: 4 >= dst rows 3
   SC-CT 3 2 SC-IX 3 SC-DST SCATTER-ADD ;

T-RESET

\ ---- PAD-SCATTER: 2x2 cotangent into a zero 4x2 at offset r0=1 -------------
\ ct = [[1,2],[3,4]] ; dst = [[0,0],[1,2],[3,4],[0,0]]
1 SC-CT 0 SC-SET  2 SC-CT 1 SC-SET  3 SC-CT 2 SC-SET  4 SC-CT 3 SC-SET
SC-CT 2 2 1 4 SC-DST PAD-SCATTER
SC-DST 0 SC-CELL 0 T=   SC-DST 1 SC-CELL 0 T=               \ row0 zero
SC-DST 2 SC-CELL 1 T=   SC-DST 3 SC-CELL 2 T=               \ row1 = ct row0
SC-DST 4 SC-CELL 3 T=   SC-DST 5 SC-CELL 4 T=               \ row2 = ct row1
SC-DST 6 SC-CELL 0 T=   SC-DST 7 SC-CELL 0 T=               \ row3 zero

\ ---- SCATTER-ADD: 3x2 cotangent, idx {0,2,0} -> zero 3x2 (row 0 accumulates) --
\ ct = [[1,1],[2,2],[3,3]] ; dst row0 = ct0+ct2 = [4,4], row1 = [0,0], row2 = ct1 = [2,2]
1 SC-CT 0 SC-SET  1 SC-CT 1 SC-SET
2 SC-CT 2 SC-SET  2 SC-CT 3 SC-SET
3 SC-CT 4 SC-SET  3 SC-CT 5 SC-SET
0 0 SC-IX!  2 1 SC-IX!  0 2 SC-IX!
SC-CT 3 2 SC-IX 3 SC-DST SCATTER-ADD
SC-DST 0 SC-CELL 4 T=   SC-DST 1 SC-CELL 4 T=               \ row0 = ct0 + ct2
SC-DST 2 SC-CELL 0 T=   SC-DST 3 SC-CELL 0 T=               \ row1 untouched (zero)
SC-DST 4 SC-CELL 2 T=   SC-DST 5 SC-CELL 2 T=               \ row2 = ct1

\ ---- fail closed -----------------------------------------------------------
' TRY-SC-RANGE E-SC-RANGE TTHROWS
' TRY-SC-INDEX E-SC-INDEX TTHROWS

T-REPORT

end-package
