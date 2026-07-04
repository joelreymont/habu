\ maki/reduce-bwd-test.f - checked tests for the reduce-backward references (cad-9e).
\ Hand-computed column sums (ROWSUM-BWD) and a full-reduce dot (FULLSUM-DOT-BWD)
\ over row-major float-cell buffers, plus the fail-closed column-mismatch path.

require lib/test.f
require maki/reduce-bwd.f

package MAKI

create RB-SRC 8 cells allot
create RB-CT  8 cells allot
create RB-X   8 cells allot
create RB-DST 8 cells allot

: RB-CELL ( ptr a n -- n )  T-GET 0.5 f+ f>s ;               \ read a float cell as an int
: RB-SET ( n ptr a n -- ) {: v:n base:ptr k:n :}  v s>f base k T-SET ;

: TRY-RB-COLS ( -- )  RB-SRC 3 2 RB-DST 3 ROWSUM-BWD ;       \ dst cols 3 != src cols 2

T-RESET

\ ---- ROWSUM-BWD: 3x2 -> 1x2 column sums ------------------------------------
\ src = [[1,2],[3,4],[5,6]] ; column sums = [1+3+5, 2+4+6] = [9,12]
1 RB-SRC 0 RB-SET  2 RB-SRC 1 RB-SET
3 RB-SRC 2 RB-SET  4 RB-SRC 3 RB-SET
5 RB-SRC 4 RB-SET  6 RB-SRC 5 RB-SET
RB-SRC 3 2 RB-DST 2 ROWSUM-BWD
RB-DST 0 RB-CELL 9  T=
RB-DST 1 RB-CELL 12 T=

\ ---- FULLSUM-DOT-BWD: dot of ct and x over 4 elems -> 1x1 ------------------
\ ct = [1,2,3,4] ; x = [10,20,30,40] ; sum = 10+40+90+160 = 300
1 RB-CT 0 RB-SET  2 RB-CT 1 RB-SET  3 RB-CT 2 RB-SET  4 RB-CT 3 RB-SET
10 RB-X 0 RB-SET  20 RB-X 1 RB-SET  30 RB-X 2 RB-SET  40 RB-X 3 RB-SET
RB-CT RB-X 4 RB-DST FULLSUM-DOT-BWD
RB-DST 0 RB-CELL 300 T=

\ ---- fail closed: rowsum destination cols disagree with source cols --------
' TRY-RB-COLS E-RB-COLS TTHROWS

T-REPORT

end-package
