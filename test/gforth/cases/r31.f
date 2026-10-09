\ 2>r 2r> 2r@ on one-cell values: checked, inside a quotation, and at top
\ level, where they run as primitives (habu1.f:1858-1865 B2TOR B2RFROM
\ B2RFETCH) and the pair keeps its order on the return stack.
: F ( n n -- n n ) 2>r 2r> ;
: G ( n n -- n n n n ) 2>r 2r@ 2r> ;
: H ( n n -- n n ) 2>r r> r> ;
: K ( n n -- n n ) >r >r 2r> ;
: Q ( n n -- n n ) [: 2>r 2r> ;] execute ;
1 2 F . .
3 4 G . . . .
5 6 H . .
7 8 K . .
9 10 Q . .
11 12 2>r 13 14 2>r 2r@ . . 2r> . . 2r> . .
