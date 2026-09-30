\ prim-cases.f — the integer primitive case sets, as data.
\
\ One case set per row of src/habu/prims.f, in table order: every line is a
\ row's inputs and expected outputs, ended by the shape word that runs it. The
\ file defines no word. A file runs the sets by including it (`include`, never
\ `require`, so a second includer in one image is not skipped) with these in
\ scope:
\
\ - `CASES ( n -- )`, which parses the primitive name after it and opens the
\   set for that name's zero-based overload row, and `;CASES`, which closes it;
\ - the fourteen shape words, a flag column being 0 or 1:
\     SHUF ( n -- )                    1 2 3 4 in, the window's digit spelling out
\     NN-N NN-F FF-F ( n n n -- )      two inputs, the expected result
\     N-N N-F ( n n -- )               one input, the expected result
\     NN-NN ( n n n n -- )             two inputs, the two expected results
\     NN-THROWS ( n n n -- )           two inputs, the expected throw code
\     MEM ( n n -- )                   a value put through the row's memory
\                                      scenario, the expected answer
\     PN-P NP-P PP-N PP-F ( n n n -- ) two inputs, the expected result
\     P-P ( n n -- )                   one input, the expected result
\   where a pointer column or result is a byte offset into one declared byte
\   fixture (`ptr-field` refuses a raw `create` base);
\ - `MAX-N` and `MIN-N`, the extreme cells, and `E-DIV-ZERO`;
\ - a `+!` scenario that adds 3 to the stored cell.

\ ---- stack shufflers: 1 2 3 4 in, the whole window out ----------------------
0 CASES dup     12344 SHUF ;CASES
0 CASES drop      123 SHUF ;CASES
0 CASES swap     1243 SHUF ;CASES
0 CASES over    12343 SHUF ;CASES
0 CASES nip       124 SHUF ;CASES
0 CASES tuck    12434 SHUF ;CASES
0 CASES rot      1342 SHUF ;CASES
0 CASES -rot     1423 SHUF ;CASES
0 CASES 2dup   123434 SHUF ;CASES
0 CASES 2drop      12 SHUF ;CASES
0 CASES 2swap    3412 SHUF ;CASES
0 CASES 2over  123412 SHUF ;CASES

\ ---- arithmetic --------------------------------------------------------------
0 CASES +
           3           4                   7  NN-N
          -7           4                  -3  NN-N
           0           0                   0  NN-N
       MAX-N           1               MIN-N  NN-N     \ wraps
       MIN-N       MIN-N                   0  NN-N
;CASES

1 CASES +                              \ (ptr a n -- ptr a)
           4           3                   7  PN-P
           8          -3                   5  PN-P
           0           0                   0  PN-P
;CASES

2 CASES +                              \ (n ptr a -- ptr a)
           3           4                   7  NP-P
          -3           8                   5  NP-P
           0           0                   0  NP-P
;CASES

0 CASES -
           7           4                   3  NN-N
           4           7                  -3  NN-N
       MIN-N           1               MAX-N  NN-N     \ wraps
          -1          -1                   0  NN-N
;CASES

1 CASES -                              \ (ptr a n -- ptr a)
           8           3                   5  PN-P
           4          -3                   7  PN-P
           0           0                   0  PN-P
;CASES

2 CASES -                              \ (ptr a ptr a -- n)
           8           3                   5  PP-N
           3           8                  -5  PP-N
           4           4                   0  PP-N
;CASES

0 CASES *
           6           7                  42  NN-N
          -6           7                 -42  NN-N
          -6          -7                  42  NN-N
           0       MAX-N                   0  NN-N
 $100000000 $100000000                   0  NN-N     \ wraps
;CASES

\ Truncated toward zero, so the remainder carries the dividend's sign. A zero
\ divisor is refused by name and MIN-N -1 wraps: both are contracts every backend
\ answers, stated in docs/forth.md.
0 CASES /
           7           2                   3  NN-N
          -7           2                  -3  NN-N
           7          -2                  -3  NN-N
          -7          -2                   3  NN-N
           6           3                   2  NN-N
           0           5                   0  NN-N
       MAX-N           2   $3FFFFFFFFFFFFFFF  NN-N
       MIN-N          -1               MIN-N  NN-N     \ wraps
           7           0          E-DIV-ZERO  NN-THROWS
       MIN-N           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES mod
           7           2                   1  NN-N
          -7           2                  -1  NN-N
           7          -2                   1  NN-N
          -7          -2                  -1  NN-N
           6           3                   0  NN-N
       MIN-N          -1                   0  NN-N     \ the wrapped quotient's remainder
           7           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES /mod
           7           2               1     3  NN-NN
          -7           2              -1    -3  NN-NN
           7          -2               1    -3  NN-NN
          -7          -2              -1     3  NN-NN
       MIN-N          -1               0 MIN-N  NN-NN   \ wraps
           7           0          E-DIV-ZERO  NN-THROWS
;CASES

0 CASES and
         $F0         $3C                 $30  NN-N
          -1         $FF                 $FF  NN-N
           0          -1                   0  NN-N
;CASES
1 CASES and
           1           1                   1  FF-F
           1           0                   0  FF-F
           0           0                   0  FF-F
;CASES

0 CASES or
         $F0         $0C                 $FC  NN-N
           0           0                   0  NN-N
          -1           0                  -1  NN-N
;CASES
1 CASES or
           1           0                   1  FF-F
           0           0                   0  FF-F
           1           1                   1  FF-F
;CASES

0 CASES xor
         $F0         $3C                 $CC  NN-N
          -1          -1                   0  NN-N
         $FF           0                 $FF  NN-N
;CASES
1 CASES xor
           1           1                   0  FF-F
           1           0                   1  FF-F
           0           0                   0  FF-F
;CASES

0 CASES 1+
           3                               4  N-N
          -1                               0  N-N
       MAX-N                           MIN-N  N-N     \ wraps
;CASES

1 CASES 1+
           0                               1  P-P
           7                               8  P-P
;CASES

0 CASES 1-
           3                               2  N-N
           0                              -1  N-N
       MIN-N                           MAX-N  N-N     \ wraps
;CASES

1 CASES 1-
           1                               0  P-P
           8                               7  P-P
;CASES

0 CASES negate
           5                              -5  N-N
          -5                               5  N-N
           0                               0  N-N
       MIN-N                           MIN-N  N-N     \ the one fixed point
;CASES

0 CASES invert
           0                              -1  N-N
          -1                               0  N-N
         $F0            $FFFFFFFFFFFFFF0F  N-N
;CASES

0 CASES 0=
           0                               1  N-F
           1                               0  N-F
          -1                               0  N-F
;CASES

0 CASES 0<
          -1                               1  N-F
           0                               0  N-F
           1                               0  N-F
       MIN-N                               1  N-F
       MAX-N                               0  N-F
;CASES

0 CASES =
           3           3                   1  NN-F
           3           4                   0  NN-F
          -1          -1                   1  NN-F
;CASES

1 CASES =
           3           3                   1  PP-F
           3           4                   0  PP-F
;CASES

0 CASES <
           3           4                   1  NN-F
           4           3                   0  NN-F
           3           3                   0  NN-F
          -1           1                   1  NN-F
       MIN-N       MAX-N                   1  NN-F
;CASES

1 CASES <
           3           4                   1  PP-F
           4           3                   0  PP-F
           3           3                   0  PP-F
;CASES

0 CASES >
           4           3                   1  NN-F
           3           4                   0  NN-F
       MAX-N       MIN-N                   1  NN-F
;CASES

1 CASES >
           4           3                   1  PP-F
           3           4                   0  PP-F
           3           3                   0  PP-F
;CASES

0 CASES <>
           3           3                   0  NN-F
           3           4                   1  NN-F
;CASES

1 CASES <>
           3           3                   0  PP-F
           3           4                   1  PP-F
;CASES

0 CASES <=
           3           3                   1  NN-F
           3           4                   1  NN-F
           4           3                   0  NN-F
       MIN-N       MAX-N                   1  NN-F
;CASES

1 CASES <=
           3           3                   1  PP-F
           3           4                   1  PP-F
           4           3                   0  PP-F
;CASES

0 CASES >=
           3           3                   1  NN-F
           4           3                   1  NN-F
           3           4                   0  NN-F
;CASES

1 CASES >=
           3           3                   1  PP-F
           4           3                   1  PP-F
           3           4                   0  PP-F
;CASES

0 CASES abs
          -5                               5  N-N
           5                               5  N-N
           0                               0  N-N
       MIN-N                           MIN-N  N-N     \ negate's fixed point again
;CASES

0 CASES min
           3           4                   3  NN-N
          -3           4                  -3  NN-N
           5           5                   5  NN-N
       MIN-N       MAX-N               MIN-N  NN-N
;CASES

0 CASES max
           3           4                   4  NN-N
          -3          -4                  -3  NN-N
       MIN-N       MAX-N               MAX-N  NN-N
;CASES

\ The shift count is taken modulo the cell width: 64 shifts by none. Measured on
\ arm64 and pinned here so a backend that answers zero instead is red.
0 CASES lshift
           1           4                 $10  NN-N
           1          63               MIN-N  NN-N
           1          64                   1  NN-N
           1          65                   2  NN-N
          -1           1                  -2  NN-N
;CASES

0 CASES rshift
         $10           4                   1  NN-N
          -1           1               MAX-N  NN-N     \ logical, not arithmetic
          -1          63                   1  NN-N
           1          64                   1  NN-N
;CASES

0 CASES cells
           1                               8  N-N
           3                              24  N-N
           0                               0  N-N
;CASES

1 CASES cell+
           0                               8  N-N
           8                              16  N-N
;CASES

0 CASES cell+
           0                               8  P-P
           8                              16  P-P
;CASES

0 CASES chars
           1                               1  N-N
           7                               7  N-N
;CASES

1 CASES char+
           0                               1  N-N
           7                               8  N-N
;CASES

0 CASES char+
           0                               1  P-P
           7                               8  P-P
;CASES

\ ---- memory ------------------------------------------------------------------
0 CASES !
           0                               0  MEM
       $1234                           $1234  MEM
          -1                              -1  MEM
;CASES

0 CASES c!
           0                               0  MEM
         $7F                             $7F  MEM
         $FF                             $FF  MEM
;CASES

0 CASES +!
           0                               3  MEM
          10                              13  MEM
          -3                               0  MEM
;CASES

0 CASES count
           0                               0  MEM
           5                               5  MEM
         $FF                             $FF  MEM
;CASES

0 CASES ptr-field                      \ (ptr a n -- ptr ptr b), n counts cells
           0           0                   0  PN-P
           0           1                   8  PN-P
           8           2                  24  PN-P
;CASES

0 CASES byte-view
       $1234                             $34  MEM     \ little-endian, as both live targets are
          -1                             $FF  MEM
           0                               0  MEM
;CASES

0 CASES cell-view
       $1234                           $1234  MEM
          -1                              -1  MEM
;CASES
