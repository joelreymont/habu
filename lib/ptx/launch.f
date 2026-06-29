\ ptx-launch.f - checked PTX launch-contract helpers.
\
\ Load after lib/errors.f and lib/ptx/header.f.

: PTX-LAUNCH-POSITIVE ( n -- )
   0 <= if E-PTX-BLOCK throw then ;

: PTX-ROW-LAUNCH-CHECK ( n n n -- ) {: rows:n cols:n block:n :}
   rows PTX-LAUNCH-POSITIVE
   cols PTX-LAUNCH-POSITIVE
   block PTX-BLOCK-CHECK
   cols block > if E-PTX-BLOCK throw then ;
