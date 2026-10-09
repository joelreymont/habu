\ set-preflight refuses an xt outside the live code, a data address, even into
\ an empty cell: fd 2 names it and the process exits 70 (habu1.f:3747
\ BSETPREFLIGHT).
0 set-check
here set-preflight ." installed" cr
