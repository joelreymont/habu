\ clobber-dofall.f - fixture read as text by clobber-lint-test.f (never loaded).

\ The `do` body always runs, so A returns before B. Real: 0.
variable LDF-A
variable LDF-B
: EMIT-DF-ROUTINES ( n -- ) {: n:n :}
   LDF-A LABEL@ LBL,
   n 0 do RET, loop
   LDF-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-DF ( -- )
   9 0 MOVZ,
   LDF-A LABEL@ BL,
   4 9 0 LDRB, ;
