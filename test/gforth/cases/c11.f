\ leaving a counted loop early with unloop exit
: FIRST-OVER ( n -- n ) {: lim:n :}
   100 0 do i i * lim > if i unloop exit then loop -1 ;
: C11 ( -- ) 50 FIRST-OVER . 20000 FIRST-OVER . ;
C11
