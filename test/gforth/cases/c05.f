\ a counted loop and a begin/while/repeat loop
: SUM-TO ( n -- n ) {: lim:n :}
   0 lim 0 ?do i + loop ;
: STEPS ( n -- n )
   0 swap begin dup 0 > while 2 - swap 1 + swap repeat drop ;
: C05 ( -- )
   10 SUM-TO .
   7 STEPS . ;
C05
