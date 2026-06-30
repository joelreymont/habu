variable RISSHARED
0 RISSHARED !
: RISWORK ( -- )
   2 3 + RISSHARED ! ;
' RISWORK MEM-ALLOC-64K run-in-stack
RISSHARED @ . cr         \ expect 5
7 8 + . cr               \ expect 15 (caller stack intact)
s" run-in-stack ok" type cr
