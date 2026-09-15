require lib/errors.f
require lib/memory.f

variable RISSHARED
0 RISSHARED !
: RISWORK ( -- )
   2 3 + RISSHARED ! ;
' RISWORK STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED run-in-stack
RISSHARED @ . cr         \ expect 5
7 8 + . cr               \ expect 15 (caller stack intact)
s" run-in-stack ok" type cr
