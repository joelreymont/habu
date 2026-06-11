\ t-cg-ctrl.fs — compile Forth control flow + comparisons to native and check.
\ Real programs end-to-end. Slow (exec per case); run explicitly:
\   gforth test/t-cg-ctrl.fs -e bye
require tester.fs
require ../bootstrap/cg/walk.fs

\ abs via IF/THEN + NEGATE
T{ s" DUP 0< IF NEGATE THEN"   7 NATIVE-EVAL -> 7 }T
T{ s" DUP 0< IF NEGATE THEN"  -7 NATIVE-EVAL -> 7 }T

\ IF/ELSE/THEN
T{ s" DUP 5 < IF DROP 100 ELSE DROP 200 THEN" 3 NATIVE-EVAL -> 100 }T
T{ s" DUP 5 < IF DROP 100 ELSE DROP 200 THEN" 9 NATIVE-EVAL -> 200 }T

\ comparisons -> Forth flag (0 / -1; -1 & 0xff = 255)
T{ s" 0="   0 NATIVE-EVAL -> 255 }T
T{ s" 0="   3 NATIVE-EVAL ->   0 }T
T{ s" 5 <"  3 NATIVE-EVAL -> 255 }T
T{ s" 5 <"  7 NATIVE-EVAL ->   0 }T

\ BEGIN/UNTIL countdown to zero
T{ s" BEGIN 1- DUP 0= UNTIL"   5 NATIVE-EVAL -> 0 }T

\ ?DO/LOOP with I: sum 1..n and factorial n
T{ s" 0 SWAP 1+ 1 ?DO I + LOOP" 5 NATIVE-EVAL ->  15 }T
T{ s" 1 SWAP 1+ 1 ?DO I * LOOP" 5 NATIVE-EVAL -> 120 }T

\ div / mod
T{ s" 7 MOD" 17 NATIVE-EVAL -> 3 }T
T{ s" 3 /"   20 NATIVE-EVAL -> 6 }T
