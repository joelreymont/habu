\ t-cg-word.fs — compile real Forth stack-code bodies to native Mac executables
\ and check the results. The bridge from checked-Forth source to ARM64 machine
\ code. Slow (codesign+exec per case); run explicitly, not in all.fs:
\   gforth test/t-cg-word.fs -e bye
require tester.fs
require ../src/cg/templ.fs
\ NATIVE-EVAL ( body-addr body-u input -- exit-code )

T{ s" DUP *"          7 NATIVE-EVAL -> 49 }T   \ square
T{ s" DUP DUP * *"    3 NATIVE-EVAL -> 27 }T   \ 3 -> 3*3*3
T{ s" DUP +"         10 NATIVE-EVAL -> 20 }T   \ double
T{ s" 1+ 1+"          5 NATIVE-EVAL ->  7 }T   \ +2
T{ s" 1-"             9 NATIVE-EVAL ->  8 }T
T{ s" DUP DUP * SWAP DROP" 4 NATIVE-EVAL -> 16 }T   \ keep the square
T{ s" 7 *"            6 NATIVE-EVAL -> 42 }T   \ literal in body
T{ s" 3 + 2 *"       20 NATIVE-EVAL -> 46 }T   \ (20+3)*2
T{ s" DUP OVER + +"   8 NATIVE-EVAL -> 24 }T   \ 8 -> 8 8 8; + + -> 24
