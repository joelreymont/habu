\ t-shake.fs — call-graph tree-shaker reachability (treeshake.f). A word is kept
\ only when REACHABLE from the roots (here MAIN) through the program's
\ definition call graph; dead definitions and names in comments/strings are
\ shaken. This is the AOT shaker — precise, unlike the textual KEEP?.
\ Run: gforth test/t-shake.fs -e bye
require tester.fs
include ../src/habu/treeshake.f

create PROG 2048 allot
: SETPROG ( a u -- )  dup SHK-U !  PROG SHK-A !  PROG swap move ;
: R? ( a u -- f )  IN-REACH? ;

\ reachable from MAIN: MAIN -> fib -> {dup < - + swap recurse}, MAIN -> {. cr}.
\ DEAD (and its only-here f+) is never called -> shaken.
s" : DEAD f+ ; : FIB dup 2 < if exit then dup 1 - recurse swap 2 - recurse + ; : MAIN 10 fib . cr ;"
SETPROG  s" MAIN" SHK-FROM
T{ s" main" R? -> -1 }T
T{ s" fib"  R? -> -1 }T
T{ s" dup"  R? -> -1 }T
T{ s" swap" R? -> -1 }T
T{ s" cr"   R? -> -1 }T
T{ s" ."    R? -> -1 }T
T{ s" f+"   R? -> 0 }T          \ named only inside the dead DEAD -> shaken
T{ s" dead" R? -> 0 }T          \ the unreachable def itself

\ names appearing only in comments / strings are not roots
s" : MAIN 5 . ; \ noise here" SETPROG  s" MAIN" SHK-FROM
T{ s" ."     R? -> -1 }T
T{ s" noise" R? -> 0 }T
s\" : MAIN s\" hi\" type ; ( drop swap )" SETPROG  s" MAIN" SHK-FROM
T{ s" type" R? -> -1 }T
T{ s" hi"   R? -> 0 }T          \ inside a string literal
T{ s" drop" R? -> 0 }T          \ inside a ( ) comment

\ transitive: MAIN -> A -> B keeps B; an unrelated chain C -> D does not
s" : B 7 + ; : A B ; : D 9 * ; : C D ; : MAIN 3 A . ;" SETPROG  s" MAIN" SHK-FROM
T{ s" a" R? -> -1 }T
T{ s" b" R? -> -1 }T
T{ s" c" R? -> 0 }T
T{ s" d" R? -> 0 }T

cr ." t-shake: " #ERRORS @ . ." failure(s)" cr
#ERRORS @ 0<> negate (bye)
