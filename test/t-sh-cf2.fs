\ t-sh-cf2.fs — second-wave control flow: EXIT (placeholder chain patched at `;`),
\ RECURSE (bl to the open definition), ?DO (zero-trip skip via the LEAVE chain),
\ +LOOP (ANS signed-crossing), J (outer index), LEAVE, UNLOOP, and the CHAR /
\ [CHAR] / EMIT / CR / SPACE / U. conveniences.
\ Run: gforth test/t-sh-cf2.fs -e bye
require sh-driver.fs

: CF2-OUT ( a u -- a2 u2 )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;

T{ s" : T dup 5 > if drop 99 exit then 1+ ; 3 T . 7 T ."  CF2-OUT s\" 4\n99\n" compare 0= -> true }T
T{ s" : F dup 2 < if drop 1 exit then dup 1- recurse swap 2 - recurse + ; 10 F ."
   CF2-OUT s\" 89\n" compare 0= -> true }T
T{ s" : T 0 5 0 ?do 1+ loop ; T ."                        CF2-OUT s\" 5\n"  compare 0= -> true }T
T{ s" : T 0 3 3 ?do 1+ loop ; T ."                        CF2-OUT s\" 0\n"  compare 0= -> true }T
T{ s" : T 0 10 0 do 1+ 2 +loop ; T ."                     CF2-OUT s\" 5\n"  compare 0= -> true }T
T{ s" : T 0 3 0 do 4 0 do j + loop loop ; T ."            CF2-OUT s\" 12\n" compare 0= -> true }T
T{ s" : T 0 10 0 do 1+ dup 4 = if leave then loop ; T ."  CF2-OUT s\" 4\n"  compare 0= -> true }T
T{ s" : T 7 5 0 do unloop exit loop ; T ."                CF2-OUT s\" 7\n"  compare 0= -> true }T
T{ s" : T [char] A emit [char] B emit cr ; T"             CF2-OUT s\" AB\n" compare 0= -> true }T
T{ s" char Z ."                                           CF2-OUT s\" 90\n" compare 0= -> true }T
T{ s" -1 u."                                              CF2-OUT s\" 18446744073709551615\n" compare 0= -> true }T
T{ s" : T 65 emit space 66 emit cr ; T"                   CF2-OUT s\" A B\n" compare 0= -> true }T
\ typed locals run with bare-name references
T{ s" : T {: a:n b:n :} a b + . ; 3 4 T"                  CF2-OUT s\" 7\n"  compare 0= -> true }T
