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
\ immediate / postpone / compile, — user-extensible compile words (phase A):
\ an immediate word EXECUTES during compilation; postpone compiles either the
\ call (immediate target) or code that compiles the call (ordinary target).
T{ s" : STAR 42 emit ; immediate : T STAR ; T"                    CF2-OUT s" *"     compare 0= -> true }T
T{ s\" : FOO 7 . ; : E7 ['] FOO compile, ; immediate : T E7 ; T" CF2-OUT s\" 7\n" compare 0= -> true }T
T{ s" : FOO 9 . ; : P postpone FOO ; immediate : T P ; T"         CF2-OUT s\" 9\n" compare 0= -> true }T
T{ s" : IM 3 . ; immediate : P2 postpone IM ; immediate : T P2 ; T" CF2-OUT s\" 3\n" compare 0= -> true }T
\ DOES> + runtime CREATE: the defining word patches its created word into
\ `push dfield ; b does-body` (Ldoespatch runs from engine text — the region
\ can't un-execute the page it is running).
T{ s" : CONST create , does> @ ; 5 CONST FIVE FIVE ."         CF2-OUT s\" 5\n"  compare 0= -> true }T
T{ s" : CONST create , does> @ ; 5 CONST F 9 CONST N F N + ." CF2-OUT s\" 14\n" compare 0= -> true }T
T{ s" : ARR create cells allot does> swap cells + ; 4 ARR A4 7 2 A4 ! 2 A4 @ ." CF2-OUT s\" 7\n" compare 0= -> true }T
T{ s" : CNT create 0 , does> dup @ 1 + dup rot ! ; CNT K K . K . K ." CF2-OUT s\" 1\n2\n3\n" compare 0= -> true }T
\ does>-patched words must never be INLINED by c-call (the patch lives in the
\ ret slot, outside the inline scan): compiled calls go through bl.
T{ s" : ARR create cells allot does> swap cells + ; 4 ARR A4 : USE 2 A4 @ . ; 7 2 A4 ! USE" CF2-OUT s\" 7\n" compare 0= -> true }T
