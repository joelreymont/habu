\ t-sh-fold.fs — JIT constant folding (jit): literal arithmetic folds at compile
\ time and must equal the runtime (register/call) result; mixed fold+spill+call,
\ locals, IF arms, and deep VS chains all behave. Run: gforth test/t-sh-fold.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : GO 6 7 * . ; GO"                          OUT s\" 42\n"  compare 0= -> true }T
T{ s" : GO 1 2 + 3 * 4 - . ; GO"                  OUT s\" 5\n"   compare 0= -> true }T
T{ s" : GO 2 3 + dup * . ; GO"                    OUT s\" 25\n"  compare 0= -> true }T
T{ s" : GO $F0 $0F or $FF and . ; GO"             OUT s\" 255\n" compare 0= -> true }T
T{ s" : GO $FF $F0 xor . ; GO"                    OUT s\" 15\n"  compare 0= -> true }T
T{ s" : GO {: a :} 2 3 + a + . ; 10 GO"           OUT s\" 15\n"  compare 0= -> true }T
T{ s" : GO 1 2 3 + if 9 . then . ; GO"            OUT s\" 9\n1\n" compare 0= -> true }T
T{ s" : GO 1 2 3 4 5 6 7 + + + + + + . ; GO"      OUT s\" 28\n"  compare 0= -> true }T
T{ s" : F 6 7 * ; : GO F F + . ; GO"              OUT s\" 84\n"  compare 0= -> true }T
T{ s" : GO 10 20 30 swap drop + . ; GO"           OUT s\" 40\n"  compare 0= -> true }T
\ VS shuffles (jit C): relabels on constant entries; dead literals vanish
T{ s" : GO 5 drop 6 7 * . ; GO"                   OUT s\" 42\n"  compare 0= -> true }T
T{ s" : GO 1 2 swap - . ; GO"                     OUT s\" 1\n"   compare 0= -> true }T
T{ s" : GO 10 20 over + + . ; GO"                 OUT s\" 40\n"  compare 0= -> true }T
T{ s" : GO 1 2 nip . ; GO"                        OUT s\" 2\n"   compare 0= -> true }T
T{ s" : GO 7 dup . . ; GO"                        OUT s\" 7\n7\n" compare 0= -> true }T
\ jit D: register binops/comparisons + locals-into-registers
T{ s" : T {: p q :} 5 p 7 + q + . . ; 2 9 T"      OUT s\" 18\n5\n" compare 0= -> true }T
T{ s" : T {: w x y z :} w x + y z + * . ; 1 2 3 4 T" OUT s\" 21\n" compare 0= -> true }T
T{ s" : T {: a b c d e f g h :} a b + c d + e f + g h + + + + . ; 1 2 3 4 5 6 7 8 T" OUT s\" 36\n" compare 0= -> true }T
T{ s" : T {: a :} a 5 < . a 5 > . a 5 = . ; 3 T"  OUT s\" -1\n0\n0\n" compare 0= -> true }T
T{ s" : T {: a :} a 3 >= if 9 . then ; 7 T"       OUT s\" 9\n" compare 0= -> true }T
T{ s" : S {: a b :} a b + a b - * ; : GO 10 3 S . ; GO" OUT s\" 91\n" compare 0= -> true }T
