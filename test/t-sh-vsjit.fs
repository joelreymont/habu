\ t-sh-vsjit.fs — adversarial audit of the register-allocating : compiler (vsjit):
\ every compile-mode path interacting with a LIVE VS (constants and registers):
\ IF/ELSE arms, loops, locals pops, s"/['] baking, catch/throw, pool exhaustion,
\ non-VS prims over live entries, VSMAX overflow. Run: gforth test/t-sh-vsjit.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : T {: a :} 5 a 0 > if 1 + then . ; 3 T"        OUT s\" 6\n"      compare 0= -> true }T
T{ s" : T {: a :} 5 a 0 > if 1 + then . ; 0 T"        OUT s\" 5\n"      compare 0= -> true }T
T{ s" : T {: a :} 9 a if 2 + else 3 + then . ; 1 T"   OUT s\" 11\n"     compare 0= -> true }T
T{ s" : T {: a :} 9 a if 2 + else 3 + then . ; 0 T"   OUT s\" 12\n"     compare 0= -> true }T
T{ s" : T {: n :} 7 n begin 1- dup 0= until drop + . ; 3 T" OUT s\" 7\n" compare 0= -> true }T
T{ s" : T 0 5 0 do 3 + loop . ; T"                    OUT s\" 15\n"     compare 0= -> true }T
T{ s" : T 0 3 0 do i + loop . ; T"                    OUT s\" 3\n"      compare 0= -> true }T
T{ s" : T 1 2 {: a b :} a b - . ; T"                  OUT s\" -1\n"     compare 0= -> true }T
T{ s\" : T 7 s\" abc\" nip nip . ; T"                OUT s\" 3\n"      compare 0= -> true }T
T{ s" : F 42 . ; : T 5 ['] F execute . ; T"           OUT s\" 42\n5\n"  compare 0= -> true }T
T{ s" : B {: x :} x 0 < if 7 throw then x 1 + ; : T 5 ['] B catch . . ; T" OUT s\" 0\n6\n" compare 0= -> true }T
T{ s" : B {: x :} x 0 < if 7 throw then x 1 + ; : T -1 ['] B catch . ; T"  OUT s\" 7\n"   compare 0= -> true }T
T{ s" : T 1 2 3 rot . . . ; T"                        OUT s\" 1\n3\n2\n" compare 0= -> true }T
T{ s" : T {: a :} a negate . a invert . ; 5 T"        OUT s\" -5\n-6\n" compare 0= -> true }T
T{ s" : T {: a :} a dup * a 1 - dup * + . ; 4 T"      OUT s\" 25\n"     compare 0= -> true }T
\ VSMAX overflow: 40 constants force a mid-word spill, then fold the tail
: DEEP ( -- a u )  0 CL !  s" : T " +B  40 0 do s" 1 " +B loop
   39 0 do s" + " +B loop  s" . ; T" +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ DEEP  s\" 40\n" compare 0= -> true }T
