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
\ reg-aware shuffles (REG-COPY): dup/over of a register = one mov; swap/drop/nip free
T{ s" : T {: a :} a dup * . ; 7 T"                OUT s\" 49\n" compare 0= -> true }T
T{ s" : T {: a b :} a b swap drop dup * . ; 3 9 T" OUT s\" 81\n" compare 0= -> true }T
T{ s" : T {: a :} a 2 over + + . ; 10 T"          OUT s\" 22\n" compare 0= -> true }T
T{ s" : T {: a b :} a b nip dup * . ; 3 5 T"      OUT s\" 25\n" compare 0= -> true }T
\ VS-aware unary ops: con folds at JIT time; reg gets one in-place op
T{ s" : T 5 1+ . 5 1- . ; T"                      OUT s\" 6\n4\n"  compare 0= -> true }T
T{ s" : T 0 0= . 7 0= . ; T"                      OUT s\" -1\n0\n" compare 0= -> true }T
T{ s" : T 7 negate . 0 invert . ; T"              OUT s\" -7\n-1\n" compare 0= -> true }T
T{ s" : T {: a :} a 1+ . a 1- . ; 5 T"            OUT s\" 6\n4\n"  compare 0= -> true }T
T{ s" : T {: a :} a 0= . a 0< . ; 0 T"            OUT s\" -1\n0\n" compare 0= -> true }T
T{ s" : T {: a :} a 0< . ; 0 7 - T"               OUT s\" -1\n"    compare 0= -> true }T
T{ s" : T {: a :} a negate . a invert . ; 9 T"    OUT s\" -9\n-10\n" compare 0= -> true }T
T{ s" : T {: a :} a 1+ 1+ negate . ; 3 T"         OUT s\" -5\n"    compare 0= -> true }T
T{ s" : T {: a b :} a 1+ b 1- * . ; 4 7 T"        OUT s\" 30\n"    compare 0= -> true }T
T{ s" : T 1+ . ; 5 T"                             OUT s\" 6\n"     compare 0= -> true }T
\ minimal literal chains (Lvmovk): movz/movn form + skipped chunks, all shapes
T{ s" : T 0 . 0 1 - . ; T"                        OUT s\" 0\n-1\n"  compare 0= -> true }T
T{ s" : T 65536 . $123456789ABC . ; T"           OUT s\" 65536\n20015998343868\n" compare 0= -> true }T
T{ s" : T 0 2 - {: a :} a 7 * . ; T"              OUT s\" -14\n"    compare 0= -> true }T
T{ s" : T $FFFF0000FFFF0000 $FFFF and . ; T"      OUT s\" 0\n"      compare 0= -> true }T
T{ s" : T {: a :} $100000001 a + . ; 1 T"         OUT s\" 4294967298\n" compare 0= -> true }T
