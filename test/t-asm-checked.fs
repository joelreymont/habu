\ t-asm-checked.fs — the checked encoder core (asm-checked.fs) is CERTIFIED by caf's own
\ checker (every word's CHECK-CODE = 0, no fallback) AND matches asm.fs machine code.
\ The encoders, in checked Forth. Run: gforth test/t-asm-checked.fs -e bye
require ../src/caf.fs
require tester.fs
variable WORST  0 WORST !
: NOTE ( -- )  CHECK-CODE @ ?dup if WORST ! then ;
\ load each checked encoder and record any non-certified verdict
: A-ADD  ( a b c -- d )  16 lshift swap 5 lshift or swap or 2332033024 or ;  NOTE
: A-ADDI ( a b c -- d )  10 lshift swap 5 lshift or swap or 2432696320 or ;  NOTE
: A-MOVZ ( a b c -- d )  21 lshift swap 5 lshift or swap or 3531603968 or ;  NOTE
: A-LDR  ( a b c -- d )  3 rshift 10 lshift swap 5 lshift or swap or 4181721088 or ;  NOTE
T{ WORST @ -> 0 }T                          \ caf certified every encoder
T{ 1 2 3 A-ADD   -> 2332229697 }T           \ add x1,x2,x3
T{ 1 2 10 A-ADDI -> 2432706625 }T           \ add x1,x2,#10
T{ 5 42 0 A-MOVZ -> 3531605317 }T           \ movz x5,#42
T{ 1 2 16 A-LDR  -> 4181723201 }T           \ ldr x1,[x2,#16]
