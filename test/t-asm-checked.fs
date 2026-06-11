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
\ engine-ISA extension: divides/shifts, logical-imm, indirect branches, cset
: A-SDIV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC00C00 or ;  NOTE
: A-ANDI ( a b c -- d )  10 lshift swap 5 lshift or swap or $92000000 or ;  NOTE
: A-BLR  ( a -- b )  5 lshift $D63F0000 or ;  NOTE
: A-CSET ( a b -- c )  1 xor 12 lshift or $9A9F07E0 or ;  NOTE
T{ WORST @ -> 0 }T                          \ caf certified the extension too
T{ 5 1 2 A-SDIV -> $9AC20C25 }T             \ sdiv x5,x1,x2
T{ 5 1 $1234 A-ANDI -> $9248D025 }T         \ and x5,x1,#nis($1234)
T{ 7 A-BLR -> $D63F00E0 }T                  \ blr x7
T{ 5 11 A-CSET -> $9A9FA7E5 }T            \ cset x5,lt
