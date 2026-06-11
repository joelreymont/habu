\ asm-checked.fs — the ARM64 encoder core as CHECKED, typed Forth. Each word carries a
\ caf stack-effect signature caf's checker CERTIFIES (pure bit math: lshift/or/rshift).
\ The encoders, written in checked Forth — dogfooding caf on its own toolchain. caf
\ type vars are single letters, so sigs read ( a b c -- d ); operand meaning is in the
\ name/comment. Load under caf (src/caf.fs); correctness cross-checked vs asm.fs.
\ 3-operand shifted-register ( rd rn rm -- w ): w = base | rd | rn<<5 | rm<<16
: A-ADD  ( a b c -- d )  16 lshift swap 5 lshift or swap or 2332033024 or ;
: A-SUB  ( a b c -- d )  16 lshift swap 5 lshift or swap or 3405774848 or ;
: A-MUL  ( a b c -- d )  16 lshift swap 5 lshift or swap or 2600500224 or ;
: A-AND  ( a b c -- d )  16 lshift swap 5 lshift or swap or 2315255808 or ;
: A-ORR  ( a b c -- d )  16 lshift swap 5 lshift or swap or 2852126720 or ;
: A-EOR  ( a b c -- d )  16 lshift swap 5 lshift or swap or 3388997632 or ;
\ add/sub immediate ( rd rn imm -- w ): base | rd | rn<<5 | imm<<10
: A-ADDI ( a b c -- d )  10 lshift swap 5 lshift or swap or 2432696320 or ;
: A-SUBI ( a b c -- d )  10 lshift swap 5 lshift or swap or 3506438144 or ;
\ move-wide ( rd imm hw -- w ): base | rd | imm<<5 | hw<<21
: A-MOVZ ( a b c -- d )  21 lshift swap 5 lshift or swap or 3531603968 or ;
: A-MOVK ( a b c -- d )  21 lshift swap 5 lshift or swap or 4068474880 or ;
\ load/store unsigned-offset ( rd rn off -- w ): base | rd | rn<<5 | (off>>3)<<10
: A-LDR  ( a b c -- d )  3 rshift 10 lshift swap 5 lshift or swap or 4181721088 or ;
: A-STR  ( a b c -- d )  3 rshift 10 lshift swap 5 lshift or swap or 4177526784 or ;
