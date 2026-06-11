\ asm-checked.fs — the ARM64 encoder core as CHECKED, typed Forth. Each word carries a
\ habu stack-effect signature habu's checker CERTIFIES (pure bit math: lshift/or/rshift).
\ The encoders, written in checked Forth — dogfooding habu on its own toolchain. habu
\ type vars are single letters, so sigs read ( a b c -- d ); operand meaning is in the
\ name/comment. Load under habu (src/habu.fs); correctness cross-checked vs asm.fs.
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
\ data-processing 2-source ( rd rn rm -- w ): same RRR layout, divide/shift bases
: A-SDIV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC00C00 or ;
: A-UDIV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC00800 or ;
: A-LSLV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC02000 or ;
: A-LSRV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC02400 or ;
: A-ASRV ( a b c -- d )  16 lshift swap 5 lshift or swap or $9AC02800 or ;
\ logical immediate ( rd rn nis -- w ): base | rd | rn<<5 | nis<<10
: A-ANDI ( a b c -- d )  10 lshift swap 5 lshift or swap or $92000000 or ;
: A-ORRI ( a b c -- d )  10 lshift swap 5 lshift or swap or $B2000000 or ;
: A-EORI ( a b c -- d )  10 lshift swap 5 lshift or swap or $D2000000 or ;
\ asr immediate (SBFM) ( rd rn sh -- w )
: A-ASRI ( a b c -- d )  16 lshift swap 5 lshift or swap or $9340FC00 or ;
\ indirect branches ( rn -- w ), cache maintenance ( rt -- w )
: A-BLR  ( a -- b )  5 lshift $D63F0000 or ;
: A-BR   ( a -- b )  5 lshift $D61F0000 or ;
: A-ICIVAU ( a -- b )  $D50B7520 or ;
: A-DCCVAU ( a -- b )  $D50B7B20 or ;
\ cset ( rd cond -- w ): csinc rd,xzr,xzr,inv(cond) — branchless invert via xor 1
: A-CSET ( a b -- c )  1 xor 12 lshift or $9A9F07E0 or ;
