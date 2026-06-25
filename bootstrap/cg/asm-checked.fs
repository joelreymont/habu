\ asm-checked.fs — the ARM64 encoder core as CHECKED, typed Forth. Each word carries a
\ habu stack-effect signature habu's checker CERTIFIES (pure bit math: lshift/or/rshift).
\ The encoders, written in checked Forth — dogfooding habu on its own toolchain. habu
\ type vars are single letters, so sigs read ( a b c -- d ); operand meaning is in the
\ name/comment. Load under habu (src/habu.fs); correctness cross-checked vs asm.fs.
\ Shared ARM64 instruction layouts.
: A-RRR16 ( a b c d -- e ) {: rd rn rm base :}
   base rm 16 lshift or rn 5 lshift or rd or ;

: A-RRI10 ( a b c d -- e ) {: rd rn imm base :}
   base imm 10 lshift or rn 5 lshift or rd or ;

: A-MOVW ( a b c d -- e ) {: rd imm hw base :}
   base hw 21 lshift or imm 5 lshift or rd or ;

: A-LS-UOFF ( a b c d -- e ) {: rd rn off base :}
   base off 3 rshift 10 lshift or rn 5 lshift or rd or ;

: A-R1-5 ( a b -- c ) {: rn base :}
   base rn 5 lshift or ;

: A-CSET-LAYOUT ( a b c -- d ) {: rd cond base :}
   base cond 1 xor 12 lshift or rd or ;

\ 3-operand shifted-register ( rd rn rm -- w ): w = base | rd | rn<<5 | rm<<16
: A-ADD  ( a b c -- d )  2332033024 A-RRR16 ;

: A-SUB  ( a b c -- d )  3405774848 A-RRR16 ;

: A-MUL  ( a b c -- d )  2600500224 A-RRR16 ;

: A-AND  ( a b c -- d )  2315255808 A-RRR16 ;

: A-ORR  ( a b c -- d )  2852126720 A-RRR16 ;

: A-EOR  ( a b c -- d )  3388997632 A-RRR16 ;

\ add/sub immediate ( rd rn imm -- w ): base | rd | rn<<5 | imm<<10
: A-ADDI ( a b c -- d )  2432696320 A-RRI10 ;

: A-SUBI ( a b c -- d )  3506438144 A-RRI10 ;

\ move-wide ( rd imm hw -- w ): base | rd | imm<<5 | hw<<21
: A-MOVZ ( a b c -- d )  3531603968 A-MOVW ;

: A-MOVK ( a b c -- d )  4068474880 A-MOVW ;

\ load/store unsigned-offset ( rd rn off -- w ): base | rd | rn<<5 | (off>>3)<<10
: A-LDR  ( a b c -- d )  4181721088 A-LS-UOFF ;

: A-STR  ( a b c -- d )  4177526784 A-LS-UOFF ;

\ data-processing 2-source ( rd rn rm -- w ): same RRR layout, divide/shift bases
: A-SDIV ( a b c -- d )  $9AC00C00 A-RRR16 ;

: A-UDIV ( a b c -- d )  $9AC00800 A-RRR16 ;

: A-LSLV ( a b c -- d )  $9AC02000 A-RRR16 ;

: A-LSRV ( a b c -- d )  $9AC02400 A-RRR16 ;

: A-ASRV ( a b c -- d )  $9AC02800 A-RRR16 ;

\ logical immediate ( rd rn nis -- w ): base | rd | rn<<5 | nis<<10
: A-ANDI ( a b c -- d )  $92000000 A-RRI10 ;

: A-ORRI ( a b c -- d )  $B2000000 A-RRI10 ;

: A-EORI ( a b c -- d )  $D2000000 A-RRI10 ;

\ asr immediate (SBFM) ( rd rn sh -- w )
: A-ASRI ( a b c -- d )  $9340FC00 A-RRR16 ;

\ indirect branches ( rn -- w ), cache maintenance ( rt -- w )
: A-BLR  ( a -- b )  $D63F0000 A-R1-5 ;

: A-BR   ( a -- b )  $D61F0000 A-R1-5 ;

: A-ICIVAU ( a -- b )  $D50B7520 or ;

: A-DCCVAU ( a -- b )  $D50B7B20 or ;

\ cset ( rd cond -- w ): csinc rd,xzr,xzr,inv(cond) — branchless invert via xor 1
: A-CSET ( a b -- c )  $9A9F07E0 A-CSET-LAYOUT ;
