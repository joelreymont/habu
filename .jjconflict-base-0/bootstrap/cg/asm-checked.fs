\ asm-checked.fs — the ARM64 encoder core as CHECKED, typed Forth. Each word carries a
\ habu stack-effect signature habu's checker CERTIFIES (pure bit math: lshift/or/rshift).
\ Register operands are nominal `reg` roles so emitter callers cannot swap a
\ register with an offset, label, or symbol index while still type-checking.
\ Shared ARM64 instruction layouts.
: A-RRR16 ( reg reg n n -- n ) {: rd rn bits base :}
   base bits 16 lshift or rn REG>N 5 lshift or rd REG>N or ;

: A-RRI10 ( reg reg n n -- n ) {: rd rn imm base :}
   base imm 10 lshift or rn REG>N 5 lshift or rd REG>N or ;

: A-MOVW ( reg n n n -- n ) {: rd imm hw base :}
   base hw 21 lshift or imm 5 lshift or rd REG>N or ;

: A-LS-UOFF ( reg reg off n -- n ) {: rd rn off base :}
   base off OFF>N 3 rshift 10 lshift or rn REG>N 5 lshift or rd REG>N or ;

: A-R1-5 ( reg n -- n ) {: rn base :}
   base rn REG>N 5 lshift or ;

: A-CSET-LAYOUT ( reg n n -- n ) {: rd cond base :}
   base cond 1 xor 12 lshift or rd REG>N or ;

\ 3-operand shifted-register ( rd rn rm -- w ): w = base | rd | rn<<5 | rm<<16
: A-ADD  ( reg reg reg -- n )  REG>N 2332033024 A-RRR16 ;

: A-SUB  ( reg reg reg -- n )  REG>N 3405774848 A-RRR16 ;

: A-MUL  ( reg reg reg -- n )  REG>N 2600500224 A-RRR16 ;

: A-AND  ( reg reg reg -- n )  REG>N 2315255808 A-RRR16 ;

: A-ORR  ( reg reg reg -- n )  REG>N 2852126720 A-RRR16 ;

: A-EOR  ( reg reg reg -- n )  REG>N 3388997632 A-RRR16 ;

\ add/sub immediate ( rd rn imm -- w ): base | rd | rn<<5 | imm<<10
: A-ADDI ( reg reg n -- n )  2432696320 A-RRI10 ;

: A-SUBI ( reg reg n -- n )  3506438144 A-RRI10 ;

\ move-wide ( rd imm hw -- w ): base | rd | imm<<5 | hw<<21
: A-MOVZ ( reg n n -- n )  3531603968 A-MOVW ;

: A-MOVK ( reg n n -- n )  4068474880 A-MOVW ;

\ load/store unsigned-offset ( rd rn off -- w ): base | rd | rn<<5 | (off>>3)<<10
: A-LDR  ( reg reg off -- n )  4181721088 A-LS-UOFF ;

: A-STR  ( reg reg off -- n )  4177526784 A-LS-UOFF ;

\ data-processing 2-source ( rd rn rm -- w ): same RRR layout, divide/shift bases
: A-SDIV ( reg reg reg -- n )  REG>N $9AC00C00 A-RRR16 ;

: A-UDIV ( reg reg reg -- n )  REG>N $9AC00800 A-RRR16 ;

: A-LSLV ( reg reg reg -- n )  REG>N $9AC02000 A-RRR16 ;

: A-LSRV ( reg reg reg -- n )  REG>N $9AC02400 A-RRR16 ;

: A-ASRV ( reg reg reg -- n )  REG>N $9AC02800 A-RRR16 ;

\ logical immediate ( rd rn nis -- w ): base | rd | rn<<5 | nis<<10
: A-ANDI ( reg reg n -- n )  $92000000 A-RRI10 ;

: A-ORRI ( reg reg n -- n )  $B2000000 A-RRI10 ;

: A-EORI ( reg reg n -- n )  $D2000000 A-RRI10 ;

\ asr immediate (SBFM) ( rd rn sh -- w )
: A-ASRI ( reg reg n -- n )  $9340FC00 A-RRR16 ;

\ indirect branches ( rn -- w ), cache maintenance ( rt -- w )
: A-BLR  ( reg -- n )  $D63F0000 A-R1-5 ;

: A-BR   ( reg -- n )  $D61F0000 A-R1-5 ;

: A-ICIVAU ( reg -- n )  REG>N $D50B7520 or ;

: A-DCCVAU ( reg -- n )  REG>N $D50B7B20 or ;

\ cset ( rd cond -- w ): csinc rd,xzr,xzr,inv(cond) — branchless invert via xor 1
: A-CSET ( reg n -- n )  $9A9F07E0 A-CSET-LAYOUT ;
