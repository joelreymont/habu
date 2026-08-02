\ asm-src-test.f - unchecked source ARM64 encoder regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/asm-src-test.f

require lib/errors.f
require lib/string.f
require lib/test.f

: AST-WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

: AST-LOAD-ASM ( -- )
   s" ENC-ADD" AST-WORD? 0= if s" src/arch/arm64/asm.f" included then
   s" LIT64," AST-WORD? 0= if s" src/arch/arm64/icode.f" included then
   s" ADD," AST-WORD? 0= if s" src/arch/arm64/mnem.f" included then ;

AST-LOAD-ASM

s" MOVZHW" s" n n n -- n" TRUST
s" ENC-ADD" s" n n n -- n" TRUST
s" ENC-LDR" s" n n n -- n" TRUST
s" ENC-FADD" s" n n n -- n" TRUST
s" ENC-FSUB" s" n n n -- n" TRUST
s" ENC-FMUL" s" n n n -- n" TRUST
s" ENC-FDIV" s" n n n -- n" TRUST
s" ENC-FNEG" s" n n -- n" TRUST
s" ENC-FABS" s" n n -- n" TRUST
s" ENC-FSQRT" s" n n -- n" TRUST
s" ENC-SCVTF" s" n n -- n" TRUST
s" ENC-FCVTZS" s" n n -- n" TRUST
s" ENC-FMOVXD" s" n n -- n" TRUST
s" ENC-FMOVDX" s" n n -- n" TRUST
s" ENC-FMOVDD" s" n n -- n" TRUST
s" ENC-FLDR" s" n n n -- n" TRUST
s" ENC-FSTR" s" n n n -- n" TRUST
s" ENC-LDAR" s" n n -- n" TRUST
s" ENC-BLR" s" n -- n" TRUST
s" >LIMM" s" n -- n" TRUST
s" ENC-ANDI" s" n n n -- n" TRUST
s" CW@" s" n -- ptr u8" TRUST
s" CODE-BYTE+" s" ptr u8 n -- ptr u8" TRUST
s" ARESET" s" --" TRUST
s" ADD," s" n n n --" TRUST
s" LDAR," s" n n --" TRUST
s" ASM-LEN" s" -- n" TRUST
s" LIT64," s" n n --" TRUST

$2000000000000000 constant AST-DNAME-EXT

: AST-U32@ ( n -- n )
   CW@ dup c@
   swap 1 CODE-BYTE+ dup c@ $8 lshift
   swap 1 CODE-BYTE+ dup c@ $10 lshift
   swap 1 CODE-BYTE+ c@ $18 lshift
   or or or ;

: AST-TEST-CORE ( -- )
   1 2 3 ENC-ADD $8B030041 T=
   1 2 3 MOVZHW $D2E00041 T=
   1 2 $18 ENC-LDR $F9400C41 T=
   14 15 ENC-LDAR $C8DFFDEE T=
   11 5 ENC-STLR $C89FFCAB T=
   7 ENC-BLR $D63F00E0 T= ;

\ The floating-point set, against the values the system assembler produces for
\ the same instructions. Every one of these is an instruction the native chain
\ now emits for a float body, and the two frame accesses are the ones a spilled
\ double travels through - which no acceptance row reaches, because the routine
\ contract hands out the whole D file, so this is where they are checked at all.
: AST-TEST-FP ( -- )
   1 2 3 ENC-FADD $1E632841 T=
   1 2 3 ENC-FSUB $1E633841 T=
   1 2 3 ENC-FMUL $1E630841 T=
   1 2 3 ENC-FDIV $1E631841 T=
   5 6 ENC-FNEG $1E6140C5 T=
   5 6 ENC-FABS $1E60C0C5 T=
   5 6 ENC-FSQRT $1E61C0C5 T=
   7 9 ENC-SCVTF $9E620127 T=
   7 9 ENC-FCVTZS $9E780127 T=
   4 11 ENC-FMOVXD $9E670164 T=
   4 11 ENC-FMOVDX $9E660164 T=
   2 13 ENC-FMOVDD $1E6041A2 T=
   3 31 24 ENC-FLDR $FD400FE3 T=
   3 31 24 ENC-FSTR $FD000FE3 T=
   31 1 32760 ENC-FLDR $FD7FFC3F T=
   0 19 0 ENC-FSTR $FD000260 T= ;

: AST-TEST-LIMM ( -- )
   1 >LIMM $1000 T=
   $FF >LIMM $1007 T=
   AST-DNAME-EXT >LIMM $10C0 T=
   14 14 AST-DNAME-EXT >LIMM ENC-ANDI $924301CE T= ;

: AST-TEST-ICODE ( -- )
   ARESET
   1 2 3 ADD,
   0 AST-U32@ $8B030041 T=
   ARESET
   14 15 LDAR,
   0 AST-U32@ $C8DFFDEE T=
   ARESET
   11 5 STLR,
   0 AST-U32@ $C89FFCAB T=
   ARESET
   9 AST-DNAME-EXT LIT64,
   ASM-LEN $4 T=
   0 AST-U32@ $D2E40009 T= ;

: AST-MAIN ( -- )
   T-RESET
   AST-TEST-CORE
   AST-TEST-FP
   AST-TEST-LIMM
   AST-TEST-ICODE
   T-REPORT
   s" asm-src-test: ok" type cr ;

AST-MAIN
